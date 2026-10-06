(ns engine.core.settle
  "Settling a round: booking a listed job's or a reflex job's outcome, judging reflex ends against the
  entry's persistence, and starting a round."
  (:require [engine.core.base :refer [drop-instance! drop-reflex-job! emit! free-owner! job-fields manual? now paused? reflex-text remove-listed running save-memory! set-owner! state stopped-result? wait-reason waiting-text]]
            [engine.core.attention :refer [request-attention! resolve-job-attention!]]
            [engine.core.fruitless :refer [book-round! note-fruitless!]]
            [engine.core.register :refer [trigger-holds?]]
            [engine.core.round :refer [run-round]]
            [engine.backoff :as backoff]
            [engine.memory :as mem]))

(defn stopped-text
  "The words of a :stopped event: the result's own :text (clipped to 200 chars), else \"stopped: <reason>\"."
  [{:keys [text reason cell]}]
  (cond
    (and (string? text) (> (count text) 200)) (str (subs text 0 199) "…")
    (and (string? text) (seq text)) text
    :else (str "stopped: " (some-> reason name) (some->> cell pr-str (str " at ")))))

(defn note-child-wait!
  "A listed job's round returned :declined because its child's check waits (reason, or nil when the child gave none):
  the job waits with that reason, told once. Any other round result drops a wait taken from a child."
  [eng id status reason]
  (let [before (get @(:waiting eng) id)]
    (cond
      (and (= :declined status) reason)
      (let [r (with-meta (wait-reason reason) {:child true})]
        (when (not= before r)
          (swap! (:waiting eng) #(assoc (select-keys % (:list (state eng))) id r))
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :waiting :level :info :data r :text (waiting-text r)}))))
      (:child (meta before))
      (swap! (:waiting eng) dissoc id))))

(defn settle-listed! [eng {:keys [id]} {:keys [status error result child-wait]}]
  (note-child-wait! eng id status child-wait)
  (let [idx (.indexOf (:list (state eng)) id)
        fields (job-fields eng id)]
    (case status
      :done
      (do (resolve-job-attention! eng id :job-completed
                                  #(assoc (remove-listed % id) :cursor (max idx 0)))
          (swap! (:fruitless eng) dissoc id)
          (mem/delete-job! (:store eng) id)
          (emit! eng (if (stopped-result? result)
                       (merge fields {:source :job :kind :stopped :level :warn :attention :notice
                                      :data (dissoc result :dug)
                                      :text (stopped-text result)})
                       (merge fields {:source :job :kind :completed :level :info
                                      :attention :notice
                                      :data {:status :completed}}))))

      :cut
      (do (swap! (:state eng) assoc :resume id :current nil)
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :cut :level :info :by :primitives
                             :text "a primitive rejected with cut; the job stays listed"})))

      :error
      (request-attention! eng {:job-id id :reason :round-failed :kind :failed
                               :context (select-keys fields [:round :chain])
                               :data {:error (str error)}
                               :message (str "Job failed and is parked until retried or cancelled: " error)
                               :state-update #(-> %
                                                  (assoc-in [:failed id] {:error (str error) :t (now eng)})
                                                  (assoc :cursor (inc idx) :current nil))})

      (do (swap! (:state eng) assoc :cursor (inc idx) :current nil)
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :yielded :level :debug :status status}))))))

(defn judge-end!
  "Classify a reflex end now and apply the entry's persistence when its trigger
  still holds, the cooldown counted from the end's :ended-at; emit its one
  reflex.ended with extra fields."
  [eng {:keys [reflex job outcome reason ended-at text]} extra]
  (let [entry (some #(when (= reflex (:id %)) %) (:register (state eng)))
        still? (and entry (trigger-holds? eng entry (:primitives eng) (mem/view (:store eng))))]
    (when still?
      (case (:persistence entry)
        :cooldown (swap! (:state eng) assoc-in [:reflex-state reflex :cooldown-until]
                         (+ ended-at (* 1000 (:cooldown-s entry 0))))
        :stop (swap! (:state eng) assoc-in [:reflex-state reflex :stopped?] true)
        nil))
    (emit! eng (merge {:source :reflex :kind :ended :level :info :reflex reflex :job job
                       :outcome outcome :text (str text ": " (name outcome))
                       :how (if still? :completed_not_cleared :cleared)}
                      (when reason {:reason reason})
                      extra))))

(defn end-reflex!
  "A reflex job ended on its own with outcome; classify and apply the entry's
  persistence. While the body is settling, offline or under manual control the
  senses cannot say whether the trigger still holds: the end is deferred to the first ready tick. A :stopped
  outcome carries the result's reason."
  [eng {:keys [id reflex]} outcome reason]
  (let [end (cond-> {:reflex reflex :job id :outcome outcome :ended-at (now eng)
                     :text (reflex-text reflex (get-in (state eng) [:instances id :spec]))}
              reason (assoc :reason reason))]
    (drop-instance! eng id)
    (if (or (paused? eng) (manual? eng))
      (swap! (:state eng) update :deferred-ends conj end)
      (judge-end! eng end {}))))

(defn judge-deferred-ends!
  "On a ready tick: judge the reflex ends deferred while settling, in order."
  [eng]
  (let [ends (:deferred-ends (state eng))]
    (swap! (:state eng) assoc :deferred-ends [])
    (doseq [end ends]
      (judge-end! eng end {:deferred-ms (- (now eng) (:ended-at end))}))))

(def continued-warn-ms
  "Least gap between two reflex.continued warns of one reflex."
  3600000)

(defn warn-continued!
  "Reflex job run returned :continue, which a reflex job may not (it runs one round): warn, at most once per reflex id
  per continued-warn-ms."
  [eng {:keys [id reflex]}]
  (let [t (now eng)
        last (get @(:continued eng) reflex)]
    (when (or (nil? last) (>= (- t last) continued-warn-ms))
      (swap! (:continued eng) assoc reflex t)
      (emit! eng {:source :reflex :kind :continued :level :warn :reflex reflex :job id
                  :text (str (reflex-text reflex (get-in (state eng) [:instances id :spec]))
                             " returned :continue; a reflex job runs one round, so it counts as declined")}))))

(defn settle-reflex!
  "End reflex job run after its one round."
  [eng run {:keys [status error result]}]
  (case status
    :declined
    (do (emit! eng {:source :reflex :kind :declined :level :info :reflex (:reflex run) :job (:id run)
                    :text (str "reflex " (name (:reflex run)) ": job " (:id run)
                               " declined; dropped, it may fire again after the trigger's cooldown")})
        (end-reflex! eng run :declined nil))
    (:error :cut)
    (do (emit! eng {:source :job :kind :failed :level :warn :job (:id run) :reflex (:reflex run)
                    :error (str error)})
        (end-reflex! eng run (if (= :cut status) :cut :failed) nil))
    (if (stopped-result? result)
      (do (emit! eng {:source :job :kind :stopped :level :warn :job (:id run) :reflex (:reflex run)
                      :data (dissoc result :dug) :text (stopped-text result)})
          (end-reflex! eng run :stopped (:reason result)))
      (end-reflex! eng run :done nil))))

(defn settle!
  "Book a finished round, unless it was cut (its token is no longer current)."
  [eng run outcome]
  (when (= (:token run) (:token (running eng)))
    (reset! (:running eng) nil)
    (free-owner! eng)
    (if (:reflex run)
      (let [outcome (if (= :continue (:status outcome))
                      (do (warn-continued! eng run) (assoc outcome :status :declined))
                      outcome)]
        (book-round! eng run outcome)
        (settle-reflex! eng run outcome))
      (do (note-fruitless! eng (:id run) (:status outcome) (get @(:rounds eng) (:id run)))
          (swap! (:rounds eng) dissoc (:id run))
          (settle-listed! eng run outcome)))
    (save-memory! eng))
  nil)

(defn start-round!
  "Give the body to instance id for one round; returns the round's promise."
  [eng id]
  (let [token (str "t" (swap! (:tokens eng) inc))
        _ (set-owner! eng token)
        _ (swap! (:rounds eng) assoc id backoff/empty-round)
        s (swap! (:state eng)
                 (fn [s] (let [inst (get-in s [:instances id])]
                           (cond-> (update-in s [:instances id :round] inc)
                             (not (:reflex inst)) (assoc :current id)
                             (= id (:resume s)) (assoc :resume nil)))))
        inst (get-in s [:instances id])
        run {:id id :token token :reflex (:reflex inst) :round (:round inst)}]
    (reset! (:running eng) run)
    (reset! (:activity eng) {:token token :id id :in-flight 0 :last-at (now eng)})
    (emit! eng (merge (job-fields eng id) {:source :job :kind :round_started :level :debug}))
    (-> (run-round eng run inst)
        (.then #(settle! eng run %)))))
