(ns engine.core.fruitless
  "Fruitless rounds: the act tracker of each round, the backoff of register entries (see engine.backoff)
  and the job.fruitless request of listed jobs whose every act fails."
  (:require [engine.core.base :refer [emit! job-fields now state stopped-result?]]
            [engine.core.attention :refer [request-attention! resolve-attention! same-request?]]
            [engine.backoff :as backoff]))

(def default-backoff-alert-ms
  "Least gap between two job.backoff warns of one job."
  300000)

(defn backoff-config
  "The backoff config of register entry rid (false: off): the engine's, then its trigger's default
  (triggers/defaults.edn), then the entry's own."
  [eng rid]
  (let [entry (some #(when (= rid (:id %)) %) (:register (state eng)))]
    (backoff/config (:backoff eng) (get-in eng [:triggers (:trigger entry) :backoff]) (:backoff entry))))

(defn backoff-fields [_eng k] {:source :reflex :reflex k})

(defn backoff-entries [eng] @(:backoffs eng))

(defn backoff-entry [eng k] (get @(:backoffs eng) k))

(defn reset-backoff!
  "Forget every backoff count and delay."
  [eng]
  (reset! (:backoffs eng) {})
  (reset! (:passes eng) {}))

(defn forget-backoff! [eng k]
  (swap! (:backoffs eng) dissoc k)
  (swap! (:passes eng) dissoc k))

(defn backing-off? [eng k]
  (backoff/backing-off? (backoff-entry eng k) (now eng)))

(defn pass-over?
  "Whether key k is backing off now; counts each pass that skips it."
  [eng k]
  (when (backing-off? eng k)
    (swap! (:passes eng) update k (fnil inc 0))
    true))

(defn alert-backoff! [eng k entry]
  (let [{:keys [act status reason]} (:last entry)]
    (emit! eng (merge (backoff-fields eng k)
                      {:kind :backoff :level :warn :act act :status status :reason reason
                       :delay-ms (:delay-ms entry) :fruitless (:fruitless entry)
                       :passes (get @(:passes eng) k 0) :since (:since entry)
                       :text (str (if (keyword? k) (name k) k) " backs off for "
                                  (/ (:delay-ms entry) 1000) " s after " (:fruitless entry)
                                  " fruitless rounds; last act " (some-> act name) ": " status
                                  (when reason (str " (" reason ")")))}))))

(defn fruitless!
  "Count one more fruitless run for reflex k; start the backoff or double
  its delay, warning when one is due. Returns the entry."
  [eng k cfg last]
  (let [t (now eng)
        entry (backoff/fruitless (backoff-entry eng k) cfg t last)
        due? (backoff/alert-due? entry t (:backoff-alert-ms eng))
        entry (cond-> entry due? (assoc :alerted t))]
    (swap! (:backoffs eng) assoc k entry)
    (when due? (alert-backoff! eng k entry))
    entry))

(defn fruitless-run?
  "Whether a reflex job's run (round, its act tracker) that ended with outcome counts toward its entry's backoff:
  no act made progress, and every act failed or the job gave up (declined, or stopped). A cut or failed run
  does not count."
  [round {:keys [status result]}]
  (let [{:keys [acts failed]} round]
    (and (not (#{:cut :error} status))
         (= acts failed)
         (or (pos? acts) (= :declined status) (stopped-result? result)))))

(defn book-round!
  "After a round of run ended with outcome: a fruitless run of a reflex job counts toward its register entry's
  backoff. Listed jobs have none. True when the entry is now backing off."
  [eng {:keys [id reflex]} outcome]
  (let [round (get @(:rounds eng) id)
        cfg (when reflex (backoff-config eng reflex))]
    (swap! (:rounds eng) dissoc id)
    (boolean
     (when (and cfg round (fruitless-run? round outcome))
       (backoff/backing-off? (fruitless! eng reflex cfg (:last round)) (now eng))))))

(defn recovered!
  "A progress act of key k: forget its backoff, and say so when it was backing off."
  [eng k]
  (let [entry (backoff-entry eng k)
        passes (get @(:passes eng) k 0)]
    (when entry
      (forget-backoff! eng k)
      (when (:since entry)
        (emit! eng (merge (backoff-fields eng k)
                          {:kind :recovered :level :info :fruitless (:fruitless entry)
                           :passes passes :since (:since entry)
                           :text (str (if (keyword? k) (name k) k) " made progress after "
                                      (:fruitless entry) " fruitless rounds; backoff over")}))))))

(defn record-act!
  "Book the result r of act k of the round of root (moved: blocks a moveTo moved
  the body): a failure status counts toward a fruitless round, any other status
  is progress and resets a reflex's backoff at once; a neutral act is neither."
  [eng {:keys [root reflex]} k r moved]
  (let [status (.-status r)]
    (swap! (:rounds eng) #(cond-> % (contains? % root) (update root backoff/note-act k status (.-reason r) moved)))
    (when (and reflex (not (or (backoff/failure? status) (backoff/neutral? k status moved))))
      (recovered! eng reflex))))

(def fruitless-rounds
  "Rounds in a row in which every act failed before a listed job's job.fruitless warn."
  3)

(defn note-fruitless!
  "After a round of listed job id that ended with status: count rounds in a row in which every act failed (round is
  its act tracker, see engine.backoff; a round that ends the job, is cut or fails is not counted). The third raises job.fruitless, a required attention request, once per spell; a round
  with progress ends the spell and resolves the request; a round with no act neither counts nor ends it. It only flags: nothing is held against the job."
  [eng id status round]
  (when-not (#{:cut :error :done} status)
    (if (backoff/fruitless-round? round)
      (let [n (get (swap! (:fruitless eng) update id (fnil inc 0)) id)]
        (when (= n fruitless-rounds)
          (let [{:keys [act status reason]} (:last round)
                text (str "every act failed in " n " rounds in a row; last " (some-> act name) ": " status
                          (when reason (str " (" reason ")")))]
            (request-attention! eng {:job-id id :reason :fruitless :kind :fruitless
                                     :context (select-keys (job-fields eng id) [:round :chain])
                                     :data {:rounds n :act act :status status :why reason}
                                     :message (str "Job " id " makes no progress: " text)}))))
      (when (and (pos? (:acts round 0)) (get @(:fruitless eng) id))
        (swap! (:fruitless eng) dissoc id)
        (doseq [[request-id request] (:attention (state eng))
                :when (same-request? request id :fruitless)]
          (resolve-attention! eng request-id :progress))))))
