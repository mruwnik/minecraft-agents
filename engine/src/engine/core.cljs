(ns engine.core
  "The scheduler: the job list (round-robin with preconditions and holding),
  the reflex register (ordered triggers with TTL mutes and moves), cuts by
  ownership-token rotation, and persistence of both to engine.edn.

  The state atom holds plain EDN, written on every change:
    :list [id]              listed instance ids, in cycle order
    :instances {id inst}    {:id :job :args :round :hold? :wake :not-before :blocked? :reflex}
    :register [entry]       {:id :trigger :job :args :persistence :cooldown-s :builtin?}
    :changes {rid {prop {:value v :until ms-or-nil}}}  prop is :mute or :position
    :reflex-state {rid {:cooldown-until ms :stopped? bool}}
    :cursor n               where the next round-robin scan starts
    :resume id              a cut listed job, next once the body is free
    :current id             the listed job whose round is in flight
    :pending-reflex id      a reflex job between its rounds; it keeps the body
    :next-id n
  The in-flight round itself ({:id :token :reflex :round}) is not persisted."
  (:require [engine.conditions :as conditions]
            [engine.events :as events]
            [engine.fsutil :as fsu]
            [engine.memory :as mem]
            ["path" :as path]))

(def default-min-recheck-ms
  "How long a :not-ready round with no wake keeps its job from being stepped again."
  5000)

(def empty-state
  {:list [] :instances {} :register [] :changes {} :reflex-state {}
   :cursor 0 :resume nil :current nil :pending-reflex nil :next-id 1})

;; ------------------------------------------------------------------ errors

(defn cut-error []
  (doto (js/Error. "cut: the ownership token changed") (aset "code" "cut")))

(defn cut? [e]
  (and (instance? js/Error e) (= "cut" (.-code e))))

;; ------------------------------------------------------------------ pure register

(defn live? [change now]
  (and (some? change) (or (nil? (:until change)) (< now (:until change)))))

(defn index-of-id [order id]
  (first (keep-indexed (fn [i e] (when (= id (:id e)) i)) order)))

(defn insert-relative
  "Insert entry above or below its anchor; at the bottom when the anchor is gone."
  [order entry {:keys [above below]}]
  (let [idx (index-of-id order (or above below))]
    (cond
      (nil? idx) (conj order entry)
      above (-> (subvec order 0 idx) (conj entry) (into (subvec order idx)))
      :else (-> (subvec order 0 (inc idx)) (conj entry) (into (subvec order (inc idx)))))))

(defn effective-register
  "The register in firing order at time now: live moves applied, muted entries removed."
  [{:keys [register changes]} now]
  (let [change (fn [e prop] (get-in changes [(:id e) prop]))
        moved? (fn [e] (live? (change e :position) now))
        muted? (fn [e] (live? (change e :mute) now))
        order (reduce (fn [order e] (insert-relative order e (:value (change e :position))))
                      (filterv (complement moved?) register)
                      (filter moved? register))]
    (filterv (complement muted?) order)))

(defn expired-changes [state now]
  (for [[rid props] (:changes state)
        [prop change] props
        :when (not (live? change now))]
    [rid prop change]))

(defn preempts?
  "Whether a firing entry takes the body from its holder ({:id :reflex} or nil)."
  [order holder entry]
  (let [position #(or (index-of-id order %) ##Inf)]
    (cond
      (nil? holder) true
      (nil? (:reflex holder)) true
      (= (:reflex holder) (:id entry)) false
      :else (< (position (:id entry)) (position (:reflex holder))))))

(defn remove-listed [state id]
  (let [idx (.indexOf (:list state) id)]
    (cond-> (-> state
                (update :list #(filterv (fn [x] (not= id x)) %))
                (update :instances dissoc id))
      (and (>= idx 0) (< idx (:cursor state))) (update :cursor dec)
      (= id (:resume state)) (assoc :resume nil)
      (= id (:current state)) (assoc :current nil))))

(defn restore
  "Saved state after a restart: the in-flight round is lost (its job resumes
  first) and reflex jobs are dropped."
  [saved]
  (let [s (merge empty-state saved)
        reflex-ids (keep (fn [[id inst]] (when (:reflex inst) id)) (:instances s))
        current (:current s)]
    (-> s
        (assoc :resume (if (some #{current} (:list s)) current (:resume s))
               :current nil
               :pending-reflex nil)
        (update :instances #(apply dissoc % reflex-ids)))))

(defn reflex-instance-ids [saved]
  (keep (fn [[id inst]] (when (:reflex inst) id)) (:instances saved)))

(defn normalize-result [r]
  (cond
    (#{:done :continue :not-ready} r) {:status r}
    (and (map? r) (#{:done :continue :not-ready} (:status r :continue))) (merge {:status :continue} r)
    :else {:status :error :error (js/Error. (str "a round returned " (pr-str r)))}))

;; ------------------------------------------------------------------ engine plumbing

(defn state [eng] @(:state eng))
(defn running [eng] @(:running eng))
(defn now [eng] ((:now eng)))

(defn job-def [eng job]
  (or (get-in eng [:catalog :jobs job])
      (throw (ex-info (str "unknown job " job) {:job job}))))

(defn trigger-def [eng trigger]
  (or (get-in eng [:catalog :triggers trigger])
      (throw (ex-info (str "unknown trigger " trigger) {:trigger trigger}))))

(defn emit! [eng event]
  (events/emit! (:events eng) event))

(defn job-fields [eng id]
  (let [inst (get-in (state eng) [:instances id])]
    {:job id :chain [id] :round (:round inst) :reflex (:reflex inst) :name (:job inst)}))

(defn set-owner! [eng token]
  (.setOwner (:primitives eng) token))

(defn memory-view [eng p]
  (let [d (mem/snapshot (:store eng))]
    {:common (:common d) :body (:body d) :job (mem/job (:store eng) p)}))

(defn new-id! [eng]
  (let [n (:next-id (state eng))]
    (swap! (:state eng) update :next-id inc)
    (str "j" n)))

(defn add-instance [state id job args opts]
  (assoc-in state [:instances id] (merge {:id id :job job :args (or args {}) :round 0} opts)))

(defn drop-instance! [eng id]
  (swap! (:state eng) #(cond-> (update % :instances dissoc id)
                         (= id (:pending-reflex %)) (assoc :pending-reflex nil)))
  (mem/delete-job! (:store eng) id))

;; ------------------------------------------------------------------ ctx and rounds

(declare submit! step-child)

(defn make-ctx
  "The ctx a round receives. base is {:id :path :chain :token :args :round :reflex}."
  [eng {:keys [id path chain token args round reflex] :as base}]
  (let [store (:store eng)
        p (:primitives eng)
        check! #(when-not (.isOwner p token) (throw (cut-error)))
        read (fn ([] (mem/job store path))
               ([scope] (if (= scope :job) (mem/job store path) (mem/scope store scope))))
        commit (fn ([m] (check!) (mem/commit-job! store path m))
                 ([scope m] (check!)
                  (if (= scope :job) (mem/commit-job! store path m) (mem/commit! store scope m))))]
    {:engine eng :primitives p :token token :args args :id id :path path
     :chain chain :round round :reflex reflex
     :memory {:get read :commit commit}
     :step-child (fn [slot job child-args] (step-child eng base slot job child-args))
     :submit (fn [job job-args opts] (check!) (submit! eng job job-args (assoc opts :by id)))
     :emit (fn [kind level fields]
             (emit! eng (merge fields {:source :job :kind kind :level level :job id
                                       :chain chain :round round :reflex reflex})))}))

(defn ^:async step-child
  "One round of the child job in slot under parent; see README.md. Resolves
  to the child's status keyword, or {:status :not-ready :wake w} when the
  child asked to wait for a wake condition."
  [eng parent slot job args]
  (let [store (:store eng)
        p (:primitives eng)
        child-path (conj (:path parent) (keyword slot))]
    (if (mem/done? store child-path)
      :done
      (let [def (job-def eng job)
            pre (if-let [f (:precondition def)] (f p (memory-view eng child-path) args) true)]
        (if-not (true? pre)
          :not-ready
          (let [id (mem/path->id child-path)
                c (make-ctx eng (assoc parent :id id :path child-path :args args
                                       :chain (conj (:chain parent) id)))
                {:keys [status error wake]} (normalize-result (await ((:round def) c)))]
            (when-not (.isOwner p (:token parent)) (throw (cut-error)))
            (when (= status :error) (throw error))
            (when (= status :done) (mem/mark-done! store child-path))
            (if wake {:status status :wake wake} status)))))))

(defn ^:async run-round [eng run inst]
  (let [def (job-def eng (:job inst))
        c (make-ctx eng {:id (:id run) :path [(:id run)] :chain [(:id run)] :token (:token run)
                         :args (:args inst) :round (:round run) :reflex (:reflex run)})]
    (try
      (normalize-result (await ((:round def) c)))
      (catch :default e
        (if (cut? e) {:status :cut :error e} {:status :error :error e})))))

;; ------------------------------------------------------------------ readiness

(defn call-guarded
  "Call f; on a throw emit a warn and return fallback. For user predicates."
  [eng what fallback f]
  (try
    (f)
    (catch :default e
      (emit! eng {:source :system :kind :error :level :warn :text (str what " threw: " e)})
      fallback)))

(defn backing-off?
  "Whether inst's last round was a :not-ready with no wake, and the re-check interval has not passed."
  [eng inst]
  (some-> (:not-before inst) (> (now eng))))

(defn readiness [eng inst]
  (let [def (job-def eng (:job inst))
        p (:primitives eng)
        view (memory-view eng [(:id inst)])]
    (cond
      (backing-off? eng inst) :not-yet
      (:wake inst) (if (conditions/holds? (:wake inst) p view (now eng)) true :not-yet)
      (:precondition def) (call-guarded eng (str "precondition of " (:job inst)) false
                                        #((:precondition def) p view (:args inst)))
      :else true)))

(defn ready?
  "Whether listed instance id may run now; emits job.blocked once per spell of false."
  [eng id]
  (let [inst (get-in (state eng) [:instances id])
        r (readiness eng inst)]
    (cond
      (true? r)
      (do (when (:blocked? inst) (swap! (:state eng) assoc-in [:instances id :blocked?] false))
          true)

      (false? r)
      (do (when-not (:blocked? inst)
            (swap! (:state eng) assoc-in [:instances id :blocked?] true)
            (emit! eng (merge (job-fields eng id)
                              {:source :job :kind :blocked :level :warn
                               :text (str (name (:job inst)) " cannot run: its precondition is false")})))
          false)

      :else false)))

(defn choose-listed
  "The listed job to run next: a holder, else the cut job, else round-robin."
  [eng]
  (let [{:keys [list instances resume cursor]} (state eng)
        n (count list)
        holder (some #(when (:hold? (instances %)) %) list)]
    (cond
      holder (when-not (backing-off? eng (instances holder)) holder)
      (and resume (some #{resume} list) (ready? eng resume)) resume
      (zero? n) nil
      :else (some (fn [i] (let [id (nth list (mod (+ cursor i) n))] (when (ready? eng id) id)))
                  (range n)))))

;; ------------------------------------------------------------------ settling a round

(defn settle-listed! [eng {:keys [id]} {:keys [status error wake]}]
  (let [idx (.indexOf (:list (state eng)) id)
        not-before (when (and (= :not-ready status) (nil? wake))
                     (+ (now eng) (:min-recheck-ms eng default-min-recheck-ms)))]
    (case status
      :done
      (do (swap! (:state eng) #(assoc (remove-listed % id) :cursor (max idx 0)))
          (mem/delete-job! (:store eng) id)
          (emit! eng {:source :job :kind :completed :level :info :job id :chain [id]}))

      :cut
      (do (swap! (:state eng) assoc :resume id :current nil)
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :cut :level :info :by :primitives
                             :text "a primitive rejected with cut; the job stays listed"})))

      :error
      (do (swap! (:state eng) #(assoc (remove-listed % id) :cursor (max idx 0)))
          (mem/delete-job! (:store eng) id)
          (emit! eng {:source :job :kind :failed :level :warn :job id :chain [id]
                      :error (str error) :text (str "dropped: " error)}))

      (do (swap! (:state eng) #(-> %
                                   (assoc :cursor (inc idx) :current nil)
                                   (assoc-in [:instances id :wake] wake)
                                   (assoc-in [:instances id :not-before] not-before)))
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :yielded :level :info :status status :wake wake}))))))

(defn trigger-holds?
  "Whether entry's trigger holds: (:when world memory args), where memory is
  {:common :body :now ms} and args are the entry's args."
  [eng entry world memory]
  (let [t (trigger-def eng (:trigger entry))
        memory (assoc memory :now (now eng))]
    (boolean (call-guarded eng (str "trigger " (:trigger entry)) false
                           #((:when t) world memory (:args entry))))))

(defn end-reflex!
  "A reflex job ended on its own; classify and apply the entry's persistence."
  [eng {:keys [id reflex]}]
  (drop-instance! eng id)
  (let [entry (some #(when (= reflex (:id %)) %) (:register (state eng)))
        d (mem/snapshot (:store eng))
        still? (and entry (trigger-holds? eng entry (:primitives eng) (select-keys d [:common :body])))]
    (when still?
      (case (:persistence entry)
        :cooldown (swap! (:state eng) assoc-in [:reflex-state reflex :cooldown-until]
                         (+ (now eng) (* 1000 (:cooldown-s entry 0))))
        :stop (swap! (:state eng) assoc-in [:reflex-state reflex :stopped?] true)
        nil))
    (emit! eng {:source :reflex :kind :ended :level :info :reflex reflex :job id
                :how (if still? :completed_not_cleared :cleared)})))

(defn settle-reflex! [eng run {:keys [status error]}]
  (case status
    :continue (swap! (:state eng) assoc :pending-reflex (:id run))
    (:error :cut)
    (do (emit! eng {:source :job :kind :failed :level :warn :job (:id run) :reflex (:reflex run)
                    :error (str error)})
        (end-reflex! eng run))
    (end-reflex! eng run)))

(defn settle!
  "Book a finished round, unless it was cut (its token is no longer current)."
  [eng run outcome]
  (when (= (:token run) (:token (running eng)))
    (reset! (:running eng) nil)
    (set-owner! eng nil)
    (if (:reflex run)
      (settle-reflex! eng run outcome)
      (settle-listed! eng run outcome)))
  nil)

(defn start-round!
  "Give the body to instance id for one round; returns the round's promise."
  [eng id]
  (let [token (str "t" (swap! (:tokens eng) inc))
        _ (set-owner! eng token)
        s (swap! (:state eng)
                 (fn [s] (let [inst (get-in s [:instances id])]
                           (cond-> (-> s
                                       (update-in [:instances id :round] inc)
                                       (assoc-in [:instances id :wake] nil)
                                       (assoc-in [:instances id :not-before] nil))
                             (not (:reflex inst)) (assoc :current id)
                             (= id (:resume s)) (assoc :resume nil)
                             (= id (:pending-reflex s)) (assoc :pending-reflex nil)))))
        inst (get-in s [:instances id])
        run {:id id :token token :reflex (:reflex inst) :round (:round inst)}]
    (reset! (:running eng) run)
    (emit! eng (merge (job-fields eng id) {:source :job :kind :round_started :level :info}))
    (-> (run-round eng run inst)
        (.then #(settle! eng run %)))))

;; ------------------------------------------------------------------ cuts and firing

(defn holder
  "Who has the body: the running round, or a reflex job between its rounds."
  [eng]
  (or (running eng)
      (when-let [id (:pending-reflex (state eng))]
        {:id id :reflex (get-in (state eng) [:instances id :reflex])})))

(defn cut!
  "Take the body from holder h for entry; cause is the seq of the firing."
  [eng h by cause]
  (set-owner! eng nil)
  (reset! (:running eng) nil)
  (if (:reflex h)
    (do (drop-instance! eng (:id h))
        (emit! eng {:source :reflex :kind :ended :level :info :reflex (:reflex h) :job (:id h)
                    :how :dropped :by by :cause cause}))
    (do (swap! (:state eng) assoc :resume (:id h) :current nil)
        (emit! eng (merge (job-fields eng (:id h))
                          {:source :job :kind :cut :level :info :by by :cause cause})))))

(defn eligible? [s entry t]
  (let [{:keys [cooldown-until stopped?]} (get-in s [:reflex-state (:id entry)])]
    (and (not stopped?) (or (nil? cooldown-until) (>= t cooldown-until)))))

(defn evaluate-register!
  "Evaluate every trigger in order; clear :stop latches whose condition is
  false; return the first entry that holds and is eligible."
  [eng order]
  (let [p (:primitives eng)
        memory (select-keys (mem/snapshot (:store eng)) [:common :body])
        results (mapv (fn [e] [e (trigger-holds? eng e p memory)]) order)
        t (now eng)]
    (doseq [[e holds] results
            :when (and (not holds) (get-in (state eng) [:reflex-state (:id e) :stopped?]))]
      (swap! (:state eng) update-in [:reflex-state (:id e)] dissoc :stopped?))
    (some (fn [[e holds]] (when (and holds (eligible? (state eng) e t)) e)) results)))

(defn fire! [eng entry h]
  (let [id (new-id! eng)
        fired (emit! eng {:source :reflex :kind :fired :level :info :reflex (:id entry)
                          :job id :interrupted (:id h)})]
    (when h (cut! eng h (:id entry) fired))
    (swap! (:state eng) add-instance id (:job entry) (:args entry) {:reflex (:id entry)})
    (start-round! eng id)))

(defn expire-changes! [eng]
  (doseq [[rid prop change] (expired-changes (state eng) (now eng))]
    (swap! (:state eng) (fn [s] (let [s (update-in s [:changes rid] dissoc prop)]
                                  (if (empty? (get-in s [:changes rid]))
                                    (update s :changes dissoc rid)
                                    s))))
    (emit! eng {:source :reflex :kind :reverted :level :info :reflex rid :property prop
                :from (:value change) :to :default})))

(defn tick!
  "One scheduling pass. Synchronous; returns the promise of a round it
  started (resolving once that round is settled), or nil."
  [eng]
  (expire-changes! eng)
  (let [order (effective-register (state eng) (now eng))
        firing (evaluate-register! eng order)
        h (holder eng)]
    (cond
      (and firing (preempts? order h firing)) (fire! eng firing h)
      (running eng) nil
      (:pending-reflex (state eng)) (start-round! eng (:pending-reflex (state eng)))
      :else (when-let [id (choose-listed eng)] (start-round! eng id)))))

;; ------------------------------------------------------------------ list edits (agents, and submit from rounds)

(defn submit!
  "Put a new instance of job on the list. opts: :hold? :front? :by. Returns its id."
  [eng job args {:keys [hold? front? by]}]
  (job-def eng job)
  (let [id (new-id! eng)]
    (swap! (:state eng) #(cond-> (-> %
                                     (add-instance id job args {:hold? (boolean hold?)})
                                     (update :list (fn [l] (if front? (into [id] l) (conj l id)))))
                           (and front? (pos? (count (:list %)))) (update :cursor inc)))
    (emit! eng {:source :job :kind :queued :level :info :job id :chain [id] :name job
                :args args :hold (boolean hold?) :by by})
    id))

(defn cancel! [eng id]
  (when (= id (:id (running eng)))
    (set-owner! eng nil)
    (reset! (:running eng) nil))
  (swap! (:state eng) remove-listed id)
  (mem/delete-job! (:store eng) id)
  (emit! eng {:source :job :kind :cancelled :level :info :job id :chain [id] :by :agent}))

(defn do-now!
  "Cut a running listed job and put job at the front, holding the body."
  [eng job args]
  (let [r (running eng)]
    (when (and r (not (:reflex r)))
      (cut! eng r :do-now nil))
    (submit! eng job args {:hold? true :front? true :by :agent})))

;; ------------------------------------------------------------------ register edits (agents only)

(defn entry-from
  "A register entry from a spec {:trigger ...overrides}, filled from the trigger."
  [eng spec]
  (let [t (trigger-def eng (:trigger spec))]
    (merge {:id (:trigger spec) :trigger (:trigger spec) :job (:job t)
            :persistence (:persistence t :retry) :cooldown-s (:cooldown-s t 0) :builtin? false}
           (select-keys spec [:id :job :persistence :cooldown-s :builtin?])
           {:args (merge (:args t {}) (:args spec))})))

(defn register-reflex!
  "Append a reflex to the register. Returns its id."
  [eng spec]
  (let [entry (entry-from eng spec)]
    (job-def eng (:job entry))
    (when (index-of-id (:register (state eng)) (:id entry))
      (throw (ex-info (str "reflex already registered: " (:id entry)) {:id (:id entry)})))
    (swap! (:state eng) update :register conj entry)
    (emit! eng {:source :reflex :kind :changed :level :info :reflex (:id entry)
                :property :registered :value (:job entry) :by :agent})
    (:id entry)))

(defn remove-reflex!
  "Remove a reflex; refused (false) for built-ins."
  [eng id]
  (let [entry (some #(when (= id (:id %)) %) (:register (state eng)))]
    (cond
      (nil? entry) false
      (:builtin? entry)
      (do (emit! eng {:source :reflex :kind :refused :level :warn :reflex id
                      :text "built-in reflexes can be muted or moved, not removed"})
          false)
      :else
      (do (swap! (:state eng) #(-> %
                                   (update :register (fn [r] (filterv (fn [e] (not= id (:id e))) r)))
                                   (update :changes dissoc id)
                                   (update :reflex-state dissoc id)))
          (emit! eng {:source :reflex :kind :changed :level :info :reflex id
                      :property :registered :value nil :by :agent})
          true))))

(defn change!
  "Put property prop of reflex id under one change, replacing any earlier one."
  [eng id prop value ttl-s]
  (when-not (index-of-id (:register (state eng)) id)
    (throw (ex-info (str "no such reflex " id) {:id id})))
  (swap! (:state eng) assoc-in [:changes id prop]
         {:value value :until (when ttl-s (+ (now eng) (* 1000 ttl-s)))})
  (emit! eng {:source :reflex :kind :changed :level :info :reflex id :property prop
              :value value :ttl ttl-s :by :agent}))

(defn mute! [eng id ttl-s] (change! eng id :mute true ttl-s))

(defn move!
  "where is {:above other-id} or {:below other-id}."
  [eng id where ttl-s]
  (change! eng id :position where ttl-s))

(defn clear-change! [eng id prop]
  (swap! (:state eng) update-in [:changes id] dissoc prop)
  (emit! eng {:source :reflex :kind :reverted :level :info :reflex id :property prop
              :to :default :by :agent}))

;; ------------------------------------------------------------------ scenario, lifecycle

(defn load-scenario!
  "Register a scenario's reflexes and queue its jobs, in order."
  [eng {:keys [register queue]}]
  (doseq [spec register] (register-reflex! eng spec))
  (doseq [{:keys [job args hold?]} queue]
    (submit! eng job args {:hold? hold? :by :scenario})))

(defn self-pos [p]
  (let [pos (.-pos (.self p))]
    {:x (.-x pos) :y (.-y pos) :z (.-z pos)}))

(defn record-body-event! [eng e]
  (let [m (js->clj e :keywordize-keys true)]
    (mem/add-record! (:store eng) (assoc m :t (now eng)))
    (emit! eng (merge (dissoc m :kind)
                      {:source :body :kind (keyword (:kind m))
                       :level (if (= "died" (:kind m)) :error :info)}))))

(defn create
  "An engine over primitives p with state under dir. Restores engine.edn and
  memory when present. Options: :primitives :catalog :dir :now :events :body :min-recheck-ms."
  [{:keys [primitives catalog dir now events body min-recheck-ms] :or {now js/Date.now}}]
  (let [file (path/join dir "engine.edn")
        saved (fsu/read-edn file)
        username (or body (.-username (.self primitives)))
        ev (or events (events/make {:body username :file (path/join dir "events.jsonl") :stdout? true
                                    :now now :pos-fn #(self-pos primitives)}))
        eng {:primitives primitives :catalog catalog :dir dir :now now :events ev
             :min-recheck-ms (or min-recheck-ms default-min-recheck-ms)
             :store (mem/open dir)
             :state (atom (if saved (restore saved) empty-state))
             :running (atom nil)
             :tokens (atom 0)}]
    (doseq [id (when saved (reflex-instance-ids saved))]
      (mem/delete-job! (:store eng) id))
    (add-watch (:state eng) ::persist (fn [_ _ old new] (when (not= old new) (fsu/write-edn! file new))))
    (fsu/write-edn! file (state eng))
    (set-owner! eng nil)
    (.onBodyEvent primitives #(record-body-event! eng %))
    (mem/add-record! (:store eng) {:kind "restart" :t (now)})
    (emit! eng {:source :system :kind (if saved :restored :started) :level :info
                :list (:list (state eng)) :register (mapv :id (:register (state eng)))})
    eng))

(defn shutdown!
  "Release the body for a shutdown. The in-flight round is cut by token
  rotation and its outcome is never booked, so its job stays on the list
  (persisted as :current, resumed first after a restart) with its memory."
  [eng]
  (set-owner! eng nil)
  (reset! (:running eng) nil)
  (emit! eng {:source :system :kind :stopping :level :info :job (:current (state eng))}))

(defn start!
  "Tick every tick-ms until the returned stop fn is called."
  [eng {:keys [tick-ms] :or {tick-ms 250}}]
  (let [stopped (atom false)]
    (letfn [(step []
              (when-not @stopped
                (try
                  (tick! eng)
                  (catch :default e
                    (emit! eng {:source :system :kind :error :level :error :text (str "tick failed: " e)})))
                (js/setTimeout step tick-ms)))]
      (step))
    #(reset! stopped true)))
