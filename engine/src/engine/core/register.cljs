(ns engine.core.register
  "The reflex register: the effective order (TTL mutes and moves), trigger evaluation and eligibility,
  and the agent's edits of entries."
  (:require [engine.core.base :refer [call-guarded emit! now state trigger-def]]
            [engine.core.fruitless :refer [backoff-entry pass-over?]]
            [engine.backoff :as backoff]
            [engine.expr :as expr]
            [engine.memory :as mem]))

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

(defn trigger-holds?
  "Whether entry's trigger holds: (:when world view args plans live), where view is a
  memory view {:data :now} (see engine.memory), args are the entry's args, plans is the
  engine's jobs.lib.world-files (a trigger may ignore it) and live is the set of ids of
  the jobs still listed (running, queued or paused; not the parked failed ones)."
  [eng entry world view]
  (let [t (trigger-def eng (:trigger entry))
        st (state eng)
        live (into #{} (remove #(contains? (:failed st) %)) (:list st))]
    (boolean (call-guarded eng (str "trigger " (:trigger entry)) false
                           #((:when t) world view (:args entry) (:world eng) live)))))

(defn eligible?
  "Whether entry may fire at t: not stopped, past its cooldown, not backing off
  (backoff-entry is its entry in the engine's backoffs, or nil)."
  [s entry backoff-entry t]
  (let [{:keys [cooldown-until stopped?]} (get-in s [:reflex-state (:id entry)])]
    (and (not stopped?)
         (or (nil? cooldown-until) (>= t cooldown-until))
         (not (backoff/backing-off? backoff-entry t)))))

(defn evaluate-register!
  "Evaluate every trigger in order; clear :stop latches whose condition is
  false; return the first entry that holds and is eligible."
  [eng order]
  (let [p (:primitives eng)
        view (mem/view (:store eng))
        results (mapv (fn [e] [e (trigger-holds? eng e p view)]) order)
        t (now eng)]
    (doseq [[e holds] results
            :when (and (not holds) (get-in (state eng) [:reflex-state (:id e) :stopped?]))]
      (swap! (:state eng) update-in [:reflex-state (:id e)] dissoc :stopped?))
    (doseq [[e holds] results :when holds]
      (pass-over? eng (:id e)))
    (some (fn [[e holds]] (when (and holds (eligible? (state eng) e (backoff-entry eng (:id e)) t)) e)) results)))

(defn expire-changes! [eng]
  (doseq [[rid prop change] (expired-changes (state eng) (now eng))]
    (swap! (:state eng) (fn [s] (let [s (update-in s [:changes rid] dissoc prop)]
                                  (if (empty? (get-in s [:changes rid]))
                                    (update s :changes dissoc rid)
                                    s))))
    (emit! eng {:source :reflex :kind :reverted :level :info :reflex rid :property prop
                :from (:value change) :to :default})))

(defn entry-from
  "A register entry from a spec {:trigger ...overrides}, filled from the
  trigger. :job is a job spec (an expression without hold); :args are the
  trigger's, merged over its defaults."
  [eng spec]
  (let [t (trigger-def eng (:trigger spec))]
    (merge {:id (:trigger spec) :trigger (:trigger spec) :job (:job t)
            :persistence (:persistence t :retry) :cooldown-s (:cooldown-s t 0) :builtin? false}
           (select-keys spec [:id :job :persistence :cooldown-s :builtin? :backoff])
           {:args (merge (:args t {}) (:args spec))})))

(defn register-reflex!
  "Append a reflex to the register. Returns its id."
  [eng spec]
  (let [entry (entry-from eng spec)]
    (expr/parse (:jobs eng) (:job entry))
    (backoff/validate! (:backoff entry))
    (when (index-of-id (:register (state eng)) (:id entry))
      (throw (ex-info (str "reflex already registered: " (:id entry)) {:id (:id entry)})))
    (swap! (:state eng) update :register conj entry)
    (emit! eng {:source :reflex :kind :changed :level :info :reflex (:id entry)
                :property :registered :value (pr-str (:job entry)) :by :agent})
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
