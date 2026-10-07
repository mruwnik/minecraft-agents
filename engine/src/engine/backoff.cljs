(ns engine.backoff
  "Backoff for jobs whose rounds keep failing at once: pure functions; the
  wiring is in engine.core.fruitless. See README.md, Backoff.

  A round is fruitless when it ran at least one act and every act failed
  (a failure status below). After :after fruitless rounds in a row the job gets
  no round for :first-s seconds; each further fruitless round doubles the wait,
  up to :max-s. Any act with another status is progress and resets everything,
  except neutral acts: they count for neither."
  (:require [engine.settings :as settings]))

(def settings
  {:engine.backoff/after {:default 3 :type :int :min 1 :doc "Fruitless rounds in a row before a job gets no round."}
   :engine.backoff/first-s {:default 1 :type :number :min 0.001 :doc "The first wait, s; each further fruitless round doubles it."}
   :engine.backoff/max-s {:default 30 :type :number :min 0.001 :doc "The longest wait, s."}
   :engine.backoff/moved-min {:default 1 :type :number :min 0 :doc "Blocks a failed moveTo must have moved the body to be neutral."}
   :engine.backoff/walk-moved-min {:default 8 :type :number :min 0
                                   :doc "Blocks a walk round that got no nearer must have moved the body to be neutral: a long way round is not a failure, shuffling on the spot is."}})

(defn defaults
  "The engine-wide backoff {:after :first-s :max-s} as set."
  []
  {:after (settings/get settings :engine.backoff/after)
   :first-s (settings/get settings :engine.backoff/first-s)
   :max-s (settings/get settings :engine.backoff/max-s)})

(def failure-statuses
  "Act statuses that count as a failure; every other status is progress."
  #{"blocked" "failed" "unreachable" "cannot" "no-effect" "timeout" "gone" "out-of-reach" "no-item"
    "no-support" "no-headroom" "occupied" "full" "disconnected" "unsupported" "not-night"
    "monsters-near" "unchanged" "no-room" "missing"})

(defn failure? [status]
  (contains? failure-statuses status))

(def neutral-acts
  "Acts that neither count toward a fruitless round nor reset a backoff. A
  steer is one leg of a walk round (engine.path.near), which is booked whole
  as a :walk; breathe's steer that holds the body still is a wait."
  #{:look :wait :equip :steer})

(defn neutral?
  "Whether an act is neutral (counts for neither side). Neutral when:
  - the act is in neutral-acts,
  - it is a failed moveTo that moved the body at least moved-min blocks, or
  - it is a :walk round (booked by engine.path.near) that ended nearer
    (\"partial\") or moved the body at least walk-moved-min blocks.
  An arrival is progress. A walk with no path, or one that got nowhere, is a failure."
  [act status moved]
  (or (contains? neutral-acts act)
      (and (= :moveTo act) (failure? status) (some? moved) (>= moved (settings/get settings :engine.backoff/moved-min)))
      (and (= :walk act)
           (or (= "partial" status)
               (and (failure? status) (some? moved) (>= moved (settings/get settings :engine.backoff/walk-moved-min)))))))

(defn validate!
  "Throws unless cfg is nil, false (off) or a map of :after :first-s :max-s to positive numbers."
  [cfg]
  (when-not (or (nil? cfg) (false? cfg)
                (and (map? cfg)
                     (every? (fn [[k v]] (and (contains? (defaults) k) (number? v) (pos? v))) cfg)))
    (throw (ex-info (str ":backoff must be false or a map of " (pr-str (keys (defaults)))
                         " to positive numbers, not " (pr-str cfg))
                    {:backoff cfg}))))

(defn config
  "The effective config of levels, least specific first (engine, job def, entry);
  nil levels are skipped, a map is merged over what is below, false means off
  (returned as false)."
  [& levels]
  (reduce (fn [cfg l]
            (cond (false? l) false
                  (map? l) (merge (or cfg (defaults)) l)
                  :else cfg))
          (defaults) levels))

;; ------------------------------------------------------------------ one round

(def empty-round {:acts 0 :failed 0 :last nil})

(defn note-act
  "The round after an act with status (and reason, when the result has one);
  moved is how far a moveTo or a walk round moved the body, else nil. A neutral act leaves it as is."
  [round act status reason moved]
  (if (neutral? act status moved)
    round
    (cond-> (update round :acts inc)
      (failure? status) (-> (update :failed inc)
                            (assoc :last {:act act :status status :reason reason})))))

(defn fruitless-round? [{:keys [acts failed]}]
  (and (pos? acts) (= acts failed)))

;; ------------------------------------------------------------------ the entry

(defn stale?
  "Whether the backoff of entry ended :max-s or more ago: it is over and must not count against a new failure."
  [entry max-s now]
  (boolean (some-> (:until entry) (+ (* 1000 max-s)) (<= now))))

(defn fruitless
  "The backoff entry (nil: none yet) after one more fruitless round at now: the
  count, and once it reaches :after the wait, doubled each time up to :max-s. An entry whose backoff ended
  :max-s ago starts over."
  [entry {:keys [after first-s max-s]} now last]
  (let [entry (when-not (stale? entry max-s now) entry)
        n (inc (:fruitless entry 0))
        e (assoc entry :fruitless n :last last)]
    (if (< n after)
      e
      (let [d (min (* 1000 max-s) (if-let [d (:delay-ms entry)] (* 2 d) (* 1000 first-s)))]
        (assoc e :delay-ms d :until (+ now d) :since (or (:since entry) now))))))

(defn backing-off? [entry now]
  (boolean (some-> (:until entry) (> now))))

(defn alert-due?
  "Whether a backoff alert is due: the first one when the backoff starts, then
  one per alert-ms."
  [entry now alert-ms]
  (and (some? (:since entry))
       (or (nil? (:alerted entry)) (>= (- now (:alerted entry)) alert-ms))))
