(ns engine.backoff
  "Backoff for jobs whose rounds keep failing at once: pure functions; the
  wiring is in engine.core.fruitless. See README.md, Backoff.

  A round is fruitless when it ran at least one act and every act failed. Only a
  progress status (the :engine.backoff/progress setting, or one a trigger or entry
  declares) is progress and resets everything; neutral acts and statuses count for
  neither; any other status is a failure. After :after fruitless rounds in a row the
  job gets no round for :first-s seconds; each further fruitless round doubles the
  wait, up to :max-s."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {:engine.backoff/after {:default 3 :spec (a/int-in 1 nil) :doc "Fruitless rounds in a row before a job gets no round."}
   :engine.backoff/first-s {:default 1 :spec (a/num-in 0.001 nil) :doc "The first wait, s; each further fruitless round doubles it."}
   :engine.backoff/max-s {:default 30 :spec (a/num-in 0.001 nil) :doc "The longest wait, s."}
   :engine.backoff/moved-min {:default 1 :spec (a/num-in 0 nil) :doc "Blocks a failed moveTo must have moved the body to be neutral."}
   :engine.backoff/walk-moved-min {:default 8 :spec (a/num-in 0 nil)
                                   :doc "Blocks a walk round that got no nearer must have moved the body to be neutral: a long way round is not a failure, shuffling on the spot is."}
   :engine.backoff/progress {:default #{"ok" "done" "success" "arrived" "partial" "landed" "surfaced" "placed" "dug"
                                        "used" "crafted" "bought" "enchanted" "collected" "tossed" "equipped" "empty"
                                        "ate" "hit" "killed" "mounted" "already-mounted" "dismounted" "sleeping" "sent"}
                             :spec (a/set-of a/name?)
                             :doc "Act statuses that are progress; any status neither progress nor neutral is a failure."}
   :engine.backoff/neutral {:default #{"cut" "closed"} :spec (a/set-of a/name?)
                            :doc "Act statuses that count for neither side."}})

(def status-keys
  "The status classes a :backoff declaration may set, each a set of act statuses."
  [:progress :failure :neutral])

(defn defaults
  "The engine-wide backoff {:after :first-s :max-s} as set."
  []
  {:after (settings/get settings :engine.backoff/after)
   :first-s (settings/get settings :engine.backoff/first-s)
   :max-s (settings/get settings :engine.backoff/max-s)})

(defn declared-class
  "The class cfg's :progress :failure :neutral sets declare status in, else nil."
  [cfg status]
  (some #(when (contains? (get cfg %) status) %) status-keys))

(defn status-class
  "The class of act status, :progress :neutral or :failure: as cfg's declared :progress :failure :neutral sets say
  (config keeps them apart, the most specific level winning), else the settings' progress and neutral sets; any
  other status is a failure."
  [cfg status]
  (or (declared-class cfg status)
      (cond (contains? (settings/get settings :engine.backoff/neutral) status) :neutral
            (contains? (settings/get settings :engine.backoff/progress) status) :progress
            :else :failure)))

(defn failure?
  ([status] (failure? nil status))
  ([cfg status] (= :failure (status-class cfg status))))

(def neutral-acts
  "Acts that neither count toward a fruitless round nor reset a backoff. A
  steer is one leg of a walk round (engine.path.near), which is booked whole
  as a :walk; breathe's steer that holds the body still is a wait."
  #{:look :wait :equip :steer})

(defn neutral?
  "Whether an act is neutral (counts for neither side). Neutral when:
  - the act is in neutral-acts or its status is neutral (status-class under cfg),
  - it is a failed moveTo that moved the body at least moved-min blocks, or
  - it is a :walk round (booked by engine.path.near) that ended nearer
    (\"partial\") or moved the body at least walk-moved-min blocks.
  An arrival is progress. A walk with no path, or one that got nowhere, is a failure."
  ([act status moved] (neutral? act status moved nil))
  ([act status moved cfg]
   (or (contains? neutral-acts act)
       (= :neutral (status-class cfg status))
       (and (= :moveTo act) (failure? cfg status) (some? moved) (>= moved (settings/get settings :engine.backoff/moved-min)))
       (and (= :walk act)
            (or (and (= "partial" status) (nil? (declared-class cfg status)))
                (and (failure? cfg status) (some? moved) (>= moved (settings/get settings :engine.backoff/walk-moved-min))))))))

(defn validate!
  "Throws unless cfg is nil, false (off) or a map of :after :first-s :max-s to positive numbers and of
  :progress :failure :neutral to sets of act statuses (strings), no status in two of them."
  [cfg]
  (when-not (or (nil? cfg) (false? cfg)
                (and (map? cfg)
                     (every? (fn [[k v]] (if (some #{k} status-keys)
                                           (and (set? v) (every? string? v))
                                           (and (contains? (defaults) k) (number? v) (pos? v))))
                             cfg)
                     (let [all (mapcat #(get cfg %) status-keys)]
                       (= (count all) (count (set all))))))
    (throw (ex-info (str ":backoff must be false or a map of " (pr-str (keys (defaults)))
                         " to positive numbers and of " (pr-str status-keys) " to sets of statuses, not " (pr-str cfg))
                    {:backoff cfg}))))

(defn merge-level
  "cfg with level l merged over it: a status l declares leaves the classes it had below."
  [cfg l]
  (reduce (fn [c k]
            (if-let [declared (get l k)]
              (reduce (fn [c other] (cond-> c (contains? c other) (update other #(reduce disj % declared))))
                      (update c k (fnil into #{}) declared)
                      (remove #{k} status-keys))
              c))
          (merge cfg (apply dissoc l status-keys))
          status-keys))

(defn config
  "The effective config of levels, least specific first (engine, job def, entry);
  nil levels are skipped, a map is merged over what is below, false means off
  (returned as false). A map of only status classes leaves it off; one with :after :first-s or :max-s turns it on."
  [& levels]
  (reduce (fn [cfg l]
            (cond (false? l) false
                  (and (false? cfg) (not-any? #(contains? l %) (keys (defaults)))) false
                  (map? l) (merge-level (or cfg (defaults)) l)
                  :else cfg))
          (defaults) levels))

;; ------------------------------------------------------------------ one round

(def empty-round {:acts 0 :failed 0 :last nil})

(defn note-act
  "The round after an act with status (and reason, when the result has one), classed under cfg (the backoff
  config, nil: the settings); moved is how far a moveTo or a walk round moved the body, else nil. A neutral act
  leaves it as is."
  ([round act status reason moved] (note-act round act status reason moved nil))
  ([round act status reason moved cfg]
   (if (neutral? act status moved cfg)
     round
     (cond-> (update round :acts inc)
       (failure? cfg status) (-> (update :failed inc)
                                 (assoc :last {:act act :status status :reason reason}))))))

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
