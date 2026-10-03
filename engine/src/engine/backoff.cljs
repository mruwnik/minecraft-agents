(ns engine.backoff
  "Backoff for jobs whose rounds keep failing at once: pure functions; the
  wiring is in engine.core. See README.md, Backoff.

  A round is fruitless when it ran at least one act and every act failed
  (a failure status below). After :after fruitless rounds in a row the job gets
  no round for :first-s seconds; each further fruitless round doubles the wait,
  up to :max-s. Any act with another status is progress and resets everything,
  except neutral acts: they count for neither.")

(def defaults {:after 3 :first-s 1 :max-s 30})

(def failure-statuses
  "Act statuses that count as a failure; every other status is progress."
  #{"blocked" "failed" "unreachable" "cannot" "no-effect" "timeout" "gone" "out-of-reach" "no-item"
    "no-support" "no-headroom" "occupied" "full" "disconnected" "unsupported" "not-night"
    "monsters-near"})

(defn failure? [status]
  (contains? failure-statuses status))

(def neutral-acts
  "Acts that neither count toward a fruitless round nor reset a backoff."
  #{:look :wait :equip})

(def moved-min
  "Blocks a failed walk must have moved the body to be neutral."
  1)

(defn neutral?
  "Whether act with status is neutral: a neutral act, or a failed moveTo that
  moved the body (straight line, blocks) at least moved-min."
  [act status moved]
  (or (contains? neutral-acts act)
      (and (= :moveTo act) (failure? status) (some? moved) (>= moved moved-min))))

(defn validate!
  "Throws unless cfg is nil, false (off) or a map of :after :first-s :max-s to positive numbers."
  [cfg]
  (when-not (or (nil? cfg) (false? cfg)
                (and (map? cfg)
                     (every? (fn [[k v]] (and (contains? defaults k) (number? v) (pos? v))) cfg)))
    (throw (ex-info (str ":backoff must be false or a map of " (pr-str (keys defaults))
                         " to positive numbers, not " (pr-str cfg))
                    {:backoff cfg}))))

(defn config
  "The effective config of levels, least specific first (engine, job def, entry);
  nil levels are skipped, a map is merged over what is below, false means off
  (returned as false)."
  [& levels]
  (reduce (fn [cfg l]
            (cond (false? l) false
                  (map? l) (merge (or cfg defaults) l)
                  :else cfg))
          defaults levels))

;; ------------------------------------------------------------------ one round

(def empty-round {:acts 0 :failed 0 :last nil})

(defn note-act
  "The round after an act with status (and reason, when the result has one);
  moved is how far a moveTo moved the body, else nil. A neutral act leaves it as is."
  [round act status reason moved]
  (if (neutral? act status moved)
    round
    (cond-> (update round :acts inc)
      (failure? status) (-> (update :failed inc)
                            (assoc :last {:act act :status status :reason reason})))))

(defn fruitless-round? [{:keys [acts failed]}]
  (and (pos? acts) (= acts failed)))

;; ------------------------------------------------------------------ the entry

(defn fruitless
  "The backoff entry (nil: none yet) after one more fruitless round at now: the
  count, and once it reaches :after the wait, doubled each time up to :max-s."
  [entry {:keys [after first-s max-s]} now last]
  (let [n (inc (:fruitless entry 0))
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
