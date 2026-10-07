(ns world-test.expect
  "Expectations of a world fixture, judged over the body's event log (pure). An :event expectation passes when a
  matching event comes within :within-s seconds of the act's start; a :no-event one passes when none comes for
  :for-s seconds (events before :from-s seconds after the start are ignored). Patterns are partial: a map matches a map holding at least its keys (recursively), a set matches
  any of its members, [:> n] [:>= n] [:< n] [:<= n] compare numbers, [:near [x y z] r] a position ({:x :y :z} or
  [x y z]) within r blocks, [:contains \"text\"] a substring, [:has p] a list with at least one element matching p (other elements and the length are free; a plain vector pattern needs the same length), [:not p] the opposite of p, [:any] anything present;
  anything else matches by equality.")

(def ops #{:> :>= :< :<= :near :contains :has :not :any})

(defn op-pattern? [p] (and (vector? p) (keyword? (first p)) (ops (first p))))

(defn as-xyz [v]
  (cond
    (map? v) (when (every? number? [(:x v) (:y v) (:z v)]) [(:x v) (:y v) (:z v)])
    (and (sequential? v) (= 3 (count v)) (every? number? v)) (vec v)
    :else nil))

(defn matches?
  "Whether value matches pattern (see the namespace doc)."
  [pattern value]
  (cond
    (op-pattern? pattern)
    (let [[op a b] pattern]
      (case op
        :any (some? value)
        :not (not (matches? a value))
        :contains (and (string? value) (.includes value a))
        :has (and (sequential? value) (boolean (some #(matches? a %) value)))
        :near (let [p (as-xyz value) q (as-xyz a)]
                (boolean (and p q (<= (js/Math.hypot (- (p 0) (q 0)) (- (p 1) (q 1)) (- (p 2) (q 2))) b))))
        (and (number? value)
             (case op :> (> value a) :>= (>= value a) :< (< value a) :<= (<= value a)))))

    (map? pattern) (and (map? value) (every? (fn [[k p]] (matches? p (get value k))) pattern))
    (set? pattern) (boolean (some #(matches? % value) pattern))
    (and (vector? pattern) (sequential? value)) (and (= (count pattern) (count value))
                                                     (every? true? (map matches? pattern value)))
    :else (= pattern value)))

(defn of-jobs?
  "Whether event belongs to one of job-ids (its chain starts with one, or its job-id is one)."
  [job-ids event]
  (let [ctx (:context event)
        root (or (first (:chain ctx)) (:job-id ctx))]
    (boolean (and root (contains? job-ids root)))))

(defn event-match
  "The first event (in order) matching expectation e's pattern (and its job, with :of-job), or nil."
  [e pattern events job-ids]
  (some #(when (and (matches? pattern %) (or (not (:of-job e)) (of-jobs? job-ids %))) %) events))

(defn evidence
  "A short text for an event."
  [event]
  (str (name (or (:source event) :?)) "/" (name (or (:kind event) :?))
       (when-let [m (:message event)] (str " \"" m "\""))))

(defn judge
  "One expectation's state at now-ms: {:expect e :status :pass|:fail|:pending :evidence text :at-s seconds after t0}.
  events are those logged since the act started (t0-ms); job-ids the jobs the act submitted.
  A :no-event window starts at t0 (or :from-s, or the first :from-event match, then :for-s long) and ends early at the first :until match inside it."
  [e events {:keys [t0-ms now-ms job-ids]}]
  (if-let [pattern (:event e)]
    (let [deadline (+ t0-ms (* 1000 (:within-s e)))
          hit (event-match e pattern (filter #(<= (:time-ms % 0) deadline) events) job-ids)]
      (cond
        hit {:expect e :status :pass :evidence (evidence hit) :at-s (/ (- (:time-ms hit) t0-ms) 1000)}
        (> now-ms deadline) {:expect e :status :fail :evidence (str "no matching event within " (:within-s e) " s")}
        :else {:expect e :status :pending}))
    (let [start (when-let [p (:from-event e)] (some #(when (matches? p %) %) events))
          from (if-let [p (:from-event e)]
                 (when start (:time-ms start))
                 (+ t0-ms (* 1000 (or (:from-s e) 0))))
          cap (+ (if start from t0-ms) (* 1000 (:for-s e)))
          until (when-let [p (:until e)] (some #(when (and from (>= (:time-ms % 0) from) (matches? p %)) %) events))
          deadline (if until (min cap (:time-ms until)) cap)
          hit (when from (event-match e (:no-event e) (filter #(<= from (:time-ms % 0) deadline) events) job-ids))]
      (cond
        (nil? from) {:expect e :status :pending}
        hit {:expect e :status :fail :evidence (str "unwanted " (evidence hit)) :at-s (/ (- (:time-ms hit) t0-ms) 1000)}
        (and until (<= (:time-ms until) cap)) {:expect e :status :pass :evidence (str "none until " (evidence until))}
        (> now-ms cap) {:expect e :status :pass :evidence (str "none in " (:for-s e) " s")}
        :else {:expect e :status :pending}))))

(defn judge-all [expectations events opts] (mapv #(judge % events opts) expectations))

(defn decided? [results] (not-any? #(= :pending (:status %)) results))

(defn failed?
  "Whether some expectation already failed for good (a forbidden event came, or an :event one ran out of time)."
  [results] (boolean (some #(= :fail (:status %)) results)))

(defn stop-early
  "results of a run that ends at its first failure: the ones still pending become failures saying they were not judged."
  [results]
  (mapv #(if (= :pending (:status %))
           (assoc % :status :fail :evidence "not judged: the run stopped at the first failure")
           %)
        results))

(defn passed? [results] (every? #(= :pass (:status %)) results))

(defn deadline-s
  "The latest second (after t0) at which any expectation can still change."
  [expectations]
  (reduce max 0 (map #(or (:within-s %) (:for-s %) 0) expectations)))
