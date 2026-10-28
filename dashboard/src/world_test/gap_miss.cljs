(ns world-test.gap-miss
  "tools/event-counts.mjs gap-miss: the miss rate of gap jumps (jobs.lib.walk.gaps' :gap-jump events) per [width run-up dy sprint], to add to
  the planner's physics prior (base/GAP-MISS-JUMPS, run-up 1 or 2). A miss is a jump that ended :fell; the rate is the Beta(prior-a + fell, prior-b + n - fell) posterior,
  reported with its mean and 90% interval (5% and 95% quantiles). :stuck jumps are counted apart: no miss, no landing.
  Pure part: rows, report, beta-quantile.")

(def prior "Beta(1,1), as the planner's physics counts." [1 1])

(defn rows
  "Rows [{:id :width :dy :sprint :out}] of the :gap records in records (event-counts/scan-line's)."
  [records]
  (for [{:keys [gen t] :as r} records :when (= :gap (:k r))]
    (assoc (select-keys r [:width :run :dy :sprint :out]) :id [gen t (:from r)])))

(defn ln-gamma [x]
  (let [cs [76.18009172947146 -86.50532032941677 24.01409824083091 -1.231739572450155 0.1208650973866179e-2 -0.5395239384953e-5]
        tmp (- (+ x 5.5) (* (+ x 0.5) (js/Math.log (+ x 5.5))))
        ser (reduce + 1.000000000190015 (map-indexed (fn [j c] (/ c (+ x j 1))) cs))]
    (- (js/Math.log (/ (* 2.5066282746310005 ser) x)) tmp)))

(defn beta-cf [a b x]
  (let [tiny 1e-30
        step (fn [[c d h] m]
               (let [m2 (* 2 m)
                     aa (/ (* m (- b m) x) (* (+ a m2 -1) (+ a m2)))
                     d (/ 1 (let [v (+ 1 (* aa d))] (if (< (js/Math.abs v) tiny) tiny v)))
                     c (let [v (+ 1 (/ aa c))] (if (< (js/Math.abs v) tiny) tiny v))
                     h (* h d c)
                     aa (/ (* -1 (+ a m) (+ a b m) x) (* (+ a m2) (+ a m2 1)))
                     d (/ 1 (let [v (+ 1 (* aa d))] (if (< (js/Math.abs v) tiny) tiny v)))
                     c (let [v (+ 1 (/ aa c))] (if (< (js/Math.abs v) tiny) tiny v))]
                 [c d (* h d c)]))
        d0 (/ 1 (let [v (- 1 (/ (* (+ a b) x) (+ a 1)))] (if (< (js/Math.abs v) tiny) tiny v)))]
    (nth (reduce step [1 d0 d0] (range 1 300)) 2)))

(defn beta-cdf
  "The regularised incomplete beta I_x(a, b): the cumulative of Beta(a, b) at x."
  [a b x]
  (cond
    (<= x 0) 0
    (>= x 1) 1
    :else (let [front (js/Math.exp (+ (- (ln-gamma (+ a b)) (ln-gamma a) (ln-gamma b)) (* a (js/Math.log x)) (* b (js/Math.log (- 1 x)))))]
            (if (< x (/ (+ a 1) (+ a b 2)))
              (/ (* front (beta-cf a b x)) a)
              (- 1 (/ (* front (beta-cf b a (- 1 x))) b))))))

(defn beta-quantile
  "The x where Beta(a, b)'s cumulative reaches p, by bisection."
  [a b p]
  (loop [lo 0 hi 1 n 0]
    (let [mid (/ (+ lo hi) 2)]
      (cond
        (>= n 60) mid
        (< (beta-cdf a b mid) p) (recur mid hi (inc n))
        :else (recur lo mid (inc n))))))

(defn r4 [x] (/ (js/Math.round (* 10000 x)) 10000))

(defn report
  "{[width run dy sprint] {:n :fell :stuck :mean :lo :hi}} of rows (rows): n the jumps that landed or fell, mean the posterior mean of the
  miss chance, [:lo :hi] its 90% interval."
  [rs]
  (let [[a0 b0] prior]
    (into (sorted-map)
          (for [[k g] (group-by (juxt :width :run :dy :sprint) rs)
                :let [fell (count (filter (comp #{:fell} :out) g))
                      n (+ fell (count (filter (comp #{:landed} :out) g)))
                      a (+ a0 fell) b (+ b0 (- n fell))]]
            [k {:n n :fell fell :stuck (count (filter (comp #{:stuck} :out) g))
                :mean (r4 (/ a (+ a b))) :lo (r4 (beta-quantile a b 0.05)) :hi (r4 (beta-quantile a b 0.95))}]))))
