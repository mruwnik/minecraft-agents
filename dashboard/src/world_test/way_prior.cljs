(ns world-test.way-prior
  "tools/event-counts.mjs way-on: counts jobs.lib.walk.way-log's events (:way-on.priced, :way-on.seen) for the prior of
  jobs.lib.cost/way-on-chance, with the counts as they were when the way was priced (no hindsight).
  A place is one :way-on.priced of a body with the ways it had not listed before; each of its ways later seen is one
  Bernoulli trial (the step goes on or is rock). Model: the place's chance theta ~ Beta(a, b); its k of n counted stands lead
  out ~ Binomial(n, theta); each of its y-of-s read ways goes on ~ Bernoulli(theta). So a place's likelihood is
  B(a+k+y, b+n-k+s-y) / B(a, b), and the next way's chance is (k+a) / (n+a+b), the formula way-on-chance reads.
  Hyperpriors (proper): the mean a/(a+b) ~ Uniform(0, 1); the strength a+b ~ LogNormal(ln 2, 1.5), centred on the strength
  of Beta(1,1) with 95% between 0.1 and 40. The posterior is a grid over both; intervals are 90% (5% to 95%).
  Settings are the light the body perceived at the feet (:lit, :dark, :unknown), each fitted alone.")

(def hyper "The hyperpriors, as reported." {:mean "Uniform(0,1)" :strength "LogNormal(ln 2, 1.5)"})

(defn places
  "The places of one body's events (maps {:kind :time-ms :seq :data}): [{:set :k :n :ways :seen :on}] and the count of its
  ways never read. A way listed again later stays in the place that first listed it."
  [events]
  (let [{:keys [placed ps read]}
        (reduce (fn [{:keys [placed] :as st} {:keys [kind data]}]
                  (case kind
                    :way-on.priced
                    (let [fresh (vec (remove placed (:ways data)))]
                      (if (empty? fresh)
                        st
                        (let [i (count (:ps st))]
                          (-> st
                              (update :ps conj {:set (or (:set data) :unknown) :k (:k data) :n (:n data) :ways (count fresh) :seen 0 :on 0})
                              (update :placed into (map #(vector % i)) fresh)))))
                    :way-on.seen
                    (let [w (:way data) i (placed w)]
                      (if (and i (not (contains? (:read st) w)))
                        (-> st
                            (update :read (fnil conj #{}) w)
                            (update-in [:ps i :seen] inc)
                            (update-in [:ps i :on] + (if (:on data) 1 0)))
                        st))
                    st))
                {:placed {} :ps []}
                (sort-by (juxt :time-ms :seq) events))]
    {:places ps :unread (- (count placed) (count read))}))

;; ln Γ (Lanczos, g 7)
(def ^:private lanczos [0.99999999999980993 676.5203681218851 -1259.1392167224028 771.32342877765313 -176.61502916214059
                        12.507343278686905 -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7])

(defn ln-gamma [x]
  (if (< x 0.5)
    (- (js/Math.log (/ js/Math.PI (js/Math.sin (* js/Math.PI x)))) (ln-gamma (- 1 x)))
    (let [x (dec x)
          t (+ x 7.5)
          s (reduce + (first lanczos) (map-indexed (fn [i c] (/ c (+ x i 1))) (rest lanczos)))]
      (+ (* 0.5 (js/Math.log (* 2 js/Math.PI))) (* (+ x 0.5) (js/Math.log t)) (- t) (js/Math.log s)))))

(defn- ln-beta [a b] (- (+ (ln-gamma a) (ln-gamma b)) (ln-gamma (+ a b))))

(defn log-likelihood
  "Of places [{:k :n :seen :on}] under Beta(a, b)."
  [ps a b]
  (let [base (ln-beta a b)]
    (reduce (fn [s {:keys [k n seen on]}] (+ s (- (ln-beta (+ a k on) (+ b (- n k) (- seen on))) base))) 0 ps)))

(def ^:private grid {:logit [-7 7] :ln-s [-4 6] :steps 140})

(defn- sigmoid [l] (/ 1 (+ 1 (js/Math.exp (- l)))))

(defn posterior
  "The grid posterior of (a, b) over places (those with a way read): [{:a :b :w}] with weights summing to 1."
  [ps]
  (let [{[l0 l1] :logit [u0 u1] :ln-s steps :steps} grid
        cells (for [i (range (inc steps)) j (range (inc steps))
                    :let [l (+ l0 (* i (/ (- l1 l0) steps))) u (+ u0 (* j (/ (- u1 u0) steps)))
                          m (sigmoid l) s (js/Math.exp u)
                          a (* m s) b (* (- 1 m) s)
                          z (/ (- u (js/Math.log 2)) 1.5)]]
                {:a a :b b :lw (+ (log-likelihood ps a b) (js/Math.log (* m (- 1 m))) (* -0.5 z z))})
        top (apply max (map :lw cells))
        ws (map #(js/Math.exp (- (:lw %) top)) cells)
        total (reduce + ws)]
    (mapv (fn [c w] (assoc (dissoc c :lw) :w (/ w total))) cells ws)))

(defn- quantile [cells f q]
  (loop [[c & more] (sort-by f cells) acc 0]
    (let [acc (+ acc (:w c))]
      (if (or (>= acc q) (empty? more)) (f c) (recur more acc)))))

(defn- r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn- summary [cells f]
  {:mean (r3 (reduce + (map #(* (:w %) (f %)) cells)))
   :lo (r3 (quantile cells f 0.05)) :hi (r3 (quantile cells f 0.95))})

(defn fit
  "{:places :ways :on :a :b :mean :strength} of places, each a mean with a 90% interval; nil with no way read."
  [ps]
  (let [ps (filterv #(pos? (:seen %)) ps)]
    (when (seq ps)
      (let [cells (posterior ps)]
        {:places (count ps) :ways (reduce + (map :seen ps)) :on (reduce + (map :on ps))
         :a (summary cells :a) :b (summary cells :b)
         :mean (summary cells #(/ (:a %) (+ (:a %) (:b %)))) :strength (summary cells #(+ (:a %) (:b %)))}))))

(defn report
  "{:hyper :all :by-setting} over places; :unread the ways never seen."
  [places unread]
  {:hyper hyper
   :unread unread
   :all (fit places)
   :by-setting (into (sorted-map) (keep (fn [[s ps]] (when-let [f (fit ps)] [s f]))) (group-by :set places))})
