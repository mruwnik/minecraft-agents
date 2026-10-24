(ns world-test.way-prior
  "tools/event-counts.mjs way-on: counts jobs.lib.walk.way-log's events (:way-on.priced, :way-on.seen, :way-on.censored) for the
  prior of jobs.lib.cost/way-on-chance, with the counts as they were when the way was priced (no hindsight).
  A place is one :way-on.priced of a body with the ways it had not listed before; each of its ways is one Bernoulli trial
  (the step goes on or is rock), read (:seen, :on of them) or not (:unread: dropped by way-log, or still waiting).
  Model: the place's chance theta ~ Beta(a, b); its k of n counted stands lead out ~ Binomial(n, theta); each way goes on
  ~ Bernoulli(theta). The next way's chance is then (k+a) / (n+a+b), the formula way-on-chance reads.
  Hyperpriors (proper): the mean a/(a+b) ~ Uniform(0, 1); the strength a+b ~ LogNormal(ln 2, 1.5), centred on the strength
  of Beta(1,1) with 95% between 0.1 and 40. The posterior is a grid; intervals are 90% (5% to 95%).
  A way is read only if the body later sees its cells, and whether it does may hang on the outcome (a way fled through is seen,
  a wall may never be), so each fit is given four ways: :read the read ways alone (unread ones missing at random), :shut every
  unread way rock, :on every unread way going on (the bounds), and :censored, where a way that goes on is read with chance
  r-on and one that is rock with chance r-shut (each Uniform(0, 1), the same for every place of a setting, ways read or not
  independently of each other and of k and n), the unread ways summed over.
  Settings are the light the body perceived at the feet (:lit, :dark, :unknown), each fitted alone.")

(def hyper "The hyperpriors, as reported." {:mean "Uniform(0,1)" :strength "LogNormal(ln 2, 1.5)" :r-on "Uniform(0,1)" :r-shut "Uniform(0,1)"})

(defn places
  "The places of one body's events (maps {:kind :time-ms :seq :data}): {:places [{:set :k :n :ways :seen :on :unread}] :evicted
  the unread ways way-log dropped}. A way listed again later stays in the place that first listed it."
  [events]
  (let [{:keys [placed ps read evicted]}
        (reduce (fn [{:keys [placed] :as st} {:keys [kind data]}]
                  (case kind
                    :way-on.priced
                    (let [fresh (vec (distinct (remove placed (:ways data))))]
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
                            (update :read conj w)
                            (update-in [:ps i :seen] inc)
                            (update-in [:ps i :on] + (if (:on data) 1 0)))
                        st))
                    :way-on.censored
                    (cond-> st (placed (:way data)) (update :evicted conj (:way data)))
                    st))
                {:placed {} :ps [] :read #{} :evicted #{}}
                (sort-by (juxt :time-ms :seq) events))]
    {:places (mapv #(assoc % :unread (- (:ways %) (:seen %))) ps)
     :evicted (count (remove read evicted))}))

;; ln Γ (Lanczos, g 7)
(def ^:private lanczos #js [0.99999999999980993 676.5203681218851 -1259.1392167224028 771.32342877765313 -176.61502916214059
                            12.507343278686905 -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7])

(defn ln-gamma [x]
  (if (< x 0.5)
    (- (js/Math.log (/ js/Math.PI (js/Math.sin (* js/Math.PI x)))) (ln-gamma (- 1 x)))
    (let [x (dec x)
          t (+ x 7.5)
          s (loop [i 1 s (aget lanczos 0)] (if (< i 9) (recur (inc i) (+ s (/ (aget lanczos i) (+ x i)))) s))]
      (+ (* 0.5 (js/Math.log (* 2 js/Math.PI))) (* (+ x 0.5) (js/Math.log t)) (- t) (js/Math.log s)))))

(defn- ln-beta [a b] (- (+ (ln-gamma a) (ln-gamma b)) (ln-gamma (+ a b))))

(defn- ln-choose [c j] (- (ln-gamma (inc c)) (ln-gamma (inc j)) (ln-gamma (inc (- c j)))))

(defn- place-terms
  "What a place {:k :n :seen :on :unread} gives at Beta(a, b) whatever the read chances: {:on :shut :c :ts} with ts[j] =
  ln C(c, j) + ln B(a+k+on+j, b+n-k+shut+c-j) - ln B(a, b), j of its c unread ways going on (c 0: unread ways left out)."
  [{:keys [k n seen on unread]} a b censored?]
  (let [shut (- seen on)
        c (if censored? unread 0)
        base (ln-beta a b)
        ts (make-array (inc c))]
    (dotimes [j (inc c)]
      (aset ts j (- (+ (ln-choose c j) (ln-beta (+ a k on j) (+ b (- n k) shut (- c j)))) base)))
    {:on on :shut shut :c c :ts ts}))

(defn- terms-at
  "The log-likelihood of a place's place-terms t when a way that goes on is read with chance r-on, a rock one with r-shut
  (nil: the unread ways left out)."
  [{:keys [on shut c ts]} r-on r-shut]
  (if (nil? r-on)
    (aget ts 0)
    (let [l-on (js/Math.log (- 1 r-on)) l-shut (js/Math.log (- 1 r-shut))
          xs (make-array (inc c))]
      (dotimes [j (inc c)] (aset xs j (+ (aget ts j) (* j l-on) (* (- c j) l-shut))))
      (let [top (apply max xs)]
        (+ (* on (js/Math.log r-on)) (* shut (js/Math.log r-shut))
           top (js/Math.log (areduce xs j s 0 (+ s (js/Math.exp (- (aget xs j) top))))))))))

(defn log-likelihood
  "Of places [{:k :n :seen :on :unread}] under Beta(a, b); with r-on and r-shut (see terms-at) the unread ways are summed over,
  without them left out."
  ([ps a b] (log-likelihood ps a b nil nil))
  ([ps a b r-on r-shut]
   (reduce + 0 (map #(terms-at (place-terms % a b (some? r-on)) r-on r-shut) ps))))

(def ^:private grid {:logit [-7 7] :ln-s [-4 6] :steps 140 :censored-steps 50 :r-steps 10})

(defn- sigmoid [l] (/ 1 (+ 1 (js/Math.exp (- l)))))

(defn posterior
  "The grid posterior over places: [{:a :b :w}] with weights summing to 1; censored? also over :r-on :r-shut (terms-at)."
  ([ps] (posterior ps false))
  ([ps censored?]
   (let [{[l0 l1] :logit [u0 u1] :ln-s :keys [r-steps]} grid
         steps (if censored? (:censored-steps grid) (:steps grid))
         groups (vec (frequencies (map #(select-keys % [:k :n :seen :on :unread]) ps)))
         rs (if censored? (mapv #(/ (+ % 0.5) r-steps) (range r-steps)) [nil])
         cells (vec (for [i (range (inc steps)) j (range (inc steps))
                          :let [l (+ l0 (* i (/ (- l1 l0) steps))) u (+ u0 (* j (/ (- u1 u0) steps)))
                                m (sigmoid l) s (js/Math.exp u)
                                a (* m s) b (* (- 1 m) s)
                                z (/ (- u (js/Math.log 2)) 1.5)
                                prior (+ (js/Math.log (* m (- 1 m))) (* -0.5 z z))
                                ts (mapv (fn [[g times]] [(place-terms g a b censored?) times]) groups)]
                          r-on rs r-shut rs]
                      (cond-> {:a a :b b :lw (reduce (fn [acc [t times]] (+ acc (* times (terms-at t r-on r-shut)))) prior ts)}
                        censored? (assoc :r-on r-on :r-shut r-shut))))
         top (apply max (map :lw cells))
         ws (mapv #(js/Math.exp (- (:lw %) top)) cells)
         total (reduce + ws)]
     (mapv (fn [c w] (assoc (dissoc c :lw) :w (/ w total))) cells ws))))

(defn- quantile [cells f q]
  (loop [[c & more] (sort-by f cells) acc 0]
    (let [acc (+ acc (:w c))]
      (if (or (>= acc q) (empty? more)) (f c) (recur more acc)))))

(defn- r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn- summary [cells f]
  {:mean (r3 (reduce + (map #(* (:w %) (f %)) cells)))
   :lo (r3 (quantile cells f 0.05)) :hi (r3 (quantile cells f 0.95))})

(defn- fit-of [ps censored?]
  (let [cells (posterior ps censored?)]
    (cond-> {:a (summary cells :a) :b (summary cells :b)
             :mean (summary cells #(/ (:a %) (+ (:a %) (:b %)))) :strength (summary cells #(+ (:a %) (:b %)))}
      censored? (assoc :r-on (summary cells :r-on) :r-shut (summary cells :r-shut)))))

(defn fit
  "{:places :ways :seen :on-seen :read :shut :on :censored} of places: the counts, then each fit (see the ns doc) of :a :b :mean
  :strength, each a mean with a 90% interval, and :censored's :r-on :r-shut; nil with no place."
  [ps]
  (when (seq ps)
    (let [as (fn [f] (mapv #(assoc (f %) :unread 0) ps))]
      {:places (count ps) :ways (reduce + (map :ways ps)) :seen (reduce + (map :seen ps)) :on-seen (reduce + (map :on ps))
       :read (fit-of ps false)
       :shut (fit-of (as #(update % :seen + (:unread %))) false)
       :on (fit-of (as #(-> % (update :seen + (:unread %)) (update :on + (:unread %)))) false)
       :censored (fit-of ps true)})))

(defn report
  "{:hyper :unread :evicted :all :by-setting} over places; :unread the ways never seen, :evicted those of them way-log dropped."
  [places evicted]
  {:hyper hyper
   :unread (reduce + (map :unread places))
   :evicted evicted
   :all (fit places)
   :by-setting (into (sorted-map) (keep (fn [[s ps]] (when-let [f (fit ps)] [s f]))) (group-by :set places))})
