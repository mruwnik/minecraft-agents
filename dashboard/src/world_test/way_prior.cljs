(ns world-test.way-prior
  "tools/event-counts.mjs way-on: the measured prior jobs.lib.walk.way-on ::prior, from the trial bodies' logs and sight memory.
  A spot is where a flight (a respond run's first flee decision, at the body's last position within pos-age-ms) or a walk (a
  walk child's :planned) started, one per kind per body per 16-block section. Its counts are way-on/way-counts from that
  feet cell over the body's seen.bin (the later read: what it has seen there by now): k of the n ways out of seen stands lead
  on. Its setting is the recorded chunks' sky light at the feet: 15 :surface, 1-14 :roofed, 0 :cave.
  Model: each spot's chance θ ~ Beta(a, b), k ~ Binomial(n, θ); a and b the beta-binomial's marginal maximum likelihood over
  every spot (the posterior mode under a flat prior on logit mean and on log(a + b)). Each setting's mean is reported
  shrunk toward the others' by the spread the data show (DerSimonian-Laird).
  Pure parts: spots, setting-of, fit, by-setting, report."
  (:require [jobs.lib.walk.way-on :as way-on]))

(def pos-age-ms "Longest age of the position a flight is read from (event-counts' pos-age-ms)." 2000)

(defn section-of [{:keys [x y z]}] (mapv #(js/Math.floor (/ % 16)) [x y z]))

(defn spots
  "Spots [{:id [kind section] :kind :flee|:walk :t :at {:x :y :z}}] of one body's records (event-counts scan-line's, sorted by
  :t; a :pos with :walk? is a walk's plan), the first of each kind in each section."
  [records]
  (let [spot (fn [st kind t at]
               (let [id [kind (section-of at)]]
                 (if ((:seen st) id) st (-> st (update :seen conj id) (update :out conj {:id id :kind kind :t t :at at})))))]
    (:out
     (reduce (fn [st {:keys [k gen t decision walk?] :as r}]
               (cond
                 (= :pos k) (cond-> (assoc-in st [:last-pos gen] r) walk? (spot :walk t (select-keys r [:x :y :z])))
                 (and (= :decided k) (= :flee decision))
                 (let [p (get-in st [:last-pos gen])]
                   (if (and p (<= (- t (:t p)) pos-age-ms)) (spot st :flee t (select-keys p [:x :y :z])) st))
                 :else st))
             {:last-pos {} :seen #{} :out []}
             records))))

(defn setting-of
  "The setting of a cell of sky light sky (nil: not recorded)."
  [sky]
  (cond (nil? sky) nil (>= sky 15) :surface (pos? sky) :roofed :else :cave))

(defn feet [{:keys [x y z]}] (mapv js/Math.floor [x y z]))

(defn counted
  "spot with :k :n, way-on/way-counts over known (fn [cx sy cz] -> ids + 1) from its feet, and :setting by sky-at (fn [x y z] -> sky light or nil)."
  [spot known sky-at]
  (let [[x y z :as f] (feet (:at spot))]
    (merge spot (way-on/way-counts known f) {:setting (setting-of (sky-at x y z))})))

;; ln Γ (Lanczos, g 7): the beta-binomial's likelihood
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
  "Of {[k n] times} under Beta(a, b) spots (the binomial coefficients left out: they do not depend on a, b)."
  [freqs a b]
  (let [base (ln-beta a b)]
    (reduce-kv (fn [s [k n] m] (+ s (* m (- (ln-beta (+ k a) (+ (- n k) b)) base)))) 0 freqs)))

(def ^:private span "logit mean and ln(a + b) searched: a mean 0.0025-0.9975, a + b e^-4 to e^9." {:logit [-6 6] :ln-s [-4 9]})

(defn fit
  "{:a :b :mean :strength a + b :spots :k :n :at-edge} of rows [{:k :n}] (rows with n 0 tell nothing): the beta-binomial's
  maximum-likelihood a, b, by a grid over logit mean and ln(a + b) zoomed on its best point. :at-edge: the best ln(a + b) is
  the searched span's end (the spots do not differ beyond chance: a + b grows without bound), nil without a row."
  [rows]
  (let [rows (filter #(pos? (:n %)) rows)
        freqs (frequencies (map (juxt :k :n) rows))
        ab (fn [l s] (let [m (/ 1 (+ 1 (js/Math.exp (- l)))) t (js/Math.exp s)] [(* m t) (* (- 1 m) t)]))
        ll (fn [[l s]] (apply log-likelihood freqs (ab l s)))
        steps 24]
    (when (seq rows)
      (let [[l s] (loop [[l0 l1] (:logit span) [s0 s1] (:ln-s span) i 0 best nil]
                    (if (= i 12)
                      best
                      (let [grid (for [a (range (inc steps)) b (range (inc steps))]
                                   [(+ l0 (* a (/ (- l1 l0) steps))) (+ s0 (* b (/ (- s1 s0) steps)))])
                            [bl bs :as best] (apply max-key ll grid)
                            hl (/ (- l1 l0) 4) hs (/ (- s1 s0) 4)]
                        (recur [(max (first (:logit span)) (- bl hl)) (min (second (:logit span)) (+ bl hl))]
                               [(max (first (:ln-s span)) (- bs hs)) (min (second (:ln-s span)) (+ bs hs))]
                               (inc i) best))))
            [a b] (ab l s)]
        {:a a :b b :mean (/ a (+ a b)) :strength (+ a b) :spots (count rows)
         :k (reduce + (map :k rows)) :n (reduce + (map :n rows))
         :at-edge (> s (- (second (:ln-s span)) 0.01))}))))

(defn- mean-and-var
  "{:mean K/N :var :spots} of rows: the ratio mean and its variance with the spots as clusters (each spot's ways share its place)."
  [rows]
  (let [k (reduce + (map :k rows)) n (reduce + (map :n rows)) j (count rows) m (/ k n)]
    {:mean m :spots j
     :var (when (> j 1) (* (/ j (dec j)) (/ (reduce + (map #(js/Math.pow (- (:k %) (* m (:n %))) 2) rows)) (* n n))))}))

(defn by-setting
  "{setting {:spots :mean :se :shrunk :fit}} of rows with a :setting (and :all: tau2, the DerSimonian-Laird between-setting
  variance of the means): :shrunk the setting's mean pulled toward the random-effects mean by tau2 against its own
  variance (tau2 0: each is that mean)."
  [rows]
  (let [groups (into {} (keep (fn [[g rs]] (let [rs (filter #(pos? (:n %)) rs)] (when (and g (seq rs)) [g rs])))) (group-by :setting rows))
        mv (update-vals groups mean-and-var)
        used (filter (fn [[_ {v :var}]] (and v (pos? v))) mv)
        w (into {} (map (fn [[g {v :var}]] [g (/ 1 v)])) used)
        sum-w (reduce + (vals w))
        fixed (when (pos? sum-w) (/ (reduce + (map (fn [[g x]] (* x (:mean (mv g)))) w)) sum-w))
        q (reduce + (map (fn [[g x]] (* x (js/Math.pow (- (:mean (mv g)) fixed) 2))) w))
        c (- sum-w (/ (reduce + (map #(* % %) (vals w))) (if (pos? sum-w) sum-w 1)))
        tau2 (if (and (> (count used) 1) (pos? c)) (max 0 (/ (- q (dec (count used))) c)) 0)
        w* (into {} (map (fn [[g {v :var}]] [g (/ 1 (+ v tau2))])) used)
        re (when (seq w*) (/ (reduce + (map (fn [[g x]] (* x (:mean (mv g)))) w*)) (reduce + (vals w*))))
        shrunk (fn [{:keys [mean var]}] (cond (nil? re) mean (or (nil? var) (zero? tau2)) re
                                              :else (/ (+ (/ mean var) (/ re tau2)) (+ (/ 1 var) (/ 1 tau2)))))]
    (into {:all {:tau2 tau2 :mean re}}
          (map (fn [[g rs]] (let [{:keys [mean var spots] :as x} (mv g)]
                              [g {:spots spots :mean mean :se (some-> var js/Math.sqrt) :shrunk (shrunk x) :fit (fit rs)}])))
          groups)))

(defn r3 [x] (when x (/ (js/Math.round (* 1000 x)) 1000)))

(defn report
  "The prior ::prior takes (:setting {:a :b :spots :k :n}: fit over every spot), the fit, the settings' (:all :tau the
  between-setting spread of their means) and each kind's."
  [rows]
  (let [{:keys [a b spots k n] :as f} (fit rows)
        round (fn [m] (update-vals m #(if (number? %) (r3 %) %)))]
    {:setting (when f {:a (r3 a) :b (r3 b) :spots spots :k k :n n})
     :fit (some-> f round)
     :by-setting (let [{:keys [all] :as g} (by-setting rows)]
                   (assoc (update-vals (dissoc g :all) (fn [m] (-> (round m) (update :fit #(some-> % round)))))
                          :all {:tau (r3 (js/Math.sqrt (:tau2 all))) :mean (r3 (:mean all))}))
     :by-kind (into (sorted-map) (map (fn [[kind rs]] [kind (some-> (fit rs) round)])) (group-by :kind rows))
     :unread (count (remove #(pos? (:n %)) rows))}))

(defn rows
  "The counted spots of one body's records, ids [body kind section]: known its seen.bin's sections (nil: none, no rows)."
  [body records known sky-at]
  (when known (map #(update (counted % known sky-at) :id (fn [id] (into [body] id))) (spots records))))

(defn fold
  "way-spots (the counts file's rows by id) with rows merged in: a later read replaces one that read fewer ways (a
  section evicted from memory since reads none)."
  [way-spots rows]
  (reduce (fn [m r] (update m (:id r) #(if (and % (> (:n %) (:n r))) % r))) way-spots rows))
