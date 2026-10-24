(ns world-test.censored
  "Times seen only as intervals. fit: the Turnbull estimate (the nonparametric maximum-likelihood distribution of interval- and
  right-censored times; Kaplan-Meier when each time is exact or right-censored). shrink: each group's fit pooled partially toward the
  fit of the other groups, by the between-group spread of the means the data show (DerSimonian-Laird).")

(defn fit
  "The Turnbull estimate of xs, each [lo hi]: a time in (lo, hi], hi nil when it is only known to be past lo (lo = hi: seen exactly).
  {:at sorted support points :p their masses}; ##Inf holds the mass past every finite end."
  [xs]
  (let [at (vec (concat (sort (distinct (concat (map first xs) (keep second xs)))) [##Inf]))
        m (count at)
        index (into {} (map-indexed (fn [i v] [v i])) at)
        spans (mapv (fn [[lo hi]] [(if (= lo hi) (index lo) (inc (index lo))) (if hi (index hi) (dec m))]) xs)
        n (count xs)
        step (fn [p]
               (let [cum (vec (reductions + 0 p))
                     diff (reduce (fn [d [a b]] (let [share (/ 1 (- (cum (inc b)) (cum a)))]
                                                  (-> d (update a + share) (update (inc b) - share))))
                                  (vec (repeat (inc m) 0)) spans)]
                 (mapv (fn [pj s] (/ (* pj s) n)) p (rest (reductions + 0 diff)))))]
    (loop [p (vec (repeat m (/ 1 m))) i 0]
      (let [q (step p)]
        (if (or (= i 5000) (< (reduce max 0 (map #(js/Math.abs (- %1 %2)) p q)) 1e-10))
          {:at at :p q}
          (recur q (inc i)))))))

(defn- finite
  "f with its mass at ##Inf moved to its last finite point: the data say only that those times are at least that."
  [{:keys [at p]}]
  (let [k (- (count at) 2)]
    (if (neg? k) {:at [] :p []} {:at (subvec at 0 (inc k)) :p (update (subvec p 0 (inc k)) k + (peek p))})))

(defn quantile
  "The least support point of f at which its distribution reaches q; past every finite end, the last finite point (a lower bound)."
  [f q]
  (let [{:keys [at p]} (finite f)]
    (or (some (fn [[x c]] (when (>= c (- q 1e-9)) x)) (map vector at (reductions + p))) (peek at))))

(defn mean [f] (let [{:keys [at p]} (finite f)] (reduce + 0 (map * at p))))

(defn- spread-of
  "The variance of the points xs with masses p (0 when they hold no mass)."
  [xs p]
  (let [total (reduce + 0 p)
        mu (when (pos? total) (/ (reduce + 0 (map * xs p)) total))]
    (if mu (/ (reduce + 0 (map #(* %2 (js/Math.pow (- %1 mu) 2)) xs p)) total) 0)))

(defn variance [f] (let [{:keys [at p]} (finite f)] (spread-of at p)))

(defn information
  "What the times xs (fit's) tell of a mean, in times seen exactly: each counts 1 less the share of ref's variance (a fit) still open
  within its interval, Rubin's fraction of missing information: a time seen exactly 1, one censored early about 0."
  [xs ref]
  (let [{:keys [at p]} (finite ref)
        whole (spread-of at p)
        open (fn [[lo hi]] (let [in (keep-indexed (fn [i x] (when (and (< lo x) (or (nil? hi) (<= x hi))) i)) at)]
                             (spread-of (map at in) (map p in))))]
    (reduce + 0 (map (fn [[lo hi :as x]] (if (or (= lo hi) (not (pos? whole))) 1 (max 0 (- 1 (/ (open x) whole))))) xs))))

(defn- kinds
  "Groups {g xs} -> {g {:own fit :rest the other groups' fit :n information against :rest :mean :var of :own}}."
  [groups]
  (into {} (map (fn [[g xs]] (let [own (fit xs) rest-fit (fit (mapcat val (dissoc groups g)))]
                               [g {:own own :rest rest-fit :n (information xs rest-fit) :mean (mean own) :var (variance own)}])))
        groups))

(defn- dl
  "between of kinds' groups."
  [ks]
  (let [rows (filter #(pos? (:n %)) (vals ks))
        sum #(reduce + 0 %)
        many (filter #(>= (:n %) 2) rows)
        sigma2 (/ (sum (map #(* (dec (:n %)) (:var %)) many)) (sum (map #(dec (:n %)) many)))
        total (sum (map :n rows))
        mbar (/ (sum (map #(* (:n %) (:mean %)) rows)) total)
        q (/ (sum (map #(* (:n %) (js/Math.pow (- (:mean %) mbar) 2)) rows)) sigma2)
        c (/ (- total (/ (sum (map #(* (:n %) (:n %)) rows)) total)) sigma2)
        tau2 (max 0 (/ (- q (dec (count rows))) c))]
    {:tau2 tau2 :sigma2 sigma2 :alpha (if (pos? tau2) (/ sigma2 tau2) ##Inf)}))

(defn between
  "The DerSimonian-Laird estimate over groups {g xs} (fit's xs; a group that tells nothing of its mean left out) of the
  between-group variance of the means (:tau2), the within-group variance pooled over the groups of two or more (:sigma2) and the
  prior strength :alpha = sigma2 / tau2: a group weighs n / (n + alpha) against the others, n its information against the other
  groups' fit (##Inf: the groups do not differ beyond chance, each is the pool)."
  [groups]
  (dl (kinds groups)))

(defn shrink
  "Groups {g xs} -> {g {:n :w :own :rest :pooled}}: at each level of qs, :own the group's quantile, :rest that of the other groups'
  times together, :pooled w x own + (1 - w) x rest, w = n / (n + alpha), n the group's information (between's)."
  [groups qs]
  (let [ks (kinds groups)
        {:keys [alpha]} (dl ks)]
    (into (sorted-map)
          (map (fn [[g {:keys [own rest n]}]]
                 (let [w (/ n (+ n alpha))]
                   [g {:n n :w w :own (mapv #(quantile own %) qs) :rest (mapv #(quantile rest %) qs)
                       :pooled (mapv #(+ (* w (quantile own %)) (* (- 1 w) (quantile rest %))) qs)}])))
          ks)))
