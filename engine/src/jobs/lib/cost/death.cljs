(ns jobs.lib.cost.death
  "Death as an ordinary cost, in seconds like every other: the time to get the carried items back plus what the lost
  experience is worth. A route pays chance-of-dying x death-cost (jobs.lib.cost.answer/death-chance: the known threats
  are priced by the answer the body would take to each). Pure.

  body: {:level experience levels, :respawn {:x :y :z} where it would wake, :carried the seconds the carried items are
  worth (item-value)}.

  A remembered death fades: death-weight. One by terrain (a fall, water, lava, a wall) costs go-to's plans near its spot
  as terrain-danger.

  spawn-worth: what a respawn point here is worth over the next ::spawn-horizon-s, the deaths expected meanwhile times what
  each would cost more from the respawn the body has."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.value :as value]))

(a/defargs settings
  {::seconds-per-level {:default 5 :spec (a/num-in 0 nil)
                        :doc "Worth of one experience level lost at a death, in seconds (the game drops a few levels' worth)."}
   ::notice-rate {:default {:other {:k 8 :n-s 42.4}
                            :ranged [{:case "lit 2" :blocks 2 :k 8 :n-s 11.2} {:case "lit 5" :blocks 5 :k 8 :n-s 18.7}
                                     {:case "lit 9" :blocks 9 :k 9 :n-s 46.9} {:case "lit 13" :blocks 13 :k 8 :n-s 21.2}
                                     {:case "dark 9" :blocks 9 :k 4 :n-s 27.7}]}
                  :spec (let [counts (a/map-with {:k (a/int-in 0 nil) :n-s (a/num-in 0 nil)})
                              class (a/or-of counts (a/coll-of counts))]
                          (a/map-with {:other class :ranged class}))
                  :doc "Per class of mob (:ranged: threat/ranged?; :other: caught within melee reach), the counts {:k :n-s} of one case or of each case counted apart (:blocks, when given, the mob's distance from the body in it): n-s the weighted seconds sightings spent in the body's field (weight 1 within a few steps of the mob's way, or a ranged mob's range, less beyond), k the catches (a melee mob: a hurt from it, or a respond decision with it in reach; a ranged mob: its first hit, at the time of that hit). notice-prior and notice-factor read them. :other: 24 trial sightings (8 censored by a position gap; zombie 6 of 13 s, creeper 2 of 29.4 s). :ranged: the world fixture ranged-notice (a bow skeleton, a clear line of fire, a body standing still), by distance in blocks and light; trial play first saw every skeleton beyond its range. Recount: node tools/event-counts.mjs notice-rate (:by-case), 2026-10-24."}
   ::notice-spread-scale {:default 1 :spec a/pos-num?
                          :doc "The scale of the half-Cauchy prior on how far the notice rates of a class's distances spread round its rate, as their coefficient of variation (notice-posterior): weak, the usual prior for a spread over few cases."}
   ::deaths-per-hour {:default 1.3 :spec (a/num-in 0 nil)
                      :doc "Deaths an hour of play, every cause: the fresh-start trials of 2026-10-21/22 (Wicken r5, 8 deaths in 6h15; docs/fresh-start-design.md section 1)."}
   ::spawn-walk-blocks {:default 5000 :spec (a/num-in 0 nil)
                        :doc "Blocks from world spawn back to where a body with no respawn bed died: fresh bodies are dropped 5000 or more from it (docs/fresh-start-design.md section 4)."}
   ::spawn-horizon-s {:default 1200 :spec (a/num-in 0 nil)
                      :doc "Seconds a respawn point is priced over (spawn-worth): one in-game day, after which the needs-bed errand comes again."}
   ::memory-ms {:default 7200000 :spec (a/num-in 1 nil)
                :doc "Milliseconds over which the memory of a death fades from full weight to none."}
   ::terrain-rate {:default 4 :spec (a/num-in 0 nil)
                   :doc "hp a second a fresh death by terrain costs a plan within ::terrain-close of its spot (the planner's options.dangers rate)."}
   ::terrain-close {:default 1 :spec (a/num-in 0 nil)
                    :doc "Blocks round a death by terrain within which it costs its full rate."}
   ::terrain-radius {:default 4 :spec (a/num-in 0 nil)
                     :doc "Blocks round a death by terrain past which it costs nothing: the cells that killed are near the spot."}})

(defn death-weight
  "1 for a death just now, falling to 0 at ::memory-ms age (or at `memory-ms`, for a record kept shorter)."
  ([age-ms] (death-weight age-ms (settings/get settings ::memory-ms)))
  ([age-ms memory-ms] (max 0 (min 1 (- 1 (/ age-ms memory-ms))))))

(defn terrain-radius [] (settings/get settings ::terrain-radius))

(defn terrain-danger
  "The planner danger {:x :y :z :close :radius :rate :mob :block} of a death by terrain `cause` at pos, its rate ::terrain-rate x weight
  (death-weight): never a block, the body may cross the spot."
  [{:keys [pos cause weight]}]
  {:x (:x pos) :y (:y pos) :z (:z pos) :close (settings/get settings ::terrain-close) :radius (terrain-radius)
   :rate (* weight (settings/get settings ::terrain-rate)) :mob cause :block false})

(defn experience-cost
  "Seconds the experience levels of body are worth."
  [body]
  (* (settings/get settings ::seconds-per-level) (or (:level body) 0)))

(defn death-cost
  "Seconds a death at `at` ({:x :y :z}) costs: the walk from the body's :respawn to `at` and back for the items (a trip
  and the walk price), plus experience-cost. What the death leaves, the :carried items (fresh), is lost when the walk back cannot beat the despawn
  (value/fetch-cost's :too-far), as with no :respawn: that body walks ::spawn-walk-blocks from world spawn."
  [body at]
  (let [from (:respawn body)
        blocks (if from
                 (js/Math.hypot (- (:x from) (:x at)) (- (:y from) (:y at)) (- (:z from) (:z at)))
                 (settings/get settings ::spawn-walk-blocks))
        gets (fn [elapsed-ms] (and from (not (:reason (value/fetch-cost {:distance blocks :elapsed-ms elapsed-ms})))))
        left (if (gets 0) 0 (or (:carried body) 0))]
    (+ (value/trip) (:cost (value/walk-cost {:distance blocks})) left (experience-cost body))))

(defn blow-outcome
  "{:dies :lived} of blows of about hit hp, damage hp of them expected, against the health and absorption of body: dies the
  chance at least life/hit of them land, lived the hp they take in the counts the body lives through, each by its chance
  (E[damage, alive]: the hp of a death is not paid, respawn heals it). Their count is a Poisson one, or with shape (a
  Gamma-distributed rate: an uncertain fit) a negative binomial one, the Poisson's posterior predictive. sure: hp of
  damage that lands for certain in big blows (a creeper's blast), taken off the life first: the small blows count against
  what it leaves, not as parts of a big one."
  [{:keys [health absorption]} damage hit sure shape]
  (let [life (- (+ (or health 20) (or absorption 0)) sure)
        hit (max 0.1 (or hit 1))
        lam (/ (max 0 (- damage sure)) hit)
        k (js/Math.ceil (/ life hit))
        [p0 step] (if shape
                    (let [q (/ lam (+ shape lam))]
                      [(js/Math.exp (* shape (js/Math.log1p (- q)))) (fn [p i] (/ (* p q (+ shape i -1)) i))])
                    [(js/Math.exp (- lam)) (fn [p i] (/ (* p lam) i))])
        ps (vec (take k (reductions step p0 (iterate inc 1))))
        below (reduce + ps)]
    (if (pos? life)
      {:dies (max 0 (- 1 below)) :lived (reduce + (map-indexed (fn [i p] (* p (+ sure (* i hit)))) ps))}
      {:dies 1 :lived 0})))

(defn blow-chance
  "The :dies of blow-outcome."
  ([body damage hit] (blow-chance body damage hit 0 nil))
  ([body damage hit sure] (blow-chance body damage hit sure nil))
  ([body damage hit sure shape] (:dies (blow-outcome body damage hit (or sure 0) shape))))

(defn log-rising
  "ln (alpha (alpha + 1) ... (alpha + k - 1)), ln Gamma(alpha + k) / Gamma(alpha) for a count k."
  [alpha k]
  (loop [j 0 acc 0] (if (< j k) (recur (inc j) (+ acc (js/Math.log (+ alpha j)))) acc)))

(def notice-posterior
  "{:near :groups :points} of a class's cases [{:blocks :k :n-s}] (two or more): :groups [[d k n-s]] the counts summed
  by distance d (none: 0), near the least; :points #js [mu alpha w], those of weight w over 1e-7, the grid posterior of
  a hierarchical Gamma-Poisson: each distance's rate is mu e, e its own Gamma(alpha, alpha) of mean 1, no trend forced
  on distance (a skeleton strafes and aims by range); k notices in n-s seconds a Poisson count. Priors: mu Jeffreys'
  (mu^-1/2, as one case's Gamma(k + 0.5, n-s)); 1/sqrt(alpha), the distances' coefficient of variation, half-Cauchy
  of scale spread-scale. Memoized: a few classes."
  (memoize
    (fn [cases spread-scale]
      (let [groups (->> (group-by #(or (:blocks %) 0) cases)
                        (map (fn [[d cs]] [d (reduce + (map :k cs)) (reduce + (map :n-s cs))]))
                        (sort-by first)
                        vec)
            a0 (js/Math.log (/ (+ (reduce + (map second groups)) 0.5) (reduce + (map peek groups))))
            grid (fn [lo hi n] (map #(+ lo (/ (* (- hi lo) %) (dec n))) (range n)))
            logs (for [a (grid (- a0 4) (+ a0 4) 25)
                       lt (grid (js/Math.log 0.02) (js/Math.log 5) 16)
                       :let [t (js/Math.exp lt) alpha (/ 1 (* t t)) mu (js/Math.exp a)]]
                   [mu alpha
                    (reduce + (+ (* 0.5 a) (- (js/Math.log1p (js/Math.pow (/ t spread-scale) 2))) lt)
                            (map (fn [[_ k n-s]]
                                   (let [m (* n-s mu)]
                                     (+ (log-rising alpha k) (* alpha (js/Math.log (/ alpha (+ alpha m)))) (* k (js/Math.log (/ m (+ alpha m)))))))
                                 groups))])
            top (apply max (map peek logs))
            ws (map #(js/Math.exp (- (peek %) top)) logs)
            total (reduce + ws)]
        {:near (ffirst groups)
         :groups groups
         :points (into-array (keep identity (map (fn [[mu alpha] w] (when (< 1e-7 (/ w total)) #js [mu alpha (/ w total)])) logs ws)))}))))

(defn group-missed
  "The chance a mob at a measured distance, counts [d k n-s], gets no notice in x weighted seconds: the worse of its own
  rate's conditional posterior under the pooling, Gamma(alpha + k, alpha/mu + n-s), and its counts alone, Gamma(k + 0.5,
  n-s): pooling with calmer distances never prices a measured one below its own counts (Q16)."
  [points [_ k n-s] x]
  (min (js/Math.pow (+ 1 (/ x n-s)) (- (+ k 0.5)))
       (areduce points i acc 0
                (let [p (aget points i) mu (aget p 0) alpha (aget p 1)]
                  (+ acc (* (aget p 2) (js/Math.pow (+ 1 (/ x (+ (/ alpha mu) n-s))) (- (+ alpha k)))))))))

(defn posterior-missed
  "The chance a mob d blocks off gets no notice in x weighted seconds, over notice-posterior {:groups :points}: at a
  measured distance group-missed; between two, -ln of it straight between theirs; past the nearest or the farthest,
  the worse of the two measured distances at that end, never a trend carried on (Q16)."
  [{:keys [groups points]} d x]
  (let [at #(group-missed points (nth groups %) x)
        last-i (dec (count groups))
        end (fn [i j] (min (at i) (at (max 0 (min last-i j)))))
        hi (count (take-while #(< (first %) d) groups))]
    (cond (and (< hi (count groups)) (== d (first (nth groups hi)))) (at hi)
          (zero? hi) (end 0 1)
          (> hi last-i) (end last-i (dec last-i))
          :else (let [[d0] (nth groups (dec hi)) [d1] (nth groups hi) u (/ (- d d0) (- d1 d0))]
                  (js/Math.exp (+ (* (- 1 u) (js/Math.log (at (dec hi)))) (* u (js/Math.log (at hi)))))))))

(def notice-fit
  "{:shape :rate :factors} of notice-posterior: the Gamma whose chances of a notice 0.2 and 0.8 come at the exposures x1
  and x2 the posterior gives them at its nearest case, and :factors, a fn of whole blocks d: what the rate there is
  times, so the Gamma gives d's chance at x1. Memoized: a few classes."
  (memoize
    (fn [cases spread-scale]
      (let [post (notice-posterior cases spread-scale)
            solve (fn [f lo hi] (loop [lo lo hi hi i 0] (if (< i 60) (let [m (/ (+ lo hi) 2)] (if (f m) (recur m hi (inc i)) (recur lo m (inc i)))) lo)))
            at (fn [q] (js/Math.exp (solve #(< (- 1 q) (posterior-missed post (:near post) (js/Math.exp %))) -12 12)))
            x1 (at 0.2)
            x2 (at 0.8)
            spread (fn [s] (/ (- (js/Math.pow 0.2 (/ -1 s)) 1) (- (js/Math.pow 0.8 (/ -1 s)) 1)))
            shape (js/Math.exp (solve #(< (/ x2 x1) (spread (js/Math.exp %))) -8 12))
            rate (/ x1 (- (js/Math.pow 0.8 (/ -1 shape)) 1))]
        {:shape shape :rate rate
         :factors (memoize (fn [d] (* (/ rate x1) (- (js/Math.pow (posterior-missed post d x1) (/ -1 shape)) 1))))}))))

(defn class-cases
  "The ::notice-rate cases of mob's class with seconds counted: the counts of one case as one."
  [mob]
  (let [counts (get (settings/get settings ::notice-rate) (if (threat/ranged? {:name mob}) :ranged :other))]
    (filterv #(pos? (:n-s %)) (if (map? counts) [counts] counts))))

(defn case-fit
  "notice-fit of mob's class, nil when it has fewer than two cases."
  [mob]
  (let [cases (class-cases mob)]
    (when (< 1 (count cases))
      (notice-fit cases (settings/get settings ::notice-spread-scale)))))

(defn notice-prior
  "{:shape :rate} of the Gamma over the notice rate of mob, per weighted second, the mobs of a class sharing the one
  rate: its class's ::notice-rate cases fitted (notice-fit), the rate at the nearest case's distance (times
  notice-factor elsewhere); one case, the posterior (k + 0.5, n-s) of its counts."
  [mob]
  (if-let [fit (case-fit mob)]
    (select-keys fit [:shape :rate])
    (let [[{:keys [k n-s]}] (class-cases mob)]
      {:shape (+ (or k 0) 0.5) :rate (max 1e-9 (or n-s 0))})))

(defn notice-factor
  "A fn of a cell x y z: what mob's notice rate is times there (notice-fit's factors, the distance from the cell's
  middle to pos, at most threat/fire-range for a ranged mob; with no pos the most of any distance, the worse case), or
  nil when its class's cases give no distance."
  [mob pos]
  (let [fit (case-fit mob)]
    (when (and fit (some :blocks (class-cases mob)))
      (let [far (if (threat/ranged? {:name mob}) (threat/fire-range mob) threat/max-follow-range)
            ^js f (into-array (map (:factors fit) (range (+ 2 (js/Math.ceil far)))))
            at (fn [d] (let [d (min d far) i (js/Math.floor d)] (+ (aget f i) (* (- d i) (- (aget f (inc i)) (aget f i))))))
            most (apply max f)]
        (if pos
          (fn [x y z] (at (js/Math.hypot (- (+ x 0.5) (:x pos)) (- y (:y pos)) (- (+ z 0.5) (:z pos)))))
          (constantly most))))))

(defn missed
  "The chance a mob of notice prior {:shape :rate} gets no notice in x weighted seconds: e^(-r x) over the Gamma of r."
  [{:keys [shape rate]} x]
  (js/Math.pow (+ 1 (/ x rate)) (- shape)))

(def rate-nodes
  "[[r w]] of the Gamma notice prior {:shape :rate}: a trapezoid rule in log r, where the chances of the rate are smooth,
  64 nodes from 14/shape + 3 below the log mean to 3 above it, the weights summing to 1. Memoized: a few priors."
  (memoize
    (fn [{:keys [shape rate]}]
      (let [mid (js/Math.log (/ shape rate))
            lo (- mid 3 (/ 14 shape))
            h (/ (- (+ mid 3) lo) 63)
            us (map #(+ lo (* h %)) (range 64))
            logs (map #(- (* shape %) (* rate (js/Math.exp %))) us)
            top (apply max logs)
            ws (map #(js/Math.exp (- % top)) logs)
            total (reduce + ws)]
        (mapv (fn [u w] [(js/Math.exp u) (/ w total)]) us ws)))))

(defn cell-seconds
  "The seconds spent on each of route cells [{:x :y :z :s}]: a cell's :s, else the walk to it from the cell before (a
  block for the first)."
  [cells]
  (let [speed (value/walk-blocks-per-s)]
    (mapv (fn [prev {:keys [x y z s]}]
            (or s (/ (if prev (js/Math.hypot (- x (:x prev)) (- y (:y prev)) (- z (:z prev))) 1) speed)))
          (cons nil cells) cells)))

(defn met-stretch
  "{:met :exposure :prior :xs} of a threat mob at pos whose reach (a fn of a cell x y z, its weight 0-1, nil outside) holds
  route cells [{:x :y :z :s}]: xs the weighted seconds sum(w f dt) up to and with each cell, dt its cell-seconds (dts,
  when the caller has them), f its notice-factor (1 when none), exposure the whole sum; prior its notice-prior and met
  the chance it gets to the body, 1 - missed."
  ([mob reach cells] (met-stretch mob reach cells (cell-seconds cells) nil))
  ([mob reach cells dts] (met-stretch mob reach cells dts nil))
  ([mob reach cells dts pos]
   (let [prior (notice-prior mob)
         factor (or (notice-factor mob pos) (constantly 1))
         xs (vec (rest (reductions + 0 (map (fn [{:keys [x y z]} dt]
                                              (let [x (js/Math.floor x) y (js/Math.floor y) z (js/Math.floor z)
                                                    w (or (reach x y z) 0)]
                                                (if (pos? w) (* dt w (factor x y z)) 0)))
                                            cells dts))))
         e (or (peek xs) 0)]
     {:met (- 1 (missed prior e)) :exposure e :prior prior :xs xs})))

(defn spawn-horizon-s [] (settings/get settings ::spawn-horizon-s))

(defn spawn-worth
  "Seconds a respawn point at `at` is worth to body: the deaths expected over ::spawn-horizon-s (::deaths-per-hour) times
  what each costs more from the body's own :respawn (none: world spawn) than from `at`."
  [body at]
  (* (settings/get settings ::deaths-per-hour) (/ (spawn-horizon-s) 3600)
     (max 0 (- (death-cost body at) (death-cost (assoc body :respawn at) at)))))

