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
                            :ranged [{:case "lit 2" :k 8 :n-s 15.2} {:case "lit 5" :k 8 :n-s 22.7} {:case "lit 9" :k 9 :n-s 51.4}
                                     {:case "lit 13" :k 8 :n-s 25.2} {:case "dark 9" :k 4 :n-s 29.7}]}
                  :spec (let [counts (a/map-with {:k (a/int-in 0 nil) :n-s (a/num-in 0 nil)})
                              class (a/or-of counts (a/coll-of counts))]
                          (a/map-with {:other class :ranged class}))
                  :doc "Per class of mob (:ranged: threat/ranged?; :other: caught within melee reach), the counts {:k :n-s} of one case or of each case counted apart: n-s the weighted seconds sightings spent in the body's field (weight 1 within a few steps of the mob's way, or a ranged mob's range, less beyond), k the catches (a melee mob: a hurt from it, or a respond decision with it in reach; a ranged mob: its first hit, at the middle of the health sample it was seen in). notice-prior pools the cases. :other: 24 trial sightings (8 censored by a position gap; zombie 6 of 13 s, creeper 2 of 29.4 s). :ranged: the world fixture ranged-notice (a bow skeleton, a clear line of fire, a body standing still), by distance in blocks and light; trial play first saw every skeleton beyond its range. Recount: node tools/event-counts.mjs notice-rate (:by-case), 2026-10-24."}
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

(defn blow-chance
  "The chance that blows of about hit hp, damage hp of them expected, take the health and absorption of body: at least
  life/hit of them land. Their count is a Poisson one, or with shape (a Gamma-distributed rate: an uncertain fit) a
  negative binomial one, the Poisson's posterior predictive. sure: hp of damage that lands for certain in big blows (a
  creeper's blast), taken off the life first: the small blows count against what it leaves, not as parts of a big one."
  ([body damage hit] (blow-chance body damage hit 0 nil))
  ([body damage hit sure] (blow-chance body damage hit sure nil))
  ([{:keys [health absorption]} damage hit sure shape]
   (let [life (- (+ (or health 20) (or absorption 0)) sure)
         hit (max 0.1 (or hit 1))
         lam (/ (max 0 (- damage sure)) hit)
         k (js/Math.ceil (/ life hit))
         [p0 step] (if shape
                     (let [q (/ lam (+ shape lam))]
                       [(js/Math.exp (* shape (js/Math.log1p (- q)))) (fn [p i] (/ (* p q (+ shape i -1)) i))])
                     [(js/Math.exp (- lam)) (fn [p i] (/ (* p lam) i))])
         below (reduce + (take k (reductions step p0 (iterate inc 1))))]
     (if (pos? life) (max 0 (- 1 below)) 1))))

(defn notice-prior
  "{:shape :rate} of the Gamma over the notice rate of mob, per weighted second, its class's ::notice-rate cases pooled
  partially: the mean (k + 0.5) / n-s of all of them, the variance that mean's own (the posterior Gamma(k + 0.5, n-s) of
  one case) plus the cases' spread round it (DerSimonian-Laird over the cases' k / n-s, each of Poisson variance mean / n-s).
  The mobs of a class share the one rate."
  [mob]
  (let [counts (get (settings/get settings ::notice-rate) (if (threat/ranged? {:name mob}) :ranged :other))
        cases (filter #(pos? (:n-s %)) (if (map? counts) [counts] counts))
        k (reduce + 0 (map :k cases))
        s (max 1e-9 (reduce + 0 (map :n-s cases)))
        m (/ k s)
        ws (map #(/ (:n-s %) (max 1e-9 m)) cases)
        q (reduce + 0 (map (fn [{:keys [k n-s]} w] (* w (js/Math.pow (- (/ k n-s) m) 2))) cases ws))
        spread (if (and (pos? m) (< 1 (count cases)))
                 (max 0 (/ (- q (dec (count cases))) (- (reduce + ws) (/ (reduce + (map * ws ws)) (reduce + ws)))))
                 0)
        mean (/ (+ k 0.5) s)
        v (+ spread (/ mean s))]
    (if (pos? spread)
      {:shape (/ (* mean mean) v) :rate (/ mean v)}
      {:shape (+ k 0.5) :rate s})))

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
  "{:met :exposure :prior :xs} of a threat mob whose reach (a fn of a cell x y z, its weight 0-1, nil outside) holds
  route cells [{:x :y :z :s}]: xs the weighted seconds sum(w dt) up to and with each cell, dt its cell-seconds (dts, when
  the caller has them), exposure the whole sum; prior its notice-prior and met the chance it gets to the body, 1 - missed."
  ([mob reach cells] (met-stretch mob reach cells (cell-seconds cells)))
  ([mob reach cells dts]
   (let [prior (notice-prior mob)
         xs (vec (rest (reductions + 0 (map (fn [{:keys [x y z]} dt] (* dt (or (reach (js/Math.floor x) (js/Math.floor y) (js/Math.floor z)) 0)))
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

