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
   ::chance-surface-day {:default 0.0005 :spec (a/num-in 0 1)
                         :doc "Chance of dying on a route over the surface by day."}
   ::chance-surface-night {:default 0.005 :spec (a/num-in 0 1)
                           :doc "Chance of dying on a route over the surface at night."}
   ::chance-underground {:default 0.002 :spec (a/num-in 0 1)
                         :doc "Chance of dying on a route underground."}
   ::notice-rate {:default {:other {:k 8 :n-s 42.4} :ranged {:k 0 :n-s 0}}
                  :spec (let [counts (a/map-with {:k (a/int-in 0 nil) :n-s (a/num-in 0 nil)})] (a/map-with {:other counts :ranged counts}))
                  :doc "Per class of mob (:ranged: threat/ranged?, caught by a line of fire within the ranged radius; :other: caught within melee reach) of the weighted seconds n-s that watch sightings of hostiles within follow range spent in the body's field (weight 1 within a few steps of the mob's way, or a ranged mob's range, less beyond), the catches k: a hurt from that kind, or a respond decision with the mob in reach; one catch ends the one sighting of the kind nearest the event (the events carry no entity id). Priced by the Gamma posterior mean (k + 0.5) / n-s, the chance a second a hostile at weight 1 gets to the body (met-chance). :other counts 24 sightings (8 ended in a position gap, censored: seconds up to it, no catch; zombie 6 of 13 s, creeper 2 of 29.4 s, too few to split by mob). :ranged has no seconds counted: every skeleton seen was first seen beyond its 15 blocks (threat/fire-range), so ::ranged-notice-rate prices it, not the melee counts. Recount: node tools/event-counts.mjs notice-rate, 2026-10-23; per-case evidence replaces it."}
   ::ranged-notice-rate {:default 2 :spec (a/num-in 0 nil)
                         :doc "Per weighted second, the notice rate of a ranged mob while its class has no seconds counted: vanilla's target goal looks for a target every 10 ticks on average (NearestAttackableTargetGoal, interval 10), so a ranged mob with a line of fire has noticed within half a second and shoots, with no approach to walk."}
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
  and the walk price), plus experience-cost. A body with no :respawn walks ::spawn-walk-blocks from world spawn and loses
  what it :carried: the pile despawns long before it gets there."
  [body at]
  (let [from (:respawn body)
        walk (if from
               (:cost (value/walk-cost {:distance (js/Math.hypot (- (:x from) (:x at)) (- (:y from) (:y at)) (- (:z from) (:z at)))}))
               (+ (:cost (value/walk-cost {:distance (settings/get settings ::spawn-walk-blocks)})) (or (:carried body) 0)))]
    (+ (value/trip) walk (experience-cost body))))

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

(defn notice-rate
  "The mean (k + 0.5) / n-s of the Gamma posterior of ::notice-rate for mob, per weighted second: its class's counts; a ranged mob's class with no seconds counted gives ::ranged-notice-rate instead (it notices fast and needs no approach, so the melee counts would understate it)."
  [mob]
  (let [counts (settings/get settings ::notice-rate)
        ranged? (threat/ranged? {:name mob})
        {:keys [k n-s]} (get counts (if ranged? :ranged :other))]
    (if (and ranged? (not (pos? n-s)))
      (settings/get settings ::ranged-notice-rate)
      (/ (+ k 0.5) (max 1e-9 n-s)))))

(defn met-chance
  "The chance a threat mob whose reach (a fn of a cell x y z, its weight 0-1, nil outside) holds route cells [{:x :y :z :s}]
  gets to the body on them: 1 - prod(1 - w r dt), r (notice-rate of mob), dt the cell's :s, else the walk to it from the cell
  before (a block for the first)."
  [mob reach cells]
  (let [r (notice-rate mob)
        speed (value/walk-blocks-per-s)]
    (- 1 (reduce (fn [miss [prev {:keys [x y z s] :as cell}]]
                   (if-let [w (reach (js/Math.floor x) (js/Math.floor y) (js/Math.floor z))]
                     (* miss (- 1 (min 1 (* w r (or s (/ (if prev (js/Math.hypot (- x (:x prev)) (- y (:y prev)) (- z (:z prev))) 1) speed))))))
                     miss))
                 1 (map vector (cons nil cells) cells)))))

(defn spawn-horizon-s [] (settings/get settings ::spawn-horizon-s))

(defn spawn-worth
  "Seconds a respawn point at `at` is worth to body: the deaths expected over ::spawn-horizon-s (::deaths-per-hour) times
  what each costs more from the body's own :respawn (none: world spawn) than from `at`."
  [body at]
  (* (settings/get settings ::deaths-per-hour) (/ (spawn-horizon-s) 3600)
     (max 0 (- (death-cost body at) (death-cost (assoc body :respawn at) at)))))

