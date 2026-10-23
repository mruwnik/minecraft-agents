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
   ::step-s {:default 0.25 :spec (a/num-in 0 nil)
             :doc "Seconds a route spends in one of its cells when the cell does not say (:s): a walking step."}
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
  "The chance that blows of about hit hp, damage hp of them expected (their count a Poisson one), take the health and
  absorption of body: at least life/hit of them land."
  [{:keys [health absorption]} damage hit]
  (let [hit (max 0.1 (or hit 1))
        lam (/ (max 0 damage) hit)
        k (js/Math.ceil (/ (+ (or health 20) (or absorption 0)) hit))
        below (reduce + (take k (reductions (fn [p i] (/ (* p lam) i)) (js/Math.exp (- lam)) (iterate inc 1))))]
    (max 0 (- 1 below))))

(defn exposure-s
  "Seconds the route cells [{:x :y :z :s}] (:s the seconds spent there, default ::step-s) lie within a threat's reach
  (a fn of a cell x y z, its weight 0-1, nil outside), each cell's seconds times its weight."
  [reach cells]
  (let [step (settings/get settings ::step-s)]
    (reduce + 0 (keep (fn [{:keys [x y z s]}]
                        (when-let [w (reach (js/Math.floor x) (js/Math.floor y) (js/Math.floor z))] (* w (or s step))))
                      cells))))

(defn spawn-horizon-s [] (settings/get settings ::spawn-horizon-s))

(defn spawn-worth
  "Seconds a respawn point at `at` is worth to body: the deaths expected over ::spawn-horizon-s (::deaths-per-hour) times
  what each costs more from the body's own :respawn (none: world spawn) than from `at`."
  [body at]
  (* (settings/get settings ::deaths-per-hour) (/ (spawn-horizon-s) 3600)
     (max 0 (- (death-cost body at) (death-cost (assoc body :respawn at) at)))))

