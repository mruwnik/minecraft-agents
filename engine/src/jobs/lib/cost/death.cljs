(ns jobs.lib.cost.death
  "Death as an ordinary cost, in seconds like every other: the time to get the carried items back plus what the lost
  experience is worth. A route pays chance-of-dying x death-cost. All are stubs to expand; each takes the body, which
  today only experience-cost and death-cost read. Pure.

  body: {:level experience levels, :respawn {:x :y :z} where it would wake}.

  A remembered death fades: death-weight. One by terrain (a fall, water, lava, a wall) costs go-to's plans near its spot
  as terrain-danger."
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
   ::memory-ms {:default 7200000 :spec (a/num-in 1 nil)
                :doc "Milliseconds over which the memory of a death fades from full weight to none."}
   ::terrain-rate {:default 4 :spec (a/num-in 0 nil)
                   :doc "hp a second a fresh death by terrain costs a plan within ::terrain-close of its spot (the planner's options.dangers rate)."}
   ::terrain-close {:default 1 :spec (a/num-in 0 nil)
                    :doc "Blocks round a death by terrain within which it costs its full rate."}
   ::terrain-radius {:default 4 :spec (a/num-in 0 nil)
                     :doc "Blocks round a death by terrain past which it costs nothing: the cells that killed are near the spot."}})

(defn death-weight
  "1 for a death just now, falling to 0 at ::memory-ms age."
  [age-ms]
  (max 0 (min 1 (- 1 (/ age-ms (settings/get settings ::memory-ms))))))

(defn terrain-danger
  "The planner danger {:x :y :z :close :radius :rate :mob :block} of a death by terrain `cause` at pos, its rate ::terrain-rate x weight
  (death-weight): never a block, the body may cross the spot."
  [{:keys [pos cause weight]}]
  {:x (:x pos) :y (:y pos) :z (:z pos) :close (settings/get settings ::terrain-close) :radius (settings/get settings ::terrain-radius)
   :rate (* weight (settings/get settings ::terrain-rate)) :mob cause :block false})

(defn experience-cost
  "Seconds the experience levels of body are worth."
  [body]
  (* (settings/get settings ::seconds-per-level) (or (:level body) 0)))

(defn death-cost
  "Seconds a death at `at` ({:x :y :z}) costs: the walk from the body's :respawn to `at` and back for the items (a trip
  and the walk price), plus experience-cost. A body with no :respawn pays the trip only."
  [body at]
  (let [from (:respawn body)
        dist (if (and from at)
               (js/Math.hypot (- (:x from) (:x at)) (- (:y from) (:y at)) (- (:z from) (:z at)))
               0)]
    (+ (value/trip) (:cost (value/walk-cost {:distance dist})) (experience-cost body))))

(defn death-chance
  "The chance of dying on a route {:underground? :night?}; the body is not read yet."
  [_body {:keys [underground? night?]}]
  (settings/get settings (cond underground? ::chance-underground night? ::chance-surface-night :else ::chance-surface-day)))

(defn route-death-cost
  "What a route adds to its cost for the death it risks: death-chance x death-cost at `at` (where it would die)."
  [body route at]
  (* (death-chance body route) (death-cost body at)))
