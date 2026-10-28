(ns jobs.lib.cost.risk
  "How many hp of expected damage a risk may take at a health and food, the one rule (jobs.lib.price/allowed adds a goal's
  worth and the cannot-heal lift): ::risk-share of the health, cut by ::hungry-share at ::hungry-food or under. A job's
  work: the seconds it stays (work-s of its category, :click, :work or :long) and the hp they cost (work-hp: ::work-hp-rate
  a second). too-hurt?: a job's work takes more than that and would not at full health. Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::risk-share {:default 0.5 :spec (a/num-in 0 nil)
                 :doc "Share of the health the body has that the expected damage of a walk (spawns met on its dark open stretches, hostiles near its way), of a stay at an exposed place or of a risky job's work may take; above it the start check waits."}
   ::hungry-food {:default 6 :spec (a/int-in 0 20)
                  :doc "Food at or under which the body cannot sprint away and does not heal: the damage it can afford is cut by ::hungry-share."}
   ::hungry-share {:default 0.5 :spec (a/num-in 0 1)
                   :doc "Share of the affordable damage a body at ::hungry-food or under may risk."}
   ::place-stay-s {:default 10 :spec (a/num-in 0 nil)
                   :doc "Seconds a click act (a door, a lever, one block, a trade) is taken to stay at its place."}
   ::work-stay-s {:default 120 :spec (a/num-in 0 nil)
                  :doc "Seconds a job that works a place for a while (a field, a pen, a tree, an apiary) is taken to stay at its place."}
   ::long-stay-s {:default 600 :spec (a/num-in 0 nil)
                  :doc "Seconds a mine, a strip or a build is taken to stay at its place."}
   ::work-hp-rate {:default 0.0032 :spec (a/num-in 0 nil)
                   :doc "hp a second of a job's own work costs (a fall, gravel, lava, a mob met at the face). Measured: 117.6 hp of hurt over 36691 s of the work of the jobs that bind a fit? (dig, stair, tunnel, mine, pen...; 81.8 hp / 32597 s of digging), walks, flights and fights not counted (go-to and the fight price those), the 154 bodies' event logs of 2026-10-28."}})

(def stay-settings {:click ::place-stay-s :work ::work-stay-s :long ::long-stay-s})

(defn work-s "Seconds a job's work of category (:click, :work, :long) stays at its place." [category]
  (settings/get settings (stay-settings category)))

(defn work-hp "hp of expected damage a job's work of category costs: ::work-hp-rate over its work-s." [category]
  (* (settings/get settings ::work-hp-rate) (work-s category)))

(defn share-hp
  "hp of expected damage a body at health and food may risk while waiting would heal it: ::risk-share of the health, cut
  while hungry (food unknown: not hungry)."
  [health food]
  (* (settings/get settings ::risk-share) health
     (if (some-> food (<= (settings/get settings ::hungry-food))) (settings/get settings ::hungry-share) 1)))

(defn too-hurt?
  "Whether a job's work of category (work-hp; :long, a mine's or a build's, for the body's fitness as such) is more than a
  body at health and food may risk (share-hp), and healed it may."
  [health food category]
  (let [w (work-hp category)]
    (and (> w (share-hp health food)) (<= w (share-hp 20 food)))))
