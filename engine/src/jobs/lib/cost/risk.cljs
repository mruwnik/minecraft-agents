(ns jobs.lib.cost.risk
  "How many hp of expected damage a risk may take at a health and food, the one rule (jobs.lib.price/allowed adds a goal's
  worth and the cannot-heal lift): ::risk-share of the health, cut by ::hungry-share at ::hungry-food or under. A job's
  work: the seconds it stays (work-s of its category, :click, :work or :long) and the hp they cost at a health (work-hp:
  ::work-hurt's hits and the death a hit of at least that health risks). too-hurt?: a job's work takes more than that and would not at full health. Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.death :as death]))

(a/defargs settings
  {::risk-share {:default 0.5 :spec (a/num-in 0 nil)
                 :doc "Share of the health the body has that the expected damage of a walk (spawns met on its dark open stretches, hostiles near its way), of a stay at an exposed place or of a risky job's work may take; above it the start check waits."}
   ::hungry-food {:default 6 :spec (a/int-in 0 20)
                  :doc "Food at or under which the body cannot sprint away and does not heal: the damage it can afford is cut by ::hungry-share."}
   ::hungry-share {:default 0.5 :spec (a/num-in 0 1)
                   :doc "Share of the affordable damage a body at ::hungry-food or under may risk."}
   ::place-stay-s {:default 10 :spec (a/num-in 0 nil)
                   :doc "Seconds a click act (a door, a lever, one block, a trade) is taken to stay at its place. Not measured: no fit? binds :click."}
   ::work-stay-s {:default 120 :spec (a/num-in 0 nil)
                  :doc "Seconds a job that works a place for a while (a field, a pen, a tree, an apiary) is taken to stay at its place. Not measured yet: tools/event-counts.mjs work-hurt's :stay-s, the run of the outermost binding job, is 75 s (median 13, 669 runs)."}
   ::long-stay-s {:default 600 :spec (a/num-in 0 nil)
                  :doc "Seconds a mine, a strip or a build is taken to stay at its place. Not measured yet: work-hurt's :stay-s is 108 s (median 35, 377 runs)."}
   ::work-hurt {:default {:long {:s 17990 :hp 7 :hits 4 :lethal 0} :work {:s 20765 :hp 30 :hits 15 :lethal 0}}
                :spec (a/map-of #{:long :work} (a/map-with {:s (a/num-in 0 nil) :hp (a/num-in 0 nil) :hits (a/int-in 0 nil) :lethal (a/int-in 0 nil)}))
                :doc "The hurt of a job's own work by its fit? category (:click takes :work's): :hp and :hits (:lethal of them left health 0) over :s seconds of the innermost job binding the category, a hit taken before a reflex ran (a fall, gravel, lava, a mob at the face) counted, walks, flights, fights, reflexes and holds not (go-to and the fight price those); the bodies were gated at 6 hp, so few worked hurt. All the bodies' event logs to 2026-10-28. Recount: tools/event-counts.mjs work-hurt."}})

(def stay-settings {:click ::place-stay-s :work ::work-stay-s :long ::long-stay-s})

(defn work-s "Seconds a job's work of category (:click, :work, :long) stays at its place." [category]
  (settings/get settings (stay-settings category)))

(defn- hurt-of [category] (get (settings/get settings ::work-hurt) (if (= :click category) :work category)))

(defn hit-mean
  "hp of a mean hit of work, every category's hits pooled (19 hits, 1.95 hp, in ::work-hurt's default): hit sizes taken
  as exponential, a prior that new logs refine; a lethal hit (cut at the health it left) censored."
  []
  (let [rows (vals (settings/get settings ::work-hurt))
        whole (reduce + (map #(- (:hits %) (:lethal %)) rows))]
    (if (pos? whole) (/ (reduce + (map :hp rows)) whole) 1)))

(defn hit-rate
  "Hits a second of a job's work of category: its ::work-hurt hits over its seconds, blended with every category's pooled
  rate as a Gamma prior of one hit (Gamma(1 + hits, 1 / pooled + s), its mean): a category of few hits leans on the pool."
  [category]
  (let [rows (vals (settings/get settings ::work-hurt))
        pooled (/ (reduce + (map :hits rows)) (max 1 (reduce + (map :s rows))))
        {:keys [s hits]} (hurt-of category)]
    (if (pos? pooled) (/ (+ 1 hits) (+ (/ 1 pooled) s)) 0)))

(defn work-death-chance
  "The chance a job's work of category kills a body at health: a hit of at least health (exp(-health / hit-mean)) among
  its hits (hit-rate) over s seconds (default work-s)."
  ([category health] (work-death-chance category health (work-s category)))
  ([category health s]
   (- 1 (js/Math.exp (- (* s (hit-rate category) (js/Math.exp (- (/ health (hit-mean))))))))))

(defn work-hurt
  "hp a job's work of category hurts a body on average: hit-rate x hit-mean over work-s."
  [category]
  (* (hit-rate category) (hit-mean) (work-s category)))

(defn work-hp
  "hp a job's work of category costs a body at health: its hurt (hit-rate x hit-mean over work-s) and its death
  (work-death-chance) in hp at `death` seconds (cost/lethal-hp; cost/death-s), as price/stay-hp."
  [category health death]
  (+ (work-hurt category)
     (answer/lethal-hp {:chance (work-death-chance category health) :death death})))

(defn share-hp
  "hp of expected damage a body at health and food may risk while waiting would heal it: ::risk-share of the health, cut
  while hungry (food unknown: not hungry)."
  [health food]
  (* (settings/get settings ::risk-share) health
     (if (some-> food (<= (settings/get settings ::hungry-food))) (settings/get settings ::hungry-share) 1)))

(defn too-hurt?
  "Whether a job's work of category (work-hp; :long, a mine's or a build's, for the body's fitness as such) is more than a
  body at health and food may risk (share-hp), and healed it may; `death` seconds a death costs (cost/death-s)."
  [health food category death]
  (and (> (work-hp category health death) (share-hp health food)) (<= (work-hp category 20 death) (share-hp 20 food))))
