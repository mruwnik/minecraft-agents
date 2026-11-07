(ns jobs.lib.cost.risk
  "A job's work: the seconds it stays (work-s of its category, :click, :work or :long) and what they cost at a health
  (work-price-s: ::work-hurt's hits and the death a hit of at least that health risks). too-hurt? (Q140): going now costs
  more over going healed than the heal wait. share-hp: what a call with no worth may risk (price/allowed). Pure."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.health :as health]))

(a/defargs settings
  {::risk-share {:default 0.5 :spec (a/num-in 0 nil)
                 :doc "Share of the health the body has that the expected damage may take where no call worth prices it (a nil worth: share-hp), and the health a risky walk's heal wait heals to. A call with a worth, ##Inf included, is priced by cost alone (walk-risk/status, ready/priced)."}
   ::hungry-food {:default 6 :spec (a/int-in 0 20)
                  :doc "Food at or under which the body cannot sprint away and does not heal: the damage it can afford is cut by ::hungry-share."}
   ::hungry-share {:default 0.5 :spec (a/num-in 0 1)
                   :doc "Share of the affordable damage a body at ::hungry-food or under may risk."}
   ::place-stay-s {:default 10 :spec (a/num-in 0 nil)
                   :doc "Seconds a click act (a door, a lever, one block, a trade) is taken to stay at its place. Not measured: no fit? binds :click."}
   ::work-stay-s {:default 75 :spec (a/num-in 0 nil)
                  :doc "Seconds a job that works a place for a while (a field, a pen, a tree, an apiary) is taken to stay at its place. The mean run of the outermost binding job (tools/event-counts.mjs work-hurt :stay-s: 75 s, median 13, 669 runs); a job that quotes its own seconds uses them."}
   ::long-stay-s {:default 108 :spec (a/num-in 0 nil)
                  :doc "Seconds a mine, a strip or a build is taken to stay at its place. The mean run of the outermost binding job (work-hurt :stay-s: 108 s, median 35, 377 runs); a job that quotes its own seconds uses them."}
   ::work-hurt {:default {:long {:s 17990 :hp 7 :hits 4 :lethal 0} :work {:s 20765 :hp 30 :hits 15 :lethal 0}}
                :spec (a/map-of #{:long :work} (a/map-with {:s (a/num-in 0 nil) :hp (a/num-in 0 nil) :hits (a/int-in 0 nil) :lethal (a/int-in 0 nil)}))
                :doc "The hurt of a job's own work by its fit? category (:click takes :work's): :hp and :hits (:lethal of them left health 0) over :s seconds of the innermost job binding the category, a hit taken before a reflex ran (a fall, gravel, lava, a mob at the face) counted, walks, flights, fights, reflexes and holds not (go-to and the fight price those); the bodies were gated at 6 hp, so few worked hurt. All the bodies' event logs to 2026-10-28. Recount: tools/event-counts.mjs work-hurt."}})

(def stay-settings {:click ::place-stay-s :work ::work-stay-s :long ::long-stay-s})

(defn work-s "Seconds a job's work of category (:click, :work, :long) stays at its place." [category]
  (settings/get settings (stay-settings category)))

(defn heal-s
  "Seconds healing by waiting takes from health to full: the stay of a caller with no job (a trigger), as in walk-risk's heal wait."
  [health]
  (* (health/heal-s-per-hp) (max 0 (- 20 health))))

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
        rate (fn [{:keys [s hits]}] (/ (+ 1 hits) (+ (/ 1 pooled) s)))]
    (cond (not (pos? pooled)) 0
          (= :heal category) (apply max (map rate rows))
          :else (rate (hurt-of category)))))

(defn stay-of
  "Seconds of category's stay at health: work-s, and for :heal (a caller with no job) the heal-s wait, at the riskiest measured rate."
  [category health]
  (if (= :heal category) (heal-s health) (work-s category)))

(defn work-death-chance
  "The chance a job's work of category kills a body at health: a hit of at least health (exp(-health / hit-mean)) among
  its hits (hit-rate) over s seconds (default stay-of)."
  ([category health] (work-death-chance category health (stay-of category health)))
  ([category health s]
   (- 1 (js/Math.exp (- (* s (hit-rate category) (js/Math.exp (- (/ health (hit-mean))))))))))

(defn work-hurt
  "hp a job's work of category hurts a body on average: hit-rate x hit-mean over work-s."
  [category]
  (* (hit-rate category) (hit-mean) (work-s category)))

(defn share-hp
  "hp of expected damage a body at health and food may risk on a call with no worth (price/allowed): ::risk-share of the
  health, cut while hungry (food unknown: not hungry)."
  [health food]
  (* (settings/get settings ::risk-share) health
     (if (some-> food (<= (settings/get settings ::hungry-food))) (settings/get settings ::hungry-share) 1)))

(defn work-price-s
  "Seconds a job's work of category staying s seconds costs a body at health: its hurt (hit-rate x hit-mean) and its death
  chance (work-death-chance) at `death` seconds, priced as a walk's risk (cost.health/risk-s)."
  [category health death s]
  (health/risk-s health (* (hit-rate category) (hit-mean) s) (answer/lethal-hp {:chance (work-death-chance category health s) :death death})))

(defn heal-wait-s
  "Seconds healing to full by waiting costs a body at health: its idle time (heal-s). The food its hp eat is not counted: a
  body that goes now eats the same food healing them at work."
  [health]
  (heal-s health))

(defn hurry-s
  "Seconds going now to a job's work of category costs a body at health over going healed (at 20): work-price-s at both, over
  `seconds` (default stay-of at health), its death at each (answer/death-at), plus (present h), what the dangers about the body
  cost it at health h (nil: none)."
  [health category death {:keys [seconds present]}]
  (let [s (or seconds (stay-of category health))
        at #(+ (work-price-s category % (answer/death-at death %) s) (if present (present %) 0))]
    (- (at health) (at 20))))

(defn too-hurt?
  "Whether a body at health should heal before a job's work of category (:long, a mine's or a build's; :heal, the riskiest
  work over the heal wait, for a caller with no job): going now costs more over going healed (hurry-s, opts :seconds
  :present) than the heal wait does (heal-wait-s). `death` seconds a death costs (cost/death-s), or a fn of the health (answer/death-at)."
  ([health category death] (too-hurt? health category death nil))
  ([health category death opts]
   (> (hurry-s health category death opts) (heal-wait-s health))))

(defn averted-fn
  "fn of a health h: the seconds of death the hp from h to h + 1 averts on the riskiest work over the heal wait from health
  (work-death-chance of :heal), at `death` seconds (answer/death-at)."
  [health death]
  (let [s (stay-of :heal health)]
    (fn [h] (* (answer/death-at death h) (- (work-death-chance :heal h s) (work-death-chance :heal (inc h) s))))))
