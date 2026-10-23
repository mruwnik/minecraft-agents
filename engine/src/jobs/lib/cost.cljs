(ns jobs.lib.cost
  "The cost and estimate calculators, in one place. Pure: they take data (equipment, mobs, items, a kind-at lookup) and
  never read the world themselves. They compose: armour under mob threat, mob threat under fight cost, route danger and
  the planner's rate; route danger under fetch cost.

    armour   armour-stats, after-armour, equipment-of: vanilla's per-hit formula, the only one; fall-damage, route-fall-damage, fall-profile: a fall's (a route's drops') hp and the drops a body takes
             (jobs.lib.cost.armour).
    threat   hostile?, mob-hurt: what one mob does over some seconds, after armour (jobs.lib.cost.threat).
    weapon   attack-gap-ms, weapon-damage, remaining-health, mob-max-health, ranged-mobs: the fight inputs; the one tool-material table
             (tool-tier, weapon-rank, cheapness) (jobs.lib.cost.weapon).
    fight    fight-damage, fight-seconds: the damage (and length) of a fight with the current kit (jobs.lib.cost.fight).
    answer   fight-answer, followed, flee-answer, build-answer, cover-answer, answer-price, decide: what each answer to hostiles costs and the cheapest; kill-price: a kill of one mob; death-chance, route-death-cost, route-hurt, threat-hurt, threats-chance, spawns-chance, lethal-hp: the one chance of dying (route-hurt the same in hp, at most the life; threat-hurt one mob met; water-factors the planner's price of a second in water, a drowned met there; threats-chance its known threats, spawns-chance that of mobs spawned beside the body, lethal-hp a death chance in hp) (a known threat priced by the answer the body would take) and a route's death price; blast-hurt: a creeper's blast (jobs.lib.cost.answer).
    danger   route-danger, straight-route: the danger of a walk past mobs; hurt-rate, exposure-price: what standing exposed to them costs; stance, danger-rate, threat-factor: go-to's
             planner dangers; reach-first?, wins-race?: a body's race to a refuge against its chasers; catches?, caught-on?: whether a chaser hits a walk (straight, or a timed route) before it is past; arrival-s: a chaser's seconds to a point (over its way when given); body-speed; flee-walk-speed, flee-stop-s: a flight's measured pace, its steps and the stops between them (jobs.lib.cost.danger).
    health   hp-seconds, health-scale, hp-price, commute-s (a walk there and back against sheltering here), drown-prices, damage-budget, survivable-budget: what an hp costs and how many a walk may spend (jobs.lib.cost.health).
    food     food-reserve: the food a body keeps carried, 3 days of it; cook-worth?, cook-gain: whether cooking raw food beats eating it now; meat-factor, meat-animal: raw meat's replacement cost, dear for hunted, 1 with a working pen (jobs.lib.cost.food).
    value    item-value, reserve-cost (using a reserved unit: value x s/(1-s)), fetch-cost, walk-cost, walk-beats-making (walk to what exists vs make one and place it), detour-cost: what items are worth, what fetching them, walking or a pick-up detour costs; tour-cost, tour-worth, food-worth: a tour of
             frontiers against what its goal is worth; cover-tour-s: the walk that brings a set of points within reach; light-cost: a torch go-to places; light-wait-worth: what n torches fetched save over a wait for light; dig-cost: the seconds a block takes to dig, kill-drops: the items a kill drops (jobs.lib.cost.value);
             per-dark, dark-factor: the one price of a dark block, which go-to's planner costs; dark-way-s, torch-gap: a dark way lit as it goes; spawn-chance: the chance a mob spawns at a cell or
             over an area (a set or {:min :max} box of cells) per second, from the light the body knows (unseen counts as dark), spawn-share its share of cells that spawn; exposure: the chance a walk meets and
             what lowers it ({:chance :lowered-by #{:light}}).
    planner  planner-costs-problem, planner-costs, planner-names: go-to's :costs, the per-move prices it may override; landing-problem, planner-landing: go-to's :landing, fall damage by landing block; air-profile, water-breathing-s, air-used, air-costs: the planner's air drain and grace from the body's helmet, the air a dive has used; air-left-s, wet-move-s, wet-way-s, wet-spare-s, wet-ok?: the air a walk over unseen water keeps back at its last breath, each move at its measured pace; swim-profile: its swimH from Depth Strider on the boots; gait-problem, gait-costs: go-to's :gait; unknown-prices: the planner's prices of unseen cells (jobs.lib.cost.planner); unseen-route-s, unseen-route-hp: those prices of a route's unseen steps, in seconds or hp; unseen-walk-s: with a drop onto an unseen end; unseen-way-cost: a flight's unseen part at the worse case (a dead end, a mob in the dark); flight-caught-chance: the chance a flight is caught, from its search's ways.
    tolls    farm-tolls, zone-tolls: go-to's :tolls for planted cells and zone cells to cross only as a last resort; fail-chance,
             failing-step-s: a step the walker failed at, priced by its failures (jobs.lib.cost.tolls).
    death    experience-cost, death-cost: death as an ordinary cost, the walk back for the items plus the experience; spawn-worth: a respawn point's worth; worth-to, find-worth, items-find-worth, leg-price, search-radius, wait-price: what a find is worth, a search leg's price, how far its legs may go, what waiting for a listed maker costs; opening-price, hold-price: opening a shelter to mobs against holding on in it; death-weight, terrain-danger: a remembered death's fade and the plan cost near a death by terrain (jobs.lib.cost.death).
    base-rates  density, biome-chance, expected-density, peak-density: the ore a block of ground is expected to hold, from the game's
             own placements, host-blocks: the rock its veins replace, and the chance of a biome on ground the body has not seen (jobs.lib.cost.base-rates).
    look     look-answer, look-value, closed-chance, look-s: the value of a look before deciding, a boundary's unseen part closed (jobs.lib.cost.look).
    find     look-chance, find-chance, searched-for?: the chance a look round finds the animals a search is for, from their base rate and the ground already searched in vain, looked-area: that ground; ore-of, seen-ore, tunnel-ore, ore-chance: an ore's blocks expected among the faces a look sees or a tunnel cuts and the chance of a vein there; ore-level: the height a tunnel for it runs at; leg-chance, look-leg-chance (from what the body saw), tunnel-chance: a search leg's or a tunnel leg's chance of a find; leg-stops?: a search's :price stop; search-s: its seconds per find, leg-spacing: the blocks between its legs (jobs.lib.cost.find)."
  (:require [clojure.string :as str]
            [engine.settings :as settings]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.find :as find]
            [jobs.lib.cost.look :as glance]
            [jobs.lib.cost.food :as food]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.planner :as planner]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.tolls :as tolls]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.table :as table]
            [jobs.lib.cost.value :as value]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.torch-plan :as torch-plan]
            [jobs.lib.util :as u]))

(def equipment-of armour/equipment-of)
(def armour-stats armour/armour-stats)
(def after-armour armour/after-armour)
(def fall-damage armour/fall-damage)
(def fall-profile armour/fall-profile)
(def route-fall-damage armour/route-fall-damage)

(def hostile? threat/hostile?)
(def mob-hurt threat/mob-hurt)

(def attack-gap-ms weapon/attack-gap-ms)
(def weapon-damage weapon/weapon-damage)
(def remaining-health weapon/remaining-health)
(def ranged-mobs weapon/ranged-mobs)
(def tool-tier weapon/tool-tier)
(def weapon-rank weapon/weapon-rank)
(def cheapness weapon/cheapness)

(def default-reserve fight/default-reserve)
(def fight-damage fight/fight-damage)
(def fight-seconds fight/fight-seconds)
(def walk-speed fight/walk-speed)

(def fight-answer answer/fight-answer)
(def followed answer/followed)
(def hit-range answer/hit-range)
(def flee-answer answer/flee-answer)
(def build-answer answer/build-answer)
(def cover-answer answer/cover-answer)
(def answer-price answer/answer-price)
(def decide answer/decide)
(def look-answer glance/look-answer)
(def look-value glance/look-value)
(def closed-chance glance/closed-chance)
(def look-s glance/look-s)
(def blast-hurt answer/blast-hurt)
(def swell-start answer/swell-start)
(def leaves-reserve? fight/leaves-reserve?)

(def route-danger danger/route-danger)
(def straight-route danger/straight-route)
(def stance danger/stance)
(def danger-rate danger/danger-rate)
(def hurt-rate danger/hurt-rate)
(def exposure-price danger/exposure-price)
(def danger-opts danger/danger-opts)
(def threat-factor danger/threat-factor)
(def threat-factor-cap danger/threat-factor-cap)
(def max-dangers danger/max-dangers)
(def danger-stances danger/stances)
(def danger-max-rate danger/max-rate)
(def danger-cap danger/danger-cap)
(def reach-first? danger/reach-first?)
(def wins-race? danger/wins-race?)
(def outruns? danger/outruns?)
(def catches? danger/catches?)
(def caught-on? danger/caught-on?)
(def arrival-s danger/arrival-s)
(defn melee-reach [] (danger/melee-reach))
(def body-speed danger/body-speed)
(def flee-walk-speed danger/flee-walk-speed)
(defn flee-stop-s [] (danger/flee-stop-s))
(def mob-kind danger/mob-kind)
(def chase-speed danger/chase-speed)

(def hp-seconds health/hp-seconds)
(def walk-hp-seconds health/walk-hp-seconds)
(def health-scale health/health-scale)
(def hp-price health/hp-price)
(def hp-cost health/hp-cost)
(def commute-s health/commute-s)
(def drown-prices health/drown-prices)
(def damage-budget health/damage-budget)
(def survivable-budget health/survivable-budget)

(def planner-costs-problem planner/planner-costs-problem)
(def planner-costs planner/planner-costs)
(def air-profile planner/air-profile)
(def water-breathing-s planner/water-breathing-s)
(def air-used planner/air-used)
(def swim-profile planner/swim-profile)
(def air-costs planner/air-costs)
(def air-left-s planner/air-left-s)
(def wet-move-s planner/wet-move-s)
(def policy-gait planner/policy-gait)
(def wet-way-s planner/wet-way-s)
(def wet-spare-s planner/wet-spare-s)
(def wet-ok? planner/wet-ok?)
(def planner-names planner/planner-names)
(def landing-problem planner/landing-problem)
(def planner-landing planner/planner-landing)
(def walk-costs planner/walk-costs)
(def walk-landing planner/walk-landing)
(def walk-drop-cost planner/walk-drop-cost)
(def walk-unknown-prices planner/walk-unknown-prices)
(def gaits planner/gaits)
(def gait-problem planner/gait-problem)
(def gait-costs planner/gait-costs)
(def unknown-prices planner/unknown-prices)

(defn unseen-route-s
  "Seconds the steps of route ([{:x :y :z}] cells) add over cells kind-at reads :ground (a seen-lookup's unseen cell): the
  planner's unknown prices (walk-unknown-prices :cell per unseen feet or head cell, :floor per unseen floor) of a block's
  seconds (per-block)."
  [kind-at route]
  (let [{:keys [cell floor]} (planner/walk-unknown-prices)
        unseen (fn [x y z price] (if (keyword-identical? :ground (kind-at x y z)) price 0))]
    (* (value/per-block)
       (reduce + 0 (for [{:keys [x y z]} route]
                     (+ (unseen x y z cell) (unseen x (inc y) z cell) (unseen x (dec y) z floor)))))))

(defn unseen-walk-s
  "unseen-route-s of route, plus the planner's :peek-s when its last cell stands on a floor kind-at reads :ground: the
  worse case, a drop onto it (a stop at the edge to look)."
  [kind-at route]
  (let [{:keys [x y z]} (peek route)]
    (+ (unseen-route-s kind-at route)
       (if (and x (keyword-identical? :ground (kind-at x (dec y) z))) (:peek-s (planner/walk-unknown-prices)) 0))))

(defn unseen-way-cost
  "Seconds the unseen part of a route adds to its price, each unseen cell at its worse case but finite: the chance
  (planner ::dead-end-chance, under ground when underground?) that the way ends unseen, times turning back (unseen-s seconds in and as many out,
  plus trapped, what the chasers met on the way back cost, nil none); and a mob that spawns or waits there (an
  unseen cell counts as dark): dark-factor of kit over unseen-s."
  [{:keys [unseen-s trapped kit underground?]}]
  (+ (* (planner/dead-end-chance underground?) (+ (* 2 unseen-s) (or trapped 0)))
     (* (value/dark-factor kit) unseen-s)))

(defn flight-caught-chance
  "The chance a flight is caught (flee-answer :caught) by what its search found: ways, the ring routes it planned;
  caught, those a chaser cuts; unseen, how many of the rest run through cells the body has not seen. 1 when every way
  is cut or none was planned; else the measured prior (answer ::caught-chance) times (caught + 1)/(ways + 1), the
  share cut counted with one more of each (a clear field lowers it, never to 0), and, when every way left runs
  unseen, the chance each ends there (planner ::dead-end-chance, under ground when underground?)."
  [{:keys [ways caught unseen underground?]}]
  (if (>= caught ways)
    1
    (let [cut (* (settings/get answer/settings ::answer/caught-chance) (/ (inc caught) (inc ways)))
          dead (if (= (or unseen 0) (- ways caught)) (js/Math.pow (planner/dead-end-chance underground?) unseen) 0)]
      (- 1 (* (- 1 cut) (- 1 dead))))))

(defn unseen-route-hp
  "unseen-route-s in hp, at walk-hp-seconds an hp."
  [kind-at route]
  (/ (unseen-route-s kind-at route) (health/walk-hp-seconds)))

(def item-value value/item-value)
(def fetch-cost value/fetch-cost)
(def reserve-cost value/reserve-cost)
(def walk-cost value/walk-cost)
(defn own-place-blocks [] (value/own-place-blocks))
(def walk-beats-making value/walk-beats-making)
(defn walk-blocks-per-s [] (value/walk-blocks-per-s))
(defn per-block [] (value/per-block))
(def dig-cost table/dig-cost)
(def kill-drops table/kill-drops)
(def smelt-step "Seconds one smelt step takes beyond its input." (:smelt-step table/default-basis))
(def detour-cost value/detour-cost)
(def tour-cost value/tour-cost)
(def tour-worth value/tour-worth)
(def cover-tour-s value/cover-tour-s)
(def food-worth value/food-worth)
(def animal-worth value/animal-worth)
(def expected-drops value/expected-drops)
(def food-point-worth value/food-point-worth)
(defn trip [] (value/trip))
(def light-cost value/light-cost)
(def per-danger value/per-danger)
(def spawn-chance value/spawn-chance)
(def spawn-share value/spawn-share)

(defn near-body?
  "Whether the cell `to` lies within sh-inside/spawn-free-radius of the body's feet: no mob spawns that near a player, so dark there is no danger to go to or work at."
  [p to]
  (let [feet (sh-sense/feet p)]
    (<= (u/dist feet (update to :y #(or % (:y feet)))) sh-inside/spawn-free-radius)))

(defn exposure
  "{:chance :lowered-by} of the walk of p to `to` ({:x :y :z}, a column {:x :z} at the feet's height; floored): the mob spawn chance per second it meets and
  what lowers it (with :light-in, the seconds until light, when :light does), data the readiness checks read (empty: nothing foreseeable does, so never). A place outside the shelter
  (sh-inside/walk-leaves-shelter?, with the body's latest :shelter entry data when given) that p has no light data of is guessed under open sky; any
  other the spawn-chance at `to` from the light p has seen; :light lowers a chance that darkness alone puts there and light-in says waiting lifts, but only
  at a cell the sky reaches down its column (under leaves too): a roofed dark cell (a stair, a cave) lowers nothing; ready/at-status prices it only in a dark hall. A cell p
  has no light data of is guessed dark and lowered by #{:look}, with :look-at [[x y z]]. A solid `to` is judged by the first seen open cell over it (the air over a pile, not the unseen logs between), else the cell over it (a solid cell has no light that night changes). Lava and the like are not spawn
  chance. A dark cell has none when no dark spawnable cell beyond the spawn-free radius of it is reachable on foot (sh-inside/walled-off-at?: a closed seen region without a way in). A crude stub."
  ([p to] (exposure p to nil))
  ([p to shelter]
   (let [to (update-vals (assoc to :y (or (:y to) (:y (sh-sense/feet p)))) js/Math.floor)
         solid? (sh-sense/solid-at? p to)
         over (fn [y] (assoc to :y y))
         open-over (when solid? (first (filter #(and (sh-sense/seen-cell? p (over %)) (not (sh-sense/solid-at? p (over %)))) (range (inc (:y to)) (+ (:y to) 32)))))
         cell [(:x to) (or open-over (if solid? (inc (:y to)) (:y to))) (:z to)]
         dark (look/dark-light)
         made-out? (fn [at] (or (sh-sense/seen-cell? p at) (look/glimpse? p at)))
         block-of (fn [c] (let [[x y z] c
                                at {:x x :y y :z z}]
                            (when (made-out? at) (sh-sense/block-light-at p at))))
         raw-light (fn [c] (when-let [block (block-of c)] {:block block :sky (or (sh-sense/sky-light-at p (let [[x y z] c] {:x x :y y :z z})) 0)}))
         open-sky (fn [c] {:block (or (block-of c) 0) :sky 15})
         lit-by (fn [raw darkening] (fn [c] (when-let [{:keys [block sky]} (raw c)] {:block block :sky (if (< (- sky darkening) dark) 0 15)})))
         leaves? (sh-inside/walk-leaves-shelter? p shelter to 1)
         raw (if leaves? (fn [c] (or (raw-light c) (open-sky c))) raw-light)
         darkening (sh-sense/sky-subtract-now p)
         at {:x (nth cell 0) :y (nth cell 1) :z (nth cell 2)}
         dark-chance (spawn-chance cell {:light (lit-by raw darkening)})
         chance (if (and (pos? dark-chance) (not leaves?) (raw cell) (sh-inside/walled-off-at? p at)) 0 dark-chance)]
     (if (and (not leaves?) (nil? (raw cell)))
       {:chance chance :lowered-by #{:look} :look-at [cell]}
       (let [open-sky? (or leaves? (sh-sense/sky-reaches-from-above? p {:x (nth cell 0) :y (nth cell 1) :z (nth cell 2)}))
             wait (when (and open-sky? (> chance (spawn-chance cell {:light (lit-by raw 0)})))
                    (if leaves? (sh-sense/sky-in p) (sh-sense/light-in p {:x (nth cell 0) :y (nth cell 1) :z (nth cell 2)})))]
         (if (and wait (pos? wait))
           {:chance chance :lowered-by #{:light} :light-in wait}
           {:chance chance :lowered-by #{}}))))))
(defn per-dark [] (value/per-dark))
(defn dark-factor ([] (value/dark-factor)) ([kit] (value/dark-factor kit)))

(def farm-tolls tolls/farm-tolls)
(def zone-tolls tolls/zone-tolls)
(def fail-chance tolls/fail-chance)
(def failing-step-s tolls/failing-step-s)

(def food-reserve food/food-reserve)
(def food-short food/food-short)
(def food-reserve-points food/reserve-points)
(def cook-worth? food/cook-worth?)
(def cook-gain food/cook-gain)
(def meat-factor food/meat-factor)
(def meat-animal food/meat-animal)

(def experience-cost death/experience-cost)
(def death-cost death/death-cost)
(def death-chance answer/death-chance)
(def route-hurt answer/route-hurt)
(def threats-chance answer/threats-chance)
(def spawns-chance answer/spawns-chance)
(def lethal-hp answer/lethal-hp)
(def threat-hurt answer/threat-hurt)
(def water-factors answer/water-factors)
(def route-death-cost answer/route-death-cost)
(def kill-price answer/kill-price)
(def spawn-worth death/spawn-worth)
(defn spawn-horizon-s [] (death/spawn-horizon-s))
(def death-weight death/death-weight)
(def terrain-danger death/terrain-danger)
(defn terrain-radius [] (death/terrain-radius))

(def density base-rates/density)
(def biome-chance base-rates/biome-chance)
(def expected-density base-rates/expected-density)
(def peak-density base-rates/peak-density)
(def host-blocks base-rates/host-blocks)

(def find-chance find/find-chance)
(def look-chance find/look-chance)
(def search-s find/search-s)
(def leg-chance find/leg-chance)
(def leg-stops? find/leg-stops?)
(def ore-of find/ore-of)
(def seen-ore find/seen-ore)
(def tunnel-ore find/tunnel-ore)
(def tunnel-chance find/tunnel-chance)
(def ore-level find/ore-level)
(def ore-chance find/ore-chance)
(defn leg-spacing [] (find/leg-spacing))
(def looked-area find/looked-area)
(def searched-for? find/searched-for?)

(defn look-leg-chance
  "leg-chance of a search for names by the body p, its looks of radius r: an animal's over the ground the notes ns
  searched in vain round [x z], an ore's over the faces of its host rock p has seen within r."
  [p ns names [x z] r]
  (leg-chance names {:looked (looked-area ns names [x z])
                     :a (* js/Math.PI r r)
                     :seen (when-let [ore (ore-of names)]
                             (look/seen-blocks p {:names (vec (host-blocks ore)) :radius r :max js/Infinity}))}))

(def torch-gap "Blocks of a dark way one torch lights: its light's spread (jobs.lib.torch-plan) to each side." (inc (* 2 torch-plan/spread)))

(defn dark-way-s
  "Seconds a dark way of s seconds over n blocks costs lit as it goes: s at the dark price (dark-factor) and a torch
  (light-cost, carried of them carried) every torch-gap blocks."
  [s n carried]
  (+ (* s (inc (dark-factor))) (* (js/Math.ceil (/ n torch-gap)) (:cost (light-cost {:carried carried})))))

(defn death-body
  "The body of p as the death costs read it: :level, :respawn (the live spawn bed, sh-spawn/spawn-bed-pos) and :carried
  (the item-value of the inventory)."
  [p view]
  (let [self (.self p)]
    {:level (or (some-> self .-experience .-level) 0)
     :respawn (sh-spawn/spawn-bed-pos p view)
     :carried (:value (item-value (u/inventory p)))}))

(defn bed? [n] (or (= "bed" n) (str/ends-with? n "_bed")))

(defn worth-to
  "Seconds getting one of names is worth to body at `at`: the least item-value among them, a bed (it sets the respawn)
  with its spawn-worth on top; 0 for no names."
  [body names at]
  (if (seq names)
    (apply min (map (fn [n] (+ (:value (item-value [{:name n :count 1}]))
                               (if (bed? n) (spawn-worth body at) 0)))
                    names))
    0))

(defn leg-price
  "Seconds a search leg costs: go-to's quote q of the walk (a found route its planner's price: seconds, darkness by the
  kit, dangers and damage at the body's health; else the quote's mean time) and death-s, the death the leg risks
  (route-death-cost; jobs.lib.threats/route-death-cost for a body's way past its known threats)."
  [q death-s]
  (let [r (:route q)]
    (+ (if (:found r) (or (:priced r) (:seconds r)) (second (get-in q [:uses :job-time]))) death-s)))

(defn opening-price
  "Seconds a way out costs body (death-body keys and :health): the hurt hp it expects (route-danger's :danger of the way),
  at most its health (the rest is the death), at hp-price of its health, and the death it risks (route-death-cost of
  route at `at`)."
  [body hurt route at]
  (+ (* (min hurt (:health body)) (hp-price (:health body))) (route-death-cost body route at)))

(defn hold-price
  "Seconds a hold has cost: the seconds held and the hp lost meanwhile (hunger), each at hp-price of the health left."
  [{:keys [held-s hp-lost health]}]
  (+ held-s (* (max 0 hp-lost) (hp-price health))))

(defn light-wait-worth
  "Seconds fetching n torches to light a site is worth instead of waiting wait-s for light: the wait less placing them (light-cost)."
  [wait-s n]
  (- wait-s (* n (:place (:parts (light-cost {:carried 1}))))))

(defn find-worth
  "Seconds getting `n` of names is worth to body at `at` less a trip: a search's :worth for it (leg-price weighs each
  walk against it). Not positive: the trip takes it all."
  [body names at n]
  (- (* n (worth-to body names at)) (trip)))

(defn items-find-worth
  "Seconds getting all of `items` ({name n}: a bill, a plan's shortage) is worth less a trip: the search worth of a goal
  that needs every one of them."
  [items]
  (- (:value (item-value (map (fn [[n k]] {:name n :count k}) items))) (trip)))

(defn search-radius
  "Blocks from its origin a search's legs spacing apart may go for a find worth `worth`: the rings within it hold as
  many legs as, each at least a walk of spacing blocks, the worth pays for. The legs' price binds first."
  [worth spacing]
  (js/Math.sqrt (/ (* (max 0 worth) spacing) (* js/Math.PI planner/walk-floor))))

(defn wait-price
  "Seconds waiting wait-s for a listed job to bring a find worth `worth` costs: the share of the need-s until it is
  needed (default death/spawn-horizon-s) spent without it, all of it from need-s on."
  ([worth wait-s] (wait-price worth wait-s (death/spawn-horizon-s)))
  ([worth wait-s need-s] (* worth (min 1 (/ wait-s (max 1 need-s))))))
