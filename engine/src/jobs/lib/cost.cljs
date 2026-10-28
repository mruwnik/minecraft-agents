(ns jobs.lib.cost
  "The cost and estimate calculators, in one place. Pure: they take data (equipment, mobs, items, a kind-at lookup) and
  never read the world themselves. They compose: armour under mob threat, mob threat under fight cost, route danger and
  the planner's rate; route danger under fetch cost.

    armour   armour-stats, after-armour, equipment-of: vanilla's per-hit formula, the only one; fall-damage, fall-profile: a fall's hp and the drops a body takes
             (jobs.lib.cost.armour).
    threat   hostile?, mob-hurt: what one mob does over some seconds, after armour (jobs.lib.cost.threat).
    weapon   attack-gap-ms, weapon-damage, remaining-health, mob-max-health, ranged-mobs: the fight inputs; the one tool-material table
             (tool-tier, weapon-rank, cheapness) (jobs.lib.cost.weapon).
    fight    fight-damage, fight-seconds: the damage (and length) of a fight with the current kit (jobs.lib.cost.fight).
    answer   fight-answer, followed, small (an answer's small blows, the ones its :shape is the width of), flee-answer, build-answer, cover-answer, cornered-answers, answer-price, decide: what each answer to hostiles costs and the cheapest; kill-price: a kill of one mob; death-chance, route-death-cost, route-parts (the death chance and hurt of a route's threats, each pair met in one answer by the chance the second comes while the first is answered), threat-hurt, spawn-kill-chance, spawns-chance, spawns-hurt, lethal-hp: the one chance of dying (threat-hurt one mob met; water-factors the planner's price of a second in water, a drowned met there, unseen-water-factors a phantom floor's by the seen water near it; spawns-chance that of mobs spawned beside the body (spawn-kill-chance one's), spawns-hurt their hurt, lethal-hp a death chance in hp) (a known threat priced by the answer the body would take) and a route's death price; blast-hurt: a creeper's blast (jobs.lib.cost.answer).
    danger   route-danger: the danger of a walk past mobs; hurt-rate, exposure-price: what standing exposed to them costs; stance, danger-rate, threat-factor: go-to's
             planner dangers; reach-first?, wins-race?: a body's race to a refuge against its chasers; catches?, caught-on?: whether a chaser hits a walk (straight, or a timed route) before it is past; reach-s: the seconds it is within its blow of a timed route; arrival-s: a chaser's seconds to a point (over its way when given); body-speed; flee-walk-speed, flee-stop-s: a flight's measured pace, its steps and the stops between them (jobs.lib.cost.danger).
    health   hp-seconds, health-scale, hp-price, risk-s (a walk's hurt and death chance in seconds), commute-s (a walk there and back against sheltering here), drown-prices, damage-budget, survivable-budget: what an hp costs and how many a walk may spend (jobs.lib.cost.health).
    food     food-reserve: the food a body keeps carried, 3 days of it; cook-worth?, cook-gain: whether cooking raw food beats eating it now; meat-factor, meat-animal: raw meat's replacement cost, dear for hunted, 1 with a working pen (jobs.lib.cost.food).
    value    item-value, survived-worth (a find's worth times the chance the trip does not end in death), reserve-cost (using a reserved unit: value x s/(1-s)), fetch-cost, walk-cost, walk-beats-making (walk to what exists vs make one and place it), detour-cost: what items are worth, what fetching them, walking or a pick-up detour costs; tour-cost, tour-worth, food-worth, crop-worth (a ripe crop's loot): a tour of
             frontiers against what its goal is worth; cover-tour-s: the walk that brings a set of points within reach; light-cost: a torch go-to places; light-wait-worth: what n torches fetched save over a wait for light; wear-saved: the wear of a dear tool a spare's digs save; dig-cost: the seconds a block takes to dig, kill-drops: the items a kill drops (jobs.lib.cost.value);
             per-dark, dark-factor: the one price of a dark block, which go-to's planner costs; lit-way-s, torch-gap: a way lit as it goes; dark-way-s: one walked or dug dark; spawn-chance: the chance a mob spawns at a cell or
             over an area (a set or {:min :max} box of cells) per second, from the light the body knows (unseen counts as dark), spawn-share its share of cells that spawn.
    planner  planner-costs-problem, planner-costs, planner-names: go-to's :costs, the per-move prices it may override; least-block-s: the least a plan prices a block; landing-problem, planner-landing: go-to's :landing, fall damage by landing block; air-profile, water-breathing-s, air-used, air-costs: the planner's air drain and grace from the body's helmet, the air a dive has used; air-left-s, wet-move-s, wet-way-s, wet-spare-s, wet-ok?: the air a walk over unseen water keeps back at its last breath, each move at its measured pace; swim-profile: its swimH from Depth Strider on the boots; gait-problem, gait-costs: go-to's :gait; unknown-prices: the planner's peek and aim prices of unseen cells (jobs.lib.cost.planner); way-on-chance, unseen-share: the one price of walking over unseen cells, a dead end and a mob in the dark at the worse case, from how often the seen ways near the body go on (the planner's :cell); unseen-route-s: that price of a route's unseen steps; dead-end-cost: what an unseen dead end costs a flight; flight-caught-chance: the chance a flight is caught by its way, from its search's ways.
    tolls    farm-tolls, zone-tolls: go-to's :tolls for planted cells and zone cells to cross only as a last resort; fail-chance,
             failing-step-s: a step the walker failed at, priced by its failures (jobs.lib.cost.tolls).
    death    experience-cost, death-cost: death as an ordinary cost, the walk back for the items plus the experience; spawn-worth: a respawn point's worth; worth-to, find-worth, items-find-worth, search-radius, wait-price: what a find is worth, how far its legs may go, what waiting for a listed maker costs; opening-price, hold-price: opening a shelter to mobs against holding on in it; death-weight, terrain-danger: a remembered death's fade and the plan cost near a death by terrain (jobs.lib.cost.death).
    base-rates  density, biome-chance, expected-density, peak-density: the ore a block of ground is expected to hold, from the game's
             own placements, host-blocks: the rock its veins replace, and the chance of a biome on ground the body has not seen (jobs.lib.cost.base-rates).
    look     look-answer, look-value, closed-chance, look-s: the value of a look before deciding, a boundary's unseen part closed (jobs.lib.cost.look).
    find     look-chance, find-chance, searched-for?: the chance a look round finds the animals a search is for, from their base rate and the ground already searched in vain, looked-area: that ground; ore-of, seen-ore, tunnel-ore, ore-chance: an ore's blocks expected among the faces a look sees or a tunnel cuts and the chance of a vein there; ore-level: the height a tunnel for it runs at; leg-chance, look-leg-chance (from what the body saw), tunnel-chance: a search leg's or a tunnel leg's chance of a find; leg-stops?: a search's :price stop; search-s: its seconds per find, leg-spacing: the blocks between its legs (jobs.lib.cost.find).
    farm     field-share: the share of a field's unseen cells ripe or sowable, from the measured passes and its seen cells; ripe-chance: a cell seen growing ripe by now, at the game's growth rate, growing-s: the seconds of them within the server's simulation distance of the body; grown-share, sown-worth: what a crop still to grow and a sown cell are worth now; field-step-s: the measured seconds of a tend step, earthwork-s: levelling a patch's cells off height (jobs.lib.cost.farm).
    pen      gap-loss: the animal worth a herd loses through fence gaps till the next pass, from a Gamma prior on the escape rate (jobs.lib.cost.pen).
    rise     expected-rise: the blocks up to a sky the body has not seen, from its height, what it knows of the surface, and a prior on the ground height (jobs.lib.cost.rise)."
  (:require [clojure.string :as str]
            [engine.settings :as settings]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.farm :as farm]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.find :as find]
            [jobs.lib.cost.look :as glance]
            [jobs.lib.cost.pen :as pen]
            [jobs.lib.cost.rise :as rise]
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
(def small answer/small)
(def hit-range answer/hit-range)
(def flee-answer answer/flee-answer)
(def build-answer answer/build-answer)
(def cover-answer answer/cover-answer)
(def cornered-answers answer/cornered-answers)
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
(def reach-s danger/reach-s)
(def arrival-s danger/arrival-s)
(defn melee-reach [] (danger/melee-reach))
(def body-speed danger/body-speed)
(def flee-walk-speed danger/flee-walk-speed)
(defn flee-stop-s [] (danger/flee-stop-s))
(def mob-kind danger/mob-kind)
(def chase-speed danger/chase-speed)

(def hp-seconds health/walk-hp-seconds)
(def walk-hp-seconds health/walk-hp-seconds)
(def health-scale health/health-scale)
(def hp-price health/hp-price)
(def hp-cost health/hp-cost)
(def risk-s health/risk-s)
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
(def least-block-s planner/least-block-s)
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

(defn way-on-chance
  "The chance a way on through cells the body has not seen goes on: a uniform Beta(1,1) prior updated by counts {:k :n}
  of the seen stands near it (jobs.lib.walk.way-on/way-counts: n seen, k of them with a way that leads out), (k+1)/(n+2)."
  [{:keys [k n]}]
  (/ (inc (or k 0)) (+ (or n 0) 2)))

(defn dark-factor
  "The planner's extra cost of a dark cell as a share of its own seconds (options.dark.factor): value/dark-factor, the
  floor; with a kit {:weapon :equipment :health :food} the hurt of a mob spawned beside it (spawns-hurt) times
  spawn-chance of a dark cell and hp-seconds on top: a bare body pays more than an armed, armoured one."
  ([] (value/dark-factor))
  ([kit] (+ (value/dark-factor) (* (answer/spawns-hurt kit 1 nil) (value/spawn-chance [0 0 0]) (health/walk-hp-seconds)))))

(defn unseen-share
  "Seconds a second of walk over cells the body has not seen adds, at the worse case but finite: the chance the way does
  not go on (1 less way-on-chance of counts) times the walk back out, and a mob that spawns or waits there (an unseen
  cell counts as dark): dark-factor of kit. The replan at the dead end is not in it (unmeasured)."
  [counts kit]
  (+ (* 2 (- 1 (way-on-chance counts))) (dark-factor kit)))

(defn dead-end-cost
  "Seconds a route that runs on into cells the body has not seen adds past their unseen-share (its planner price):
  at the chance the way does not go on (counts), what a dead end costs: trapped (what the chasers cost where they corner the body), or the
  cheapest of backs [{:s :caught-s :unseen? :trapped}], a walk back to a fork and away along another way: its seconds
  and that way's price on, trapped once a blow lands (one each answer/melee-gap-s over caught-s, the seconds a chaser is
  within its blow: reach-s), else, when that way runs unseen too, its own dead end at the same chance and its trapped;
  never below 0."
  ([counts trapped] (dead-end-cost counts trapped nil))
  ([counts trapped backs]
   (let [dead (- 1 (way-on-chance counts))]
     (* dead (reduce min trapped (map (fn [b]
                                        (let [hit (- 1 (js/Math.exp (- (/ (:caught-s b) answer/melee-gap-s))))]
                                          (max 0 (+ (:s b) (* hit trapped) (if (:unseen? b) (* (- 1 hit) dead (:trapped b)) 0)))))
                                      backs))))))

(defn unseen-route-s
  "Seconds the steps of route ([{:x :y :z}] cells) add over cells kind-at reads :ground (a seen-lookup's unseen cell):
  share (unseen-share) of a block's seconds (per-block) for each step whose feet, head or floor is one."
  [share kind-at route]
  (let [unseen? (fn [x y z] (keyword-identical? :ground (kind-at x y z)))]
    (* share (value/per-block)
       (count (filter (fn [{:keys [x y z]}] (or (unseen? x y z) (unseen? x (inc y) z) (unseen? x (dec y) z))) route)))))

(defn flight-caught-chance
  "The chance a flight is caught by the way its search found (flee-answer :caught; jobs.lib.cost.caught/prior is added there,
  whatever the way): 1 with no way left (way? false: every way it planned is cut); 0 on a way the race calls free; when
  the way runs on into cells the body has not seen (unseen?), that way's dead end (1 less way-on-chance of counts): met
  on the way back, the chasers catch it."
  [{:keys [way? unseen? counts]}]
  (cond (not way?) 1
        unseen? (- 1 (way-on-chance counts))
        :else 0))

(def item-value value/item-value)
(def wear-saved value/wear-saved)
(def fetch-cost value/fetch-cost)
(def survived-worth value/survived-worth)
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
(def crop-worth value/crop-worth)
(def expected-drops value/expected-drops)
(def food-point-worth value/food-point-worth)
(def food-search-worth value/food-search-worth)
(defn trip [] (value/trip))
(def field-share farm/share)
(def ripe-chance farm/ripe-chance)
(def growing-s farm/growing-s)
(def grown-share farm/grown-share)
(def sown-worth farm/sown-worth)
(def field-step-s farm/step-s)
(def gap-loss pen/gap-loss)
(def earthwork-s farm/earthwork-s)
(def light-cost value/light-cost)
(def per-danger value/per-danger)
(def spawn-chance value/spawn-chance)
(def spawn-share value/spawn-share)

(defn near-body?
  "Whether the cell `to` lies within sh-inside/spawn-free-radius of the body's feet: no mob spawns that near a player, so dark there is no danger to go to or work at."
  [p to]
  (let [feet (sh-sense/feet p)]
    (<= (u/dist feet (update to :y #(or % (:y feet)))) sh-inside/spawn-free-radius)))

(defn per-dark [] (value/per-dark))

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
(def route-parts answer/route-parts)
(def spawn-kill-chance answer/spawn-kill-chance)
(def spawns-chance answer/spawns-chance)
(def spawns-hurt answer/spawns-hurt)
(def lethal-hp answer/lethal-hp)
(def threat-hurt answer/threat-hurt)
(def water-factors answer/water-factors)
(def unseen-water-factors answer/unseen-water-factors)
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
(def expected-rise rise/expected-rise)
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

(defn lit-way-s
  "Seconds a way of s seconds over n blocks costs lit as it goes (the mine's stair and tunnel): s and a torch every
  torch-gap blocks, each its worth and placing (light-cost of one carried)."
  [s n]
  (+ s (* (js/Math.ceil (/ n torch-gap)) (:cost (light-cost {:carried 1})))))

(defn dark-way-s
  "Seconds a way of s seconds over n blocks costs walked or dug dark: s, its n blocks' walk at the planner's dark price
  (dark-factor), and the hurt a spawn costs the body of kit over its s seconds (the kit's share of dark-factor, a rate)."
  [s n kit]
  (+ s (* (dark-factor) (:cost (walk-cost {:distance n}))) (* (- (dark-factor kit) (dark-factor)) s)))

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

(defn opening-price
  "Seconds a way out costs body (death-body keys and :health): the hurt hp it expects (route-danger's :danger of the way),
  at most its health, at hp-price of its health, and the death it risks (route-death-cost of route {:cells :threats
  :spawns} at `at`)."
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
  "Seconds getting `n` of names is worth to body at `at` less a trip: a search's :worth for it (each walk of it is weighed
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
