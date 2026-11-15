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
    answer   fight-answer, followed, small (an answer's small blows, the ones its :shape is the width of), flee-answer, swim-reach (the blocks of a swim flee-answer prices),build-answer, refuge-answer (a run into a known refuge as a cover), stand-loss (what doing nothing about mobs costs: the loss a reflex averts; stand-answer its answer), hold-answer (a hold where no enderman reaches), cover-answer, cover-answers, cornered-answers, leave-answer (an answer begun after the seconds of breaking out of a seal), answer-price, meet-death (a death beside a near respawn meets the mobs again), decide: what each answer to hostiles costs and the cheapest; kill-price: a kill of one mob; route-death, death-chance, route-death-cost, route-parts (the death chance and hurt of a route's threats, each pair met in one answer by the chance the second comes while the first is answered), threat-hurt, spawn-kill, threat-parts, spawns-chance, spawns-hurt, lethal-hp: the one chance of dying (threat-hurt one mob met; water-factors the planner's price of a second in water, a drowned met there, swim-dark-factor a second swum in the dark, unseen-water-factors a phantom floor's by the seen water near it; spawns-chance that of mobs spawned beside the body (spawn-kill one's, with what its death costs), spawns-hurt their hurt (spawn-hurt one's, spawned-hurt of a known one's), spawn-price one's price by the answer taken, lethal-hp a death chance in hp) (a known threat priced by the answer the body would take) and a route's death price; blast-hurt: a creeper's blast (chance-blast one that comes by chance, blast-hp, blast-mean; blast-env-s what it breaks about it, blast-break-chance a block's chance, blast-reach); counted-chance, floor-absent-chance, guess-bad-chance, floor-dead-end-chance, hall-chance, hall-edge-chance, spot-plantable-chance, retry-chance: a counted share over its prior (jobs.lib.cost.value), craft-step-s: the seconds of a craft step with its timeouts; try-again?: whether another try of a call is worth its price.
    danger   route-danger: the danger of a walk past mobs; danger-opts, threat-factor-cap: go-to's
             planner danger factors and cap; reach-first?, wins-race?: a body's race to a refuge against its chasers; catches?, caught-on?: whether a chaser hits a walk (straight, or a timed route) before it is past; reach-s: the seconds it is within its blow of a timed route; arrival-s: a chaser's seconds to a point (over its way when given); lead-s, run-saved-s: the lead a chaser leaves a body at a place, the run a nearer refuge saves; search-budget-s, under-ceiling?: the CPU one search may take (Q141), whether a part of it may still start; search-pays?, refuge-search-pays?: whether a search's CPU fits a budget too, a refuge search's the chasers' lead, the run it saves or the shooters' fire; body-speed; flee-walk-speed, flee-stop-s: a flight's measured pace, its steps and the stops between them (jobs.lib.cost.danger).
    mob-toll answered-together ({:stance :price :near} of mobs met together), threat-factor (its meet rate x that price): go-to's planner dangers, a mob priced by the answer the reflex would take to it (answer/decide) (jobs.lib.cost.mob-toll).
    health   hp-seconds (the food that heals an hp again), hp-cost, landing-hp (hp a falling block that lands on the body costs; landing-cost-s its price), risk-s (a walk's hurt and death chance in seconds), commute-s (a walk there and back against sheltering here), night-lost-s (the work a way through the night costs), night-way-s (the one price of a way through the night), night-walk-bound-s (the longest walk that price could still take), damage-budget, survivable-budget: what an hp costs and how many a walk may spend (jobs.lib.cost.health).
    food     food-reserve: the food a body keeps carried, 3 days of it; outlasts, trip-points, carry-points: the food a trip carries, priced against the chance the trip outlasts it, at least that reserve less the home food; cook-worth?, cook-gain: whether cooking raw food beats eating it now; meat-factor, meat-animal: raw meat's replacement cost, dear for hunted, 1 with a working pen (jobs.lib.cost.food).
    value    item-value, survived-worth (a find's worth times the chance the trip does not end in death), chance-times (p x s, 0 at p 0 though s be ##Inf), affords? (a price within a worth, never ##Inf), quote-gain (the seconds a walk quote is expected to save), reserve-cost (using a reserved unit: value x s/(1-s)), fetch-cost, pile-lost (a pile burned or fallen out of the world), walk-cost, walk-beats-making (walk to what exists vs make one and place it), detour-cost: what items are worth, what fetching them, walking or a pick-up detour costs; tour-cost, tour-worth, turn-back-cost (an unseen floor's look and, not there, the walk back), food-worth, animal-worth (its meat's food points at food-point-worth), worth-left / others-share-s (a child's worth: the call's less the rest of its plan), crop-worth (a ripe crop's food points at food-point-worth): a tour of
             frontiers against what its goal is worth; cover-tour-s: the walk that brings a set of points within reach; light-cost: a torch go-to places; light-wait-worth: what n torches fetched save over a wait for light; wear-saved: the wear of a dear tool a spare's digs save; dig-cost: the seconds a block takes to dig, reset-s: a block broken and placed again, kill-drops, block-drops: the items a kill or a block's break drops (jobs.lib.cost.value);
             per-dark, dark-factor: the one price of a dark block, which go-to's planner costs; lit-way-s, torch-way-s, torch-gap: a way lit as it goes; dark-way-s: one walked or dug dark; climb-back-s: back up from land with no known way up; dark-or-lit: a dark stay against lighting it; spawn-chance: the chance a mob spawns at a cell or
             over an area (a set or {:min :max} box of cells) per second, from the light the body knows (unseen counts as dark) and spawn-rate at its height, spawn-share its share of cells that spawn.
    planner  planner-costs-problem, planner-costs, planner-names: go-to's :costs, the per-move prices it may override; least-block-s: the least a plan prices a block, least-route-s a straight way to within a range; search-cell-s, search-read-s, flood-cell-s: the CPU seconds of one cell a route search expands, of one it reads (dig-route's price), of one cell go-to's room flood visits; walk-cell-s, room-read-s: of one cell a refuge walk search expands, a refuge room flood reads (measured means, engine.search-recount-test), walk-cell-ceiling-s, room-read-ceiling-s their upper bounds for the 100 ms meter; landing-problem, planner-landing, landing-factor: go-to's :landing, fall damage by landing block; air-profile, water-breathing-s, air-used, air-costs: the planner's air drain and grace from the body's helmet, the air a dive has used; air-left-s, wet-move-s, wet-way-s, wet-spare-s, wet-ok?: the air a walk over unseen water keeps back at its last breath, each move at its measured pace; swim-profile: its swimH from Depth Strider on the boots; gait-problem, gait-costs: go-to's :gait; unknown-prices: the planner's peek and aim prices of unseen cells (jobs.lib.cost.planner); way-on-chance, unseen-share: the one price of walking over unseen cells, a dead end and a mob in the dark at the worse case, from how often the seen ways near the body go on, under ground a flat measured rate (the planner's :cell); unseen-cell-share: that price where the way is shut at a given chance (a flight's cell by cell); unseen-route-s: that price of a route's unseen steps, unseen-step-s of one, walk-step-s a walked block; flood-dig-s: a dig that lets water beside it into the way (flood-under-s the time it is crossed under water), wet-dig-s and wet-dig-pace, wet-step-s: a dig and a step inside that flood, wet-s: time under water over dry, drowning (drown-s) included; stair-leg-pricer, pillar-s: a stair step's and a pillar's seconds with the unseen step on top (go-to's escalations, surface-up, the one-way climb-out); unseen-fall-s: the fall and climb out when an unseen floor a step down is not there; gap-look-cost: a refused gap jump over unseen floors filled now or looked at first; dead-end-cost, dead-end-race: what an unseen dead end costs a flight and the chance it catches it; flight-caught-chance: the chance a flight is caught by its way, from its search's ways.
    tolls    farm-tolls, zone-tolls, flood-tolls: go-to's :tolls for planted cells and zone cells to cross only as a last resort; gate-tolls: a
             stocked pen's gates at the herd's loss while one stands open; fail-chance,
             failing-step-s: a step the walker failed at, priced by its failures (jobs.lib.cost.tolls).
    death    experience-cost, death-cost: death as an ordinary cost, the walk back for the items or to the goal plus the experience and the ::extra-s setting (default 0); spawn-worth: a respawn point's worth; goal-value (item-value with measured gathers), worth-to, find-worth, items-find-worth, search-radius: what a find is worth, how far its legs may go; opening-price, hold-price: opening a shelter to mobs against holding on in it; leg-start, leg-price, dig-leg-price: a walk or dig leg's clock seconds and hp lost, never under its straight line; death-weight, terrain-danger: a remembered death's fade and the plan cost near a death by terrain (jobs.lib.cost.death).
    base-rates  density, biome-chance, expected-density, peak-density: the ore a block of ground is expected to hold, from the game's
             own placements, host-blocks: the rock its veins replace, and the chance of a biome on ground the body has not seen (jobs.lib.cost.base-rates).
    destroy  destroyed-s, destroyed-fn, cut-fn (a cut cell's dig with it): what digging a cell destroys, in seconds of replacing it (a crop's growth, farmland's dirt and till, a build put back with put-back-s; natural terrain and tree wood nothing, a log that looks placed (horizontal, in a line, joined to planks or glass) a build) (jobs.lib.cost.destroy).
    look     look-answer, look-value, closed-chance, closed-cell-chance, round-looks, turn-chance, turn-saving, cell-looks, redo-every, look-s: the value of a look before deciding, a boundary's unseen part closed, the looks a decision's unknown cells pay (jobs.lib.cost.look).
    find     look-chance, find-chance, searched-for?: the chance a look round finds the animals a search is for, from their base rate and the ground already searched in vain, looked-area: that ground; ore-of, seen-ore, tunnel-ore, ore-chance, vein-seen (ore blocks a find of a vein yields): an ore's blocks expected among the faces a look sees or a tunnel cuts and the chance of a vein there; ore-level: the height a tunnel for it runs at; leg-chance, look-leg-chance (from what the body saw), tunnel-chance: a search leg's or a tunnel leg's chance of a find, tunnel-rate: a tunnel's blocks of ore per second over the cells it cuts (a heading's price), chance-of: leg i's; leg-stops?: a search's :price stop; search-s: its seconds per find, leg-spacing: the blocks between its legs (jobs.lib.cost.find).
    farm     field-share: the share of a field's unseen cells ripe or sowable, from the measured passes and its seen cells; ripe-chance: a cell seen growing ripe by now, at the game's growth rate, ripen-s, ripen-sd: the wait for a count of a crop to ripen and its spread, growing-s: the seconds of them within the server's simulation distance of the body; harvests, sown-worth: the harvests a crop still to grow is worth now and what a sown cell is worth (its food at food-point-worth); field-step-s: the measured seconds of a tend step, field-grow: growing a count from a fresh field (hoe, seeds, work, ripening) with its sd, earthwork-s: levelling a patch's cells off height (jobs.lib.cost.farm).
    pen      gap-loss: the animal worth a herd loses through fence gaps till the next pass, from a Gamma prior on the escape rate (jobs.lib.cost.pen); herd-loss, through one gap open a while.
    repair   repair-worth: what mending a plan's broken cells is worth: each cell's put-back and its :loss (a light's averted spawns), a herd's gap-loss, a crop's harvests lost growing dry (jobs.lib.cost.repair).
    rise     expected-rise: the blocks up to a sky the body has not seen, from its height, what it knows of the surface, and a prior on the ground height (jobs.lib.cost.rise).
    averted  open-door-s, open-gate-s, short-slot-s, left-cell-s, riding-s, wedged-s, stuck-s, burn-hp: the loss a reflex averts where its trigger prices none, broad priors (jobs.lib.cost.averted); tree-worth: a tree's logs; log-share: the share of more logs the held stock leaves wanted (jobs.lib.cost.stock)."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.averted :as averted]
            [jobs.lib.cost.stock :as stock]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.destroy :as destroy]
            [jobs.lib.cost.farm :as farm]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.find :as find]
            [jobs.lib.cost.look :as glance]
            [jobs.lib.cost.pen :as pen]
            [jobs.lib.cost.repair :as repair]
            [jobs.lib.cost.rise :as rise]
            [jobs.lib.cost.mob-toll :as mob-toll]
            [jobs.lib.cost.food :as food]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.planner :as planner]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.tolls :as tolls]
            [jobs.lib.cost.ways :as ways]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.table :as table]
            [jobs.lib.crops :as crops]
            [jobs.lib.cost.value :as value]
            [jobs.lib.goal :as goal]
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

(defn drop-limit
  "The longest drop in blocks a body {:health :absorption :on-fire :effects} with equipment survives within the walk's
  settings ({:max-damage}): the one limit of go-to's planner, its unseen-floor price and a door's landing."
  [equipment body settings]
  (:max-drop (fall-profile {:equipment equipment :damage-budget (health/survivable-budget body (select-keys settings [:max-damage]))})))

(def hostile? threat/hostile?)
(def mob-hurt threat/mob-hurt)

(def attack-gap-ms weapon/attack-gap-ms)
(def weapon-damage weapon/weapon-damage)
(def remaining-health weapon/remaining-health)
(def ranged-mobs weapon/ranged-mobs)
(def tool-tier weapon/tool-tier)
(def weapon-rank weapon/weapon-rank)
(def cheapness weapon/cheapness)

(def fight-damage fight/fight-damage)
(def fight-seconds fight/fight-seconds)
(def walk-speed fight/walk-speed)

(def fight-answer answer/fight-answer)
(def followed answer/followed)
(def small answer/small)
(def hit-range answer/hit-range)
(def flee-answer answer/flee-answer)
(def swim-reach answer/swim-reach)
(def refuge-answer answer/refuge-answer)
(def build-answer answer/build-answer)
(def stand-loss answer/stand-loss)
(def stand-answer answer/stand-answer)
(def hold-answer answer/hold-answer)
(def stays-chance answer/stays-chance)
(def cover-answer answer/cover-answer)
(def cover-answers answer/cover-answers)
(def cornered-answers answer/cornered-answers)
(def leave-answer answer/leave-answer)
(def answer-price answer/answer-price)
(def answer-price* answer/answer-price*)
(def meet-death answer/meet-death)
(def meet-death-fn answer/meet-death-fn)
(def counted-chance value/counted-chance)
(def decide answer/decide)
(def look-answer glance/look-answer)
(def look-value glance/look-value)
(def closed-chance glance/closed-chance)
(def closed-cell-chance glance/closed-cell-chance)
(def look-s glance/look-s)
(def round-looks glance/round-looks)
(def turn-chance glance/turn-chance)
(def turn-saving glance/turn-saving)
(def cell-looks glance/cell-looks)
(def redo-every glance/redo-every)
(def blast-hurt answer/blast-hurt)
(def blast-break-chance answer/blast-break-chance)
(def blast-env-s answer/blast-env-s)
(def blast-reach answer/blast-reach)
(def blast-damage answer/blast-damage)
(def chance-blast answer/chance-blast)
(def blast-hp answer/blast-hp)
(def blast-mean answer/blast-mean)
(def swell-start answer/swell-start)
(def fuse-s answer/fuse-s)
(def fuse-cancel-range answer/fuse-cancel-range)

(def route-danger danger/route-danger)
(defn exposure-s [] (danger/exposure-s))
(def mob-of-threat danger/mob-of-threat)
(def answered-together mob-toll/answered-together)
(def danger-opts danger/danger-opts)
(def threat-factor mob-toll/threat-factor)
(def threat-factor-cap danger/threat-factor-cap)
(def max-dangers danger/max-dangers)
(def danger-max-rate danger/max-rate)
(def danger-cap danger/danger-cap)
(def reach-first? danger/reach-first?)
(def wins-race? danger/wins-race?)
(def outruns? danger/outruns?)
(def catches? danger/catches?)
(def caught-on? danger/caught-on?)
(def reach-s danger/reach-s)
(def swim-arrivals danger/swim-arrivals)
(def arrival-s danger/arrival-s)
(def lead-s danger/lead-s)
(def run-saved-s danger/run-saved-s)
(defn search-budget-s [] (danger/search-budget-s))
(def under-ceiling? danger/under-ceiling?)
(def search-pays? danger/search-pays?)
(def refuge-search-pays? danger/refuge-search-pays?)
(def centred danger/centred)
(defn melee-reach [] (danger/melee-reach))
(def body-speed danger/body-speed)
(def flee-walk-speed danger/flee-walk-speed)
(defn flee-stop-s [] (danger/flee-stop-s))
(def flee-pace danger/flee-pace)
(def mob-kind danger/mob-kind)
(def chase-speed danger/chase-speed)

(def hp-seconds health/hp-seconds)
(def hp-cost health/hp-cost)
(def landing-hp health/landing-hp)
(def risk-s health/risk-s)
(def commute-s health/commute-s)
(def night-lost-s health/night-lost-s)
(def night-way-s health/night-way-s)
(def night-walk-bound-s health/night-walk-bound-s)
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
(def least-route-s planner/least-route-s)
(def search-cell-s planner/search-cell-s)
(def search-read-s planner/search-read-s)
(def search-cell-ceiling-s planner/search-cell-ceiling-s)
(def plan-nodes-per-block planner/plan-nodes-per-block)
(def search-read-ceiling-s planner/search-read-ceiling-s)

(defn search-cpu-s
  "Seconds of CPU a route search has spent: the larger of its cells read and its cells expanded at their per-cell prices
  (the CPU grows with both; each price is the whole CPU over one count at the measured mix, so the larger bounds a cost
  linear in both from above, exact at that mix). With the mean prices it is the search's price, with the ceiling prices
  the meter of a wall-time limit."
  [read-s cell-s reads cells]
  (max (* read-s reads) (* cell-s cells)))
(def walk-cell-s planner/walk-cell-s)
(def room-read-s planner/room-read-s)
(def walk-cell-ceiling-s planner/walk-cell-ceiling-s)
(def room-read-ceiling-s planner/room-read-ceiling-s)
(def flood-cell-s planner/flood-cell-s)
(def landing-problem planner/landing-problem)
(def planner-landing planner/planner-landing)
(def landing-factor planner/landing-factor)
(def walk-costs planner/walk-costs)
(def walk-landing planner/walk-landing)
(def walk-drop-cost planner/walk-drop-cost)
(def gap-miss-jumps planner/gap-miss-jumps)
(def walk-unknown-prices planner/walk-unknown-prices)
(def gaits planner/gaits)
(def gait-problem planner/gait-problem)
(def gait-costs planner/gait-costs)
(def unknown-prices planner/unknown-prices)

(def way-on-priors
  "Beta(on, dead) of a way on through unseen cells: the surface (1 1), calibrated (0.14 dead measured, 0.14 predicted); under
  ground (sky light 0 at the feet) 6 and 4: 0.37-0.43 of the unseen ways there measured dead, and the seen counts did not
  predict it better than that rate."
  {:surface [1 1] :under [6 4]})

(defn way-on-chance
  "The chance a way on through cells the body has not seen goes on: the prior of its context (way-on-priors) updated by counts
  {:k :n} of the seen stands near it (jobs.lib.walk.way-on/way-counts: n seen, k of them with a way that leads out). With
  :under? true the counts are left out: a flat rate; :under? :unknown (no sky light known) is the lower of the two."
  [{:keys [k n under?]}]
  (let [chance (fn [prior counts?]
                 (let [[on dead] (way-on-priors prior) [k n] (if counts? [(or k 0) (or n 0)] [0 0])]
                   (/ (+ k on) (+ n on dead))))]
    (case under?
      true (chance :under false)
      :unknown (min (chance :under false) (chance :surface true))
      (chance :surface true))))

(defonce ^:private kit-kept (atom {}))

(defn by-kit
  "(f kit), kept per kit, settings, call's overrides and the game state prices read (difficulty, simulation distance,
  version), so callers under different overrides in turn each hit: f a price of a kit alone, which every costed world
  asks again (each is a decide over every answer to every spawned mob)."
  [tag f kit]
  (let [k [tag kit (:values @settings/state) (.getStore settings/call-scope) (threat/difficulty) (threat/simulation-distance) @game/version]]
    (if-let [hit (find @kit-kept k)]
      (val hit)
      (let [v (f kit)]
        (swap! kit-kept #(assoc (if (>= (count %) 64) {} %) k v))
        v))))

(defn dark-factor
  "The planner's extra cost of a dark cell as a share of its own seconds (options.dark.factor): value/dark-factor, the
  floor; with a kit {:weapon :equipment :health :food :at} the price of a mob spawned beside it (answer/spawn-price: the
  answer the body takes, its time, hurt and death) times spawn-rate at the kit's :at height (the feet's 16-block floor), on
  top: a weapon or armour never adds to it (by-kit)."
  ([] (value/dark-factor))
  ([kit] (by-kit ::dark-factor
                 #(+ (value/dark-factor) (* (value/spawn-rate (get-in % [:at :y])) (answer/spawn-price % nil)))
                 kit)))

(defn unseen-cell-share
  "Seconds a second of walk over cells the body has not seen adds when the way is shut there at chance shut: that chance
  times the walk back out, and a mob that spawns or waits there (an unseen cell counts as dark): dark-factor of kit. The
  replan at the dead end is not in it (unmeasured)."
  [shut kit]
  (+ (* 2 shut) (dark-factor kit)))

(defn unseen-share
  "unseen-cell-share at the worse case but finite: the chance the way does not go on, 1 less way-on-chance of counts."
  [counts kit]
  (unseen-cell-share (- 1 (way-on-chance counts)) kit))

(defn look-first
  "hp or seconds of walking to an unseen place to look before staying: the walk there, and the walk back at the chance
  `dark` the look shows the place too dark to stay (Q66). The stay is judged on arrival."
  [there back dark]
  (+ there (* dark back)))

(defn dead-end-race
  "{:cost :chance} of an unseen dead end that catches the body at (counts) (1 less way-on-chance) when it is a dead end:
  :cost seconds, the cheapest of trapped (what the chasers cost where they corner the body; chance 1) and backs
  [{:s :caught-s :unseen? :trapped}], a walk back to a fork and away along another way: its seconds and that way's price
  on, trapped once a blow lands (hit: one each answer/melee-gap-s over caught-s, the seconds a chaser is within its
  blow: reach-s), else, when that way runs unseen too, its own dead end at the same chance and its trapped. :chance the
  chance the body is caught by it at the cheapest option (the dead end's chance times hit; no chaser, no blow); never
  below 0 in cost."
  [counts trapped backs]
  (let [dead (- 1 (way-on-chance counts))
        options (cons {:cost trapped :hit 1}
                      (map (fn [b]
                             (let [hit (- 1 (js/Math.exp (- (/ (:caught-s b) answer/melee-gap-s))))]
                               {:cost (max 0 (+ (:s b) (* hit trapped) (if (:unseen? b) (* (- 1 hit) dead (:trapped b)) 0)))
                                :hit (if (:unseen? b) (+ hit (* (- 1 hit) dead)) hit)}))
                           backs))
        best (reduce #(if (< (:cost %2) (:cost %1)) %2 %1) options)]
    {:cost (* dead (:cost best)) :chance (* dead (:hit best))}))

(defn dead-end-cost
  "Seconds a route that runs on into cells the body has not seen adds past their unseen-share (its planner price): the
  :cost of dead-end-race, backs as there; trapped is the dead end's price with no way back."
  ([counts trapped] (dead-end-cost counts trapped nil))
  ([counts trapped backs] (:cost (dead-end-race counts trapped backs))))

(defn unseen-route-s
  "Seconds the steps of route ([{:x :y :z}] cells) add over cells kind-at reads :ground (a seen-lookup's unseen cell):
  share (unseen-share) of a block's seconds (per-block) for each step whose feet, head or floor is one."
  [share kind-at route]
  (let [unseen? (fn [x y z] (keyword-identical? :ground (kind-at x y z)))]
    (* share (value/per-block)
       (count (filter (fn [{:keys [x y z]}] (or (unseen? x y z) (unseen? x (inc y) z) (unseen? x (dec y) z))) route)))))

(defn walk-step-s "Seconds one walked block takes." [] (:cost (value/walk-cost {:distance 1})))

(defn dig-cells-s
  "Seconds a row of cells dug and walked into takes: clear-ms, the ms each cell's block takes to break, plus a walked step each."
  [clear-ms]
  (transduce (map #(+ (/ % 1000) (walk-step-s))) + 0 clear-ms))

(def water-spread "Blocks water flows from its source on level ground (vanilla)." 7)

(defn flood-reaches?
  "Whether water let in at cell [ox oy oz] up to level may reach cell: at or under the level and no farther across (x
  plus z) than it flows, water-spread at oy and as far again for each block lower, and the origin's extra (nil: 0) on top
  (flood-origin-join). Every cell between counts as open, seen rock or not: never dry where the water could come."
  [[ox oy oz extra] level [x y z]]
  (and (<= y level) (<= (+ (js/Math.abs (- x ox)) (js/Math.abs (- z oz))) (+ (or extra 0) (* water-spread (inc (- oy y)))))))

(defn flood-origin-join
  "The origin of two floods let in at origins a and b: the higher one with an extra reach of the distance to the other
  (and its extra), so every cell either reaches is reached (an upper bound, as flood-reaches?)."
  [a b]
  (let [[[hx hy hz he] [lx _ lz le]] (if (>= (a 1) (b 1)) [a b] [b a])
        d (+ (js/Math.abs (- hx lx)) (js/Math.abs (- hz lz)))]
    [hx hy hz (max (or he 0) (+ (or le 0) d))]))

(defn flood-origin-within?
  "Whether every cell water let in at origin a may reach (flood-reaches?) water let in at origin b may reach too, at any level."
  [[ax ay az ae] [bx by bz be]]
  (<= (+ (js/Math.abs (- ax bx)) (js/Math.abs (- az bz)) (or ae 0) (* water-spread (- ay by))) (or be 0)))

(defn wet-dig-pace
  "How many times its dry seconds a dig takes with the head under water, standing on ground (vanilla): 5, or 1 with Aqua
  Affinity on the helmet. equipment: self's :equipment (cljs or JS)."
  [equipment]
  (if (some #(= "aqua_affinity" (:name %)) (:enchants (get (armour/equipment-of equipment) "head"))) 1 5))

(defn drown-s
  "Seconds under-s seconds with the head under water cost a body under policy at health: past the air it has (air-left-s)
  it drowns, 2 hp a second (vanilla), each hp at hp-s; ##Inf when that kills it."
  [policy health hp-s under-s]
  (let [hp (* 2 (js/Math.ceil (max 0 (- under-s (planner/air-left-s policy)))))]
    (if (>= hp health) ##Inf (* hp hp-s))))

(defn flood-under-s
  "Seconds the head stays under water while a flood let into the way crosses it: water-spread cells past the opened one,
  each a level step under water (wet-move-s under policy, the body's air-profile and swim-profile)."
  [policy]
  (* (inc water-spread) (planner/wet-move-s policy :walk)))

(defn wet-s
  "Seconds wet-s seconds with the head under water, after under-s already under it, cost over dry-s seconds dry: the time
  they add and the drowning (drown-s), never under 0; ##Inf when it kills."
  [policy health hp-s under-s dry-s wet-s]
  (let [after (drown-s policy health hp-s (+ under-s wet-s))]
    (if (= ##Inf after) ##Inf (max 0 (+ (- wet-s dry-s) (- after (drown-s policy health hp-s under-s)))))))

(defn flood-dig-s
  "Seconds a dig that opens a cell to water beside it costs over a dry one: the water flows into the cell and on along the
  way, crossed under water (flood-under-s) in place of walked steps (wet-s). Each dig and step on inside the flood is
  wet-dig-s, wet-step-s."
  [policy health hp-s]
  (wet-s policy health hp-s 0 (* (inc water-spread) (walk-step-s)) (flood-under-s policy)))

(defn wet-dig-s
  "Seconds a dig of dig-s dry seconds costs over a dry one when made inside a flood the head has been under for under-s:
  it takes policy :dig-pace (wet-dig-pace; no helmet's when absent) times as long (wet-s)."
  [policy health hp-s under-s dig-s]
  (wet-s policy health hp-s under-s dig-s (* (:dig-pace policy (wet-dig-pace nil)) dig-s)))

(defn wet-step-s
  "Seconds a level step with the head under water, under it for under-s, costs over a walked one: wet-move-s (wet-s)."
  [policy health hp-s under-s]
  (wet-s policy health hp-s under-s (walk-step-s) (planner/wet-move-s policy :walk)))

(defn unseen-step-s
  "Seconds one step into a cell not seen takes on top: unseen-route-s of one such step at the share of the world not seen."
  [share]
  (unseen-route-s share (constantly :ground) [{:x 0 :y 0 :z 0}]))

(defn stair-leg-pricer
  "A fn [cuts-s fill-s unseen?] of the seconds one stair step takes at the unseen share: ways/stair-step-cost of a walk step,
  the seconds of each cut (nil for an open cell), a fill of the floor when fill-s is given, and unseen-step-s when unseen?."
  [share]
  (let [step-s (walk-step-s)
        unseen-s (unseen-step-s share)]
    (fn [cuts-s fill-s unseen?]
      (ways/stair-step-cost step-s (ways/stair-cost 1) (cond-> (vec cuts-s) fill-s (conj fill-s)) (if unseen? unseen-s 0)))))

(defn pillar-s
  "Seconds a pillar of height blocks takes: ways/pillar-cost (short of them fetched, default none), and unseen-step-s for
  each of the unseen-blocks whose cell or the two over it is unseen."
  ([share height unseen-blocks] (pillar-s share height unseen-blocks 0))
  ([share height unseen-blocks short]
   (+ (ways/pillar-cost height short) (* (unseen-step-s share) unseen-blocks))))

(defn pillar-knock-block-s
  "Seconds of hp the n-th block of a pillar costs in expectation: value/pillar-knock-chance, times the open share (0..1)
  of the column's sides there, that a blow shoves the body off it, a fall of n blocks."
  [equipment n open]
  (* (value/pillar-knock-chance) open (health/hp-cost (fall-damage equipment n))))

(defn pillar-knock-s
  "Seconds of hp a pillar costs in expectation: the sum of pillar-knock-block-s of its blocks, opens their open shares."
  [equipment opens]
  (transduce (map-indexed (fn [i open] (pillar-knock-block-s equipment (inc i) open))) + 0 opens))

(defn pillar-knock-of
  "pillar-knock-s of opens for body p's kit."
  [p opens]
  (let [self (.self p)]
    (pillar-knock-s (equipment-of (.-equipment self)) opens)))

(defn unseen-fall-s
  "Seconds a step drop-by blocks down (kind :down, :drop, :drop-4 or :drop-8: jobs.lib.walk.sight/prior-kind of its rise) onto a floor the body has not seen costs on top of turning
  back when that floor is not there: the walk goes on down onto what lies under it, value/::unseen-fall-counts' share of
  kind at each depth, when the whole drop is one the planner takes (max-drop; a deeper one it does not, and its dead end
  is priced apart): the fall's hp (fall-damage at hp-cost) and a pillar back up to the takeoff (pillar-s,
  every block unseen at the unseen share, the blocks past carried fetched). With :else-s, the body has peeked over the edge
  first (jobs.lib.walk.sight): each depth costs the cheaper of that fall and :else-s (turning back for the next way), a
  depth the peek does not see (deeper than :see-depth) or the planner does not take :else-s."
  [kind drop-by {:keys [equipment max-drop carried share else-s see-depth]}]
  (let [counts (get (settings/get value/settings ::value/unseen-fall-counts) kind)
        total (reduce + 0 (vals counts))]
    (if (pos? total)
      (reduce-kv (fn [s below n]
                   (let [fall (+ drop-by below)
                         taken? (and (<= fall max-drop) (or (nil? else-s) (<= fall see-depth)))
                         fall-s #(+ (health/hp-cost (fall-damage equipment fall)) (pillar-s share fall fall (max 0 (- fall carried))))
                         cost (cond (not taken?) (or else-s 0) else-s (min (fall-s) else-s) :else (fall-s))]
                     (+ s (value/chance-times (/ n total) cost))))
                 0 counts)
      0)))

(defn gap-look-cost
  "{:fill :look} seconds a gap jump the walker refuses over n floors the body has not seen adds (the worse case: none is
  there): :fill fills them now, a carried block each (ways/fill-cost with carried in the pack); :look first walks detour
  blocks out of the way to look at them (aim-s each) and fills them only when one is not there, each not there at chance p."
  [n carried detour p]
  (let [fill (transduce (map #(ways/fill-cost (- carried %))) + 0 (range n))]
    {:fill fill
     :look (+ (:cost (value/walk-cost {:distance (max 0 detour)})) (* n (:aim-s (walk-unknown-prices)))
              (* (- 1 (js/Math.pow (- 1 p) n)) fill))}))

(defn flight-caught-chance
  "The chance a flight is caught by the way its search found (flee-answer :caught; jobs.lib.cost.caught/prior is added there,
  whatever the way): 1 with no way left (way? false: every way it planned is cut); 0 on a way the race calls free; when
  the way runs on into cells the body has not seen (unseen?), the chance its dead end catches it: :dead-chance, the
  :chance of dead-end-race of the end (its search's backs), else that of a dead end with no way back (counts)."
  [{:keys [way? unseen? counts dead-chance]}]
  (cond (not way?) 1
        unseen? (or dead-chance (:chance (dead-end-race counts 0 nil)))
        :else 0))

(def item-value value/item-value)
(def wear-saved value/wear-saved)
(def fetch-cost value/fetch-cost)
(def pile-lost value/pile-lost)
(def survived-worth value/survived-worth)
(def chance-times value/chance-times)
(def affords? value/affords?)

(defn quote-gain
  "Seconds quoting a candidate of bound b is expected to save on the best price so far: its price is b times one of
  the ratios (quoted price over bound) of the quotes so far or, as often as each of them, b itself (a candidate unlike
  them all). Not an affine fit (a fixed term plus a ratio): the samples are few and one ratio each is what Laplace's
  rule needs."
  [best b ratios]
  (/ (reduce (fn [s r] (let [p (* b r)] (if (< p best) (+ s (- best p)) s))) (max 0 (- best b)) ratios) (inc (count ratios))))
(def reserve-cost value/reserve-cost)
(def walk-cost value/walk-cost)
(defn own-place-blocks [] (value/own-place-blocks))
(def walk-beats-making value/walk-beats-making)
(defn walk-blocks-per-s [] (value/walk-blocks-per-s))
(defn per-block [] (value/per-block))
(def dig-cost table/dig-cost)
(defn reset-s "Seconds breaking block name and placing it again take: its dig-cost and a place." [name] (+ (or (dig-cost name) 0) (ways/place-s)))
(def kill-drops table/kill-drops)
(def block-drops table/block-drops)
(def smelt-step table/smelt-step)
(def smelt-price value/smelt-price)
(def craft-step table/craft-step)

(def detour-cost value/detour-cost)
(def tour-cost value/tour-cost)
(def tour-worth value/tour-worth)
(def floor-absent-chance answer/floor-absent-chance)
(def guess-bad-chance answer/guess-bad-chance)
(def floor-dead-end-chance answer/floor-dead-end-chance)
(def hall-chance answer/hall-chance)
(def hall-edge-chance answer/hall-edge-chance)
(def spot-plantable-chance answer/spot-plantable-chance)
(def retry-chance value/retry-chance)
(def craft-step-s value/craft-step-s)

(def retry-s value/retry-s)

(defn refused
  "kind's -refused kind (value/::retry-counts :place-refused, :craft-refused, :child-round-refused): a try after the same
  failure twice in a row, which repeats until the world changes."
  [kind]
  (keyword (str (name kind) "-refused")))

(defn refusal-kind
  "kind, or its refused kind once the server refused the call with the same key twice in a row (u/failed! c key)."
  [c kind]
  (if (< 1 (u/refusals c)) (refused kind) kind))

(defn try-again?
  "Whether call c's next try of kind (a key of value/::retry-counts) is worth its price: its retry-chance after the call's
  failures in a row (u/failed!, u/progress! resets) times ctx/worth over the kind's measured :try-s. Never at a worth of 0. opts {:fails :try-s :worth} replace
  the call's failures, the kind's seconds and the call's worth for a caller that counts its own."
  ([c kind] (try-again? c kind nil))
  ([c kind {:keys [fails try-s worth]}]
   (> (value/chance-times (retry-chance kind (or fails (u/failures c))) (or worth (ctx/worth c)))
      (or try-s (retry-s kind)))))

(defn worth-left
  "The worth a child of call c gets: the call's (ctx/worth) less rest-s, the price of the rest of c's plan after the
  child, 0 at least; ##Inf stays ##Inf. Sunk cost does not lower it."
  [c rest-s]
  (let [w (ctx/worth c)]
    (if (infinite? w) w (max 0 (- w rest-s)))))

(defn others-share-s
  "rest-s for worth-left of a child after n of the call's total units: the other units, each at an even share of the
  call's worth (none when n covers total)."
  [c n total]
  (if (>= n total) 0 (* (ctx/worth c) (/ (- total n) total))))

(def turn-back-cost value/turn-back-cost)
(def cover-tour-s value/cover-tour-s)
(def food-worth food/food-worth)
(def animal-worth value/animal-worth)
(def crop-worth value/crop-worth)
(def expected-drops value/expected-drops)
(def food-point-worth value/food-point-worth)
(def food-search-worth value/food-search-worth)
(defn trip [] (value/trip))
(def field-share farm/share)
(def ripe-chance farm/ripe-chance)
(def ripen-s farm/ripen-s)
(def ripen-sd farm/ripen-sd)
(def growing-s farm/growing-s)
(def harvests farm/harvests)
(def sown-worth farm/sown-worth)
(def field-step-s farm/step-s)
(def destroyed-s destroy/destroyed-s)
(def destroyed-fn destroy/destroyed-fn)
(def cut-fn destroy/cut-fn)
(def put-back-s destroy/put-back-s)
(def gap-loss pen/gap-loss)
(def herd-loss pen/herd-loss)
(def repair-worth repair/repair-worth)
(def open-door-s averted/open-door-s)
(def open-gate-s averted/open-gate-s)
(def short-slot-s averted/short-slot-s)
(def left-cell-s averted/left-cell-s)
(def riding-s averted/riding-s)
(def wedged-s averted/wedged-s)
(def stuck-s averted/stuck-s)
(def burn-hp averted/burn-hp)
(def log-share stock/log-share)
(def earthwork-s farm/earthwork-s)
(def light-cost value/light-cost)
(def per-danger value/per-danger)
(def spawn-chance value/spawn-chance)
(def spawn-rate value/spawn-rate)
(def spawn-share value/spawn-share)

(defn near-body?
  "Whether the cell `to` lies within sh-inside/spawn-free-radius of the body's feet: no mob spawns that near a player, so dark there is no danger to go to or work at."
  [p to]
  (let [feet (sh-sense/feet p)]
    (<= (u/dist feet (update to :y #(or % (:y feet)))) sh-inside/spawn-free-radius)))

(defn per-dark [] (value/per-dark))

(def farm-tolls tolls/farm-tolls)
(def zone-tolls tolls/zone-tolls)
(def flood-tolls tolls/flood-tolls)
(def gate-tolls tolls/gate-tolls)
(def fail-chance tolls/fail-chance)
(def failing-step-s tolls/failing-step-s)

(def food-reserve food/food-reserve)
(def food-reserve-points food/reserve-points)
(def cook-worth? food/cook-worth?)
(def cook-gain food/cook-gain)
(def meat-factor food/meat-factor)
(def meat-animal food/meat-animal)

(def experience-cost death/experience-cost)
(def death-cost death/death-cost)
(def death-chance answer/death-chance)
(def route-death answer/route-death)
(def either answer/either)
(def route-parts answer/route-parts)
(def spawn-kill answer/spawn-kill)
(def threat-parts answer/threat-parts)
(def spawns-chance answer/spawns-chance)
(def spawn-hurt answer/spawn-hurt)
(def spawn-kill-hurt answer/spawn-kill-hurt)
(def spawned-hurt answer/spawned-hurt)
(def spawns-hurt answer/spawns-hurt)
(def spawn-price answer/spawn-price)
(def lethal-hp answer/lethal-hp)
(def threat-hurt answer/threat-hurt)
(defn water-factors "answer/water-factors, by-kit." [kit] (by-kit ::water-factors answer/water-factors kit))

(defn swim-dark-factor
  "The planner's extra share of a second swum in the dark for kit: a drowned in dark water (water-factors' first, its measured
  meet rate) on top of the dark-factor the planner adds to a dark water cell (with-dark)."
  [kit]
  (+ (dark-factor kit) (first (water-factors kit))))
(def unseen-water-factors answer/unseen-water-factors)
(def route-death-cost answer/route-death-cost)
(def kill-price answer/kill-price)
(def spawn-worth death/spawn-worth)
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
(def chance-of find/chance-of)
(def leg-stops? find/leg-stops?)
(def ore-of find/ore-of)
(defn vein-seen [] (find/vein-seen))
(def seen-ore find/seen-ore)
(def tunnel-ore find/tunnel-ore)
(def tunnel-chance find/tunnel-chance)

(defn tunnel-rate
  "find/tunnel-rate of resource's blocks of kinds over cells, each cell its dig-cost, each block its walk."
  [resource kinds cells]
  (find/tunnel-rate resource kinds cells dig-cost (/ 1 walk-speed)))
(def ore-level find/ore-level)
(def ore-chance find/ore-chance)
(def expected-rise rise/expected-rise)
(defn leg-spacing [] (find/leg-spacing))
(def looked-area find/looked-area)
(def searched-for? find/searched-for?)

(defn look-leg-chance
  "leg-chance of a search for names by the body p, its looks of radius r: an animal's over the ground the notes ns
  searched in vain round [x z], an ore's over the faces of its host rock p has seen within r, but those beside own,
  the cells [x y z] the body cut itself: its own tunnel's walls tell nothing of the rock a look elsewhere shows."
  [p ns names [x z] r own]
  (let [beside-own? (fn [{{:keys [x y z]} :pos}]
                      (let [[x y z] (map js/Math.floor [x y z])]
                        (some own [[(inc x) y z] [(dec x) y z] [x (inc y) z] [x (dec y) z] [x y (inc z)] [x y (dec z)]])))]
    (leg-chance names {:looked (looked-area ns names [x z])
                       :a (* js/Math.PI r r)
                       :seen (when-let [ore (ore-of names)]
                               (remove beside-own? (look/seen-blocks p {:names (vec (host-blocks ore)) :radius r :max js/Infinity})))})))

(defn torch-gap-of
  "Blocks of a dark way one torch of item (nil: the plain torch) lights: its light's reach (jobs.lib.torch-plan) to each side."
  [item]
  (inc (* 2 (torch-plan/reach item))))

(def torch-gap (torch-gap-of nil))

(defn torch-way-s
  "Seconds the torches of a way of n blocks lit as it goes cost: one every torch-gap-of item blocks (nil: the plain torch), each its worth and placing
  (light-cost), and one trip for them when fewer are carried than the way hangs."
  ([n carried] (torch-way-s n carried nil))
  ([n carried item]
  (let [torches (js/Math.ceil (/ n (torch-gap-of item)))
        {:keys [torch place]} (:parts (light-cost {:carried 1}))]
    (+ (* torches (+ torch place)) (if (< carried torches) (trip) 0)))))

(defn lit-way-s
  "Seconds a way of s seconds over n blocks costs lit as it goes (the mine's stair and tunnel): s and its torches of item (torch-way-s, all carried)."
  [s n item]
  (+ s (torch-way-s n ##Inf item)))

(defn dark-or-lit
  "{:way :lit or :dark, :dark-s :lit-s}: the cheaper of staying in the dark at `dark-s` seconds (price/stay-risk-s) and lighting the job's way instead (torch-way-s of `torch-way` {:blocks :carried :item}; nil: the job hangs none)."
  [dark torch-way]
  (let [lit (if torch-way (torch-way-s (:blocks torch-way) (:carried torch-way) (:item torch-way)) ##Inf)]
    {:way (if (< lit dark) :lit :dark) :dark-s dark :lit-s lit}))

(defn dark-way-s
  "Seconds a way of s seconds over n blocks costs walked or dug dark: s, its n blocks' walk at the planner's dark price
  (dark-factor), and the hurt a spawn costs the body of kit over its s seconds (the kit's share of dark-factor, a rate)."
  [s n kit]
  (+ s (* (dark-factor) (:cost (walk-cost {:distance n}))) (* (- (dark-factor kit) (dark-factor)) s)))

(defn climb-back-s
  "Seconds getting back up `blocks` blocks costs a body that went down into land it does not know the way back from: at
  chance `trap` no walk leads up and it climbs out in stair-s seconds (one-way/climb-out-s, with its kit), else it walks
  up (walk-cost); each way dark (dark-way-s of kit) unless `lit?`."
  [blocks trap lit? kit stair-s]
  (let [way #(if (or lit? (= ##Inf %)) % (dark-way-s % blocks kit))]
    (+ (chance-times (- 1 trap) (way (:cost (walk-cost {:distance blocks}))))
       (chance-times trap (way stair-s)))))

(defn respawn-of
  "Where a death of the body of p would wake: the live spawn bed (sh-spawn/spawn-bed-pos; view nil: none), else where it
  last respawned (the latest :respawned entry's :pos, the eight latest are kept), else the world spawn the game gives every player;
  nil until the game has sent it."
  [p view]
  (or (when view (or (sh-spawn/spawn-bed-pos p view) (:pos (:data (mem/latest view :respawned)))))
      (some-> (.self p) .-worldSpawn u/pos-of)))

(defn goal-value
  "value/made-value of items as a goal: each measured item at its measured worth (value/measured-worth), what is made of
  it after them, whole crafts and their stations from nothing, and :sd, the seconds of its spread (value/leaves-sd).
  overrides (item-value's) go over the measured worths."
  ([items] (goal-value items nil))
  ([items overrides]
   (let [v (value/made-value items :overrides (merge (value/measured-worths @game/version) overrides) :basis (:goal-basis (value/measured-worth @game/version)))]
     (assoc v :sd (value/leaves-sd (:leaves v))))))

(defn fetch-value
  "goal-value of items with the walk to each gathered item's source added ((distance-of name) blocks, walk-cost), as
  the measured gathers start beside their source. distance-of nil, or nil for a name: the radius a source counts as
  seen within (look/memory-radius), the farthest, a broad overestimate (no walk to a source is measured apart)."
  ([items] (fetch-value items nil))
  ([items distance-of]
   (let [v (goal-value items)
         walk #(:cost (walk-cost {:distance (or (when distance-of (distance-of %)) look/memory-radius)}))]
     (update v :value + (reduce + 0 (map walk (keys (:leaves v))))))))

(defn field-grow
  "Seconds growing n of crop from a fresh w x h field takes, {:value mean :sd}: the cheapest hoe (none when carried) and the seeds
  short of a cell each got (goal-value), the cells tilled and sown, the water poured, the crop's ripening (ripen-s, ripen-sd),
  then each of the n cut and sown again (field-step-s); the gathers' and the ripening's spreads independent."
  ([crop n w h] (field-grow crop n w h nil))
  ([crop n w h {:keys [hoe? seeds]}]
   (let [cells (* w h)
         short (- cells (or seeds 0))
         got (cond-> []
               (not hoe?) (conj {:name (str (first weapon/cheapness) "_hoe") :count 1})
               (pos? short) (conj {:name (get-in crops/field-items [crop :seed]) :count short}))
         fetch (if (seq got) (goal-value got) {:value 0 :sd 0})
         ripen-sd (farm/ripen-sd crop n cells 0)]
     {:value (+ (:value fetch) (farm/ripen-s crop n cells 0)
                (farm/step-s :till cells) (farm/step-s :plant cells) (farm/step-s :irrigate 1)
                (farm/step-s :harvest n) (farm/step-s :plant n))
      :sd (js/Math.sqrt (+ (* (:sd fetch) (:sd fetch)) (* ripen-sd ripen-sd)))})))

(defn tree-worth
  "Seconds a tree of species with n logs is worth: the goal-value of its logs."
  [species n]
  (:value (goal-value [{:name (str species "_log") :count n}])))

(defn death-body
  "The body of p as the death costs read it: :level, :respawn (respawn-of), :carried (the goal-value of the inventory, its
  replacement cost), :health and :food, what a respawn refills, and :goal, the open walk the body was last bound for
  (jobs.lib.goal), where its job goes on."
  [p view]
  (let [self (.self p)
        g (some-> view (mem/latest goal/kind) :data :goal)]
    (cond-> {:level (or (some-> self .-experience .-level) 0)
             :respawn (respawn-of p view)
             :carried (:value (goal-value (u/inventory p)))
             :health (.-health self)
             :food (.-food self)}
      g (assoc :goal (if (vector? g) (zipmap [:x :y :z] g) g)))))

(defn death-s
  "Seconds a death of the body of p costs at `at` (default its feet): death-cost of death-body."
  ([p view] (death-s p view (some-> (.self p) .-pos u/pos-of)))
  ([p view at] (death-cost (death-body p view) at)))

(defn hurt-loss-s
  "Seconds hp of sure damage costs a body {:health :absorption} (Q151): death-s once the hp reach its life, else the hp past
  the absorption (taken first) at hp-cost."
  [{:keys [health absorption]} hp death-s]
  (if (>= hp (+ health absorption)) death-s (hp-cost (max 0 (- hp absorption)))))

(defn landing-cost-s
  "Seconds a falling block that lands on a body {:health :absorption} costs: the health/landing-s it is held in the block
  and the landing-hp it takes there (hurt-loss-s: the death once they reach its life)."
  [body death-s]
  (+ health/landing-s (hurt-loss-s body landing-hp death-s)))

(defn death-fn
  "fn of a health h: death-s of the body of p at its feet were its health h (what the respawn refills follows it)."
  [p view]
  (let [body (death-body p view)
        at (some-> (.self p) .-pos u/pos-of)]
    (fn [h] (death-cost (assoc body :health h) at))))

(defn bed? [n] (or (= "bed" n) (str/ends-with? n "_bed")))

(defn worth-to
  "Seconds getting k (default 1) of one of names is worth to body at `at`: the least goal-value among them, a bed (it
  sets the respawn) with its spawn-worth on top, put down and died near at `at`; 0 for no names."
  ([body names at] (worth-to body names at 1))
  ([body names at k]
   (if (seq names)
     (apply min (map (fn [n] (+ (:value (goal-value [{:name n :count k}]))
                                (if (bed? n) (* k (spawn-worth body at at)) 0)))
                     names))
     0)))

(defn opening-price
  "Seconds a way out costs body (death-body keys and :health): the hurt hp it expects (route-danger's :hurt of the way),
  at most its health, at hp-cost, and the death it risks (route-death-cost of route {:cells :threats
  :spawns} at `at`)."
  [body hurt route at]
  (+ (hp-cost (min hurt (:health body))) (route-death-cost body route at)))

(defn hold-price
  "Seconds a hold has cost: the seconds held and the hp lost meanwhile (hunger) at hp-cost."
  [{:keys [held-s hp-lost]}]
  (+ held-s (hp-cost hp-lost)))

(defn leg-start
  "The clock and health of the body now, for leg-price once a walk or dig leg ends."
  [c]
  {:at (ctx/now c) :health (.-health (.self (:primitives c)))})

(defn leg-price
  "Seconds a leg from `start` (leg-start) to now cost c: the clock seconds it took and the hp it lost (hold-price), never
  under its `steps` at `step-s` (the straight-line walk)."
  [c start steps step-s]
  (let [{:keys [health]} (leg-start c)]
    (max (* steps step-s)
         (hold-price {:held-s (/ (- (ctx/now c) (:at start)) 1000) :hp-lost (- (:health start) health) :health health}))))

(defn dig-leg-price
  "Seconds a dig leg from `start` (leg-start) to now cost c, with `plan-s` the plan's prices of the cells it booked (the one that
  failed too, unseen ones at the hidden guess): leg-price, never under the plan."
  [c start plan-s]
  (leg-price c start 1 (reduce + plan-s)))

(defn light-wait-worth
  "Seconds fetching n torches to light a site is worth instead of waiting wait-s for light: the wait less placing them (light-cost)."
  [wait-s n]
  (- wait-s (* n (:place (:parts (light-cost {:carried 1}))))))

(defn find-worth
  "Seconds getting `n` of names is worth to body at `at` (worth-to): a root obtain's worth. Its walks price their own trip."
  [body names at n]
  (worth-to body names at n))

(defn items-find-worth
  "Seconds getting all of `items` ({name n}: a bill, a plan's shortage) is worth (goal-value): a root kit's or bill's worth."
  [items]
  (:value (goal-value (map (fn [[n k]] {:name n :count k}) items))))

(defn search-radius
  "Blocks from its origin a search's legs spacing apart may go for a find worth `worth`: the rings within it hold as
  many legs as, each at least a walk of spacing blocks, the worth pays for. The legs' price binds first."
  [worth spacing]
  (js/Math.sqrt (/ (* (max 0 worth) spacing) (* js/Math.PI planner/walk-floor))))
