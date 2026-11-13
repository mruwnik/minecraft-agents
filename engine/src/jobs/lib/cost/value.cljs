(ns jobs.lib.cost.value
  "What a list of items is worth (item-value) and what fetching them costs (fetch-cost), in one unit: about a second
  of a player's work. recover-drops compares the two; any job may ask what a pile, a chest or a kit is worth.

  The worth of one item is the cheapest way to obtain it, from minecraft-data for the body's version. Every constant
  and the few small tables (ore rarity, natural-and-craftable blocks, passive-mob health, rare drop chances) live in
  default-basis (jobs.lib.cost.table), overridable by :basis (deep-merged). The tables are defaults, not data minecraft-data has.

    block        A block's drops (blockLoot, no silk touch, chance at least :min-drop-chance): (dig + find) / drops.
                 dig = hardness x :hardness-s / the speed of the stone tool (the block's own tier if higher; the base assumes a
                 tool, never the hand);
                 find = :terrain, :wood for a log, stem, bamboo or bamboo block, or :ores {ore [find drops]} (a nil find: :ore-scan-s, fitted to the measured ores (measured-worth), over the base-rates density of the ore's best layer), or :find-s for a block. Crafted blocks are no source
                 except :natural ones (granite, clay ...); a block that is no item (redstone_wire) is none.
    crop         A block with an age state: (:farm-s + :find :crop) / the age-7 yield.
    mob          entityLoot of the mobs in weapon/mob-max-health and passive-max-health: (kill + hurt + find) / expected
                 drops; kill = the stone sword's hits, hurt = :hurt-hp-per-hp x health x :danger hp, each at health/hp-seconds, find by hostile or passive.
    made         The cheapest recipe: the ingredients plus :craft-step, divided by the result count; or smelting by
                 name rule (raw_X -> X_ingot, X -> cooked_X, ancient_debris -> netherite_scrap, cobblestone -> stone,
                 sand -> glass, clay_ball -> brick): the input plus :smelt-step and the cheapest fuel's share (fuel-each). The table is a fixpoint, then each tool
                 is raised to at least the next weaker material's tool of its kind plus :craft-step.
    goal         made-value: a goal's count at least made from nothing, whole crafts and each station once, smelts with their fuel.
    unused       A sourced item that is no tool, armour, edible food, block or ingredient is worth :junk-share of that.
    food         An edible food is worth at least its points x :food-point-s.
    no source    :no-source-each (4) x 64 / stack size (a 64-stack 4, a 16-stack 16, an unstackable 256); a precious food nothing yields: jobs.lib.cost.precious/hp-worth at :unsourced-food-health (10). A tool, weapon or armour piece nothing yields: its durability x what a use saves (jobs.lib.cost.gear, :gear).
    unknown      A name minecraft-data does not list: :unknown-each (1).

  Per stack: worth x count (nil counts 1; 0, negative or not a number counts 0), x the durability left (:durability
  against maxDurability, at least :min-durability-share), plus each enchantment (:enchants [{:name :lvl|:level}]) at
  :per-enchant-level x level x 10 / the enchantment's weight. Walking a block costs about 0.3 (fetch-cost).

  Overrides ({name-or-group value}): a number is the worth of one item, {:times n} multiplies it. A name override
  goes into the table, so what is made of the item follows ({\"iron_ingot\" 0} makes an iron pickaxe worth its
  sticks). Group overrides apply to the final worth of their members: \"ore\" (what an ore block drops), \"tool\"
  (has durability, not armour), \"armor\", \"food\", \"block\" (placeable), \"unknown\". A name override beats a
  group one.

  Prices ([[regex price] ...], first match wins) set the worth of one item by name pattern, with :else the price of
  items no pattern matches, and :enchanted the worth of a stack with :enchants or :nbt: a job that ranks items its own
  way passes its own table, and the durability and enchantment terms do not apply to a priced
  item. A row [regex price :per-stack] prices the whole stack, not each item."
  (:require [engine.args :as a]
            [jobs.lib.game-facts :as facts]
            [clojure.set :as set]
            [clojure.string :as str]
            [jobs.lib.recipes :as recipes]
            [jobs.lib.cost.table :as tbl]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [jobs.lib.foods :as foods]
            [engine.game :as game]
            [engine.settings :as settings]))

(a/defargs settings
  {::trip {:default 10 :spec (a/num-in 0 nil)
           :doc "Any fetch: turning round, finding the pile, the risk of the place one died at, in seconds."}
   ::per-block {:default 0.3 :spec (a/num-in 0.01 nil)
                :doc "One block walked, there and back about a second of a player's time per 3 blocks."}
   ::own-place-blocks {:default 8 :spec (a/num-in 0 nil)
                       :doc "Blocks of walk the placing of a furnace of one's own counts for, on top of making it (jobs.lib.cost/walk-beats-making)."}
   ::per-climb {:default 2 :spec (a/num-in 0 nil)
                :doc "One block climbed past the first on a detour (stairs or a pillar back out of a drop, or up to a ledge), in seconds."}
   ::per-dark {:default 0 :spec (a/num-in 0 nil)
               :doc "Floor of one block walked in the dark, on top of per-block; the price of the mobs that spawn to the kit (jobs.lib.cost/dark-factor) comes on top, so a bare body pays a dark block most and an armed one less."}
   ::spawn-chance-per-s {:default {:k 120 :n-s 113230 :below {40 {:k 41 :n-s 18260}}}
                         :spec (a/pred "a map {:k n :n-s s} counts, :below an optional map of height to such counts"
                                       (fn [m] (let [ok? #(and (map? %) (number? (:k %)) (<= 0 (:k %)) (number? (:n-s %)) (<= 0.1 (:n-s %)))]
                                                 (and (ok? m) (every? (fn [[y c]] (and (number? y) (ok? c))) (:below m))))))
                         :doc "Counts {:k :n-s} behind the chance a mob spawns and reaches the body each second it stands on a cell where one can spawn (spawn-rate reads (k + 1) / n-s): k hostile meets (the first respond decision about a spawnable mob) in n-s seconds the body stood on a cell not lit-light? at the record's time of day (recorded block and sky light, sky less the darkening of the clock the bodies' memory entries stamp, clear sky) and not shut in (both room cells and every cell round them and over the head recorded solid), trial logs to 2026-11-13. :below {y counts}: a cell under y counts by the lowest such y above it, the rest by the pooled counts: under y 40 41 in 18260 s (0.0023), at y 40 and over 120 in 113230 s (0.00107; y 40-55 alone 5 in 9498, too few for a band). Recount: tools/event-counts.mjs dark-meets."}
   ::loose-over-loose-counts {:default {:h1 {:loose 38.8 :other 5.2} :h2 {:loose 31.1 :other 12.9} :h3 {:loose 29.6 :other 14.4}
                                        :h4 {:loose 10.2 :other 33.8} :h5 {:loose 4 :other 40}}
                              :spec (a/pred "a map of :h1 to :h5 to {:loose n :other n} pseudo-counts >= 0"
                                            (fn [m] (and (map? m) (every? (fn [k] (let [c (get m k)] (and (map? c) (every? #(and (number? (get c %)) (<= 0 (get c %))) [:loose :other]))))
                                                                          [:h1 :h2 :h3 :h4 :h5]))))
                              :doc "The Beta prior per height h (:h1 to :h5, the run of sand, red sand or gravel cells under the cell, capped) of the cell above a loose cell, when it is not air or a fluid (an unseen cell), being loose ground too (jobs.lib.cost.value/loose-over-loose-chance): 2.31M pairs in 4038 recorded chunk columns (every 20th, the fixture grid left out, 2026-11-07), 0.881, 0.707, 0.672, 0.232, 0.092 of 734k, 612k, 401k, 122k, 25k pairs. Spread fitted by moments over columns, as a call digs in one place (effective n about 44; h4 and h5 scaled to it). One world: layers of loose ground here stop at 3-4 cells. Recount: node tools/event-counts.mjs loose-stack --stride 20."}
   ::hidden-fall-share {:default 0.05 :doc "Chance an unseen cell over a cut cell holds a falling block (gravel, sand) that drops once the cut opens: a prior, not measured (the logs do not say what an unseen cell over a cut held)." :spec (a/num-in 0 1)}
   ::detour-factor {:default 1.27 :spec (a/num-in 1 nil)
                    :doc "Times the straight line a walk's way is taken to be (detours), for walk-blocks-per-s: 30255 blocks of steers over 23766 of straight line in 1235 walks (go-to walk jobs of 4 blocks or more), trial logs to 2026-10-28; a lower bound (steer chords). Recount: tools/event-counts.mjs walk-detour (:ratio)."}
   ::per-frontier-walk {:default 1 :spec (a/num-in 0 nil)
                        :doc "Each walk of a tour to the edge of loaded land that brought the body no nearer its goal, on top of its blocks, in seconds."}
   ::guess-rock-counts {:default {:layer-64 {:bad 516864 :ok 0} :layer-63 {:bad 207.139 :ok 52.078} :layer-62 {:bad 144.43 :ok 95.243}
                                  :layer-61 {:bad 95.887 :ok 142.434} :layer-60 {:bad 44.229 :ok 175.068}
                                  :placed {:bad 4.835 :ok 11.96} :apart {:bad 0.002 :ok 322.652}}
                        :spec (a/pred "a map of the escape/guess-classes to {:bad n :ok n} pseudo-counts >= 0"
                                      (fn [m] (and (map? m) (every? (fn [k] (let [c (get m k)] (and (map? c) (every? #(and (number? (get c %)) (<= 0 (get c %))) [:bad :ok]))))
                                                                    [:layer-64 :layer-63 :layer-62 :layer-61 :layer-60 :placed :apart]))))
                        :doc "The Beta prior per class (escape/guess-class) of a go-to dig route's guess (an unseen cell read as rock) being rock an escalation never digs (bedrock and the like), the call's own looks on top (answer/guess-bad-chance): the solid cells of the sections of 16 blocks of 2019 recorded chunk files (every 40th, the fixture grid left out, 2026-11-07). In the overworld's bedrock layer (below y -59) one class per y, :layer-64 to :layer-60, 1.0, 0.80, 0.60, 0.40 and 0.20 of 516k cells each whatever is beside it; above it :placed a protected neighbour, bedrock only over y layer-top (a build: 232 of 1231 cells, chests and doors) and :apart the rest (3.4e-6; a bedrock neighbour at y layer-top too). Spread fitted by moments over the sections with 20 cells of the class (12 for :placed), as a call digs in one place (effective n 516864, 259, 240, 238, 219, 17, 323): builds differ, so a call's own looks soon outweigh :placed's. Recount: node tools/event-counts.mjs guess-rock --stride 40; the bodies' own looks: guess-looks."}
   ::unseen-floor-counts {:default {:level {:absent 2.97 :present 9.84} :down {:absent 1.69 :present 5.71}
                                    :up {:absent 0.06 :present 1.68} :drop {:absent 1.4 :present 0.99}
                                    :drop-4 {:absent 4.7 :present 1.7} :drop-8 {:absent 7.3 :present 1.2}}
                          :spec (a/pred "a map of step kind (:level :down :up :drop :drop-4 :drop-8) to {:absent n :present n} pseudo-counts >= 0"
                                        (fn [m] (and (map? m) (every? (fn [k] (let [c (get m k)] (and (map? c) (every? #(and (number? (get c %)) (<= 0 (get c %))) [:absent :present]))))
                                                                      [:level :down :up :drop :drop-4 :drop-8]))))
                          :doc "The Beta prior per step kind (jobs.lib.walk.sight/prior-kind: a :drop is 2 or 3 lower, :drop-4 4 to 7, :drop-8 8 or more) of an unseen floor being there, a call's own floors of the kind found there or not on top (answer/floor-absent-chance): from a stand whose floor is there, the next stand's floor one block over in 2009 recorded chunk files (every 40th, the fixture grid left out, 2026-10-29), a drop is 2 or more lower (depth bands recounted 2026-11-06: absent 0.56 at 2-3, 0.78 at 4-7, 0.89 at 8 and more, from the same files with the depth taken to 48); of a stand under sky and one under none, the worse; spread fitted by moments over the chunks, as a call walks one place (effective n 12.8, 7.4, 1.7, 2.4, 6.4, 8.5; refitted over the chunks with 20 steps the 2-3 band gives about 5.6: why the stored 2.4 is lower is not known, it stays). Recount: node tools/event-counts.mjs floor-neighbours --stride 40; the bodies' own outcomes: unseen-floors."}
   ::unseen-fall-counts {:default {:down {0 4058 1 47045 2 16354 3 8482 4 5439 5 3135 6 2165 7 1426 8 1224 9 997 10 932 11 812 12 665 13 526 14 476 15 432 16 410 17 354 18 277 19 201 20 194 21 185 22 147 23 169 24 142 25 139 26 111 27 107 28 114 29 93 30 63 31 42 32 28 33 22 34 32 35 18 36 31 37 45 38 15 39 26 40 11 41 16 42 17 43 28 44 16 45 15 46 19 47 13 48 10 49 125}
                                   :drop {0 2983 1 30229 2 16656 3 10113 4 6204 5 4178 6 3117 7 2604 8 2213 9 1991 10 1708 11 1387 12 1149 13 1029 14 984 15 884 16 703 17 528 18 435 19 415 20 377 21 368 22 359 23 322 24 306 25 284 26 267 27 240 28 190 29 133 30 88 31 76 32 93 33 77 34 216 35 292 36 117 37 81 38 73 39 27 40 37 41 51 42 48 43 36 44 38 45 33 46 27 47 44 48 49 49 671}
                                   :drop-4 {0 1738 1 14291 2 9321 3 6782 4 5330 5 4595 6 3921 7 3378 8 2857 9 2416 10 2133 11 1913 12 1687 13 1412 14 1138 15 943 16 812 17 783 18 736 19 690 20 665 21 606 22 573 23 524 24 457 25 373 26 278 27 209 28 181 29 153 30 309 31 369 32 333 33 373 34 190 35 108 36 110 37 78 38 85 39 87 40 86 41 69 42 65 43 77 44 76 45 88 46 91 47 64 48 67 49 1125}
                                   :drop-8 {0 1539 1 10872 2 9458 3 8317 4 7289 5 6368 6 5601 7 5006 8 4480 9 3985 10 3508 11 3125 12 2826 13 2607 14 2399 15 2198 16 2023 17 1830 18 1666 19 1512 20 1365 21 1232 22 1101 23 996 24 919 25 870 26 836 27 804 28 751 29 730 30 536 31 438 32 422 33 376 34 380 35 374 36 360 37 341 38 324 39 320 40 325 41 312 42 302 43 272 44 253 45 228 46 211 47 208 48 186 49 6693}}
                         :spec (a/pred "a map of :down, :drop, :drop-4 and :drop-8 to {blocks n}, blocks an integer >= 0 and n >= 0"
                                       (fn [m] (and (map? m) (every? (fn [k] (let [c (get m k)] (and (map? c) (every? (fn [[b n]] (and (integer? b) (<= 0 b) (number? n) (<= 0 n))) c))))
                                                                     [:down :drop :drop-4 :drop-8]))))
                         :doc "How many blocks under an unseen floor of a :down step or a drop (jobs.lib.walk.sight/prior-kind: :drop 2-3 lower, :drop-4 4-7, :drop-8 8 or more) that is not there the walk lands, as counts per block (49: deeper than 48) (cost/unseen-fall-s): of the absent floors ::unseen-floor-counts counts (2009 recorded chunk files, every 40th, the fixture grid left out, 2026-10-30), the blocks under each to the first cell that is not open (a fluid lands at 0); recounted 2026-11-06 over 2019 files, :down from stands under no sky, the depth taken to 48 and by the step's own depth (mean 3.2, 5.2, 8.0, 12.6). Recount: node tools/event-counts.mjs floor-neighbours --stride 40 (:below)."}
   ::unseen-edge-dark-counts {:default {:roofed {:far 2.947 :near 0.681} :sky {:far 3.838 :near 0.071}}
                              :spec (a/pred "a map of :roofed and :sky to {:far n :near n} pseudo-counts >= 0"
                                            (fn [m] (and (map? m) (every? (fn [k] (let [c (get m k)] (and (map? c) (every? #(and (number? (get c %)) (<= 0 (get c %))) [:far :near]))))
                                                                          [:roofed :sky]))))
                              :doc "The Beta prior, by the sky light where the body stands (:sky over 0, :roofed 0), that a dark place whose walkers' region ends at an unseen edge is walked to from dark floor farther than 24 blocks (answer/hall-chance): of dark stands (block light 0) in 2009 recorded chunk files (every 40th, the fixture grid left out, 2026-10-29), the share a walker reaches from such floor over the true blocks, roofed 0.80 (7214 of 8990), under sky 0.98 (3154 of 3218); a walk into land never recorded is left out, so both are low if anything; spread fitted by moments over the chunks (effective n 3.6, 3.9). Recount: node tools/event-counts.mjs dark-reach --stride 40."}
   ::unseen-spot-counts {:default {:plantable 2.417 :unfit 0.634}
                         :spec (a/pred "a map {:plantable n :unfit n} of pseudo-counts >= 0"
                                       (fn [m] (and (map? m) (every? #(and (number? (get m %)) (<= 0 (get m %))) [:plantable :unfit]))))
                         :doc "The Beta prior that a replant spot the body has not seen the soil, room or light of can be planted (answer/spot-plantable-chance): of the bases of felled trees (a log on soil) in 2009 recorded chunk files (every 40th, the fixture grid left out, 2026-10-29), 3513 of 4448 had the species' growth space free of anything a tree cannot grow through (logs counted felled) and an open side lit 9 or more, 0.79; spread fitted by moments over 482 chunks (effective n 3.1). Recount: node tools/event-counts.mjs tree-spots --stride 40."}
   ::pillar-knock-counts {:default {:knock 0.116 :kept 4.741}
                          :spec (a/pred "a map {:knock n :kept n} of pseudo-counts >= 0"
                                        (fn [m] (and (map? m) (every? #(and (number? (get m %)) (<= 0 (get m %))) [:knock :kept]))))
                          :doc "The Beta prior that a blow shoves the body off a pillar for each block placed (cost/pillar-knock-s): 1 off-column ending in 99 blocks placed (pillar.done and pillar.gave-up of 7 bodies' logs, 2026-10-30; no pillar.knockback logged yet, so a lower bound), spread fitted by moments over the bodies' shares (effective n 4.9). Recount: node tools/event-counts.mjs pillar-knocks (:prior)."}
   ::retry-counts {:default {:craft {:win 0.141 :fail 0.013 :try-s 3.677} :dig {:win 0.022 :fail 0.163 :try-s 10.764} :furnace {:win 1 :fail 1 :try-s 0.293} :place {:win 0.095 :fail 0.104 :try-s 8.968} :place-refused {:win 0.01 :fail 1 :try-s 8.968} :craft-refused {:win 0.01 :fail 1 :try-s 3.677} :walk {:win 0.031 :fail 0.101 :try-s 17.148} :walk-round {:win 0.016 :fail 0.24 :try-s 1.771} :respond {:win 0.043 :fail 0.093 :try-s 9.054} :risky-stay {:win 0.043 :fail 0.093 :try-s 5} :comb-look {:win 0.043 :fail 0.093 :try-s 5} :kit-later {:win 0.043 :fail 0.093 :try-s 30} :escalate {:win 0.043 :fail 0.093 :try-s 17.148} :reseal {:win 0.043 :fail 0.093 :try-s 8.968} :refill {:win 0.043 :fail 0.093 :try-s 10.764} :child-round {:win 0.043 :fail 0.093 :try-s 17.148}}
                   :spec (a/pred "a map of retry kind to {:win n :fail n :try-s s}, pseudo-counts >= 0 not both 0 and seconds >= 0"
                                 (fn [m] (and (map? m) (every? (fn [{:keys [win fail try-s]}] (and (number? win) (number? fail) (number? try-s) (<= 0 win) (<= 0 fail) (pos? (+ win fail)) (<= 0 try-s)))
                                                               (vals m)))))
                   :doc "The Beta prior per retry kind that a try after a failed one wins (retry-chance; cost/try-again?), and :try-s the seconds that try takes: of the tries after a failed one in the same job in the trial bodies' logs (probes left out, 2026-10-29), :craft a craft action after a timeout (also a bait's equip that timed out or did not take), 17 of 20 won in 18 calls; :furnace a furnace action after a busy slot, none counted (the uniform prior, a visit's own seconds); :dig a dig action after a timeout, a failure or an unreachable block, 11 of 92 won in 2 calls (too few calls to fit a spread: the mean 0.12 at the mean pseudo-count total of the kinds that did fit); :place a place action after an occupied cell, no support, a failure or a timeout, or a jumpPlace action (the pillar's) after a partial, a failure or a timeout, 121 of 4857 in 117 calls; :walk a go-to after one that did not arrive (the end read from a :result, the root go-to's :completed/:stopped or the job.child_ended of a job id with any go-to.* event; 2026-10-30), 167 of 1512 in 377 calls; :walk-round a round inside one go-to after a fruitless one that planned again (go-to.retrying; walk/learn-again?, reach/pick!), counted per go-to call (a job id holds many; the first retrying is the condition), a later retrying failed and the go-to's result after one the call's last round (won when it arrived; a call that ends unseen adds none), 72 of 1550 in 838 calls (2026-11-04; 66 of 1098 in 611 calls without the nested seal-cleanup loops), left out of :pooled; spread fitted by moments over the calls (effective n 0.26: a call's own failures soon outweigh the pool). :respond (respond-to-hostile's call after calls that changed nothing, :hostile.call): none counted yet, so :pooled, all kinds' retries fitted together, 316 of 6481 in 514 calls; its seconds come from the caller. :risky-stay (jobs.lib.near/risky-wait!'s re-check after a stay refused for its price, :near.risky-wait and :near.risky-over): none counted yet, so :pooled; its seconds are the caller's pause. :comb-look (jobs.apiary.harvest's look for comb after one that took none, :apiary.comb-wait and :apiary.comb-over): none counted yet, so :pooled; its seconds are the comb-wait-s pause. :place-refused and :craft-refused the try after a second identical refusal in a row (same cell and same cause, cost/refusal-kind); a server refusal (spawn protection, a claimed area, a full inventory, a rejected craft) is a rule, not a chance, so it repeats until the world changes; the 121 of 4857 :place wins and 17 of 20 :craft wins above include other causes (occupied cells, timeouts), and no per-act refusal events are logged to recount, so these are set at a 1% mean, under the 2.5% of :place, with the seconds of :place and :craft. :kit-later (jobs.items.get-kit's bill run again after a :later stop or a gathering that ended, :kit.later-wait, :kit.later-over and :kit.retry): none counted yet, so :pooled; its seconds are the rest. :escalate (go-to's next way out or in after ways that started no nearer the goal, go-to.escalated :n over 1; counted by retry-wins as a try ending at the call's next escalation or its go-to end): 10 won of 100 tries in 92 calls (2026-11-13), the go-to ends of ~2000 escalations mostly unlogged, so too few to measure: :pooled; its seconds are the way's price, else those of :walk (a walk to a cell to make it from). :reseal (jobs.lib.lava/seal!'s place in a lava cell the fluid flowed back over, counted from :stair.sealed and :blocks.dig.sealed \"sealed lava at\" events: a cell's seals but the last failed): none in the trial bodies' logs (2026-11-13: 0 cells sealed twice), so :pooled, with the seconds of :place; recount when trial logs hold some. :refill (jobs.access.stair's dig of a cell its earlier digs left refilled, a falling block or something else putting it back): none counted yet, so :pooled, with the seconds of :dig. :child-round (a caller's next round of a child after rounds of it that brought nothing, jobs.lib.child/exhausted?: obtain's craft and gather runs, breed's attempts, get-food's searches, a recovery's seal clean-ups, a herd's gate shut, grow's tend passes): none counted yet, so :pooled; its seconds are the caller's last fruitless round's, start to end, else (a child that never ran) those of :walk (most such rounds walk). Recount: node tools/event-counts.mjs retry-wins (its per-call counts are kept in the counts file, so rotated logs still count)."}
   ::gather-counts {:default {"birch_log" {:s 34.417, :got 6, :calls 3}, "birch_sapling" {:s 342.572, :got 9, :calls 7}, "coal" {:s 104.443, :got 7, :calls 6}, "cobblestone" {:s 1408.92, :got 202, :calls 32}, "dirt" {:s 181.444, :got 69, :calls 4}, "log" {:s 5073.127, :got 0, :calls 58}, "oak_log" {:s 65.975, :got 10, :calls 6}, "oak_sapling" {:s 339.004, :got 12, :calls 9}, "raw_iron" {:s 5873.701, :got 28, :calls 21}, "white_wool" {:s 101.411, :got 3, :calls 5}}
                    :spec (a/pred "a map of item name to {:s seconds >= 0 :got items >= 0}"
                                  (fn [m] (and (map? m) (every? (fn [{:keys [s got]}] (and (number? s) (number? got) (<= 0 s) (<= 0 got))) (vals m)))))
                    :doc "Seconds the trial bodies' gathers of an item held the body and the items of it picked up meanwhile, fruitless gathers and their child jobs' seconds counted, a reflex's or an offline body's not (logs to 2026-10-29, probes left out, gathers logged at debug alone: 21 raw iron gathers got 28 in 5874 s, 32 cobblestone 202 in 1409 s, 6 coal 7 in 104 s). A name minecraft-data has not (log: any log) counts for nothing. measured-worth weighs them with the recipe tree. Recount: node tools/event-counts.mjs item-costs (:gather-counts)."}
   ::gather-prior-items {:default 1.516 :spec (a/num-in 0 nil)
                         :doc "Items a measured item's recipe-tree price counts as beside its gathers (measured-worth): the shape of the gathers' rates about their item's, fitted by moments over the items with two or more gathers and pooled. Recount: node tools/event-counts.mjs item-costs (:prior-items)."}
   ::food-worth-per-point {:default 30 :spec (a/num-in 0 nil)
                           :doc "Seconds reaching food is worth per food point the body is under ::food-worth-from."}
   ::food-worth-from {:default 10 :spec (a/num-in 0 20)
                      :doc "Food at or over which reaching food is worth nothing; it rises as the bar falls under it."}
   ::torch-place-s {:default 1.5 :spec (a/num-in 0 nil)
                    :doc "Placing a torch to light a dark floor and waiting for the light to show it, in seconds."}})

(defn count-of [{:keys [count]}]
  (cond (nil? count) 1
        (and (number? count) (js/isFinite count) (pos? count)) count
        :else 0))

(defn enchant-worth [version basis enchants]
  (let [md (facts/data version)]
    (reduce + 0 (map (fn [e]
                       (let [ench (some->> (or (:name e) (:id e)) str (aget (.-enchantmentsByName md)))
                             weight (or (some-> ench .-weight) 10)
                             level (let [l (or (:lvl e) (:level e) 1)] (if (number? l) l 1))]
                         (* (:per-enchant-level basis) level (/ 10 (max 1 weight)))))
                     enchants))))

(defn enchanted?
  "Whether the entry carries a non-empty :enchants or :nbt."
  [entry]
  (boolean (some #(seq (get entry %)) [:enchants :nbt])))

(defn price-of
  "[each stack?] of the first prices row matching name ([regex price] or [regex price :per-stack]), else nil."
  [prices name]
  (some (fn [[re p mode]] (when (re-find re name) [p (= :per-stack mode)])) prices))

(defn stack-worth
  "{:name :count :each :value} of one entry; group overrides skip a name that has its own (named, a map)."
  [version basis table group-overrides named {:keys [prices else enchanted]} {:keys [name durability enchants] :as entry}]
  (let [n (count-of entry)
        [price per-stack] (or (when (and enchanted (enchanted? entry)) [enchanted true])
                              (price-of prices name)
                              (when else [else false]))
        md (facts/data version)
        item (aget (.-itemsByName md) name)
        base (get table name ((tbl/override-fn (get group-overrides "unknown")) (:unknown-each basis)))
        grouped (reduce (fn [w g] ((tbl/override-fn (get group-overrides g)) w))
                        base (if (and (contains? table name) (not (contains? named name))) (tbl/groups-for version name) []))
        max-d (some-> item .-maxDurability)
        share (if (and (number? durability) (number? max-d) (pos? max-d))
                (max (:min-durability-share basis) (min 1 (/ durability max-d)))
                1)
        each (cond (nil? price) (+ (* grouped share) (enchant-worth version basis enchants))
                   per-stack (/ price (max 1 n))
                   :else price)]
    {:name name :count n :each each :value (* n each)}))

(declare item-value)

(defn- tree-each
  "Seconds one `n` is worth by the recipe tree alone, its find seconds from `find` (over :find in default-basis)."
  [version n find]
  (or (:each (first (:items (item-value [{:name n :count 1}] :version version :basis {:find find})))) 0))

(defn- fit
  "Seconds per unit of find, fitted pooled to rows {:s :got :a :b} whose tree price is a + b x the unit: nil with none."
  [rows]
  (let [per (reduce + (map #(* (:got %) (:b %)) rows))]
    (when (pos? per) (max 0 (/ (reduce + (map #(- (:s %) (* (:got %) (:a %))) rows)) per)))))

(def ^:private measured
  (memoize
   (fn [version counts k]
     (let [md (facts/data version)
           terrain (get-in tbl/default-basis [:find :terrain])
           tree #(tree-each version %1 {:ore-scan-s %2 :terrain %3})
           rows (for [[n {:keys [s got]}] counts
                      :when (aget (.-itemsByName md) n)
                      :let [a (tree n 0 terrain) at (tree n 0 0)]]
                  {:n n :s s :got got :a a :b (- (tree n 1 terrain) a) :at at :bt (- (tree n 0 1) at)})
           ores (filter #(pos? (:b %)) rows)
           others (remove #(pos? (:b %)) rows)
           scan (fit ores)
           find (fit (map #(assoc % :a (:at %) :b (:bt %)) (filter #(< 0 (:bt %) 1.001) others)))
           worth (fn [{:keys [s got]} tree-s] (/ (+ s (* k tree-s)) (+ got k)))]
       {:basis (if scan {:find {:ore-scan-s scan}} {})
        :goal-basis (if find {:find {:terrain find :crop find}} {})
        :ores (into {} (map (fn [r] [(:n r) (worth r (+ (:a r) (* (:b r) (or scan 0))))])) ores)
        :goals (into {} (map (fn [r] [(:n r) (worth r (+ (:at r) (* (:bt r) (or find terrain))))])) others)}))))

(defn measured-worth
  "{:basis :goal-basis :ores :goals} of ::gather-counts for minecraft-data version. Each measured item is worth (s + k x
  tree) / (got + k), its gathers' seconds and items with the recipe tree's price at k = ::gather-prior-items items.
  :basis's :ore-scan-s is fitted to the measured ores (an item whose tree price grows with it) pooled, so every ore
  follows by density at the margin too (item-value); :ores are those ores' worths. :goal-basis's :terrain
  is the find fitted so to the measured terrain blocks one of which gives an item (cobblestone, dirt; not a sapling of
  leaves, dug many at one find), a goal's walk to the block, a ripe crop's (:crop) too; :goals, the
  other measured items' worths with it. Both are the goal's alone (goal-value): at the margin a block is its dig and
  its find (an ore's at the fitted scan), its fruitless runs not counted."
  [version]
  (measured version (settings/get settings ::gather-counts) (settings/get settings ::gather-prior-items)))

(defn measured-worths
  "{item seconds} of the measured items (measured-worth :ores and :goals), as item-value :overrides for a goal;
  cobblestone's for every stone tool material (jobs.lib.recipes/stone-materials), as one gather takes whichever is seen."
  [version]
  (let [{:keys [ores goals]} (measured-worth version)
        m (merge ores goals)]
    (if-let [cobble (m "cobblestone")]
      (merge (zipmap (recipes/stone-materials version) (repeat cobble)) m)
      m)))

(def ^:private group-names #{"ore" "tool" "armor" "food" "block" "unknown"})

(defn counted-chance
  "The chance of a measured share, k of n counted {:k :n}, under a Beta(a, b) prior of pseudo-counts :prior [a b] (a of the
  share, b of the rest; default the uniform [1 1]): (k + a) / (n + a + b)."
  [{:keys [k n prior]}]
  (let [[a b] (or prior [1 1])]
    (/ (+ k a) (+ n a b))))

(defn pillar-knock-chance
  "The chance a blow shoves the body off a pillar for each block placed: the mean of ::pillar-knock-counts."
  []
  (let [{:keys [knock kept]} (settings/get settings ::pillar-knock-counts)]
    (counted-chance {:k 0 :n 0 :prior [knock kept]})))

(defn retry-chance
  "The chance the next try of kind wins after fails tries of the call failed in a row: counted-chance of no win in the
  fails after the first over ::retry-counts' pseudo-counts of kind. Those are counted over tries that followed a
  failed one, so the first failure is their condition, not evidence: the first retry wins at the prior's mean."
  [kind fails]
  (let [{:keys [win fail]} (get (settings/get settings ::retry-counts) kind)]
    (counted-chance {:k 0 :n (max 0 (dec fails)) :prior [win fail]})))

(defn retry-s
  "The measured seconds one try of kind takes (::retry-counts :try-s)."
  [kind]
  (:try-s (get (settings/get settings ::retry-counts) kind)))

(defn craft-step-s
  "Expected seconds one craft step takes: a finished one's tbl/craft-step, or with the measured timeout share, a timeout's
  seconds and the retries after it (the :craft ::retry-counts :try-s over the chance each wins, retry-chance :craft after the first failure)."
  []
  (let [{:keys [k n s]} (tbl/craft-timeouts)
        p (/ k n)]
    (+ (* (- 1 p) (tbl/craft-step)) (* p (+ s (/ (retry-s :craft) (retry-chance :craft 1)))))))

(defn- table-basis
  "[table basis] item-value prices by: the worth table of the name overrides and the whole basis."
  [version name-overrides basis]
  (let [basis (tbl/deep-merge {:craft-step (craft-step-s)} (tbl/deep-merge (when-not (get-in basis [:find :ore-scan-s]) (:basis (measured-worth version))) basis))]
    [(tbl/table-for version name-overrides basis) (tbl/full-basis basis)]))

(def ^:private smelts-of (memoize (fn [version] (tbl/smelt-sources (map #(.-name %) (array-seq (.-itemsArray (facts/data version))))))))

(def ^:private fuel-each (memoize tbl/fuel-each))

(defn smelt-price
  "Seconds one smelted item takes: ::smelt-step and the cheapest fuel's share of it (fuel-each). Options :version, :basis."
  [& {:keys [version basis]}]
  (let [[table basis] (table-basis (or version @game/version) {} basis)]
    (+ (:smelt-step basis) (fuel-each table))))

(defn- made
  "{:s :stations} of n of name made from nothing at the table's worths (m: :version :table :basis, :stations-s the
  seconds of a set of stations): an item cheaper got than made (its table worth below every way's) n x its worth, else
  the way (recipe or smelt) whose whole crafts, a craft step each, and stations (crafting table, furnace) cost least,
  a smelt costing its step and its fuel per item;
  Infinity for an item made of itself up the chain (an ingot of nuggets of ingots)."
  [{:keys [version table basis stations-s] :as m} name n chain]
  (let [unit (get table name js/Infinity)
        step (:craft-step basis)
        smelt-s (+ (:smelt-step basis) (fuel-each table))
        ways (filter #(js/isFinite (:per %))
                     (concat (for [r (get (recipes/recipes-for version) name)]
                               {:per (/ (+ step (reduce + (map (fn [[k v]] (* v (get table k js/Infinity))) (:needs r)))) (:count r)) :r r})
                             (for [i (get (smelts-of version) name)] {:per (+ smelt-s (get table i js/Infinity)) :smelt i})))
        within (conj chain name)
        made-way (fn [{:keys [r smelt]}]
                   (if smelt
                     (let [in (made m smelt n within)]
                       {:s (+ (* n smelt-s) (:s in)) :stations (conj (:stations in) "furnace")})
                     (let [ins (map (fn [[k v]] (made m k (* v (js/Math.ceil (/ n (:count r)))) within)) (:needs r))]
                       {:s (+ step (reduce + (map :s ins)))
                        :stations (cond-> (reduce set/union #{} (map :stations ins)) (:table? r) (conj recipes/table-item))})))]
    (cond
      (contains? chain name) {:s js/Infinity :stations #{}}
      (or (empty? ways) (< unit (* (apply min (map :per ways)) (- 1 1e-9)))) {:s (* n unit) :stations #{}}
      :else (apply min-key #(+ (:s %) (stations-s (:stations %))) (map made-way ways)))))

(def ^:private station-base
  "made of station st, its own stations priced at their table worths, so pricing one never needs itself."
  (memoize (fn [version table basis st] (made {:version version :table table :basis basis :stations-s #(reduce + 0 (map table %))} st 1 #{}))))

(defn- stations-closure
  "Stations st and the stations making them needs."
  [version table basis st]
  (let [more (reduce set/union st (map #(:stations (station-base version table basis %)) st))]
    (if (= more st) st (recur version table basis more))))

(def ^:private made-memo
  (memoize (fn [version table basis name n]
             (made {:version version :table table :basis basis
                    :stations-s (fn [st] (reduce + 0 (map #(:s (station-base version table basis %)) (stations-closure version table basis st))))}
                   name n #{}))))

(declare item-value)

(defn made-value
  "item-value of items as a goal from a reference start, nothing carried and no station near: each plain stack (no
  durability, enchantments or nbt) at least what making its count takes (made; one nothing makes keeps its item-value), and each crafting table or furnace
  those crafts and smelts need, and what those need, once, unless the items hold it. Options as item-value's but :prices."
  [items & {:keys [overrides version basis] :as opts}]
  (let [version (or version @game/version)
        [table basis] (table-basis version (apply dissoc (tbl/normal-overrides overrides) group-names) basis)
        named (group-by :name (filter #(string? (:name %)) items))
        plain (set (keep (fn [[n es]] (when (every? (fn [e] (not-any? #(some? (get e %)) [:durability :enchants :nbt])) es) n)) named))
        rows (map (fn [{:keys [name count value] :as row}]
                    (if (plain name)
                      (let [{:keys [s stations]} (made-memo version table basis name count) v (if (js/isFinite s) (max value s) value)]
                        (assoc row :value v :each (/ v count) :stations stations))
                      row))
                  (:items (apply item-value items (mapcat identity (dissoc opts :prices)))))
        stations (stations-closure version table basis (reduce set/union #{} (keep :stations rows)))]
    {:value (+ (reduce + 0 (map :value rows)) (reduce + 0 (map #(:s (station-base version table basis %)) (remove (set (keys named)) stations))))
     :items (vec (sort-by :value > (map #(dissoc % :stations) rows)))}))

(defn item-value
  "{:value total :items [{:name :count :each :value} ...]} of items ([{:name :count ...}], inventory or drop shape),
  main contributors first. Stacks of one name with the same durability, enchantments and :nbt are added. See the ns doc.
  Options: :overrides, :prices, :else and :enchanted (see the ns doc), :basis (over default-basis in jobs.lib.cost.table), :version
  (minecraft-data, default the body's). Never throws on odd entries: no name is worth 0, an unknown name :unknown-each."
  [items & {:keys [overrides prices else enchanted version basis]}]
  (let [version (or version @game/version)
        overrides (tbl/normal-overrides overrides)
        group-overrides (select-keys overrides group-names)
        name-overrides (apply dissoc overrides group-names)
        [table basis] (table-basis version name-overrides basis)
        named (filter #(string? (:name %)) items)
        merged (map (fn [[k stacks]] (assoc k :count (reduce + 0 (map count-of stacks))))
                    (group-by #(select-keys % [:name :durability :enchants :nbt]) named))
        opts {:prices prices :else else :enchanted enchanted}
        rows (->> merged
                  (map #(stack-worth version basis table group-overrides name-overrides opts %))
                  (filter #(pos? (:count %)))
                  (sort-by :value >)
                  vec)]
    {:value (reduce + 0 (map :value rows)) :items rows}))

(defn wear-saved
  "Seconds of wear on tool `dear` that digging n blocks with tool `spare` instead saves: dear's whole item-value over
  its maxDurability for each use, at most spare's maxDurability uses (n nil: all of them). 0 when either has none."
  [dear spare n & {:keys [version]}]
  (let [md (facts/data (or version @game/version))
        uses #(some-> (aget (.-itemsByName md) %) .-maxDurability)
        dear-uses (uses dear)
        spare-uses (uses spare)]
    (if (and (pos? dear-uses) (pos? spare-uses))
      (* (/ (:value (item-value [{:name dear :count 1}] :version version)) dear-uses) (min spare-uses (or n spare-uses)))
      0)))

;; ---------------------------------------------------------------- the cost

(defn reserve-cost
  "What using one unit worth value costs against a reservation of strength s (0..1): value x s/(1-s), so 0.5 counts the
  item twice and 0.9 ten times; strength 1 is ##Inf (never)."
  [value s]
  (if (>= s 1) ##Inf (* value (/ s (- 1 s)))))

(defn chance-times
  "Chance p of seconds s, the expected seconds: 0 when p is 0, though s be ##Inf (a price with no way), where p x s is NaN."
  [p s]
  (if (zero? p) 0 (* p s)))

(defn affords?
  "Whether a price of `s` seconds is within `worth`: never a certain failure (##Inf)."
  [s worth]
  (and (js/isFinite s) (<= s worth)))

(defn trip [] (settings/get settings ::trip))
(defn per-block [] (settings/get settings ::per-block))
(defn per-dark [] (settings/get settings ::per-dark))
(defn cells-of
  "The cells of a place: a cell [x y z], a collection of cells, or a box {:min [x y z] :max [x y z]} (inclusive)."
  [place]
  (cond
    (map? place) (let [[x0 y0 z0] (:min place) [x1 y1 z1] (:max place)]
                   (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [x y z]))
    (number? (first place)) [place]
    :else place))

(defn spawns-fn
  "(fn [cell]) the chance a mob spawns at cell. :light (cell -> {:block :sky} as the body saw it, the sky light already less
  the sky's darkening and 0 where it no longer lights; {:dark} the chance an unseen cell is dark, jobs.lib.look's guess; nil
  or missing: unseen, dark). A cell spawns only at block light 0 and sky light 0."
  [{:keys [light]}]
  (fn [cell] (let [{:keys [block sky dark] :or {block 0 sky 0}} (when light (light cell))]
               (or dark (if (and (zero? block) (zero? sky)) 1 0)))))

(defn spawn-share
  "The share of place's cells (a cell, a set of cells or a {:min :max} box) a mob spawns at (spawns-fn of opts)."
  [place opts]
  (let [cells (cells-of place)]
    (/ (transduce (map (spawns-fn opts)) + 0 cells) (max 1 (count cells)))))

(defn spawn-rate
  "The chance per second a mob spawns and reaches a body on a dark cell at height y (nil: the pooled counts): (k + 1) / n-s
  of ::spawn-chance-per-s, of its :below band when y is under one."
  ([] (spawn-rate nil))
  ([y] (spawn-rate (settings/get settings ::spawn-chance-per-s) y))
  ([{:keys [below] :as counts} y]
   (let [{:keys [k n-s]} (or (when y (some (fn [[top c]] (when (< y top) c)) (sort-by key below))) counts)]
     (/ (inc k) n-s))))

(defn spawn-chance
  "The chance per second a mob spawns at place and reaches a body standing there: over its cells, spawn-rate at each one's
  height x spawns-fn of opts."
  ([place] (spawn-chance place nil))
  ([place opts]
   (let [cells (cells-of place)
         spawns (spawns-fn opts)
         counts (settings/get settings ::spawn-chance-per-s)]
     (/ (transduce (map #(* (spawn-rate counts (second %)) (spawns %))) + 0 cells) (max 1 (count cells))))))

(def loose-stack-cap "The tallest run of loose cells ::loose-over-loose-counts tells apart." 5)

(defn loose-over-loose-chance
  "The chance the unseen cell above a run of h loose cells (sand, red sand, gravel) is loose too: ::loose-over-loose-counts of height h (capped at loose-stack-cap), loose over loose plus other."
  [h]
  (let [{:keys [loose other]} (get (settings/get settings ::loose-over-loose-counts) (keyword (str "h" (max 1 (min h loose-stack-cap)))))]
    (/ loose (max 1e-9 (+ loose other)))))

(defn hidden-fall-share [] (settings/get settings ::hidden-fall-share))

(defn dark-factor
  "The floor of the planner's extra cost of a dark cell as a share of its own seconds: per-dark over per-block
  (jobs.lib.cost/dark-factor adds a kit's spawn price)."
  [] (/ (per-dark) (per-block)))
(defn per-danger "One expected point of damage (jobs.lib.cost/route-danger): what an hp costs (jobs.lib.cost.health)." [] (health/hp-seconds))
(defn own-place-blocks [] (settings/get settings ::own-place-blocks))
(defn walk-blocks-per-s
  "Blocks of straight line a second a walk covers: fight/walk-speed over ::detour-factor."
  []
  (/ fight/walk-speed (settings/get settings ::detour-factor)))
(defn pile-lost
  "Why a death's pile is gone at once, else nil: :void for a fall out of the world; :burned for a death by lava or fire
  with lava or fire where the feet stood (feet-blocks: the names of the feet cell and the one under it, nil when unknown;
  items burn the moment they land there). A body burned to death beside lava or off the fire drops its pile on the floor."
  [cause feet-blocks]
  (let [c (str/lower-case (if (keyword? cause) (name cause) (str cause)))]
    (cond
      (some #(str/includes? c %) ["void" "out_of_world"]) :void
      (and (some #(str/includes? c %) ["lava" "fire" "burn"])
           (or (nil? feet-blocks) (some #{"lava" "fire" "soul_fire"} feet-blocks))) :burned)))

(defn walk-cost
  "{:cost :parts {:walk :danger}} of walking `distance` blocks at the per-block price every trip is costed with, and
  `danger-s` seconds of danger on the way (jobs.lib.cost/risk-s)."
  [{:keys [distance danger-s]}]
  (let [parts {:walk (* (per-block) distance) :danger (or danger-s 0)}]
    {:cost (reduce + (vals parts)) :parts parts}))

(defn survived-worth
  "Seconds a find worth `worth` is worth to a body whose trip to it ends in death with `chance`: a dead body does not bring it
  back. The trip's death cost (carried kit, walk back) is priced beside it, in the walk's danger."
  [worth chance]
  (chance-times (- 1 (min 1 (max 0 (or chance 0)))) worth))

(defn walk-beats-making
  "Whether walking `distance` blocks to something that exists costs no more than making one's own (cost `make`, in seconds)
  and walking the own-place-blocks setting to put it down."
  [{:keys [distance make]}]
  (<= (:cost (walk-cost {:distance distance}))
      (+ make (:cost (walk-cost {:distance (settings/get settings ::own-place-blocks)})))))

(defn tour-cost
  "{:cost :parts {:walk :walks}} of a tour of frontiers (walks to the edge of loaded land): the blocks `explored` since
  the body last came nearer its goal at the walk price, and per-frontier-walk for each of the `walks` since then."
  [{:keys [explored walks]}]
  (let [parts {:walk (:cost (walk-cost {:distance explored})) :walks (* (settings/get settings ::per-frontier-walk) walks)}]
    {:cost (reduce + (vals parts)) :parts parts}))

(defn cover-tour-s
  "Seconds (walk-cost) of a greedy walk from point `from` ([x y z]) that brings every one of `points` within `reach`
  blocks: each leg walks to reach of the nearest point still beyond it; the points within reach of a stop are done."
  [from points reach]
  (let [d (fn [a b] (apply js/Math.hypot (map - a b)))]
    (loop [at from left points s 0]
      (let [left (remove #(<= (d at %) reach) left)]
        (if (empty? left)
          s
          (let [p (apply min-key #(d at %) left)
                gap (d at p)]
            (recur (mapv #(+ %2 (* (- %1 %2) (/ reach gap))) at p) (remove #{p} left) (+ s (:cost (walk-cost {:distance (- gap reach)}))))))))))

(defn tour-worth
  "Seconds of tour a goal `straight` blocks off is worth to a caller counting arrival `worth` seconds:
  that less the straight walk, 0 at least. A goal go-to is given is known: a search nets its own trip in its worth."
  [straight worth]
  (max 0 (- worth (:cost (walk-cost {:distance straight})))))

(defn turn-back-cost
  "{:cost :parts {:look :back}} of a way over unseen `floors` [{:look :back :p :extra-s :absent-s}] in walk order, each not there at
  chance p: the way reaches a floor while those before it held, pays its look (seconds), and when it is not there walks
  back to the way's start (:back blocks) and takes the next-best way, :extra-s seconds dearer, or what its look shows
  instead costs, :absent-s seconds in all when given."
  [floors]
  (let [parts (:parts (reduce (fn [{:keys [reach] :as acc} {:keys [look back p extra-s absent-s]}]
                                (-> acc
                                    (update-in [:parts :look] + (* reach look))
                                    (update-in [:parts :back] + (chance-times (* reach p) (or absent-s (+ (:cost (walk-cost {:distance back})) (or extra-s 0)))))
                                    (assoc :reach (* reach (- 1 p)))))
                              {:reach 1 :parts {:look 0 :back 0}}
                              floors))]
    {:cost (+ (:look parts) (:back parts)) :parts parts}))

(defn food-point-worth
  "Seconds one hunger point of food is worth while the pack lacks the food reserve."
  []
  (settings/get settings ::food-worth-per-point))

(defn food-search-worth
  "Seconds a search for food is worth while the pack lacks `short` hunger points: food-point-worth each, less one trip."
  [short]
  (max 0 (- (* (food-point-worth) short) (trip))))

(def meal-points
  "{item hunger points} per item of the items a crafting recipe of that item alone makes a food of (3 wheat a bread: 5/3 a wheat), memoised per version."
  (memoize
   (fn [version]
     (let [food (foods/table-for version)]
       (reduce (fn [m [out n ins]]
                 (let [pts (get-in food [out :points])]
                   (if (and pts (apply = ins)) (update m (first ins) (fnil max 0) (/ (* pts n) (count ins))) m)))
               {} (tbl/recipe-list version))))))

(defn food-points
  "Hunger points one item of name gives at best: eaten (none when harmful), cooked or made into a food alone (meal-points); 0 when none."
  [name]
  (let [version @game/version
        own #(if (contains? foods/harmful %) 0 (get-in (foods/table-for version) [% :points] 0))]
    (max (own name) (if-let [c (foods/cooked-of name)] (own c) 0) (get (meal-points version) name 0))))

(def crop-points-for
  (memoize (fn [version crop] (reduce + 0 (map (fn [{:keys [name count]}] (* count (food-points name))) (tbl/crop-yields crop :version version))))))

(defn crop-points
  "Hunger points a ripe crop of block name gives when cut: food-points of what it drops (table/crop-yields)."
  [crop]
  (crop-points-for @game/version crop))

(defn crop-worth
  "Seconds a ripe crop of block name is worth when cut: its crop-points at food-point-worth."
  [crop]
  (* (food-point-worth) (crop-points crop)))

(def expected-drops "Meat items a kill is counted to give when the game data lists none." 2)

(defn animal-worth
  "Seconds an animal of kind `mob` is worth: the food-points of its raw meat's kill-drops at food-point-worth, as crop-worth
  prices a crop; a kind with no meat its own name's item-value at the measured gathers."
  [mob]
  (if-let [meat (get (set/map-invert game/meat-animal) mob)]
    (* (food-point-worth) (food-points meat) (or (tbl/kill-drops mob meat) expected-drops))
    (:value (item-value [{:name mob :count expected-drops}] :overrides (measured-worths @game/version)))))

(defn per-climb [] (settings/get settings ::per-climb))

(defn detour-cost
  "{:cost :parts {:walk :climb}} of stepping off the way to pick something up `distance` blocks off and `climb` blocks
  above or below the feet: per-block a block (there and back) plus per-climb for each block of climb past the first.
  opts :per-block and :per-climb override the settings."
  ([spot] (detour-cost spot {}))
  ([{:keys [distance climb]} opts]
   (let [block (or (:per-block opts) (per-block))
         up (or (:per-climb opts) (per-climb))
         parts {:walk (* block distance) :climb (* up (max 0 (dec (abs (or climb 0)))))}]
     {:cost (reduce + (vals parts)) :parts parts})))

(defn torch-place-s [] (settings/get settings ::torch-place-s))

(defn light-cost
  "{:cost :parts {:torch :place :trip}} of one torch go-to places to light a dark floor (the planner's lightCost,
  seconds): the torch's worth, torch-place-s, and a trip when none is carried (it is fetched first)."
  [{:keys [carried]}]
  (let [parts (cond-> {:torch (:value (item-value [{:name "torch" :count 1}])) :place (torch-place-s)}
                (not (pos? carried)) (assoc :trip (trip)))]
    {:cost (reduce + (vals parts)) :parts parts}))

(defn fetch-cost
  "{:cost :parts {:trip :walk :danger :collect}} of fetching a pile `distance` blocks off, with `danger-s` seconds of danger
  on the way, `elapsed-ms` after it dropped; `back?` walks the distance twice (the despawn check is for the way there and the stay), `stay-s`
  seconds are spent standing at the pile.
  Or {:cost js/Infinity :reason}: :no-position (distance nil), :window-closed (the 5 min despawn passed), :too-far
  (the walk would end after the despawn), or `lost` (pile-lost: the items burned or fell out of the world)."
  [{:keys [distance danger-s elapsed-ms lost back? stay-s]}]
  (let [elapsed (or elapsed-ms 0)
        despawn-ms (game/despawn-ms)
        walk-ms (when (number? distance) (* 1000 (/ distance (walk-blocks-per-s))))
        reason (cond
                 (nil? walk-ms) :no-position
                 (>= elapsed despawn-ms) :window-closed
                 lost lost
                 (>= (+ elapsed walk-ms (* 1000 (or stay-s 0))) despawn-ms) :too-far)]
    (if reason
      {:cost js/Infinity :reason reason}
      (let [w (walk-cost {:distance (* distance (if back? 2 1)) :danger-s danger-s})
            stay (or stay-s 0)]
        {:cost (+ (trip) stay (:cost w)) :parts (cond-> (assoc (:parts w) :trip (trip)) (pos? stay) (assoc :collect stay))}))))
