(ns engine.goal-worth-test
  "Goal jobs price their own goal at the root (their registry worth-of): what the goal yields, never ##Inf."
  (:require [cljs.test :refer [deftest is]]
            [engine.apiary-maintain-test :as am]
            [engine.night-support :refer [sleeper]]
            [engine.farm-tidy-test :as ft]
            [engine.harvest-test :as ht]
            [engine.herd-test :as herd]
            [engine.library-test :as lt]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.animals :as animals]
            [jobs.farm.maintain :as farm-maintain]
            [jobs.farm.tend :as tend]
            [jobs.forestry.maintain :as forest-maintain]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.farm :as cost-farm]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.crops :as crops]
            [jobs.lib.trees :as forestry]
            [jobs.lib.price :as price]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.tidy :as tidy]
            [jobs.forestry.plant-sapling :as plant-sapling]
            [jobs.maintenance.shut :as shut]
            [jobs.lib.util :as u]
            [jobs.survival.bed-down :as bed-down]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.world-files :as world]))

(defn ctx-of
  "A root ctx over primitives p with args (more: extra ctx keys)."
  [p args more]
  (merge {:primitives p :args args :id "j" :root "j" :slots [] :view (fn [] {:now 0 :data mem/empty-data})
          :engine {:world (world/of-data {} {} []) :warned (atom #{})}}
         more))

(defn worth
  "The root worth job's worth-of gives (ctx-of p args more)."
  ([job p args] (worth job p args {}))
  ([job p args more] ((:worth-of (get registry/jobs job)) (ctx-of p args more))))

(defn priced? [w] (and (pos? w) (js/isFinite w)))

(defn item [n k] {:name n :count k})

(deftest fish-trade-breed-bring-and-hunt-are-worth-what-they-still-want
  (let [fish #(worth 'jobs.food.fish (tu/fake {:inventory %2}) {:count %1})
        trade #(worth 'jobs.village.trade (tu/fake {}) {:buy "bread" :count %})
        breed #(worth 'jobs.animals.breed (tu/fake {}) {:mob "cow" :pairs %})]
    (is (priced? (fish 2 [])))
    (is (= (fish 2 []) (fish 3 [(item "cod" 1)])) "a fish carried is caught already")
    (is (priced? (trade 1)))
    (is (< (trade 1) (trade 4) (* 4 (trade 1))) "four breads: four times the wheat, one crafting table")
    (is (= (* 2 (breed 1)) (breed 2)) "a calf a pair")
    (is (priced? (worth 'jobs.animals.bring (tu/fake {}) {:mob "cow"})))
    (is (= (worth 'jobs.combat.hunt (tu/fake {}) {:item "beef" :count 1})
           (worth 'jobs.combat.hunt (tu/fake {:inventory [(item "beef" 1)]}) {:item "beef" :count 2})) "a hunt: the items still to carry")))

(deftest food-jobs-are-worth-the-hunger-or-the-reserve-not-inf
  (let [food (fn [job food args] (worth job (tu/fake {:self {:food food}}) args))]
    (is (= (cost/food-worth 4 20) (food 'jobs.survival.eat 4 {})) "eat: what reaching food is worth a hungry body")
    (is (= (- (cost/food-worth 4 20) (:cost (cost/walk-cost {:distance (cost/search-radius (cost/food-worth 4 20) (cost/leg-spacing))})))
           (food 'jobs.survival.get-food 4 {:carry 0}))
        "get-food: hunger, no reserve wanted, less the walk back from the search's reach")
    (is (priced? (food 'jobs.survival.get-food 20 {:carry 40})) "a fed body's reserve walk is priced by the points the pack lacks")
    (is (zero? (food 'jobs.survival.eat 20 {})) "fed: eating is worth nothing")
    (is (= (cost/food-search-worth cost/food-reserve-points) (food 'jobs.farm.grow 20 {})))
    (is (= (food 'jobs.farm.grow 20 {}) (food 'jobs.farm.find-spot 20 {})))))

(def oak-world {:blocks (merge {"3,63,0" "dirt"} (lt/tree 3 0 "oak" 4))})

(deftest harvest-wood-is-worth-the-logs-it-still-wants-or-the-tree-it-fells
  (let [wood #(worth 'jobs.forestry.harvest-wood (tu/seeing-all (tu/fake-on-floor (assoc oak-world :inventory %2))) (merge {:radius 16} %1))]
    (is (priced? (wood {:count 2 :species "oak"} [])))
    (is (= (wood {:count 2 :species "oak"} []) (wood {:count 3 :species "oak"} [(item "oak_log" 1)])))
    (is (= (wood {:count 4 :species "oak"} []) (wood {} [])) "one tree: its four logs")))

(deftest a-harvest-with-no-tree-in-sight-is-worth-a-typical-tree-not-inf
  (let [none (tu/seeing-all (tu/fake-on-floor {}))
        w (worth 'jobs.forestry.harvest-wood none {:radius 16})]
    (is (priced? w))
    (is (< (worth 'jobs.forestry.harvest-wood none {:radius 16 :species "oak"}) ##Inf))))

(deftest a-root-mine-is-worth-the-items-it-digs
  (let [mine #(worth 'jobs.gather.mine (tu/fake {}) %)]
    (is (priced? (mine {:item "coal" :count 2})))
    (is (= (* 2 (mine {:item "coal" :count 1})) (mine {:item "coal" :count 2})))
    (is (= (mine {:item "coal" :count 1}) (worth 'jobs.gather.mine (tu/fake {:inventory [(item "coal" 1)]}) {:item "coal" :count 2})) "what is carried is got already")
    (is (priced? (mine {:item ["coal" "charcoal"] :count 2})) "a list of items")
    (is (= (mine {:item ["coal" "charcoal"] :count 2}) (min (mine {:item "coal" :count 2}) (mine {:item "charcoal" :count 2}))) "any counts toward :count: priced once, not per item")
    (is (= (mine {:item "cobblestone" :count 3}) (mine {:block "stone" :count 3})) "a block is worth what it drops")))

(deftest set-spawn-is-worth-the-respawn-at-its-bed
  (let [p (tu/seeing-all (tu/fake-on-floor {:blocks {"6,64,0" "red_bed"}}))]
    (is (priced? (worth 'jobs.survival.set-spawn p {:bed {:x 6 :y 64 :z 0}})))
    (is (priced? (worth 'jobs.survival.set-spawn (tu/fake-on-floor {}) {})) "no bed: a bed where the body stands")))

(deftest a-harvest-is-worth-its-ripe-crops-and-their-replanting
  (let [seen (tu/seeing-all (tu/fake-on-floor ht/wheat-world))
        harvest #(worth 'jobs.farm.harvest %1 (ht/plain-args %2))]
    (is (= (* 9 (cost/crop-worth "wheat")) (harvest seen {:replant false})) "nine ripe wheat")
    (is (< (harvest seen {:replant false}) (harvest seen {})) "replanting adds the sown cells")
    (is (priced? (harvest (tu/fake-on-floor ht/wheat-world) {})) "nothing seen yet: one cell at the measured prior, a look first")
    (is (< (harvest (tu/fake-on-floor ht/wheat-world) {}) (harvest seen {})) "less than nine ripe wheat")))

(deftest jobs-that-pick-up-their-own-drops-are-worth-what-they-gain-not-inf
  (let [gv #(:value (cost/goal-value %))
        seen #(tu/seeing-all (tu/fake-on-floor %))
        sheep (fn [id x] {:id id :uuid (str "u" id) :name "sheep" :kind "passive" :pos {:x x :y 64 :z 0}})
        cow (fn [id x more] (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))]
    (is (= (gv [(item "white_wool" (* 2 animals/shear-wool))]) (worth 'jobs.animals.shear (seen {:entities [(sheep 1 3) (sheep 2 4)]}) {:radius 16})))
    (is (= (gv [(item "white_wool" animals/shear-wool)]) (worth 'jobs.animals.shear (seen {:entities [(sheep 1 3) (sheep 2 4)]}) {:radius 16 :count 1})) "at most :count")
    (is (= (gv [(item "bone_meal" 2)]) (worth 'jobs.farm.compost (tu/fake {}) {:times 2})))
    (is (= (gv [(item "wheat_seeds" 5)]) (worth 'jobs.gather.get-seeds (tu/fake {:inventory [(item "wheat_seeds" 3)]}) {:item "wheat_seeds" :count 8})) "the seeds still wanted")
    (is (= (gv [(item "bread" 3)]) (worth 'jobs.items.give (tu/fake {:inventory [(item "bread" 3)]}) {:to "Alex" :item "bread"})) "no :count: all of it carried")
    (is (priced? (worth 'jobs.items.give (tu/fake {}) {:to "11111111-2222-3333-4444-555555555555" :count 2})) "a villager's food not carried: what it fetches")
    (is (= (gv [(item "oak_boat" 1)]) (worth 'jobs.movement.boat-recover (seen {:entities [{:id 7 :name "oak_boat" :kind "other" :pos {:x 3 :y 64 :z 0.5}}]}) {})))
    (is (= (gv [(item "chest_minecart" 1)]) (worth 'jobs.strip.loot-cart (tu/fake {}) {})))
    (is (= (gv [(item "lead" 2)]) (worth 'jobs.animals.unleash (seen {:entities [(cow 1 3 {:leashed true :leashedToMe true}) (cow 2 4 {:leashed true :leashedToMe true}) (cow 3 5 {})]}) {:radius 8}))
        "a lead back per animal on one")
    (is (= (* 2 (cost/animal-worth "cow")) (worth 'jobs.animals.herd (tu/seeing-all (tu/fake (herd/world {:entities [(herd/cow 1 13 3)]}))) {:mob "cow" :box herd/box :target 3 :radius 24}))
        "a herd: the adults the pen lacks")))

(deftest apiary-harvest-and-guard-are-worth-a-hive-yield-each-as-the-apiary-pass-prices-them
  (let [yield (:value (cost/goal-value [(item "honeycomb" 3)]))
        carpeted (tu/seeing-all (tu/fake-on-floor (assoc-in (am/world {:inventory (am/inv "shears" 1)}) [:blocks "2,65,0"] "white_carpet")))
        open (tu/seeing-all (tu/fake-on-floor (am/world {:inventory (am/inv "shears" 1 "white_carpet" 1)})))]
    (is (= yield (worth 'jobs.apiary.harvest carpeted {:box am/box :with :either :max 8})) "one ripe hive")
    (is (= yield (worth 'jobs.apiary.guard open {:box am/box})) "one open fire")
    (is (zero? (worth 'jobs.apiary.guard carpeted {:box am/box})) "a carpeted fire needs no guard")))

(deftest apiary-harvest-and-guard-of-an-area-never-seen-are-worth-one-hive-at-the-prior-ripe-share
  (let [prior (* (cost/field-share :ripe 0 0) (:value (cost/goal-value [(item "honeycomb" 3)])))
        unseen #(doto (tu/blind (tu/fake-on-floor (am/world {:inventory (am/inv "shears" 1)}))) (aset "sensedAt" (fn [_] #js {:unknown true})))]
    (is (pos? prior))
    (is (= prior (worth 'jobs.apiary.harvest (unseen) {:box am/box :with :either})) "no ripe hive known yet")
    (is (= prior (worth 'jobs.apiary.guard (unseen) {:box am/box})) "no fire known yet")))

(def field {:min {:x 2 :y 63 :z 2} :max {:x 4 :y 64 :z 4}})

(deftest a-plant-is-worth-its-bare-cells-more-on-a-watered-field
  (let [bare (into {} (for [x (range 2 5) z (range 2 5)] [(str x ",63," z) "farmland"]))
        seeds [(item "wheat_seeds" 9)]
        plant #(worth 'jobs.farm.plant (tu/seeing-all (tu/fake-on-floor {:blocks (merge bare %) :inventory seeds}))
                      {:area {:min {:x 2 :y 63 :z 2} :max {:x 4 :y 63 :z 4}} :seed "wheat_seeds"})
        dry (plant {})]
    (is (priced? dry))
    (is (> (plant {"3,63,5" "water"}) dry) "water beside the field: it grows faster")))

(deftest a-farm-pass-is-worth-its-tend-and-the-stock-it-stores
  (let [p (tu/seeing-all (tu/fake-on-floor (assoc ht/wheat-world :inventory [(item "wheat" 30)])))
        args {:area field :till false :irrigate false :fertilize false :carry {}}]
    (is (priced? (worth 'jobs.farm.maintain p args)))
    (is (= (+ (worth 'jobs.farm.maintain p args) (:value (cost/goal-value [(item "wheat" 28)])))
           (worth 'jobs.farm.maintain p (assoc args :chest {:x 8 :y 64 :z 0}))) "the wheat above the 2 kept as bait, stored at its goal-value")))

(deftest a-farm-pass-prices-its-steps-beyond-tend-not-inf
  (let [args {:area field :till false :irrigate false :fertilize false :carry {}}
        farm #(worth 'jobs.farm.maintain %1 (merge args %2))
        grown (tu/seeing-all (tu/fake-on-floor (assoc ht/wheat-world :ages (ht/ages 3 (range 2 5) (range 2 5)))))
        meal (tu/seeing-all (tu/fake-on-floor (assoc ht/wheat-world :ages (ht/ages 3 (range 2 5) (range 2 5)) :inventory [(item "bone_meal" 4)])))
        base (farm grown {})
        c (ctx-of grown (merge args {:irrigate true}) {})]
    (with-redefs [tend/check-facts (let [real tend/check-facts] #(assoc (real %) :dry 5))]
      (is (= (:worth (tend/pass-price c (tend/check-facts c))) (farm grown {:irrigate true})) "irrigate: the pass price already sows wet, no second gain")
      (is (= [] (:unpriced (tend/pass-price c (tend/check-facts c)))) "irrigate is priced, so tend's walk check runs"))
    (let [wet (crops/stages-per-s true nil)]
      (is (< (Math/abs (- (* 9 (/ farm-maintain/bone-meal-stages wet) (/ (cost/sown-worth "wheat" wet) (cost-farm/horizon-s)))
                          (- (farm meal {:fertilize true}) (farm meal {}))))
             1e-6) "fertilize: nine unripe wheat cells, each the stages a bone meal gives over the stage rate, at the sown-worth share of the horizon"))
    (is (priced? (farm meal {:fertilize true})))
    (let [seeds (tu/seeing-all (tu/fake-on-floor (assoc ht/wheat-world :inventory [(item "melon_seeds" 100)])))]
      (is (< (Math/abs (- (:value (cost/goal-value [(item "bone_meal" (* 36 (/ farm-maintain/seed-compost-chance farm-maintain/compost-levels)))]))
                          (- (farm seeds {:composter {:x 8 :y 64 :z 0}}) (farm seeds {}))))
             1e-6) "compost: the bone meal the seeds above the 64 kept make"))
    (let [stray (tu/seeing-all (tu/fake-on-floor ft/tidy-spec))
          c (ctx-of stray (assoc args :area {:plan "field"}) {:engine {:world (world/of-data {"field" ft/field-plan} {} []) :warned (atom #{})}})]
      (is (= (* 3 (cost/sown-worth "wheat" (crops/stages-per-s false nil))) (farm-maintain/tidy-worth c))
          "tidy: the weed, cobblestone and sapling in crop cells (blocked), each sown after they are cleared"))))

(deftest an-apiary-pass-is-worth-the-honeycomb-of-its-ripe-hive-and-the-produce-it-stores
  (let [args {:box am/box :with :either :carry {} :ways #{:carried}}
        apiary #(worth 'jobs.apiary.maintain % args)
        seen #(tu/seeing-all (tu/fake-on-floor (assoc-in (am/world %) [:blocks "2,65,0"] "white_carpet")))
        p (seen {:inventory (am/inv "shears" 1)})]
    (is (= (:value (cost/goal-value [(item "honeycomb" 3)])) (apiary p)) "a carpeted fire, a ripe hive")
    (is (zero? (apiary (seen {:inventory (am/inv "shears" 1) :level 0}))) "no hive ripe")
    (is (= (:value (cost/goal-value [(item "honeycomb" 7)]))
           (worth 'jobs.apiary.maintain (seen {:inventory (am/inv "shears" 1 "honeycomb" 4)}) (assoc args :chest am/chest))) "the hive's 3 combs and the 4 carried, stored")
    (is (= (* (cost/field-share :ripe 0 0) (:value (cost/goal-value [(item "honeycomb" 3)])))
           (apiary (doto (tu/blind (tu/fake-on-floor (am/world {:inventory (am/inv "shears" 1)}))) (aset "sensedAt" (fn [_] #js {:unknown true}))))) "an area never seen: one hive at the prior ripe share")))

(deftest an-apiary-pass-prices-guard-and-breed-at-their-settings
  (let [args {:box am/box :with :either :carry {} :ways #{:carried} :target 3}
        yield (:value (cost/goal-value [(item "honeycomb" 3)]))
        apiary #(worth 'jobs.apiary.maintain (tu/seeing-all (tu/fake-on-floor (am/world %))) args)
        flowers (am/inv "shears" 1 "dandelion" 2)
        bees [(am/bee 1 1) (am/bee 2 2)]]
    (is (zero? (apiary {:inventory (am/inv "shears" 1) :level 0 :raised? true :entities bees})) "no bait: no breeding")
    (is (= yield (apiary {:inventory flowers :level 0 :raised? true :entities bees})) "breed: a hive's yield")
    (is (= yield (apiary {:inventory (am/inv "shears" 1 "white_carpet" 1) :level 5})) "guard: an open fire smoking a ripe hive, one hive's yield")))

(def forest {:id "forest" :parts [{:id "a" :cells [[3 64 0]] :want {:tree "oak"}}]})

(deftest a-forest-pass-is-worth-the-logs-of-its-grown-trees-and-its-planting
  (let [forestry #(worth 'jobs.forestry.maintain (tu/seeing-all (tu/fake-on-floor %)) {:plan "forest" :accept #{:fluid-adjacent}}
                         {:engine {:world (world/of-data {"forest" forest} {} []) :warned (atom #{})}})]
    (is (= (:value (cost/goal-value [(item "oak_log" 4)])) (forestry oak-world)))
    (is (= (:value (cost/goal-value [(item "oak_log" (forestry/logs-per-tree))]))
           (forestry {:blocks {"3,63,0" "dirt"} :inventory [(item "oak_sapling" 1)]})) "a sapling to plant: the mean grown tree")))

(deftest a-forest-pass-with-a-chest-is-worth-the-logs-it-stores
  (let [forestry (fn [args] (worth 'jobs.forestry.maintain (tu/seeing-all (tu/fake-on-floor (update oak-world :inventory conj (item "oak_log" 20))))
                                   (merge {:plan "forest" :accept #{:fluid-adjacent}} args)
                                   {:engine {:world (world/of-data {"forest" forest} {} []) :warned (atom #{})}}))
        chest {:chest {:x 10 :y 64 :z 0}}]
    (is (< (Math/abs (- (:value (cost/goal-value [(item "oak_log" 20)])) (- (forestry chest) (forestry {})))) 1e-6) "the carried logs a store would put away")
    (is (< (Math/abs (- (:value (cost/goal-value [(item "oak_log" 5)])) (- (forestry (assoc chest :carry {"oak_log" 15})) (forestry {})))) 1e-6) "less the logs the carry keeps")))

(deftest a-forest-pass-prices-readying-and-unseen-cells
  (let [forestry #(worth 'jobs.forestry.maintain % {:plan "forest" :accept #{:fluid-adjacent}} {:engine {:world (world/of-data {"forest" forest} {} []) :warned (atom #{})}})
        tree (:value (cost/goal-value [(item "oak_log" (forestry/logs-per-tree))]))]
    (is (= tree (forestry (tu/seeing-all (tu/fake-on-floor {:blocks {"3,63,0" "stone"}})))) "a cell with no ground is readied: a planted tree")
    (is (= (* (cost/field-share :ripe 0 0) tree) (forestry (doto (tu/blind (tu/fake-on-floor {:blocks {"3,63,0" "dirt"}})) (aset "sensedAt" (fn [_] #js {:unknown true}))))) "a cell never seen: one tree at the prior ripe share")))

(deftest the-night-is-worth-its-exposure-and-the-time-a-sleep-that-skips-it-saves
  (let [night (fn [status] (worth 'jobs.survival.night (tu/seeing-all (tu/fake-on-floor {:time 14000})) {}
                                  {:view (fn [] {:now 100000 :data {:entries {:sleep-status [{:t 99000 :data status}]}}})}))
        alone (night {:sleeping 0 :needed 1})
        short (night {:sleeping 0 :needed 4})
        left (second (get-in (sh-clock/night-left-quote {:primitives (tu/fake {:time 14000})}) [:uses :job-time]))]
    (is (pos? left))
    (is (< (Math/abs (- left (- alone short))) 1e-6) "alone the sleep skips the night: its seconds are worth saving; three short it skips nothing")))

(deftest the-night-is-worth-only-the-exposure-it-adds-to-the-day
  (let [c (ctx-of (tu/fake-on-floor {:time 14000}) {} {})
        stand (bed-down/stand-cells (:primitives c))
        night (bed-down/open-price c stand 600)
        gain (bed-down/open-gain c stand 600)
        day (bed-down/exposed-price c (spawn-light/chance-at (:primitives c) (first stand) false 0 true) 600 (first stand))]
    (is (pos? day) "an unseen cell is dark by day by the chance it is")
    (is (< (Math/abs (- gain (- night day))) 1e-6) "the night's exposure less the same span by day")
    (is (< gain night))))

(deftest the-open-night-is-priced-at-the-worst-of-the-cells-the-body-may-stand-in
  (let [c (ctx-of (tu/fake-on-floor {:time 14000}) {} {})
        cells [{:x 0 :y 64 :z 0} {:x 1 :y 64 :z 0} {:x 2 :y 64 :z 0} {:x 3 :y 64 :z 0}]
        chance #(case (:x %2) 1 0.5 0)
        price (fn [cs] (with-redefs [spawn-light/night-chance chance] (bed-down/open-price c cs 600)))
        at (fn [cell] (bed-down/exposed-price c (chance nil cell) 600 cell))]
    (is (pos? (at (second cells))))
    (is (= (at (second cells)) (price cells)) "one dangerous cell among four safe ones sets the price: a mean over the cells would give a quarter of it, a first or last cell rule none")
    (is (= (price [(second cells)]) (price cells)))))

(deftest the-night-is-worth-the-skip-the-other-players-online-wait-for-while-the-body-stays-awake
  (let [night #(worth 'jobs.survival.night (tu/seeing-all (tu/fake-on-floor (merge {:time 14000} %))) {})
        left (second (get-in (sh-clock/night-left-quote {:primitives (tu/fake {:time 14000})}) [:uses :job-time]))
        asleep (assoc sleeper :name "Alex")]
    (is (< (Math/abs (- left (- (night {}) (night {:players ["Alex"]})))) 1e-6) "one other awake and not known to lie down: its own sleep skips nothing (the worth lost is the night left) and nobody waits for it")
    (is (< (Math/abs (- left (- (night {:players ["Alex"] :entities [asleep]}) (night {})))) 1e-6) "the one other asleep: it waits out the night left for the body")))

(deftest an-exposed-night-prices-its-hurt-and-death-by-cost-risk-s
  (let [p (tu/seeing-all (tu/fake-on-floor {:self {:health 5}}))
        c (ctx-of p {} {})
        at (zipmap [:x :y :z] (u/self-pos c))
        {:keys [hurt dies death]} (price/stay-parts c 0.01 600)]
    (is (pos? (* dies death)))
    (is (< (Math/abs (- (cost/risk-s 5 hurt (cost/lethal-hp {:chance dies :death death})) (bed-down/exposed-price c 0.01 600 at))) 1e-6)
        "the one risk-s formula: hurt at hp-cost, the death at hp-seconds")))

(deftest a-reflex-is-worth-the-loss-it-averts-never-its-own-quote
  (let [fake (tu/fake-on-floor {})
        plain (fn [f] (f))
        two-open (fn [f] (with-redefs [shut/ops (constantly {:targets (fn [& _] [{:cell [1 64 0]} {:cell [2 64 0]}])})] (f)))
        owed (fn [f] (with-redefs [tidy/entries (constantly [{} {} {}])] (f)))
        debt (fn [t] (fn [f] (with-redefs [plant-sapling/current-target (constantly t)] (f))))
        full (vec (for [i (range 34)] (item (str "stone_" i) 1)))]
    (doseq [[label expected job p args around]
            [["rest: the hp lacked" (cost/hp-cost 8) 'jobs.survival.rest (tu/fake {:self {:health 12}}) {} plain]
             ["leave-vehicle" cost/riding-s 'jobs.movement.leave-vehicle fake {} plain]
             ["make-room: each slot short" (* 2 cost/short-slot-s) 'jobs.storage.make-room (tu/fake {:inventory full}) {:free 4} plain]
             ["restore-broken: each cell owed" (* 3 cost/left-cell-s) 'jobs.survival.restore-broken fake {} owed]
             ["shut: each door open" (* 2 cost/open-door-s) 'jobs.maintenance.shut fake {} two-open]
             ["shut: each gate open" (* 2 cost/open-gate-s) 'jobs.maintenance.shut fake {:openings :gates} two-open]
             ["plant-sapling: a grown tree" (cost/tree-worth "birch" (forestry/logs-per-tree)) 'jobs.forestry.plant-sapling fake {}
              (debt {:species "birch" :pos {:x 9 :y 64 :z 0}})]
             ["plant-sapling: no debt" 0 'jobs.forestry.plant-sapling fake {} (debt nil)]
             ["fell-tree: the trunk's logs" (cost/tree-worth "oak" 4) 'jobs.forestry.fell-tree (tu/seeing-all (tu/fake-on-floor oak-world)) {:radius 16} plain]
             ["pick-ores: the seen ores' drops" (:value (cost/goal-value [(item "raw_iron" 2)])) 'jobs.gather.pick-ores
              (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [(item "iron_pickaxe" 1)]
                                       :blocks (merge (tu/box -4 58 -4 4 63 4 "stone") {"2,63,0" "iron_ore" "-2,63,0" "deepslate_iron_ore"})}))
              {} plain]]]
      (around #(is (= expected (worth job p args)) label))))
  (let [p (tu/fake-on-floor {:time 18000})
        c (ctx-of p {} {})
        chance (spawn-light/chance-at p (update-vals (u/self-pos c) js/Math.floor) false)]
    (is (pos? chance))
    (is (= (price/stay-risk-s c chance (risk/work-s :work)) (worth 'jobs.build.light-area p {:near 3}))
        "light-area: a job's work in the dark where it stands")))

(deftest a-reflex-is-worth-what-doing-nothing-loses-not-a-whole-death
  (let [death (fn [p] (cost/death-s p (ctx/view (ctx-of p {} {}))))
        water {"0,64,0" "water" "0,65,0" "water" "0,66,0" "water" "0,67,0" "water"}
        drowning (tu/fake {:self {:inWater true :oxygen 4} :blocks water})
        enclosed (tu/fake-on-floor {:blocks {"0,65,0" "stone"}})
        lava (tu/fake-on-floor {:self {:inLava true :onFire true}})
        lava-absorbing (tu/fake-on-floor {:self {:inLava true :onFire true :absorption 4}})
        low (tu/fake-on-floor {:self {:onFire true :health 10}})
        dusk (tu/fake-on-floor {:time 12000})
        lit-tunnel (tu/fake-on-floor {:time 18000 :light-default [0 15]})]
    (doseq [[label job p expected]
            [["breathe drowning: a sure death, the swim up keeps a death's worth" 'jobs.survival.breathe drowning (death drowning)]
             ["breathe enclosed: a sure death" 'jobs.survival.breathe enclosed (death enclosed)]
             ["breathe afloat, head out: nothing hurts" 'jobs.survival.breathe (tu/fake {:self {:inWater true :oxygen 5} :blocks {"0,64,0" "water"}}) 0]
             ["extinguish in lava: a sure death" 'jobs.survival.extinguish lava (death lava)]
             ["extinguish in lava with absorption: still a death, absorption only delays it" 'jobs.survival.extinguish lava-absorbing (death lava-absorbing)]
             ["extinguish alight at full health: the burn's hp" 'jobs.survival.extinguish (tu/fake-on-floor {:self {:onFire true}}) (cost/hp-cost cost/burn-hp)]
             ["extinguish alight with absorption: it takes the first hp, as drowning's" 'jobs.survival.extinguish
              (tu/fake-on-floor {:self {:onFire true :absorption 4}}) (cost/hp-cost (- cost/burn-hp 4))]
             ["extinguish alight at 10 hp: the burn kills" 'jobs.survival.extinguish low (death low)]
             ["unwedge: held still, no hurt" 'jobs.survival.unwedge (tu/fake-on-floor {:blocks {"0,64,0" "sand"}}) cost/wedged-s]]]
      (is (= expected (worth job p {})) label))
    (is (< 0 (worth 'jobs.survival.go-home dusk {:commute? true}) (death dusk)) "dusk-home in the open: a night's stay here, not a death")
    (is (= 1000 (with-redefs [price/stay-risk-s (fn ([_ _ _] 1000) ([_ _ _ _] 1000))] (worth 'jobs.survival.go-home dusk {:commute? true})))
        "dusk-home: the night's stay risk, not capped at what sheltering here would cost")
    (is (pos? (worth 'jobs.survival.go-home lit-tunnel {:limit 1000})) "stuck-underground in a lit tunnel: still stuck, never nothing")))
