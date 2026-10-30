(ns engine.goal-worth-test
  "Goal jobs price their own goal at the root (their registry worth-of): what the goal yields, not ##Inf (arrive), and
  ##Inf only while a step would run that no price covers."
  (:require [cljs.test :refer [deftest is]]
            [engine.apiary-maintain-test :as am]
            [engine.farm-tidy-test :as ft]
            [engine.harvest-test :as ht]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.farm.maintain :as farm-maintain]
            [jobs.farm.tend :as tend]
            [jobs.forestry.maintain :as forest-maintain]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.farm :as cost-farm]
            [jobs.lib.crops :as crops]
            [jobs.lib.trees :as forestry]
            [jobs.lib.price :as price]
            [jobs.lib.spawn-light :as spawn-light]
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
    (is (= (* 4 (trade 1)) (trade 4)))
    (is (= (* 2 (breed 1)) (breed 2)) "a calf a pair")
    (is (priced? (worth 'jobs.animals.bring (tu/fake {}) {:mob "cow"})))
    (is (= (worth 'jobs.combat.hunt (tu/fake {}) {:item "beef" :count 1})
           (worth 'jobs.combat.hunt (tu/fake {:inventory [(item "beef" 1)]}) {:item "beef" :count 2})) "a hunt: the items still to carry")))

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
      (is (< (Math/abs (- (* 36 (/ farm-maintain/seed-compost-chance farm-maintain/compost-levels) (:value (cost/goal-value [(item "bone_meal" 1)])))
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

(deftest the-night-is-worth-the-skip-the-other-players-online-wait-for-while-the-body-stays-awake
  (let [night #(worth 'jobs.survival.night (tu/seeing-all (tu/fake-on-floor {:time 14000 :players %})) {})
        left (second (get-in (sh-clock/night-left-quote {:primitives (tu/fake {:time 14000})}) [:uses :job-time]))]
    (is (< (Math/abs (- (night []) (night ["Alex"]))) 1e-6) "alone its sleep skips the night; with one other awake, the other waits it out")
    (is (< (Math/abs (- (* 2 left) (- (night ["Alex" "Bo" "Cy"]) (night ["Alex"])))) 1e-6) "each other player waits out the night left")))

(deftest an-exposed-night-weighs-a-death-at-its-cost-not-scaled-by-health-again
  (let [p (tu/seeing-all (tu/fake-on-floor {:self {:health 5}}))
        c (ctx-of p {} {})
        at (zipmap [:x :y :z] (u/self-pos c))
        {:keys [hurt dies death]} (price/stay-parts c 0.01 600)]
    (is (pos? (* dies death)))
    (is (< (Math/abs (- (+ (* (cost/hp-price 5) hurt) (* dies death)) (bed-down/exposed-price c 0.01 600 at))) 1e-6)
        "hurt at hp-price, the death as chance x cost: the death scale already counts low health in its chance")))
