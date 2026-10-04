(ns engine.fake.use-on-test
  "The fake world's useOn over cljs world data; the cases of js/fake-use-on.test.mjs plus the lead tie.
  A world is {:blocks :states :ages :inventory :entities :self {:pos :held}}, a result {:status :before :after :consumed}."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.fake.use-on :as use-on]))

(def p [1 64 0])

(defn owned
  [{:keys [blocks states ages inventory held entities]}]
  {:blocks (or blocks {}) :states (or states {}) :ages (or ages {}) :inventory (vec inventory) :entities (vec entities)
   :next-entity-id 1 :self {:pos [0 64 0] :held held}})

(defn click [w args] (use-on/use-on w (merge {:pos p} args)))
(defn result [w args] (second (click w args)))
(defn stack [name count] {:name name :count count})
(defn drops [w] (mapv (fn [e] [(get-in e [:item :name]) (get-in e [:item :count]) (:pos e)]) (filter #(= "item" (:kind %)) (:entities w))))
(defn plain [name] {:name name :properties {}})
(defn counts [w] (mapv (juxt :name :count) (:inventory w)))

(deftest missing-carries-only-the-status
  (is (= {:status "missing"} (result (owned {}) {}))))

(deftest an-item-not-carried-is-no-item
  (is (= {:status "no-item" :before (plain "dirt") :after (plain "dirt") :consumed 0}
         (result (owned {:blocks {p "dirt"}}) {:item "bone_meal"}))))

(deftest a-block-out-of-reach-is-unreachable
  (let [w (owned {:blocks {[9 64 0] "dirt"} :inventory [(stack "iron_hoe" 1)]})
        [w' r] (click w {:pos [9 64 0] :item "iron_hoe"})]
    (is (= ["unreachable" 0 "too-far" 9.53] ((juxt :status :consumed :reason :distance) r)))
    (is (= "dirt" (get-in w' [:blocks [9 64 0]])))))

(deftest the-item-becomes-held
  (let [w (owned {:blocks {p "stone"} :inventory [(stack "bone_meal" 1)] :held "bread"})
        [w1] (click w {:item "bone_meal"})
        [w2] (click w1 {})]
    (is (= "bone_meal" (get-in w1 [:self :held])))
    (is (nil? (get-in w2 [:self :held])))))

(deftest hoe-makes-farmland
  (doseq [[hoe ground] [["wooden_hoe" "dirt"] ["iron_hoe" "grass_block"] ["diamond_hoe" "dirt_path"]]]
    (testing (str hoe " on " ground)
      (let [[w r] (click (owned {:blocks {p ground} :inventory [(stack hoe 1)]}) {:item hoe})]
        (is (= {:status "used" :before (plain ground) :after (plain "farmland") :consumed 0} r))
        (is (= "farmland" (get-in w [:blocks p])))))))

(deftest hoe-leaves-others-unchanged
  (doseq [[label blocks face] [["stone" {p "stone"} "up"]
                               ["dirt with a block above" {p "dirt" [1 65 0] "stone"} "up"]
                               ["dirt from below" {p "dirt"} "down"]]]
    (testing label
      (let [[w r] (click (owned {:blocks blocks :inventory [(stack "iron_hoe" 1)]}) {:item "iron_hoe" :face face})]
        (is (= ["unchanged" 0] ((juxt :status :consumed) r)))
        (is (= blocks (:blocks w)))))))

(deftest bone-meal-on-crops
  (doseq [[crop age status after consumed left] [["wheat" 0 "used" 2 1 2] ["wheat" 6 "used" 7 1 2] ["carrots" 5 "used" 7 1 2]
                                                 ["potatoes" 7 "unchanged" 7 0 3] ["beetroots" 0 "used" 2 1 2]
                                                 ["beetroots" 2 "used" 3 1 2] ["beetroots" 3 "unchanged" 3 0 3]]]
    (testing (str crop " age " age)
      (let [[w r] (click (owned {:blocks {p crop} :ages {p age} :inventory [(stack "bone_meal" 3)]}) {:item "bone_meal"})]
        (is (= [status age after consumed]
               [(:status r) (get-in r [:before :properties :age]) (get-in r [:after :properties :age]) (:consumed r)]))
        (is (= after (get-in w [:ages p])))
        (is (= left (get-in w [:inventory 0 :count])))))))

(deftest bone-meal-on-saplings-and-grass
  (doseq [block ["oak_sapling" "birch_sapling" "grass_block"]]
    (testing block
      (let [[w r] (click (owned {:blocks {p block} :inventory [(stack "bone_meal" 1)]}) {:item "bone_meal"})]
        (is (= ["used" 1] ((juxt :status :consumed) r)))
        (is (= block (get-in w [:blocks p])))
        (is (= [] (:inventory w)))))))

(deftest emptied-composter-spits-bone-meal
  (let [[w r] (click (owned {:blocks {p "composter"} :states {p {:level 8}}}) {})]
    (is (= {:status "used" :before {:name "composter" :properties {:level 8}} :after {:name "composter" :properties {:level 0}} :consumed 0} r))
    (is (= [["bone_meal" 1 [1 65 0]]] (drops w)))))

(deftest full-composter-empties-for-a-held-item-too
  (let [[w r] (click (owned {:blocks {p "composter"} :states {p {:level 8}} :inventory [(stack "wheat" 2)]}) {:item "wheat"})]
    (is (= ["used" 0] ((juxt :status :consumed) r)))
    (is (= 1 (count (drops w))))
    (is (= [["wheat" 2]] (counts w)))))

(deftest composting-raises-the-level
  (doseq [[level states after] [[nil {} 1] [0 {p {:level 0}} 1] [3 {p {:level 3}} 4] [6 {p {:level 6}} 8]]]
    (testing (str "level " level)
      (let [[w r] (click (owned {:blocks {p "composter"} :states states :inventory [(stack "wheat_seeds" 2)]}) {:item "wheat_seeds"})]
        (is (= ["used" 1 after] [(:status r) (:consumed r) (get-in r [:after :properties :level])]))
        (is (= [["wheat_seeds" 1]] (counts w)))
        (is (= [] (drops w)))))))

(deftest composter-unchanged-cases
  (doseq [[label level args] [["level 7 with seeds" 7 {:item "wheat_seeds"}] ["a non-compostable item" 2 {:item "stick"}] ["an empty hand" 2 {}]]]
    (testing label
      (let [[w r] (click (owned {:blocks {p "composter"} :states {p {:level level}} :inventory [(stack "wheat_seeds" 1) (stack "stick" 1)]}) args)]
        (is (= ["unchanged" 0 level] [(:status r) (:consumed r) (get-in r [:after :properties :level])]))
        (is (= 2 (count (:inventory w))))))))

(deftest anything-else-is-unchanged
  (is (= {:status "unchanged" :before (plain "stone") :after (plain "stone") :consumed 0}
         (result (owned {:blocks {p "stone"} :inventory [(stack "apple" 1)]}) {:item "apple"}))))

(deftest guards
  (doseq [[label block item reason] [["a bed" "white_bed" "iron_hoe" "bed"] ["a respawn anchor" "respawn_anchor" "iron_hoe" "bed"]
                                     ["a chest" "chest" "iron_hoe" "container"] ["a crafting table" "crafting_table" "iron_hoe" "container"]
                                     ["a barrel" "barrel" "iron_hoe" "container"] ["a crafter" "crafter" "iron_hoe" "container"]
                                     ["a command_block" "command_block" "iron_hoe" "container"]
                                     ["a chain_command_block" "chain_command_block" "iron_hoe" "container"]
                                     ["a repeating_command_block" "repeating_command_block" "iron_hoe" "container"]
                                     ["a structure_block" "structure_block" "iron_hoe" "container"] ["a jigsaw" "jigsaw" "iron_hoe" "container"]
                                     ["a vault" "vault" "iron_hoe" "container"]
                                     ["flint_and_steel" "dirt" "flint_and_steel" "hazard"] ["fire_charge" "dirt" "fire_charge" "hazard"]
                                     ["lava_bucket" "dirt" "lava_bucket" "hazard"]
                                     ["a block item" "dirt" "cobblestone" "use-place"] ["oak_planks" "stone" "oak_planks" "use-place"]]]
    (testing label
      (let [[w r] (click (owned {:blocks {p block} :inventory [(stack item 2)]}) {:item item})]
        (is (= {:status "cannot" :reason reason :before (plain block) :after (plain block) :consumed 0} r))
        (is (= block (get-in w [:blocks p])))
        (is (= [[item 2]] (counts w)))))))

(deftest a-composter-still-accepts-oak-leaves
  (let [r (result (owned {:blocks {p "composter"} :inventory [(stack "oak_leaves" 2)]}) {:item "oak_leaves"})]
    (is (= ["used" 1] ((juxt :status :consumed) r)))))

(defn hive [level] {:blocks {p "beehive"} :states {p {:honey_level level}}})

(deftest shears-on-a-ripe-hive
  (let [[w r] (click (owned (assoc (hive 5) :inventory [(stack "shears" 1)])) {:item "shears"})]
    (is (= {:status "used" :before {:name "beehive" :properties {:honey_level 5}} :after {:name "beehive" :properties {:honey_level 0}} :consumed 0} r))
    (is (= [["honeycomb" 3 [1 65 0]]] (drops w)))))

(deftest glass-bottle-on-a-ripe-bee-nest
  (let [[w r] (click (owned {:blocks {p "bee_nest"} :states {p {:honey_level 5}} :inventory [(stack "glass_bottle" 2)]}) {:item "glass_bottle"})]
    (is (= ["used" 1 0] [(:status r) (:consumed r) (get-in r [:after :properties :honey_level])]))
    (is (= [["glass_bottle" 1] ["honey_bottle" 1]] (counts w)))))

(deftest hive-unchanged-cases
  (doseq [[label spec item] [["an unripe hive" (hive 4) "shears"] ["a hive with the wrong item" (hive 5) "stick"]]]
    (testing label
      (let [[w r] (click (owned (assoc spec :inventory [(stack item 1)])) {:item item})]
        (is (= "unchanged" (:status r)))
        (is (= [] (drops w)))))))

(deftest hand-flips-gates-doors-and-trapdoors
  (doseq [name ["oak_fence_gate" "oak_door" "spruce_trapdoor" "copper_door" "bamboo_fence_gate"]]
    (testing name
      (let [[w1 opened] (click (owned {:blocks {p name}}) {})
            [_ shut] (click w1 {})]
        (is (= "used" (:status opened)))
        (is (= {:name name :properties {:open true}} (:after opened)))
        (is (= {:name name :properties {:open false}} (:after shut)))))))

(deftest iron-doors-ignore-a-hand
  (doseq [name ["iron_door" "iron_trapdoor"]]
    (is (= "unchanged" (:status (result (owned {:blocks {p name}}) {}))))))

(deftest a-lever-flips-powered
  (let [[w1 r1] (click (owned {:blocks {p "lever"}}) {})
        [_ r2] (click w1 {})]
    (is (= {:name "lever" :properties {:powered true}} (:after r1)))
    (is (= {:name "lever" :properties {:powered false}} (:after r2)))))

(deftest a-button-is-pressed-once
  (let [[w1 r1] (click (owned {:blocks {p "stone_button"}}) {})
        [_ r2] (click w1 {})]
    (is (= ["used" {:name "stone_button" :properties {:powered true}}] [(:status r1) (:after r1)]))
    (is (= "unchanged" (:status r2)))))

(deftest a-locked-block-does-not-change
  (let [r (result (owned {:blocks {p "oak_door"} :states {p {:open false :locked true}}}) {})]
    (is (= ["unchanged" false] [(:status r) (get-in r [:after :properties :open])]))))

(deftest a-hand-click-flips-both-halves-of-a-door
  (let [door (owned {:blocks {[1 64 0] "oak_door" [1 65 0] "oak_door"}
                     :states {[1 64 0] {:half "lower" :open false} [1 65 0] {:half "upper" :open false}}})
        opens (fn [w] (mapv #(get-in w [:states % :open]) [[1 64 0] [1 65 0]]))]
    (is (= [[true true] [true true]] (mapv #(opens (first (click door {:pos [1 % 0]}))) [64 65])))))

(deftest a-fence-click-ties-the-led-animals
  (let [cow {:id 7 :name "cow" :kind "animal" :pos [3 64 0] :leashed-to-me true}
        [w r] (click (owned {:blocks {p "oak_fence"} :entities [cow]}) {:item nil})
        knot (first (filter #(= "leash_knot" (:name %)) (:entities w)))
        [_ again] (click w {})]
    (is (= "used" (:status r)))
    (is (= [p false (:id knot)] [(:pos knot) (:leashed-to-me (first (:entities w))) (:leash-holder (first (:entities w)))]))
    (is (= "unchanged" (:status again)))))

(deftest a-fence-click-with-nothing-led-is-unchanged
  (is (= "unchanged" (:status (result (owned {:blocks {p "oak_fence"}}) {})))))
