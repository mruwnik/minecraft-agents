(ns engine.jobs-value-test
  "jobs.lib.cost.value: item-value (built from minecraft-data) and fetch-cost, the two sides of a fetch-or-skip decision."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.cost.value :as v]))

(defn value [items & opts] (:value (apply v/item-value items opts)))
(defn each [name] (value [{:name name :count 1}]))

(def card-pile
  "The field report's death pile: 24 raw iron over two slots, 100 cobblestone, a stone pickaxe, 29 planks."
  [{:name "raw_iron" :count 4 :slot 17} {:name "raw_iron" :count 20 :slot 18}
   {:name "cobblestone" :count 64} {:name "cobblestone" :count 36}
   {:name "stone_pickaxe" :count 1} {:name "oak_planks" :count 29}])

(def short-fetch (v/fetch-cost {:distance 40 :danger 0 :elapsed-ms 5000}))

(deftest the-card-pile-is-worth-far-more-than-a-short-fetch
  (is (> (value card-pile) (* 10 (:cost short-fetch)))))

(deftest ores-and-metals-rank-by-rarity-per-item
  (is (> (each "diamond") (each "raw_iron") (each "cobblestone") 0))
  (is (> (each "diamond") (each "iron_ingot") (each "coal") (each "cobblestone")))
  (is (> (each "gold_ingot") (each "copper_ingot"))))

(deftest crafted-things-are-worth-their-ingredients
  (is (> (each "stone_pickaxe") (each "cobblestone")))
  (is (> (each "stone_pickaxe") (* 3 (each "cobblestone"))))
  (is (> (each "diamond_pickaxe") (each "iron_pickaxe") (each "wooden_pickaxe") (each "stone_pickaxe")) "wood costs a walk to a tree, cobble none")
  (is (> (each "diamond_pickaxe") (* 3 (each "diamond"))))
  (is (> (each "iron_ingot") (each "raw_iron")) "a smelted ingot is its raw ore plus the smelting")
  (is (< (each "oak_planks") (each "oak_log")) "a log makes four planks"))

(deftest a-stack-of-dirt-is-cheap
  (is (< (value [{:name "dirt" :count 64}]) (each "raw_iron")))
  (is (< (value [{:name "dirt" :count 64}]) (:cost (v/fetch-cost {:distance 100 :danger 0 :elapsed-ms 0}))))
  (is (< (value [{:name "dirt" :count 64}]) (each "iron_pickaxe"))))

(deftest a-rare-unstackable-is-dearer-than-a-common-stackable-from-no-known-source
  (is (> (each "totem_of_undying") (each "ender_pearl") (each "rotten_flesh"))))

(deftest value-is-monotonic-in-count
  (doseq [name ["dirt" "raw_iron" "diamond" "stone_pickaxe" "mystery_thing"]]
    (let [values (map #(value [{:name name :count %}]) [0 1 2 10 64])]
      (is (apply < values) name))))

(deftest stacks-add-up
  (is (= (value [{:name "raw_iron" :count 24}])
         (value [{:name "raw_iron" :count 4} {:name "raw_iron" :count 20}]))))

(deftest odd-counts-never-throw
  (are [items expected] (= expected (value items))
    [] 0
    nil 0
    [{:name "dirt" :count 0}] 0
    [{:name "dirt" :count -5}] 0
    [{:name "dirt" :count js/NaN}] 0
    [{:name nil :count 3}] 0
    [{:count 3}] 0)
  (is (= (each "dirt") (value [{:name "dirt"}])) "a missing count is one")
  (is (= (each "dirt") (value [{:name "dirt" :count nil}]))))

(deftest unknown-items-get-a-small-default
  (is (= (:unknown-each v/default-basis) (each "mystery_thing")))
  (is (= 7 (value [{:name "mystery_thing"}] :basis {:unknown-each 7})) "the unknown worth is in the basis")
  (is (pos? (each "mystery_thing")))
  (is (< (each "mystery_thing") (each "raw_iron"))))

(deftest worn-tools-are-worth-less-and-enchanted-ones-more
  (let [full (each "diamond_pickaxe")
        half (value [{:name "diamond_pickaxe" :count 1 :durability 780}])
        nearly-broken (value [{:name "diamond_pickaxe" :count 1 :durability 1}])
        enchanted (value [{:name "diamond_pickaxe" :count 1 :enchants [{:name "efficiency" :lvl 5}]}])
        mending (value [{:name "diamond_pickaxe" :count 1 :enchants [{:name "mending" :level 1}]}])]
    (is (< nearly-broken half full enchanted))
    (is (pos? nearly-broken))
    (is (> mending full) "a rare enchantment adds value")
    (is (= full (value [{:name "diamond_pickaxe" :count 1 :enchants []}])))))

(deftest name-overrides-set-the-value-and-flow-into-what-is-made-of-it
  (is (= 50 (value [{:name "raw_iron" :count 1}] :overrides {"raw_iron" 50})))
  (is (= 500 (value [{:name "raw_iron" :count 10}] :overrides {"raw_iron" 50})))
  (is (= 0 (value [{:name "iron_ingot" :count 5}] :overrides {"iron_ingot" 0})))
  (is (< (value [{:name "iron_pickaxe"}] :overrides {"iron_ingot" 0}) (each "stone_pickaxe"))
      "worthless iron makes an iron pickaxe worth its sticks")
  (is (= (* 2 (each "diamond")) (value [{:name "diamond"}] :overrides {"diamond" {:times 2}})))
  (is (= (each "dirt") (value [{:name "dirt"}] :overrides {"raw_iron" 50})) "other items keep their value"))

(deftest group-overrides-apply-to-every-item-of-the-group
  (is (= (* 3 (each "iron_pickaxe")) (value [{:name "iron_pickaxe"}] :overrides {"tool" {:times 3}})))
  (is (= 0 (value [{:name "diamond_chestplate"}] :overrides {"armor" 0})))
  (is (= 7 (value [{:name "raw_iron"}] :overrides {"ore" 7})))
  (is (= 9 (value [{:name "mystery_thing"}] :overrides {"unknown" 9})))
  (is (= 1 (value [{:name "raw_iron"}] :overrides {"ore" 7 "raw_iron" 1})) "a name override beats a group one"))

(deftest overrides-take-keywords-and-string-times
  (is (= 50 (value [{:name "raw_iron"}] :overrides {:raw_iron 50})))
  (is (= (* 2 (each "diamond")) (value [{:name "diamond"}] :overrides {"diamond" {"times" 2}}))))

(deftest the-breakdown-names-the-main-contributors-first
  (let [r (v/item-value card-pile)]
    (is (= "raw_iron" (:name (first (:items r)))))
    (is (= 24 (:count (first (:items r)))))
    (is (= (:value r) (reduce + (map :value (:items r)))))))

(deftest own-prices-replace-the-base-per-pattern-first-match-wins
  (let [prices [[#"^iron_" 5] [#"_ingot$" 2]]]
    (is (= 5 (value [{:name "iron_ingot"}] :prices prices)) "first matching pattern")
    (is (= 2 (value [{:name "gold_ingot"}] :prices prices)))
    (is (= 15 (value [{:name "iron_ingot" :count 3}] :prices prices)) "price per item, times the count")
    (is (= (each "dirt") (value [{:name "dirt"}] :prices prices)) "unmatched items keep the base")))

(deftest a-per-stack-row-prices-the-whole-stack
  (let [prices [[#"^iron_ingot$" 2] [#"^cobblestone$" 1 :per-stack]]]
    (is (= 6 (value [{:name "iron_ingot" :count 3}] :prices prices)))
    (is (= 1 (value [{:name "cobblestone" :count 64}] :prices prices)))
    (is (= 1 (value [{:name "cobblestone"}] :prices prices)) "a missing count is one")))

(deftest enchanted-stacks-take-the-enchanted-price-and-a-priced-item-ignores-durability
  (let [opts [:prices [[#"^diamond_pickaxe$" 7]] :else 0 :enchanted 25]]
    (is (= 25 (apply value [{:name "dirt" :count 5 :enchants [{:name "mending"}]}] opts)))
    (is (= 25 (apply value [{:name "dirt" :nbt {:x 1}}] opts)))
    (is (= 0 (apply value [{:name "dirt" :enchants []}] opts)) "an empty :enchants is not enchanted")
    (is (= 7 (apply value [{:name "diamond_pickaxe" :durability 1}] opts)) "no durability term on a price")
    (is (= 21 (apply value [{:name "diamond_pickaxe" :durability 1 :count 3}] :prices [[#"^diamond_pickaxe$" 21 :per-stack]] (drop 2 opts)))
        "nor on a per-stack price")
    (is (< (value [{:name "diamond_pickaxe" :durability 1}]) (value [{:name "diamond_pickaxe"}])) "an unpriced one has it")
    (is (= 25 (apply value [{:name "diamond_pickaxe" :enchants [{:name "mending"}]}] opts)))))

(deftest else-prices-every-unmatched-item
  (is (= 0 (value [{:name "dirt"} {:name "mystery_thing"}] :prices [[#"^diamond$" 9]] :else 0)))
  (is (= 9 (value [{:name "diamond"}] :prices [[#"^diamond$" 9]] :else 0))))

(deftest the-base-gives-sane-defaults
  (is (< (each "granite") (* 2 (each "cobblestone"))) "a natural block a recipe also makes is not its recipe")
  (are [name] (> (each name) (each "dirt"))
    "carrot" "redstone" "string" "torch")
  (is (< (* 3 (each "wheat")) (each "bread") (* 4 (each "wheat"))) "bread is three wheat and a craft")
  (is (> (each "bread") (each "carrot")))
  (is (> (each "bone") (each "rotten_flesh")))
  (is (> (each "gunpowder") (each "bone")))
  (is (< (each "lapis_lazuli") (each "raw_iron")))
  (is (< (each "copper_ingot") (each "iron_ingot")))
  (is (< (each "apple") (each "diamond")) "a 0.5% drop is not sourced from leaves")
  (is (< (each "cobblestone") (each "coal") (each "raw_iron") (each "diamond"))))

(deftest placed-forms-are-no-free-source-and-no-source-items-keep-the-stack-rule
  (is (> (each "redstone") 1))
  (is (= (* (:no-source-each v/default-basis) 64) (each "totem_of_undying")) "no source: 4 x 64 / stack of 1")
  (is (= 640 (value [{:name "totem_of_undying"}] :basis {:no-source-each 10})) "the no-source worth is in the basis"))

(deftest durability-and-enchantment-constants-are-in-the-basis
  (let [pick [{:name "diamond_pickaxe" :durability 1}]]
    (is (> (value pick :basis {:min-durability-share 0.5}) (* 4 (value pick))))
    (is (> (value [{:name "dirt" :enchants [{:name "mending" :lvl 1}]}] :basis {:per-enchant-level 60})
           (* 1.5 (value [{:name "dirt" :enchants [{:name "mending" :lvl 1}]}]))))))

(deftest hurt-is-a-share-of-the-hp-price-and-enderman-is-dangerous
  (is (< (each "gunpowder") (value [{:name "gunpowder"}] :basis {:mob {:hurt-hp-per-hp 0.1}})) "the hurt is priced per hp")
  (is (> (value [{:name "gunpowder"}] :basis {:mob {:hurt-hp-per-hp 0}}) 0))
  (is (> (each "ender_pearl") (* 1.5 (each "bone"))) "an enderman is a hard fight"))

(deftest basis-options-change-the-table
  (is (> (value [{:name "rotten_flesh"}] :basis {:junk-share 1}) (* 5 (each "rotten_flesh"))))
  (is (> (value [{:name "granite"}] :basis {:natural #{}}) (* 10 (each "granite"))) "the natural set is a default")
  (is (> (value [{:name "diamond"}] :basis {:find {:ores {"diamond" [600 1.5]}}}) (* 1.9 (each "diamond"))))
  (is (= (each "coal") (value [{:name "coal"}] :basis {:find {:ores {"diamond" [600 1.5]}}})) "the other ores are kept")
  (is (not= (value [{:name "diamond"}] :basis {:find {:ores {"diamond" [600 1.5]}}}) (each "diamond"))
      "two bases, two tables")
  (is (< (value [{:name "iron_pickaxe"}] :overrides {"iron_ingot" 0} :basis {:junk-share 1}) (each "stone_pickaxe"))
      "a name override still flows through"))

;; ---------------------------------------------------------------- fetch-cost

(deftest fetch-cost-grows-with-distance-and-danger
  (is (< (:cost (v/fetch-cost {:distance 10 :danger 0 :elapsed-ms 0}))
         (:cost (v/fetch-cost {:distance 100 :danger 0 :elapsed-ms 0}))
         (:cost (v/fetch-cost {:distance 100 :danger 9 :elapsed-ms 0})))))

(deftest fetch-cost-is-infinite-when-the-pile-is-gone-or-unreachable
  (are [args reason] (= [js/Infinity reason] ((juxt :cost :reason) (v/fetch-cost args)))
    {:distance 10 :elapsed-ms 300000} :window-closed
    {:distance 2000 :elapsed-ms 0} :too-far
    {:distance 100 :elapsed-ms 280000} :too-far
    {:distance 10 :elapsed-ms 0 :cause "lava"} :lethal-cause
    {:distance 10 :elapsed-ms 0 :cause :out_of_world} :lethal-cause
    {:distance nil :elapsed-ms 0} :no-position))

(deftest fetch-cost-lists-its-parts
  (let [c (v/fetch-cost {:distance 100 :danger 9 :elapsed-ms 0})]
    (is (= (:cost c) (reduce + (vals (:parts c)))))
    (is (= #{:trip :walk :danger} (set (keys (:parts c)))))))

(deftest walk-cost-is-the-walk-and-danger-parts-of-a-fetch
  (let [w (v/walk-cost {:distance 20 :danger 2})
        f (v/fetch-cost {:distance 20 :danger 2 :elapsed-ms 0})]
    (is (= {:walk 6 :danger 20} (:parts w)))
    (is (= 26 (:cost w)))
    (is (= (+ v/trip (:cost w)) (:cost f)) "a fetch is a trip plus the walk")
    (is (= (assoc (:parts w) :trip v/trip) (:parts f)))))

(deftest a-dark-block-costs-twice-a-lit-one
  (is (= 1 v/dark-factor) "per-dark over per-block")
  (is (= v/per-dark v/per-block)))
