(ns engine.value-test
  "make-room's item worth (jobs.lib.cost.value/item-value per item) against its :toss-below default."
  (:require [cljs.test :refer [deftest is are]]
            [engine.settings :as settings]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.gear :as gear]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.precious :as precious]
            [jobs.lib.cost.table :as tbl]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.meal :as meal]
            [jobs.lib.room-order :as ord]))

(def toss-below (get-in ord/settings [::ord/toss-below :default]))

(deftest everyday-junk-is-below-the-toss-line
  (are [item] (< (ord/item-worth item) toss-below)
    {:name "dirt" :count 64}
    {:name "cobblestone" :count 64}
    {:name "gravel" :count 10}
    {:name "rotten_flesh" :count 5}
    {:name "wheat_seeds" :count 20}
    {:name "oak_sapling" :count 2}
    {:name "stick" :count 8}
    {:name "mystery_thing" :count 1}))

(deftest ores-metals-tools-and-food-are-kept
  (are [item] (>= (ord/item-worth item) toss-below)
    {:name "coal" :count 30}
    {:name "charcoal" :count 30}
    {:name "raw_copper" :count 12}
    {:name "raw_gold" :count 3}
    {:name "lapis_lazuli" :count 5}
    {:name "raw_iron" :count 3}
    {:name "iron_ingot" :count 3}
    {:name "emerald" :count 3}
    {:name "diamond" :count 1}
    {:name "iron_pickaxe" :count 1}
    {:name "stone_pickaxe" :count 1}
    {:name "wooden_pickaxe" :count 1}
    {:name "bread" :count 2}
    {:name "cooked_beef" :count 4}
    {:name "apple" :count 1}
    {:name "oak_log" :count 20}
    {:name "oak_planks" :count 20}
    {:name "crimson_stem" :count 8}
    {:name "stick" :count 1 :enchants [{:id "sharpness" :lvl 1}]}))

(deftest worth-is-per-item-and-a-missing-count-is-one
  (is (= (ord/item-worth {:name "iron_ingot" :count 8}) (ord/item-worth {:name "iron_ingot"})))
  (is (> (ord/item-worth {:name "diamond"}) (ord/item-worth {:name "iron_ingot"}) (ord/item-worth {:name "dirt"}))))

(deftest a-precious-food-nothing-yields-is-worth-the-hp-it-heals
  (is (= (precious/hp-worth {:health (:unsourced-food-health tbl/default-basis) :absorption 0} "enchanted_golden_apple" false)
         (ord/item-worth {:name "enchanted_golden_apple" :count 1}))
      "not the no-source junk default"))

(deftest unsourced-gear-is-worth-what-its-uses-save
  (let [worth #(ord/item-worth {:name % :count 1})
        junk (* (:no-source-each tbl/default-basis) 64)]
    (doseq [item ["elytra" "trident" "netherite_chestplate" "netherite_pickaxe" "netherite_sword" "chainmail_chestplate"]]
      (is (not= junk (worth item)) (str item " is not the 256 s no-source default")))
    (is (> (worth "netherite_chestplate") (worth "netherite_helmet")) "more armour points save more")
    (is (> (worth "netherite_pickaxe") (worth "diamond_pickaxe")) "the dearer tool is worth more")
    (is (> (worth "stone_sword") (worth "trident")) "a trident hits 9 but every 910 ms: three hits (2.7 s) are slower than a stone sword's four (2.5 s)")))

(deftest gear-use-is-saved-against-the-reference-zombie-fall-and-stone
  (let [basis (assoc tbl/default-basis :gear (assoc (:gear tbl/default-basis) :ref-hardness 1.5))
        hp-s (health/walk-hp-seconds)]
    (is (= (* hp-s (gear/hit-saved "iron_chestplate" 3)) (gear/use-saved basis "iron_chestplate")) "a zombie's 3 hp hit")
    (is (= (* hp-s (armour/fall-damage {} armour/ref-fall)) (gear/use-saved basis "elytra")) "the fall fall-profile reads")
    (is (= (- (weapon/kill-seconds "stone_sword" 20) (weapon/kill-seconds "iron_sword" 20)) (gear/use-saved basis "iron_sword")) "a zombie's 20 hp")))

(deftest the-item-table-follows-the-settings-the-hp-price-reads
  (let [worth #(ord/item-worth {:name "enchanted_golden_apple" :count 1})
        base (worth)]
    (is (not= base (settings/with-settings {:jobs.lib.cost.health/hp-seconds 2} worth))
        "a cheaper hp makes the same apple worth less, not the memoised table")))

(deftest a-fed-body-decides-a-precious-food-as-the-table-prices-it
  (let [self (fn [food health] #js {:health health :food food :absorption 0 :inventory #js [#js {:name "enchanted_golden_apple" :count 1}]})]
    (is (= (meal/precious-worth? (self 20 8) "enchanted_golden_apple") (meal/precious-worth? (self 5 8) "enchanted_golden_apple")))
    (is (true? (meal/precious-worth? (self 20 8) "enchanted_golden_apple")) "below the flip it is eaten")
    (is (true? (meal/precious-worth? (self 20 9) "enchanted_golden_apple")))
    (is (false? (meal/precious-worth? (self 20 10) "enchanted_golden_apple")) "above it it is kept")))
