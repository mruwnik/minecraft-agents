(ns engine.value-test
  "make-room's item worth (jobs.lib.cost.value/item-value per item) against its :toss-below default."
  (:require ["minecraft-data" :as minecraft-data]
            [cljs.test :refer [deftest is are]]
            [engine.game :as game]
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
    (is (> (worth "netherite_chestplate") (worth "netherite_boots")) "more armour points save more")
    (is (> (worth "netherite_pickaxe") (worth "diamond_pickaxe")) "the dearer tool is worth more")
    (is (> (worth "stone_sword") (worth "trident")) "a trident hits 9 but every 910 ms: three hits (2.7 s) are slower than a stone sword's four (2.5 s)")))

(deftest gear-use-is-saved-against-the-reference-zombie-fall-and-stone
  (let [md (minecraft-data game/default-version)
        basis (tbl/ref-gear-basis md tbl/default-basis)
        hardness (get-in basis [:gear :ref-hardness])
        stone-speed (weapon/tool-speed "stone_pickaxe")
        saved #(gear/use-saved basis %)
        hp-s (health/walk-hp-seconds)]
    (is (pos? hardness) "the reference block's hardness is read from the data")
    (is (= (* hp-s (gear/hit-saved "iron_chestplate" 3)) (saved "iron_chestplate")) "a zombie's 3 hp hit")
    (is (= (* hp-s (armour/fall-damage {} armour/ref-fall)) (saved "elytra")) "the fall fall-profile reads")
    (is (= (- (weapon/kill-seconds "stone_sword" 20) (weapon/kill-seconds "iron_sword" 20)) (saved "iron_sword")) "a zombie's 20 hp")
    (is (= (- (gear/dig-time (:dig basis) stone-speed hardness) (gear/dig-time (:dig basis) (weapon/tool-speed "iron_pickaxe") hardness))
           (saved "iron_pickaxe")) "a tool saves the dig time of the reference block over a stone tool")
    (is (> (saved "netherite_pickaxe") (saved "diamond_pickaxe")) "netherite digs at 9, diamond at 8: a use saves more")
    (is (> (saved "golden_pickaxe") (saved "iron_pickaxe")) "gold digs at 12 and harvests stone as wood does")
    (is (zero? (saved "iron_axe")) "an axe is not stone's tool: speed 1 and no drop, slower than the stone pickaxe")
    (is (zero? (saved "diamond_shovel")))))

(deftest a-tool-that-cannot-harvest-digs-at-a-hundred-over-speed
  (let [dig (:dig tbl/default-basis)]
    (is (= 7.5 (gear/dig-time dig 1 1.5 false)) "100 x 1.5 / 1 = 150 ticks")
    (is (= 1.15 (gear/dig-time dig 4 3 true)) "a harvesting tool keeps the 30 x hardness rule")
    (is (= 4 (gear/tier-speed dig 0)) "below the default tier it is the default's")
    (is (zero? (gear/use-saved (assoc-in (tbl/ref-gear-basis (minecraft-data game/default-version) tbl/default-basis) [:gear :ref-tier] 3) "golden_pickaxe"))
        "a golden pickaxe (tier 1) cannot harvest a block of tier 3")
    (is (= [4 6 8] (mapv #(gear/tier-speed dig %) [2 3 4])) "the weakest tool of the tier, read from the tool table")))

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

(deftest a-dig-takes-whole-ticks-of-thirty-hardness-over-speed
  (let [dig (:dig tbl/default-basis)
        secs #(gear/dig-time dig (weapon/tool-speed %) 3)]
    (is (= 1.15 (secs "stone_pickaxe")) "90/4 = 22.5 ticks takes 23")
    (is (= 0.6 (secs "diamond_pickaxe")) "90/8 = 11.25 takes 12 ticks")
    (is (= 0.5 (secs "netherite_pickaxe")) "90/9 = 10 ticks")))
