(ns engine.obtain-unseen-price-test
  "items.obtain's choice between a way whose raw needs are all seen and a search for one that is not: each priced, the
  cheaper taken."
  (:require [cljs.test :refer [deftest is]]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.items.obtain :as obtain]
            [jobs.items.obtain.plan :as plan]
            [jobs.items.obtain.price :as price]
            [jobs.items.obtain.table :as table]
            [jobs.items.recipes :as recipes]
            [jobs.lib.cost :as cost]
            [jobs.lib.makers :as makers]))

(defn spider [x] {:id 1 :name "spider" :kind "hostile" :pos {:x x :y 64 :z 0} :drops [{:name "string" :count 1}]})

(defn ctx [world]
  (let [view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))]
    {:primitives (tu/fake-on-floor (merge {:floor h/flight-floor} world)) :view (constantly view)
     :args {:item "white_bed" :count 1 :ways #{:craft :smelt :gather :hunt :search} :explore true}}))

(defn kit [wool]
  (cond-> (into h/sword [{:name "oak_planks" :count 3} {:name "crafting_table" :count 1}])
    (pos? wool) (conj {:name "white_wool" :count wool})))

(defn choice
  "The raw items obtain gathers for a white bed in world with wool carried, and the price of each way: the sheep search
  and the spider hunt."
  [world wool]
  (let [c (ctx (assoc world :inventory (kit wool)))
        v (game/version-of (:primitives c))
        price #(plan/plan-cost c v {:steps [] :gather %})]
    {:gather (:gather (plan/gather-plan c ["white_bed"] 1))
     :search (price {"white_wool" (- 3 wool)})
     :hunt (price {"string" (* 4 (- 3 wool))})}))

(deftest a-hurt-body-searches-for-sheep-by-night-rather-than-fight-a-far-spider-for-each-string-and-a-near-spider-by-day-is-hunted
  (let [far-night (choice {:time 18000 :self {:health 8} :entities [(spider 40)]} 0)
        near-day (choice {:time 6000 :entities [(spider 5)]} 2)]
    (is (< (:search far-night) (:hunt far-night)) "twelve fights of a hurt body cost more than a likely sheep search")
    (is (= {"white_wool" 3} (:gather far-night)))
    (is (< (:hunt near-day) (:search near-day)) "four kills a few blocks off cost less than a search")
    (is (= {"string" 4} (:gather near-day)))))

(deftest no-search-is-priced-when-the-call-may-not-search
  (let [c (assoc-in (ctx {:time 6000 :inventory (kit 0) :entities [(spider 40)]}) [:args :explore] false)]
    (is (= {"string" 12} (:gather (plan/gather-plan c ["white_bed"] 1))) "the seen spider, however dear")))

(deftest another-way-that-leaves-out-nothing-more-ends-the-look-for-one
  (let [c (ctx {:time 6000 :inventory (kit 0)})
        v (game/version-of (:primitives c))
        pl {:steps [] :gather {"white_wool" 3}}]
    (with-redefs [recipes/plan (fn [& _] pl)]
      (is (= pl (plan/preferred c v pl {} "white_bed" 1 {}))
          "the other way gathers the same unseen wool: no way is left, and the plan stays"))))


(defn torch-choice
  "The raw items obtain gathers for 4 torches, sticks and a furnace carried, standing on a floor of block at y (its ground)
  with logs at the given cells, every block in sight seen (and the call's searches so far, :explored), and the price of
  each way: the coal search and the charcoal."
  [block y logs & {:keys [explored]}]
  (let [p (tu/seeing-all (tu/fake-on-floor {:floor [y -50 -45 50 45] :floor-block block :time 6000
                                            :self {:pos [0.5 (inc y) 0.5]}
                                            :blocks (into {} (map (fn [[x z]] [(str x "," (inc y) "," z) "oak_log"])) logs)
                                            :inventory [{:name "stick" :count 2} {:name "furnace" :count 1}]}))
        view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))
        c {:primitives p :view (constantly view)
           :args {:item "torch" :count 4 :ways #{:craft :smelt :gather :search} :explore true :ignore-zones? true}}
        v (game/version-of p)
        price #(plan/plan-cost c v {:steps [] :gather %})]
    (with-redefs [ctx/mem (if explored (constantly {:explored explored}) ctx/mem)]
      {:gather (:gather (plan/gather-plan c ["torch"] 4))
       :coal (price {"coal" 1})
       :charcoal (price {"log" 1})})))

(deftest coal-unseen-is-searched-for-in-shallow-rock-and-charcoal-made-from-logs-beside-the-body-where-coal-is-improbable
  (let [rock (torch-choice "stone" 59 [])
        far-logs (torch-choice "stone" 149 [[42 0] [42 1] [42 -1]])
        dirt (torch-choice "dirt" 63 [[2 0]])
        deep (torch-choice "deepslate" -31 [[2 0]])]
    (is (< (:coal rock) js/Infinity) "the coal the rock seen round may hold is priced by its search")
    (is (= {"coal" 1} (:gather rock)) "no logs seen: the coal search")
    (is (< (:coal far-logs) (:charcoal far-logs)) "high rock, where both coal placements reach, makes a coal search cheaper than a walk to logs 42 off and the smelt (the world case)")
    (is (= {"coal" 1} (:gather far-logs)))
    (is (= {"log" 1} (:gather (torch-choice "stone" 149 [[42 0] [42 1] [42 -1]] :explored {"coal" 1}))) "the coal search found none: the logs are next")
    (is (< (:charcoal dirt) (:coal dirt)) "dirt holds no coal")
    (is (= {"log" 1} (:gather dirt)))
    (is (< (:charcoal deep) (:coal deep)) "no coal is placed below y 0")
    (is (= {"log" 1} (:gather deep)))))

(deftest a-kill-whose-loot-holds-none-of-the-need-is-no-way-to-it
  (let [c (ctx {:time 6000 :inventory (kit 0) :entities [(spider 5)]})]
    (is (< (plan/need-cost c {:key "string" :count 4 :mob "spider" :job 'jobs.combat.hunt}) js/Infinity))
    (is (infinite? (plan/need-cost c {:key "bone" :count 1 :mob "spider" :job 'jobs.combat.hunt})) "a spider drops no bone")))

(defn iron-way
  "search-way of 3 unseen raw iron (worth the call's :search-worth, given) for a body on a floor of block at y (every block
  in sight seen), carrying a stone pickaxe and inventory (16 torches by default), and the price of the plan that gathers them."
  [block y & {:keys [worth inventory ways blocks] :or {inventory [{:name "torch" :count 16}] ways #{:craft :smelt :gather :search}}}]
  (let [p (tu/seeing-all (tu/fake-on-floor {:floor [y -50 -45 50 45] :floor-block block :time 6000 :blocks blocks
                                            :self {:pos [0.5 (inc y) 0.5]}
                                            :inventory (into [{:name "stone_pickaxe" :count 1}] inventory)}))
        view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))
        c {:primitives p :view (constantly view)
           :args (cond-> {:item "raw_iron" :count 3 :ways ways :explore true :ignore-zones? true}
                   worth (assoc :search-worth worth))}
        v (game/version-of p)
        need (first (table/gather-needs {"raw_iron" 3} v))]
    (assoc (plan/search-way c need)
           :tunnel (:cost (plan/tunnel-way c need))
           :plan (plan/plan-cost c v {:steps [] :gather {"raw_iron" 3}}))))

(def kit-worth (cost/items-find-worth (zipmap makers/iron-kit (repeat 1))))

(deftest the-iron-kits-worth-pays-the-stair-up-from-deep-levels
  (is (<= 1000 kit-worth) "the climb fixture's :search-worth 1000 is no more than the kit bill's own worth")
  (doseq [y [-39 -20]]
    (is (= [:tunnel 16] ((juxt :way :y) (iron-way "dirt" y :worth kit-worth)))
        "the 36 to 55 dark levels up cost less than the finds a strip tunnel at the feet would wait for")))

(deftest unseen-iron-is-tunnelled-for-at-its-level-unless-a-cave-of-rock-in-sight-is-cheaper-to-look-over
  (let [grass (iron-way "grass_block" 69)
        cave (iron-way "stone" 39)]
    (is (= [:tunnel 16] ((juxt :way :y) grass)) "no rock in sight from y 70: a stair down to y 16 and a strip tunnel there")
    (is (< (:plan grass) js/Infinity) "the get-kit plan for iron has a price")
    (is (= :look (:way cave)) "a cave floor of stone seen at y 40 shows iron more cheaply than a stair and a tunnel")
    (is (< (:cost cave) (:tunnel cave) js/Infinity))
    (is (= [:tunnel 16] ((juxt :way :y) (iron-way "dirt" -42 :worth kit-worth)))
        "deep below iron's level, where only its small placement reaches, a stair up to iron's level and a tunnel there")
    (is (= [:tunnel -41] ((juxt :way :y) (iron-way "dirt" -42 :worth kit-worth :blocks {"0,-36,-5" "bedrock" "0,-36,5" "bedrock" "5,-36,0" "bedrock" "-5,-36,0" "bedrock"})))
        "a seen bedrock block in the cells the stair would cut: no stair goes up, the kit's bill still pays a tunnel at the feet")
    (is (= [:tunnel 16] ((juxt :way :y) (iron-way "dirt" -42 :worth kit-worth :blocks {"3,-36,3" "bedrock"})))
        "bedrock seen beside the stair's line is no bar")
    (is (= :look (:way (iron-way "stone" -41 :worth kit-worth)))
        "a hall floored with stone at y -41 (obtain-iron-look) is looked over at the kit's worth, not tunnelled")))

(deftest a-tunnel-without-torches-pays-for-getting-them-and-none-to-be-got-is-no-tunnel
  (let [stocked (:tunnel (iron-way "stone" 39))
        coal (:tunnel (iron-way "stone" 39 :inventory [{:name "coal" :count 4} {:name "stick" :count 4}]))
        planks (:tunnel (iron-way "stone" 39 :inventory [{:name "oak_planks" :count 4}]))
        no-source (:tunnel (iron-way "stone" 39 :inventory [{:name "oak_planks" :count 4}] :ways #{:search}))]
    (is (< stocked coal planks js/Infinity) "the torches carried; crafted from coal and sticks carried; their coal searched for")
    (is (infinite? no-source) "no way to get torches: the tunnel's dark stair and legs are not dug")))

(defn coal-below-cost
  "need-cost of a coal ore seen 2 below the body's floor, 9 south, its cut at light [block sky], nothing carried."
  [light]
  (let [p (tu/seeing-all (tu/fake-on-floor {:time 6000
                                            :blocks {"0,62,9" "coal_ore" "0,63,9" "air"}
                                            :light (into {} (for [x (range -1 2) y (range 61 66) z (range 8 11)] [[x y z] light]))}))
        view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))
        c {:primitives p :view (constantly view) :args {:item "coal" :count 1 :ways #{:gather} :ignore-zones? true}}]
    (plan/need-cost c (first (table/gather-needs {"coal" 1} (game/version-of p))))))

(deftest a-dark-ore-seen-below-with-no-torch-is-priced-by-its-dark-way-not-refused
  (let [dark (coal-below-cost [0 0])]
    (is (< (coal-below-cost [0 15]) dark js/Infinity) "the dark walk there costs more than a lit one, but is a way")))

(deftest the-torches-a-tunnel-stocks-are-priced-once-for-all-the-tunnelled-needs-of-a-plan
  (let [p (tu/seeing-all (tu/fake-on-floor {:floor [69 -50 -45 50 45] :floor-block "grass_block" :time 6000
                                            :self {:pos [0.5 70 0.5]}
                                            :inventory [{:name "stone_pickaxe" :count 1}]}))
        view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))
        c {:primitives p :view (constantly view)
           :args {:item "iron_ingot" :count 3 :ways #{:craft :smelt :gather :search} :explore true :ignore-zones? true}}
        v (game/version-of p)
        pl {:steps [] :gather {"raw_iron" 3 "raw_copper" 3}}
        needs (table/gather-needs (:gather pl) v)]
    (with-redefs [plan/stock-s (constantly 100)]
      (is (= [:tunnel :tunnel] (map (comp :way #(plan/search-way c %)) needs)))
      (is (= (- (reduce + (map #(plan/search-cost c %) needs)) 100) (plan/plan-cost c v pl))))))

(defn buried-search
  "search-s of a call for item by a body in a 2-high pocket of rock at y 40 that has seen no sky, carrying a stone pickaxe."
  [item & [worth]]
  (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 40 :z 0.5}} :inventory [{:name "stone_pickaxe" :count 1}]
                                                      :blocks (dissoc (tu/box -3 39 -3 3 43 3 "stone") "0,40,0" "0,41,0")})))
        view (mem/view (mem/open (tu/tmp-dir) {:now (constantly 0)}))
        c {:primitives p :view (constantly view) :args {:item item :count 1 :ways #{:craft :gather :search} :explore true :ignore-zones? true :worth worth}}]
    (with-redefs [ctx/mem (constantly {}) ctx/entries (constantly [])]
      {:price (price/search-s c [item] 1) :not-worth (obtain/not-worth c)})))

(deftest a-buried-bodys-search-pays-the-stair-up-only-when-the-gather-climbs-first
  (let [ore (:price (buried-search "raw_iron"))
        log (:price (buried-search "oak_log"))]
    (is (= (cost/trip) ore) "an unseen ore is tunnelled to: no stair up in its price")
    (is (< (cost/trip) log) "a tree is climbed toward before any search: the stair is in its price")))

(deftest the-start-check-refuses-a-buried-bodys-log-search-worth-less-than-trip-and-stair-but-not-its-ore-search
  (let [worth (+ (cost/trip) 1)]
    (is (nil? (:not-worth (buried-search "raw_iron" worth))) "the ore is tunnelled to: the trip is within the worth")
    (is (= :not-worth (:reason (:not-worth (buried-search "oak_log" worth)))) "the log search adds the stair up")))
