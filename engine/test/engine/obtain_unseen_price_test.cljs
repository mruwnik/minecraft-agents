(ns engine.obtain-unseen-price-test
  "items.obtain's choice between a way whose raw needs are all seen and a search for one that is not: each priced, the
  cheaper taken."
  (:require [cljs.test :refer [deftest is]]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.items.obtain.plan :as plan]
            [jobs.items.recipes :as recipes]))

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
