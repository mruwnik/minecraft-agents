(ns engine.go-to-priced-ways-test
  "go-to's world-changing ways chosen by price for a free body: a stair down to a goal far below, a bridge over a gap,
  and a walk that exists is taken before either (jobs.lib.escape.span, escalate-choose/goal-way)."
  (:require [cljs.test :refer [deftest is are async]]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.span :as span]
            [jobs.movement.go-to.escalate-choose :as choose]
            [engine.test-util :as tu :refer [box]]
            [engine.go-to-escalate-support :refer [run-job! feet events-of]]))

(defn block-at-of-map [blocks] (fn [[x y z]] (get blocks (str x "," y "," z) "air")))

(def edge-floor (box 0 63 -2 1 63 2 "stone"))

(deftest gap-along-reads-the-floor-cells-ahead
  (let [blocks (merge edge-floor (box 5 63 -2 8 63 2 "stone"))
        at (block-at-of-map blocks)
        never (constantly false)]
    (are [feet d want] (= want (span/gap-along at never feet d))
      [1 64 0] [1 0] {:length 3 :landing [5 64 0]}
      [1 64 0] [-1 0] nil
      [0 64 0] [-1 0] nil)))

(deftest gap-along-gives-up-on-unsensed-floor-and-solid-ahead
  (let [at (block-at-of-map (merge edge-floor (box 5 63 -2 8 63 2 "stone")))]
    (is (nil? (span/gap-along at #(= % [3 63 0]) [1 64 0] [1 0])) "a floor cell never seen is no known gap")
    (is (nil? (span/gap-along (block-at-of-map (box 0 63 -2 8 63 2 "stone")) (constantly false) [1 64 0] [1 0])) "solid floor ahead: nothing to bridge")))

(deftest bridge-span-picks-the-gap-nearest-the-goal-and-counts-the-blocks-short
  (let [at (block-at-of-map (merge edge-floor (box 5 63 -2 8 63 2 "stone")))
        opts (fn [count] {:pillar-item (when count {:item "dirt" :count count}) :skip #{} :fetch? false})]
    (is (= {:step :bridge :heading :east :length 3 :item "dirt" :short 0 :landing [5 64 0]}
           (span/bridge-span at (constantly false) [1 64 0] [8 64 0] (opts 5))))
    (is (nil? (span/bridge-span at (constantly false) [1 64 0] [8 64 0] (opts 2))) "short of blocks and not fetching")
    (is (= 1 (:short (span/bridge-span at (constantly false) [1 64 0] [8 64 0] (assoc (opts 2) :fetch? true)))))
    (is (nil? (span/bridge-span at (constantly false) [1 64 0] [8 64 0] (assoc (opts 5) :skip #{:bridge}))))
    (is (nil? (span/bridge-span at (constantly false) [1 64 0] [-8 64 0] (opts 5))) "the way west leads nowhere nearer than the way back")))

(deftest down-stair-needs-a-goal-two-below-and-cuts-it-may-dig
  (let [stone (box -25 40 -5 20 63 5 "stone")
        at (block-at-of-map stone)
        none (constantly false)]
    (is (= {:step :stair :dir :down :heading :east :steps 10 :aside 2} (span/down-stair at [0 63 0] [8 53 0] (constantly true) none #{})))
    (is (nil? (span/down-stair at [0 63 0] [8 62 0] (constantly true) none #{})) "one below is a drop or a dig-route")
    (is (= :west (:heading (span/down-stair at [0 63 0] [8 53 0] (constantly true) none #{[:stair :east] [:stair :south] [:stair :north]}))))
    (is (nil? (span/down-stair at [0 63 0] [8 53 0] (constantly false) none #{})) "no cell may be dug")))

(deftest the-ways-are-priced-in-seconds-by-their-blocks
  (is (< (cost/pillar-cost 3) (cost/stair-cost 3)))
  (is (< (cost/bridge-cost 4) (cost/bridge-cost 4 1)) "a block to fetch adds to the price"))

(def gulf
  "Stone floors at y 63 either side of a 5-wide void (x 1..5), z -20..20, the body on the west edge."
  (merge (box -20 63 -20 0 63 20 "stone") (box 6 63 -20 20 63 20 "stone")))

(deftest a-free-body-bridges-a-gap-the-planner-cannot-cross
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks gulf :inventory [{:name "dirt" :count 9}]}
                                                    {:pos [10 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [10 64 0] (feet p)))
          (is (= [:bridge] (map :step (events-of seen :go-to.escalated)))))))))

(deftest a-walk-only-call-does-not-bridge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks gulf :inventory [{:name "dirt" :count 9}]}
                                                  {:pos [10 64 0] :range 0 :ways #{:walk}}))]
          (is (= false (:arrived @out)))
          (is (empty? (events-of seen :go-to.escalated))))))))

(deftest a-walk-round-the-gap-is-taken-before-a-bridge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [round (merge gulf (box 1 63 -20 5 63 -19 "stone"))
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks round :inventory [{:name "dirt" :count 9}]}
                                                    {:pos [10 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [10 64 0] (feet p)))
          (is (empty? (events-of seen :go-to.escalated))))))))

(deftest a-free-body-cuts-a-stair-down-to-a-goal-far-below
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [deep (apply dissoc (box -20 30 -20 40 63 20 "stone") ["14,44,0" "14,45,0"])
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks deep :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [14 44 0] :range 0}))
              first-way (first (events-of seen :go-to.escalated))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [14 44 0] (feet p)))
          (is (= :stair (:step first-way)) "a dug route of over 16 cells is over the dig limit"))))))
