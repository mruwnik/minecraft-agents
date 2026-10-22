(ns engine.reach-job-test
  "jobs.movement.reach: a stair down to a goal far below and a bridge over a gap, chosen by price over go-to; a walk that
  exists is taken before either (jobs.lib.escape.span)."
  (:require [cljs.test :refer [deftest is are async]]
            [jobs.lib.cost.ways :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.span :as span]
            [jobs.movement.reach :as reach]
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
    (is (nil? (span/down-stair at [0 63 0] [8 53 0] (constantly true) none #{[:stair :east] [:stair :south] [:stair :north]})) "west ends 18 blocks off the goal's column")
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
                                                    'jobs.movement.reach
                                                    {:pos [10 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [10 64 0] (feet p)))
          (is (= [:bridge] (map :step (events-of seen :reach.span)))))))))

(deftest a-walk-only-call-does-not-bridge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks gulf :inventory [{:name "dirt" :count 9}]}
                                                  'jobs.movement.reach
                                                  {:pos [10 64 0] :range 0 :ways #{:walk}}))]
          (is (= false (:arrived @out)) (pr-str @out (mapv (juxt :kind :text) @seen)))
          (is (empty? (events-of seen :reach.span))))))))

(deftest a-walk-round-the-gap-is-taken-before-a-bridge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [round (merge gulf (box 1 63 -20 5 63 -19 "stone"))
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks round :inventory [{:name "dirt" :count 9}]}
                                                    'jobs.movement.reach
                                                    {:pos [10 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [10 64 0] (feet p)))
          (is (empty? (events-of seen :reach.span))))))))

(deftest a-free-body-cuts-a-stair-down-to-a-goal-far-below
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [deep (apply dissoc (box -20 30 -20 40 63 20 "stone") ["14,44,0" "14,45,0"])
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks deep :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    'jobs.movement.reach
                                                    {:pos [14 44 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [14 44 0] (feet p)))
          (is (= [:stair] (distinct (map :step (events-of seen :reach.span)))) "a dug route of over 16 cells is over the dig limit"))))))

(deftest only-a-walk-with-no-route-is-no-case-for-a-span
  (are [kind why want] (= want (reach/no-way? {:kind kind :data {:why why :kind :gap-up}}))
    :nothing-possible :goal-cut-off true
    :nothing-possible :goal-enclosed true
    :nothing-possible :exhausted true
    :nothing-possible :detour true
    :nothing-possible :abilities true
    :nothing-possible :door-stuck false
    :nothing-possible :stuck false
    :nothing-possible :goal-dangerous false
    :nothing-possible :unseen-floor-dark true
    :nothing-possible :needs-health false
    :condition-failed :goal-cut-off false
    :limit-reached :goal-cut-off false))

(deftest a-sprint-or-door-the-body-lacks-is-no-case-for-a-span
  (are [kind] (false? (reach/no-way? {:kind :nothing-possible :data {:why :abilities :kind kind}}))
    :corner-jump :gap-sprint :open))

(deftest a-body-far-from-the-gap-walks-to-its-edge-and-bridges
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x -12.5 :y 64 :z 0.5}} :blocks gulf :inventory [{:name "dirt" :count 9}]}
                                                    'jobs.movement.reach
                                                    {:pos [10 64 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [10 64 0] (feet p)))
          (is (= [[:bridge true] [:bridge false]] (distinct (map (juxt :step :approach) (events-of seen :reach.span)))) "walked to the edge first"))))))

(deftest the-walk-lights-a-dark-way-before-a-span-is-built
  (are [walk want] (= want (:torches (reach/walk-args {:quote-call {} :args {:pos [1 2 3] :range 1 :walk walk}} false)))
    {} :fetch
    {:torches :never} :never))

(deftest a-body-in-a-mine-wider-than-a-room-stairs-up-to-a-surface-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [rock (box -30 0 -30 70 40 30 "stone") ; rock past every stair: no ledge branch (card a6be0289)
              mine (reduce dissoc rock (for [x (range -20 21) y (range 10 13) z (range -20 21)] (str x "," y "," z)))
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x -19.5 :y 10 :z 0.5}} :blocks mine :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    'jobs.movement.reach
                                                    {:pos [40 41 0] :range 1}))]
          (is (true? (:arrived @out)) (pr-str @out (mapv (juxt :kind :text) @seen)))
          (is (<= 40 (second (feet p)))))))))
