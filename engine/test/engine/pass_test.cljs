(ns engine.pass-test
  "engine.path.pass: the cutting of a plan at the steps that open something, the zone rule, and what a round leaves behind."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.path.pass :as pass]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [engine.world :as world]))

(defn lane
  "Steps along +x at y 64, z 0, x 0..n, with the :opens maps in opens, {index [cells]}."
  [n opens]
  (vec (for [x (range (inc n))] (cond-> {:x x :y 64 :z 0 :move :walk} (opens x) (assoc :opens (opens x))))))

(def gate [{:x 5 :y 64 :z 0}])

(deftest a-segment-ends-before-the-run-into-the-next-opening
  (are [steps s pending expected] (= expected (pass/segment steps s pending))
    (lane 10 {})        0 [] {:end 10 :open nil}
    (lane 10 {6 gate})  0 [] {:end 4 :open 6}
    (lane 10 {5 gate})  0 [] {:end 4 :open 5}
    (lane 10 {6 gate})  4 [] {:end 4 :open 6}
    (lane 10 {6 gate})  5 [] {:end 5 :open 6}
    (lane 10 {})        4 [(pass/column-of (first gate) nil)] {:end 6 :open nil}
    (lane 10 {})        6 [] {:end 10 :open nil}
    (lane 10 {})       10 [] {:end 10 :open nil}))

(deftest the-segment-after-an-opening-ends-where-the-body-is-clear-of-its-column
  (let [airlock (lane 12 {5 [{:x 4 :y 64 :z 0}] 9 [{:x 8 :y 64 :z 0}]})
        a (pass/column-of {:x 4 :y 64 :z 0} nil)]
    (is (= {:end 3 :open 5} (pass/segment airlock 0 [])))
    (is (= {:end 5 :open nil} (pass/segment (update airlock 5 dissoc :opens) 3 [a])) "clear of the first gate at x 5")
    (is (= {:end 7 :open 9} (pass/segment (update airlock 5 dissoc :opens) 5 [])) "the second gate is opened from before its column")))

(deftest a-trapdoor-over-a-ladder-is-cut-before-its-column-and-clear-above-it
  (let [climb [{:x 3 :y 64 :z 0 :move :walk} {:x 3 :y 65 :z 0 :move :climb-up}
               {:x 3 :y 66 :z 0 :move :open :opens [{:x 3 :y 66 :z 0}]} {:x 3 :y 67 :z 0 :move :climb-up} {:x 4 :y 67 :z 0 :move :walk}]
        hatch (pass/column-of {:x 3 :y 66 :z 0} nil)]
    (is (= {:end 0 :open 2} (pass/segment climb 0 [])) "opened from the cell below the one under the hatch")
    (is (= {:end 3 :open nil} (pass/segment (update climb 2 dissoc :opens) 0 [hatch])) "shut once the feet are above it")))

(deftest an-open-move-is-a-climb
  (let [steps [{:x 3 :y 64 :z 0 :move :walk} {:x 3 :y 65 :z 0 :move :open} {:x 3 :y 64 :z 0 :move :open} {:x 4 :y 64 :z 0 :move :walk}]]
    (is (= [:walk :climb-up :climb-down :walk] (mapv :move (pass/with-climbs steps))))))

(deftest a-door-column-spans-both-halves
  (are [cell props low high] (= [low high] ((juxt :low :high) (pass/column-of cell props)))
    {:x 5 :y 64 :z 0} {:half "lower"} 64 65
    {:x 5 :y 65 :z 0} {:half "upper"} 64 65
    {:x 5 :y 64 :z 0} nil 64 64))

(deftest a-zone-of-another-owner-reaches-one-block-past-its-box
  (let [zone {:name "pen" :min [4 63 -3] :max [6 66 3] :owner "Other"}]
    (are [zones cell expected] (= expected (pass/foreign? zones "Fake" cell))
      [zone] [5 64 0] true
      [zone] [7 64 0] true
      [zone] [8 64 0] false
      [zone] [5 64 4] true
      [zone] [5 64 5] false
      [zone] [5 68 0] false
      [(assoc zone :owner "Fake")] [5 64 0] false
      [] [5 64 0] false)))

;; ------------------------------------------------------------------ leftovers of a cut walk

(def gate-world
  {:self {:pos {:x 3 :y 64 :z 0}}
   :blocks (merge (floor -2 -3 12 3) (assoc (box 5 64 -6 5 64 6 "oak_fence") "5,64,0" "oak_fence_gate"))
   :states {"5,64,0" {:open true :facing "east"}}})

(defn ^:async leftover-run
  "A job that records an :opened entry for the open gate at 5 64 0 as made by `by` (:self for itself) and runs
  shut-leftovers! with doors; the gate's open state afterwards and the :opened entries left."
  [by doors]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake gate-world)
        cell {:x 5 :y 64 :z 0}
        job {:check (constantly true)
             :round (fn ^:async leftover-round [c]
                      (ctx/remember! c :opened {:cell cell :by (if (= :self by) (:id c) by) :t 0} pass/opened-policy)
                      (await (pass/shut-leftovers! c doors))
                      :done)}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'leftover job) :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(leftover) {})
    (loop [i 0]
      (when (and (< i 5) (seq (:list (core/state eng))))
        (await (core/tick! eng))
        (recur (inc i))))
    {:open (.-open (.-properties (.blockAt p #js {:x 5 :y 64 :z 0})))
     :entries (mapv :data (mem/entries (mem/view (:store eng)) :opened))}))

(deftest a-round-shuts-the-blocks-an-earlier-round-of-its-own-job-left-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (leftover-run :self :shut))]
          (is (false? (:open r)))
          (is (= [] (:entries r))))))))

(deftest a-round-leaves-what-another-job-opened-and-what-its-policy-keeps-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[by doors] [["other-job" :shut] [:self :leave-open]]]
          (let [r (await (leftover-run by doors))]
            (is (true? (:open r)) (str by doors))
            (is (= 1 (count (:entries r))) (str by doors))))))))
