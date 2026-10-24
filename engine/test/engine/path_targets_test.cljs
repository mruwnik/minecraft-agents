(ns engine.path-targets-test
  "jobs.lib.targets: the nearest of many targets by walking cost, one bounded search a call, against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.memory :as mem]
            [jobs.lib.clock :as clock]
            [jobs.lib.targets :as targets]
            [jobs.lib.walk.plan :as wplan]
            [engine.test-util :as tu]))

(defn box
  "Blocks named name filling x0..x1, y0..y1, z0..z1."
  [x0 y0 z0 x1 y1 z1 name]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(def floor
  "Stone at y 63 well inside chunks -1 and 1: no stand is within 2 columns of an unloaded chunk (no frontier)."
  (box -13 63 -13 28 63 28 "stone"))

(defn sealed
  "Stone round the cell x 64 z on its four sides (two high) and over it: no move enters it."
  [x z]
  (merge (box (dec x) 64 z (dec x) 65 z "stone") (box (inc x) 64 z (inc x) 65 z "stone")
         (box x 64 (dec z) x 65 (dec z) "stone") (box x 64 (inc z) x 65 (inc z) "stone") (box x 66 z x 66 z "stone")))

(def start {:x 2.5 :y 64 :z 2.5})

(defn ^:async answer-of
  "nearest! called (chunk-expansions 16) from start over blocks until it answers more than :searching: [answer calls].
  world (optional) is merged into the fake's spec (:entities, ...), self into its :self."
  ([blocks ts range opts] (answer-of blocks ts range opts nil))
  ([blocks ts range opts {:keys [world self]}]
  (let [p (tu/fake (merge {:blocks blocks :self (merge {:pos start} self)} world))
        c {:primitives p :view (fn [] {:data mem/empty-data :now 0})}]
    (reset! targets/searches {})
    (await (tu/with-expansion-budget 16
             (fn ^:async run []
               (loop [calls 1]
                 (let [a (await (targets/nearest! c ts range opts))]
                   (if (and (= :searching (:status a)) (< calls 500))
                     (recur (inc calls))
                     [a calls])))))))))

(def near-sealed {:x 6 :y 64 :z 2})
(def far-open {:x 20 :y 64 :z 2})

(deftest a-walled-off-nearest-target-is-passed-over-for-a-reachable-farther-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[a] (await (answer-of (merge floor (sealed 6 2)) [near-sealed far-open] 0 nil))]
          (is (= {:status :found :target far-open :index 1} (select-keys a [:status :target :index])))
          (is (> (:cost a) 0)))))))

(deftest a-search-over-the-budget-goes-on-at-the-next-call-and-gives-the-same-answer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge floor (sealed 6 2))
              [whole] (await (answer-of blocks [near-sealed far-open] 0 nil))
              [a calls] (await (answer-of blocks [near-sealed far-open] 0 {:budget 16}))]
          (is (> calls 3) "several calls")
          (is (= whole a))
          (is (= {} @targets/searches) "the search is over"))))))

(deftest a-search-over-the-round-time-goes-on-at-the-next-call-and-gives-the-same-answer
  ;; the budget is out of reach, so only the clock ends a round: each read of the ticking clock is 1000 ms
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge floor (sealed 6 2))
              [whole] (await (answer-of blocks [near-sealed far-open] 0 nil))
              stopped clock/now
              ticks (atom 0)
              _ (set! clock/now #(swap! ticks + 1000))
              [a calls] (try (await (answer-of blocks [near-sealed far-open] 0 {:budget 1e9}))
                             (finally (set! clock/now stopped)))]
          (is (> calls 3) "the time cut ends rounds before the search is over")
          (is (= whole a)))))))

(deftest every-target-walled-off-ends-on-the-node-cap-unproved
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cap targets/max-nodes
              _ (set! targets/max-nodes 300)
              [a] (await (answer-of (merge floor (sealed 6 2) (sealed 20 20)) [near-sealed {:x 20 :y 64 :z 20}] 0 nil))]
          (set! targets/max-nodes cap)
          (is (= {:status :none :reason :budget :proved false} a)))))))

(deftest every-target-walled-off-in-land-it-can-search-whole-is-proved
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[a] (await (answer-of (merge floor (sealed 6 2) (sealed 20 20)) [near-sealed {:x 20 :y 64 :z 20}] 0 nil))]
          (is (= :none (:status a)))
          (is (true? (:proved a))))))))

(deftest one-target-is-not-searched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[a calls] (await (answer-of (merge floor (sealed 6 2)) [near-sealed] 0 nil))]
          (is (= {:status :found :target near-sealed :index 0} a))
          (is (= 1 calls)))))))

(def zombie {:id 9 :uuid "u9" :name "zombie" :kind "hostile" :pos {:x 12.5 :y 64 :z 2.5}})

(deftest a-danger-on-the-way-costs-more-at-low-health
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ts [{:x 20 :y 64 :z 2} {:x 20 :y 64 :z 14}]
              full (first (await (answer-of floor ts 0 nil {:world {:entities [zombie]}})))
              low (first (await (answer-of floor ts 0 nil {:world {:entities [zombie]} :self {:health 4}})))]
          (is (> (:cost low) (:cost full)) (str "low " (:cost low) " full " (:cost full))))))))

(def pen-ring
  "A fence ring round x -1..5, z -1..5 at y 64 with a shut oak_fence_gate on its east side at x 5, z 2."
  (-> (into {} (for [x (range -1 6) z (range -1 6) :when (#{-1 5} (if (#{-1 5} x) x z))]
                 [(str x ",64," z) "oak_fence"]))
      (assoc "5,64,2" "oak_fence_gate")))

(deftest a-shut-fence-gate-in-a-pen-ring-lets-the-search-find-targets-outside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [outside [{:x 12 :y 64 :z 2} {:x 12 :y 64 :z 8}]
              [a] (await (answer-of (merge floor pen-ring) outside 3 nil))]
          (is (= :found (:status a)) (pr-str a)))))))
