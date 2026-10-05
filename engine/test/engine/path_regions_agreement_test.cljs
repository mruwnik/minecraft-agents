(ns engine.path-regions-agreement-test
  "The region map against the planner on every recorded bench case (test/planner-bench.json: the 144 courses and,
  when the frozen bench is on this machine, the world queries): where the planner found a path the map answers
  reachable (no false unreachable), and where the map proves a goal unreachable the planner found none."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.courses :as courses]
            [engine.path.regions :as regions]
            [engine.planner-bench-test :as bench]
            [engine.planner-fixture :as pf]))

(defn world [snapshot] #js {:snapshot snapshot :table @pf/table :space @pf/space})

(defn check
  "[id planner-status map-answer] of each case whose answers disagree: found -> anything but reachable, or unreachable
  where the planner found a path"
  [cases rm-of recorded]
  (vec (for [{:keys [id snapshot query]} cases
             :let [status (first (recorded id))
                   r (regions/route (rm-of snapshot) (:from query) [(:goal query)])]
             :when (or (and (= status "found") (not= :reachable (:status r)))
                       (and (= :unreachable (:status r)) (= status "found")))]
         [id status (select-keys r [:status :why])])))

(defn tally [cases rm-of recorded]
  (frequencies (for [{:keys [id snapshot query]} cases]
                 [(first (recorded id)) (:status (regions/route (rm-of snapshot) (:from query) [(:goal query)]))])))

(defn course-cases []
  (for [n (bench/course-names)
        :let [{:keys [snapshot from goal]} (courses/course-snapshot n)]]
    {:id n :snapshot snapshot :query {:from from :goal goal}}))

(deftest every-course-the-planner-walks-the-map-calls-reachable
  (let [cases (course-cases)
        recorded (fn [n] (get-in @bench/recorded [:course (keyword n)]))
        rm-of (fn [snapshot] (regions/create (world snapshot)))]
    (is (= 144 (count cases)))
    (is (= [] (check cases rm-of recorded)))
    ;; every course the planner does not walk is proved unreachable (none merely unknown)
    (is (= {["found" :reachable] 122 ["partial" :unreachable] 20 ["none" :unreachable] 1 ["none" :unknown] 1}
           (tally cases rm-of recorded)))))

(deftest every-world-query-the-planner-walks-the-map-calls-reachable
  (let [cases (bench/world-queries)
        recorded (fn [id] (get-in @bench/recorded [:world (keyword id)]))
        shared (volatile! nil)
        rm-of (fn [snapshot] (or @shared (vreset! shared (regions/create (world snapshot)))))]
    (if (empty? cases)
      (is (bench/skip-world?) "the frozen bench is missing (PLANNER_BENCH_SKIP_WORLD=1 to skip)")
      (do (is (= [] (check cases rm-of recorded)))
          ;; the one the planner did not find that the map calls reachable ran into the planner's search box: without
          ;; the box the planner finds it
          (let [over-box (for [{:keys [id snapshot query]} cases
                               :when (and (not= "found" (first (recorded id)))
                                          (= :reachable (:status (regions/route (rm-of snapshot) (:from query) [(:goal query)]))))]
                           [id (:status (pf/plan snapshot query {:margin 2048 :yMargin 384 :maxNodes 2000000}))])]
            (is (= [["darkforest-far-8" "found"]] (vec over-box))))
          (is (= {["found" :reachable] 135 ["partial" :reachable] 1 ["partial" :unreachable] 13 ["none" :unreachable] 11}
                 (tally cases rm-of recorded)))))))
