(ns engine.goal-test
  "jobs.lib.goal: the destination of a walk that was cut, read from other jobs' memory."
  (:require [cljs.test :refer [deftest is]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.goal :as goal]))

(defn view-of [jobs]
  (let [store (mem/open (tu/tmp-dir) {})]
    (doseq [[id data] jobs] (mem/write! store (mem/job-kind id) data mem/job-policy))
    (mem/view store)))

(def goal-a {:x 5 :y 64 :z 9})

(deftest the-goal-of-an-open-go-to-in-another-job-is-found
  (let [v (view-of {"j1" {:children {:walk {:children {:go {:open 1 :goal goal-a}}}}}
                    "j2" {:children {:flee {:open 2 :goal {:x 0 :y 0 :z 0}}}}})]
    (is (= goal-a (goal/interrupted-goal v "j2")) "the reflex's own memory is skipped")
    (is (= {:x 0 :y 0 :z 0} (goal/interrupted-goal v "j1")) "from j1's side the other job's walk is the one")))

(deftest a-finished-walk-leaves-no-goal
  (is (nil? (goal/interrupted-goal (view-of {"j1" {:children {:go {:goal goal-a}}}}) "j9")) "no :open, no cut walk"))

(deftest an-errands-walk-is-not-the-destination
  (let [v (view-of {"j1" {:job "jobs.survival.chop" :children {:go {:open 1 :goal goal-a}}}
                    "j2" {:job "jobs.access.cleanup" :children {:go {:open 2 :goal {:x 0 :y 0 :z 0}}}}})]
    (is (= goal-a (goal/interrupted-goal v "j9")) "the cleanup walk, though written last, is skipped")))
