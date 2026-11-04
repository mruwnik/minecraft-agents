(ns engine.goal-test
  "jobs.lib.goal: the destination of a walk that was cut, kept in body memory."
  (:require [cljs.test :refer [deftest is]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.goal :as goal]))

(def goal-a {:x 5 :y 64 :z 9})

(defn ctx-of
  "A ctx over a memory store, for root and slots; reflex marks a trigger's job."
  ([store root] (ctx-of store root [] nil))
  ([store root slots reflex]
   {:root root :slots slots :reflex reflex
    :view (fn [] (mem/view store))
    :remember (fn [kind data policy] (mem/write! store kind data policy))
    :forget (fn [kind pred] (mem/forget-where! store kind pred))}))

(defn store [] (mem/open (tu/tmp-dir) {}))

(deftest the-goal-of-an-open-walk-of-another-job-is-found
  (let [s (store)]
    (goal/note! (ctx-of s "j1" [:walk :go] nil) goal-a)
    (goal/note! (ctx-of s "j2") {:x 0 :y 0 :z 0})
    (is (= goal-a (goal/interrupted-goal (mem/view s) "j2")) "the reflex's own walk is skipped")
    (is (= {:x 0 :y 0 :z 0} (goal/interrupted-goal (mem/view s) "j1")) "from j1's side the other job's walk is the one")))

(deftest a-finished-walk-leaves-no-goal
  (let [s (store) c (ctx-of s "j1")]
    (goal/note! c goal-a)
    (goal/clear! c)
    (is (nil? (goal/interrupted-goal (mem/view s) "j9")))))

(deftest a-walk-noted-again-keeps-one-goal-per-call
  (let [s (store) c (ctx-of s "j1")]
    (goal/note! c goal-a)
    (goal/note! c {:x 0 :y 0 :z 0})
    (is (= 1 (count (mem/entries (mem/view s) :walk-goal))))
    (is (= {:x 0 :y 0 :z 0} (goal/interrupted-goal (mem/view s) "j9")))))

(deftest a-triggers-walk-is-not-the-destination
  (let [s (store)]
    (goal/note! (ctx-of s "j1") goal-a)
    (goal/note! (ctx-of s "j2" [] :ore-seen) {:x 0 :y 0 :z 0})
    (is (= goal-a (goal/interrupted-goal (mem/view s) "j9")) "the errand's walk, though written last, is not kept")))

(deftest clearing-a-walk-with-no-goal-writes-nothing
  (let [s (store) writes (atom 0)
        c (-> (ctx-of s "j1")
              (update :forget (fn [f] (fn [& a] (swap! writes inc) (apply f a)))))]
    (goal/clear! c)
    (goal/note! c goal-a)
    (goal/clear! c)
    (goal/clear! c)
    (is (= 1 @writes) "only the clear that dropped a goal is a write")))

(deftest the-route-of-an-open-walk-is-kept-by-its-corners
  (let [s (store) c (ctx-of s "j1")
        cell (fn [x z] {:x x :y 64 :z z})]
    (goal/note! c goal-a)
    (goal/note-route! c [(cell 0 0) (cell 1 0) (cell 2 0) (cell 2 1) (cell 2 2)])
    (is (= [[0 64 0] [2 64 0] [2 64 2]] (:route (goal/interrupted-walk (mem/view s) "j9"))))
    (is (= 1 (count (mem/entries (mem/view s) :walk-goal))) "one entry per call")
    (is (= goal-a (goal/interrupted-goal (mem/view s) "j9")) "the goal stays")))
