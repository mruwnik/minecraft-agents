(ns engine.heap-test
  (:require [cljs.test :refer [deftest is]]
            [engine.heap :as heap]
            [jobs.lib.walk.search :as wsearch]))

(deftest the-sample-reads-the-heap-and-the-planner-stores
  (reset! wsearch/known-land {:a {:cells (js/Set. #js [1 2 3]) :edges (js/Set. #js [1])} :b {:cells (js/Set. #js [4]) :edges (js/Set.)}})
  (reset! wsearch/goal-floods {:a {}})
  (reset! wsearch/searches {:a {:k1 {} :k2 {}} :b {}})
  (let [s (heap/sample)]
    (is (every? number? (map s [:heap-used :heap-limit :rss :external :array-buffers])))
    (is (< (:heap-used s) (:heap-limit s)))
    (is (= {:known-cells 4 :known-edges 1 :goal-floods 1 :searches 2} (select-keys s [:known-cells :known-edges :goal-floods :searches])))))

(deftest a-timer-more-than-the-stall-threshold-late-is-a-stall-event-with-what-was-running
  (let [ctx {:running "j1" :job ['jobs.mining.dig]}]
    (is (nil? (heap/stall-event 1900 0 1000 ctx)) "0.9 s late is within the threshold")
    (let [e (heap/stall-event 21000 0 1000 ctx)]
      (is (= [:body.stall 20000 "j1" ['jobs.mining.dig]] ((juxt :kind :late-ms :running :job) e)))
      (is (= :warn (:level e))))))

(deftest the-loop-delay-window-reads-and-resets
  (let [stop (heap/start! (fn [_]) 60000 (fn [] {}))]
    (is (number? (heap/loop-max-ms!)))
    (stop)))

(deftest the-running-context-names-the-leaf-jobs-of-the-round-in-flight
  (let [eng {:running (atom {:id "j1" :round 3})
             :state (atom {:instances {"j1" {:spec {:op :seq :children [{:op :leaf :job 'a.b} {:op :repeat :child {:op :leaf :job 'c.d}}]}}}})}]
    (is (= {:running "j1" :round 3 :job ['a.b 'c.d]} (heap/running-context eng)))))
