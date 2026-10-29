(ns world-test.respawn-distance-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.respawn-distance :as r]))

(defn line [kind pos] (pr-str {:source :body :kind kind :data {:pos pos :cause "zombie"}}))

(deftest a-death-is-measured-to-the-respawn-that-follows-it
  (is (= {:k :died :pos {:x 0 :y 0 :z 0}} (r/scan-line (line :died {:x 0 :y 0 :z 0}))))
  (is (nil? (r/scan-line (line :moved {:x 0 :y 0 :z 0}))))
  (let [at (fn [k x] {:k k :pos {:x x :y 0 :z 0}})]
    (is (= [30 5] (r/distances [(at :died 0) (at :respawned 30) (at :died 100) (at :died 95) (at :respawned 100)]))
        "two deaths in a row count the last")
    (is (= [] (r/distances [(at :respawned 3)])) "a respawn with no death before it counts nothing")))

(deftest the-report-gives-the-median-and-quartiles
  (is (= {:n 5 :median 30 :q1 20 :q3 40} (r/report [50 20 30 10 40])))
  (is (= {:n 0 :median nil :q1 nil :q3 nil} (r/report []))))
