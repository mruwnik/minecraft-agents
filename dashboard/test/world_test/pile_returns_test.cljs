(ns world-test.pile-returns-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.pile-returns :as m]))

(defn died [t x n] (pr-str {:time-ms t :source :body :kind :died :data {:pos {:x x :y 64 :z 0} :inventory [{:name "cobblestone" :count n}]}}))
(defn respawned [t x] (pr-str {:time-ms t :source :body :kind :respawned :data {:pos {:x x :y 64 :z 0}}}))
(defn picked [t x n] (pr-str {:time-ms t :source :body :kind :picked-up :data {:item "cobblestone" :count n :pos {:x x :y 64 :z 0}}}))

(deftest a-death-near-its-respawn-counts-a-return-when-half-its-pile-is-picked-up-there
  (is (= {:k 1 :n 1} (m/report [(died 0 0 10) (respawned 10 40) (picked 9000 1 6)] 420000 12)) "6 of 10 back at the spot")
  (is (= {:k 0 :n 1} (m/report [(died 0 0 10) (respawned 10 40) (picked 9000 1 4) (picked 9500 30 6)] 420000 12)) "4 at the spot, the rest elsewhere")
  (is (= {:k 0 :n 1} (m/report [(died 0 0 10) (respawned 10 40) (picked 500000 1 10)] 420000 12)) "picked up after the window")
  (is (= {:k 0 :n 0} (m/report [(died 0 0 10) (respawned 10 200) (picked 9000 1 10)] 420000 12)) "a respawn past the despawn range")
  (is (= {:k 0 :n 0} (m/report [(died 0 0 10) (respawned 10 40) (picked 4000000 1 10)] 420000 12)) "no pick-up logged within the hour: not counted"))
