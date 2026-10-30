(ns world-test.respawn-meets-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.respawn-meets :as m]))

(defn died [t x id] (pr-str {:time-ms t :source :body :kind :died :data {:pos {:x x :y 0 :z 0} :damages [{:attacker {:id id :name "zombie"}}]}}))
(defn respawned [t x] (pr-str {:time-ms t :source :body :kind :respawned :data {:pos {:x x :y 0 :z 0}}}))
(defn hurt [t id] (pr-str {:time-ms t :source :body :kind :hurt :data {:attacker {:id id :name "zombie"}}}))

(deftest a-death-near-its-respawn-counts-the-killer-met-again
  (is (= {:k 1 :n 1} (m/report [(died 0 0 7) (respawned 10 5) (hurt 5000 7)] 600000)) "named again by its id after the respawn")
  (is (= {:k 0 :n 1} (m/report [(died 0 0 7) (respawned 10 5) (hurt 5000 8) (hurt 700000 7)] 600000)) "another mob, or too late")
  (is (= {:k 0 :n 0} (m/report [(died 0 0 7) (respawned 10 200) (hurt 5000 7)] 600000)) "a respawn past the zombie's follow range is no meet"))
