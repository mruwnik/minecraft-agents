(ns world-test.pillar-knocks-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.pillar-knocks :as t]))

(defn line [kind built extra] (pr-str (merge {:kind kind :data {:built built}} extra)))

(deftest counts-knockbacks-per-block-placed
  (let [rows (keep t/scan-line [(line :pillar.done 6 nil) (line :pillar.gave-up 4 nil) (line :pillar.knockback nil nil)
                                (line :pillar.knockback nil nil) (line :other 9 nil)])]
    (is (= {:blocks 10 :knocks 2 :ended-off 0 :chance 0.2} (t/report rows)))))

(deftest older-logs-count-the-off-column-endings
  (let [rows (keep t/scan-line [(line :pillar.done 8 nil) (pr-str {:kind :pillar.gave-up :data {:built 2 :reason :off-column}})])]
    (is (= {:blocks 10 :knocks 0 :ended-off 1 :chance 0.1} (t/report rows)))))
