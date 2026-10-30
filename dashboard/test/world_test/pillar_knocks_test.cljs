(ns world-test.pillar-knocks-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.pillar-knocks :as t]))

(defn line [kind t extra] (pr-str (merge {:kind kind :time-ms t :data {:built (:built extra 0)}} (dissoc extra :built))))

(defn rows [body lines] (map #(assoc % :body body) (keep t/scan-line lines)))

(deftest counts-knockbacks-per-block-placed-from-the-first-knockback-on
  (let [r (t/report (concat (rows "A" [(line :pillar.done 1 {:built 50}) (line :pillar.knockback 5 nil) (line :pillar.done 10 {:built 6}) (line :pillar.gave-up 11 {:built 4})
                                        (line :pillar.knockback 13 nil) (line :other 14 {:built 9})])))]
    (is (= {:blocks 10 :knocks 2 :ended-off 0 :chance 0.2} (select-keys r [:blocks :knocks :ended-off :chance]))
        "the 50 blocks before the first knockback was logged are not in the denominator")))

(deftest older-logs-count-the-off-column-endings
  (let [r (t/report (rows "A" [(line :pillar.done 1 {:built 8}) (pr-str {:kind :pillar.gave-up :time-ms 2 :data {:built 2 :reason :off-column}})]))]
    (is (= {:blocks 10 :knocks 0 :ended-off 1 :chance 0.1} (select-keys r [:blocks :knocks :ended-off :chance])))))
