(ns engine.recover-drops-status-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.survival.recover-drops :as recover-drops]))

(def died-entry {:t 1000 :data {:pos {:x 50.5 :y 40 :z 3.5} :cause "skeleton" :inventory [{:name "raw_iron" :count 24}]}})

(deftest death-summary-reports-where-cause-and-time-left
  (is (= {:pos {:x 50.5 :y 40 :z 3.5} :cause "skeleton" :ago-ms 62000 :despawns-in-ms 238000}
         (recover-drops/death-summary died-entry 63000))))

(deftest death-summary-marks-a-pile-recovered-after-the-death
  (is (= {:pos {:x 50.5 :y 40 :z 3.5} :cause "skeleton" :ago-ms 62000 :despawns-in-ms 238000 :recovered :collected}
         (recover-drops/death-summary died-entry 63000 {:t 2000 :data {:decision :collected}})))
  (is (not (contains? (recover-drops/death-summary died-entry 63000 {:t 500 :data {:decision :collected}}) :recovered))
      "a :recovered older than the death is another death's"))

(deftest death-summary-omits-an-unknown-cause
  (is (= {:pos {:x 1 :y 2 :z 3} :ago-ms 0 :despawns-in-ms 300000}
         (recover-drops/death-summary {:t 5 :data {:pos {:x 1 :y 2 :z 3}}} 5))))

(deftest death-summary-is-nil-once-the-drops-have-despawned-or-without-a-death
  (is (nil? (recover-drops/death-summary died-entry 301000)))
  (is (nil? (recover-drops/death-summary nil 5))))
