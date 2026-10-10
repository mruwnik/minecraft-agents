(ns engine.settings-jobs-access-survival-test
  "The access, survival and combat job tuning values are settings: each default is the old constant and an override is
  read at use."
  (:require [cljs.test :refer [deftest is are]]
            [engine.settings :as settings]
            [jobs.access.bridge :as bridge]
            [jobs.access.cleanup :as cleanup]
            [jobs.access.pillar :as pillar]
            [jobs.access.stair :as stair]
            [jobs.combat.attack :as attack]
            [jobs.survival.breathe :as breathe]
            [jobs.survival.extinguish :as extinguish]
            [jobs.survival.night.sites :as sites]
            [jobs.survival.recover-drops :as recover-drops]
            [jobs.survival.retreat-flight :as flight]))

(deftest defaults-are-the-old-constants
  (are [f v] (= v (f))
    bridge/max-length 64
    cleanup/max-collect-radius 48
    pillar/max-height 64
    stair/max-steps 400
    attack/reach 3
    breathe/max-passes 120
    extinguish/walk-cost 0.5
    extinguish/pour-wait-ms 250
    sites/retry-after-ms 660000
    recover-drops/top-n 3
    flight/resume-gap-ms 5000))

(deftest an-override-changes-the-value-read-at-use
  (settings/with-settings {:jobs.access.pillar/max-height 10 :jobs.survival.extinguish/walk-cost 1.5}
    (fn []
      (is (= 10 (pillar/max-height)))
      (is (= 1.5 (extinguish/walk-cost)))
      (is (= 64 (bridge/max-length)) "an unset key keeps its default"))))
