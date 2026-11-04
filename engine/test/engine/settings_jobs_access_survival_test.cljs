(ns engine.settings-jobs-access-survival-test
  "The access, survival and combat job tuning values are settings: each default is the old constant and an override is
  read at use."
  (:require [cljs.test :refer [deftest is]]
            [engine.settings :as settings]
            [jobs.access.bridge :as bridge]
            [jobs.access.cleanup :as cleanup]
            [jobs.access.pillar :as pillar]
            [jobs.access.stair :as stair]
            [jobs.combat.attack :as attack]
            [jobs.survival.breathe :as breathe]
            [jobs.survival.extinguish :as extinguish]
            [jobs.survival.night.sites :as sites]
            [jobs.survival.recover-drops.config :as recover-drops]
            [jobs.survival.respond.state :as flight]))

(deftest an-override-changes-the-value-read-at-use
  (settings/with-settings {:jobs.access.pillar/max-height 10 :jobs.survival.extinguish/hazard-reach 7
                        :jobs.survival.recover-drops.config/search-radius 5}
    (fn []
      (is (= 10 (pillar/max-height)))
      (is (= 7 (extinguish/hazard-reach)))
      (is (= 5 (recover-drops/search-radius)))
      (is (= 64 (bridge/max-length)) "an unset key keeps its default"))))
