(ns engine.settings-jobs-test
  "The job-family tuning values (items, village, animals, farm, gather, build, apiary, explore, blocks) are settings:
  each default is the old constant and an override is read at use."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.game-facts :as facts]
            [engine.settings :as settings]
            [jobs.animals.herd-pen :as herd-pen]
            [jobs.animals.herd-run :as herd-run]
            [jobs.animals.leash :as leash]
            [jobs.animals.unleash :as unleash]
            [jobs.apiary.harvest :as harvest]
            [jobs.farm.till :as till]
            [jobs.gather.mine.drops :as mine-drops]
            [jobs.items.give :as give]
            [jobs.items.use :as interact]
            [jobs.items.smelt :as smelt]
            [jobs.village.feed :as feed]))

(deftest an-override-changes-the-value-read-at-use
  (settings/with-settings {:jobs.items.use/reach 4 :jobs.gather.mine.drops/pickup-grace-ms 10}
    (fn []
      (is (= 4 (interact/reach)))
      (is (= 10 (mine-drops/pickup-grace-ms)))
      (is (= 2 (interact/max-walks)) "an unset key keeps its default"))))
