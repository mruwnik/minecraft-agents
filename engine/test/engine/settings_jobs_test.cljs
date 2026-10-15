(ns engine.settings-jobs-test
  "The job-family tuning values (items, village, animals, farm, gather, build, apiary, explore, blocks) are settings:
  each default is the old constant and an override is read at use."
  (:require [cljs.test :refer [deftest is are]]
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
            [jobs.items.interact :as interact]
            [jobs.items.smelt :as smelt]
            [jobs.village.feed :as feed]))

(deftest defaults-are-the-old-constants
  (are [f v] (= v (f))
    interact/reach 2.5
    interact/max-walks 2
    smelt/slack-ms 2000
    smelt/seen-radius 32
    feed/min-gap 2
    feed/foot-height 0.3
    leash/max-in-row 3
    unleash/knot-margin 12
    herd-run/watch-radius 64
    herd-run/crowd-out-radius 2.5
    herd-pen/pinned-ms 6000
    harvest/max-steps 400
    till/max-cells 256
    mine-drops/pickup-grace-ms 400
    give/head-height 1.6))

(deftest game-facts-are-constants
  (is (= [64 64 5 12000] [facts/stack-size facts/slot-max facts/ripe-level facts/dusk-tick])))

(deftest an-override-changes-the-value-read-at-use
  (settings/with-settings {:jobs.items.interact/reach 4 :jobs.gather.mine.drops/pickup-grace-ms 10}
    (fn []
      (is (= 4 (interact/reach)))
      (is (= 10 (mine-drops/pickup-grace-ms)))
      (is (= 2 (interact/max-walks)) "an unset key keeps its default"))))
