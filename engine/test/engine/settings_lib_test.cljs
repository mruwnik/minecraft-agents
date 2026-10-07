(ns engine.settings-lib-test
  "The jobs.lib tuning values are settings: each reads its override at use, and its default is the old constant."
  (:require [cljs.test :refer [deftest is are]]
            [engine.settings :as settings]
            [jobs.lib.cost.value :as value]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.pass :as pass]
            [jobs.lib.shelter :as shelter]
            [jobs.lib.threats :as threats]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.toll-cells :as toll-cells]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]))

(deftest defaults-are-the-old-constants
  (are [f v] (= v (f))
    pass/await-polls 6
    pass/shut-wait-ms 500
    u/max-failures 3
    tidy/max-tries 3
    fetch/chest-radius 32
    watch/alert-radius 24
    threats/sensed-radius 24
    toll-cells/max-cells 8000
    tools/low-fraction 0.1
    foods/low-health 10
    dig-look/settle-wait-ms 500
    dig-look/settle-waits 6
    dig-look/flow-delay-ticks 34
    watch/turn-gap-ms 5000
    value/trip 10
    value/per-block 0.3
    value/per-dark 0.3
    value/walk-blocks-per-s 2.9
    value/dark-factor 1
    shelter/seen-bed-radius 6
    shelter/default-player-radius 128
    look/dark-light 8
    look/toss-reach 10
    look/glance-ahead 4
    look/glance-near 1.5))

(deftest an-override-changes-the-value-read-at-use
  (settings/with-settings {:jobs.lib.pass/await-polls 2 :jobs.lib.util/max-failures 7 :jobs.lib.watch/alert-radius 5}
    (fn []
      (is (= 2 (pass/await-polls)))
      (is (= 7 (u/max-failures)))
      (is (= 5 (watch/alert-radius)))
      (is (= 500 (pass/shut-wait-ms)) "a key without an override keeps its default"))))

(deftest a-derived-value-follows-its-parts
  (settings/with-settings {:jobs.lib.cost.value/per-dark 0.6 :jobs.lib.dig-look/flow-delay-ticks 40}
    (fn []
      (is (< (js/Math.abs (- 2 (value/dark-factor))) 1e-9) "per-dark over per-block")
      (is (= (settings/ticks->ms 40) (dig-look/flow-delay-ms)))
      (is (= 5000 (:ttl (watch/turn-policy)))))))
