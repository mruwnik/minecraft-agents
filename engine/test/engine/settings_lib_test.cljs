(ns engine.settings-lib-test
  "The jobs.lib tuning values are settings: each reads its override at use, and its default is the old constant."
  (:require [cljs.test :refer [deftest is are]]
            [engine.settings :as settings]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.foods :as foods]
            [jobs.lib.pass :as pass]
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
    foods/low-health 10))

(deftest an-override-changes-the-value-read-at-use
  (settings/with-settings {:jobs.lib.pass/await-polls 2 :jobs.lib.util/max-failures 7 :jobs.lib.watch/alert-radius 5}
    (fn []
      (is (= 2 (pass/await-polls)))
      (is (= 7 (u/max-failures)))
      (is (= 5 (watch/alert-radius)))
      (is (= 500 (pass/shut-wait-ms)) "a key without an override keeps its default"))))
