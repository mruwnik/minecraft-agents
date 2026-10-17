(ns engine.settings-lib-test
  "The jobs.lib tuning values are settings: each reads its override at use, and its default is the old constant."
  (:require [cljs.test :refer [deftest is]]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.value :as value]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.stock :as stock]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.pass :as pass]
            
            [jobs.lib.threats :as threats]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.toll-cells :as toll-cells]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]))

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

(deftest per-block-must-stay-positive
  (is (some? (a/problem :jobs.lib.cost.value/per-block 0)) "dark-factor divides by it")
  (is (nil? (a/problem :jobs.lib.cost.value/per-block 0.3))))
