(ns engine.set-spawn-test
  "jobs.survival.set-spawn against the fake world: a bed used by day sets the respawn point."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.shelter-test :as st :refer [setup know-bed! calls entries emitted noon night]]))

(def bed {:x 6 :y 64 :z 0})

(deftest set-spawn-uses-the-remembered-bed-by-day-and-records-the-spawn-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time noon :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng bed)
          (core/submit! eng '(jobs.survival.set-spawn) {})
          (await (run-until-empty eng 6))
          (is (= [bed] (mapv st/arg-pos (calls p "useOn"))))
          (is (= [{:pos bed}] (entries eng :spawn-bed)))
          (is (= [] (emitted seen :spawn_not_set))))))))

(deftest set-spawn-does-not-click-again-when-the-spawn-bed-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time noon :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng bed)
          (core/submit! eng '(jobs.survival.set-spawn) {})
          (await (run-until-empty eng 6))
          (core/submit! eng '(jobs.survival.set-spawn) {})
          (await (run-until-empty eng 6))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest set-spawn-waits-at-night-and-without-a-bed
  (doseq [world [{:time night :blocks {"6,64,0" "red_bed"}} {:time noon}]]
    (let [{:keys [eng p]} (setup world)]
      (when (:blocks world) (know-bed! eng bed))
      (core/submit! eng '(jobs.survival.set-spawn) {})
      (core/tick! eng)
      (is (= [] (calls p "useOn"))))))
