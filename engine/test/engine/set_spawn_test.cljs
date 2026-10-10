(ns engine.set-spawn-test
  "jobs.survival.set-spawn against the fake world: a bed used by day sets the respawn point."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.job-api :as job-api]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [run-async run-until-empty]]
            [engine.shelter-test :as st :refer [setup know-bed! calls entries emitted noon night]]))

(def bed {:x 6 :y 64 :z 0})
(def far-bed {:x 90 :y 64 :z 0})
(def bed-world {:time noon :blocks {"6,64,0" "red_bed"}})
(def hut
  "A walled hut (inside x 5..9, z -2..2) with a doorway at x 4, z 0, and a bed in it beside the north wall."
  (merge (apply dissoc (tu/box 4 64 -3 10 66 3 "stone")
                (concat (for [x (range 5 10) z (range -2 3) y [64 65 66]] (str x "," y "," z))
                        ["4,64,0" "4,65,0"]))
         {"6,64,-2" "red_bed"}))

(def policy {:cap 5 :ttl 1200000})

(defn ^:async run-set-spawn
  "Set up world, run (f eng) before the job, run jobs.survival.set-spawn with args: {:eng :p :seen :id}."
  [world args f]
  (let [s (setup world)]
    (f (:eng s))
    (let [id (core/submit! (:eng s) (list 'jobs.survival.set-spawn args) {})]
      (await (run-until-empty (:eng s) 6))
      (assoc s :id id))))

(defn wait-reason [world known-bed]
  (let [{:keys [eng]} (setup world)]
    (when known-bed (know-bed! eng known-bed))
    (let [id (core/submit! eng '(jobs.survival.set-spawn) {})]
      (core/tick! eng)
      (:reason (:waiting (job-api/summary eng id))))))

(deftest set-spawn-uses-the-remembered-bed-by-day-and-records-the-spawn-bed
  (async done
    (run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (run-set-spawn bed-world {} #(know-bed! % bed)))]
          (is (= [bed] (mapv st/arg-pos (calls p "useOn"))))
          (is (= [{:pos bed}] (entries eng :spawn-bed)))
          (is (= [] (emitted seen :spawn_not_set))))))))

(deftest set-spawn-uses-the-bed-argument-without-a-remembered-bed
  (async done
    (run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (run-set-spawn bed-world {:bed bed} (constantly nil)))]
          (is (= [bed] (mapv st/arg-pos (calls p "useOn"))))
          (is (= [{:pos bed}] (entries eng :spawn-bed))))))))

(deftest set-spawn-walks-in-to-a-bed-behind-a-wall-instead-of-clicking-through-it
  (async done
    (run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-set-spawn {:time noon :blocks hut :self {:pos {:x 6.5 :y 64 :z -4.5}}} {:bed {:x 6 :y 64 :z -2}} (constantly nil)))
              pos (.-pos (.self p))]
          (is (< -3 (.-z pos)) (str "the body is inside the hut, at " (.-x pos) " " (.-z pos))))))))

(deftest set-spawn-forgets-a-remembered-bed-that-is-gone-and-uses-the-one-it-sees
  (async done
    (run-async done
      (fn ^:async t []
        (let [seen-bed {:x 2 :y 64 :z 0}
              {:keys [eng p]} (await (run-set-spawn {:time noon :blocks {"2,64,0" "red_bed"}} {} #(know-bed! % bed)))]
          (is (= [seen-bed] (mapv st/arg-pos (calls p "useOn"))))
          (is (= {:gone true :was bed} (:data (mem/latest (mem/view (:store eng)) :bed))))
          (is (= [{:pos seen-bed}] (entries eng :spawn-bed))))))))

(deftest set-spawn-forgets-a-remembered-bed-that-is-gone-and-waits-when-it-sees-none
  (let [{:keys [eng]} (setup {:time noon})]
    (know-bed! eng bed)
    (let [id (core/submit! eng '(jobs.survival.set-spawn) {})]
      (core/tick! eng)
      (is (= :no-bed (:reason (:waiting (job-api/summary eng id))))))))

(deftest set-spawn-does-not-click-again-when-the-spawn-bed-is-known
  (async done
    (run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (run-set-spawn bed-world {} #(know-bed! % bed)))]
          (core/submit! eng '(jobs.survival.set-spawn) {})
          (await (run-until-empty eng 6))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest set-spawn-clicks-again-after-a-spawn-reset
  (async done
    (run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (run-set-spawn bed-world {} #(know-bed! % bed)))]
          (swap! clock + 1000)
          (mem/write! (:store eng) :spawn-reset {} policy)
          (core/submit! eng '(jobs.survival.set-spawn) {})
          (await (run-until-empty eng 6))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest set-spawn-stops-and-warns-when-the-click-does-not-set-it
  (async done
    (run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (run-set-spawn {:time noon :blocks {"6,64,0" "respawn_anchor"}} {:bed bed} (constantly nil)))]
          (is (= [] (entries eng :spawn-bed)))
          (is (= 1 (count (emitted seen :spawn_not_set))))
          (is (re-find #"did not set the respawn point" (:text (first (emitted seen :spawn_not_set))))))))))

(deftest set-spawn-skips-a-remembered-bed-beyond-the-bed-radius
  (is (= :no-bed (wait-reason {:time noon :blocks {"90,64,0" "red_bed"}} far-bed))))

(deftest set-spawn-says-why-it-waits
  (are [world known-bed reason] (= reason (wait-reason world known-bed))
    {:time night :blocks {"6,64,0" "red_bed"}} bed :night
    {:time noon} nil :no-bed))

(deftest set-spawn-clicks-when-the-spawn-set-near-the-bed-was-reset
  (async done
    (run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup bed-world)]
          (know-bed! eng bed)
          (mem/write! (:store eng) :spawn-set {:pos bed} policy)
          (swap! clock + 1000)
          (mem/write! (:store eng) :spawn-reset {} policy)
          (core/submit! eng '(jobs.survival.set-spawn) {})
          (await (run-until-empty eng 6))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest set-spawn-records-the-spawn-bed-when-a-spawn-set-near-the-bed-is-known
  (async done
    (run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup bed-world)]
          (know-bed! eng bed)
          (mem/write! (:store eng) :spawn-set {:pos bed} policy)
          (core/submit! eng '(jobs.survival.set-spawn) {})
          (await (run-until-empty eng 6))
          (is (= 0 (count (calls p "useOn"))))
          (is (= [{:pos bed}] (entries eng :spawn-bed))))))))
