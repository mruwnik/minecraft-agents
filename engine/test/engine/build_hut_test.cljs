(ns engine.build-hut-test
  "jobs.build.hut against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.harvest-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.build.hut :as hut]
            [jobs.farm.find-spot :as find-spot]
            [jobs.lib.world-files :as world]))

(def ring
  "A 3 x 4 ring of cobblestone, one layer, clear inside."
  {:id "ring" :front :south :key {"S" "cobblestone" "." :clear} :layers [["SSS" "S.S" "S.S" "SSS"]]})

(def ring-cells [[0 0 0] [1 0 0] [2 0 0] [0 0 1] [2 0 1] [0 0 2] [2 0 2] [0 0 3] [1 0 3] [2 0 3]])

(defn start [inventory plans]
  (let [[_ sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor {:inventory inventory :self {:pos {:x 0.5 :y 64 :z 0.5}}}))
        w (world/of-data plans {"ring" ring} [])
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                          :world w})]
    {:eng eng :p p :w w}))

(defn check-ctx [eng args] (core/make-ctx eng {:root "hut" :slots [] :chain [] :token nil :args args}))

(defn cells-at [[x y z]] (mapv (fn [[dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)]) ring-cells))

(deftest find-spot-sizes-the-patch-to-a-blueprint-with-no-crops
  (let [{:keys [eng]} (start [] {})]
    (is (= {:w 3 :h 4 :farmland? false}
           (select-keys (find-spot/patch-of (check-ctx eng {}) {:blueprint "ring"}) [:w :h :farmland?]))
        "not the default 5 x 5 farm patch")))

(deftest a-hut-is-placed-as-the-bodys-own-plan-and-built
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p w]} (start [{:name "cobblestone" :count 10}] {})
              result (await (tu/child-outcome eng 'jobs.build.hut {:blueprints ["ring"]} 600))
              part (first (:parts (:plan (world/plan w "hut-Fake"))))]
          (is (= :done (:status result)) (pr-str result))
          (is (= {:by "Fake"} (:metadata (:plan (world/plan w "hut-Fake")))))
          (is (= "ring" (:blueprint part)))
          (is (= 64 (get (:at part) 1)) "the bottom layer stands on the floor at y 63")
          (is (every? #(= "cobblestone" (h/block-at p (% 0) (% 1) (% 2))) (cells-at (:at part)))))))))

(deftest a-placed-plan-of-the-body-is-built-where-it-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [placed {:id "hut-Fake" :parts [{:id "hut" :blueprint "ring" :at [6 64 2]}] :metadata {:by "Fake"}}
              {:keys [eng p]} (start [{:name "cobblestone" :count 10}] {"hut-Fake" placed})
              result (await (tu/child-outcome eng 'jobs.build.hut {:blueprints ["ring"]} 600))]
          (is (= :done (:status result)) (pr-str result))
          (is (every? #(= "cobblestone" (h/block-at p (% 0) (% 1) (% 2))) (cells-at [6 64 2])) "no new site is chosen on a resume"))))))

(deftest with-nothing-carried-and-no-fetch-no-hut-is-placed
  (let [{:keys [eng w]} (start [] {})
        [kind _ data] (hut/site-check (check-ctx eng {:blueprints ["ring"] :ways #{:carried}}))]
    (is (= :later kind))
    (is (= {:reason :no-items :blueprint "ring" :needs {"cobblestone" 10}} data))
    (is (nil? (world/plan w "hut-Fake")))))

(deftest another-bodys-plan-is-never-taken
  (let [theirs {:id "hut-Fake" :parts [{:id "hut" :blueprint "ring" :at [6 64 2]}] :metadata {:by "Other"}}
        {:keys [eng]} (start [{:name "cobblestone" :count 10}] {"hut-Fake" theirs})
        [kind _ data] (hut/site-check (check-ctx eng {:blueprints ["ring"]}))]
    (is (= :never kind))
    (is (= :taken (:reason data)))))
