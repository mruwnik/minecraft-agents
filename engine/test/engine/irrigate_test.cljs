(ns engine.irrigate-test
  "jobs.farm.irrigate against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup [world]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake world))]
    {:eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                        :world (ew/of-data {} {} [])
                        :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
     :p p}))

(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))
(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(def job 'jobs.farm.irrigate)
(def bucket [{:name "water_bucket" :count 1}])

(defn strip
  "A row of farmland from x 0 to n-1 at y 63, z 0, with stone beside it to walk on."
  [n]
  (into {} (mapcat (fn [x] [[(str x ",63,1") "stone"] [(str x ",63,-1") "stone"]
                            [(str x ",62,0") "dirt"] [(str x ",63,0") (if (< -1 x n) "farmland" "stone")]]))
        (range -2 (+ n 2))))

(defn args [n] {:from {:x 0 :y 63 :z 0} :to {:x (dec n) :y 63 :z 0}})

(deftest a-dry-field-gets-one-water-spot-that-wets-all-of-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory bucket :blocks (strip 9)})
              r (await (tu/child-outcome eng job (args 9) 40))
              water (filterv #(= "water" (block-at p {:x % :y 63 :z 0})) (range 9))]
          (is (= 1 (count water)))
          (is (= [] (:dry r)))
          (is (= 1 (count (:poured r)))))))))

(deftest a-field-wider-than-the-water-reach-keeps-its-far-cell-dry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory bucket :blocks (strip 10)})
              r (await (tu/child-outcome eng job (args 10) 40))]
          (is (= 1 (count (:poured r))))
          (is (= 1 (count (:dry r))) "one bucket wets nine of ten cells; the rest are reported"))))))

(deftest a-field-with-water-in-reach-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory bucket :blocks (assoc (strip 9) "4,63,3" "water")})
              r (await (tu/child-outcome eng job (args 9) 20))]
          (is (empty? (calls p "place")))
          (is (empty? (:poured r))))))))

(deftest no-bucket-and-no-fetch-refuses-the-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (strip 5)})
              r (await (tu/child-outcome eng job (assoc (args 5) :fetch false) 10))]
          (is (= {:reason :need :item "water_bucket"} (select-keys (:cause (:ending r)) [:reason :item]))))))))

