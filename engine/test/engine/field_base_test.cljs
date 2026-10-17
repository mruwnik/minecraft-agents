(ns engine.field-base-test
  "blueprints/field-base.edn: valid, and built by jobs.build.from-plan into two double chests, two furnaces, a bed, a
  door, a table and a torch, with :spots naming the cells jobs.strip reads."
  (:require [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            ["path" :as path]
            [cljs.reader :as reader]
            [engine.fake]
            [engine.core :as core]
            [engine.events :as events]
            [engine.harvest-test :as h]
            [engine.path.fixture :as fx]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [plan.shape :as shape]))

(def blueprint
  (reader/read-string (fs/readFileSync (path/join (fx/engine-root) ".." "blueprints" "field-base.edn") "utf8")))

(def plan {:id "base" :parts [{:id "base" :blueprint "field-base" :at [0 64 0]}]})

(defn spot [name] (get-in (shape/expand plan {"field-base" blueprint}) [:spots (str "base/" name)]))

(defn clicks [p item] (mapv #(js->clj (.-click (.-args %)) :keywordize-keys true) (filter #(= item (.-item (.-args %))) (h/calls p "place"))))

(def inventory
  [{:name "cobblestone" :count 64} {:name "oak_planks" :count 128} {:name "chest" :count 4} {:name "furnace" :count 2}
   {:name "crafting_table" :count 1} {:name "white_bed" :count 1} {:name "oak_door" :count 1} {:name "torch" :count 2}])

(defn state-at [p pos] (js->clj (.-properties (.blockAt p (clj->js (zipmap [:x :y :z] pos)))) :keywordize-keys true))
(defn name-at [p [x y z]] (h/block-at p x y z))

(deftest the-blueprint-is-valid
  (is (= [] (shape/blueprint-errors blueprint "field-base"))))

(deftest the-spots-name-every-fitting
  (is (= {"bed" [1 65 2] "chest-1" [1 65 1] "chest-2" [2 65 1] "chest-3" [4 65 1] "chest-4" [5 65 1]
          "furnace-1" [5 65 2] "furnace-2" [5 65 3] "table" [1 65 3] "door" [3 65 4] "torch" [1 66 3]}
         (into {} (map (fn [n] [n (spot n)])) ["bed" "chest-1" "chest-2" "chest-3" "chest-4" "furnace-1" "furnace-2" "table" "door" "torch"]))))

(deftest from-plan-builds-the-base
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ sink] (tu/legacy-capture-sink)
              p (tu/seeing-all (tu/fake-on-floor {:inventory inventory :self {:pos {:x -2.5 :y 64 :z -2.5}}}))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                                :world (world/of-data {"base" plan} {"field-base" blueprint} [])})
              result (await (tu/child-outcome eng 'jobs.build.from-plan {:plan "base"} 1500))]
          (is (every? #(<= 66 (second %)) (:missing result)) "only wall tops and roof cells the fake body cannot reach are left")
          (is (empty? (:wrong result)) "the server joins the chests, so their type is no claim")
          (is (every? #(not (:sneak %)) (clicks p "chest")) "no sneak, so a chest joins its neighbour")
          (is (every? #(= "chest" (name-at p (spot %))) ["chest-1" "chest-2" "chest-3" "chest-4"]))
          (is (every? #(= ["furnace" "west"] [(name-at p (spot %)) (:facing (state-at p (spot %)))]) ["furnace-1" "furnace-2"]))
          (is (= "crafting_table" (name-at p (spot "table"))))
          (is (= "white_bed" (name-at p (spot "bed"))))
          (is (= "oak_door" (name-at p (spot "door"))))
          (is (= "torch" (name-at p (spot "torch")))))))))
