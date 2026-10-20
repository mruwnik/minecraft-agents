(ns engine.plan-field-test
  "jobs.lib.plan-field: every way a plan can be unworkable, and the status each gives."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.farm.field :as fld]
            [jobs.lib.plan-field :as pf]
            [jobs.lib.world-files :as world]))

(def plan {:id "p" :parts [{:id "a" :cells [[0 64 0]] :want {:crop "wheat"}}
                           {:id "b" :cells [[1 64 0]] :want "stone"}]})

(defn read [args zones & [opts]]
  (let [c {:engine {:world (world/of-data {"p" plan} {} zones)} :args args}]
    (pf/read-plan c (merge {:cells fld/crop-cells :what "crop cells" :zones? true} opts))))

(deftest a-workable-plan-gives-its-cells
  (is (= {:x 0 :y 64 :z 0} (-> (read {:plan "p" :part "a"} []) :cells keys first))))

(deftest each-trouble-has-its-text-and-whether-it-may-pass
  (are [args zones trouble later?]
       (= {:trouble trouble :later? later?} (read args zones))
    {:plan "gone"} [] "no such plan" true
    {:plan "p" :part "b"} [] "no crop cells" false
    {:plan "p" :part "a"} nil pf/no-zones true))

(deftest ignore-zones-skips-the-zone-list-and-zones-off-never-asks-for-one
  (is (:cells (read {:plan "p" :part "a" :ignore-zones? true} nil)))
  (is (:cells (read {:plan "p" :part "a"} nil {:zones? false}))))

(deftest the-status-follows-the-trouble
  (are [r kind] (= kind (first (pf/plan-can-run? r)))
    {} :ok
    {:trouble "x" :later? true} :later
    {:trouble "x" :later? false} :never))
