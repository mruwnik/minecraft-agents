(ns engine.trigger-stored-args-test
  "A body's stored trigger entry may lack a key the default args gained or renamed since: every default trigger must
  judge the same with {} as with its own default args."
  (:require [cljs.test :refer [deftest is]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(def worlds
  [{:time 6000 :self {:food 4 :health 8}}
   {:time 11000 :self {:food 4}}
   {:time 11000 :floor [-200 -200 200 200] :inventory [{:name "bread" :count 1}] :self {:food 4}}
   {:time 18000 :inventory [{:name "bread" :count 1}] :self {:food 4}}
   {:time 11000 :entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]}])

(defn judge [t world args]
  (let [store (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (mem/write! store :home {:pos {:x 400 :y 64 :z 0}} mem/place-policy)
    (boolean ((:when t) (tu/fake-on-floor (merge {:floor [-10 -10 10 10]} world)) (mem/view store) args (ew/of-data {} {} []) nil))))

(deftest every-default-trigger-judges-the-same-without-its-stored-args
  (doseq [[id t] triggers/all
          world worlds
          :let [args (when (map? (:args t)) (:args t))]]
    (is (= (judge t world args) (judge t world {})) (str id " " (pr-str world)))))
