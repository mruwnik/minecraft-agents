(ns engine.settings-triggers-test
  "The trigger tuning numbers: engine.registry gathers them from triggers/, each keeps today's default and answers to a
  settings layer."
  (:require [cljs.test :refer [deftest is]]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.danger :as danger-q]
            [triggers.storage.inventory-nearly-full :as nearly-full]
            [triggers.survival.hostile-near :as hostile-near]
            [triggers.survival.hungry :as hungry]
            [triggers.survival.tidy-pending :as tidy-pending]))

(def defaults-then
  {:triggers.storage.inventory-nearly-full/free 2
   :triggers.survival.hungry/bake-wheat 3
   :triggers.survival.hungry/rest-s 600
   :triggers.survival.tidy-pending/range 48
   :triggers.survival.tidy-pending/retry-s 120})

(def all-specs
  (into {} (comp (filter (fn [[ns _]] (re-find #"^triggers\." (str ns)))) (map val) cat) registry/settings-by-ns))

(deftest the-registry-gathers-the-trigger-settings-with-their-old-defaults
  (is (= defaults-then (into {} (map (fn [[k spec]] [k (:default spec)])) all-specs))))

(deftest every-trigger-setting-is-documented
  (doseq [[k spec] all-specs]
    (is (seq (:doc spec)) (str k " has a doc"))))

(deftest a-layer-moves-the-nearly-full-threshold
  (let [full (tu/fake {:inventory (vec (repeat 34 {:name "dirt" :count 64}))})]
    (is (true? (nearly-full/inventory-nearly-full full nil {})) "2 free is nearly full by default")
    (settings/with-settings {:triggers.storage.inventory-nearly-full/free 1}
      (fn [] (is (false? (nearly-full/inventory-nearly-full full nil {})))))
    (is (false? (nearly-full/inventory-nearly-full full nil {:free 1})) "an entry's own :free still wins")))

(deftest a-layer-moves-the-accessors
  (settings/with-settings {:jobs.lib.danger/hostile-radius 5 :jobs.lib.danger/ranged-radius 6
                           :triggers.survival.hungry/bake-wheat 4 :triggers.survival.hungry/rest-s 7
                           :triggers.survival.tidy-pending/retry-s 9}
    (fn []
      (is (= [5 6] [(danger-q/hostile-radius) (danger-q/ranged-radius)]))
      (is (= [4 7] [(hungry/bake-wheat) (hungry/default-rest-s)]))
      (is (= 9 (tidy-pending/retry-s))))))

(deftest the-defaults-maps-leave-the-settings-to-the-trigger
  (is (nil? (:radius hostile-near/defaults)))
  (is (nil? (:ranged-radius hostile-near/defaults)))
  (is (nil? (:free nearly-full/defaults)))
  (is (nil? (:rest-s hungry/defaults))))
