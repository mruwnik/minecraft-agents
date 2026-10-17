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

(def all-specs
  (into {} (comp (filter (fn [[ns _]] (re-find #"^triggers\." (str ns)))) (map val) cat) registry/settings-by-ns))

(deftest every-trigger-setting-is-documented
  (doseq [[k spec] all-specs]
    (is (seq (:doc spec)) (str k " has a doc"))))

(deftest a-layer-moves-the-nearly-full-threshold
  (let [full (tu/fake {:inventory (vec (repeat 34 {:name "dirt" :count 64}))})]
    (is (true? (nearly-full/inventory-nearly-full full nil {})) "2 free is nearly full by default")
    (settings/with-settings {:triggers.storage.inventory-nearly-full/free 1}
      (fn [] (is (false? (nearly-full/inventory-nearly-full full nil {})))))
    (is (false? (nearly-full/inventory-nearly-full full nil {:free 1})) "an entry's own :free still wins")))

(defn rested [now t] {:now now :data {:entries {:make-room-rest [{:t t :data {}}]}}})

(deftest nearly-full-rests-after-make-room-tossed
  (let [full (tu/fake {:inventory (vec (repeat 34 {:name "dirt" :count 64}))})]
    (is (false? (nearly-full/inventory-nearly-full full (rested 100000 50000) {})) "50 s after a toss: resting")
    (is (true? (nearly-full/inventory-nearly-full full (rested 200000 50000) {})) "150 s after: fires again")
    (is (true? (nearly-full/inventory-nearly-full full (rested 100000 50000) {:rest-s 10})) "the entry's :rest-s wins")
    (settings/with-settings {:triggers.storage.inventory-nearly-full/rest-s 300}
      (fn [] (is (false? (nearly-full/inventory-nearly-full full (rested 200000 50000) {})))))))

(defn gave-up [now t]
  {:now now :data {:entries {:job-report [{:t t :data {:job 'jobs.survival.get-food :kind :nothing-possible :cause :no-food}}]}}})

(deftest hungry-rests-after-a-give-up-but-a-starving-body-only-a-minute
  (let [at (fn [food health] (tu/fake {:self {:food food :health health}}))]
    (is (not (hungry/hungry (at 3 10) (gave-up 100000 50000) {})) "hungry, 50 s: resting")
    (is (= :urgent (hungry/hungry (at 0 10) (gave-up 120000 50000) {})) "food 0, 70 s after: fires again")
    (is (not (hungry/hungry (at 0 10) (gave-up 100000 50000) {})) "food 0, 50 s: still resting")
    (is (hungry/hungry (at 3 5) (gave-up 120000 50000) {}) "losing health while hungry: 70 s is enough")
    (is (not (hungry/hungry (at 6 15) (gave-up 120000 50000) {})) "hungry but healthy, 70 s: still resting")))

(defn refused [now t]
  {:now now :data {:entries {:food-too-dangerous [{:t t :data {:why :creeper}}]}}})

(deftest hungry-rests-after-get-food-refused-on-danger-only-while-fragile
  (let [at (fn [food health] (tu/fake {:self {:food food :health health}}))]
    (is (not (hungry/hungry (at 3 5) (refused 100000 50000) {})) "fragile, 50 s after a refusal: resting")
    (is (hungry/hungry (at 3 5) (refused 700000 50000) {}) "650 s after: fires again")
    (is (hungry/hungry (at 3 15) (refused 100000 50000) {}) "healed: the refusal no longer holds it")))

(deftest a-layer-moves-the-accessors
  (settings/with-settings {:jobs.lib.danger/hostile-radius 5 :jobs.lib.danger/ranged-radius 6
                           :triggers.survival.hungry/bake-wheat 4 :triggers.survival.hungry/rest-s 7
                           :triggers.survival.tidy-pending/retry-s 9}
    (fn []
      (is (= [5 6] [(danger-q/hostile-radius) (danger-q/ranged-radius)]))
      (is (= [4 7] [(hungry/bake-wheat) (hungry/default-rest-s)]))
      (is (= 9 (tidy-pending/retry-s))))))

