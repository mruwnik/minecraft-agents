(ns engine.obtain-fish-test
  "Fish as a gathered need of items.obtain: raw cod and salmon are caught with a rod (jobs.food.fish), also for the
  furnace step of a cooked fish."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fish-test :as ft]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [engine.core :as core]
            [engine.game :as game]
            [engine.sense-test :as sense]
            [jobs.items.obtain :as obtain]
            [jobs.items.obtain.seen :as seen]
            [jobs.items.obtain.table :as table]
            [jobs.items.recipes :as recipes]))

(defn ^:async obtain [world args]
  (let [{:keys [eng p]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.items.obtain args 200))]
    {:res res :p p}))

(deftest raw-cod-is-caught-with-a-rod
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (obtain (ft/world [ft/rod] {:catch ["cod"]}) {:item "cod" :count 2}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 2 (ft/carried p "cod"))))))))

(deftest no-rod-no-fish-says-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (obtain (ft/world [] {}) {:item "cod" :count 1}))]
          (is (= :stopped (:status res)))
          (is (= :no-source (:reason res))))))))

(deftest no-water-seen-looks-round-before-obtain-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup (assoc (ft/world [ft/rod] {}) :blocks {}))]
          (core/submit! eng '(jobs.items.obtain {:item "cod" :count 1}) {})
          (await (tu/run-until-empty eng 6))
          (is (pos? (sense/looks p))))))))

(deftest a-cooked-fish-plan-gathers-the-raw-fish
  (is (= {"cod" 2} (:gather (recipes/plan game/default-version {} "cooked_cod" 2 (merge (table/source-opts) {:gather? true :table? true}))))))

(deftest the-fish-gather-carries-the-shortfall-on-top-of-all-fish-carried
  (let [p (tu/fake {:inventory [{:name "salmon" :count 2} {:name "cod" :count 1}]})
        need (first (table/gather-needs {"cod" 2}))]
    (is (= 5 (get-in (seen/with-count p need) [:args :count])))))
