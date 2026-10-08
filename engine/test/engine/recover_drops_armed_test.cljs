(ns engine.recover-drops-armed-test
  "recover-drops against a killer mob with an unarmed body: a weapon and armour a known chest holds are fetched first."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.death-test :as dt :refer [die! job recovered pile-drops]]
            [engine.library-test :refer [setup inv]]
            [engine.recover-drops-danger-test :refer [decided-text]]
            [engine.test-util :as tu :refer [run-until-empty]]
            [jobs.lib.fetch :as fetch]
            [engine.memory :as mem]))

(def chest-cell [-2 64 3])

(def kit-in-chest [{:name "iron_sword" :count 1} {:name "iron_helmet" :count 1}])

(defn worn [p]
  (into {} (keep (fn [[k v]] (when (and v (#{"head" "torso" "legs" "feet"} k)) [k (get v "name")])))
        (js->clj (.-equipment (.self p)))))

(defn run-with-chest
  "Die to cause with the field-report pile; a chest holds items (nil: no chest). known?: the body booked its stock."
  [items known? cause]
  (fn ^:async t []
    (let [pile dt/death-pile-of-the-field-report
          world (cond-> {:floor tu/walk-floor :entities (vec (pile-drops pile))}
                  items (assoc :blocks {"-2,64,3" "chest"} :containers {"-2,64,3" items}))
          {:keys [eng seen p]} (setup world)]
      (when known? (mem/write! (:store eng) fetch/stock-kind {:pos chest-cell :items (fetch/stacks->items items)} fetch/stock-policy))
      (die! eng {:pos dt/death-pos :inventory pile :cause cause})
      (core/submit! eng job {})
      (await (run-until-empty eng 60))
      {:eng eng :seen seen :p p})))

(deftest an-unarmed-body-fetches-a-sword-and-a-helmet-before-the-skeleton-trip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (await ((run-with-chest kit-in-chest true "skeleton")))]
          (is (= 1 (get (inv p) "iron_sword")))
          (is (= "iron_helmet" (get (worn p) "head")))
          (is (re-find #"^fetch: " (decided-text seen)))
          (is (not= :skip (:decision (recovered eng)))))))))

(deftest no-chest-leaves-the-unarmed-skip-unchanged
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await ((run-with-chest nil false "skeleton")))]
          (is (= :skip (:decision (recovered eng))))
          (is (nil? (get (inv p) "iron_sword"))))))))

(deftest a-chest-whose-stock-is-unknown-is-not-searched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await ((run-with-chest kit-in-chest false "skeleton")))]
          (is (= :skip (:decision (recovered eng))))
          (is (nil? (get (inv p) "iron_sword"))))))))

(deftest a-fall-calls-for-no-kit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await ((run-with-chest kit-in-chest true "fall")))]
          (is (nil? (get (inv p) "iron_sword"))))))))
