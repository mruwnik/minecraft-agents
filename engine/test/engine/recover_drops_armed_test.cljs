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

(defn run-with-pile
  "Like run-with-chest for any pile; fetch-stub (or nil) replaces jobs.lib.fetch/fetch!."
  [pile fetch-stub]
  (fn ^:async t []
    (let [world {:floor tu/walk-floor :entities (vec (pile-drops pile))
                 :blocks {"-2,64,3" "chest"} :containers {"-2,64,3" kit-in-chest}}
          {:keys [eng seen p]} (setup world)
          real fetch/fetch!]
      (mem/write! (:store eng) fetch/stock-kind {:pos chest-cell :items (fetch/stacks->items kit-in-chest)} fetch/stock-policy)
      (die! eng {:pos dt/death-pos :inventory pile :cause "skeleton"})
      (core/submit! eng job {})
      (set! fetch/fetch! (or fetch-stub real))
      (try (await (run-until-empty eng 60))
           (finally (set! fetch/fetch! real)))
      {:eng eng :seen seen :p p})))

(def kinds-of (fn [seen] (mapv :kind @seen)))

(deftest a-worthless-pile-does-not-send-the-body-to-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await ((run-with-pile dt/junk nil)))]
          (is (nil? (get (inv p) "iron_sword")))
          (is (= :skip (:decision (recovered eng)))))))))

(deftest a-waiting-kit-fetch-is-a-wait-not-a-decline
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom 0)
              real fetch/fetch!
              stub (fn ^:async stub-fetch [c j pr]
                     (if (= 1 (swap! calls inc)) :continue (await (real c j pr))))
              {:keys [eng seen p]} (await ((run-with-pile dt/death-pile-of-the-field-report stub)))]
          (is (< 1 @calls))
          (is (not (some #{:yielded :recover-drops.declined} (kinds-of seen))))
          (is (= 1 (get (inv p) "iron_sword")))
          (is (not= :skip (:decision (recovered eng)))))))))

(deftest a-fetch-that-ends-the-job-drops-the-kit-and-the-trip-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await ((run-with-pile dt/death-pile-of-the-field-report (fn ^:async stub-fetch [c j pr] :done))))]
          (is (some #{:recover-drops.kit-dropped} (kinds-of seen)))
          (is (some? (recovered eng))))))))
