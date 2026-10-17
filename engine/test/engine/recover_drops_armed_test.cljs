(ns engine.recover-drops-armed-test
  "recover-drops against a killer mob with an unarmed body: a weapon and armour a known chest holds are fetched first."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.death-test :as dt :refer [die! job recovered pile-drops]]
            [engine.library-test :refer [setup inv]]
            [engine.recover-drops-danger-test :refer [decided-text]]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.contract :as contract]
            [jobs.lib.stock :as stock]
            [jobs.lib.need :as need]
            [engine.memory :as mem]))

(def chest-cell [-2 64 3])

(def arity4 "need/blocks! callers use its 4-arity, which the compiler calls directly" "cljs$core$IFn$_invoke$arity$4")

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
      (when known? (mem/write! (:store eng) stock/stock-kind {:pos chest-cell :items (stock/stacks->items items)} stock/stock-policy))
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
          (is (nil? (recovered eng)) "a danger skip stays open")
          (is (nil? (get (inv p) "iron_sword"))))))))

(deftest a-chest-whose-stock-is-unknown-is-not-searched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await ((run-with-chest kit-in-chest false "skeleton")))]
          (is (nil? (recovered eng)) "a danger skip stays open")
          (is (nil? (get (inv p) "iron_sword"))))))))

(deftest a-fall-calls-for-no-kit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await ((run-with-chest kit-in-chest true "fall")))]
          (is (nil? (get (inv p) "iron_sword"))))))))

(defn run-with-pile
  "Like run-with-chest for any pile; fetch-stub (or nil) replaces jobs.lib.need/blocks!."
  [pile fetch-stub]
  (fn ^:async t []
    (let [world {:floor tu/walk-floor :entities (vec (pile-drops pile))
                 :blocks {"-2,64,3" "chest"} :containers {"-2,64,3" kit-in-chest}}
          {:keys [eng seen p]} (setup world)
          real (aget need/blocks! arity4)]
      (mem/write! (:store eng) stock/stock-kind {:pos chest-cell :items (stock/stacks->items kit-in-chest)} stock/stock-policy)
      (die! eng {:pos dt/death-pos :inventory pile :cause "skeleton"})
      (core/submit! eng job {})
      (aset need/blocks! arity4 (or fetch-stub real))
      (try (await (run-until-empty eng 60))
           (finally (aset need/blocks! arity4 real)))
      {:eng eng :seen seen :p p})))

(def kinds-of (fn [seen] (mapv :kind @seen)))

(deftest a-worthless-pile-does-not-send-the-body-to-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await ((run-with-pile dt/junk nil)))]
          (is (nil? (get (inv p) "iron_sword")))
          (is (= :skip (:decision (recovered eng))) "a skip for value is final"))))))

(deftest a-waiting-kit-fetch-is-a-wait-not-a-decline
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom 0)
              real (aget need/blocks! arity4)
              stub (fn ^:async stub-fetch [c names n opts]
                     (if (= 1 (swap! calls inc)) :continue (await (real c names n opts))))
              {:keys [eng seen p]} (await ((run-with-pile dt/death-pile-of-the-field-report stub)))]
          (is (< 1 @calls))
          (is (not (some #{:yielded} (kinds-of seen))))
          (is (not-any? #(and (= :stopped (:kind %)) (#{:danger :unreachable} (:reason %))) @seen) "never declined")
          (is (= 1 (get (inv p) "iron_sword")))
          (is (not= :skip (:decision (recovered eng)))))))))

(deftest a-fetch-that-fails-drops-the-kit-and-the-trip-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await ((run-with-pile dt/death-pile-of-the-field-report (fn ^:async stub-fetch [c names n opts] (contract/ending :nothing-possible :failed nil)))))]
          (is (some #{:recover-drops.kit-dropped} (kinds-of seen)))
          (is (some #{:recover-drops.decided} (kinds-of seen))))))))

(deftest the-kit-fetch-is-limited-to-the-minutes-left-in-the-despawn-window
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts (atom nil)
              real (aget need/blocks! arity4)
              stub (fn ^:async stub-fetch [c names n o] (reset! opts o) (await (real c names n o)))]
          (await ((run-with-pile dt/death-pile-of-the-field-report stub)))
          (is (number? (:minutes @opts)) "the fetch carries a time limit")
          (is (<= (:minutes @opts) 3) "never above the configured limit")
          (is (< 0 (:minutes @opts) 5) "within the 5 minute window"))))))
