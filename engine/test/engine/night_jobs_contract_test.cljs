(ns engine.night-jobs-contract-test
  "jobs.survival.dig-niche, sleep, go-home and set-spawn as contract jobs: what each way of ending a call is, read through ctx/call."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.dig-niche-test :as niche]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn ^:async call!
  "Run job with args as a ctx/call child of a recording parent over fake body p; prep gets the engine first. Its ending."
  [p job args prep]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call c :kid job args))]
                           (if (= :continue r) r (do (reset! out r) :done))))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (st/dawn-after! p st/default-dawn)
    (prep eng)
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng 40))
    @out))

(defn ending-of [e] [(:kind e) (contract/cause-key e)])

(defn spawn-world [extra] (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor :time st/night} extra)))

(deftest a-hill-niche-ends-done-with-its-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:offlineScale 0.0001 :time 14000 :blocks (merge niche/ground niche/hill)
                                         :drops {"stone" "cobblestone"} :inventory [{:name "iron_pickaxe" :count 1}]}))
              e (await (call! p 'jobs.survival.dig-niche {} (constantly nil)))]
          (is (= :done (:kind e)))
          (is (= 2 (count (:door (:data e))))))))))

(deftest flat-ground-has-no-niche-site-a-fact
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:offlineScale 0.0001 :time 14000 :blocks niche/ground :inventory [{:name "iron_pickaxe" :count 1}]}))
              e (await (call! p 'jobs.survival.dig-niche {} (constantly nil)))]
          (is (= [:condition-failed :no-site] (ending-of e))))))))

(deftest no-block-to-plug-the-niche-is-a-fact
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:offlineScale 0.0001 :time 14000 :blocks (merge niche/ground niche/hill)
                                         :inventory [{:name "iron_pickaxe" :count 1}]}))
              e (await (call! p 'jobs.survival.dig-niche {:blocks ["dirt"]} (constantly nil)))]
          (is (= [:condition-failed :no-blocks] (ending-of e))))))))

(deftest a-click-that-does-not-set-the-spawn-is-a-failed-try
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (spawn-world {:time st/noon :blocks {"6,64,0" "respawn_anchor"}})
              e (await (call! p 'jobs.survival.set-spawn {:bed {:x 6 :y 64 :z 0}} (constantly nil)))]
          (is (= [:nothing-possible :not-set] (ending-of e))))))))

(deftest set-spawn-with-no-bed-is-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:time st/noon}) 'jobs.survival.set-spawn {} (constantly nil)))]
          (is (= [:condition-failed :no-bed] (ending-of e))))))))

(deftest go-home-beyond-its-limit-is-a-fact
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:time st/noon}) 'jobs.survival.go-home {}
                              #(mem/write! (:store %) :home {:pos {:x 300 :y 64 :z 0}} mem/place-policy)))]
          (is (= [:condition-failed :too-far] (ending-of e)))
          (is (= 300 (:x (:pos (:cause e))))))))))

(deftest go-home-arriving-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:time st/noon}) 'jobs.survival.go-home {}
                              #(mem/write! (:store %) :home {:pos {:x 12 :y 64 :z 0}} mem/place-policy)))]
          (is (= :done (:kind e)))
          (is (= {:x 12 :y 64 :z 0} (:home (:data e)))))))))

(deftest sleep-by-day-is-condition-failed-not-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:time st/noon :blocks {"6,64,0" "red_bed"}}) 'jobs.survival.sleep {:bed {:x 6 :y 64 :z 0}} (constantly nil)))]
          (is (= [:condition-failed :not-night] (ending-of e))))))))

(deftest a-bed-in-the-open-is-condition-failed-when-open-beds-are-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:blocks {"6,64,0" "red_bed"}}) 'jobs.survival.sleep {:bed {:x 6 :y 64 :z 0} :open-bed false} (constantly nil)))]
          (is (= [:condition-failed :bed-open] (ending-of e))))))))

(deftest dig-niche-by-day-is-condition-failed-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:offlineScale 0.0001 :time st/noon :blocks (merge niche/ground niche/hill)
                                         :inventory [{:name "iron_pickaxe" :count 1}]}))
              e (await (call! p 'jobs.survival.dig-niche {} (constantly nil)))]
          (is (= [:condition-failed :day] (ending-of e))))))))

(deftest a-roof-over-an-open-field-is-not-a-shelter-for-dig-niche
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:offlineScale 0.0001 :time 14000
                                         :blocks (merge niche/ground niche/hill (niche/blocks "oak_leaves" [-2 3] [67 67] [-3 3]))
                                         :drops {"stone" "cobblestone"} :inventory [{:name "iron_pickaxe" :count 1}]}))
              e (await (call! p 'jobs.survival.dig-niche {} (constantly nil)))]
          (is (= :done (:kind e)))
          (is (= 2 (count (:door (:data e)))) "it cut a niche, the canopy did not count as shelter"))))))

(deftest set-spawn-on-a-bed-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:time st/noon :blocks {"6,64,0" "red_bed"}}) 'jobs.survival.set-spawn
                              {:bed {:x 6 :y 64 :z 0}} (constantly nil)))]
          (is (= :done (:kind e))))))))

(deftest sleep-at-night-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:blocks {"6,64,0" "red_bed"}}) 'jobs.survival.sleep {:bed {:x 6 :y 64 :z 0}} (constantly nil)))]
          (is (= :done (:kind e)))
          (is (= {:x 6 :y 64 :z 0} (:pos (:data e)))))))))

(deftest a-bed-no-walk-reaches-is-nothing-possible-bed-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:blocks {"6,90,0" "red_bed"}}) 'jobs.survival.sleep {:bed {:x 6 :y 90 :z 0}} (constantly nil)))]
          (is (= [:nothing-possible :bed-unreachable] (ending-of e))))))))

(deftest a-failed-go-home-walk-carries-go-tos-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (call! (spawn-world {:time st/noon :blocks {"12,90,0" "red_bed"}}) 'jobs.survival.go-home {}
                              #(mem/write! (:store %) :home {:pos {:x 12 :y 90 :z 0}} mem/place-policy)))]
          (is (= [:nothing-possible :walk-failed] (ending-of e)))
          (is (keyword? (:walk (:cause e))) "go-to's cause rides along"))))))

(deftest a-listed-sleep-beside-an-open-bed-waits-bed-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:blocks {"1,64,0" "red_bed"} :time st/night})]
          (core/submit! eng '(jobs.survival.sleep {:bed {:x 1 :y 64 :z 0} :open-bed false}) {})
          (await (tu/run-until-empty eng 5))
          (is (= [:bed-open] (distinct (map :reason (filter #(= :waiting (:kind %)) @seen)))))
          (is (empty? (filter #(= :stopped (:kind %)) @seen)) "it waits, it does not end"))))))
