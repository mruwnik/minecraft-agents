(ns engine.get-levels-test
  "jobs.items.get-levels and jobs.lib.xp against the fake world, its dig child a stub that pays the points."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.ready :as ready]
            [jobs.lib.xp :as xp]))

(def clock (atom 1000000))
(def pickaxe {:name "iron_pickaxe" :count 1})
(def coal "3,64,0")
(def lapis-near "4,64,0")
(def lapis-far "9,64,0")

(deftest the-level-curve-is-the-game-s
  (are [level points] (= points (xp/total-points level))
    0 0, 1 7, 2 16, 16 352, 17 394, 30 1395, 31 1507, 32 1628))

(defn level-at [points] (last (take-while #(<= (xp/total-points %) points) (range 100))))

(defn pay! [p n]
  (fake/swap-self! p (fn [s] (let [pts (+ n (get-in s [:experience :points] 0)) l (level-at pts)]
                               (assoc s :experience {:level l :points pts :progress (/ (- pts (xp/total-points l)) (xp/level-points l))})))))

(defn paying-dig
  "jobs.blocks.dig that notes the :pos and pays n points: with n above 0 the block is gone, else it stays (a dig that
  failed to break it)."
  [body dug n]
  (assoc (get registry/jobs 'jobs.blocks.dig)
         :can-start? (constantly (contract/ok :stub "a stub")) :safe? ready/ok :fit? ready/ok
         :round (fn ^:async dig [c]
                  (let [pos (:pos (:args c))]
                    (swap! dug conj pos)
                    (when (pos? n) (fake/remove-block! @body pos) (pay! @body n))
                    (ctx/end c :done nil {})))))

(defn ^:async run! [world args n]
  (let [body (atom nil) dug (atom [])
        p (tu/seeing-all (tu/fake-on-floor world))
        _ (reset! body p)
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'jobs.blocks.dig (paying-dig body dug n)) :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock) :events (events/make {:body "Fake" :sinks [] :now #(deref clock)})})
        r (await (tu/child-outcome eng 'jobs.items.get-levels (merge {:levels 1 :ways #{:carried}} args) 40))]
    [r @dug p]))

(def ores {:blocks {coal "coal_ore" lapis-near "lapis_ore" lapis-far "lapis_ore"} :inventory [pickaxe]})

(deftest the-ore-cheapest-a-point-is-dug-first-until-the-level-is-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r dug p] (await (run! ores {} 4))]
          (is (= :done (:status r)) (pr-str r))
          (is (= [[4 64 0] [9 64 0]] (mapv (juxt :x :y :z) dug)) "lapis (3.5 points) beats the nearer coal (1); the far lapis still beats it a point")
          (is (= 1 (.-level (.-experience (.self p))))))))))

(deftest a-way-that-earns-nothing-is-not-taken-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r dug] (await (run! {:blocks {"4,64,0" "lapis_ore" "5,64,0" "lapis_ore" "6,64,0" "lapis_ore" "7,64,0" "lapis_ore"}
                                        :inventory [pickaxe]} {} 0))]
          (is (= :no-way (:reason r)) (pr-str r))
          (is (= :nothing-possible (get-in r [:ending :kind])))
          (is (= 3 (count (distinct dug)) (count dug)) "each ore dug once, none again, until the one left earns too few points")
          (is (= 3 (count (:tried r)))))))))

(deftest no-way-known-waits-and-ends-condition-failed-as-a-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r dug] (await (run! {:blocks {} :inventory [pickaxe]} {} 4))]
          (is (= :no-way (:reason r)) (pr-str r))
          (is (= :condition-failed (get-in r [:ending :kind])))
          (is (empty? dug)))))))

(deftest an-ore-with-no-harvesting-tool-and-no-fetch-is-no-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r dug] (await (run! (assoc ores :inventory []) {} 4))]
          (is (= :no-way (:reason r)) (pr-str r))
          (is (empty? dug)))))))
