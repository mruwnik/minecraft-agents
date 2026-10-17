(ns engine.equip-test
  "Holding an item: jobs.items.equip against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fetch-test :as ft]
            [engine.fake :as fake]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def job 'jobs.items.equip)

(defn stacks [& names] {:inventory (mapv (fn [n] {:name n :count 1}) names)})

(defn setup
  "An engine over the fake world; a recording parent runs the job and puts the child's result in :out."
  [world args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        out (atom :not-done)
        parent (tu/recording-parent job args out)
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn ^:async equip [world args]
  (let [s (setup world args)]
    (dotimes [_ 6]
      (await (core/tick! (:eng s)))
      (swap! (:clock s) + 500))
    s))

(defn held [p] (some-> (.-equipment (.self p)) .-mainHand .-name))
(defn off-hand [p] (some-> (.-equipment (.self p)) .-offHand .-name))

(deftest holds-a-carried-item-in-the-main-hand-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (equip (stacks "stone_pickaxe" "iron_pickaxe") {:item "iron_pickaxe"}))]
          (is (= "iron_pickaxe" (held p)))
          (is (= {:status :done :item "iron_pickaxe" :hand "main" :text "holding iron_pickaxe"} @out)))))))

(deftest holds-a-carried-item-in-the-off-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (equip (stacks "shield") {:item "shield" :hand "off"}))]
          (is (= "shield" (off-hand p)))
          (is (nil? (held p)))
          (is (= {:status :done :item "shield" :hand "off" :text "holding shield"} @out)))))))

(deftest the-hand-takes-a-string-or-a-keyword
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [hand ["off" :off]]
          (let [{:keys [p]} (await (equip (stacks "shield") {:item "shield" :hand hand}))]
            (is (= "shield" (off-hand p)) (pr-str hand))))))))

(deftest an-item-already-in-hand-is-done-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (equip (assoc (stacks "iron_pickaxe") :self {:held "iron_pickaxe"}) {:item "iron_pickaxe"}))]
          (is (= {:status :done :already? true :item "iron_pickaxe" :hand "main" :text "holding iron_pickaxe"} @out)))))))

(deftest a-missing-item-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (equip (stacks "dirt") {:item "iron_pickaxe" :fetch false}))]
          (is (= [:condition-failed :no-item] [(:kind (:ending @out)) (:reason @out)]))
          (is (nil? (held p))))))))

(deftest an-unknown-item-name-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng]} (await (equip (stacks "dirt") {:item "iron_pickaxx"}))]
          (is (= :impossible (get-in @out [:ending :kind])))
          (is (empty? (:list (core/state eng))) "impossible: it leaves the queue"))))))

(deftest no-item-given-is-refused-with-a-clear-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (equip (stacks "dirt") {}))]
          (is (= [:impossible :bad-args] [(get-in @out [:ending :kind]) (:reason @out)]) "refused before it starts"))))))

(deftest a-bad-hand-is-refused-at-submit
  (is (re-find #"jobs.items.equip :hand must be one of .*\"off\".*got \"foot\""
               (expr/problem registry/jobs '(jobs.items.equip {:item "dirt" :hand "foot"})))))

(deftest a-default-call-gets-a-missing-item-from-a-seen-chest-then-holds-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ft/start (ft/world ft/pickaxe-stock) [])]
          (core/submit! (:eng s) (list job {:item "wooden_pickaxe"}) {})
          (await (ft/run-ticks s 40))
          (is (= "wooden_pickaxe" (held (:p s))))
          (is (= ["holding wooden_pickaxe"] (mapv :text (ft/events-of s :completed))))
          (is (empty? (ft/chest-items s [-2 64 3]))))))))

(deftest fetch-with-nothing-to-get-ends-the-call-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ft/start (ft/world {}) [])]
          (core/submit! (:eng s) (list job {:item "wooden_pickaxe"}) {})
          (await (ft/run-ticks s 40))
          (is (nil? (held (:p s))))
          (is (empty? (ft/events-of s :completed)))
          (is (empty? (:list (core/state (:eng s)))) "the call ended, it does not wait for the item"))))))

(deftest fetch-false-opts-out-and-the-job-waits-without-touching-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ft/start (ft/world ft/pickaxe-stock) [])]
          (core/submit! (:eng s) (list job {:item "wooden_pickaxe" :fetch false}) {})
          (await (ft/run-ticks s 10))
          (is (nil? (held (:p s))))
          (is (empty? (ft/events-of s :fetch.started)))
          (is (empty? (ft/inspects s)))
          (is (seq (ft/chest-items s [-2 64 3])))
          (is (empty? (ft/events-of s :completed)))
          (is (seq (ft/listed s)) "the job waits on its condition"))))))
