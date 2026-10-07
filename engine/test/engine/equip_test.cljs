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
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
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
          (is (= {:status :done :item "iron_pickaxe" :hand "main"} @out)))))))

(deftest holds-a-carried-item-in-the-off-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (equip (stacks "shield") {:item "shield" :hand "off"}))]
          (is (= "shield" (off-hand p)))
          (is (nil? (held p)))
          (is (= {:status :done :item "shield" :hand "off"} @out)))))))

(deftest an-item-already-in-hand-is-done-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (equip (assoc (stacks "iron_pickaxe") :self {:held "iron_pickaxe"}) {:item "iron_pickaxe"}))]
          (is (= {:status :done :item "iron_pickaxe" :hand "main" :held true} @out)))))))

(deftest a-missing-item-is-refused-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out seen]} (await (equip (stacks "dirt") {:item "iron_pickaxe" :fetch false}))]
          (is (= {:status :stopped :reason :no-item :item "iron_pickaxe"} (select-keys @out [:status :reason :item])))
          (is (nil? (held p)))
          (is (some #(re-find #"equip.refused" %) (tu/kinds seen))))))))

(deftest an-unknown-item-name-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (equip (stacks "dirt") {:item "iron_pickaxx"}))]
          (is (= :unknown-item (:reason @out)))
          (is (= :stopped (:status @out))))))))

(deftest a-bad-hand-is-refused-at-submit
  (is (re-find #"jobs.items.equip :hand must be one of \"main\", \"off\", got \"foot\""
               (expr/problem registry/jobs '(jobs.items.equip {:item "dirt" :hand "foot"})))))

(deftest a-default-call-gets-a-missing-item-from-a-seen-chest-then-holds-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ft/start (ft/world ft/pickaxe-stock) [])]
          (core/submit! (:eng s) (list job {:item "wooden_pickaxe"}) {})
          (await (ft/run-ticks s 40))
          (is (= "wooden_pickaxe" (held (:p s))))
          (is (= 1 (count (ft/events-of s :fetch.done))))
          (is (= ["wooden_pickaxe"] (mapv :item (ft/events-of s :equip.done))))
          (is (empty? (ft/chest-items s [-2 64 3]))))))))

(deftest fetch-with-nothing-to-get-fails-and-the-job-waits-with-the-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ft/start (ft/world {}) [])
              id (core/submit! (:eng s) (list job {:item "wooden_pickaxe"}) {})]
          (await (ft/run-ticks s 40))
          (is (nil? (held (:p s))))
          (is (= 1 (count (ft/events-of s :fetch.failed))))
          (is (empty? (ft/events-of s :equip.done)))
          (is (empty? (ft/events-of s :equip.refused)) "it waits for the item, it does not stop :no-item")
          (is (= 1 (count (:list (core/state (:eng s))))) "the job is still queued, not finished")
          (let [w (core/waiting (:eng s) id)]
            (is (= :need (:reason w)))
            (is (= "wooden_pickaxe" (:item w)))
            (is (some? (get-in w [:fetch :failed])))))))))

(deftest fetch-false-opts-out-and-the-round-stops-no-item-without-touching-the-chest
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
          (is (= [:no-item] (mapv :reason (ft/events-of s :equip.refused))))
          (is (empty? (ft/listed s))))))))
