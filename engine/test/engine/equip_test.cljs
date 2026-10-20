(ns engine.equip-test
  "Holding an item and wearing armour: jobs.items.equip and jobs.lib.armour against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [jobs.lib.armour :as armour]
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
          (is (= {:status :done :item "iron_pickaxe" :slot "hand" :worn [{:item "iron_pickaxe" :slot "hand"}] :text "holding iron_pickaxe"} @out)))))))

(deftest holds-a-carried-item-in-the-off-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (equip (stacks "shield") {:item "shield" :hand "off"}))]
          (is (= "shield" (off-hand p)))
          (is (nil? (held p)))
          (is (= {:status :done :item "shield" :slot "off-hand" :worn [{:item "shield" :slot "off-hand"}] :text "holding shield"} @out)))))))

(deftest a-shield-goes-to-the-off-hand-without-being-told
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (equip (stacks "shield") {:item "shield"}))]
          (is (= "shield" (off-hand p))))))))

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
          (is (= {:status :done :already? true :item "iron_pickaxe" :slot "hand" :worn [] :text "holding iron_pickaxe"} @out)))))))

(deftest a-missing-item-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (equip (stacks "dirt") {:item "iron_pickaxe" :ways #{:carried}}))]
          (is (= [:condition-failed :no-item] [(:kind (:ending @out)) (:reason @out)]))
          (is (nil? (held p))))))))

(deftest an-unknown-item-name-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng]} (await (equip (stacks "dirt") {:item "iron_pickaxx"}))]
          (is (= :impossible (get-in @out [:ending :kind])))
          (is (empty? (:list (core/state eng))) "impossible: it leaves the queue"))))))

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
          (core/submit! (:eng s) (list job {:item "wooden_pickaxe" :ways #{:carried}}) {})
          (await (ft/run-ticks s 10))
          (is (nil? (held (:p s))))
          (is (empty? (filter #(= :need (:slot %)) (ft/events-of s :child_started))))
          (is (empty? (ft/inspects s)))
          (is (seq (ft/chest-items s [-2 64 3])))
          (is (empty? (ft/events-of s :completed)))
          (is (seq (ft/listed s)) "the job waits on its condition"))))))

(defn worn [p] (into {} (keep (fn [[k v]] (when (and v (#{"head" "torso" "legs" "feet"} k)) [k (get v "name")]))) (js->clj (.-equipment (.self p)))))
(defn carried [p] (set (map #(.-name %) (.-inventory (.self p)))))

(deftest plan-ranks-materials-and-picks-the-slot
  (are [item slot] (= slot (:slot (armour/parse item)))
    "iron_helmet" "head" "diamond_chestplate" "torso" "golden_leggings" "legs" "netherite_boots" "feet" "turtle_helmet" "head")
  (are [item] (nil? (armour/parse item))
    "dirt" "iron_pickaxe" "turtle_boots" "shield" nil)
  (is (apply <= (map armour/rank-of ["leather_helmet" "golden_helmet" "turtle_helmet" "chainmail_helmet" "iron_helmet" "diamond_helmet" "netherite_helmet"]))
      "by armour points, then toughness")
  (is (= (armour/rank-of "golden_helmet") (armour/rank-of "turtle_helmet") (armour/rank-of "iron_helmet"))
      "helmets of equal points tie: a worn one is not swapped")
  (is (< (armour/rank-of "golden_leggings") (armour/rank-of "chainmail_leggings") (armour/rank-of "iron_leggings")))
  (is (< (armour/rank-of "diamond_boots") (armour/rank-of "netherite_boots")) "toughness breaks the points tie"))

(deftest plan-without-item-takes-the-best-for-empty-or-worse-slots
  (is (= [{:item "diamond_helmet" :slot "head"} {:item "iron_boots" :slot "feet"}]
         (armour/plan {"torso" "iron_chestplate" "legs" "iron_leggings"}
                      ["iron_helmet" "diamond_helmet" "iron_chestplate" "leather_boots" "iron_boots"])))
  (is (= [] (armour/plan {"head" "diamond_helmet"} ["iron_helmet" "dirt"]))))

(deftest wearing-an-iron-set-fills-every-slot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (equip (stacks "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots" "dirt") {}))]
          (is (= {"head" "iron_helmet" "torso" "iron_chestplate" "legs" "iron_leggings" "feet" "iron_boots"} (worn p)))
          (is (= #{"dirt"} (carried p)))
          (is (= 4 (count (:worn @out)))))))))

(deftest the-best-piece-is-chosen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (equip (stacks "leather_helmet" "diamond_helmet" "iron_helmet" "golden_helmet") {}))]
          (is (= {"head" "diamond_helmet"} (worn p)))
          (is (= #{"leather_helmet" "iron_helmet" "golden_helmet"} (carried p))))))))

(deftest a-worn-piece-goes-back-to-the-pockets-when-replaced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (equip (assoc (stacks "diamond_helmet") :equipment {:head {:name "iron_helmet" :count 1}}) {}))]
          (is (= {"head" "diamond_helmet"} (worn p)))
          (is (= #{"iron_helmet"} (carried p))))))))

(deftest a-named-armour-piece-goes-to-its-slot-and-a-worn-one-comes-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (equip (assoc (stacks "iron_helmet") :equipment {:head {:name "leather_helmet" :count 1}}) {:item "iron_helmet"}))]
          (is (= {"head" "iron_helmet"} (worn p)))
          (is (= #{"leather_helmet"} (carried p)))
          (is (= [{:item "iron_helmet" :slot "head"}] (:worn @out))))))))

(deftest equipping-armour-quotes-a-time-per-piece
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [q (await ((:quote-of (get registry/jobs job)) {:primitives (tu/fake (stacks "iron_helmet" "iron_boots")) :args {}}))]
          (is (nil? (contract/quote-problem q)))
          (is (pos? (second (get-in q [:uses :job-time])))))))))
