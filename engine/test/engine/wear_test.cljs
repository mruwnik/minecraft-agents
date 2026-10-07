(ns engine.wear-test
  "Wearing armour: jobs.lib.armour's plan and jobs.items.wear against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [jobs.lib.armour :as armour]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def job 'jobs.items.wear)

(defn stacks [& names] {:inventory (mapv (fn [n] {:name n :count 1}) names)})

(def iron-set ["iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

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

(defn ^:async wear [world args]
  (let [s (setup world args)]
    (dotimes [_ 6]
      (await (core/tick! (:eng s)))
      (swap! (:clock s) + 500))
    s))

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
         (:put (armour/plan {"torso" "iron_chestplate" "legs" "iron_leggings"}
                            ["iron_helmet" "diamond_helmet" "iron_chestplate" "leather_boots" "iron_boots"] nil))))
  (is (= [] (:put (armour/plan {"head" "diamond_helmet"} ["iron_helmet" "dirt"] nil)))))

(deftest wearing-an-iron-set-fills-every-slot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (wear (stacks "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots" "dirt") {}))]
          (is (= {"head" "iron_helmet" "torso" "iron_chestplate" "legs" "iron_leggings" "feet" "iron_boots"} (worn p)))
          (is (= #{"dirt"} (carried p)))
          (is (= 4 (count (:worn @out)))))))))

(deftest the-best-piece-is-chosen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (wear (stacks "leather_helmet" "diamond_helmet" "iron_helmet" "golden_helmet") {}))]
          (is (= {"head" "diamond_helmet"} (worn p)))
          (is (= #{"leather_helmet" "iron_helmet" "golden_helmet"} (carried p))))))))

(deftest a-worn-piece-goes-back-to-the-pockets-when-replaced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (wear (assoc (stacks "diamond_helmet") :equipment {:head {:name "iron_helmet" :count 1}}) {}))]
          (is (= {"head" "diamond_helmet"} (worn p)))
          (is (= #{"iron_helmet"} (carried p))))))))

(deftest a-named-non-armour-item-is-refused-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (wear (stacks "dirt") {:item "dirt"}))]
          (is (= {:worn [] :reason "not-armour"} @out))
          (is (empty? (worn p))))))))

(deftest a-missing-item-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (wear (stacks "dirt") {:item "iron_helmet"}))]
          (is (= {:worn [] :reason "no-item"} @out)))))))
