(ns engine.shelter-hidden-test
  "The shelter plans (retreat's pit, jobs.survival.dig-pocket's pocket) for a body that has seen only the surface (Q98): a cell
  inside rock reads stone when it lies behind seen solid faces, and the dig looks at what it laid open and stops on
  fluid or a hole that shows. The primitives are wrapped; nothing inside the rock is known."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.lib.seal :as seal]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-pocket :as dig-pocket]
            [jobs.survival.pit-cells :as pit]))

(defn stone [[x0 x1] [y0 y1] [z0 z1]]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) "stone"])))

(def ground (stone [-4 8] [50 63] [-4 4]))
(def hill (stone [3 8] [64 68] [-4 4]))
(def kit [{:name "iron_pickaxe" :count 1} {:name "cobblestone" :count 8}])
(def feet {:x 0 :y 64 :z 0})

(defn sensing [spec]
  (let [p (tu/fake (merge {:self {:pos {:x 0 :y 64 :z 0}} :inventory kit :time 14000} spec))
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})]
    (perception/wrap p per)))

(defn ^:async see-surface!
  "The body has looked about it: the top of the ground, the air over it and the face and top of the hill; nothing inside."
  [p]
  (.setOwner p "t1")
  (loop [cells (concat (for [x (range -3 3) z (range -3 4) y [63 64 65]] [x y z])
                       (for [z (range -3 4) y [64 65 66 67 68]] [3 y z])
                       (for [x (range 3 8) z (range -3 4)] [x 69 z]))]
    (when-let [[x y z] (first cells)]
      (await (.look p "t1" #js {:pos #js {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}}))
      (recur (rest cells))))
  p)

(defn seen-world [extra]
  (see-surface! (sensing {:blocks (merge ground hill extra)})))

(defn ctx-of [p] {:primitives p :view (constantly {:data {} :now 0}) :root 1 :slots [] :args {}})

(deftest a-pit-in-rock-the-body-has-not-looked-into-is-planned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (seen-world {}))
              plan (pit/pit-plan (ctx-of p) {:x -1 :y 64 :z 0})]
          (is (= 61 (:y (:to (peek (:steps plan))))) "three deep on flat ground, every cell read as stone behind the seen floor"))))))

(deftest a-seen-fluid-beside-the-pit-still-rules-it-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (seen-world {"0,63,0" "water"}))]
          (is (nil? (pit/pit-plan (ctx-of p) {:x -1 :y 64 :z 0}))))))))

(deftest a-pocket-in-the-hill-face-is-planned-on-the-faces-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (seen-world {}))]
          (is (= [{:x 3 :y 64 :z 0} {:x 3 :y 65 :z 0} {:x 4 :y 64 :z 0} {:x 4 :y 65 :z 0}] (pit/pocket-cells (ctx-of p) {:x 2 :y 64 :z 0} [1 0]))))))))

(deftest a-pocket-site-in-the-hill-is-found-on-the-faces-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (seen-world {}))]
          (is (= [{:x 3 :y 64 :z 0} {:x 3 :y 65 :z 0} {:x 4 :y 64 :z 0} {:x 4 :y 65 :z 0}] (pit/pocket-site (ctx-of p) {:x 2 :y 64 :z 0}))))))))

(deftest rock-with-a-seen-opening-over-it-is-no-pocket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (await (seen-world {"3,66,0" "air"}))]
          (is (nil? (pit/pocket-site (ctx-of p) {:x 2 :y 64 :z 0}))))))))

(defn setup [p job args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        out (atom :not-done)
        parent (assoc (tu/recording-parent job args out) :can-start? #(ctx/child-status % :kid job args))
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (.override (.-world p) "wait" (fn ^:async f [token a impl]
                                    (swap! clock + (.-ms a))
                                    (await (impl token a))))
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn ^:async tick-out! [{:keys [eng clock] :as s}]
  (loop [i 0]
    (when (and (< i 400) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn ^:async run-pocket! [extra]
  (await (tick-out! (setup (await (seen-world extra)) 'jobs.survival.dig-pocket {}))))

(defn digs [p] (mapv (juxt :x :y :z) (map #(:pos (js->clj (.-args %) :keywordize-keys true))
                                           (filter #(= "dig" (.-name %)) (.-calls (.-world p))))))

(deftest a-pocket-in-clean-rock-is-cut-and-plugged
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-pocket! {}))]
          (is (= :done (:status @out)))
          (is (= 4 (count (digs p)))))))))

(deftest water-behind-the-pocket-shows-after-the-dig-and-the-pocket-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-pocket! {"5,65,0" "water"}))]
          (is (= :stopped (:status @out)))
          (is (= :fluid-adjacent (get-in @out [:ending :data :reason])))
          (is (not-any? #{[5 65 0] [5 64 0]} (digs p))))))))
