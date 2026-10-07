(ns engine.dig-in-seen-test
  "dig-in's pit reads through perception (Q98): a cell inside rock is unknown until the body looks; it looks at what each
  dig laid open and refuses a floor, fluid or air it sees before it steps into the hole. The primitives are wrapped."
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
            [jobs.lib.world-files :as world]))

(defn stone [x0 x1 y0 y1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) "stone"])))

(def ground (stone -4 4 50 64 -4 4))
(def kit [{:name "iron_pickaxe" :count 1} {:name "cobblestone" :count 8}])

(defn sensing [spec]
  (let [p (tu/fake (merge {:self {:pos {:x 0 :y 65 :z 0}} :inventory kit :time 14000} spec))
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})]
    (perception/wrap p per)))

(defn setup [p job args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        out (atom :not-done)
        parent {:check (fn [c] (boolean (ctx/check-child c :kid job args)))
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
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

(defn ^:async see-surface!
  "The body has looked about it from the surface: the top of the ground and the air over it, nothing inside the rock."
  [p]
  (.setOwner p "t1")
  (loop [cells (for [x (range -3 4) z (range -3 4) y [64 65]] [x y z])]
    (when-let [[x y z] (first cells)]
      (await (.look p "t1" #js {:pos #js {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}}))
      (recur (rest cells))))
  p)

(defn ^:async run-pit! [spec]
  (await (tick-out! (setup (await (see-surface! (sensing spec))) 'jobs.survival.dig-in {}))))

(defn calls [p kind] (mapv #(js->clj (.-args %) :keywordize-keys true)
                           (filter #(= kind (.-name %)) (.-calls (.-world p)))))
(defn digs [p] (mapv (juxt :x :y :z) (map :pos (calls p "dig"))))
(defn feet-y [p] (js/Math.floor (.-y (.-pos (.self p)))))
(defn failed [{:keys [seen]}] (filter #(= :dig_in_failed (:kind %)) @seen))

(deftest a-pit-over-clean-rock-is-dug-and-roofed-on-what-the-body-saw-of-the-surface
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (run-pit! {:blocks ground}))]
          (is (= [[0 64 0] [0 63 0] [0 62 0]] (digs p)))
          (is (= 62 (feet-y p)))
          (is (empty? (filter #(= :dig_in_failed (:kind %)) @seen))))))))

(defn hover-over-digs!
  "A dig leaves the body where it stands for the moment (the fall into the hole takes ticks on a server): the job decides
  its step into the hole while the body is still over it."
  [p]
  (.override (.-world p) "dig"
             (fn ^:async f [token a impl]
               (let [pos (.-pos (.self p))
                     at [(.-x pos) (.-y pos) (.-z pos)]
                     r (await (impl token a))]
                 (fake/swap-self! p assoc :pos at)
                 r)))
  p)

(defn ^:async run-hover! [spec]
  (await (tick-out! (setup (hover-over-digs! (await (see-surface! (sensing spec)))) 'jobs.survival.dig-in {}))))

(deftest lava-seen-under-the-hole-is-not-stepped-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (run-hover! {:blocks (assoc ground "0,62,0" "lava")}))]
          (is (= [[0 64 0] [0 63 0]] (digs p)) "the cell over the lava is not dug")
          (is (= 64 (feet-y p)))
          (is (= 1 (count (failed s))))
          (is (re-find #"lava under the hole" (str (:text (first (failed s)))))))))))

(deftest air-seen-under-the-hole-is-not-stepped-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (run-hover! {:blocks (assoc ground "0,62,0" "air")}))]
          (is (= 64 (feet-y p)))
          (is (re-find #"air under" (str (:text (first (failed s)))))))))))

(deftest rock-seen-under-the-hole-is-stepped-into-after-the-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (run-hover! {:blocks ground}))]
          (is (= [[0 64 0] [0 63 0] [0 62 0]] (digs p)))
          (is (= 62 (feet-y p)))
          (is (empty? (failed s))))))))
