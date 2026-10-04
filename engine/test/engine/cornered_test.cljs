(ns engine.cornered-test
  "A retreat in a 1-wide tunnel: it walks back down the stair it came up, and
  a body with no way out escalates (seal with carried blocks, fight with a tool
  or the fist) instead of failing the same round again and again."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async first-round [spec world]
  (let [s (setup world)]
    (core/submit! (:eng s) spec {})
    (await (core/tick! (:eng s)))
    s))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn last-move [p]
  (let [a (.-args (peek (calls p "moveTo")))]
    {:x (.. a -pos -x) :y (.. a -pos -y) :z (.. a -pos -z)}))

(defn placed-cells [p]
  (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)]) (calls p "place")))

(defn blocked-events [seen] (filterv #(= :retreat_blocked (:kind %)) @seen))

(defn skeleton [x y z] {:id 9 :name "skeleton" :kind "hostile" :pos {:x x :y y :z z} :health 20})

(defn key-of [x y z] (str x "," y "," z))

(defn rock
  "Stone over x0..x1, y0..y1, z -1..1, less the open cells (a set of [x y z])."
  [[x0 x1] [y0 y1] open]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z [-1 0 1]
                 :when (not (open [x y z]))]
             [(key-of x y z) "stone"])))

(def stair-cells
  "A 1-wide stair rising one block per column towards +x along z 0, three cells tall
  (feet, head and the headroom to step up), the feet at 64 + x."
  (set (for [x (range -9 6) dy [0 1 2]] [x (+ 64 x dy) 0])))

(def stair (rock [-10 6] [52 74] stair-cells))

(def retreat '(jobs.survival.retreat))

(deftest a-retreat-walks-back-down-the-stair-it-came-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (first-round retreat {:blocks stair :entities [(skeleton 3 67 0)]}))
              m (last-move p)]
          (is (= 1 (count (calls p "moveTo"))) "the stair behind is open: it walks")
          (is (< (:x m) -3) "away from the skeleton at the top")
          (is (< (:y m) 61) "down the steps, not along its own height")
          (is (= 0 (:z m)) "in the stair's own column")
          (is (empty? (blocked-events seen))))))))

(deftest respond-unarmed-flees-down-the-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round '(jobs.survival.respond-to-hostile)
                                              {:blocks stair :inventory [{:name "stone_pickaxe" :count 1}]
                                               :entities [(skeleton 3 67 0)]}))]
          (is (zero? (count (calls p "attack"))) "a pickaxe is no weapon: it flees first")
          (is (< (:y (last-move p)) 61) "down the stair"))))))

(def tunnel-cells
  "A flat 1-wide tunnel along x on z 0, feet and head height, x -8..8."
  (set (for [x (range -8 9) y [64 65]] [x y 0])))

(deftest a-body-at-a-block-centre-probes-its-own-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round retreat {:self {:pos [0.5 64 0.5]}
                                                       :blocks (rock [-9 9] [62 67] tunnel-cells)
                                                       :entities [(skeleton 5.5 64 0.5)]}))
              m (last-move p)]
          (is (= 1 (count (calls p "moveTo"))) "x 0.5 is column 0, the tunnel, not column 1")
          (is (= {:x -6 :y 64 :z 0} m)))))))

(def dead-end-cells
  "A 1-wide tunnel along x on z 0 closed at x -1 behind the body: x 0..8."
  (set (for [x (range 0 9) y [64 65]] [x y 0])))

(def dead-end (rock [-3 9] [62 67] dead-end-cells))

(deftest a-cornered-body-with-blocks-seals-itself-in-and-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (await (first-round retreat {:blocks dead-end
                                                                      :inventory [{:name "cobblestone" :count 20}]
                                                                      :entities [(skeleton 4 64 0)]}))]
          (is (= [[1 64 0] [1 65 0]] (placed-cells p)) "the open side towards the skeleton, feet then head")
          (is (zero? (count (calls p "attack"))))
          (is (zero? (count (calls p "moveTo"))))
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "place"))) "sealed: nothing more to place")
          (is (= ["j1"] (:list (core/state eng))) "it waits in the seal while the skeleton is near")
          (is (empty? (blocked-events seen)) "never a blocked retreat")
          (swap! clock + 61000)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "not for ever: done after max-hide-ms")
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen))) "one notice that it walled itself in"))))))

(deftest a-sealed-body-is-done-once-the-hostile-has-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round retreat {:blocks dead-end
                                                                 :inventory [{:name "cobblestone" :count 20}]
                                                                 :entities [(skeleton 4 64 0)]}))]
          (swap! (fake/state p) assoc :entities [])
          (swap! clock + 1000)
          (await (core/tick! eng))
          (swap! clock + 5000)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest a-cornered-body-without-blocks-fights-with-its-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (first-round retreat {:blocks dead-end
                                                            :inventory [{:name "stone_pickaxe" :count 1}]
                                                            :entities [(skeleton 2 64 0)]}))]
          (is (= 1 (count (calls p "attack"))) "no blocks: it hits back")
          (is (= ["stone_pickaxe"] (mapv #(.. % -args -item) (calls p "equip"))) "with the pickaxe in hand")
          (is (empty? (blocked-events seen))))))))

(deftest a-cornered-body-with-nothing-fights-with-its-fist
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (await (first-round retreat {:blocks dead-end :entities [(skeleton 2 64 0)]}))]
          (is (= 1 (count (calls p "attack"))))
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack"))) "it keeps hitting; no failed rounds")
          (is (empty? (blocked-events seen))))))))

(deftest a-cornered-body-that-cannot-place-fights-instead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round retreat {:blocks dead-end
                                                       :inventory [{:name "cobblestone" :count 20}]
                                                       :entities [(assoc (skeleton 1 64 0) :name "zombie")]}))]
          (is (= 1 (count (calls p "attack"))) "the zombie stands in the gap: the seal fails, it hits back"))))))
