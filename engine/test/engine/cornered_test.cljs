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
            [engine.triggers :as triggers]
            [jobs.survival.retreat :as retreat]))

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

(defn last-move
  "The target of the latest walk of the engine walker."
  [eng]
  (select-keys (peek (tu/walked-to eng)) [:x :y :z]))

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
        (let [{:keys [eng seen]} (await (first-round retreat {:blocks stair :entities [(skeleton 3 67 0)]}))
              m (last-move eng)]
          (is (= 1 (count (tu/walked-to eng))) "the stair behind is open: it walks")
          (is (< (:x m) -3) "away from the skeleton at the top")
          (is (< (:y m) 61) "down the steps, not along its own height")
          (is (= 0 (:z m)) "in the stair's own column")
          (is (empty? (blocked-events seen))))))))

(deftest respond-unarmed-flees-down-the-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (first-round '(jobs.survival.respond-to-hostile)
                                              {:blocks stair :inventory [{:name "stone_pickaxe" :count 1}]
                                               :entities [(skeleton 3 67 0)]}))]
          (is (zero? (count (calls p "attack"))) "a pickaxe is no weapon: it flees first")
          (is (< (:y (last-move eng)) 61) "down the stair"))))))

(def tunnel-cells
  "A flat 1-wide tunnel along x on z 0, feet and head height, x -8..8."
  (set (for [x (range -8 9) y [64 65]] [x y 0])))

(deftest a-body-at-a-block-centre-probes-its-own-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (first-round retreat {:self {:pos [0.5 64 0.5]}
                                                         :blocks (rock [-9 9] [62 67] tunnel-cells)
                                                         :entities [(assoc (skeleton 5.5 64 0.5) :visible true)]}))
              m (last-move eng)]
          (is (= 1 (count (tu/walked-to eng))) "x 0.5 is column 0, the tunnel, not column 1")
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
          (is (= [0 64 0] (mapv js/Math.floor (:pos (fake/self p)))) "no walk: at most a step to the middle of its own cell")
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

(def corpse-zombie
  "A zombie one hit from death whose corpse stays listed after it dies (a death animation)."
  {:id 1 :name "zombie" :kind "hostile" :pos {:x 1 :y 64 :z 0} :health 5 :lingers true})

(deftest a-cornered-fight-that-kills-its-hostile-neither-swings-at-the-corpse-nor-warns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (await (first-round retreat {:blocks dead-end :entities [corpse-zombie]}))]
          (is (= 1 (count (calls p "attack"))) "one hit kills it")
          (dotimes [_ 3]
            (swap! clock + 1000)
            (await (core/tick! eng)))
          (is (= 1 (count (calls p "attack"))) "the dead id is not attacked again")
          (is (empty? (blocked-events seen)) "the fight succeeded: no warn")
          (swap! clock + 6000)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "done once it has been gone for the cooldown"))))))

(defn edge-world
  "Floor (stone at y 63) for x <= edge on z 0 and air everywhere else."
  [edge]
  (fn [{:keys [x y z]}] (if (and (= y 63) (<= x edge) (= z 0)) "stone" "air")))

(deftest walk-cells-stop-at-the-edge-of-a-floor
  (is (= [1 2 3] (mapv :x (retreat/walk-cells (edge-world 3) {:x 0.5 :y 64 :z 0.5} [1 0] 6))) "the cells past the edge have no floor")
  (is (= [1 2 3] (mapv :x (retreat/walk-cells (edge-world 3) {:x 0.5 :y 64 :z 0.5} [1 0] 3))))
  (is (= [1 2 3 4 5 6] (mapv :x (retreat/walk-cells (edge-world 9) {:x 0.5 :y 64 :z 0.5} [1 0] 6)))))
