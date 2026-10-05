(ns engine.path-regions-test
  "engine.path.regions over small fixture worlds: reachable, proved unreachable, unknown; invalidation (a map updated by
  invalidate! equals one built afresh after placing, digging and water changes); the background build over a source."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.path.courses :as courses]
            [engine.path.fixture :as fx]
            [engine.path.regions :as regions]
            [engine.planner-fixture :as pf]))

(defn world [snapshot] #js {:snapshot snapshot :table @pf/table :space @pf/space})

(defn rmap [snapshot] (regions/create (world snapshot)))

(def from {:x 2 :y 64 :z 2})

(defn route [snapshot goal] (regions/route (rmap snapshot) from [goal]))

(deftest an-open-floor-is-reachable
  (let [r (route (pf/world {}) (pf/near 30 64 30 1))]
    (is (= :reachable (:status r)))
    (is (< 0 (:estimate r)))
    (is (seq (:regions r)))))

(deftest a-goal-on-a-plateau-no-move-climbs-is-proved-unreachable
  (let [snap (pf/world {:fill [[20 64 20 30 70 30 "stone"]]})]
    (is (= {:status :unreachable :why :goal-enclosed}
           (select-keys (route snap (pf/near 25 71 25 0)) [:status :why])))))

(deftest a-sealed-room-is-proved-unreachable
  (let [snap (pf/world {:fill [[20 64 20 24 67 24 "stone"] [21 64 21 23 65 23 "air"]]})]
    (is (= :unreachable (:status (route snap (pf/near 22 64 22 0)))))))

(deftest a-goal-inside-stone-is-not-standable
  (let [snap (pf/world {:fill [[20 64 20 24 67 24 "stone"]]})]
    (is (= {:status :unreachable :why :goal-not-standable}
           (select-keys (route snap (pf/near 22 65 22 0)) [:status :why])))))

(deftest a-goal-in-unloaded-land-is-unknown
  (is (= {:status :unknown :why :goal-unloaded}
         (select-keys (route (pf/world {}) (pf/near 200 64 200 0)) [:status :why]))))

(deftest an-enclosure-open-to-unloaded-land-is-unknown
  ;; the walled-off goal's land runs on to the unloaded column east of it: a way in may run there
  (let [snap (pf/snapshot {:fill [[0 60 0 47 63 31 "stone"] [30 64 0 30 71 31 "stone"]]})]
    (is (= {:status :unknown :why :unloaded}
           (select-keys (route snap (pf/near 40 64 10 0)) [:status :why])))))

(deftest a-start-in-the-air-is-unknown
  (is (= {:status :unknown :why :start-not-standable}
         (select-keys (regions/route (rmap (pf/world {})) {:x 2 :y 70 :z 2} [(pf/near 30 64 30 1)]) [:status :why]))))

(deftest a-goal-set-names-the-goal-reached
  (let [snap (pf/world {:fill [[20 64 20 24 67 24 "stone"] [21 64 21 23 65 23 "air"]]})
        r (regions/route (rmap snap) from [(pf/near 22 64 22 0) (pf/near 10 64 30 0)])]
    (is (= [:reachable 1] [(:status r) (:goal r)]))))

(deftest an-xz-goal-is-found-at-any-height
  (is (= :reachable (:status (route (pf/world {}) (pf/xz 30 30 1))))))

(deftest a-query-allowance-too-small-answers-building
  (is (= {:status :unknown :why :building}
         (select-keys (regions/route (rmap (pf/world {})) from [(pf/near 30 64 30 1)] {:max-builds 1}) [:status :why]))))

;; ---- invalidation ----

(defn section-data [^js sec]
  (mapv #(some-> ^js % js/Array.from vec)
        [(.-keys sec) (.-comp sec) (.-rep sec) (.-flags sec) (.-eoff sec) (.-etarget sec) (.-ecost sec) (.-ioff sec) (.-iin sec)]))

(defn build-all!
  "every section of the columns cx0..cx1 x cz0..cz1"
  [rm [cx0 cx1 cz0 cz1]]
  (doseq [cx (range cx0 (inc cx1)) cz (range cz0 (inc cz1))] (regions/queue-column! rm cx cz))
  (while (pos? (regions/pending rm)) (regions/build-step! rm 1000)))

(defn differences
  "keys of the sections whose data differ between rm and a map built afresh over the same snapshot"
  [rm snapshot cols]
  (let [fresh (rmap snapshot)]
    (build-all! fresh cols)
    (build-all! rm cols)
    (vec (for [k (js/Array.from (.keys ^js (.-sections fresh)))
               :let [a (.get ^js (.-sections rm) k) b (.get ^js (.-sections fresh) k)]
               :when (or (nil? a) (not= (section-data a) (section-data b)))]
           k))))

(def COLS [-1 2 -1 2])

(defn change! [rm snapshot x y z name]
  (let [old (.stateAt ^js snapshot x y z)]
    (.setState ^js snapshot x y z (fx/state-id name {}))
    (regions/invalidate! rm x y z old)))

(deftest a-map-updated-by-invalidate-equals-one-built-afresh
  (testing "placing and digging on land, with a ledge and a wall"
    (let [snap (pf/world {:fill [[10 64 10 14 66 14 "stone"] [20 64 5 20 66 30 "stone"]]})
          rm (rmap snap)]
      (build-all! rm COLS)
      (doseq [[x y z name] [[5 64 5 "stone"] [5 65 5 "stone"] [12 66 12 "air"] [20 64 18 "air"] [20 65 18 "air"]
                            [3 63 3 "air"] [15 64 15 "oak_fence"] [16 64 16 "oak_slab"] [8 64 8 "ladder"]]]
        (change! rm snap x y z name)
        (is (= [] (differences rm snap COLS)) (str "after " name " at " [x y z])))))
  (testing "water: a pond made and drained under a tower a drop into the water starts from"
    (let [snap (pf/world {:fill [[10 80 10 12 80 12 "stone"] [11 64 13 11 79 13 "air"]]})
          rm (rmap snap)]
      (build-all! rm COLS)
      (doseq [[x y z name] [[11 63 13 "water"] [11 62 13 "water"] [11 61 13 "water"] [11 63 13 "stone"]
                            [11 62 13 "air"] [11 63 13 "water"]]]
        (change! rm snap x y z name)
        (is (= [] (differences rm snap COLS)) (str "after " name " at " [x y z]))))))

(deftest a-wall-built-round-the-goal-turns-reachable-to-unreachable
  (let [snap (pf/world {})
        rm (rmap snap)
        goal [(pf/near 22 64 22 0)]]
    (is (= :reachable (:status (regions/route rm from goal))))
    (doseq [x (range 20 25) z (range 20 25) y [64 65 66]
            :when (or (#{20 24} x) (#{20 24} z) (= y 66))]
      (change! rm snap x y z "stone"))
    (is (= :unreachable (:status (regions/route rm from goal))))
    (change! rm snap 20 64 22 "air")
    (change! rm snap 20 65 22 "air")
    (is (= :reachable (:status (regions/route rm from goal))))))

;; ---- the background build ----

(defn fake-source [snapshot cols]
  (let [listeners (atom #{})]
    {:emit (fn [e] (doseq [f @listeners] (f (clj->js e))))
     :source #js {:snapshot snapshot :table @pf/table :space @pf/space
                  :columns (fn [] (to-array (map to-array cols)))
                  :onChange (fn [f] (swap! listeners conj f) (fn [] (swap! listeners disj f)))}
     :listeners listeners}))

(deftest attach-builds-the-loaded-columns-in-the-background-and-follows-changes
  (let [snap (pf/world {})
        {:keys [emit source listeners]} (fake-source snap [[0 0] [1 1]])
        {:keys [map stop]} (regions/attach! source {:slice-ms 1000 :every-ms 0})]
    (is (= 48 (regions/pending map)))
    (regions/build-step! map 1000)
    (is (= 0 (regions/pending map)))
    (is (pos? (:sections (regions/memory map))))
    (let [old (.stateAt ^js snap 5 64 5)]
      (.setState ^js snap 5 64 5 (fx/state-id "stone" {}))
      (emit {:type "block" :x 5 :y 64 :z 5 :old old}))
    (is (pos? (regions/pending map)) "the sections round the change are queued again")
    (emit {:type "unload" :cx 1 :cz 1})
    (is (nil? (.get ^js (.-sections map) (regions/sec-key 1 8 1))))
    (stop)
    (is (empty? @listeners))))

(defn node-cells
  "[x y z] of every node cell of rm's built sections"
  [rm]
  (vec (for [^js sec (es6-iterator-seq (.values ^js (.-sections rm)))
             :let [k (.-key sec)]
             i (range (.-n sec))
             :let [li (bit-shift-right (aget (.-keys sec) i) 4)]]
         [(+ (* 16 (regions/key-sx k)) (bit-and li 15))
          (+ (.-min-y ^js rm) (* 16 (regions/key-sy k)) (bit-shift-right li 8))
          (+ (* 16 (regions/key-sz k)) (bit-and (bit-shift-right li 4) 15))])))

(defn column-range [^js snapshot]
  (let [cols (js->clj (.columns snapshot))]
    [(apply min (map first cols)) (apply max (map first cols)) (apply min (map second cols)) (apply max (map second cols))]))

(deftest seeded-changes-on-courses-leave-the-map-as-a-fresh-one
  ;; at node cells: a block placed at the feet, the floor dug, water poured, the block over the head dug
  (doseq [course ["lake20" "drop8-water" "door-closed" "fence-gate" "ladder6" "rand50-ew" "waterfall-up"]]
    (let [{:keys [snapshot]} (courses/course-snapshot course)
          cols (column-range snapshot)
          rm (rmap snapshot)
          seed (volatile! 7)
          rnd (fn [n] (vswap! seed #(mod (+ (* % 1103515245) 12345) 2147483648)) (mod (quot @seed 7) n))]
      (build-all! rm cols)
      (doseq [k (range 8)]
        (let [cells (node-cells rm)
              [x y z] (nth cells (rnd (count cells)))
              [dy name] (nth [[0 "stone"] [-1 "air"] [0 "water"] [2 "air"]] (mod k 4))]
          (change! rm snapshot x (+ y dy) z name)
          (is (= [] (differences rm snapshot cols)) (str course ": " name " at " [x (+ y dy) z])))))))
