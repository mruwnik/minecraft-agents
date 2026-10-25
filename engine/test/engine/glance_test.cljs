(ns engine.glance-test
  "perception's one-cell readers: glance (in view now), feel (touching the body), sensed (felt, seen, remembered or
  unknown) and the wrapped look that glances once it turned."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]
            [engine.perception.store :as store]
            [engine.test-util :as tu]))

;; The body stands at [0 64 0], eye at (0.5, 65.62, 0.5), facing south (+z) unless a test turns it.

(defn rig
  ([blocks] (rig blocks {}))
  ([blocks opts]
   (let [clock (atom 1000000)
         p (tu/fake {:blocks blocks})
         raw (fake-raw/create p)]
     {:p p :clock clock :per (perception/create raw (merge {:now #(deref clock)} opts))})))

(defn turn! [p yaw] (swap! (fake/state p) assoc :yaw yaw))
(defn dark! [p] (swap! (fake/state p) assoc :light-default [0 0]))
(defn seen-name [per pos] (:name (perception/seen-block per pos)))

(deftest a-lit-cell-in-line-is-seen-and-recorded-without-a-pass
  (let [{:keys [per]} (rig {"0,65,4" "gold_block"})]
    (is (nil? (seen-name per [0 65 4])))
    (is (= {:name "gold_block" :pos [0 65 4] :full-cube true :age-ms 0 :visible true}
           (dissoc (perception/glance! per [0 65 4]) :state-id)))
    (is (= "gold_block" (seen-name per [0 65 4])))))

(deftest a-cell-behind-stone-is-unknown
  (let [{:keys [per]} (rig (merge (tu/box -2 63 3 2 67 3 "stone") {"0,65,6" "gold_block"}))]
    (is (nil? (perception/glance! per [0 65 6])))
    (is (= {:unknown true :pos [0 65 6]} (perception/sensed per [0 65 6])))
    (is (= "stone" (:name (perception/sensed per [0 65 3]))))))

(deftest in-the-dark-a-cell-at-2-is-seen-and-at-6-is-not-unless-a-torch-is-held
  (let [{:keys [p per]} (rig {"1,65,2" "gold_block" "0,65,6" "iron_block" "-1,65,8" "diamond_block"})]
    (dark! p)
    (is (= ["gold_block" nil nil] (mapv #(:name (perception/glance! per %)) [[1 65 2] [0 65 6] [-1 65 8]])))
    (swap! (fake/state p) assoc-in [:self :held] "torch")
    (is (= ["gold_block" "iron_block" nil] (mapv #(:name (perception/glance! per %)) [[1 65 2] [0 65 6] [-1 65 8]])))))

(deftest a-sync-glance-sees-only-inside-the-view-cone
  (let [{:keys [per]} (rig {"0,65,-5" "gold_block"})]
    (is (nil? (perception/glance! per [0 65 -5])))
    (is (:unknown (perception/sensed per [0 65 -5])))))

(deftest feel-answers-only-the-cells-the-body-touches-whatever-the-light
  (let [{:keys [p per]} (rig (merge (tu/box -1 63 -1 1 66 1 "stone") {"0,64,0" "air" "0,65,0" "air" "0,63,0" "dirt"}))]
    (dark! p)
    (is (= ["dirt" "air" "air"] (mapv #(:name (perception/feel! per %)) [[0 63 0] [0 64 0] [0 65 0]])))
    (is (:felt (perception/sensed per [0 63 0])))
    (is (= "dirt" (seen-name per [0 63 0])))
    (is (every? nil? (map #(perception/feel! per %) [[1 64 0] [0 66 0] [1 65 1] [0 62 0]])))))

(defn feel-names [per cells] (mapv #(:name (perception/feel! per %)) cells))

(defn stand-at! [p y] (swap! (fake/state p) assoc-in [:self :pos] [0 y 0]))

(deftest feel-the-floor-under-a-full-block
  (let [{:keys [per]} (rig {"0,63,0" "dirt" "0,62,0" "gold_block"})]
    (is (= ["dirt" nil] (feel-names per [[0 63 0] [0 62 0]])))))

(deftest feel-does-not-reach-the-cell-under-a-slab
  (let [{:keys [p per]} (rig {"0,64,0" "oak_slab" "0,63,0" "gold_block"})]
    (stand-at! p 64.5)
    (is (= ["oak_slab" nil] (feel-names per [[0 64 0] [0 63 0]])))))

(deftest feel-the-fence-under-the-feet
  (let [{:keys [p per]} (rig {"0,64,0" "oak_fence" "0,63,0" "gold_block"})]
    (stand-at! p 65.5)
    (is (= ["oak_fence" nil] (feel-names per [[0 64 0] [0 63 0]])))))

(deftest a-falling-body-feels-nothing-below
  (let [{:keys [p per]} (rig {"0,69,0" "gold_block" "0,68,0" "gold_block"})]
    (stand-at! p 70.9)
    (is (= [nil nil] (feel-names per [[0 69 0] [0 68 0]])))))

(deftest a-fluid-cell-after-a-reload-reads-no-newer-than-its-true-time
  (let [{:keys [per clock]} (rig {"0,65,4" "water"})
        ^js st (:st per)
        sec-of #(.get (store/store-of st "overworld") (store/section-key 0 4 0))]
    (perception/glance! per [0 65 4])
    (let [fine (store/cell-seen (sec-of) (store/cell-index 0 65 4))]
      (set! (.-fine ^js (sec-of)) nil)
      (is (<= (store/cell-seen (sec-of) (store/cell-index 0 65 4)) fine)))))

(defn fill-section!
  "Records every cell of the section holding [0 64 0] as the state id at now."
  [^js st id now]
  (let [store (store/store-of st "overworld")]
    (doseq [x (range 16) y (range 64 80) z (range 16)] (store/record! st store x y z id now))
    (.get store (store/section-key 0 4 0))))

(deftest a-section-full-of-water-keeps-its-ms-times-in-one-fixed-16-kb-array
  (let [{:keys [per clock]} (rig {"0,65,4" "water"})
        ^js st (:st per)
        id (:state-id (perception/glance! per [0 65 4]))
        ^js sec (fill-section! st id @clock)]
    (is (= 16384 (some-> ^js (.-fine sec) .-buffer .-byteLength)) "off the V8 heap, the same size however many cells")
    (is (= @clock (store/cell-seen sec (store/cell-index 3 70 9))))))

(deftest a-fluid-cells-ms-time-survives-the-section-base-moving-on-and-stone-drops-it
  (let [{:keys [per clock]} (rig {"0,65,4" "water" "1,65,3" "stone"})
        ^js st (:st per)
        id (:state-id (perception/glance! per [0 65 4]))
        stone (:state-id (perception/glance! per [1 65 3]))
        store (store/store-of st "overworld")
        ^js sec (.get store (store/section-key 0 4 0))
        t0 @clock]
    (swap! clock + (* 300 60000) 1234)
    (store/record! st store 1 65 4 id @clock)
    (is (= [(+ t0 (* 300 60000) 1234) true]
           [(store/cell-seen sec (store/cell-index 1 65 4)) (> (.-base sec) t0)]))
    (store/record! st store 1 65 4 stone @clock)
    (is (= 0 (mod (- (store/cell-seen sec (store/cell-index 1 65 4)) (.-base sec)) 1000)) "stone is timed to the second")))

(deftest a-cell-diagonal-behind-a-wall-corner-inside-rock-is-unknown
  (let [{:keys [per]} (rig (merge (tu/box -1 63 -1 1 66 1 "stone") {"0,64,0" "air" "0,65,0" "air" "1,65,1" "lava"}))]
    (is (= "stone" (:name (perception/sensed per [0 65 1]))))
    (is (= {:unknown true :pos [1 65 1]} (perception/sensed per [1 65 1])))))

(deftest a-remembered-cell-answers-with-its-age-and-not-visible
  (let [{:keys [p per clock]} (rig {"0,65,4" "gold_block"})]
    (perception/pass! per)
    (turn! p 180)
    (swap! clock + 30000)
    (is (= {:name "gold_block" :pos [0 65 4] :full-cube true :age-ms 30000 :visible false}
           (dissoc (perception/sensed per [0 65 4]) :state-id)))))

(deftest a-remembered-fluid-door-or-fire-reads-as-remembered-however-old
  (let [{:keys [p per clock]} (rig {"0,65,4" "water" "1,65,4" "oak_door" "-1,65,4" "fire" "2,65,4" "stone"})
        cells [[0 65 4] [1 65 4] [-1 65 4] [2 65 4]]]
    (perception/pass! per)
    (turn! p 180)
    (swap! clock + 86400000)
    (is (= ["water" "oak_door" "fire" "stone"] (mapv #(:name (perception/sensed per %)) cells)) "what the age makes of it is the reader's")
    (is (every? #(= 86400000 (:age-ms (perception/sensed per %))) cells))))

(deftest an-unloaded-cell-is-nil-and-the-block-shape-follows-the-state
  (let [p (tu/fake {:blocks {"0,65,3" "oak_slab"} :unloaded ["0,65,5"]})
        per (perception/create (fake-raw/create p) {})]
    (is (nil? (perception/sensed per [0 65 5])))
    (is (= ["oak_slab" nil "bottom"]
           ((juxt :name :full-cube (comp :type :properties)) (perception/sensed per [0 65 3]))))))

(deftest seen-block-carries-the-state-id
  (let [{:keys [per]} (rig {"0,65,3" "gold_block"})]
    (perception/pass! per)
    (is (number? (:state-id (perception/seen-block per [0 65 3]))))
    (is (= (:state-id (perception/seen-block per [0 65 3])) (:state-id (perception/sensed per [0 65 3]))))))

(deftest a-change-in-the-same-tick-is-read-at-once
  (let [{:keys [p per]} (rig {"0,65,3" "gold_block"})
        w (perception/wrap p per)]
    (is (= "gold_block" (.-name (.sensedAt w #js {:x 0 :y 65 :z 3}))))
    (fake/set-block! p [0 65 3] "iron_block")
    (is (= "iron_block" (.-name (.sensedAt w #js {:x 0 :y 65 :z 3}))))))

(deftest the-wrapper-reads-sensed-feel-and-glance-and-block-at-is-sensed
  (let [{:keys [p per]} (rig (merge (tu/box -2 63 3 2 67 3 "stone") {"0,65,6" "gold_block" "0,63,0" "dirt"}))
        w (perception/wrap p per)
        at (fn [x y z] #js {:x x :y y :z z})]
    (is (identical? (.-sensedAt w) (.-blockAt w)))
    (is (nil? (.-name (.blockAt w (at 0 65 6)))))
    (is (= "gold_block" (.-name (.blockAt p (at 0 65 6)))))
    (is (.-unknown (.sensedAt w (at 0 65 6))))
    (is (= [true 0] [(.-visible (.sensedAt w (at 0 65 3))) (.-ageMs (.sensedAt w (at 0 65 3)))]))
    (is (.-fullCube (.sensedAt w (at 0 65 3))))
    (is (= "dirt" (.-name (.feel w (at 0 63 0)))))
    (is (nil? (.feel w (at 0 65 3))))
    (is (= {:x 0 :y 65 :z 3} (js->clj (.-pos (.glance w (at 0 65 3))) :keywordize-keys true)))
    (is (nil? (.glance w (at 0 65 6))))))

(deftest look-turns-then-glances-the-cell-and-its-neighbours
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p per]} (rig {"0,65,-5" "gold_block" "0,66,-5" "iron_block"})
              _ (.setOwner p "t1")
              w (perception/wrap p per)
              before (perception/sensed per [0 65 -5])
              r (await (.look w "t1" #js {:pos #js {:x 0.5 :y 65.5 :z -4.5}}))]
          (is (:unknown before))
          (is (= "ok" (.-status r)))
          (is (= ["gold_block" "iron_block"] [(seen-name per [0 65 -5]) (seen-name per [0 66 -5])]))
          (is (= "gold_block" (.-name (.-sensed r)))))))))

(deftest touches?-is-the-one-feel-rule-over-feet-position-and-collision-top
  (is (true? (rays/touches? 0.5 64 0.5 [0 63 0] 1)) "the full block under the feet")
  (is (false? (rays/touches? 0.5 64 0.5 [0 62 0] 1)) "the cell below the floor")
  (is (true? (rays/touches? 0.5 64.5 0.5 [0 64 0] 0.5)) "a slab under the feet")
  (is (false? (rays/touches? 0.5 64.5 0.5 [0 63 0] 1)) "the cell under a slab")
  (is (true? (rays/touches? 0.5 65.5 0.5 [0 64 0] 1.5)) "a fence under the feet")
  (is (false? (rays/touches? 0.5 65.5 0.5 [0 63 0] 1)) "below the fence")
  (is (false? (rays/touches? 0.5 70.9 0.5 [0 69 0] 1)) "a falling body: the block far below")
  (is (false? (rays/touches? 0.5 64 0.5 [1 64 0] 1)) "beside the hitbox margin")
  (is (true? (rays/touches? 0.5 64 0.5 [0 65 0] 0)) "the head cell"))

(deftest seeing-a-remembered-door-again-however-old-is-no-memory-write
  (let [{:keys [per clock]} (rig {"0,65,4" "oak_door"})
        ^js st (:st per)
        id (:state-id (perception/glance! per [0 65 4]))
        store (store/store-of st "overworld")
        again! #(let [w (.-writes st)] (store/record! st store 0 65 4 id @clock) (- (.-writes st) w))]
    (swap! clock + 5500)
    (is (zero? (again!)))
    (swap! clock + 3600000)
    (is (zero? (again!)) "the same state: its new age is jobs.lib.memory-age's business")))

(deftest a-stone-cell-seen-seconds-after-a-mark-reads-newer-than-it
  (let [{:keys [per clock]} (rig {"1,65,3" "stone"})
        ^js st (:st per)
        id (:state-id (perception/glance! per [1 65 3]))
        store (store/store-of st "overworld")
        ^js sec (.get store (store/section-key 0 4 0))
        mark (+ @clock 30500)]
    (store/record! st store 1 65 3 id (+ @clock 31200))
    (store/record! st store 2 65 3 id (+ (.-base sec) 90000))
    (is (= (+ (.-base sec) 90000) (store/cell-seen sec (store/cell-index 2 65 3))) "a cell seen 90 s after the base reads 90 s, not the minute")
    (is (< mark (store/cell-seen sec (store/cell-index 1 65 3))) "31 s after the base is newer than a mark at 30.5 s, though both fall in one minute")))
