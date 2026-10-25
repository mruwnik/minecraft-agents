(ns engine.perception-test
  (:require [cljs.test :refer [deftest is are async]]
            ["path" :as path]
            [engine.fake :as fake]
            [engine.fake.node :as node]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.mobs :as mobs]
            [engine.perception.rays :as rays]
            [engine.perception.store :as store]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.memory-age :as memory-age]
            [jobs.lib.reach.room :as reach-room]
            [engine.test-util :as tu]))

;; The body stands at [0 64 0], eye at (0.5, 65.62, 0.5), facing south (+z) unless a test turns it.

(def shading (delay (node/require-here "../tools/view/web/shading.mjs")))
(def seen-file (delay (node/require-here "./js/seen-file.mjs")))

(defn rig
  ([blocks] (rig blocks {}))
  ([blocks opts]
   (let [p (tu/blind (tu/fake {:blocks blocks}))
         raw (fake-raw/create p)]
     {:p p :raw raw :per (perception/create raw opts)})))

(defn seen-name [per pos] (:name (perception/seen-block per pos)))
(defn turn! [p yaw] (swap! (fake/state p) assoc :yaw yaw))
(defn light! [p m] (swap! (fake/state p) merge m))

(defn wall
  "stone x -8..8, y 58..72, z z0..z1, with `holes` left as air and `extra` cells put in"
  [z0 z1 holes extra]
  (merge (apply dissoc (tu/box -8 58 z0 8 72 z1 "stone") holes) extra))

(deftest ore-behind-three-blocks-of-stone-is-unknown
  (let [{:keys [per]} (rig (wall 5 8 [] {"0,65,8" "diamond_ore"}))]
    (perception/pass! per)
    (is (= ["stone" nil nil] (mapv #(seen-name per %) [[0 65 5] [0 65 6] [0 65 8]])))
    (is (:unknown (perception/seen-block per [0 65 8])))))

(deftest ore-in-a-lit-cave-is-seen-through-an-opening
  (let [{:keys [per]} (rig (wall 5 8 ["0,65,5" "0,65,6" "0,65,7"] {"0,65,8" "diamond_ore"}))]
    (perception/pass! per)
    (is (= "diamond_ore" (seen-name per [0 65 8])))
    (is (= "air" (seen-name per [0 65 6])))))

(deftest in-the-dark-nothing-past-four-blocks-is-named-and-a-torch-lit-block-is
  (let [{:keys [p per]} (rig {"-1,65,2" "gold_block" "1,65,3" "gold_block" "0,65,5" "gold_block" "1,65,7" "gold_block"})]
    (light! p {:light-default [0 0]})
    (perception/pass! per)
    (is (= ["gold_block" "gold_block" nil nil] (mapv #(seen-name per %) [[-1 65 2] [1 65 3] [0 65 5] [1 65 7]])))
    (light! p {:light {[1 65 6] [0 14] [1 66 6] [0 13] [0 65 7] [0 13] [1 66 7] [0 13]}})
    (perception/pass! per)
    (is (= "gold_block" (seen-name per [1 65 7])))))

(deftest a-block-behind-the-body-is-unknown-until-it-turns
  (let [{:keys [p per]} (rig {"0,65,-5" "gold_block"})]
    (perception/pass! per)
    (is (nil? (seen-name per [0 65 -5])))
    (turn! p 180)
    (perception/pass! per)
    (is (= "gold_block" (seen-name per [0 65 -5])))))

(deftest glass-is-see-through-and-is-itself-seen
  (let [{:keys [per]} (rig (merge (tu/box -8 58 3 8 72 3 "glass") {"0,65,6" "gold_block"}))]
    (perception/pass! per)
    (is (= ["glass" "gold_block"] (mapv #(seen-name per %) [[0 65 3] [0 65 6]])))))

(deftest ore-eight-blocks-behind-a-glass-window-is-seen
  (let [window (tu/box -2 64 3 2 66 3 "glass")
        {:keys [per]} (rig (merge (wall 3 3 (keys window) window) {"0,65,11" "diamond_ore" "2,65,11" "diamond_ore"}))]
    (perception/pass! per)
    (is (= ["diamond_ore" "diamond_ore"] (mapv #(seen-name per %) [[0 65 11] [2 65 11]])))))

(defn door-rig
  "The body at [0 64 0] (eye in the door's upper half when a door is there), the cells in `doors` [x y z] shut or open
  doors of `name` facing `facing`, gold at [0 65 -5] and [0 65 5]."
  [name doors facing open]
  (let [p (tu/blind (tu/fake {:blocks (merge {"0,65,-5" "gold_block" "0,65,5" "gold_block"}
                                             (into {} (for [[x y z] doors] [(str x "," y "," z) name])))
                              :states (into {} (for [[x y z] doors]
                                                 [(str x "," y "," z) {:facing facing :open open :hinge "left"
                                                                       :half (if (even? y) "lower" "upper")}]))}))]
    {:p p :per (perception/create (fake-raw/create p) {})}))

(deftest from-the-free-part-of-a-door-cell-the-eye-sees-out-past-the-panel-not-through-it
  (let [{:keys [p per]} (door-rig "iron_door" [[0 64 0] [0 65 0]] "north" false)]
    (perception/pass! per)
    (is (nil? (seen-name per [0 65 5])) "facing south the panel on the cell's south side hides the gold")
    (turn! p 180)
    (perception/pass! per)
    (is (= "gold_block" (seen-name per [0 65 -5])) "facing north the free part of the cell shows the gold")))

(deftest a-ray-passes-the-open-part-of-a-door-on-its-way
  (let [shut (:per (door-rig "oak_door" [[0 64 3] [0 65 3]] "north" false))
        open (:per (door-rig "oak_door" [[0 64 3] [0 65 3]] "north" true))]
    (perception/pass! shut)
    (perception/pass! open)
    (is (= ["oak_door" nil] (mapv #(seen-name shut %) [[0 65 3] [0 65 5]])) "a shut door across the ray hides the gold")
    (is (= ["oak_door" "gold_block"] (mapv #(seen-name open %) [[0 65 3] [0 65 5]])) "an open door's panel lies along it")))

(deftest a-ray-stops-at-an-unloaded-cell
  (let [p (tu/fake {:blocks {"0,65,6" "gold_block"}
                    :unloaded (for [x (range -8 9) y (range 58 73)] (str x "," y ",3"))})
        per (perception/create (fake-raw/create p) {})]
    (perception/pass! per)
    (is (nil? (seen-name per [0 65 6])))))

(deftest moonlight-outdoors-still-shows-far-blocks
  (let [p (tu/fake {:blocks {"0,65,20" "gold_block"} :time 18000})
        per (perception/create (fake-raw/create p) {})]
    (perception/pass! per)
    (is (= "gold_block" (seen-name per [0 65 20])))))

(deftest a-block-beyond-the-sight-radius-is-unknown
  (let [{:keys [per]} (rig {"0,65,30" "gold_block"} {:radius 16})]
    (perception/pass! per)
    (is (nil? (seen-name per [0 65 30])))))

(deftest a-visible-change-updates-memory-and-an-unseen-one-keeps-the-old-state
  (let [{:keys [p per]} (rig {"0,65,4" "gold_block" "0,65,-4" "gold_block"})]
    (perception/start-listening! per)
    (perception/pass! per)
    (turn! p 180)
    (perception/pass! per)
    (turn! p 0)
    (fake/set-block! p [0 65 4] "iron_block")
    (fake/set-block! p [0 65 -4] "iron_block")
    (is (= ["iron_block" "gold_block"] (mapv #(seen-name per %) [[0 65 4] [0 65 -4]])))))

(deftest a-crops-age-ten-blocks-away-is-re-read-when-it-grows-in-view
  (let [pos [0 65 10]
        {:keys [p per]} (rig {"0,64,10" "farmland" "0,65,10" "wheat"})]
    (swap! (fake/state p) assoc-in [:states pos] {:age 6})
    (perception/start-listening! per)
    (perception/pass! per)
    (swap! (fake/state p) assoc-in [:states pos] {:age 7})
    (perception/pass! per)
    (is (= "7" (get-in (perception/seen-block per pos) [:properties :age])))))

(deftest a-change-behind-a-wall-is-not-seen
  (let [{:keys [p per]} (rig (assoc (wall 3 3 [] {}) "0,65,6" "gold_block"))]
    (perception/start-listening! per)
    (perception/pass! per)
    (fake/set-block! p [0 65 6] "iron_block")
    (is (nil? (seen-name per [0 65 6])))))

(deftest a-capped-store-forgets-the-least-recently-seen-section
  (let [clock (atom 1000)
        {:keys [per]} (rig {"0,64,0" "stone" "20,64,0" "dirt" "40,64,0" "sand"}
                           {:cap-bytes (* 2 perception/section-bytes) :now #(deref clock)})]
    (perception/touch! per [0 64 0])
    (swap! clock + 10)
    (perception/touch! per [20 64 0])
    (swap! clock + 10)
    (perception/touch! per [40 64 0])
    (is (= [nil "dirt" "sand"] (mapv #(seen-name per %) [[0 64 0] [20 64 0] [40 64 0]])))
    (is (= 2 (:sections (perception/stats per))))))

(deftest one-pass-is-spread-over-the-steps-of-pass-ms
  (let [{:keys [per]} (rig {} {:pass-ms 1000 :step-ms 50})
        _ (perception/step! per)
        one (perception/stats per)
        _ (dotimes [_ 19] (perception/step! per))
        all (perception/stats per)]
    (is (= 20 (:steps-per-pass one)))
    (is (= [0 1] [(:passes one) (:passes all)]))
    (is (= (:rays-per-pass all) (:rays all)))
    (is (< (:rays one) (/ (:rays-per-pass all) 10)))))

(deftest a-body-turned-every-half-second-sees-each-view-it-faces
  (let [clock (atom 0)
        walls (merge (tu/box -5 58 5 5 72 5 "stone") (tu/box -5 58 -5 5 72 -5 "stone")
                     (tu/box 5 58 -5 5 72 5 "stone") (tu/box -5 58 -5 -5 72 5 "stone"))
        {:keys [p per]} (rig walls {:now #(deref clock) :pass-ms 1500 :step-ms 50})
        ahead (for [[x z] [[0 5] [0 -5] [5 0] [-5 0]] y [64 65 66] d [-1 0 1]]
                (if (zero? x) [d y z] [x y d]))]
    (doseq [t (range 80)]
      (when (zero? (mod t 10)) (turn! p (* 90 (mod (quot t 10) 4))))
      (swap! clock + 50)
      (perception/step! per))
    (is (every? #(= "stone" (seen-name per %)) ahead))))

(deftest a-pass-casts-the-ground-ahead-before-the-grid
  (let [{:keys [per]} (rig (tu/box -15 63 -15 15 63 15 "stone") {:pass-ms 1000 :step-ms 50})
        _ (perception/step! per)
        floor (for [z (range 4 13) x [-1 0 1]] [x 63 z])]
    (is (>= (count (filter #(seen-name per %) floor)) 10) "the first twentieth of a pass already shows much of the floor ahead")))

(deftest a-forced-look-drops-the-pass-in-flight
  (let [{:keys [p per]} (rig {} {:pass-ms 1000 :step-ms 50})]
    (perception/step! per)
    (turn! p 90)
    (perception/pass! per)
    (let [st (perception/stats per)]
      (is (= 1 (:passes st)))
      (is (< (:rays st) (* 1.5 (:rays-per-pass st)))))))

(deftest seen-block-carries-age-and-properties
  (let [clock (atom 5000)
        {:keys [per]} (rig {"0,65,3" "oak_log"} {:now #(deref clock)})]
    (perception/pass! per)
    (swap! clock + 700)
    (is (= {:name "oak_log" :pos [0 65 3] :properties {:axis "y"} :age-ms 700}
           (dissoc (perception/seen-block per [0 65 3]) :state-id)))))

(deftest passes-from-one-spot-do-not-read-recorded-cells-again-until-the-view-changes
  (let [clock (atom 0) reads (atom 0)
        p (tu/blind (tu/fake {:blocks (wall 5 8 [] {})}))
        raw (fake-raw/create p)
        w (js/Object.assign #js {} raw)
        at (.-stateAt raw)
        _ (set! (.-stateAt w) (fn [x y z] (swap! reads inc) (at x y z)))
        per (perception/create w {:now #(deref clock)})
        pass-reads (fn [] (reset! reads 0) (perception/pass! per) @reads)
        _ (pass-reads)
        _ (swap! clock + 3000)
        first-pass (pass-reads)
        again (pass-reads)
        _ (fake/set-block! p [0 70 0] "iron_block")
        after-change (pass-reads)
        _ (swap! clock + 3000)
        after-wait (pass-reads)]
    (is (< (* 5 again) first-pass))
    (is (< first-pass (* 2 after-change)))
    (is (< first-pass (* 2 after-wait)))))

(deftest seen-blocks-lists-remembered-blocks-by-name-nearest-first
  (let [{:keys [per]} (rig {"0,65,3" "gold_block" "3,65,8" "gold_block" "-2,65,5" "stone"})]
    (perception/pass! per)
    (is (= [[0 65 3] [3 65 8]] (mapv :pos (perception/seen-blocks per {:names ["gold_block"] :radius 16}))))
    (is (= [[0 65 3]] (mapv :pos (perception/seen-blocks per {:names ["gold_block"] :radius 4}))))))

(deftest memory-is-saved-and-loaded-and-another-version-is-ignored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [per]} (rig {"0,65,3" "gold_block"})
              file (path/join (tu/tmp-dir) "engine" "seen.bin")
              _ (perception/pass! per)
              _ (await (perception/save! per @seen-file file))
              fresh (:per (rig {}))
              _ (perception/load! fresh @seen-file file)
              other (perception/create #js {:version (fn [] "1.0") :sightTable (fn [] (js/Uint8Array. 0))} {})]
          (perception/load! other @seen-file file)
          (is (= "gold_block" (seen-name fresh [0 65 3])))
          (is (zero? (:sections (perception/stats other)))))))))

(deftest the-wrapper-keeps-every-primitive-raw-and-adds-the-seen-readers
  (let [{:keys [p per]} (rig (wall 5 8 [] {"0,65,8" "diamond_ore"}))
        wrapped (perception/wrap p per)]
    (perception/pass! per)
    (is (every? #(identical? (aget p %) (aget wrapped %))
                (remove (conj perception/touching-primitives "look" "blockAt") (js/Object.keys p))))
    (is (identical? (.-sensedAt wrapped) (.-blockAt wrapped)))
    (is (nil? (.-name (.blockAt wrapped #js {:x 0 :y 65 :z 8}))) "behind the wall: not sensed")
    (is (= "diamond_ore" (.-name (.blockAt p #js {:x 0 :y 65 :z 8}))))
    (is (.-unknown (.seenBlockAt wrapped #js {:x 0 :y 65 :z 8})))
    (is (= "stone" (.-name (.seenBlockAt wrapped #js {:x 0 :y 65 :z 5}))))))

(deftest sensedAt-answers-as-js-and-is-redone-when-the-eye-turns
  (let [{:keys [p per]} (rig {"0,65,-5" "gold_block"})
        wrapped (perception/wrap p per)
        at #(.sensedAt wrapped #js {:x 0 :y 65 :z -5})]
    (perception/pass! per)
    (is (true? (.-unknown (at))))
    (turn! p 180)
    (perception/pass! per)
    (let [a (at)]
      (is (= ["gold_block" true -5] [(.-name a) (.-visible a) (.-z (.-pos a))]))
      (is (= #{"name" "pos" "ageMs" "visible" "fullCube"} (set (js/Object.keys a)))))))

(deftest sensedAt-sees-a-block-changed-within-the-same-tick
  (let [{:keys [p per]} (rig {"0,65,-5" "gold_block"})
        wrapped (perception/wrap p per)
        at #(.sensedAt wrapped #js {:x 0 :y 65 :z -5})]
    (turn! p 180)
    (perception/pass! per)
    (is (= "gold_block" (.-name (at))))
    (fake/set-block! p [0 65 -5] "iron_block")
    (is (= "iron_block" (.-name (at))))))

(deftest the-seeing-curve-is-the-view-lightmap
  (are [sky block time rain]
       (let [darken (.skyDarken ^js @shading time rain 0)]
         (and (< (js/Math.abs (- (perception/sky-darken time rain 0) darken)) 1e-9)
              (< (js/Math.abs (- (perception/seeing sky block darken)
                                 (apply max (.lightColor ^js @shading sky block darken)))) 1e-9)))
    0 0 6000 0
    15 0 18000 0
    0 2 18000 0
    0 14 6000 1
    7 3 13000 0.5))

(deftest the-light-rule-is-seeing-at-least-a-fifth
  (is (= [false true true true]
         [(>= (perception/seeing 0 0 1) 0.2) (>= (perception/seeing 0 3 1) 0.2)
          (>= (perception/seeing 15 0 (perception/sky-darken 18000 0 0)) 0.2) (>= (perception/seeing 0 14 1) 0.2)])))

;; ---- review fixes

(deftest ore-under-a-lava-lake-is-unknown
  (let [{:keys [per]} (rig (merge (tu/box -8 60 3 8 66 3 "lava") {"0,65,6" "diamond_ore"}))]
    (perception/pass! per)
    (is (= ["lava" nil] (mapv #(seen-name per %) [[0 65 3] [0 65 6]])))))

(deftest a-cell-keeps-its-own-seen-time-within-a-section
  (let [clock (atom 0)
        {:keys [per]} (rig {"0,64,0" "stone" "1,64,0" "stone"} {:now #(deref clock)})]
    (perception/touch! per [0 64 0])
    (swap! clock + 600000)
    (perception/touch! per [1 64 0])
    (is (= [600000 0] (mapv #(:age-ms (perception/seen-block per %)) [[0 64 0] [1 64 0]])))))

(deftest a-cell-older-than-the-time-range-reads-as-the-oldest-it-can
  (let [clock (atom 0)
        {:keys [per]} (rig {"0,64,0" "stone" "1,64,0" "stone"} {:now #(deref clock)})]
    (perception/touch! per [0 64 0])
    (swap! clock + (* 600 60000))
    (perception/touch! per [1 64 0])
    (is (= [0 true] [(:age-ms (perception/seen-block per [1 64 0]))
                     (>= (:age-ms (perception/seen-block per [0 64 0])) (* 255 60000))]))))

(deftest seen-time-survives-a-save-and-load
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 0)
              {:keys [per]} (rig {"0,64,0" "stone" "1,64,0" "stone"} {:now #(deref clock)})
              file (path/join (tu/tmp-dir) "engine" "seen.bin")
              _ (perception/touch! per [0 64 0])
              _ (swap! clock + 600000)
              _ (perception/touch! per [1 64 0])
              _ (await (perception/save! per @seen-file file))
              fresh (:per (rig {} {:now #(deref clock)}))
              _ (perception/load! fresh @seen-file file)]
          (is (= [600000 0] (mapv #(:age-ms (perception/seen-block fresh %)) [[0 64 0] [1 64 0]]))))))))

(deftest seen-blocks-clamps-its-radius
  (let [{:keys [per]} (rig {"0,64,0" "gold_block" "100,64,0" "gold_block"})]
    (perception/touch! per [0 64 0])
    (perception/touch! per [100 64 0])
    (is (= [[0 64 0]] (mapv :pos (perception/seen-blocks per {:names ["gold_block"] :radius 100000}))))))

(deftest seen-blocks-reports-each-cells-own-age
  (let [clock (atom 0)
        {:keys [per]} (rig {"0,64,1" "gold_block" "1,64,1" "gold_block"} {:now #(deref clock)})]
    (perception/touch! per [0 64 1])
    (swap! clock + 120000)
    (perception/touch! per [1 64 1])
    (is (= [[1 64 1] 0 [0 64 1] 120000]
           (let [rows (sort-by :age-ms (perception/seen-blocks per {:names ["gold_block"]}))]
             [(:pos (first rows)) (:age-ms (first rows)) (:pos (second rows)) (:age-ms (second rows))])))))

(deftest no-pass-starts-until-the-sight-table-is-ready
  (let [ready (atom false)
        p (tu/fake {:blocks (wall 5 8 [] {"0,65,8" "diamond_ore"})})
        raw (js/Object.assign #js {} (fake-raw/create p)
                              #js {:sightTable (fn [] (if @ready (.sightTable ^js (fake-raw/create p)) (js/Uint8Array. 0)))})
        per (perception/create raw {})]
    (perception/pass! per)
    (is (= [0 0] [(:rays (perception/stats per)) (:sections (perception/stats per))]))
    (reset! ready true)
    (perception/pass! per)
    (is (= ["stone" nil] (mapv #(seen-name per %) [[0 65 5] [0 65 8]])))
    (is (pos? (:rays (perception/stats per))))))

(deftest the-bodys-own-dig-and-place-update-memory-in-the-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p per]} (rig {"0,65,3" "stone" "0,64,3" "stone"} {:near 1})
              _ (.setOwner p "t1")
              wrapped (perception/wrap p per)
              _ (light! p {:light-default [0 0]})
              _ (perception/pass! per)
              before (seen-name per [0 65 3])
              _ (await (.dig wrapped "t1" #js {:pos #js {:x 0 :y 65 :z 3}}))
              dug (seen-name per [0 65 3])]
          (is (= [nil "air"] [before dug])))))))

(deftest place-jump-place-and-use-on-touch-the-cell-even-in-the-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p per]} (rig {})
              change (fn [block] (fn [_token a] (fake/set-block! p [(.-x (.-pos a)) (.-y (.-pos a)) (.-z (.-pos a))] block)
                                   (js/Promise.resolve #js {:status "ok"})))
              stub (js/Object.assign #js {} p #js {:place (change "stone") :jumpPlace (change "dirt") :useOn (change "sand")})
              wrapped (perception/wrap stub per)
              _ (light! p {:light-default [0 0]})
              at (fn [x] #js {:pos #js {:x x :y 65 :z 4}})
              _ (await (.place wrapped "t" (at 0)))
              _ (await (.jumpPlace wrapped "t" (at 1)))
              _ (await (.useOn wrapped "t" (at 2)))]
          (is (= ["stone" "dirt" "sand"] (mapv #(seen-name per [% 65 4]) [0 1 2]))))))))

(deftest sight-passes-pause-while-offline-and-restart-after-spawn
  (let [{:keys [p per]} (rig {"0,65,3" "gold_block"} {:pass-ms 1000 :step-ms 50})
        rays #(:rays (perception/stats per))
        _ (dotimes [_ 5] (perception/step! per))
        mid (rays)
        _ (swap! (fake/state p) assoc :offline true)
        _ (dotimes [_ 40] (perception/step! per))
        paused (rays)
        steps-paused (:steps (perception/stats per))
        _ (swap! (fake/state p) assoc :offline false)
        _ (dotimes [_ 20] (perception/step! per))
        after (perception/stats per)]
    (is (pos? mid))
    (is (= mid paused))
    (is (= 5 steps-paused))
    (is (= 1 (:passes after)))
    (is (= (+ mid (:rays-per-pass after)) (:rays after)))
    (is (= "gold_block" (seen-name per [0 65 3])))))

(deftest memory-survives-an-offline-spell-and-saving-offline-is-harmless
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 0)
              {:keys [p per]} (rig {"0,64,0" "stone"} {:now #(deref clock)})
              file (path/join (tu/tmp-dir) "engine" "seen-offline.bin")
              _ (perception/touch! per [0 64 0])
              _ (swap! clock + 120000)
              _ (swap! (fake/state p) assoc :offline true)
              _ (perception/pass! per)
              _ (await (perception/save! per @seen-file file))
              _ (swap! (fake/state p) assoc :offline false)
              fresh (:per (rig {} {:now #(deref clock)}))
              _ (perception/load! fresh @seen-file file)]
          (is (= ["stone" 120000] ((juxt :name :age-ms) (perception/seen-block per [0 64 0]))))
          (is (= ["stone" 120000] ((juxt :name :age-ms) (perception/seen-block fresh [0 64 0])))))))))

(defn dark-seen
  "Names seen at 6 and 8 blocks south in the dark, with the self map merged in."
  [self]
  (let [{:keys [p per]} (rig {"0,65,6" "gold_block" "0,65,8" "gold_block"})]
    (swap! (fake/state p) update :self merge self)
    (light! p {:light-default [0 0]})
    (perception/pass! per)
    (mapv #(seen-name per %) [[0 65 6] [0 65 8]])))

(deftest an-off-hand-torch-widens-dark-sight-to-seven-blocks
  (is (= ["gold_block" nil] (dark-seen {:offhand "torch"})))
  (is (= ["gold_block" nil] (dark-seen {:offhand "soul_torch"}))))

(deftest a-main-hand-torch-widens-dark-sight-and-none-keeps-near
  (is (= ["gold_block" nil] (dark-seen {:held "torch"})))
  (is (= ["gold_block" nil] (dark-seen {:held "soul_torch"})))
  (is (= [nil nil] (dark-seen {}))))

;; ---- equivalence: the seen set of a pass on fixed worlds (jitter fixed), pinned so speed work cannot change it

(defn memory-digest
  "[cells-remembered checksum] over every remembered cell of every section."
  [per]
  (let [^js st (:st per)]
    (reduce (fn [[n sum] ^js sec]
              (let [ids (.-ids sec)]
                (loop [i 0 n n sum sum]
                  (if (>= i 4096)
                    [n sum]
                    (let [id (aget ids i)]
                      (if (zero? id)
                        (recur (inc i) n sum)
                        (recur (inc i) (inc n)
                               (mod (+ sum (* id (+ 1 i (* 4099 (.-cx sec)) (* 7919 (.-cz sec)) (* 104729 (.-sy sec))))) 1000000007))))))))
            [0 0]
            (mapcat #(es6-iterator-seq (.values ^js %)) (es6-iterator-seq (.values (.-stores st)))))))

(def pinned-world
  (merge (tu/box -12 58 -12 12 58 12 "stone")
         (wall 7 9 ["0,65,7" "1,65,7" "0,66,7"] {"0,65,9" "diamond_ore" "3,60,5" "gold_block"})
         {"-4,65,3" "glass" "5,64,4" "dirt" "-6,66,-2" "stone"}))

(deftest pass-seen-set-is-pinned-lit-world
  (let [{:keys [per]} (rig pinned-world {:rand (constantly 0.5)})]
    (perception/pass! per)
    (is (= [674 565560582] (memory-digest per)))))

(deftest pass-seen-set-is-pinned-dark-world-turned
  (let [{:keys [p per]} (rig pinned-world {:rand (constantly 0.5)})]
    (light! p {:light-default [0 0]})
    (turn! p 90)
    (perception/pass! per)
    (is (= [109 43201189] (memory-digest per)))))

(defn first-match-answer
  "sensed's rule spelt out on the public readers: nil unloaded, else feel!, else glance!, else memory (unknown when never
  seen; remembered however old)."
  [{:keys [raw opts st] :as per} [x y z :as pos]]
  (when (>= (.stateAt raw x y z) 0)
    (or (perception/feel! per pos)
        (perception/glance! per pos)
        (if-let [[id seen-at] (store/remembered st x y z)]
          (assoc (perception/block-of per id pos) :age-ms (- ((:now opts)) seen-at) :visible false)
          {:unknown true :pos pos}))))

(defn answer-kind [m] (cond (nil? m) :none (:unknown m) :unknown (:felt m) :felt (:visible m) :visible :else :remembered))

(deftest sensed-and-sensedAt-answer-and-remember-as-the-first-match-rule
  (let [blocks (merge pinned-world
                      {"0,63,0" "stone" "2,64,1" "oak_door" "-2,64,2" "wheat" "3,64,4" "water" "4,64,4" "water"})
        states {[2 64 1] {:facing "north" :open true :hinge "left" :half "lower"} [-2 64 2] {:age 3}}
        cells (for [x (range -6 7) y (range 62 68) z (range -6 11)] [x y z])
        ;; a pass, 20 s, a turn, then every cell read twice (the second from the tick's cache)
        reads (fn [answer]
                (let [clock (atom 0)
                      {:keys [p per]} (rig blocks {:now (fn [] @clock) :rand (constantly 0.5)})
                      wrapped (perception/wrap p per)]
                  (swap! (fake/state p) update :states merge states)
                  (perception/pass! per)
                  (swap! clock + 20000)
                  (turn! p 150)
                  (let [first-read (mapv #(answer per wrapped %) cells)]
                    {:answers first-read :again (mapv #(answer per wrapped %) cells) :memory (memory-digest per)})))
        as-js (fn [^js o] (when o [(vec (js/Object.keys o)) (js->clj o)]))
        rule (reads (fn [per _ pos] (first-match-answer per pos)))
        sensed (reads (fn [per _ pos] (perception/sensed per pos)))
        sensed-at (reads (fn [_ ^js w [x y z]] (as-js (.sensedAt w #js {:x x :y y :z z}))))]
    (is (= #{:felt :visible :remembered :unknown} (set (map answer-kind (:answers rule)))))
    (is (= rule sensed))
    (is (= (mapv #(as-js (perception/answer-js %)) (:answers rule)) (:answers sensed-at) (:again sensed-at)))
    (is (= (:memory rule) (:memory sensed-at)))))

(defn walk-memory
  "Passes along a walk (one cell east and half a turn each step, the clock moving step-ms between them) over a seeded
  jitter: [state reads, memory digest]."
  [blocks light seed step-ms]
  (let [clock (atom 0) reads (atom 0) rnd (atom seed)
        p (tu/blind (tu/fake {:blocks blocks}))
        raw (fake-raw/create p)
        w (js/Object.assign #js {} raw)
        at (.-stateAt raw)
        _ (set! (.-stateAt w) (fn [x y z] (swap! reads inc) (at x y z)))
        per (perception/create w {:now #(deref clock)
                                  :rand #(let [r (mod (+ (* @rnd 1103515245) 12345) 2147483648)] (reset! rnd r) (/ r 2147483648))})]
    (light! p light)
    (doseq [step (range 8)]
      (swap! (fake/state p) assoc-in [:self :pos] [(- step 3) 64 0])
      (turn! p (* 40 step))
      (swap! clock + step-ms)
      (perception/pass! per))
    [@reads (memory-digest per)]))

(deftest walking-passes-share-marks-and-remember-what-passes-of-their-own-would
  (doseq [light [{} {:light-default [0 0]}]
          seed [1 42]]
    (let [[shared-reads shared] (walk-memory pinned-world light seed 100)
          [own-reads own] (walk-memory pinned-world light seed 3000)]
      (is (= own shared) "the same cells remembered")
      (is (< shared-reads own-reads) "cells recorded from the last spot are not read again"))))

;; ---- a still body in an unchanged world looks again only every :still-ms

(defn passes-after
  "Steps a rig (clock starts at 0, pass at once), then moves the clock to each of times and steps; the pass counts."
  [{:keys [per clock]} times]
  (perception/pass! per)
  (mapv (fn [t]
          (reset! clock t)
          (perception/step! per)
          (while (.-pass ^js (:st per)) (perception/step! per))
          (:passes (perception/stats per)))
        times))

(defn clocked-rig [blocks]
  (let [clock (atom 0)]
    (assoc (rig blocks {:now #(deref clock) :pass-ms 50 :still-ms 30000}) :clock clock)))

(deftest an-unchanged-still-body-skips-idle-passes-until-still-ms
  (let [r (clocked-rig {"0,65,4" "gold_block"})]
    (is (= [1 1 2] (passes-after r [3001 29999 30001])))))

(deftest a-block-change-brings-the-next-idle-pass-forward
  (let [{:keys [p] :as r} (clocked-rig {"0,65,4" "gold_block"})]
    (perception/pass! (:per r))
    (fake/set-block! p [0 65 -40] "stone")
    (reset! (:clock r) 3001)
    (perception/step! (:per r))
    (while (.-pass ^js (:st (:per r))) (perception/step! (:per r)))
    (is (= 2 (:passes (perception/stats (:per r)))))))

(deftest a-turn-or-a-change-of-daylight-starts-a-pass-at-once
  (let [{:keys [p clock per]} (clocked-rig {"0,65,4" "gold_block"})]
    (perception/pass! per)
    (reset! clock 100)
    (turn! p 90)
    (perception/step! per)
    (while (.-pass ^js (:st per)) (perception/step! per))
    (is (= 2 (:passes (perception/stats per))))
    (swap! (fake/state p) assoc :time 18000)
    (reset! clock 3200)
    (perception/step! per)
    (while (.-pass ^js (:st per)) (perception/step! per))
    (is (= 3 (:passes (perception/stats per))))))

(deftest stats-count-passes-and-rays-by-the-reason-they-started
  (let [{:keys [p clock per]} (clocked-rig {"0,65,4" "gold_block"})
        run! #(do (perception/step! per) (while (.-pass ^js (:st per)) (perception/step! per)))
        why #(:why (perception/stats per))]
    (run!)
    (reset! clock 100)
    (turn! p 90)
    (run!)
    (reset! clock 3300)
    (fake/set-block! p [0 65 -40] "stone")
    (run!)
    (reset! clock 40000)
    (run!)
    (perception/pass! per)
    (perception/pass! per {:why :look-at})
    (is (= {:first 1 :turned 1 :changed 1 :still 1 :forced 1 :look-at 1} (update-vals (why) :passes)))
    (is (= (:rays (perception/stats per)) (reduce + (map :rays (vals (why))))))))

(deftest a-pass-jitters-its-rays-by-the-rand-opt
  (let [calls (atom 0)
        {:keys [per]} (rig {} {:rand #(do (swap! calls inc) 0.5)})]
    (perception/pass! per)
    (perception/pass! per)
    (is (= 4 @calls))))

(deftest seen-blocks-without-a-filter-leaves_out_air
  (let [{:keys [per]} (rig {"0,65,3" "gold_block" "3,65,8" "stone"})]
    (perception/pass! per)
    (let [names (set (map :name (perception/seen-blocks per {:radius 16 :max 64})))]
      (is (contains? names "gold_block"))
      (is (not-any? #{"air" "cave_air" "void_air"} names)))))

(deftest seen-blocks-keeps-only-the-nearest-max-in-order
  (let [blocks (into {} (map (fn [i] [(str (- i 6) ",65,4") "gold_block"])) (range 12))
        {:keys [per]} (rig blocks)]
    (perception/pass! per)
    (let [rows (perception/seen-blocks per {:names ["gold_block"] :radius 20 :max 3})]
      (is (= [0 65 4] (:pos (first rows))))
      (is (= #{[-1 65 4] [1 65 4]} (set (map :pos (rest rows))))))))

;; ---- ground rows: far ground at a grazing angle is seen in one pass, at every jitter and every effort

(def jitters (map #(/ % 20) (range 20)))

(defn seen-in-one-pass
  "For each jitter, whether one pass at effort sees any of cells (state names seen, or nil)."
  [blocks effort cells]
  (vec (for [j jitters]
         (let [{:keys [per]} (rig blocks {:rand (constantly j)})]
           (perception/pass! per {:effort effort})
           (boolean (some #(seen-name per %) cells))))))

(def pond-cells (for [x [-1 0 1] z [23 24 25]] [x 63 z]))

(def pond-world
  (merge (tu/floor -20 0 20 40) (tu/floor 62 -2 22 2 26)
         (into {} (map (fn [[x y z]] [(str x "," y "," z) "water"])) pond-cells)))

(deftest a-three-wide-pond-23-blocks-out-is-seen-in-one-pass-at-every-jitter-and-effort
  (is (= (repeat 20 true) (seen-in-one-pass pond-world 0 pond-cells)))
  (is (= (repeat 20 true) (seen-in-one-pass pond-world 1 pond-cells))))

(deftest ground-rows-see-nothing-behind-a-wall
  (let [open (merge (tu/floor -20 0 20 40) {"0,63,20" "gold_block"})
        walled (merge open (tu/box -8 64 12 8 67 12 "stone"))]
    (is (= (repeat 20 true) (seen-in-one-pass open 1 [[0 63 20]])))
    (is (= (repeat 20 false) (seen-in-one-pass walled 1 [[0 63 20]])))))

(def tunnel-world
  (apply dissoc (tu/box -3 63 -2 3 66 34 "stone") (for [y [64 65] z (range -2 35)] (str "0," y "," z))))

(deftest a-one-wide-tunnels-walls-and-ceiling-are-seen-far-down-it-in-one-pass-at-every-jitter
  (let [lining (for [z (range 5 28) [x y] [[-1 64] [-1 65] [1 64] [1 65] [0 66]]] [x y z])
        behind (for [z (range 5 28) x [-2 2] y [64 65]] [x y z])]
    (is (= (repeat 20 []) (for [j jitters]
                            (let [{:keys [per]} (rig tunnel-world {:rand (constantly j)})]
                              (perception/pass! per)
                              (vec (remove #(seen-name per %) lining))))))
    (is (= (repeat 20 []) (for [j jitters]
                            (let [{:keys [per]} (rig tunnel-world {:rand (constantly j)})]
                              (perception/pass! per)
                              (vec (filter #(seen-name per %) behind))))))))

(deftest a-lower-effort-casts-fewer-rays
  (let [rays (fn [effort]
               (let [{:keys [per]} (rig (tu/floor -20 0 20 40) {:rand (constantly 0.5)})]
                 (perception/pass! per {:effort effort})
                 (:rays (perception/stats per))))]
    (is (< (rays 0) (rays 0.5) (rays 1)))))

(deftest a-mob-is-forgotten-at-the-speed-the-cost-library-chases-it
  (are [mob-name] (= (* 1000 (/ 20 (danger/chase-speed mob-name))) (mobs/mob-forget-ms {:opts {:mob-drift 20}} mob-name))
    "spider" "zombie" "skeleton" "pillager" "guardian"))


(deftest a-reusing-pass-from-the-same-look-casts-nothing-until-the-world-changes
  (let [clock (atom 0)
        {:keys [p per]} (rig {"0,65,3" "gold_block"} {:now #(deref clock)})
        reuse #(perception/pass! per {:reuse? true})
        _ (reuse)
        rays #(:rays (perception/stats per))
        r1 (rays)
        again (reuse)
        _ (fake/set-block! p [0 66 3] "iron_block")
        _ (reuse)
        r3 (rays)
        _ (swap! clock + 3000)
        _ (reuse)]
    (is (pos? r1))
    (is (true? again))
    (is (> r3 r1) "a block change in range casts again")
    (is (> (rays) r3) "after the generation, casts again")))

(deftest a-targeted-look-records-only-wanted-cells-in-line-of-sight
  (let [{:keys [per]} (rig (wall 5 8 ["0,65,5" "0,65,6" "0,65,7"]
                                 {"0,65,8" "diamond_ore" "1,65,3" "gold_block" "0,65,-3" "diamond_ore" "2,65,4" "lava"
                                  "-2,65,9" "diamond_ore"}))
        _ (perception/look-for! per {:names #{"diamond_ore"} :radius 16})
        rays (:rays (perception/stats per))]
    (is (= "diamond_ore" (seen-name per [0 65 8])) "wanted and in view")
    (is (= "lava" (seen-name per [2 65 4])) "hazards are always wanted")
    (is (nil? (seen-name per [1 65 3])) "not wanted: left to the ambient pass")
    (is (nil? (seen-name per [0 65 -3])) "behind the body")
    (is (nil? (seen-name per [-2 65 9])) "behind the stone")
    (is (= 3 rays) "one ray check per candidate in the cone, not a pass")))

(deftest a-targeted-look-at-cells-sees-them-whatever-they-hold-and-not-through-walls
  (let [{:keys [per]} (rig (wall 5 8 [] {"1,65,3" "gold_block" "0,65,8" "diamond_ore"}))]
    (perception/look-for! per {:cells [[1 65 3] [0 65 8] [0 65 3]]})
    (is (= "gold_block" (seen-name per [1 65 3])))
    (is (= "air" (seen-name per [0 65 3])) "an asked-for cell of air is seen too")
    (is (nil? (seen-name per [0 65 8])) "behind the stone")))

(deftest a-room-answer-is-judged-again-only-when-a-remembered-doors-change-chance-moves-a-level
  (let [{:keys [p per clock]} (clocked-rig {"0,65,4" "oak_door"})
        w (perception/wrap p per)
        kept #(reach-room/kept w "k" (fn [_] #js {}))
        due (memory-age/due-age-ms memory-age/DOOR 0)]
    (perception/pass! per)
    (let [a (kept)]
      (reset! clock (dec due))
      (is (identical? a (kept)) "the door's chance has not moved a level: kept")
      (reset! clock due)
      (is (not (identical? a (kept))) "its first level: judged again"))))

(deftest a-room-answer-is-kept-until-memory-or-the-world-changes
  (let [{:keys [p per clock]} (clocked-rig {"0,65,4" "gold_block"})
        w (perception/wrap p per)
        kept #(reach-room/kept w "k" (fn [_] #js {}))]
    (perception/pass! per)
    (let [a (kept)]
      (reset! clock 3000)
      (turn! p 30)
      (is (identical? a (kept)) "seconds later with the eye turned and nothing new seen: no new flood")
      (turn! p 180)
      (perception/pass! per)
      (let [b (kept)]
        (is (not (identical? a b)) "a turn that shows new cells: judged again")
        (fake/set-block! p [0 65 -3] "stone")
        (is (not (identical? b (kept))) "a block changed: judged again")))))
