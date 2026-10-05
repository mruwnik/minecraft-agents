(ns engine.perception-test
  (:require [cljs.test :refer [deftest is are async]]
            ["path" :as path]
            [engine.fake :as fake]
            [engine.fake.node :as node]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]))

;; The body stands at [0 64 0], eye at (0.5, 65.62, 0.5), facing south (+z) unless a test turns it.

(def shading (delay (node/require-here "../tools/view/web/shading.mjs")))
(def seen-file (delay (node/require-here "./js/seen-file.mjs")))

(defn rig
  ([blocks] (rig blocks {}))
  ([blocks opts]
   (let [p (tu/fake {:blocks blocks})
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

(deftest in-the-dark-nothing-past-two-blocks-is-named-and-a-torch-lit-block-is
  (let [{:keys [p per]} (rig {"-1,65,2" "gold_block" "1,65,7" "gold_block"})]
    (light! p {:light-default [0 0]})
    (perception/pass! per)
    (is (= ["gold_block" nil nil] (mapv #(seen-name per %) [[-1 65 2] [1 65 7] [0 65 5]])))
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

(deftest seen-block-carries-age-and-properties
  (let [clock (atom 5000)
        {:keys [per]} (rig {"0,65,3" "oak_log"} {:now #(deref clock)})]
    (perception/pass! per)
    (swap! clock + 700)
    (is (= {:name "oak_log" :pos [0 65 3] :properties {:axis "y"} :age-ms 700}
           (perception/seen-block per [0 65 3])))))

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
                (remove perception/touching-primitives (js/Object.keys p))))
    (is (= "diamond_ore" (.-name (.blockAt wrapped #js {:x 0 :y 65 :z 8}))))
    (is (.-unknown (.seenBlockAt wrapped #js {:x 0 :y 65 :z 8})))
    (is (= "stone" (.-name (.seenBlockAt wrapped #js {:x 0 :y 65 :z 5}))))))

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
        (let [{:keys [p per]} (rig {"0,65,3" "stone" "0,64,3" "stone"})
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
