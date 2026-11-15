(ns engine.spawn-light-test
  "jobs.lib.spawn-light/exposure: the spawn chance a walk to a place meets, by chance-at, and what lowers it."
  (:require [cljs.test :refer [are deftest is]]
            [clojure.string :as str]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.store :as store]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.seen-light :as seen-light]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spawn-light :as spawn-light]))

(def open-night-chance (cost/spawn-chance [0 64 0]))
(def wide-night-chance (cost/spawn-chance [0 64 0] {:wide? true}))

(def pit
  "A 1x1 pit with a roof: dirt round the feet and head cells, a roof over the head."
  (merge (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str dx "," y "," dz) "dirt"]))
         {"0,66,0" "dirt"}))

(deftest exposure-of-a-walk-out-of-shelter-is-the-open-sky-night-figure-lowered-by-dawn
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))]
    (let [e (spawn-light/exposure p {:x 15 :y 64 :z 0})]
      (is (= #{:light} (:lowered-by e)))
      (is (< (* 0.5 open-night-chance) (:chance e) open-night-chance)
          "the open-sky figure, but for the chance full day leaves the unseen cell dark: then likely rock")
      (is (pos? (:light-in e)) "and the seconds till light"))))

(deftest an-unseen-cell-dark-even-by-day-counts-at-the-chance-it-is-open-ground-in-a-hall-walked-stayed-at-or-aimed-for
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))
        cell {:x 15 :y 64 :z 0}
        share (fn [solid lit-day] (with-redefs [look/unseen-solid-at-fn (constantly (constantly solid))
                                                look/unseen-lit-chance (fn ([_ _] 0) ([_ _ subtract] ({0 lit-day} subtract 0)))]
                                    (spawn-light/dark-share p cell false)))]
    (are [solid lit-day expected] (> 1e-9 (js/Math.abs (- expected (share solid lit-day))))
      1 0 0
      0.75 0 (* 0.25 (cost/hall-chance :roofed))
      0 0 (cost/hall-chance :roofed)
      1 1 1)
    (is (< (share 0 0) 1) "an unseen edge's hall is a prior under 1")))

(deftest a-share-asks-the-rock-chance-of-all-its-unseen-cells-from-one-fn
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))
        built (atom 0)
        make look/unseen-solid-at-fn
        walk (spawn-light/walked {:x 0 :y 64 :z 0} (map (fn [x] {:x x :y 64 :z 0}) (range 1 25)))]
    (with-redefs [look/unseen-solid-at-fn (fn [p] (swap! built inc) (make p))]
      (spawn-light/spawns-over p walk (spawn-light/share-of p false nil)))
    (is (= 1 @built) "built once for the walk, not once per cell")))

(deftest exposure-of-a-walk-out-of-shelter-judges-a-cell-by-its-light-only-once-seen
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))
        exposure (fn [seen? to] (with-redefs [sh-sense/seen-cell? (constantly seen?) sh-sense/block-light-at (constantly 14)]
                                  (spawn-light/exposure p to)))]
    (is (= {:chance 0.0 :lowered-by #{}} (exposure true {:x 3 :y 64 :z 0})) "a lit cell seen is made out")
    (is (pos? (:chance (exposure false {:x 3 :y 64 :z 0}))) "a lit cell not seen stays unknown, so dark")))

(deftest exposure-of-a-seen-log-under-unseen-ones-is-judged-by-the-seen-air-over-the-pile
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))]
    (is (= [[3 67 0]] (with-redefs [sh-sense/solid-at? (fn [_ c] (= 64 (:y c)))
                                    sh-sense/seen-cell? (fn [_ c] (or (= 64 (:y c)) (= 67 (:y c))))]
                        (:look-at (spawn-light/exposure p {:x 3 :y 64 :z 0})))))))

(deftest exposure-of-a-walk-in-the-open-is-the-spawn-chance-at-its-end
  (let [night (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))
        day (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/noon :self {:pos {:x 0 :y 64 :z 0}}}))
        seen (fn [p block sky] (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly block) sh-sense/sky-light-at (constantly sky) sh-inside/walk-leaves-shelter? (constantly nil)]
                                 (spawn-light/exposure p {:x 15 :y 64 :z 0})))]
    (is (= {:chance (spawn-light/chance-at night {:x 15 :y 64 :z 0} false) :lowered-by #{:look} :look-at [[15 64 0]]}
           (spawn-light/exposure night {:x 15 :y 64 :z 0}))
        "an unseen cell is priced by its guessed light, and looking is what lowers that")
    (is (= {:chance 0.0 :lowered-by #{}} (seen night 8 15)) "a seen lit cell spawns nothing")
    (is (= #{} (:lowered-by (seen day 0 0))) "a seen dark cave cell by day: nothing foreseeable lowers it")))

(deftest exposure-takes-the-shelter-entry-of-the-body
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,67,0" "stone"}}))]
    (with-redefs [sh-inside/walk-leaves-shelter? (fn [_ shelter _ _] (when (= :entry shelter) :dug-in))]
      (is (= #{:light} (:lowered-by (spawn-light/exposure p {:x 15 :y 64 :z 0} :entry))))
      (is (= #{:look} (:lowered-by (spawn-light/exposure p {:x 15 :y 64 :z 0})))))))

(deftest exposure-floors-the-goal-and-takes-a-column-at-the-feet-height
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))]
    (is (= {:chance (spawn-light/chance-at p {:x 15 :y 64 :z 0} false) :lowered-by #{:look} :look-at [[15 64 0]]}
           (spawn-light/exposure p {:x 15.5 :z 0.2})))))

(deftest exposure-of-a-solid-seen-goal-is-judged-by-the-air-over-it
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"15,64,0" "grass_block"}}))
        sky (fn [_ at] (if (= 64 (:y at)) 0 15))]
    (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly 0)
                  sh-sense/sky-light-at sky sh-inside/walk-leaves-shelter? (constantly nil)]
      (is (= #{:light} (:lowered-by (spawn-light/exposure p {:x 15 :y 64 :z 0}))) "the grass cell is dark by day too; the air over it lowers at dawn")
      (is (= (spawn-light/exposure p {:x 15 :y 65 :z 0}) (spawn-light/exposure p {:x 15 :y 64 :z 0}))))))

(deftest exposure-of-a-dark-cell-is-lowered-by-dawn-where-the-sky-reaches-down-its-column
  (let [at (fn [sky blocks] (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks blocks}))]
                              (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly 0)
                                            sh-sense/sky-light-at (constantly sky) sh-inside/walk-leaves-shelter? (constantly nil)]
                                (:lowered-by (spawn-light/exposure p {:x 15 :y 64 :z 0})))))]
    (is (= #{:light} (at 15 {})) "open sky: the night is lifted at dawn")
    (is (= #{:light} (at 13 {"15,70,0" "oak_leaves"})) "under a canopy the sky is dimmed but dawn still lights the floor")
    (is (= #{} (at 12 {"15,67,0" "stone"})) "a stair cell the sky reaches only round a corner: dawn is no cure, only threats count")
    (is (= #{} (at 0 {})))))

(deftest a-seen-cell-the-dusk-sky-leaves-dark-is-priced-dark
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/noon :self {:pos {:x 0 :y 64 :z 0}}}))
        at (fn [f subtract] (with-redefs [sh-sense/sky-subtract-now (constantly subtract) sh-sense/seen-cell? (constantly true)
                                          sh-sense/block-light-at (constantly 0) sh-sense/sky-light-at (constantly 13)
                                          sh-inside/walk-leaves-shelter? (constantly nil)]
                              (f)))
        cell {:x 15 :y 64 :z 0}]
    (is (pos? (at #(spawn-light/chance-at p cell false) 6)) "a canopy floor, sky light 13, at dusk")
    (is (pos? (:chance (at #(spawn-light/exposure p cell) 6))) "and readiness prices it so")
    (is (zero? (at #(spawn-light/chance-at p cell false) 0)) "lit at noon")))

(deftest a-walked-way-is-timed-at-walk-speed-its-length-is-already-the-path
  (let [cells (spawn-light/walked {:x 0 :y 64 :z 0} [{:x 10 :y 64 :z 0}])]
    (is (< (js/Math.abs (- (:t (peek cells)) (/ 10 cost/walk-speed))) 1e-9))))

(deftest walks-sharing-a-share-of-read-each-cell-once-and-count-as-apart
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))
        walk (fn [dz] (spawn-light/walked {:x 0 :y 64 :z 0} (map (fn [x] {:x x :y 64 :z dz}) (range 1 13))))
        apart (mapv #(spawn-light/spawns-over p (walk %) nil) [0 0 1])
        reads (atom 0)
        reaches? sh-sense/sky-reaches-from-above?
        shared (with-redefs [sh-sense/sky-reaches-from-above? (fn [p cell] (swap! reads inc) (reaches? p cell))]
                 (let [share (spawn-light/share-of p false nil)]
                   (mapv #(spawn-light/spawns-over p (walk %) share) [0 0 1])))]
    (is (pos? (first apart)) "a night walk meets spawns")
    (is (= apart shared) "the same counts")
    (is (= 6 @reads) "every 4th cell of each of the two ways, the same walk again none")))

(defn walled
  "Stone walls two high round the cells x and z -r..r (feet y 64), with the cells of gap left out."
  [r gap]
  (into {} (for [x (range (- -1 r) (+ r 2)) z (range (- -1 r) (+ r 2)) y [64 65]
                 :when (and (or (= (js/Math.abs x) (inc r)) (= (js/Math.abs z) (inc r))) (not (gap [x z])))]
             [(str x "," y "," z) "stone"])))

(defn hall
  "A stone hall one wide along x 0..len-1 (z 0, feet y 64), roofed at y 66 and shut at both ends."
  [len]
  (merge (tu/box -1 64 -1 len 65 -1 "stone") (tu/box -1 64 1 len 65 1 "stone") (tu/box -1 66 -1 len 66 1 "stone")
         (tu/box -1 64 0 -1 65 0 "stone") (tu/box len 64 0 len 65 0 "stone")))

(deftest a-dark-cell-counts-only-where-a-mob-walks-to-it-from-dark-floor-past-24-blocks-roofed-or-not
  (are [blocks block why expected]
       (= [why expected] [why (with-redefs [sh-sense/block-light-at (constantly block) sh-sense/sky-light-at (constantly 15)]
                                (spawn-light/chance-at (sky/with-sky (tu/fake {:floor [-30 -30 30 30] :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks blocks}))
                                                       {:x 0 :y 64 :z 0} false))])
    (walled 14 #{}) 0 "a walled yard of 841 cells, all within 24 blocks: nothing spawns that reaches the body" 0
    (walled 14 #{[15 0]}) 0 "a gap in its wall lets in the dark field beyond" open-night-chance
    {} 0 "a dark open field" wide-night-chance
    {} 14 "a lit open field, by its light" 0
    (hall 30) 0 "a roofed hall reaching past 24 blocks" open-night-chance
    (hall 10) 0 "a short shut hall" 0))

(deftest a-dark-hall-whose-far-end-the-body-has-not-seen-is-priced-by-the-measured-chance-it-goes-on-past-24-blocks
  (let [chance (with-redefs [sh-sense/block-light-at (constantly 0) sh-sense/sky-light-at (constantly 0)]
                 (spawn-light/chance-at (sky/with-sky (tu/fake {:floor [-30 -30 10 30] :time st/night :self {:pos {:x 0 :y 64 :z 0}}
                                                                :blocks (into {} (remove #(str/starts-with? (key %) "11,")) (hall 11))}))
                                        {:x 0 :y 64 :z 0} false))]
    (is (< 0 chance open-night-chance) "neither free nor a hall for sure")
    (is (= (* (cost/hall-chance :roofed) open-night-chance) chance))))

(deftest a-dim-stay-is-priced-like-a-lit-one-by-the-dark-floor-that-may-lie-round-it
  (let [chance (fn [block] (with-redefs [sh-sense/block-light-at (constantly block) sh-sense/sky-light-at (constantly 15)]
                             (spawn-light/night-chance (sky/with-sky (tu/fake {:floor [-30 -30 30 30] :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))
                                                       {:x 0 :y 64 :z 0})))]
    (is (= wide-night-chance (chance 0)) "dark floor seen round the cell, a wide view over it")
    (is (pos? (chance 4)) "a dim cell is no spawn cell, but the floor round it may be dark past what is seen")
    (is (= (chance 14) (chance 4)))))

(deftest a-stay-with-the-view-shut-is-priced-at-the-pooled-rate-and-with-a-wide-view-at-the-wide-one
  (let [chance (fn [solid?] (with-redefs [sh-sense/block-light-at (constantly 0) sh-sense/sky-light-at (constantly 15)
                                          sh-sense/solid-at? (fn [_ {:keys [x y]}] (and (= 65 y) (solid? x)))]
                              (spawn-light/night-chance (sky/with-sky (tu/fake {:floor [-30 -30 30 30] :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))
                                                        {:x 0 :y 64 :z 0})))]
    (is (< open-night-chance wide-night-chance))
    (is (= wide-night-chance (chance (constantly false))))
    (is (= open-night-chance (chance #(>= (js/Math.abs %) 3))) "a wall three blocks round shuts the sight lines")))

(deftest a-cells-wide-view-is-read-once-while-the-memory-stays
  (let [raw-p (tu/fake {:floor [-16 -16 48 48] :time st/night :self {:pos {:x 4.5 :y 64 :z 16.5}}})
        _ (swap! (fake/state raw-p) merge {:light-default [15 0]})
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000) :rand (constantly 0.5)})
        p (perception/wrap raw-p per)
        reads (atom 0)
        view cost/wide-view?]
    (doseq [yaw [0 90 180 270]]
      (swap! (fake/state raw-p) assoc :yaw yaw)
      (perception/pass! per))
    (with-redefs [cost/wide-view? (fn [& args] (swap! reads inc) (apply view args))]
      (is (= wide-night-chance (spawn-light/chance-at p {:x 30 :y 64 :z 16} false)) "a wide view is read")
      (spawn-light/chance-at p {:x 30 :y 64 :z 16} false)
      (spawn-light/night-chance p {:x 30 :y 64 :z 16})
      (is (= 1 @reads)))))

(deftest a-night-spawn-price-over-open-ground-seen-at-full-sky-relights-no-section
  ;; the memory's light of open ground seen at full sky is sky 15 and, with no light source, block 0, and of the ground
  ;; under it its own: no section is flooded for the dark-hall flood, its unseen floors' frontier nor a full day's guess
  (let [raw-p (tu/fake {:floor [-16 -16 48 48] :time st/night :self {:pos {:x 4.5 :y 64 :z 16.5}}})
        _ (swap! (fake/state raw-p) merge {:light-default [15 0]})
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000) :rand (constantly 0.5)})
        p (perception/wrap raw-p per)
        ^js table @fake-raw/light-table
        floods (atom 0)]
    (doseq [yaw [0 90 180 270]]
      (swap! (fake/state raw-p) assoc :yaw yaw)
      (perception/pass! per))
    (set! (.-lightTable ^js (:st per)) (js/Object.assign #js {} table #js {:sectionLight (fn [t ids bits] (swap! floods inc) (.sectionLight table t ids bits))}))
    (is (= wide-night-chance (spawn-light/chance-at p {:x 30 :y 64 :z 16} false)) "dark open ground")
    (is (pos? (look/unseen-lit-chance p {:x 4 :y 64 :z 90} 0)) "full day may light the ground past the seen field")
    (is (zero? @floods))))

(deftest the-memory-light-shortcuts-equal-the-full-relight-of-every-cell-by-day-and-night
  ;; the opaque-cell and full-sky shortcuts of seen-light/light-fn read what light.mjs's sectionLight gives: a wrong
  ;; shortcut (an opaque cell read as sky, a full-sky cell given the wrong block light) differs on some cell here
  (let [^js table @fake-raw/light-table
        id (fn [block] (inc (fake-raw/state-of block)))
        put! (fn [^js sec x y z block] (aset (.-ids sec) (store/cell-index x y z) (id block)))
        sky! (fn [^js bits x y z] (let [i (store/cell-index x y z)] (aset bits (bit-shift-right i 3) (bit-or (aget bits (bit-shift-right i 3)) (bit-shift-left 1 (bit-and i 7))))))
        ids (js/Uint16Array. 4096)
        bits (js/Uint8Array. 512)
        sec #js {:ids ids :sky bits :wrote 1}
        cells (for [x (range 16) y (range 16) z (range 16)] [x y z])]
    (.fill ids (id "air"))
    (doseq [[x y z] cells]
      (when (or (>= y 11) (>= x 8)) (sky! bits x y z))
      (when (= y 0) (put! sec x y z "stone"))
      (when (and (= y 10) (< x 8)) (put! sec x y z "stone"))
      (when (and (<= 1 y 2) (<= 2 x 5) (<= 2 z 5)) (put! sec x y z "water"))
      (when (and (<= 12 y 13) (<= 9 x 12) (<= 4 z 8)) (put! sec x y z "oak_leaves"))
      (when (and (= y 12) (= x 11) (= z 6)) (sky! bits x y z))
      (when (and (= y 1) (= x 6) (= z 8)) (put! sec x y z "torch"))
      (when (and (= y 5) (= x 13) (= z 13)) (put! sec x y z "glowstone"))
      (when (and (= y 3) (= x 14)) (sky! bits x y z))
      (when (and (= y 11) (= x 3) (= z 3)) (put! sec x y z "stone"))
      (when (and (= y 6) (= x 1) (= z 1)) (aset ids (store/cell-index x y z) 0)))
    (let [st #js {:stores (js/Map.) :dim "overworld" :writes 1 :lightTable table}
          p #js {:perception {:st st}}
          key (store/section-key 0 0 0)
          full (fn [with-sky?] (.sectionLight table table
                                              (let [a (js/Array. 27)] (.fill a nil) (aset a 13 ids) a)
                                              (let [a (js/Array. 27)] (.fill a nil) (when with-sky? (aset a 13 bits)) a)))
          day (full true)
          night (full false)
          f-day (do (.set (store/store-of st "overworld") key sec) (seen-light/light-fn p))
          f-night (seen-light/light-fn p true)
          wrong (fn [expected f] (for [[x y z] cells
                                       :let [want (if (zero? (aget ids (store/cell-index x y z))) -1 (aget expected (store/cell-index x y z)))]
                                       :when (not= want (f x y z))]
                                   [x y z want (f x y z)]))]
      (is (= [] (take 5 (wrong day f-day))) "day: sky and block light")
      (is (= [] (take 5 (wrong night f-night))) "night: block light alone")
      (is (< 0 (aget day (store/cell-index 10 14 5))) "the scene is lit"))))

(deftest a-spawn-region-flooded-in-one-decision-is-kept-for-the-next-while-the-memory-it-read-stays
  (let [raw (tu/fake {:blocks (merge (tu/floor -30 -30 30 30) (tu/box -2 64 -2 2 66 2 "stone") (tu/box -1 64 -1 1 65 1 "air"))
                      :time st/night :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        per (perception/create (fake-raw/create raw) {:now (constantly 1000000) :rand (constantly 0.5)})
        p (perception/wrap raw per)
        floods (atom 0)
        flood sh-inside/region-flood
        region #(binding [sh-inside/*regions* (js/Map.)] (sh-inside/spawn-region p {:x 0 :y 64 :z 0} 11))]
    (swap! (fake/state raw) assoc :yaw 0)
    (perception/pass! per)
    (with-redefs [sh-inside/region-flood (fn [& a] (swap! floods inc) (apply flood a))]
      (let [first-region (region)]
        (is (= 1 @floods))
        (is (identical? first-region (region)) "the next decision reads the kept region")
        (is (= 1 @floods))
        (fake/set-block! raw [1 64 0] "stone")
        (swap! (fake/state raw) assoc :yaw 90)
        (perception/pass! per)
        (is (not (identical? first-region (region))) "a write in a section it read floods it anew")
        (is (= 2 @floods))))))
