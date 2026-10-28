(ns engine.find-spot-test
  "jobs.farm.find-spot against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.perception :as perception]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.util :as u]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.farm.find-spot :as fs]
            [jobs.lib.patch :as pt]
            [jobs.lib.farm :as farm]
            [jobs.lib.surface :as surface]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn ^:async child-outcome
  "tu/child-outcome without the status and ending the adapter adds."
  [eng job args n]
  (let [r (await (tu/child-outcome eng job args n))]
    (if (map? r) (dissoc r :status :ending) r)))

(def job 'jobs.farm.find-spot)

(defn patch
  "Blocks of a w x h patch of name at y with NW corner x z."
  [x z w h y name]
  (into {} (for [dx (range w) dz (range h)] [(str (+ x dx) "," y "," (+ z dz)) name])))

(def small {:w 3 :h 3 :range 6})

(defn ice-path
  "Ice at y 63 from x0 to x1, z -1 to 2: ground to walk on that is no spot (an ice top is not a floor to farm)."
  [x0 x1]
  (tu/box x0 63 -1 x1 63 2 "ice"))

;; ---- pure

(defn names-of [m] (fn [{:keys [x y z]}] (get m [x y z] "air")))

(deftest column-top-table
  (are [m expected] (= expected (surface/column-top (names-of m) 0 0 64 12))
    {[0 63 0] "dirt"} 63
    {[0 63 0] "dirt" [0 60 0] "stone"} 63
    {[0 70 0] "stone"} 70
    {} nil
    {[0 63 0] "water"} nil
    {[0 63 0] "lava"} nil
    {[0 63 0] "ice"} nil
    {[0 63 0] "magma_block"} nil
    {[0 63 0] "powder_snow"} nil
    {[0 63 0] "dirt" [0 64 0] "stone"} 64
    {[0 63 0] "grass_block" [0 64 0] "short_grass"} 63
    {[0 63 0] "grass_block" [0 64 0] "poppy"} 63
    {[0 63 0] "grass_block" [0 64 0] "tall_grass" [0 65 0] "tall_grass"} 63
    {[0 63 0] "dirt" [0 64 0] "water" [0 65 0] "air"} nil
    {[0 40 0] "dirt"} nil
    {[0 53 0] "dirt"} 53
    {[0 52 0] "dirt"} nil
    {[0 76 0] "dirt"} 76
    {[0 77 0] "dirt"} nil
    {[0 63 0] "dirt" [0 64 0] "oak_log" [0 65 0] "oak_log" [0 66 0] "oak_leaves"} 63
    {[0 63 0] "dirt" [0 68 0] "birch_leaves" [0 69 0] "birch_leaves"} 63
    {[0 60 0] "dirt" [0 65 0] "birch_leaves"} 60
    {[0 63 0] "dirt" [0 64 0] "oak_leaves"} 63))

(deftest column-top-is-nil-for-unloaded-or-covered
  (is (nil? (surface/column-top (fn [{:keys [y]}] (when (not= y 70) "air")) 0 0 64 12)) "unloaded above the ground")
  (is (nil? (surface/column-top (fn [{:keys [y]}] (cond (= y 63) "dirt" (= y 64) nil :else "air")) 0 0 64 12)) "unloaded above the top")
  (is (= 63 (surface/column-top (fn [{:keys [y]}] (case y 63 "dirt" "cave_air")) 0 0 64 12))))

(deftest column-top-is-the-floor-under-a-cave-ceiling
  (let [cave (fn [roof above] (fn [{:keys [y]}] (cond (= y 63) "dirt" (<= 64 y 67) "air" (= y 68) roof :else above)))]
    (are [roof above] (= 63 (surface/column-top (cave roof above) 0 0 64 12))
      "stone" nil
      "stone" "stone"
      "dirt" "stone")
    (is (nil? (surface/column-top (cave "stone" "stone") 0 0 64 12 false)) "a building's surface never goes down under rock")
    (are [world] (= 63 (surface/column-top world 0 0 64 12 false))
      (fn [{:keys [y]}] (case y 63 "dirt" 64 "air" nil))
      (fn [{:keys [y]}] (case y 63 "dirt" nil)))
    (is (nil? (surface/column-top (fn [{:keys [y]}] (if (= y 63) "water" (if (<= 64 y 67) "air" "stone"))) 0 0 64 12)) "a lake floor is no spot")
    (is (nil? (surface/column-top (constantly "stone") 0 0 64 12)) "solid rock all through")
    (is (nil? (surface/column-top (fn [{:keys [y]}] (cond (= y 63) "dirt" (<= 64 y 67) "air" (<= y 70) "stone" (= y 71) nil :else "air")) 0 0 64 12))
        "a surface with nothing seen above it and rock under it is no roof: the pocket below is not its top")))

(defn cells [n m] (vec (repeat n (merge {:lit? true :wet? true :farmable? true} m))))

(deftest score-patch-prices-the-crops-against-the-work
  (let [w (pt/cell-worth true)
        flat (pt/score-patch {:tops (vec (repeat 9 63)) :cells (cells 9 {}) :sky true :away 0})]
    (is (= {:y 63 :level 100 :work 0 :water-share 1 :sky true :away 0 :worth (js/Math.round (* 9 w))
            :costs {:light 0 :water 0 :replace 0 :earthwork 0 :walk 0} :score (js/Math.round (* 9 w)) :dark? false :farmable 100}
           flat))
    (is (= (js/Math.round (- (* 9 w) (cost/earthwork-s 2) (pt/walk-s 8)))
           (:score (pt/score-patch {:tops (conj (vec (repeat 8 63)) 65) :cells (cells 9 {}) :away 8})))
        "levelling and the walk are seconds off the crops' worth")
    (is (= -12 (:score (pt/score-patch {:tops [63 63] :away 40})))
        "a patch for no crops is worth nothing: its score is its costs")
    (is (nil? (pt/score-patch {:tops [63 nil] :cells (cells 2 {})})))))

(deftest a-cell-is-worth-the-food-its-harvests-give
  (is (= (js/Math.round (* 25 (cost/food-point-worth) (/ 5 3) (cost/harvests "wheat" 0 (crops/stages-per-s true nil))))
         (:worth (pt/score-patch {:tops (vec (repeat 25 63)) :cells (cells 25 {}) :away 0})))
      "a wet wheat cell is a third of a bread per harvest in the food horizon, at the price of food"))

(deftest a-patch-that-does-not-pay-says-why
  (are [in why] (= why (:why (pt/score-patch (merge {:worth 9} in))))
    {:tops (vec (repeat 25 63)) :cells (cells 25 {}) :away 0} nil
    {:tops (vec (repeat 25 63)) :cells (cells 25 {}) :away 400} :far
    {:tops (vec (concat (repeat 13 60) (repeat 12 64))) :cells (cells 25 {}) :away 0} :steep
    {:tops (vec (repeat 9 63)) :cells (cells 9 {:lit? false}) :away 0} :barren
    {:tops (vec (repeat 9 63)) :cells (cells 9 {:farmable? false}) :away 0} :barren
    {:tops [63 64] :away 4000} nil))

(deftest water-is-brought-only-when-some-is-known
  (is (= 0 (:water (:costs (pt/field-price (cells 25 {:wet? false}) 0 (pt/cell-worth true) nil)))) "no water known: none is brought")
  (is (< (:water (:costs (pt/field-price (cells 25 {:wet? false}) 0 (pt/cell-worth true) 0)))
         (:water (:costs (pt/field-price (cells 25 {:wet? false}) 0 (pt/cell-worth true) 20))))
      "water farther off costs its walk"))

(deftest light-and-water-are-bought-only-where-they-pay
  (let [w 1
        dark (fn [n] (pt/field-price (cells n {:lit? false}) 1 w))
        dry (fn [n] (pt/field-price (cells n {:wet? false}) 0 w))]
    (is (not (:lit? (dark 1))) "a crop worth a second does not pay for a torch")
    (is (:lit? (dark 144)) "a dark 12 x 12 field of them pays for its torches")
    (is (= (* 3 (:cost (cost/light-cost {:carried 1}))) (:light (:costs (dark 144)))) "a torch lights pt/torch-cells crops")
    (is (not (:watered? (dry 9))) "nine dry cells do not pay for a bucket of water")
    (is (:watered? (dry 25)))
    (is (< (:net (dry 25)) (:net (pt/field-price (cells 25 {}) 0 w))) "water brought costs what water in reach does not")
    (is (:lit? (pt/field-price (cells 9 {:lit? false}) 1 (pt/cell-worth true))) "at the price of food nine wheat cells pay for a torch")))

(deftest sky-is-reported-not-scored
  (is (= (:score (pt/score-patch {:tops [63 63] :cells (cells 2 {}) :sky true :away 0}))
         (:score (pt/score-patch {:tops [63 63] :cells (cells 2 {}) :sky false :away 0})))))

(deftest hydrated-table
  (are [ws x y z expected] (= expected (farm/hydrated? ws x y z))
    #{[4 63 0]} 0 63 0 true
    #{[4 64 0]} 0 63 0 true
    #{[4 62 0]} 0 63 0 false
    #{[4 65 0]} 0 63 0 false
    #{[5 63 0]} 0 63 0 false
    #{[0 63 -4]} 0 63 0 true
    #{[0 63 5]} 0 63 0 false
    #{} 0 63 0 false))

(deftest max-step-table
  (are [tops h expected] (= expected (pt/max-step tops h))
    [63 63 63 63] 2 0
    [63 64 63 63] 2 1
    [63 63 63 66] 2 3
    [63 63 65 63] 2 2
    [63 70 70 63] 2 7
    [63 63 63 63 63 63 63 63 63] 3 0))

(deftest blocked-fn-leaves-out-plans-and-foreign-zones
  (let [in {:footprints {[5 70 5] "hut"} :zones [{:name "mine" :owner "Me" :min [0 0 0] :max [2 99 2]}
                                                  {:name "theirs" :owner "Other" :min [10 0 0] :max [12 99 2]}]
            :claims [] :self "Me" :now 0}
        b? (pt/blocked-fn in)]
    (are [x y z expected] (= expected (b? x y z))
      5 63 5 true
      6 63 5 false
      1 63 1 false
      11 63 1 true
      13 63 1 false)))

;; ---- job

(def stepped-world
  (merge (patch 2 0 3 3 63 "dirt") {"3,64,1" "dirt" "6,63,1" "water"}
         (patch -4 0 3 3 63 "dirt") {"-3,64,1" "dirt" "-3,65,1" "dirt" "-6,63,1" "water"}))

(deftest max-step-keeps-only-patches-with-small-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks stepped-world})
              all (await (child-outcome eng job small 4))
              r (await (child-outcome eng job (assoc small :max-step 1) 4))]
          (is (= [1 2] (sort (map :step (:spots all)))))
          (is (= [{:x 2 :y 63 :z 0}] (mapv :pos (:spots r)))))))))

(def wet-world
  (merge (patch 2 0 3 3 63 "dirt")
         {"6,63,1" "water"}
         (patch -4 0 3 3 63 "dirt")
         (patch 2 -6 3 3 63 "dirt")
         {"3,64,-5" "dirt" "6,63,-5" "water"}))

(deftest wet-flat-beats-wet-bumpy-and-a-small-dry-patch-pays-for-water-brought
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks wet-world})
              r (await (child-outcome eng job small 4))
              dry (first (filter #(= {:x -4 :y 63 :z 0} (:pos %)) (:spots r)))]
          (is (= {:x 2 :y 63 :z 0} (:spot r)))
          (is (= #{{:x 2 :y 63 :z 0} {:x -4 :y 63 :z 0} {:x 2 :y 63 :z -6}} (set (map :pos (:spots r)))))
          (is (= 0 (:water-share dry)))
          (is (pos? (:water (:costs dry))) "nine dry cells pay for water brought to them"))))))

(deftest never-picks-water-or-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge (patch 2 0 3 3 63 "water") (patch -4 0 3 3 63 "lava"))})
              r (await (child-outcome eng job small 4))]
          (is (= {:spot nil :reason :distance :why :distance} (dissoc r :text))))))))

(deftest no-spot-names-the-patch-fewest-cells-short
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (assoc (patch 2 0 3 3 63 "dirt") "4,63,2" "water")})
              r (await (child-outcome eng job small 4))]
          (is (= {:pos {:x 2 :y 63 :z 0} :w 3 :h 3 :short 1 :unseen 0 :no-floor 1 :dark 0 :foreign 0}
                 (dissoc (:best r) :away)))
          (is (re-find #"nearest to fit: 3x3 at 2 63 0, 1 of 9 cells short: 1 water/lava/ice$" (:text r))))))))

(deftest no-ground-is-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks {}})
              r (await (child-outcome eng job small 4))]
          (is (= {:spot nil :reason :distance :why :distance} (dissoc r :text)))
          (is (re-find #"^no 3x3 patch found \(distance\)" (:text r))))))))

(def bumpy
  "Ground x -12 to 60, z -3 to 3, in a checker of heights 63 and 64: no patch of it is flat."
  (into {} (for [x (range -12 61) z (range -3 4)] [(str x "," (if (even? (+ x z)) 63 64) "," z) "dirt"])))

(deftest explores-to-a-patch-beyond-the-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge bumpy (patch 47 -1 3 3 63 "dirt") (patch 47 -1 3 3 64 "dirt"))})
              r (await (child-outcome eng job (assoc small :max-step 0) 200))]
          (is (= [{:x 47 :y 64 :z -1}] (mapv :pos (take 1 (:spots r))))))))))

(deftest exploring-ends-none-when-no-leg-has-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks bumpy})
              r (await (child-outcome eng job (assoc small :max-step 0) 200))]
          (is (= {:spot nil :reason :distance :why :distance} (dissoc r :text :best))))))))

(deftest exploring-stops-at-the-first-patch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge bumpy (patch 47 -1 3 3 63 "dirt") (patch 47 -1 3 3 64 "dirt")
                                                   (patch 79 -1 3 3 63 "dirt") (patch 79 -1 3 3 64 "dirt"))})
              r (await (child-outcome eng job (assoc small :max-step 0) 200))]
          (is (every? #(< (:x (:pos %)) 60) (:spots r)) "the farther patch is never looked at"))))))

(deftest a-missing-plan-ends-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (patch 4 0 3 3 63 "dirt")})
              r (await (child-outcome eng job {:plan "no-such-plan"} 4))]
          (is (= {:spot nil :reason :no-plan :why :no-plan} (dissoc r :text))))))))

(deftest footprint-table
  (are [cells expected] (= expected (fs/footprint cells))
    [] nil
    [{:pos [3 64 5] :want "farmland"}] {:w 1 :h 1 :farmland? true}
    [{:pos [0 64 0] :want "oak_planks"} {:pos [4 70 2] :want "oak_planks"}] {:w 5 :h 3 :farmland? false}
    [{:pos [0 64 0] :want {:crop "wheat"}} {:pos [1 64 0] :want "water"}] {:w 2 :h 1 :farmland? true}))

(deftest limit-caps-the-spots
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks wet-world})
              r (await (child-outcome eng job (assoc small :settings {:jobs.farm.find-spot/limit 1}) 4))]
          (is (= 1 (count (:spots r)))))))))

(deftest find-spot-does-not-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (patch 4 0 3 3 63 "dirt")})]
          (await (child-outcome eng job (assoc small :range 8) 4))
          (is (empty? (tu/walked-to eng)))
          (is (empty? (tu/walk-calls p))))))))

(defn scan-at [world self args from]
  (let [p (tu/seeing-all (tu/fake {:blocks world :self {:pos self}}))]
    (pt/scan p (merge {:w 3 :h 3 :range 6 :depth 12 :limit 50} args) from)))

(def wet-patch (assoc (patch 2 0 3 3 63 "dirt") "6,63,1" "water"))

(deftest scan-reports-sky-for-open-ground-and-not-under-a-roof
  (let [deep (patch 2 0 3 3 2 "stone")
        open (scan-at (merge wet-patch deep) {:x 0 :y 64 :z 0} {} {:x 0 :y 64 :z 0})
        roofed (scan-at (merge wet-patch {"3,79,1" "oak_leaves"}) {:x 0 :y 64 :z 0} {} {:x 0 :y 64 :z 0})]
    (is (= [true] (mapv :sky open)) "stone far below must not matter")
    (is (= [false] (mapv :sky roofed)) "leaves at from-y + depth + 3 over one column")))

(defn lit-scan [light]
  (let [p (tu/fake {:blocks wet-patch :self {:pos {:x 0 :y 64 :z 0}} :time 6000 :light {} :light-default light})]
    (pt/scan p {:w 3 :h 3 :range 6 :depth 12 :limit 50 :torches 1} {:x 0 :y 64 :z 0})))

(deftest a-small-dark-floor-pays-for-its-torch-and-a-lit-one-needs-none
  (are [light dark] (= [dark] (mapv :dark? (lit-scan light)))
    [0 0] true
    [8 0] true
    [0 8] true
    [0 9] false
    [9 0] false
    [15 0] false))

(deftest a-callers-worth-decides-whether-the-light-pays
  (let [p (tu/fake {:blocks wet-patch :self {:pos {:x 0 :y 64 :z 0}} :time 6000 :light {} :light-default [0 0]})
        scan #(pt/scan p (merge {:w 3 :h 3 :range 6 :depth 12 :limit 50 :torches 1} %) {:x 0 :y 64 :z 0})]
    (is (empty? (scan {:worth 1})))
    (is (= [true] (mapv :dark? (scan {:worth 1000}))))))

(deftest a-big-dark-floor-is-a-spot-lit-with-torches
  (let [scan (fn [light] (pt/scan (tu/fake {:blocks (patch 0 0 12 12 63 "dirt") :self {:pos {:x 6 :y 64 :z 6}} :time 6000 :light {} :light-default light})
                                  {:w 12 :h 12 :range 6 :depth 12 :limit 50 :torches 1} {:x 6 :y 64 :z 6}))
        dark (scan [0 0])
        lit (scan [9 0])]
    (is (= [true] (mapv :dark? dark)) "144 crops pay for 3 torches")
    (is (pos? (:light (:costs (first dark)))))
    (is (= [false] (mapv :dark? lit)) "a lit patch is not marked dark")
    (is (< (:score (first dark)) (:score (first lit))) "a lit patch is preferred")))

(deftest scan-sees-water-sitting-on-the-platform
  (let [world (assoc (patch -3 -3 9 9 99 "dirt") "6,100,0" "water")
        spots (scan-at world {:x 0 :y 100 :z 0} {:range 8} {:x 0 :y 100 :z 0})
        at (fn [x z] (first (filter #(= {:x x :y 99 :z z} (:pos %)) spots)))]
    (is (pos? (:water-share (at 2 -1))) "patch x 2..4 is within 4 of water at x 6")
    (is (= [0 true] ((juxt :water-share (comp pos? :water :costs)) (at -3 -3))) "nine dry cells out of its reach pay for water brought")))

(deftest distance-is-measured-from-center-when-given
  (let [world (merge wet-patch (patch -14 0 3 3 63 "dirt") {"-16,63,1" "water"})
        spots (scan-at world {:x 0 :y 64 :z 0} {:range 20} {:x -12 :y 64 :z 1})]
    (is (= {:x -14 :y 63 :z 0} (:pos (first spots))) "the patch near :center beats the one near the body")
    (is (= [{:x -14 :y 63 :z 0} {:x 2 :y 63 :z 0}] (mapv :pos spots)))
    (is (= [3 3] ((juxt :w :h) (first spots))) "a spot says its size, so :pos reads as the corner of a w x h box")
    (is (= 1 (:away (first spots))) "the distance is from the patch centre (-13,1) to :center (-12,1), not from the corner")))

(deftest a-far-center-is-not-answered-with-ground-by-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge (patch 2 0 3 3 63 "dirt")
                                     (into {} (for [x (range 380 421) z (range -3 4)] [(str x "," (if (even? (+ x z)) 63 64) "," z) "dirt"])))})
              r (await (child-outcome eng job (assoc small :center {:x 400 :y 64 :z 0} :max-step 0) 60))]
          (is (nil? (:spot r)) "ground beside the body is 400 blocks from :center, beyond what the search may cover"))))))

(deftest a-spot-round-a-given-center-lies-within-its-range-not-by-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge bumpy (patch 2 -1 3 3 63 "dirt") (patch 2 -1 3 3 64 "dirt"))})
              r (await (child-outcome eng job (assoc small :center {:x 40 :y 65 :z 0} :max-step 0) 200))]
          (is (nil? (:spot r)) "the flat patch beside the body is 38 blocks from :center, past its :range 6"))))))

(deftest scan-is-one-call-in-slices-of-a-read-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge (patch 2 0 3 3 63 "dirt") (patch -4 4 3 3 63 "dirt") {"6,63,1" "water"})
              a {:w 3 :h 3 :range 10}
              from {:x 0 :y 64 :z 0}
              expected (scan-at world {:x 0 :y 64 :z 0} a from)
              {:keys [eng p]} (setup {:blocks world})
              reads (atom 0)
              orig (.-blockAt p)
              _ (set! (.-blockAt p) (fn [pos] (swap! reads inc) (.call orig p pos)))
              out (atom :not-done)
              parent (tu/recording-parent job a out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))
              ;; one patch row: (2*range+1) z cells x w columns, each reading at most the
              ;; top scan (2*depth) + 1 + the sky scan (2*depth + 8)
              row-reads (* (inc (* 2 (:range a))) (:w a) (+ (* 4 12) 9))
              bound (+ (pt/read-budget) row-reads)]
          (core/submit! eng '(recording-parent) {})
          (await (run-until-empty eng 40))
          (is (empty? (:list (core/state eng))) "the scan ends within a few ticks")
          (is (> @reads bound) (str "the scan read past one slice: " @reads))
          (is (= expected (:spots @out)))
          (is (= (:pos (first expected)) (:spot @out))))))))

(deftest a-scan-over-open-air-stops-at-the-slice-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [a {:w 16 :h 16 :range 48}
              {:keys [eng p]} (setup {:blocks {}})
              reads (atom 0)
              orig (.-blockAt p)
              _ (set! (.-blockAt p) (fn [pos] (swap! reads inc) (.call orig p pos)))
              out (atom :not-done)
              parent (tu/recording-parent job a out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (run-until-empty eng 40))
          (is (empty? (:list (core/state eng))) "the job ends")
          (is (<= @reads 66000) (str "reads over the slice cap: " @reads))
          (is (= :distance (:reason @out))))))))

(deftest a-body-moved-mid-scan-does-not-change-the-rows-scanned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge (patch 2 0 3 3 63 "dirt") (patch -4 4 3 3 63 "dirt") {"6,63,1" "water"})
              a {:w 3 :h 3 :range 10}
              expected (scan-at world {:x 0 :y 64 :z 0} a {:x 0 :y 64 :z 0})
              {:keys [eng p]} (setup {:blocks world})
              out (atom :not-done)
              parent (tu/recording-parent job a out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (swap! (fake/state p) assoc-in [:self :pos] [7 64 5])
          (await (run-until-empty eng 40))
          (is (= expected (:spots @out))))))))

(defn scan-reads
  "[reads spots] of one unbounded scan of world from [x y z]."
  [world self a]
  (let [p (tu/seeing-all (tu/fake {:blocks world :self {:pos self}}))
        reads (atom 0)
        orig (.-blockAt p)
        _ (set! (.-blockAt p) (fn [pos] (swap! reads inc) (.call orig p pos)))
        r (pt/scan p a {:x (first self) :y (second self) :z (nth self 2)})]
    [@reads r]))

(deftest flat-ground-stops-at-the-nearest-good-patches
  (let [flat (tu/box -30 62 -30 30 63 30 "grass_block")
        a {:w 5 :h 5 :range 24 :depth 12 :limit 3}
        [reads spots] (scan-reads flat [0 64 0] a)
        [full-reads] (scan-reads flat [0 64 0] (assoc a :limit 100))]
    (is (= 3 (count spots)))
    (is (every? #(<= (js/Math.abs (- (:x (:pos %)) -2)) 2) spots) "all three lie next to the centre")
    (is (< (* 3 reads) full-reads) (str reads " reads against " full-reads " for the whole range"))))

(deftest open-air-with-a-big-patch-reads-far-fewer-columns
  (let [[reads spots] (scan-reads {} [0 64 0] {:w 16 :h 16 :range 48 :depth 7 :limit 3})]
    (is (empty? spots))
    (is (< reads 20000) (str reads " reads"))))

(deftest a-pond-off-centre-beats-dry-ground-by-the-centre
  (let [world (merge (tu/box -30 62 -30 30 63 30 "grass_block") (tu/box 15 63 0 17 63 2 "water"))
        a {:w 5 :h 5 :range 24 :depth 12 :limit 3}
        [_ spots] (scan-reads world [0 64 0] a)]
    (is (pos? (:water-share (first spots))) "the best spot has water within 4: water brought to the centre walks to the pond and back")))

(deftest find-spot-looks-around-before-it-judges-the-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks wet-world})
              _ (tu/seeing-after-look p)
              r (await (child-outcome eng job small 8))]
          (is (seq (calls p "look")))
          (is (every? pos? (mapv :water-share (take 2 (:spots r))))))))))

(deftest a-stone-patch-is-no-field
  (let [p (tu/seeing-all (tu/fake {:blocks (merge (patch 0 0 3 3 63 "stone") (patch 10 0 3 3 63 "grass_block") {"-2,63,1" "water" "14,63,1" "water"})}))
        r (pt/scan-rows p {:w 3 :h 3 :range 12 :depth 4 :limit 2} {:x 1 :y 64 :z 1} {:next-ring 0 :found []} ##Inf)]
    (is (= [{:x 10 :y 63 :z 0} {:x 0 :y 63 :z 0}] (mapv :pos (:found r))) "stone tops are worth replacing with dirt, at the price of doing so")
    (is (= [100 0] (mapv :farmable (:found r))))
    (is (= [0 true] ((juxt (comp :replace :costs first) (comp pos? :replace :costs second)) (:found r))))))

(def pond-east
  (merge (tu/box -30 63 -30 30 63 30 "grass_block") (tu/box 18 63 -1 20 63 1 "water")))

(deftest a-pond-18-blocks-out-is-found-whatever-the-pass-jitter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [j [0 0.1 0.3 0.5 0.7 0.9]]
          (let [raw (tu/blind (tu/fake {:blocks pond-east}))
                p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000) :rand (constantly j)}))
                {:keys [eng]} (setup {:blocks pond-east})
                eng (assoc eng :primitives p)
                r (await (child-outcome eng job {:w 5 :h 5 :range 24} 12))]
            (is (pos? (:water-share (first (:spots r)))) (str "jitter " j ": " (pr-str (first (:spots r)))))))))))

(def hillside
  (into {} (for [i (range 22)] (tu/box (+ 24 (* 4 i)) 0 0 (+ 27 (* 4 i)) i 63 "grass_block"))))

(deftest a-hillside-finds-spots-seen-from-its-slope
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/blind (tu/fake {:blocks hillside :self {:pos [64.5 11 32.5]}}))
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000) :rand (constantly 0.5)}))
              {:keys [eng]} (setup {:blocks hillside})
              eng (assoc eng :primitives p)
              r (await (child-outcome eng job {:w 5 :h 5 :range 24} 20))]
          (is (seq (:spots r)) (pr-str r)))))))

(deftest existing-farmland-ranks-with-farmable-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge (patch 1 0 3 3 63 "farmland") (patch -6 0 3 3 63 "grass_block") {"5,63,1" "water" "-8,63,1" "water"})})
              r (await (child-outcome eng job small 4))]
          (is (= [{:x 1 :y 63 :z 0} {:x -6 :y 63 :z 0}] (mapv :pos (:spots r))))
          (is (= [100 100] (mapv :farmable (:spots r)))))))))

(deftest a-hillside-too-dear-to-level-is-no-spot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (into {} (for [x (range 0 6) z (range 0 6) y (range 60 (+ 63 (* 12 (mod (+ x z) 2))))] [(str x "," y "," z) "dirt"]))
              {:keys [eng]} (setup {:blocks blocks})
              r (await (child-outcome eng job {:w 6 :h 6 :range 6} 4))]
          (is (nil? (:spot r)) "36 dry cells with no water known do not pay for levelling 18 columns 12 high")
          (is (= :distance (:why r))))))))

(deftest lacks-text-names-steep-cells
  (is (re-find #"18 of 36 cells short: 18 off the patch's height" (pt/lacks-text {:w 6 :h 6 :short 18 :steep 18}))))


(deftest a-far-flat-patch-is-a-far-miss-not-a-steep-one
  (let [p (tu/seeing-all (tu/fake {:blocks (patch 43 0 5 5 63 "grass_block") :self {:pos {:x 45 :y 64 :z 2}}}))
        r (pt/scan-rows p {:w 5 :h 5 :range 48 :depth 4 :limit 3 :worth 25} {:x 0 :y 64 :z 2} {:next-ring 0 :found []} ##Inf)]
    (is (empty? (:found r)) "a patch worth 25 s with water brought does not pay for a 45-block walk")
    (is (= {:short 0 :far 45} (select-keys (:miss r) [:short :far :steep])))
    (is (re-find #"lies 45 blocks off" (pt/lacks-text (pt/lacks p (:miss r) 64 4))))))
