(ns engine.placement-test
  "jobs.lib.placement: from a wanted block state to the click that makes it (or a refusal), one table per family."
  (:require [cljs.test :refer [deftest is are async]]
            [jobs.lib.placement :as placement]
            [engine.test-util :as tu]
            [plan.shape :as shape]))

(def pi js/Math.PI)
(def yaw {:north 0 :west (/ pi 2) :south pi :east (* 1.5 pi)})

(def cell [0 64 0])
(def east-eye {:x 2.5 :y 65.62 :z 0.5})

(defn world
  "A block-at over cells {[x y z] name}: air elsewhere, nil (unloaded) where the name is :unloaded."
  [cells]
  (fn [pos]
    (let [n (get cells pos "air")]
      (when-not (= :unloaded n) {:name n}))))

(def floor {[0 63 0] "stone"})

(defn click [want cells] (placement/click want cell east-eye (world cells)))

(deftest stairs-take-facing-from-the-look-and-half-from-the-face
  (are [want cells out] (= out (click want cells))
    {:block "oak_stairs" :facing :east :half :bottom} floor
    {:item "oak_stairs" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:east yaw) :pitch 0}}

    {:block "oak_stairs" :facing :west} floor
    {:item "oak_stairs" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:west yaw) :pitch 0}}

    {:block "oak_stairs" :facing :west :half :top} (assoc floor [0 65 0] "stone")
    {:item "oak_stairs" :click {:against [0 65 0] :cursor [0.5 0 0.5] :yaw (:west yaw) :pitch 0}}

    {:block "oak_stairs" :facing "north" :half "top"} {[1 64 0] "stone" [-1 64 0] "stone" [0 63 0] "stone"}
    {:item "oak_stairs" :click {:against [1 64 0] :cursor [0 0.75 0.5] :yaw (:north yaw) :pitch 0}}

    {:block "oak_stairs" :facing :south :half :bottom} {[0 64 1] "stone"}
    {:item "oak_stairs" :click {:against [0 64 1] :cursor [0.5 0.25 0] :yaw (:south yaw) :pitch 0}}))

(deftest a-half-only-the-wrong-face-gives-is-refused
  (are [want cells] (= {:item (:block want) :refused :no-support} (click want cells))
    {:block "oak_stairs" :facing :east :half :top} floor
    {:block "oak_slab" :type :bottom} {[0 65 0] "stone"}
    {:block "oak_slab" :type :top} {[0 64 0] "water" [1 64 0] "water" [0 63 0] "stone"}))

(deftest slabs-take-their-half-from-the-face-and-never-merge
  (are [want cells out] (= out (click want cells))
    {:block "oak_slab" :type :top :waterlogged true} {[0 64 0] "water" [-1 64 0] "farmland" [0 63 0] "dirt"}
    {:item "oak_slab" :click {:against [-1 64 0] :cursor [1 0.75 0.5]}}

    {:block "oak_slab" :type :bottom} floor
    {:item "oak_slab" :click {:against [0 63 0] :cursor [0.5 1 0.5]}}

    {:block "oak_slab" :type :double} floor
    {:item "oak_slab" :refused :double-slab}

    "oak_slab" floor
    {:item "oak_slab"}))

(deftest logs-take-their-axis-from-the-face
  (are [want cells out] (= out (click want cells))
    {:block "oak_log" :axis :y} floor
    {:item "oak_log" :click {:against [0 63 0] :cursor [0.5 1 0.5]}}

    {:block "oak_log" :axis :y} {[0 65 0] "oak_planks"}
    {:item "oak_log" :click {:against [0 65 0] :cursor [0.5 0 0.5]}}

    {:block "spruce_log" :axis :x} (assoc floor [-1 64 0] "stone")
    {:item "spruce_log" :click {:against [-1 64 0] :cursor [1 0.5 0.5]}}

    {:block "oak_log" :axis :z} (assoc floor [1 64 0] "stone")
    {:item "oak_log" :refused :no-support}

    {:block "quartz_pillar" :axis :z} {[0 64 -1] "stone"}
    {:item "quartz_pillar" :click {:against [0 64 -1] :cursor [0.5 0.5 1]}}))

(deftest gates-doors-and-beds-face-the-way-the-body-looks
  (are [want cells out] (= out (click want cells))
    {:block "oak_fence_gate" :facing :east} floor
    {:item "oak_fence_gate" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:east yaw) :pitch 0}}

    {:block "oak_fence_gate" :facing :north :open false} floor
    {:item "oak_fence_gate" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:north yaw) :pitch 0}}

    {:block "oak_door" :facing :north :half :lower} floor
    {:item "oak_door" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:north yaw) :pitch 0}}

    {:block "white_bed" :facing :south :part :foot} floor
    {:item "white_bed" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:south yaw) :pitch 0}}))

(deftest the-second-half-and-opened-states-are-refused
  (are [want cells reason] (= {:item (:block want) :refused reason} (click want cells))
    {:block "oak_door" :facing :north :half :upper} floor :other-half
    {:block "white_bed" :facing :east :part :head} floor :other-half
    {:block "oak_fence_gate" :facing :east :open true} floor :opened
    {:block "oak_door" :facing :east :open "true"} floor :opened
    {:block "oak_trapdoor" :facing :east :half :top :open true} floor :opened
    {:block "oak_door" :facing :north :half :lower} {[-1 64 0] "stone"} :no-support
    {:block "oak_door" :facing :north :half :lower} (assoc floor [0 65 0] "stone") :no-room
    {:block "white_bed" :facing :south :part :foot} (assoc floor [0 64 1] "stone") :no-room
    {:block "white_bed" :facing :south :part :foot} (assoc floor [0 64 1] :unloaded) :no-room))

(deftest front-blocks-face-the-placer-and-chests-never-join-by-accident
  (are [want cells out] (= out (click want cells))
    {:block "chest" :facing :south} floor
    {:item "chest" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:north yaw) :pitch 0 :sneak true}}

    "chest" floor
    {:item "chest" :click {:against [0 63 0] :cursor [0.5 1 0.5] :sneak true}}

    {:block "chest" :facing :west :type :left} floor
    {:item "chest" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:east yaw) :pitch 0}}

    {:block "furnace" :facing :east} floor
    {:item "furnace" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:west yaw) :pitch 0}}

    {:block "jack_o_lantern" :facing :north} floor
    {:item "jack_o_lantern" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:south yaw) :pitch 0}}))

(deftest trapdoors-hang-on-the-side-they-face-from-else-take-the-look
  (are [want cells out] (= out (click want cells))
    {:block "oak_trapdoor" :facing :west :half :top} (assoc floor [1 64 0] "stone")
    {:item "oak_trapdoor" :click {:against [1 64 0] :cursor [0 0.75 0.5]}}

    {:block "oak_trapdoor" :facing :south :half :bottom} (assoc floor [0 64 -1] "stone")
    {:item "oak_trapdoor" :click {:against [0 64 -1] :cursor [0.5 0.25 1]}}

    {:block "oak_trapdoor" :facing :west :half :top} {[0 65 0] "stone"}
    {:item "oak_trapdoor" :click {:against [0 65 0] :cursor [0.5 0 0.5] :yaw (:east yaw) :pitch 0}}

    {:block "oak_trapdoor" :facing :west :half :bottom} floor
    {:item "oak_trapdoor" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:east yaw) :pitch 0}}

    {:block "oak_trapdoor" :facing :west :half :top} floor
    {:item "oak_trapdoor" :refused :no-support}))

(deftest torches-and-ladders-go-where-they-hang
  (are [want cells out] (= out (click want cells))
    "torch" floor
    {:item "torch" :click {:against [0 63 0] :cursor [0.5 1 0.5] :pitch (- (/ pi 2))}}

    "torch" {[0 63 0] "oak_fence" [1 64 0] "stone"}
    {:item "torch" :click {:against [0 63 0] :cursor [0.5 1 0.5] :pitch (- (/ pi 2))}}

    "torch" {[1 64 0] "stone"}
    {:item "torch" :refused :no-support}

    {:block "wall_torch" :facing :north} {[0 64 1] "stone"}
    {:item "torch" :click {:against [0 64 1] :cursor [0.5 0.5 0] :yaw (:south yaw) :pitch 0}}

    {:block "soul_wall_torch" :facing :east} {[-1 64 0] "oak_planks"}
    {:item "soul_torch" :click {:against [-1 64 0] :cursor [1 0.5 0.5] :yaw (:west yaw) :pitch 0}}

    {:block "wall_torch" :facing :north} {[0 64 1] "oak_fence"}
    {:item "torch" :refused :no-support}

    {:block "ladder" :facing :west} {[1 64 0] "stone"}
    {:item "ladder" :click {:against [1 64 0] :cursor [0 0.5 0.5] :yaw (:east yaw) :pitch 0}}

    {:block "ladder" :facing :west} floor
    {:item "ladder" :refused :no-support}))

(deftest usable-and-loose-neighbours-are-not-clicked-without-need
  (are [want cells out] (= out (click want cells))
    {:block "oak_stairs" :facing :east} {[0 63 0] "chest" [-1 64 0] "stone"}
    {:item "oak_stairs" :click {:against [-1 64 0] :cursor [1 0.25 0.5] :yaw (:east yaw) :pitch 0}}

    {:block "oak_stairs" :facing :east} {[0 63 0] "crafting_table"}
    {:item "oak_stairs" :click {:against [0 63 0] :cursor [0.5 1 0.5] :yaw (:east yaw) :pitch 0 :sneak true}}

    {:block "oak_log" :axis :y} {[0 63 0] "short_grass" [0 65 0] :unloaded}
    {:item "oak_log" :refused :no-support}))

(deftest other-blocks-are-placed-plainly
  (are [want out] (= out (click want floor))
    "oak_fence" {:item "oak_fence"}
    "oak_stairs" {:item "oak_stairs"}
    {:block "oak_planks"} {:item "oak_planks"}
    {:block "observer" :facing :up} {:item "observer"}
    {:block "oak_slab" :waterlogged true} {:item "oak_slab"}))

(deftest item-of-names-what-is-carried-for-a-block
  (are [block item] (= item (placement/item-of block))
    "wall_torch" "torch"
    "soul_wall_torch" "soul_torch"
    "redstone_wall_torch" "redstone_torch"
    "oak_wall_sign" "oak_sign"
    "oak_stairs" "oak_stairs"))

;; ---------------------------------------------------------------- the click against the fake's forward rule

(def round-trip
  "[want cells]: the click jobs.lib.placement chooses, placed in the fake (engine.fake.placing, the rule written forwards),
  must come out as wanted."
  [[{:block "oak_stairs" :facing :east :half :bottom} floor]
   [{:block "oak_stairs" :facing :north :half :top} {[1 64 0] "stone"}]
   [{:block "oak_stairs" :facing :south :half :top} {[0 65 0] "stone"}]
   [{:block "oak_slab" :type :top} {[0 64 1] "stone"}]
   [{:block "oak_slab" :type :bottom} floor]
   [{:block "oak_log" :axis :x} {[-1 64 0] "stone"}]
   [{:block "oak_log" :axis :z} {[0 64 1] "stone"}]
   [{:block "oak_log" :axis :y} floor]
   [{:block "oak_fence_gate" :facing :east :open false} floor]
   [{:block "oak_door" :facing :west :half :lower} floor]
   [{:block "white_bed" :facing :north :part :foot} floor]
   [{:block "chest" :facing :east} floor]
   [{:block "furnace" :facing :north} floor]
   [{:block "jack_o_lantern" :facing :west} floor]
   [{:block "oak_trapdoor" :facing :west :half :top} {[1 64 0] "stone"}]
   [{:block "oak_trapdoor" :facing :north :half :bottom} floor]
   [{:block "oak_trapdoor" :facing :south :half :top} {[0 65 0] "stone"}]
   ["torch" (assoc floor [1 64 0] "stone" [-1 64 0] "stone")]
   [{:block "wall_torch" :facing :east} (assoc floor [-1 64 0] "stone")]
   [{:block "ladder" :facing :south} (assoc floor [0 64 -1] "stone")]])

(defn ^:async placed-in-fake
  "The block the fake holds at cell after placing want with the chosen click, as plan.shape reads it."
  [want cells]
  (let [{:keys [item click]} (click want cells)
        p (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :inventory [{:name item :count 1}]
                    :blocks (into {} (map (fn [[[x y z] n]] [(str x "," y "," z) n])) cells)})
        js-click (clj->js (assoc click :against (zipmap [:x :y :z] (:against click)) :cursor (zipmap [:x :y :z] (:cursor click))))]
    (.setOwner p "t")
    (await (.place p "t" #js {:pos (tu/pos 0 64 0) :item item :click js-click}))
    (let [b (.blockAt p (tu/pos 0 64 0))]
      {:name (.-name b) :state (js->clj (.-properties b) :keywordize-keys true)})))

(deftest every-family-comes-out-as-wanted-in-the-fake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[want cells] round-trip]
          (let [block (await (placed-in-fake want cells))]
            (is (= :match (shape/judge want block)) (str (shape/want-text want) " came out " (pr-str block)))))))))
