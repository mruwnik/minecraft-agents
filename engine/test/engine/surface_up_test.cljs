(ns engine.surface-up-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.surface-up :as surface]))

;; a pond 7 x 7, 1 deep, on a stone floor, dry ground all round; every walk out of it fails
(deftest a-walk-out-of-water-that-fails-tries-another-stand-and-stays-after-max-failed-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:self {:inWater true :pos {:x 0.5 :y 64 :z 0.5}}
                                         :blocks (merge (box -8 63 -8 8 63 8 "stone") (box -3 64 -3 3 64 3 "water"))}))
              m (atom {})
              walked (atom [])
              c {:primitives p}
              ends (with-redefs [ctx/mem (fn [_] @m)
                                 ctx/update-mem! (fn [_ f & args] (apply swap! m f args))
                                 ctx/emit! (fn ([_ _ _]) ([_ _ _ _]))
                                 child/call-walk* (fn [_ _ args] (swap! walked conj (:pos args)) (js/Promise.resolve {:kind :nothing-possible :data {:arrived false}}))]
                     (let [a (await (surface/leave-water! c)) b (await (surface/leave-water! c)) d (await (surface/leave-water! c))
                           e (await (surface/leave-water! c)) f (await (surface/leave-water! c))]
                       [a b d e f]))]
          (is (= [:again :again :again :out :out] ends))
          (is (= 3 (count (distinct @walked))) (str "each failed stand is not walked to again: " @walked)))))))

(deftest the-wall-a-climb-starts-from-is-never-in-the-callers-dig-line
  (let [p (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (dissoc (box -2 63 -2 2 66 2 "stone") "0,64,0" "0,65,0")}))
        feet {:x 0 :y 64 :z 0}]
    (is (= {:x 1 :y 64 :z 0} (surface/wall-of p feet)))
    (is (= {:x -1 :y 64 :z 0} (surface/wall-of p feet #{[1 64 0] [2 64 0]})) "a tunnel east from here would undercut a stair up east")))

(defn buried-body
  "A body in a 2-high pocket of a stone block 7 wide at feet y, rock to y roof, carrying inv; sky light is seen from sky-y up (nil: no light read)."
  ([roof sky-y] (buried-body 64 roof sky-y))
  ([y roof sky-y] (buried-body y roof sky-y []))
  ([y roof sky-y inv]
   (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y y :z 0.5}} :inventory inv
                                                       :blocks (dissoc (box -3 (dec y) -3 3 roof 3 "stone") (str "0," y ",0") (str "0," (inc y) ",0"))})))]
     (when sky-y (aset (.-rawWorld p) "lightAt" (fn [_ y _] (if (>= y sky-y) 240 0))))
     {:primitives p})))

(defn with-mem [m f] (with-redefs [ctx/mem (constantly m) ctx/entries (constantly [])] (f)))

(deftest the-stair-up-is-priced-by-the-rock-to-the-seen-sky-and-not-at-all-in-the-open
  (with-mem {}
    #(let [near (surface/climb-s (buried-body 67 69))
           far (surface/climb-s (buried-body 71 73))
           open (surface/climb-s {:primitives (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (box -3 63 -3 3 63 3 "stone")}))})]
       (is (pos? near))
       (is (< near far) "a taller rise to the seen sky costs more")
       (is (zero? open) "a body on open ground climbs nothing"))))

(deftest a-stair-up-cut-by-hand-costs-what-the-bare-hand-takes-not-a-pickaxe
  (with-mem {}
    #(let [hand (surface/climb-s (buried-body 64 71 72 []))
           pick (surface/climb-s (buried-body 64 71 72 [{:name "stone_pickaxe" :count 1}]))]
       (is (> hand (* 3 pick)) (str "stone by hand is 7.5 s a cut, 0.6 s with a stone pickaxe: " hand " vs " pick)))))

(deftest a-worn-pickaxe-prices-the-long-stair-between-a-fresh-one-and-the-bare-hand
  (with-mem {}
    #(let [q (fn [inv] (surface/climb-s (buried-body 64 80 82 inv)))
           fresh (q [{:name "stone_pickaxe" :count 1 :durability 100}])
           worn (q [{:name "stone_pickaxe" :count 1 :durability 3}])
           hand (q [])]
       (is (< fresh worn hand) (str "3 uses left on a long stair: " fresh " < " worn " < " hand)))))

(deftest a-stair-cut-through-a-build-pays-its-put-back-but-natural-rock-is-free
  (let [buried (fn [block] {:primitives (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "stone_pickaxe" :count 1} {:name "stone_axe" :count 1}]
                                                                                   :blocks (dissoc (box -3 63 -3 3 67 3 block) "0,64,0" "0,65,0")})))})]
    (with-mem {} #(is (< (surface/climb-s (buried "stone")) (surface/climb-s (buried "oak_planks")))
                      "planks are put back, natural rock is not"))))

(deftest a-rise-through-seen-cave-air-is-priced-as-walk-steps-not-stair-steps
  (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                      :blocks (dissoc (box -3 63 -3 3 71 3 "stone") "0,64,0" "0,65,0" "0,66,0" "0,67,0" "0,68,0" "0,69,0" "0,70,0" "0,71,0")})))
        c {:primitives p}]
    (aset (.-rawWorld p) "lightAt" (fn [_ y _] (if (>= y 72) 240 0)))
    (with-mem {} #(is (< (surface/climb-s c) (/ (cw/stair-cost (surface/rise c)) 2))
                         "cut-free steps are walk steps, as dig-route prices them"))))

(deftest a-surface-not-seen-is-expected-from-the-bodys-height-not-from-the-rock-it-sees
  (with-mem {}
    #(let [sees-4 (fn [c] (update c :primitives tu/seeing-within 4))
           at-64 (surface/climb-s (sees-4 (buried-body 64 67 nil)))
           at-16 (surface/climb-s (sees-4 (buried-body 16 19 nil)))]
       (is (< 0 at-64) "near the surface the prior puts the sky a few blocks up")
       (is (< (* 3 at-64) at-16) "at y 16 about 50 blocks of stair are expected, not the rock guess")
       (is (= (surface/rise (buried-body 16 19 nil)) (js/Math.ceil (cost/expected-rise 16 (+ 3 (dig-look/rock-depth) 1) nil)))
           "past the last seen rock the rise is the expectation over the surface prior"))))

(deftest seen-cave-air-above-does-not-end-the-rise-early
  (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                      :blocks (merge (dissoc (box -3 63 -3 3 66 3 "stone") "0,64,0" "0,65,0")
                                                                     (box 0 72 0 0 72 0 "stone"))})))]
    (aset (.-rawWorld p) "lightAt" (fn [_ y _] (if (>= y 80) 240 0)))
    (with-mem {} #(is (= 16 (surface/rise {:primitives p})) "the air from y 67 to 71 is seen, the stone over it too, and the sky from 80 is the rise"))))

(deftest a-body-under-a-one-block-roof-with-open-sky-remembered-beside-it-prices-a-few-blocks-of-stair
  (let [pocket #{"0,3,0" "0,4,0"}
        p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 3 :z 0.5}}
                                                      :blocks (merge (box -9 2 -9 9 2 9 "stone")
                                                                     (apply dissoc (box -2 3 -2 2 5 2 "stone") pocket))})))
        sensed (aget p "sensedAt")
        above-roof? (fn [pos] (and (>= (js/Math.floor (.-y pos)) 6) (<= (js/Math.abs (js/Math.floor (.-x pos))) 2) (<= (js/Math.abs (js/Math.floor (.-z pos))) 2)))
        seen-at (aget p "seenBlockAt")]
    (aset p "seenBlockAt" (fn [pos] (if (above-roof? pos) #js {:unknown true :pos pos} (.call seen-at p pos))))
    (tu/set-sensed! p (fn [pos] (if (above-roof? pos) #js {:unknown true} (sensed pos))))
    (aset (.-rawWorld p) "lightAt" (fn [x y z] (if (or (> (js/Math.abs x) 2) (> (js/Math.abs z) 2)) 240 0)))
    (with-mem {} #(is (<= (surface/rise {:primitives p}) 8) "the ground round the hut is lit, so the sky is within a stair of the roof, not at the underground prior"))))

(defn known-surface-with
  "known-surface of a body at y 16 in a 7-wide block of rock, with extra blocks and the floors of the stairs it cut."
  [extra floors]
  (let [c (assoc (buried-body 16 19 nil) :primitives (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 16 :z 0.5}}
                                                                                                :blocks (merge (dissoc (box -3 15 -3 3 19 3 "stone") "0,16,0" "0,17,0") extra)}))))
        dim (.-dimension (.self (:primitives c)))]
    (with-redefs [ctx/mem (constantly {}) ctx/entries (constantly [{:data {:dim dim :floors floors}}])]
      (surface/known-surface c))))

(deftest a-stair-of-the-bodys-own-that-ends-under-the-sky-near-it-sets-where-the-surface-is
  (is (= 40 (known-surface-with {"2,39,0" "stone"} [[2 39 0]])) "the top it cut, one block over"))

(deftest a-stair-too-far-to-reach-or-ending-in-rock-is-no-surface
  (is (nil? (known-surface-with {"900,119,900" "stone"} [[900 119 900]])) "a stair far off: no stair from here gets there")
  (is (nil? (known-surface-with {"2,29,0" "stone" "2,31,0" "stone"} [[2 29 0]])) "a stair that stopped mid-mine has rock over its top, not the sky"))

(deftest the-nearest-remembered-surface-sets-the-rise-not-the-median
  (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 16 :z 0.5}}
                                                      :blocks (assoc (dissoc (box -9 59 -9 9 59 9 "stone") "4,59,0") "4,39,0" "stone")})))]
    (aset (.-rawWorld p) "lightAt" (fn [_ y _] (if (>= y 40) 240 0)))
    (with-redefs [ctx/mem (constantly {}) ctx/entries (constantly [])]
      (is (= 40 (surface/known-surface {:primitives p})) "the stand 4 blocks off is the nearest; most columns round it stand at 60"))))

(deftest a-body-that-climbed-out-or-has-a-stair-to-its-top-climbs-nothing
  (let [c (buried-body 67 nil)
        dim (.-dimension (.self (:primitives c)))]
    (with-redefs [ctx/entries (constantly [])]
      (with-redefs [ctx/mem (constantly {:surface {:done-y 66}})]
        (is (zero? (surface/climb-s c)) "climbed? : out on open ground since")))
    (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                        :blocks (merge (dissoc (box -3 63 -3 3 67 3 "stone") "0,64,0" "0,65,0")
                                                                       (box 2 68 0 2 68 0 "cobblestone"))})))
          c {:primitives p}]
      (with-redefs [ctx/mem (constantly {}) ctx/entries (constantly [{:data {:dim dim :floors [[2 68 0]]}}])]
        (is (= {:x 2 :y 69 :z 0} (surface/own-top c)) "a placed cobblestone step is ground, not a roof")
        (is (zero? (surface/climb-s c)) "own-top : a stair of its own ends under the open sky")))))

(deftest a-stair-top-under-a-roof-is-no-surface
  ;; the body's own step tops at feet 69; a roof over it, however high the room and whatever hangs under it, is cover
  (doseq [[why over] [["a hut 3 high" (box 1 72 -1 3 72 1 "oak_planks")]
                      ["a hall 4 high" (box 1 73 -1 3 73 1 "oak_planks")]
                      ["a chest under the roof" (merge (box 1 72 -1 3 72 1 "oak_planks") (box 2 71 0 2 71 0 "chest"))]]]
    (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                        :blocks (merge (dissoc (box -3 63 -3 3 67 3 "stone") "0,64,0" "0,65,0")
                                                                       (box 2 68 0 2 68 0 "cobblestone") over)})))
          dim (.-dimension (.self p))]
      (with-redefs [ctx/mem (constantly {}) ctx/entries (constantly [{:data {:dim dim :floors [[2 68 0]]}}])]
        (is (nil? (surface/own-top {:primitives p})) why)))))

(deftest a-half-cut-stair-of-the-bodys-own-is-walked-to-when-no-wall-or-open-top-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                            :blocks (dissoc (box -3 63 -3 3 80 3 "stone") "0,64,0" "0,65,0")})))
              dim (.-dimension (.self p))
              m (atom {})
              walked (atom [])
              c {:primitives p}]
          (with-redefs [ctx/mem (fn [_] @m)
                        ctx/update-mem! (fn [_ f & args] (apply swap! m f args))
                        ctx/entries (constantly [{:data {:dim dim :floors [[2 70 0]]}}])
                        surface/wall-of (constantly nil)
                        surface/stand-by-wall (constantly nil)
                        child/call-walk* (fn [_ slot args] (swap! walked conj [slot (:pos args)]) (js/Promise.resolve {:kind :nothing-possible :data {:arrived false}}))]
            (is (= {:x 2 :y 71 :z 0} (surface/stair-top c)) "its top, though rock lies over it")
            (is (= :continue (await (surface/step! c))))
            (is (= [[:half-stair-walk {:x 2 :y 71 :z 0}]] @walked))
            (is (= :stopped (await (surface/step! c))) "a walk that failed is not tried again")))))))
