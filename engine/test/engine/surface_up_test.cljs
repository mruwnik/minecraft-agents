(ns engine.surface-up-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
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
  "A body in a 2-high pocket of a stone block 7 wide at feet y, rock to y roof; sky light is seen from sky-y up (nil: no light read)."
  ([roof sky-y] (buried-body 64 roof sky-y))
  ([y roof sky-y]
   (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y y :z 0.5}}
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

(deftest a-surface-not-seen-is-expected-from-the-bodys-height-not-from-the-rock-it-sees
  (with-mem {}
    #(let [at-64 (surface/climb-s (buried-body 64 67 nil))
           at-16 (surface/climb-s (buried-body 16 19 nil))]
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

(deftest a-stair-of-the-bodys-own-sets-where-the-surface-is
  (let [c (buried-body 16 19 nil)
        feet-dim (.-dimension (.self (:primitives c)))]
    (with-redefs [ctx/mem (constantly {}) ctx/entries (constantly [{:data {:dim feet-dim :floors [[2 39 0]]}}])]
      (is (= 40 (surface/known-surface c)) "the highest step it cut"))))

(deftest a-body-that-climbed-out-or-has-a-stair-to-its-top-climbs-nothing
  (let [c (buried-body 67 nil)
        dim (.-dimension (.self (:primitives c)))]
    (with-redefs [ctx/entries (constantly [])]
      (with-redefs [ctx/mem (constantly {:surface {:done-y 66}})]
        (is (zero? (surface/climb-s c)) "climbed? : out on open ground since")))
    (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                        :blocks (merge (dissoc (box -3 63 -3 3 67 3 "stone") "0,64,0" "0,65,0")
                                                                       (box 2 68 0 2 68 0 "stone"))})))
          c {:primitives p}]
      (with-redefs [ctx/mem (constantly {}) ctx/entries (constantly [{:data {:dim dim :floors [[2 68 0]]}}])]
        (is (= {:x 2 :y 69 :z 0} (surface/own-top c)))
        (is (zero? (surface/climb-s c)) "own-top : a stair of its own ends under the open sky")))))
