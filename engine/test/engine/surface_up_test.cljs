(ns engine.surface-up-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.child :as child]
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
  "A body in a 2-high pocket of a stone block 7 wide, rock to y roof; sky light is seen from sky-y up (nil: no light read)."
  [roof sky-y]
  (let [p (tu/with-sky-only (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                      :blocks (dissoc (box -3 63 -3 3 roof 3 "stone") "0,64,0" "0,65,0")})))]
    (when sky-y (aset (.-rawWorld p) "lightAt" (fn [_ y _] (if (>= y sky-y) 240 0))))
    {:primitives p}))

(deftest the-stair-up-is-priced-by-the-rock-to-the-seen-sky-else-by-the-worse-case-and-not-at-all-in-the-open
  (with-redefs [ctx/mem (constantly {}) jobs.lib.surface-up/own-top (constantly nil)]
   (let [near (surface/climb-s (buried-body 67 69))
        far (surface/climb-s (buried-body 71 73))
        unseen (surface/climb-s (buried-body 67 nil))
        open (surface/climb-s {:primitives (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (box -3 63 -3 3 63 3 "stone")}))})]
    (is (pos? near))
    (is (< near far) "a taller rise to the seen sky costs more")
    (is (< near unseen) "a surface not seen is the seen rock and the rock guess past it: dearer than the sky 2 blocks nearer")
    (is (zero? open) "a body on open ground climbs nothing"))))
