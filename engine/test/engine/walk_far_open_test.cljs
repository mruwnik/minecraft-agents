(ns engine.walk-far-open-test
  "A masked search (go-to :unknown) over open land toward a goal far past what the body has seen."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.go-to-sensed-test :as gs]
            [engine.path.executor :as executor]
            [engine.perception :as perception]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

(def flat-world
  {:self {:pos {:x 0.5 :y 64 :z 0.5}} :yaw 270 :pitch 0
   :blocks (box -20 63 -40 140 63 40 "stone")})

(deftest a-masked-search-over-flat-unseen-land-stays-near-the-straight-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake flat-world)
              raw (fake-raw/create p)
              per (perception/create raw {:now (constantly 1000000) :radius 16 :ray-deg 4})
              w (perception/wrap p per)]
          (aset w "rawWorld" raw)
          (perception/pass! per)
          (let [{:keys [r]} (await (wplan/plan-within! {:primitives w} (wworld/path-world w (wworld/without-rock (wworld/unknown-spec))) [80 64 0] 2 1.2 executor/policy))]
            (is (= "found" (.-status r)))
            (is (< (.-expanded r) 400) "the heuristic counts the unseen-land prices, so the search does not flood the land round the way")))))))

;; go-to's own :unknown, the rock guess on: the plain past sight may read as rock, the walk still gets there
(deftest go-to-over-a-plain-past-what-the-body-saw-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gs/run-go-to (assoc flat-world :radius 16) {:pos [80 64 0] :range 1 :unknown true}
                                                   (gs/looking {:light-default [15 0]})))]
          (is (= {:arrived true} (select-keys out [:arrived])) (str (pr-str out) " at " (:pos (fake/self p)))))))))
