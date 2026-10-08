(ns engine.go-to-water-test
  "go-to and water: an arrival within :range on water walks on to a dry stand in range, else says :in-water; a body under
  water whose search goes on (no walk) swims to air when its air runs low, in manual mode as in any other."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.near :as near]))

;; a stone plain (top y 63) with a pond 6 deep (water y 58..63) from x 0 to x-end, z -4..4; the body on land west of it
(defn pond [x-end]
  (merge (box -10 55 -6 30 63 6 "stone") (box 0 58 -4 x-end 63 4 "water")))

(deftest an-arrival-on-water-walks-on-to-a-dry-stand-in-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (gt/go! {:self {:pos [-4.5 64 0.5]} :blocks (pond 12)}
                                                   {:pos [10 64 0] :range 3 :escalate false}))
              [x y z] (gt/at p)]
          (is (true? (:arrived @out)) (str "result " @out))
          (is (nil? (:in-water @out)) (str "result " @out))
          (is (false? (.-inWater (.self p))) (str "on dry land: at " [x y z]))
          (is (<= 13 x) (str "the bank east of the pond: at " [x y z]))
          (is (empty? (gt/events-of s :go-to.in-water))))))))

(deftest an-arrival-on-water-with-no-dry-stand-in-range-says-in-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (gt/go! {:self {:pos [-4.5 64 0.5]} :blocks (pond 24)}
                                                   {:pos [10 64 0] :range 3 :escalate false}))]
          (is (true? (:arrived @out)) (str "result " @out))
          (is (true? (:in-water @out)) (str "result " @out " at " (gt/at p)))
          (is (= 1 (count (gt/events-of s :go-to.in-water))) "an info says the body stays in water"))))))

;; the body on the pond floor (feet 58), short of air (oxygen 8: 5.6 s left), its search toward the far bank still going
;; on: it swims up for air while it searches, not standing on the floor until it drowns
(deftest a-body-under-water-short-of-air-swims-up-while-its-search-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [real near/walk-round!
              n (atom 0)
              rose (atom nil)]
          (with-redefs [near/walk-round! (fn [c pos range opts]
                                           (if (and (= 28 (:x pos)) (< (swap! n inc) 6))
                                             (js/Promise.resolve {:result {:status :searching :fresh true} :status "searching"
                                                                  :from (:pos c) :to {:x 4.5 :y 58 :z 0.5}})
                                             (do (when (= 28 (:x pos)) (compare-and-set! rose nil (gt/at (:primitives c))))
                                                 (real c pos range opts))))]
            (let [{:keys [out p] :as s} (await (gt/go! {:self {:pos [4.5 58 0.5] :inWater true :oxygen 8} :blocks (pond 24)}
                                                       {:pos [28 64 0] :range 1 :escalate false}))]
              (is (seq (gt/events-of s :go-to.surfacing)) "it swims to air while searching")
              (is (<= 62 (second @rose)) (str "it was at the surface before its search ended: " @rose))
              (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p))))))))))

;; plenty of air left: a search under water goes on where the body is
(deftest a-body-under-water-with-air-left-searches-in-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [real near/walk-round!
              n (atom 0)]
          (with-redefs [near/walk-round! (fn [c pos range opts]
                                           (if (and (= 28 (:x pos)) (< (swap! n inc) 3))
                                             (js/Promise.resolve {:result {:status :searching :fresh true} :status "searching"
                                                                  :from (:pos c) :to {:x 4.5 :y 58 :z 0.5}})
                                             (real c pos range opts)))]
            (let [s (await (gt/go! {:self {:pos [4.5 58 0.5] :inWater true :oxygen 20} :blocks (pond 24)}
                                   {:pos [28 64 0] :range 1 :escalate false}))]
              (is (empty? (gt/events-of s :go-to.surfacing))))))))))
