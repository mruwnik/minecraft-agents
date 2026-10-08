(ns engine.go-to-water-test
  "go-to and water: an arrival within :range on water walks on to a dry stand in range, else says :in-water; a body under
  water whose search goes on (no walk) swims to air when its air runs low, in manual mode as in any other."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt]
            [engine.fake :as fake]
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

(defn searching-slice
  "A walk-round! answer of a search still going on, the body put back on the pond floor at oxygen ox (it sank while it
  thought)."
  [c ox]
  (fake/swap-self! (:primitives c) assoc :pos [4.5 58 0.5] :inWater true :oxygen ox)
  (js/Promise.resolve {:result {:status :searching :fresh true} :status "searching" :from (:pos c) :to {:x 4.5 :y 58 :z 0.5}}))

;; a long search: the body sinks back short of air at every slice; it swims up every time, not only 3 times a call
(deftest a-body-sinking-back-short-of-air-swims-up-at-every-slice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [real near/walk-round!
              n (atom 0)]
          (with-redefs [near/walk-round! (fn [c pos range opts]
                                           (if (and (= 28 (:x pos)) (< (swap! n inc) 7))
                                             (searching-slice c 8)
                                             (real c pos range opts)))]
            (let [{:keys [out p] :as s} (await (gt/go! {:self {:pos [4.5 58 0.5] :inWater true :oxygen 8} :blocks (pond 24)}
                                                       {:pos [28 64 0] :range 1 :escalate false}))]
              (is (<= 6 (count (gt/events-of s :go-to.surfacing))) (str "surfacings " (count (gt/events-of s :go-to.surfacing))))
              (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p))))))))))

;; plenty of air, but the search under water begins afresh slice after slice (its air drifts, it never ends): it swims
;; up and plans from the air instead of standing on the floor until it drowns
(deftest a-search-under-water-that-keeps-beginning-afresh-swims-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [real near/walk-round!
              n (atom 0)]
          (with-redefs [near/walk-round! (fn [c pos range opts]
                                           (if (and (= 28 (:x pos)) (< (swap! n inc) 6))
                                             (searching-slice c 20)
                                             (real c pos range opts)))]
            (let [{:keys [out p] :as s} (await (gt/go! {:self {:pos [4.5 58 0.5] :inWater true :oxygen 20} :blocks (pond 24)}
                                                       {:pos [28 64 0] :range 1 :escalate false}))]
              (is (seq (gt/events-of s :go-to.surfacing)) "it swims up while its search goes on")
              (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p))))))))))

(defn dry-walk-fails
  "walk-round! where a walk on to the east bank (x over 12) answers r (fn of c), any other the real one."
  [r]
  (let [real near/walk-round!]
    (fn [c pos range opts]
      (if (< 12 (:x pos)) (js/Promise.resolve (r c)) (real c pos range opts)))))

;; the walk on to the dry stand gets nearer but not there: the arrival stands, in water, not a leg's end
(deftest a-failed-walk-to-the-dry-stand-still-arrives-in-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [near/walk-round! (dry-walk-fails (fn [_] {:result {:status :off-plan :partial true} :status "partial"
                                                                :to {:x 12.5 :y 63 :z 0.5}}))]
          (let [{:keys [out p] :as s} (await (gt/go! {:self {:pos [-4.5 64 0.5]} :blocks (pond 12)}
                                                     {:pos [10 64 0] :range 3 :escalate false :leg-s 30}))
                said (:text (first (gt/events-of s :go-to.in-water)))]
            (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p)))
            (is (true? (:in-water @out)) (str "result " @out))
            (is (re-find #"dry stand at" (str said)) (str "says the walk there failed: " said))))))))

;; the walk on to the dry stand never gets past its search: still an arrival in water, not a give-up
(deftest a-dry-walk-that-only-searches-still-arrives-in-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [near/walk-round! (dry-walk-fails (fn [c] {:result {:status :searching :fresh true} :status "searching"
                                                                :to (:pos c)}))]
          (let [{:keys [out p]} (await (gt/go! {:self {:pos [-4.5 64 0.5]} :blocks (pond 12)}
                                               {:pos [10 64 0] :range 3 :escalate false}))]
            (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p)))
            (is (true? (:in-water @out)) (str "result " @out))))))))

(deftest leave-water-false-keeps-an-arrival-in-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [-4.5 64 0.5]} :blocks (pond 12)}
                                             {:pos [10 64 0] :range 3 :escalate false :leave-water false}))]
          (is (true? (:arrived @out)) (str "result " @out))
          (is (true? (:in-water @out)) (str "result " @out))
          (is (< (first (gt/at p)) 13) (str "not walked on to the east bank: at " (gt/at p))))))))

;; a wide range: the bank beside the body (x 4, 5 away), not the one nearest the goal (x 9 or more)
(deftest a-wide-range-arrival-on-water-takes-the-bank-beside-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [3.5 63 0.5]} :blocks (pond 24)}
                                             {:pos [12 64 0] :range 10 :escalate false}))
              [x _ _ :as at] (gt/at p)]
          (is (= {:arrived true} @out) (str "result " @out))
          (is (= [64 5] [(second at) (nth at 2)]) (str "on the north bank: at " at))
          (is (<= x 6) (str "the bank beside it: at " at)))))))
