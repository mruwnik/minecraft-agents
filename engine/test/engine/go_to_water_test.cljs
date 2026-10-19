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
                                                   {:pos [10 64 0] :range 3 :ways #{:walk :clear-seal}}))
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
                                                   {:pos [10 64 0] :range 3 :ways #{:walk :clear-seal}}))]
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
                                                       {:pos [28 64 0] :range 1 :ways #{:walk :clear-seal}}))]
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
                                   {:pos [28 64 0] :range 1 :ways #{:walk :clear-seal}}))]
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
                                                       {:pos [28 64 0] :range 1 :ways #{:walk :clear-seal}}))]
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
                                                       {:pos [28 64 0] :range 1 :ways #{:walk :clear-seal}}))]
              (is (seq (gt/events-of s :go-to.surfacing)) "it swims up while its search goes on")
              (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p))))))))))

(deftest a-walking-round-between-fresh-searches-under-water-resets-their-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [real near/walk-round!
              n (atom 0)]
          (with-redefs [near/walk-round! (fn [c pos range opts]
                                           (let [i (swap! n inc)]
                                             (cond
                                               (not= 28 (:x pos)) (real c pos range opts)
                                               (= 3 i) (js/Promise.resolve {:result {:status :partial :reason :stuck} :status "partial"
                                                                            :from (:pos c) :to {:x 10.5 :y 58 :z 0.5}})
                                               (<= i 5) (searching-slice c 20)
                                               :else (real c pos range opts))))]
            (let [s (await (gt/go! {:self {:pos [4.5 58 0.5] :inWater true :oxygen 20} :blocks (pond 24)}
                                   {:pos [28 64 0] :range 1 :ways #{:walk :clear-seal}}))]
              (is (empty? (gt/events-of s :go-to.surfacing)) "two fresh searches each side of a walk are not more than two restarts"))))))))

(deftest a-goal-cell-that-is-water-says-so-when-the-body-stays-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (gt/go! {:self {:pos [-4.5 64 0.5]} :blocks (pond 24)}
                                                 {:pos [10 61 0] :range 0 :ways #{:walk :clear-seal}}))]
          (is (true? (:arrived @out)) (str "result " @out))
          (is (re-find #"goal cell is water" (str (:text (first (gt/events-of s :go-to.in-water)))))))))))

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
                                                     {:pos [10 64 0] :range 3 :ways #{:walk :clear-seal} :spend {:job-time 30}}))
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
                                               {:pos [10 64 0] :range 3 :ways #{:walk :clear-seal}}))]
            (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p)))
            (is (true? (:in-water @out)) (str "result " @out))))))))

(deftest leave-water-false-keeps-an-arrival-in-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [-4.5 64 0.5]} :blocks (pond 12)}
                                             {:pos [10 64 0] :range 3 :ways #{:walk :clear-seal} :settings {:jobs.movement.go-to.tuning/leave-water false}}))]
          (is (true? (:arrived @out)) (str "result " @out))
          (is (true? (:in-water @out)) (str "result " @out))
          (is (< (first (gt/at p)) 13) (str "not walked on to the east bank: at " (gt/at p))))))))

;; a wide range: the bank beside the body (x 4, 5 away), not the one nearest the goal (x 9 or more)
(deftest a-wide-range-arrival-on-water-takes-the-bank-beside-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [3.5 63 0.5]} :blocks (pond 24)}
                                             {:pos [12 64 0] :range 10 :ways #{:walk :clear-seal}}))
              [x _ _ :as at] (gt/at p)]
          (is (= {:arrived true} @out) (str "result " @out))
          (is (= [64 5] [(second at) (nth at 2)]) (str "on the north bank: at " at))
          (is (<= x 6) (str "the bank beside it: at " at)))))))

;; a long search leaves the body standing; that is said (a declared hold), not the idle warning of a round that forgot
(deftest a-long-search-declares-the-hold-while-the-body-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [real near/walk-round!
              n (atom 0)]
          (with-redefs [near/walk-round! (fn [c pos range opts]
                                           (if (< (swap! n inc) 4)
                                             (js/Promise.resolve {:result {:status :searching :fresh false} :status "searching" :from (:pos c) :to {:x 4.5 :y 64 :z 0.5}})
                                             (real c pos range opts)))]
            (let [{:keys [out] :as s} (await (gt/go! {:self {:pos [-4.5 64 0.5]} :blocks (pond 0)} {:pos [10 64 0] :range 1 :ways #{:walk :clear-seal}}))]
              (is (true? (:arrived @out)) (str "result " @out))
              (is (= [:searching] (mapv :reason (gt/events-of s :holding)))))))))))

;; a sealed tunnel (water y 58..59, x 2..31) from a shaft that reaches the air at x 1, to an air pocket at x 32: about 13 s
;; of swimming for a body with 7 s of air left (oxygen 10). It does not dive in: it is at air when the call ends.
(def sealed-tunnel
  (merge (box -2 55 -4 40 66 4 "stone")
         (box 1 58 0 1 64 0 "water") (box 1 65 0 1 66 0 "air")
         (box 2 58 0 31 59 0 "water") (box 32 58 0 32 59 0 "air")))

(deftest a-dive-longer-than-the-air-left-is-not-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (gt/go! {:self {:pos [1.5 58 0.5] :inWater true :oxygen 10} :blocks sealed-tunnel}
                                                   {:pos [32 58 0] :range 0 :ways #{:walk :clear-seal}}))
              [x y z] (gt/at p)]
          (is (< x 6) (str "not far into the tunnel: " [x y z] " result " @out))
          (is (not (and (<= y 60) (< 6 x))) (str "not under water in the tunnel: " [x y z])))))))

;; 19 blocks of tunnel is about 8 s of swimming, inside the planner's own 12 s but past the 6 s that breathe's reflex lets a
;; body use (oxygen 12): such a dive is refused, not started and cut half way
(deftest a-dive-the-breathe-reflex-would-cut-is-not-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [1.5 58 0.5] :inWater true :oxygen 20} :blocks sealed-tunnel}
                                             {:pos [20 58 0] :range 0 :ways #{:walk :clear-seal}}))
              [x y z] (gt/at p)]
          (is (not (true? (:arrived @out))) (str "result " @out " at " [x y z]))
          (is (< x 14) (str "not deep into the tunnel: " [x y z])))))))

(deftest a-dive-within-the-air-left-is-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [1.5 58 0.5] :inWater true :oxygen 20} :blocks sealed-tunnel}
                                             {:pos [32 58 0] :range 0 :ways #{:walk :clear-seal}}))]
          (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p))))))))

;; a stone plain (top y 63) with a pool 1 deep (water y 63) from x 0 to 6, z -4..4; the body in the water at the east bank
(def shallow (merge (box -10 55 -6 30 62 6 "stone") (box -10 63 -6 -1 63 6 "stone") (box 7 63 -6 30 63 6 "stone")
                    (box 0 63 -4 6 63 4 "water")))

(defn stuck-in-water
  "near/walk-round! as real, but a walk begun in water ends stuck at the bank cell [7 64 0] where the body is; walks, an
  atom, counts them."
  [real walks]
  (fn [c pos range opts]
    (if (.-inWater (.self (:primitives c)))
      (do (swap! walks inc)
          (js/Promise.resolve {:result {:status :stuck :target [7 64 0] :move :jump :why "no progress on step 1"} :status "blocked"
                               :from (:pos c) :to {:x 6.5 :y 63 :z 0.5}}))
      (real c pos range opts))))

(deftest a-body-its-walk-left-stuck-in-water-swims-at-another-bank-and-walks-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)]
          (with-redefs [near/walk-round! (stuck-in-water near/walk-round! walks)]
            (let [{:keys [out p] :as s} (await (gt/go! {:self {:pos [6.5 63 0.5] :inWater true} :blocks shallow}
                                                       {:pos [12 64 0] :range 0 :ways #{:walk :clear-seal}}))]
              (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p)))
              (is (= 1 @walks) "one stuck walk, then the swim")
              (is (= [[[7 64 -1] "landed" 1]] (map (juxt :toward :status :swim) (gt/events-of s :go-to.unstuck)))
                  "the bank beside the one the walk was stuck at, said in an event")
              (is (= [:none] (map :attention (gt/events-of s :walker-fault))) "a fault the call gets over wakes no waiting agent"))))))))

(deftest swims-at-a-bank-are-bounded-and-a-body-still-stuck-gives-up-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)]
          (with-redefs [near/walk-round! (stuck-in-water near/walk-round! walks)]
            (let [{:keys [out] :as s} (await (gt/go! {:self {:pos [6.5 63 0.5] :inWater true} :blocks shallow :swimFails true}
                                                     {:pos [12 64 0] :range 0 :ways #{:walk :clear-seal}}))]
              (is (= {:arrived false :reason :unreachable :why :stuck} (select-keys @out [:arrived :reason :why])) (str "result " @out))
              (is (string? (:text @out)) "the give-up says why in words")
              (is (= [[[7 64 -1] "timeout"] [[7 64 1] "timeout"]] (map (juxt :toward :status) (gt/events-of s :go-to.unstuck)))
                  "two swims a call, the stuck bank never reached"))))))))
