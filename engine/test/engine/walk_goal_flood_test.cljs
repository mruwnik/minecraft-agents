(ns engine.walk-goal-flood-test
  "The goal flood of go-to's budgeted searches kept per goal over its walks (engine.path.walk/goal-floods, card 59551eba;
  live j53: each progress walk began a new search whose flood started again, ~30 s to prove a sealed platform)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.path.walk :as walk]
            [engine.test-util :as tu :refer [box]]))

;; a stone deck x 10..30, z 10..30 at y 70 (441 cells, no way up) over a floor of 63 x 63 cells
(def high-deck (merge (box -2 63 -2 60 63 60 "stone") (box 10 70 10 30 70 30 "stone")))

(defn ^:async calls
  "plan-walk! with a budget of 300 expansions toward the deck's middle, the body put at the next of starts after each
  call that walked; the planner's floods small (preFlood 0, floodAfter 20, goalFlood 100). The reason of the first
  call whose search ended, else :unfinished after k calls."
  [k starts]
  (let [p (tu/fake {:blocks high-deck :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        c {:primitives p}
        chunk walk/chunk-expansions
        options walk/plan-options]
    (reset! walk/searches {})
    (walk/forget-known! c)
    (set! walk/chunk-expansions 16)
    (set! walk/plan-options (fn [& args] (js/Object.assign (apply options args) #js {:preFlood 0 :floodAfter 20 :goalFlood 100})))
    (let [out (loop [i 0
                     walked true]
                (if (>= i k)
                  :unfinished
                  (let [[x z] (nth (cycle starts) i)
                        _ (when walked ; a walk: the next call begins a new search from where it got to
                            (swap! (fake/state p) assoc-in [:self :pos] [(+ x 0.5) 64 (+ z 0.5)])
                            (reset! walk/searches {}))
                        plan (await (walk/plan-walk! c (.pathWorld p) [20 71 20] 0 walk/default-weight {:budget 300}))]
                    (if (= "searching" (.-reason (:r plan)))
                      (recur (inc i) (some? (:steps plan)))
                      (.-reason (:r plan))))))]
      (set! walk/chunk-expansions chunk)
      (set! walk/plan-options options)
      (reset! walk/searches {})
      (walk/forget-known! c)
      out)))

(deftest a-goal-flood-goes-on-after-each-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [kept walk/goal-flood!]
          (set! walk/goal-flood! (constantly nil))
          (let [fresh (await (calls 12 [[0 0] [2 0] [4 0]]))]
            (set! walk/goal-flood! kept)
            (is (= :unfinished fresh) "each walk's search floods afresh and never gets to the flood that proves it")))
        (is (= "goal-enclosed" (await (calls 12 [[0 0] [2 0] [4 0]]))))))))

(deftest forgetting-the-known-land-forgets-the-flood
  (let [p (tu/fake {:blocks high-deck :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        c {:primitives p}]
    (walk/goal-flood! c [20 71 20] 0 walk/default-weight nil nil)
    (is (some? (get @walk/goal-floods "Fake")))
    (walk/forget-known! c)
    (is (nil? (get @walk/goal-floods "Fake")))))

;; a stone room x 5..18, z -7..6 (12 x 12 inside, feet y 64) on a floor; the goal (17 64 0) beside its east wall at z 0
(def room-floor (merge (box -2 63 -10 24 63 10 "stone")
                       (apply dissoc (box 5 63 -7 18 67 6 "stone") (keys (box 6 64 -6 17 66 5 "x")))))

;; the wall beside the goal is opened after the first call: the flood kept from it read the room walled, and its cells
;; there are not expanded again; the goal must not be called walled in
(deftest a-flood-kept-from-before-a-wall-opened-does-not-call-the-goal-enclosed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:blocks room-floor :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              c {:primitives p}
              chunk walk/chunk-expansions
              options walk/plan-options
              plan! (fn [budget] (walk/plan-walk! c (.pathWorld p) [17 64 0] 0 walk/default-weight {:budget budget}))]
          (reset! walk/searches {})
          (walk/forget-known! c)
          (set! walk/chunk-expansions 16)
          (set! walk/plan-options (fn [& args] (js/Object.assign (apply options args) #js {:preFlood 0 :floodAfter 20 :goalFlood 100})))
          (let [first-plan (await (plan! 40))]
            (swap! (fake/state p) update :blocks dissoc (fake/parse-cell "18,64,0") (fake/parse-cell "18,65,0"))
            (reset! walk/searches {}) ; a walk: the next call begins a new search
            (let [then (await (plan! 100000))]
              (set! walk/chunk-expansions chunk)
              (set! walk/plan-options options)
              (reset! walk/searches {})
              (walk/forget-known! c)
              (is (= "searching" (.-reason (:r first-plan))))
              (is (= ["found" nil] [(.-status (:r then)) (.-reason (:r then))])))))))))
