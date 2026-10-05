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
  call as a walk would; the planner's floods small (preFlood 0, floodAfter 20, goalFlood 100). The reason of the first
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
    (let [out (loop [i 0]
                (if (>= i k)
                  :unfinished
                  (let [[x z] (nth (cycle starts) i)
                        _ (swap! (fake/state p) assoc-in [:self :pos] [(+ x 0.5) 64 (+ z 0.5)])
                        _ (reset! walk/searches {}) ; a walk: the next call begins a new search from where it got to
                        plan (await (walk/plan-walk! c (.pathWorld p) [20 71 20] 0 walk/default-weight {:budget 300}))]
                    (if (= "searching" (.-reason (:r plan)))
                      (recur (inc i))
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
