(ns engine.go-to-wall-walk-test
  "go-to's walk to the wall round a walled-in goal (walk/walk-to-wall!) over the sensed-only world: the flood reads only
  cells the body has seen, so a body that has seen none nearer looks round before deciding there is no nearer cell."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.go-to-escalate-support :refer [below-pocket]]
            [engine.go-to-sensed-test :as gs]
            [engine.test-util :as tu]))

(defn lit [{:keys [p]}] (swap! (fake/state p) merge {:light-default [15 0] :pitch 0}))

(deftest a-walk-only-body-that-has-not-looked-round-looks-then-walks-to-the-cell-over-the-pocket
  ;; nothing seen yet when the walk ends :goal-enclosed: without a look the flood holds only the feet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks below-pocket}
                                                      {:pos [3 60 0] :range 0 :ways #{:walk}} lit))
              pos (.-pos (.self p))]
          (is (false? (:arrived out)) (pr-str out))
          (is (seq (filter #(= :go-to.wall-walk (:kind %)) seen)) "the wall walk runs")
          (is (<= 2 (js/Math.floor (.-x pos))) (str "walked toward the pocket: at " (.-x pos))))))))
