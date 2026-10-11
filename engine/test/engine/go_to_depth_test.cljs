(ns engine.go-to-depth-test
  "go-to's :y-margin: blocks past the lowest and highest of body and goal that a walk's search may reach, so a route that
  dips far below both ends (through deep caves) is refused instead of walked; absent: the walks' wide box."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt]
            [engine.path-preview-test :as pt]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [floor]]))

;; level ground x 0..6 (feet 64), a valley stepping down one block a column to feet 52 and up again, level ground to x 40
(def valley
  (let [heights (concat (repeat 7 63) (range 62 50 -1) (range 52 64) (repeat 17 63))]
    (apply merge (map-indexed (fn [x y] (floor y x -3 x 3)) heights))))

(def goal [40 64 1])

(deftest a-narrow-margin-refuses-the-route-through-the-valley
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (:found (:out (await (pt/preview valley {:pos goal :y-margin 5}))))))))))

(deftest a-margin-that-holds-the-valley-or-none-walks-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:found (:out (await (pt/preview valley {:pos goal :y-margin 14}))))))
        (is (true? (:found (:out (await (pt/preview valley {:pos goal}))))))))))

;; A body whose goal is far above and whose only way dives first: feet 40 at x 0, a ramp down one block a column to feet
;; 10 (x 30), 8 columns level, then up one a column to the goal at feet 60 (x 88). No sky light anywhere: under ground.
(def dive
  (let [heights (concat [39] (range 38 8 -1) (repeat 8 9) (range 10 60))]
    (apply merge (map-indexed (fn [x y] (floor y x 0 x 0)) heights))))

(def top [88 60 0])

(def buried {:blocks dive :self {:pos {:x 0.5 :y 40 :z 0.5}} :light [0 0]})

(deftest a-buried-body-does-not-dive-to-reach-a-goal-far-above
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! buried {:pos top}))]
          (is (= {:arrived false :why :way-dives :dip 24} (select-keys @out [:arrived :why :dip])))
          (is (<= 20 (:rise @out)))
          (is (re-find #"dives more than 24 blocks" (str (:text @out))))
          (is (<= 16 (second (gt/at p))) "never below the floor of the call: 24 under where it began"))))))

(deftest the-dive-is-walked-under-the-sky-with-a-y-margin-or-a-deeper-dip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:arrived @(:out (await (gt/go! (assoc buried :light [15 0]) {:pos top}))))) "sky light on the body")
        (is (true? (:arrived @(:out (await (gt/go! buried {:pos top :y-margin 96}))))))
        (is (true? (:arrived @(:out (await (settings/with-settings {:jobs.movement.go-to/climb-dip 30}
                                             #(gt/go! buried {:pos top}))))))
            "setting ::climb-dip")
        (is (true? (:arrived @(:out (await (settings/with-settings {:jobs.movement.go-to/climb-rise 50}
                                             #(gt/go! buried {:pos top}))))))
            "setting ::climb-rise")))))
