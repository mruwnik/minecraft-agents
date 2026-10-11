(ns engine.go-to-depth-test
  "go-to's :y-margin: blocks past the lowest and highest of body and goal that a walk's search may reach, so a route that
  dips far below both ends (through deep caves) is refused instead of walked; absent: the walks' wide box."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.go-to-test :as gt]
            [engine.path-preview-test :as pt]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [box floor]]))

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

;; The same dive cut 3 high through a dirt mass (x -30..40, y 6..59, z -1..1): feet 40 at x 0, down one block a column to
;; feet 10 (x 30), out onto a wide cave floor (more cells than a shut-in body has), and from its far side a ramp beside
;; the mass up to its top. The goal stands on the mass, 20 above the body. No sky light.
(def mine
  (let [heights (concat [39] (range 38 8 -1) (repeat 10 9))
        cave (for [[x y] (map-indexed vector heights) dy [1 2 3]] (str x "," (+ y dy) ",0"))
        ramp (for [i (range 51)] [(str (- 70 i) "," (+ 9 i) ",2") "stone"])]
    (merge (apply dissoc (box -30 6 -1 40 59 1 "dirt") cave)
           (floor 9 41 -17 75 17)
           (into {} ramp))))

(def on-top [20 60 0])

(def in-mine {:blocks mine :self {:pos {:x 0.5 :y 40 :z 0.5}} :light [0 0]})

(defn ^:async climb!
  "go! with ticks enough for a stair, a second of fake time each (the stair waits out a lava delay): {:out :p :seen}."
  [world args]
  (let [clock (atom 1000000)
        {:keys [eng] :as s} (gt/setup (assoc world :clock clock))
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out args)))]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 1000) (seq (:list (core/state eng))))
        (await (core/tick! eng))
        (swap! clock + 1000)
        (recur (inc i))))
    (assoc s :eng eng :out out)))

(defn lowest-walk [s] (apply min (map #(:y (:to %)) (gt/moved (:eng s)))))

(deftest a-buried-body-whose-way-dives-cuts-a-stair-up-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (climb! in-mine {:pos on-top}))]
          (is (true? (:arrived @out)))
          (is (= 60 (second (gt/at p))))
          (is (= #{:stair} (set (map :step (gt/events-of s :go-to.escalated)))))
          (is (= #{:way-dives} (set (map :why (gt/events-of s :go-to.escalated)))))
          (is (<= 16 (lowest-walk s)) "never below the floor of the call"))))))

;; The mine with the floor of one step of the climb's westward stair missing ([-8 47 0]: no block carried to lay), and
;; more dirt south of there (x -9..0, y 44..59, z 2..14) for a stair on another heading.
(def holed-mine (merge (dissoc mine "-8,47,0") (box -9 44 2 0 59 14 "dirt")))

(deftest a-climb-whose-stair-stops-part-way-goes-on-along-another-heading
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (climb! (assoc in-mine :blocks holed-mine) {:pos on-top}))]
          (is (true? (:arrived @out)))
          (is (= [:no-floor] (map :reason (gt/events-of s :go-to.escalation-stopped))))
          (is (<= 16 (lowest-walk s)) "it did not walk back down toward the dive"))))))

(deftest with-the-climb-stair-setting-off-the-dive-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (settings/with-settings {:jobs.movement.go-to/climb-stair false}
                                           #(climb! in-mine {:pos on-top})))]
          (is (= {:arrived false :why :way-dives} (select-keys @out [:arrived :why])))
          (is (empty? (gt/events-of s :go-to.escalated))))))))
