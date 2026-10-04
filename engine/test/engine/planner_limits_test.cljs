(ns engine.planner-limits-test
  "engine.path.planner-tuned told the executor's limits (options.limits from engine.path.executor/planner-limits): it plans
  round the steps the executor refuses; without the option it plans the old route."
  (:require [cljs.test :refer [deftest are]]
            [engine.path.executor :as ex]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu]
            [jobs.debug.walk-plan :as walk-plan]))

(defn box
  "A fake block map: name in every cell of x0..x1, y0..y1, z0..z1."
  [x0 y0 z0 x1 y1 z1 name]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(defn plan-over
  "Plan from (0 64 1) to goal over blocks, told the executor's limits when limited?. {:status :moves :refused}: the
  planner's status, the set of step kinds, the executor's refusal kind for the plan (nil when it walks it)."
  [blocks [x y z] limited?]
  (let [pw (.pathWorld (tu/fake {:blocks blocks}))
        limits (when limited? (ex/planner-limits ex/policy (walk-plan/solid-fn pw)))
        r (planner/plan (.-snapshot pw)
                        #js {:from #js {:x 0 :y 64 :z 1 :px 0.5 :pz 1.5} :goal #js {:kind "near" :x x :y y :z z :range 0}}
                        #js {:table (.-table pw) :space (.-space pw) :weight 1.2 :limits limits})
        steps (when (.-path r) (walk-plan/plan-steps pw r))]
    {:status (.-status r) :moves (set (map :move steps)) :refused (:kind (ex/refusal ex/policy steps))}))

;; A trench x 5..6 (no floor) between a near floor x 0..4 and a far one, z -8..10; the walk round crosses at z 9..10.

(def near-floor (box 0 63 -8 4 63 10 "stone"))
(def bridge (box 5 63 9 6 63 10 "stone"))

(def gap-up-only (merge near-floor (box 7 63 -8 12 64 10 "stone")))
(def gap-up-around (merge gap-up-only bridge))

(def ceiling-only (merge near-floor (box 7 63 -8 12 63 10 "stone") (box 4 66 -8 6 66 10 "stone")))
(def ceiling-around (merge near-floor (box 7 63 -8 12 63 10 "stone") (box 4 66 -8 6 66 8 "stone") bridge))

;; a vine at x 5 against a wall; from its second cell a gap jump over x 6 onto a platform with feet at 66; the walk round
;; climbs two blocks at z 9..10
(def vine-only (merge (box 0 63 -2 6 63 10 "stone") (box 5 64 2 5 68 2 "stone") (box 5 64 1 5 66 1 "vine")
                      (box 7 63 -2 12 65 10 "stone")))
(def vine-around (merge vine-only (box 6 64 9 6 64 10 "stone")))

;; water x 5..8 over stone, crossed dry at z 9..10
(def swim-only (merge (box 0 62 -8 14 62 10 "stone") (box 0 63 -8 4 63 10 "stone") (box 9 63 -8 14 63 10 "stone")
                      (box 5 63 -8 8 63 10 "water")))
(def swim-around (merge swim-only (box 5 63 9 8 63 10 "stone")))

(def four-wide-around (merge near-floor (box 9 63 -8 14 63 10 "stone") (box 5 63 9 8 63 10 "stone")))

(deftest the-limited-planner-walks-round-what-the-executor-refuses
  (are [blocks goal refused] (= [{:status "found" :refused refused} {:status "found" :refused nil}]
                                (map #(select-keys (plan-over blocks goal %) [:status :refused]) [false true]))
    gap-up-around [10 65 1] :gap-up
    ceiling-around [10 64 1] :gap-low-ceiling
    vine-around [10 66 1] :gap-takeoff))

(deftest the-limited-planner-finds-no-whole-path-where-only-the-refused-step-leads
  (are [blocks goal refused] (= [{:status "found" :refused refused} false]
                                [(select-keys (plan-over blocks goal false) [:status :refused])
                                 (= "found" (:status (plan-over blocks goal true)))])
    gap-up-only [10 65 1] :gap-up
    ceiling-only [10 64 1] :gap-low-ceiling
    vine-only [10 66 1] :gap-takeoff))

;; water x 5..8, 3 deep (y 61..63) over stone at 60: a flush bank on the near side (stand 64), on the far side flush (x 9..14
;; stand 64) or one higher (stand 65), which a floating body cannot climb onto
(def deep-pond (merge (box 0 60 -8 14 60 10 "stone") (box 0 61 -8 4 63 10 "stone") (box 5 61 -8 8 63 10 "water")))
(def deep-flush (merge deep-pond (box 9 61 -8 14 63 10 "stone")))
(def deep-high (merge deep-pond (box 9 61 -8 14 64 10 "stone")))

(deftest water-is-planned-within-the-limits
  (are [blocks goal] (= {:status "found" :refused nil :swims true}
                        (let [r (plan-over blocks goal true)]
                          {:status (:status r) :refused (:refused r) :swims (contains? (:moves r) :swim)}))
    swim-only [12 64 1]
    deep-flush [12 64 1]))

(deftest a-bank-too-high-to-leave-the-water-is-no-path-either-way
  (are [limited?] (not= "found" (:status (plan-over deep-high [12 65 1] limited?)))
    false
    true))

(deftest a-gap-over-4-is-never-planned
  (are [limited?] (= {:status "found" :refused nil :gap false}
                     (let [r (plan-over four-wide-around [12 64 1] limited?)]
                       {:status (:status r) :refused (:refused r) :gap (contains? (:moves r) :gap)}))
    false
    true))

(defn jump-course
  "Floor x 0..4, then n empty cells, then floor dy higher (z 0..2): the gap jump is the only way."
  [n dy]
  (merge (box 0 63 0 4 63 2 "stone") (box (+ 5 n) (+ 63 dy) 0 (+ 9 n) (+ 63 dy) 2 "stone")))

(deftest the-gap-jumps-the-executor-walks-are-still-planned
  (are [n dy] (= {:status "found" :refused nil :gap true}
                 (let [r (plan-over (jump-course n dy) [(+ 7 n) (+ 64 dy) 1] true)]
                   {:status (:status r) :refused (:refused r) :gap (contains? (:moves r) :gap)}))
    1 0
    2 0
    3 0
    1 -1
    2 -1
    3 -1))
