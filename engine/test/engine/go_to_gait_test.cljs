(ns engine.go-to-gait-test
  "go-to's :gait (:auto, :walk, :sneak), seen through path-preview and body-policy: :walk never sprints, :sneak plans at sneak
  speed, never sprints, and holds sneak on level steps only (released for drops, gaps, climbs, water, in the air); the body
  setting is the default, the arg wins."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.path.planner-tuned :as planner]
            [engine.path-preview-test :as pt]
            [engine.planner-fixture :as pf]
            [engine.test-util :as tu :refer [floor box]]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.world :as wworld]))

(defn ^:async preview-out [blocks args]
  (:out (await (pt/preview blocks args))))

;; level ground x -3..40
(def flat (floor -3 -3 40 3))

;; a floor to x 5, a one-block step down to a floor at feet 63 from x 6 (the only way on is a drop of 1)
(def step-down (merge (floor -3 -3 5 3) (floor 62 6 -3 40 3)))

;; a floor to x 5, a gap of 2 cells (x 6 7), the floor again from x 8
(def gap-2 (merge (floor -3 -3 5 3) (floor 8 -3 40 3)))

;; gap-2 with a walkway round the gap at z 5..7
(def gap-2-detour (merge gap-2 (floor -3 4 40 7)))

;; a floor to x 5, a gap of 1 cell (x 6), the floor again from x 7
(def gap-1 (merge (floor -3 -3 5 3) (floor 7 -3 40 3)))

(deftest sneaking-takes-longer-than-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [auto (await (preview-out flat {:pos [30 64 1]}))
              walk (await (preview-out flat {:pos [30 64 1] :gait :walk}))
              sneak (await (preview-out flat {:pos [30 64 1] :gait :sneak}))]
          (is (true? (:found sneak)))
          (is (= (:seconds auto) (:seconds walk)) "walking and auto are priced alike on level ground")
          (is (> (:seconds sneak) (* 3 (:seconds walk))) "sneak speed is about a third of walking"))))))

(deftest sneaking-steps-down-and-over-a-one-block-gap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:found (await (preview-out step-down {:pos [20 63 1]})))))
        (is (true? (:found (await (preview-out step-down {:pos [20 63 1] :gait :walk})))))
        (is (true? (:found (await (preview-out step-down {:pos [20 63 1] :gait :sneak})))) "sneak is released for the step down")
        (is (true? (:found (await (preview-out gap-1 {:pos [20 64 1] :gait :sneak})))) "a walking jump clears 1")))))

(deftest walking-takes-no-gap-of-two
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:found (await (preview-out gap-2 {:pos [20 64 1]})))) "a sprint jump clears 2")
        (is (false? (:found (await (preview-out gap-2 {:pos [20 64 1] :gait :walk})))))
        (is (false? (:found (await (preview-out gap-2 {:pos [20 64 1] :gait :sneak})))))))))

(deftest sneaking-plans-round-a-gap-it-cannot-jump
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (await (preview-out gap-2-detour {:pos [20 64 1] :gait :sneak}))]
          (is (true? (:found out)) "the planner is told the gait's limits: it finds the way round")
          (is (zero? (get-in out [:moves :gap] 0))))))))

(defn heuristic-at
  "The planner's heuristic at x z (feet y 64) for a goal at 12 64 2 under costs."
  [costs x z]
  (let [^js s (#'planner/new-search (pf/world {}) (clj->js {:from pf/start :goal (pf/near 12 64 2)}) (pf/options-js {:costs costs}))]
    (.heuristic s x z)))

(deftest the-heuristic-never-overestimates-a-sneaking-swim
  (is (< (js/Math.abs (- (heuristic-at {} 2 2) (* 10 0.23164234422052352))) 1e-9) "auto: walking seconds, as before")
  (is (<= (heuristic-at {:walkS 0.7722007722007722 :sprintS 0.7722007722007722} 2 2) (+ 5 1e-9))
      "sneak: no more than swimming (swimH 0.5 a block)"))

(deftest a-bad-gait-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [bad [:run "walk" 3]]
          (is (= :bad-gait (:reason (await (preview-out flat {:pos [30 64 1] :gait bad})))) (pr-str bad)))))))

(defn policy [setting args]
  (wworld/body-policy {:primitives (tu/fake {:self {:health 20 :food 20}}) :args args
                       :view (constantly {:now 0 :data {:entries {:walk-settings (when setting [{:t 0 :data setting}])}}})}))

(deftest the-gait-shapes-the-executor-policy
  (are [setting args sprint? sneak? drops?] (let [p (policy setting args)]
                                              (and (= sprint? (boolean (:sprint p)))
                                                   (= sneak? (= :sneak (:gait p)))
                                                   (= drops? (contains? (:moves p) :drop))))
    nil {} true false true
    nil {:gait :auto} true false true
    nil {:gait :walk} false false true
    nil {:gait :sneak} false true true
    {:gait :sneak} {} false true true
    {:gait :sneak} {:gait :walk} false false true
    {:gait :bogus} {} true false true))

(defn on [move] {:steps [{:move :start} {:move move}] :i 1})

(def ground {:on-ground true :in-water false :on-climbable false :on-scaffolding false})

(deftest sneaking-holds-sneak-on-level-steps-only
  (are [gait state pose expected] (= expected (select-keys (walk/gait-controls {:gait gait} state pose {:sneak false :sprint true}) [:sneak :sprint]))
    :sneak (on :walk) ground {:sneak true :sprint false}
    :sneak (on :diagonal) ground {:sneak true :sprint false}
    :sneak (on :corner) ground {:sneak true :sprint false}
    :sneak (on :jump) ground {:sneak true :sprint false}
    :sneak (on :drop) ground {:sneak false :sprint false}
    :sneak (on :gap) ground {:sneak false :sprint false}
    :sneak (on :climb-down) ground {:sneak false :sprint false}
    :sneak (on :climb-up) ground {:sneak false :sprint false}
    :sneak (on :jump-climb) ground {:sneak false :sprint false}
    :sneak (on :swim) ground {:sneak false :sprint false}
    :sneak {:steps [{:move :start} {:move :walk :swim true}] :i 1} ground {:sneak false :sprint false}
    :sneak (on :walk) (assoc ground :in-water true) {:sneak false :sprint false}
    :sneak (on :walk) (assoc ground :on-ground false) {:sneak false :sprint false}
    :sneak (on :walk) (assoc ground :on-climbable true) {:sneak false :sprint false}
    :sneak (on :walk) (assoc ground :on-scaffolding true) {:sneak false :sprint false}
    :walk (on :walk) ground {:sneak false :sprint true}
    :auto (on :walk) ground {:sneak false :sprint true}))
