(ns engine.go-to-gait-test
  "go-to's :gait (:auto, :walk, :sneak), seen through path-preview and body-policy: :walk never sprints, :sneak plans at sneak
  speed with no drop and no gap and holds sneak while walking; the body setting is the default, the arg wins."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.path-preview-test :as pt]
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

(deftest sneaking-takes-no-drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:found (await (preview-out step-down {:pos [20 63 1]})))))
        (is (true? (:found (await (preview-out step-down {:pos [20 63 1] :gait :walk})))))
        (is (false? (:found (await (preview-out step-down {:pos [20 63 1] :gait :sneak})))) "sneak stops at an edge")))))

(deftest walking-takes-no-gap-of-two
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:found (await (preview-out gap-2 {:pos [20 64 1]})))) "a sprint jump clears 2")
        (is (false? (:found (await (preview-out gap-2 {:pos [20 64 1] :gait :walk})))))
        (is (false? (:found (await (preview-out gap-2 {:pos [20 64 1] :gait :sneak})))))))))

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
    nil {:gait :sneak} false true false
    {:gait :sneak} {} false true false
    {:gait :sneak} {:gait :walk} false false true
    {:gait :bogus} {} true false true))

(deftest sneaking-holds-sneak-on-land-not-in-water
  (are [gait pose controls expected] (= expected (select-keys (walk/gait-controls {:gait gait} pose controls) [:sneak :sprint]))
    :sneak {:in-water false} {:sneak false :sprint true} {:sneak true :sprint false}
    :sneak {:in-water true} {:sneak false :sprint false} {:sneak false :sprint false}
    :walk {:in-water false} {:sneak false :sprint false} {:sneak false :sprint false}
    :auto {:in-water false} {:sneak false :sprint true} {:sneak false :sprint true}))
