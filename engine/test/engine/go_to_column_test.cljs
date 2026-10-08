(ns engine.go-to-column-test
  "jobs.movement.go-to's column goals ([x z], y left out) and its re-aim of a y with nothing standable in range."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt :refer [go! at events-of refusal]]
            [engine.test-util :as tu :refer [box floor]]))

(def flat (floor -2 -3 40 3))

(defn near-x10? [p] (< (js/Math.abs (- 10 (first (at p)))) 2))

(deftest a-y-off-by-three-arrives-on-the-ground-of-that-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [y [67 61]]
          (let [{:keys [out p] :as s} (await (go! {:blocks flat} {:pos [10 y 0]}))]
            (is (= {:arrived true :resolved {:x 10 :y 64 :z 0}} @out) (str "y " y " " (pr-str @out)))
            (is (near-x10? p) (str "y " y " at " (at p)))
            (is (= [{:goal {:x 10 :y y :z 0} :to {:x 10 :y 64 :z 0}}]
                   (mapv #(select-keys % [:goal :to]) (events-of s :goal-resolved)))
                (str "y " y ": the re-aim is said once"))))))))

(deftest a-column-goal-arrives-on-the-ground-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a step up at x 8..12: the ground there stands at feet 65
        (let [hill (merge flat (box 8 64 -3 12 64 3 "stone"))]
          (doseq [pos [[10 0] {:x 10 :z 0}]]
            (let [{:keys [out p]} (await (go! {:blocks hill} {:pos pos :range 0}))]
              (is (= {:arrived true :resolved {:x 10 :y 65 :z 0}} @out) (str (pr-str pos) " " (pr-str @out)))
              (is (= [10 65 0] (mapv #(js/Math.floor %) (at p))) (pr-str pos)))))))))

(deftest a-column-goal-picks-the-ground-nearest-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a roof at y 70 over x 8..12: standable on top (feet 71) and below (feet 64); the body walks at feet 64
        (let [roofed (merge flat (box 8 70 -3 12 70 3 "stone"))
              {:keys [out p]} (await (go! {:blocks roofed} {:pos [10 0] :range 0}))]
          (is (= {:arrived true :resolved {:x 10 :y 64 :z 0}} @out))
          (is (= [10 64 0] (mapv #(js/Math.floor %) (at p)))))))))

(deftest range-three-arrives-within-three
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [20 64 0] :range 3}))
              [x _ z] (at p)]
          (is (= {:arrived true} @out))
          (is (<= (js/Math.hypot (- 20 x) z) 3) (str "at " (at p)))
          (is (> (js/Math.hypot (- 20 x) z) 1.5) (str "stopped at the edge of the range, at " (at p))))))))

(deftest y-slack-zero-gives-up-goal-not-standable-as-before
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [10 67 0] :y-slack 0}))]
          (is (= {:arrived false :reason :unreachable :why :goal-not-standable} (select-keys @out [:arrived :reason :why])))
          (is (= [0 64 0] (at p))))))))

(deftest a-y-farther-off-than-the-slack-is-not-re-aimed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go! {:blocks flat} {:pos [10 70 0]}))]
          (is (= :goal-not-standable (:why @out)) "6 above the ground, slack 4: the planner's verdict stands"))))))

(deftest a-column-goal-in-unloaded-land-is-walked-toward
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the floor ends at x 40; the column at x 120 is unloaded
        (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [120 0]}))]
          (is (not= :goal-not-standable (:why @out)) (pr-str @out))
          (is (> (first (at p)) 30) "walked to the loaded edge"))))))

(deftest a-column-goal-under-sight-resolves-on-approach
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; perception (radius 16): the column at x 30 is out of sight from the start
        (let [{:keys [out p]} (await (go! {:blocks flat :light [15 0]} {:pos [30 0] :range 0}))]
          (is (= {:arrived true :resolved {:x 30 :y 64 :z 0}} @out) (pr-str @out))
          (is (= [30 64 0] (mapv #(js/Math.floor %) (at p)))))))))

(deftest an-unseen-column-under-unknown-is-walked-toward-and-resolved-once-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; :unknown true reads no raw cell: the mound at x 28..32 (ground at feet 65) is out of sight from the start, so
        ;; the column first keeps the body's feet y (64, inside the mound) and is aimed again once the body sees it
        (let [mound (merge flat (box 28 64 -3 32 64 3 "stone"))
              {:keys [out p] :as s} (await (go! {:blocks mound :light [15 0]} {:pos [30 0] :range 0 :unknown true}))]
          (is (= {:arrived true :resolved {:x 30 :y 65 :z 0}} @out) (pr-str @out))
          (is (= [30 65 0] (mapv #(js/Math.floor %) (at p))))
          (is (= [{:x 30 :y 65 :z 0}] (mapv :to (events-of s :goal-resolved))) "resolved once, on approach"))))))

(deftest a-column-pos-is-accepted-at-submit-and-a-bad-one-refused
  (is (nil? (refusal {:pos [10 0]})))
  (is (nil? (refusal {:pos {:x 10 :z 0}})))
  (is (nil? (refusal {:pos [1 64 -3]})))
  (is (nil? (refusal {:pos {:x 1 :y 64 :z -3}})))
  (is (re-find #":pos must be \[x y z\]" (refusal {:pos [10]})))
  (is (re-find #":pos must be \[x y z\]" (refusal {:pos {:x 1 :y 2}})))
  (is (re-find #":y-slack must be a whole number" (refusal {:pos [10 64 0] :y-slack -1}))))
