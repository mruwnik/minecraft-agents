(ns engine.stuck-giveup-test
  "The stuck trigger counts only moves that show the body held where it stands (a walk that carried the body away, or
  moves made somewhere else, are not); walk-near! does not take a drop it cannot climb back toward a target it cannot
  reach; unstick ends once a body that was enclosed is out, though the goal stays out of reach."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.jobs.reach :as reach]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [engine.unstick-test :as ut]))

(defn stuck-in? [world moves]
  (let [{:keys [eng p]} (ut/setup world)]
    (ut/seed-moved! eng moves)
    ((:when (get triggers/all :stuck)) p (mem/view (:store eng)) {})))

(def target {:x 12 :y 64 :z 0})
(def far-end {:x 30 :y 64 :z 0})

(deftest walks-that-carried-the-body-away-are-not-stuck-evidence
  ;; the moat case: an attack walks 25 blocks off and gives up there (no path), a second attack from the start walks off
  ;; the same way: four "blocked" moves, none of which held the body
  (let [away {:from ut/at5 :to far-end :status "blocked" :target target}
        no-path {:from far-end :to far-end :status "blocked" :target target :no-path true}]
    (is (false? (stuck-in? {:self {:pos far-end} :floor tu/walk-floor} [away no-path no-path away])))))

(deftest bad-moves-made-somewhere-else-do-not-hold-the-body-here
  (doseq [[here expected] [[{:x 20 :y 64 :z 0} false] [ut/at5 true]]]
    (is (= expected (stuck-in? {:self {:pos here} :floor tu/walk-floor} (repeat 4 (ut/bad-move)))) (pr-str here))))

;; ------------------------------------------------------------------ walk-near! and a drop it cannot climb back

(def cliff-island
  "A plateau (feet 64) to x 10, a 3-drop to a floor (feet 61) that runs to the loaded edge at x 47, and on it a stone
  pillar at x 47 (the last loaded column) whose top (feet 67) no walk reaches."
  (merge (floor -2 -3 10 3) (floor 60 11 -3 47 3) (box 47 61 0 47 66 0 "stone")))

(deftest walk-near-does-not-drop-off-a-cliff-toward-a-target-it-cannot-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (ut/setup {:self {:pos {:x 0 :y 64 :z 0}} :blocks cliff-island})
              top {:x 47 :y 67 :z 0}]
          (core/submit! eng (list 'jobs.movement.pace {:a top :b top :laps 1 :rounds 1}) {})
          (await (core/tick! eng))
          (is (= 64 (js/Math.floor (.-y (.-pos (.self p))))) "still on the plateau"))))))

;; ------------------------------------------------------------------ unstick ends once out

(def wide-surface-pit
  "A 3-deep 1x1 dirt pit (feet at (5,61,0)) in a wide stone floor (feet 64): out of the pit the body is not enclosed."
  (dissoc (merge (box -30 60 -20 40 63 20 "stone") ut/pit) "5,61,0" "5,62,0" "5,63,0"))

(deftest unstick-ends-when-an-enclosed-body-is-out-though-the-goal-stays-out-of-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [here {:x 5 :y 61 :z 0}
              {:keys [eng p seen]} (ut/setup {:self {:pos here} :blocks (merge wide-surface-pit (box 20 64 0 20 70 0 "stone"))
                                              :inventory [{:name "dirt" :count 4}]})]
          (await (ut/run-unstick! eng here {:x 20 :y 71 :z 0}))
          (is (false? (reach/enclosed? p)) "the body can walk out")
          (is (= [] (:list (core/state eng))) "the spell is over")
          (is (nil? (ut/failed-event seen)) "no give-up after the body is out"))))))
