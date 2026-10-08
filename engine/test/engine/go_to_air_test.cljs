(ns engine.go-to-air-test
  "go-to from under water to a goal with its head in air: the planner counts only air the body has seen (airSeen), so an
  unseen goal head would refuse the way for :air."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt]
            [engine.test-util :as tu :refer [box]]))

;; go-to-air-budget drowning-body-swims-to-seen-air: a 2 high water tunnel (feet 64, head 65) under a stone roof, an air
;; pocket in the roof (x 11..13, y 66) 6 cells east of the body; oxygen 4 (12.4 s of air used). Looking east the body sees
;; a cell of the pocket edge-on through the water; looking west it sees none, and go-to looks at the goal's head first
(def tunnel
  (merge (box -4 60 -4 30 63 20 "stone") (box 3 64 7 22 68 9 "stone")
         (box 4 64 8 20 65 8 "water") (box 11 66 8 13 66 8 "air")))

(defn ^:async go-east! [yaw]
  (let [{:keys [out]} (await (gt/go! {:self {:pos [5.5 64 8.5] :inWater true :oxygen 4 :yaw yaw} :blocks tunnel :light [15 15]}
                                     {:pos [12 65 8] :range 1 :escalate false}))]
    @out))

(deftest a-drowning-body-swims-to-a-pocket-it-has-seen-a-cell-of
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (await (go-east! (/ js/Math.PI -2)))]
          (is (true? (:arrived out)) (str "result " out)))))))

(deftest a-drowning-body-facing-away-looks-at-the-goal-and-swims-to-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (await (go-east! (/ js/Math.PI 2)))]
          (is (true? (:arrived out)) (str "result " out)))))))

;; the body at the foot of a 6 deep 1x1 water shaft (x 4, water 64..69, land over all at feet 70), looking level, with
;; 10.5 or 11.25 s of air used: no way to the goal fits the air left (the swim up passes airLimit) and the goal's head is
;; no way to air (far, or not seen), yet the shaft's top is the way to air. A player swims up first.
(def shaft
  (merge (box -4 60 -4 40 63 70 "stone") (box 0 64 0 31 69 63 "stone") (box 4 64 8 4 69 8 "water")))

(deftest a-body-short-of-air-at-the-foot-of-a-shaft-swims-up-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [goal [[8 70 8] [18 70 8]] oxygen [6 5]]
          (let [{:keys [out p]} (await (gt/go! {:self {:pos [4.5 64 8.5] :inWater true :oxygen oxygen :pitch 0} :blocks shaft :light [15 15]}
                                               {:pos goal :range 1 :escalate false}))]
            (is (true? (:arrived @out)) (str "to " goal " at oxygen " oxygen ": result " @out " at " (gt/at p)))))))))

;; should fail: the only way on is a 32 cell sealed tunnel (16 s) to a shaft at x 37, the land cut by a bedrock ridge (x 11):
;; it swims up for air, and with the air back no way fits it still
(def long-tunnel
  (merge (box -4 60 -4 45 69 63 "stone") (box 4 64 8 4 69 8 "water")
         (box 5 64 8 36 65 8 "water") (box 37 64 8 37 69 8 "water") (box 11 70 -4 11 73 63 "bedrock")))

(deftest a-body-short-of-air-swims-up-and-gives-up-air-when-no-way-fits-a-breath
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [4.5 64 8.5] :inWater true :oxygen 6 :pitch 0} :blocks long-tunnel :light [15 15]}
                                             {:pos [38 70 8] :range 1 :escalate false}))]
          (is (= :air (:why @out)) (str "result " @out))
          (is (<= 69 (second (gt/at p))) (str "it swam up for air: at " (gt/at p))))))))
