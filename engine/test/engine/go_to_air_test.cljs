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
