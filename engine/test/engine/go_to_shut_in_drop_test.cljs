(ns engine.go-to-shut-in-drop-test
  "go-to's turn-back gate (jobs.movement.go-to.turn-back) prices a drop the body cannot jump back up when the plan's way
  on from its landing rests on cells it has not seen: a solid one shuts it in there."
  (:require [cljs.test :refer [deftest is]]
            [engine.core :as core]
            [engine.go-to-escalate-support :refer [setup]]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.cost :as cost]
            [jobs.movement.go-to.turn-back :as turn-back]))

(defn ctx-of [world worth]
  (let [{:keys [eng]} (setup world)]
    (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth worth} :round 1})))

(defn steps [& cells] (mapv (fn [[x y z]] {:x (+ x 0.5) :y y :z (+ z 0.5) :h 0}) cells))

(def bedrock-trench-room
  "A bedrock room (inside x -2..2, z -2..2, feet y 64), nothing to dig, opened at its east door (x 3, z 0) onto a 3-deep,
  1-wide trench (x 4, z -6..6, feet y 61) closed at both ends in bedrock; the yard beyond is level with the room floor."
  (let [inside (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z))
        trench (for [z (range -6 7) y [61 62 63]] (str "4," y "," z))]
    (apply dissoc (merge (box -20 58 -20 30 63 20 "bedrock") (box -3 64 -3 3 66 3 "bedrock")) "3,64,0" "3,65,0" (concat inside trench))))

(def trench-plan
  "The plan from the door into the trench, along it past what the body sees from the door (sight 5: the trench's walls
  and floor beside the landing) and up its unseen far end, as live (sweep a2268268f)."
  (apply steps [3 64 0] [4 61 0] (concat (for [z (range 1 9)] [4 61 z]) [[5 62 8] [6 63 8] [7 64 8] [8 64 4] [8 64 0]])))

(deftest a-drop-into-a-bedrock-trench-whose-climb-out-it-has-not-seen-costs-more-than-the-goal-is-worth
  (let [c (ctx-of {:sight 5 :self {:pos {:x 3.5 :y 64 :z 0.5}} :blocks bedrock-trench-room} 1200)
        t (turn-back/turn-back c trench-plan)]
    (is (= 2400 (:trap-s (:shut t))) "nothing it may dig there: the call's worth spent shut in, and the goal lost")
    (is (some? (turn-back/exceeded c trench-plan)) (pr-str t))))

(def dirt-pit-field
  "A dirt field (top y 63) with a 3-deep, 3-wide pit (x 4..6, z -1..1, feet y 61) on the line from the body to the goal."
  (apply dissoc (box -10 55 -10 20 63 10 "dirt") (for [x (range 4 7) z (range -1 2) y [61 62 63]] (str x "," y "," z))))

(deftest a-pit-whose-climb-out-it-has-not-seen-is-dearer-than-the-dry-way-round
  (let [worth 300
        c (ctx-of {:sight 4 :self {:pos {:x 3.5 :y 64 :z 0.5}} :blocks dirt-pit-field} worth)
        through (steps [3 64 0] [4 61 0] [5 61 0] [6 61 0] [7 62 0] [8 63 0] [9 64 0] [10 64 0])
        round (steps [3 64 0] [3 64 1] [3 64 2] [4 64 2] [5 64 2] [6 64 2] [7 64 2] [8 64 2] [9 64 1] [10 64 0])
        extra-s (- (:cost (cost/walk-cost {:distance (dec (count round))})) (:cost (cost/walk-cost {:distance (dec (count through))})))
        t (turn-back/turn-back c through)]
    (is (< extra-s (:cost t)) (pr-str t))
    (is (nil? (turn-back/turn-back c round)) "the way round rests on no drop")
    (is (nil? (turn-back/exceeded c through)) "dirt is dug out of: not a trap worth refusing the goal for")))
