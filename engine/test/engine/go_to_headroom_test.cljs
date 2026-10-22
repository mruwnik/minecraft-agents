(ns engine.go-to-headroom-test
  "go-to's give-up for a walled-in goal names a low block with no headroom over it (bed, slab under a lintel) as the way in."
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is]]
            [jobs.movement.go-to.headroom :as headroom]
            [jobs.movement.go-to.result :as end]
            [engine.test-util :as tu]))

(defn room
  "A closed room x 10..11, z 0..1 (walls and roof y 66 round x 9..12, z -1..2) whose west wall at z 0 is the block low at
  (9 64 0) with air above it; roof-over-low? puts the roof over that cell too (a 2-high lintel)."
  [low roof-over-low?]
  (let [shell (merge (tu/box 9 64 -1 12 65 2 "stone") (tu/box 9 66 -1 12 66 2 "stone"))
        hollow (for [x [10 11] z [0 1] y [64 65]] (str x "," y "," z))
        opened (cond-> (apply dissoc shell (conj hollow "9,65,0" "9,64,0"))
                 (not roof-over-low?) (dissoc "9,66,0"))]
    (tu/fake {:self {:pos {:x 4.5 :y 64 :z 0.5}} :floor [-5 -5 20 5]
              :blocks (assoc opened "9,64,0" low)})))

(def goal {:x 11 :y 64 :z 1})

(deftest a-low-block-under-a-lintel-is-named-as-the-way-in
  (is (= {:block "red_bed" :at [9 64 0] :under [9 66 0]} (headroom/low-passage (room "red_bed" true) goal)))
  (is (= "oak_slab" (:block (headroom/low-passage (room "oak_slab" true) goal)))))

(deftest a-room-cell-the-body-has-not-seen-is-no-way-to-the-low-block
  (let [p (room "red_bed" true)
        sensed (.-sensedAt p)]
    (aset p "sensedAt" (fn [q] (if (= [10 64 0] [(.-x q) (.-y q) (.-z q)]) #js {:unknown true} (sensed q))))
    (is (nil? (headroom/low-passage p goal)) "the one cell next to the bed never looked at: not walked through on the true world's word")))

(deftest no-low-passage-with-headroom-or-a-full-wall
  (is (nil? (headroom/low-passage (room "red_bed" false) goal)))
  (is (nil? (headroom/low-passage (room "stone" true) goal))))

(deftest a-walled-in-give-up-names-the-bed-and-the-lintel
  (let [low (headroom/low-passage (room "red_bed" true) goal)
        text (end/give-up-words [11 64 1] (merge (end/give-up-fields {:status :no-path :reason :goal-enclosed}) {:low-passage low}))]
    (is (re-find #"red bed at \[9 64 0\].*headroom.*\[9 66 0\]" text))
    (is (not (str/includes? text "walled in")))))
