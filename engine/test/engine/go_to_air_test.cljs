(ns engine.go-to-air-test
  "go-to from under water to a goal with its head in air: the planner counts only air the body has seen (airSeen), so an
  unseen goal head would refuse the way for :air."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt]
            [engine.test-util :as tu :refer [box]]))

;; go-to-air-budget drowning-body-swims-to-seen-air: a 2 high water tunnel (feet 64, head 65) under a stone roof, an air
;; pocket in the roof (x 11..13, y 66) 6 cells east of the body; oxygen 4 (12.4 s of air used). Looking east the body sees
;; a cell of the pocket edge-on through the water; looking west it sees none, and go-to looks at the goal's head first
(def water-tunnel
  (merge (box -4 60 -4 30 63 20 "stone") (box 3 64 7 22 68 9 "stone") (box 4 64 8 20 65 8 "water")))

(def tunnel (merge water-tunnel (box 11 66 8 13 66 8 "air")))

;; the tunnel bent north at x 20, the pocket (x 20, z 17..19) round the corner: out of sight from the start at any jitter, since
;; a face point seen is enough now (the first look at jitter 0 sees the straight pocket's edge)
(def bent-tunnel
  (merge water-tunnel (box 19 64 10 22 68 21 "stone") (box 20 64 9 20 65 19 "water") (box 20 66 17 20 66 19 "air")))

(def bent-goal [20 65 18])

;; Sight jitters each pass's rays within their grid cells; seen edge-on, the pocket is hit by some jitters only (0.25 to
;; 0.75 at the first look), so the cases run over jitters the first look misses it at too: the body looks again.
(def jitters [0 0.1 0.5 0.9 0.999])

(defn ^:async go-east!
  ([yaw jitter] (go-east! yaw jitter {}))
  ([yaw jitter args]
   (let [{:keys [out]} (await (gt/go! {:self {:pos [5.5 64 8.5] :inWater true :oxygen 4 :yaw yaw} :blocks (:blocks args tunnel) :light [15 15]
                                       :jitter jitter}
                                      (merge {:pos [12 65 8] :range 1 :ways #{:walk :clear-seal}} (dissoc args :blocks))))]
     @out)))

(deftest a-drowning-body-swims-to-a-pocket-it-has-seen-a-cell-of
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [jitter jitters]
          (let [out (await (go-east! (/ js/Math.PI -2) jitter))]
            (is (true? (:arrived out)) (str "jitter " jitter ": result " out))))))))

(deftest with-no-looks-again-a-pocket-its-first-look-missed-is-given-up-for-air
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (await (go-east! (/ js/Math.PI -2) 0 {:air-looks 0 :blocks bent-tunnel :pos bent-goal}))]
          (is (= :air (:why out)) (str "result " out)))))))

(deftest a-body-too-low-on-air-gives-up_without_looking_again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (gt/go! {:self {:pos [5.5 64 8.5] :inWater true :oxygen 2 :yaw (/ js/Math.PI -2)} :blocks bent-tunnel :light [15 15]
                                            :jitter 0}
                                           {:pos bent-goal :range 1 :ways #{:walk :clear-seal}}))
              out @out]
          (is (= :air (:why out)) (str "result " out)))))))

(deftest a-drowning-body-facing-away-looks-at-the-goal-and-swims-to-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [jitter jitters]
          (let [out (await (go-east! (/ js/Math.PI 2) jitter))]
            (is (true? (:arrived out)) (str "jitter " jitter ": result " out))))))))

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
                                               {:pos goal :range 1 :ways #{:walk :clear-seal}}))]
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
                                             {:pos [38 70 8] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= :air (:why @out)) (str "result " @out))
          (is (<= 69 (second (gt/at p))) (str "it swam up for air: at " (gt/at p))))))))

;; the same shaft in the dark: looking teaches nothing, so it swims up its own column blind, as breathe does
(deftest a-body-short-of-air-in-a-dark-shaft-swims-up-blind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [oxygen [6 5]]
          (let [{:keys [out p]} (await (gt/go! {:self {:pos [4.5 64 8.5] :inWater true :oxygen oxygen :pitch 0} :blocks shaft :light [0 0]}
                                               {:pos [8 70 8] :range 1 :ways #{:walk :clear-seal}}))]
            (is (true? (:arrived @out)) (str "at oxygen " oxygen ": result " @out " at " (gt/at p)))))))))
