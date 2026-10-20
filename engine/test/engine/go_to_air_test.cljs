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
(def tunnel-seen [[3 64 7 22 65 9] [3 66 7 10 66 9] [14 66 7 22 66 9]])

(def jitters [0 0.1 0.5 0.9 0.999])

(defn ^:async go-east!
  ([yaw jitter] (go-east! yaw jitter {}))
  ([yaw jitter args]
   (let [{:keys [out]} (await (gt/go! {:self {:pos [5.5 64 8.5] :inWater true :oxygen 4 :yaw yaw} :blocks (:blocks args tunnel) :light [15 15]
                                       :seen tunnel-seen :jitter jitter}
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
        (let [out (await (go-east! (/ js/Math.PI -2) 0 {:settings {:jobs.movement.go-to.tuning/air-looks 0} :blocks bent-tunnel :pos bent-goal}))]
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

;; should fail: the only way on is a 64 cell sealed tunnel (16 s of sprint-swimming) to a shaft at x 69, the land cut by a bedrock ridge (x 11):
;; it swims up for air, and with the air back no way fits it still
(def long-tunnel
  (merge (box -4 60 -4 77 69 63 "stone") (box 4 64 8 4 69 8 "water")
         (box 5 64 8 68 65 8 "water") (box 69 64 8 69 69 8 "water") (box 11 70 -4 11 73 63 "bedrock")))

(deftest a-body-short-of-air-swims-up-and-gives-up-air-when-no-way-fits-a-breath
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [4.5 64 8.5] :inWater true :oxygen 6 :pitch 0} :blocks long-tunnel :light [15 15]}
                                             {:pos [70 70 8] :range 1 :unknown false :ways #{:walk :clear-seal}}))]
          (is (= :air (:why @out)) (str "result " @out))
          (is (<= 69 (second (gt/at p))) (str "it swam up for air: at " (gt/at p))))))))

;; the same shaft in the dark: looking teaches nothing, so it swims up its own column blind, as breathe does
(deftest a-body-short-of-air-in-a-dark-shaft-swims-up-blind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [oxygen [16 6 5]]
          (let [{:keys [out p]} (await (gt/go! {:self {:pos [4.5 64 8.5] :inWater true :oxygen oxygen :pitch 0} :blocks shaft :light [0 0]}
                                               {:pos [8 70 8] :range 1 :ways #{:walk :clear-seal}}))]
            (is (true? (:arrived @out)) (str "at oxygen " oxygen ": result " @out " at " (gt/at p)))))))))

;; go-to-air-budget surface-stretch-arrives: two 20 cell dives (10 s each) joined by 14 cells with air over them. The body
;; starts at the bottom of the west shaft with half its air spent: no way fits, so it swims up and must wait for its air
;; before it plans the dives again (it has used 7.5 s, a dive of 10 s is over the 12 s the planner allows).
(def surface-stretch
  (merge (box 3 60 7 61 67 9 "stone") (box 4 64 8 59 65 8 "water")
         (box 4 66 8 4 67 8 "air") (box 59 66 8 59 67 8 "air") (box 25 66 8 38 66 8 "air")))

(deftest a-body-short-of-air-waits-for-its-breath-at-the-surface-before-it-plans-the-dives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [4.5 64 8.5] :inWater true :oxygen 10 :pitch 0} :blocks surface-stretch :light [15 15]}
                                             {:pos [59 65 8] :range 1 :unknown false :ways #{:walk :clear-seal}}))]
          (is (true? (:arrived @out)) (str "result " @out " at " (gt/at p))))))))

;; open water 4 deep, a land floor at feet 64 round it, the body on its bottom at night in the dark: its safe? would
;; wait for dawn (exposed), and the wait under water drowns it. go-to must swim up and walk first.
(def night-pool
  (merge gt/flat (box 3 59 -2 8 59 2 "stone") (box 3 60 -2 8 63 2 "water")))

(deftest a-submerged-body-at-night-swims-up-and-walks-instead-of-waiting-for-dawn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gt/go! {:self {:pos [5.5 60 0.5] :inWater true :oxygen 20} :blocks night-pool :light [0 0] :time 18000}
                                             {:pos [14 64 0] :range 1}))]
          (is (true? (:arrived @out)) (pr-str @out)))))))
