(ns engine.go-to-column-test
  "jobs.movement.go-to's column goals ([x z], y left out) and its re-aim of a y with nothing standable in range."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt :refer [go! at events-of refusal]]
            [engine.core :as core]
            [engine.test-util :as tu :refer [box floor]]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.column :as column]))

(def flat (floor -2 -3 40 3))

(defn near-x10? [p] (< (js/Math.abs (- 10 (first (at p)))) 2))

(deftest a-y-off-by-three-arrives-on-the-ground-of-that-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; y 61 is a cell inside the ground under x 10 (stone 60..62 under the floor), as a block's position is
        (doseq [y [67 61]]
          (let [{:keys [out p] :as s} (await (go! {:blocks (merge flat (box 9 60 -1 11 62 1 "stone"))} {:pos [10 y 0]}))]
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

(deftest the-re-aim-comes-before-any-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng]} (await (go! {:blocks flat} {:pos [10 67 0]}))]
          (is (= {:arrived true :resolved {:x 10 :y 64 :z 0}} @out))
          (is (= [{:x 10 :y 64 :z 0}] (mapv :target (gt/moved eng))) "one walk, to the re-aimed cell: no walk toward y 67"))))))

;; a sealed stone pocket under a pit: x 4..6, y 56..59, z -1..1, its one open cell (5,58,0); the pit over it (no floor
;; at y 63 in those columns) stands on its top at feet 60
(def pocket
  (merge (apply dissoc flat (for [x [4 5 6] z [-1 0 1]] (str x ",63," z)))
         (box 4 56 -1 6 59 1 "stone") (box 5 58 0 5 58 0 "air")))

(deftest a-goal-in-a-sealed-pocket-gives-up-and-never-arrives-over-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go! {:blocks pocket} {:pos [5 58 0] :escalate false}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])) (pr-str @out)))))))

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

(defn hiding
  "p (tu/fake over blocks) that sees every cell but those hidden? [x y z] answers true for (unknown, never seen)."
  [blocks hidden?]
  (let [p (tu/fake {:blocks blocks})]
    (aset p "sensedAt" (fn [pos] (if (hidden? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    p))

;; a hill at x 28..32 with its ground at feet 75, a cave in it at feet 60 (nearer feet 64 than the ground is)
(def hill-with-cave
  (merge flat (box 28 40 -3 32 74 3 "stone") (box 30 60 0 30 61 0 "air")))

(defn column-hidden-below [y] (fn [[x cy z]] (and (= [30 0] [x z]) (< cy y))))

(deftest an-unseen-column-aims-at-its-ground-open-to-the-sky-never-a-hidden-cave
  (let [p (hiding hill-with-cave (column-hidden-below 75))]
    (is (= {:pos {:x 30 :y 75 :z 0} :resolved true :provisional true} (column/aim p {:x 30 :z 0} 4 64 true))
        "the top open to the sky, as a player sees it from above")
    (is (= {:pos {:x 30 :y 64 :z 0}} (column/aim p {:x 30 :z 0} 4 64 false))
        ":unknown on reads no unseen cell: the stand-in")))

;; a lake at x 28..32: water y 49..63 (15 deep) on a stone bed at 48
(def lake (merge flat (box 28 48 -3 32 48 3 "stone") (box 28 49 -3 32 63 3 "water")))

(deftest an-unseen-lake-column-aims-at-its-surface-not-its-bed
  (let [p (hiding lake (column-hidden-below 80))]
    (is (= {:pos {:x 30 :y 63 :z 0} :resolved true :provisional true} (column/aim p {:x 30 :z 0} 4 64 true))
        "the top water cell: water is no open sky")))

(deftest a-seen-column-picks-its-seen-ground-nearest-the-feet
  (let [p (hiding hill-with-cave (constantly false))]
    (is (= {:pos {:x 30 :y 60 :z 0} :resolved true} (column/aim p {:x 30 :z 0} 4 64 true))
        "the cave was seen: it is the ground nearest the feet")))

(deftest a-column-far-above-or-below-the-feet-resolves
  (let [p (hiding flat (constantly false))]
    (doseq [feet [-50 200]]
      (is (= {:pos {:x 10 :y 64 :z 0} :resolved true} (column/aim p {:x 10 :z 0} 4 feet true)) (str "feet " feet)))))

(deftest a-column-goal-is-aimed-again-when-it-comes-into-view
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a lone block floats at y 70 over x 30: out of sight, the column aims at its top (71, out of reach); once
        ;; seen, at its ground under it (64), nearer the feet
        (let [floating (merge flat (box 30 70 0 30 70 0 "stone"))
              {:keys [out p] :as s} (await (go! {:blocks floating :light [15 0]} {:pos [30 0] :range 0}))]
          (is (= {:arrived true :resolved {:x 30 :y 64 :z 0}} @out) (pr-str @out))
          (is (= [30 64 0] (mapv #(js/Math.floor %) (at p))))
          (is (= [{:to {:x 30 :y 71 :z 0} :provisional true} {:to {:x 30 :y 64 :z 0}}]
                 (mapv #(select-keys % [:to :provisional]) (events-of s :goal-resolved)))
              "said again when the target changed; the unseen top is said as a guess"))))))

(deftest a-column-stand-in-arrival-carries-the-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the column x 3 has no floor at all: no ground, the stand-in (the feet y) is within range from the start
        (let [holed (apply dissoc flat (for [z (range -3 4)] (str "3,63," z)))
              {:keys [out]} (await (go! {:blocks holed} {:pos [3 0] :range 4}))]
          (is (= {:arrived true :resolved {:x 3 :y 64 :z 0} :stand-in true} @out) (pr-str @out)))))))

(deftest a-goal-sealed-under-a-ceiling-is-not-re-aimed-through-it
  (let [p (hiding pocket (constantly false))]
    (is (= {:pos {:x 5 :y 58 :z 0}} (column/aim p {:x 5 :y 58 :z 0} 4 64 true))
        "the ground over the pocket's roof is no way to the cell under it")
    (is (= {:pos {:x 5 :y 60 :z 0} :resolved true} (column/aim p {:x 5 :y 59 :z 0} 4 64 true))
        "a goal in the roof block itself is a position in the ground: its top")
    (is (= {:pos {:x 10 :y 64 :z 0} :resolved true} (column/aim p {:x 10 :y 66 :z 0} 4 64 true))
        "a goal in the open air over the ground comes down to it")))

(deftest an-unseen-goal-inside-a-mound-under-unknown-ends-from-near-it
  ;; no endless "searching" loop: the call ends, in a bounded number of plans
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the body starts at x 22, 6 short of the mound at x 28..32 (ground at feet 65), the goal cell (30,64,0) inside it
        (let [mound (merge flat (box 28 64 -3 32 64 3 "stone"))]
          (doseq [pos [[30 0] [30 64 0]]]
            (let [{:keys [out] :as s} (await (go! {:blocks mound :light [15 0] :self {:pos {:x 22 :y 64 :z 0}}} {:pos pos :range 0 :unknown true}))]
              (is (not= :not-done @out) (str (pr-str pos) " ended"))
              (is (< (count (events-of s :planned)) 300) (str (pr-str pos) " searches: " (count (events-of s :planned)))))))))))

(deftest an-unseen-y-goal-inside-a-mound-under-unknown-is-re-aimed-once-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the goal cell (30,64,0) is stone inside the mound (ground at feet 65), unseen from x 22, in full sky light
        (let [mound (merge flat (box 28 64 -3 32 64 3 "stone"))
              {:keys [out p] :as s} (await (go! {:blocks mound :light [15 0] :self {:pos {:x 22 :y 64 :z 0}}}
                                                {:pos [30 64 0] :range 0 :unknown true}))]
          (is (= {:arrived true :resolved {:x 30 :y 65 :z 0}} @out) (pr-str @out))
          (is (= [30 65 0] (mapv #(js/Math.floor %) (at p))))
          (is (= [{:x 30 :y 65 :z 0}] (mapv :to (events-of s :goal-resolved)))))))))

(defn ^:async go-hidden!
  "go! with the cells hidden? answers true for never seen (hiding), whatever the body looks at."
  [world hidden? args]
  (let [{:keys [eng p] :as s} (gt/setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out args)))]
    (aset p "sensedAt" (fn [pos] (if (hidden? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    (core/submit! eng '(recording-parent) {})
    (await (gt/tick-out! eng 30))
    (assoc s :eng eng :out out)))

(deftest a-column-guess-that-never-resolves-walks-near-looks-then-walks-in-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the column x 30 is never seen: its guess (the raw top, 64) is walked to within approach-range, looked at
        ;; max-column-looks times, then walked to within the :range arg; the result says it was a guess
        (let [{:keys [out p eng]} (await (go-hidden! {:blocks flat} (fn [[x _ z]] (= [30 0] [x z])) {:pos [30 0] :range 0}))
              walks (gt/moved eng)]
          (is (= {:arrived true :resolved {:x 30 :y 64 :z 0} :provisional true} @out) (pr-str @out))
          (is (= [30 64 0] (mapv #(js/Math.floor %) (at p))))
          (is (= go-to/max-column-looks (count (filter #(= "look" (.-name %)) (.-calls (.-world p))))) "its looks spent")
          (is (some #(< 2 (js/Math.hypot (- 30 (:x (:to %))) (:z (:to %))) (inc column/approach-range)) walks)
              (str "a walk stopped near the guess first: " (pr-str (mapv :to walks)))))))))
