(ns engine.go-to-column-test
  "jobs.movement.go-to's column goals ([x z], y left out) and its re-aim of a y with nothing standable in range."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as gt :refer [go! at events-of refusal]]
            [engine.go-to-support :refer [hiding]]
            [engine.core :as core]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box floor]]
            [jobs.lib.near :as near]
            [jobs.lib.sense :as sense]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.torch :as gtorch]
            [jobs.movement.go-to.aim :as gaim]
            [jobs.movement.go-to.walk :as gwalk]
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
            (is (= {:arrived true :resolved {:x 10 :y 64 :z 0} :why :explicit} @out) (str "y " y " " (pr-str @out)))
            (is (near-x10? p) (str "y " y " at " (at p)))
            (is (= [{:goal {:x 10 :y y :z 0} :to {:x 10 :y 64 :z 0} :why :explicit}]
                   (mapv #(select-keys % [:goal :to :why]) (events-of s :goal-resolved)))
                (str "y " y ": the re-aim is said once"))))))))

(deftest a-column-goal-arrives-on-the-ground-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a step up at x 8..12: the ground there stands at feet 65
        (let [hill (merge flat (box 8 64 -3 12 64 3 "stone"))]
          (doseq [pos [[10 0] {:x 10 :z 0}]]
            (let [{:keys [out p]} (await (go! {:blocks hill} {:pos pos :range 0}))]
              (is (= {:arrived true :resolved {:x 10 :y 65 :z 0} :why :surface} @out) (str (pr-str pos) " " (pr-str @out)))
              (is (= [10 65 0] (mapv #(js/Math.floor %) (at p))) (pr-str pos)))))))))

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
        (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [10 67 0] :settings {:jobs.movement.go-to.tuning/y-slack 0}}))]
          (is (= {:arrived false :reason :unreachable :why :goal-not-standable} (select-keys @out [:arrived :reason :why])))
          (is (= [0 64 0] (at p))))))))

(deftest the-re-aim-comes-before-any-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng]} (await (go! {:blocks flat} {:pos [10 67 0]}))]
          (is (= {:arrived true :resolved {:x 10 :y 64 :z 0} :why :explicit} @out))
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
        (let [{:keys [out]} (await (go! {:blocks pocket} {:pos [5 58 0] :ways #{:walk :clear-seal}}))]
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
          (is (= {:arrived true :resolved {:x 30 :y 64 :z 0} :why :surface} @out) (pr-str @out))
          (is (= [30 64 0] (mapv #(js/Math.floor %) (at p)))))))))

(deftest an-unseen-column-under-unknown-is-walked-toward-and-resolved-once-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the mound at x 28..32 (ground at feet 65) is out of sight from the start, so the column is first estimated at
        ;; the body's feet y (64, inside the mound) and is aimed again once the body sees it
        (let [mound (merge flat (box 28 64 -3 32 64 3 "stone"))
              {:keys [out p] :as s} (await (go! {:blocks mound :light [15 0]} {:pos [30 0] :range 0 :unknown true}))]
          (is (= {:arrived true :resolved {:x 30 :y 65 :z 0} :why :surface} @out) (pr-str @out))
          (is (= [30 65 0] (mapv #(js/Math.floor %) (at p))))
          (is (= [{:to {:x 30 :y 64 :z 0} :provisional true} {:to {:x 30 :y 65 :z 0}}]
                 (mapv #(select-keys % [:to :provisional]) ((juxt first last) (events-of s :goal-resolved))))
              "first the estimate, last resolved on approach"))))))

(deftest an-unseen-block-goal-inside-a-mound-at-night-is-walked-to-not-waited-for
  ;; its light cannot be seen from the start (the cell is inside the mound), so safe? must not hold the walk for a look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [mound (merge flat (box 28 64 -3 32 64 3 "stone"))
              {:keys [out p]} (await (go! {:blocks mound :light [15 0] :time 18000} {:pos [30 64 0] :range 0 :unknown true}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [30 65 0] (mapv #(js/Math.floor %) (at p)))))))))

(deftest a-column-pos-is-accepted-at-submit-and-a-bad-one-refused
  (is (nil? (refusal {:pos [10 0]})))
  (is (nil? (refusal {:pos {:x 10 :z 0}})))
  (is (nil? (refusal {:pos [1 64 -3]})))
  (is (nil? (refusal {:pos {:x 1 :y 64 :z -3}})))
  (is (re-find #":pos must be \[x y z\]" (refusal {:pos [10]})))
  (is (re-find #":pos must be \[x y z\]" (refusal {:pos {:x 1 :y 2}}))))

;; a hill at x 28..32 with its ground at feet 75, a cave in it at feet 60 (nearer feet 64 than the ground is)
(def hill-with-cave
  (merge flat (box 28 40 -3 32 74 3 "stone") (box 30 60 0 30 61 0 "air")))

(defn column-hidden-below [y] (fn [[x cy z]] (and (= [30 0] [x z]) (< cy y))))

(deftest an-unseen-column-aims-at-its-neighbours-ground-never-a-hidden-cave
  (let [p (hiding hill-with-cave (column-hidden-below 75))]
    (is (= {:pos {:x 30 :y 75 :z 0} :resolved true :provisional true} (column/aim p {:x 30 :z 0} 4 64 true))
        "the ground of the seen hill beside it, as a player guesses it")
    (is (= {:pos {:x 30 :y 75 :z 0} :resolved true :provisional true} (column/aim p {:x 30 :z 0} 4 64 false))
        "the same estimate under :unknown: it reads no unseen cell either way")))

;; a lake at x 28..32: water y 49..63 (15 deep) on a stone bed at 48
(def lake (merge flat (box 28 48 -3 32 48 3 "stone") (box 28 49 -3 32 63 3 "water")))

(deftest an-unseen-lake-column-aims-at-its-neighbours-surface-not-its-bed
  (let [p (hiding lake (column-hidden-below 80))]
    (is (= {:pos {:x 30 :y 63 :z 0} :resolved true :provisional true} (column/aim p {:x 30 :z 0} 4 64 true))
        "the top water cell of the lake beside it")))

(deftest a-seen-column-picks-its-surface-not-a-nearer-cave
  (let [p (hiding hill-with-cave (constantly false))]
    (is (= {:pos {:x 30 :y 75 :z 0} :resolved true :why :surface} (column/aim p {:x 30 :z 0} 4 64 true))
        "the cave floor is nearer the feet; the surface is the open-sky stand")
    (is (= {:pos {:x 30 :y 75 :z 0} :resolved true :why :surface} (column/aim p {:x 30 :z 0} 4 40 true))
        "from deep below too")))

(deftest a-column-seen-only-under-cover-picks-the-stand-nearest-the-feet
  (let [covered (hiding hill-with-cave #(> (second %) 70))]
    (is (= {:pos {:x 30 :y 60 :z 0} :resolved true :why :nearest-level} (column/aim covered {:x 30 :z 0} 4 62 true))
        "the cave under a seen ceiling, its surface never seen")))

(deftest a-tree-over-a-column-is-not-its-surface
  (let [tree (merge flat (box 10 65 0 10 68 0 "oak_log") (box 9 69 -1 11 70 1 "oak_leaves"))
        p (hiding tree (constantly false))]
    (is (= {:pos {:x 10 :y 64 :z 0} :resolved true :why :surface} (column/aim p {:x 10 :z 0} 4 64 true)))))

(deftest a-y-re-aim-says-it-is-explicit
  (let [p (hiding flat (constantly false))]
    (is (= {:pos {:x 10 :y 64 :z 0} :resolved true :why :explicit} (column/aim p {:x 10 :y 67 :z 0} 4 64 true)))))

(deftest a-column-far-above-or-below-the-feet-resolves
  (let [p (hiding flat (constantly false))]
    (doseq [feet [-50 200]]
      (is (= {:pos {:x 10 :y 64 :z 0} :resolved true :why :surface} (column/aim p {:x 10 :z 0} 4 feet true)) (str "feet " feet)))))

(deftest a-column-goal-is-aimed-again-when-it-comes-into-view
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a lone block floats at y 70 over x 30: out of sight, the column is estimated at the feet (64, nothing around it
        ;; seen); once seen, its ground under it (64) is said again, no longer a guess
        (let [floating (merge flat (box 30 70 0 30 70 0 "stone"))
              {:keys [out p] :as s} (await (go! {:blocks floating :light [15 0]} {:pos [30 0] :range 0 :unknown false}))]
          (is (= {:arrived true :resolved {:x 30 :y 64 :z 0} :why :surface} @out) (pr-str @out))
          (is (= [30 64 0] (mapv #(js/Math.floor %) (at p))))
          (is (= [{:to {:x 30 :y 64 :z 0} :provisional true} {:to {:x 30 :y 64 :z 0}}]
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
    (is (= {:pos {:x 5 :y 60 :z 0} :resolved true :why :explicit} (column/aim p {:x 5 :y 59 :z 0} 4 64 true))
        "a goal in the roof block itself is a position in the ground: its top")
    (is (= {:pos {:x 10 :y 64 :z 0} :resolved true :why :explicit} (column/aim p {:x 10 :y 66 :z 0} 4 64 true))
        "a goal in the open air over the ground comes down to it")))

(deftest an-unseen-goal-inside-a-mound-under-unknown-arrives-on-it-in-few-plans
  ;; the cell inside the mound is looked at from a seen stand first, not toured round over the unseen land
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the body starts at x 22, 6 short of the mound at x 28..32 (ground at feet 65), the goal cell (30,64,0) inside it
        (let [mound (merge flat (box 28 64 -3 32 64 3 "stone"))]
          (doseq [pos [[30 0] [30 64 0]]]
            (let [{:keys [out p] :as s} (await (go! {:blocks mound :light [15 0] :self {:pos {:x 22 :y 64 :z 0}}} {:pos pos :range 0 :unknown true}))]
              (is (:arrived @out) (str (pr-str pos) " " (pr-str @out)))
              (is (= [30 65 0] (mapv #(js/Math.floor %) (at p))) (pr-str pos))
              (is (< (count (events-of s :planned)) 30) (str (pr-str pos) " plans: " (count (events-of s :planned)))))))))))

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

(deftest a-goal-hidden-in-seen-ground-is-looked-at-from-a-seen-stand-before-planning-to-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the mound is seen, the goal cell inside it never is (a face point of it would show it to a real eye from the
        ;; ground, so the cell is hidden by hand)
        (let [mound (merge flat (box 28 64 -3 32 64 3 "stone"))
              s (await (go-hidden! {:blocks mound :light [15 0] :self {:pos {:x 22 :y 64 :z 0}}} #(= [30 64 0] %) {:pos [30 64 0] :range 0 :unknown true}))
              [lookout] (events-of s :go-to.lookout)]
          (is (= {:x 30 :y 64 :z 0} (:goal lookout)) (pr-str lookout))
          (is (<= (apply js/Math.hypot (map - [30 64 0] ((juxt :x :y :z) (:stand lookout)))) gaim/lookout-reach)
              (pr-str lookout)))))))

(deftest an-unseen-y-goal-inside-a-mound-under-unknown-is-re-aimed-once-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the goal cell (30,64,0) is stone inside the mound (ground at feet 65), unseen from x 22, in full sky light
        (let [mound (merge flat (box 28 64 -3 32 64 3 "stone"))
              {:keys [out p] :as s} (await (go! {:blocks mound :light [15 0] :self {:pos {:x 22 :y 64 :z 0}}}
                                                {:pos [30 64 0] :range 0 :unknown true}))]
          (is (= {:arrived true :resolved {:x 30 :y 65 :z 0} :why :explicit} @out) (pr-str @out))
          (is (= [30 65 0] (mapv #(js/Math.floor %) (at p))))
          (is (= [{:x 30 :y 65 :z 0}] (mapv :to (events-of s :goal-resolved)))))))))

(deftest a-column-guess-that-never-resolves-walks-near-looks-then-walks-in-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the column x 30 is never seen: its guess (the seen ground beside it, 64) is walked to within approach-range, looked at
        ;; max-column-looks times, then walked to within the :range arg; the result says it was a guess
        (let [{:keys [out p eng]} (await (go-hidden! {:blocks flat} (fn [[x _ z]] (= [30 0] [x z])) {:pos [30 0] :range 0 :unknown false}))
              walks (gt/moved eng)]
          (is (= {:arrived true :resolved {:x 30 :y 64 :z 0} :provisional true} @out) (pr-str @out))
          (is (= [30 64 0] (mapv #(js/Math.floor %) (at p))))
          (is (= gaim/max-column-looks (count (filter #(= "look" (.-name %)) (.-calls (.-world p))))) "its looks spent")
          (is (some #(< 2 (js/Math.hypot (- 30 (:x (:to %))) (:z (:to %))) (inc column/approach-range)) walks)
              (str "a walk stopped near the guess first: " (pr-str (mapv :to walks)))))))))

(deftest a-column-seen-open-at-the-feet-with-its-ground-unseen-is-walked-toward-and-arrived-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the body on a plateau (feet 72) with steps down to the floor (feet 64); round the column x 25 nothing below 66
        ;; is seen until the body is down the steps, so no ground is known there or beside it at the start, while its
        ;; cells at the feet y are seen open
        (let [steps (into {} (mapcat (fn [i] (box (+ 5 i) 64 -3 (+ 5 i) (- 70 i) 3 "stone")) (range 7)))
              world {:blocks (merge flat (box -2 64 -3 4 71 3 "stone") steps) :self {:pos {:x 0.5 :y 72 :z 0.5}}}
              {:keys [eng p]} (gt/setup world)
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out {:pos [25 0] :range 1})))]
          (aset p "sensedAt" (fn [pos] (if (and (< (first (at p)) 12) (>= (.-x pos) 17) (< (.-y pos) 66)) #js {:unknown true} (.blockAt p pos))))
          (core/submit! eng '(recording-parent) {})
          (await (gt/tick-out! eng 30))
          (is (true? (:arrived @out)) (pr-str @out))
          (is (> 2 (js/Math.abs (- 25 (first (at p))))) (str "at " (at p))))))))

;; ---------------------------------------------------------------- :range, seen cells only, look order, fetch

(deftest a-seen-block-goal-with-range-is-reached-beside-it-under-unknown
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a crafting table at (10,64,0) on the floor, seen: range 1 has seen stands beside it, so no re-aim onto its top
        (let [table (merge flat (box 10 64 0 10 64 0 "crafting_table"))
              {:keys [out p] :as s} (await (go! {:blocks table} {:pos [10 64 0] :range 1 :unknown true}))]
          (is (= {:arrived true} @out) (pr-str @out))
          (is (= 64 (js/Math.floor (second (at p)))) (str "beside the table, not on it: " (at p)))
          (is (empty? (events-of s :goal-resolved))))))))

(def pillar "A stone pillar at x 30 up to y 80 (its top stands at feet 81) on the flat floor." (merge flat (box 30 64 0 30 80 0 "stone")))

(deftest an-unseen-column-is-estimated-from-its-seen-neighbours-never-its-real-height
  (let [p (hiding pillar (fn [[x _ z]] (= [30 0] [x z])))]
    (doseq [raw? [true false]]
      (is (= {:pos {:x 30 :y 64 :z 0} :resolved true :provisional true} (column/aim p {:x 30 :z 0} 4 64 raw?))
          (str "raw? " raw? ": the ground of the seen columns beside it, not the pillar's unseen top (81)")))))

(deftest an-unseen-column-with-no-seen-neighbour-is-estimated-at-the-feet
  (let [p (hiding pillar (constantly true))]
    (is (= {:pos {:x 30 :y 70 :z 0} :resolved true :provisional true} (column/aim p {:x 30 :z 0} 4 70 true)))))

(deftest an-unseen-column-is-estimated-only-from-ground-within-estimate-reach-of-the-feet
  (let [plateau (box 10 40 -10 50 99 10 "stone")
        p (hiding plateau (fn [[x _ z]] (= [30 0] [x z])))
        ys (atom #{})
        unseen (fn [_ y _] (swap! ys conj y) :unknown)]
    (is (= 100 (column/estimate (column/seen-kind-at p) 30 0 70)) "the plateau's surface, 30 above the feet")
    (is (= 64 (column/estimate (column/seen-kind-at p) 30 0 64)) "36 above the feet: out of reach, the feet")
    (is (= 64 (column/estimate unseen 30 0 64)))
    (is (= [(- 64 column/estimate-reach 1) (+ 64 column/estimate-reach 1)] [(apply min @ys) (apply max @ys)])
        "an unseen neighbourhood is read only within estimate-reach of the feet")))

(defn stub-ctx
  "A go-to ctx over primitives p whose job memory is the atom mem*, events dropped."
  [p mem* args]
  {:primitives p :args args :unknown (:unknown args) :id "col-stub" :root "col-stub" :slots [] :engine {:jobs registry/jobs}
   :view (fn [] {:now 1000000 :data {:entries {(mem/job-kind "col-stub") [{:t 1000000 :data @mem*}]}
                                     :policies {(mem/job-kind "col-stub") mem/job-policy}}})
   :update-mem (fn [f more] (apply swap! mem* f more))
   :remember (fn [_ _ _] nil)
   :emit (fn [_ _ _] nil)
   :hold-still (fn [_] nil)})

(deftest a-y-goals-first-look-is-at-the-goal-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (hiding flat (constantly true))
              c (stub-ctx p (atom {}) {:pos [10 40 0] :unknown true})
              looks (atom [])]
          (with-redefs [sense/look-at! (fn [_ cell] (swap! looks conj cell) (js/Promise.resolve true))]
            (dotimes [_ gaim/max-column-looks] (await (gaim/look-at-column! c {:x 10 :y 40 :z 0}))))
          (is (= [10 40 0] (first @looks)) (pr-str @looks)))))))

(defn mound-from-22
  "A stub ctx (memory mem) of a body at x 22 before the mound x 28..32, every cell seen but those of x >= x0 in it."
  [x0 mem]
  (let [p (tu/fake {:self {:pos [22.5 64 0.5]} :blocks (merge flat (box 28 64 -3 32 64 3 "stone"))})
        hidden? (fn [[x y z]] (and (<= x0 x 32) (<= y 64) (<= -3 z 3)))]
    (aset p "sensedAt" (fn [pos] (if (hidden? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    (stub-ctx p (atom mem) {:pos [30 64 0] :range 0 :unknown true})))

(defn plateau-from
  "A stub ctx of a body at x, before a 5 high stone plateau x 28..32 (y 64..68) whose cell (30 65 0) is the only one unseen."
  [x]
  (let [p (tu/fake {:self {:pos [x 64 0.5]} :blocks (merge flat (box 28 64 -3 32 68 3 "stone"))})]
    (aset p "sensedAt" (fn [pos] (if (= [30 65 0] [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    (stub-ctx p (atom {}) {:pos [30 65 0] :range 0 :unknown true})))

(deftest an-unseen-goal-cell-under-a-seen-roof-near-the-column-is-in-the-ground
  (is (gaim/in-seen-ground? (plateau-from 22.5) {:x 30 :y 65 :z 0}) "the roof y 66..68 seen")
  (is (not (gaim/in-seen-ground? (plateau-from 10.5) {:x 30 :y 65 :z 0})) "too far from the column to take it for ground"))

(deftest an-unseen-goal-behind-a-seen-solid-is-hidden-in-the-ground-one-behind-unseen-cells-is-not
  (is (gaim/hidden-in-ground? (mound-from-22 29 {}) {:x 30 :y 64 :z 0}) "the mound's face x 28 seen")
  (is (not (gaim/hidden-in-ground? (mound-from-22 28 {}) {:x 30 :y 64 :z 0})) "nothing seen in the way"))

(deftest a-lookout-is-a-seen-stand-in-reach-that-sees-the-goal-until-the-lookouts-are-spent
  (let [[x y z :as stand] (gaim/lookout (mound-from-22 29 {}) {:x 30 :y 64 :z 0})]
    (is (<= (js/Math.hypot (- x 30) (- y 64) z) gaim/lookout-reach) (pr-str stand))
    (is (column/stands? (column/seen-kind-at (:primitives (mound-from-22 29 {}))) x y z) (pr-str stand)))
  (is (nil? (gaim/lookout (mound-from-22 29 {:lookouts (vec (repeat gaim/max-lookouts [0 0 0]))}) {:x 30 :y 64 :z 0})))
  (is (nil? (gaim/lookout (mound-from-22 29 {:re-aim true}) {:x 30 :y 64 :z 0})) "re-aimed: the goal is no longer that cell"))

(deftest a-torch-fetch-continue-is-kept-when-the-goal-is-seen-in-the-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the goal (10,63,0) is the floor block, seen; the walk ends :no-torch and the torch fetch waits on its child
        (let [p (tu/fake {:self {:pos [0.5 64 0.5]} :blocks flat})
              c (stub-ctx p (atom {}) {:pos [10 63 0] :range 0 :unknown true :torches :fetch})]
          (with-redefs [near/walk-round! (fn [_ _ _ _] (js/Promise.resolve {:result {:status :no-torch} :status "partial"
                                                                             :to {:x 2 :y 64 :z 0}}))
                        gtorch/fetch-torches! (fn [_] (js/Promise.resolve :continue))]
            (is (= :continue (await (gwalk/walk! c {:x 10 :y 63 :z 0} 0 nil))))))))))

;; ---------------------------------------------------------------- a far unseen column 20 above or below the feet

(defn slope
  "Ground of feet y (+ y0 (* dir (- x 30))) at x 31..49 between flat ground at feet y0 (x -2..30) and at feet y0 + 20 * dir
  (x 50..70), z -3..3, each column solid down to y 63."
  [y0 dir]
  (let [top (fn [x] (+ y0 -1 (* dir (min 20 (max 0 (- x 30))))))]
    (into {} (for [x (range -2 71)] (box x 63 -3 x (top x) 3 "stone")))))

(deftest a-far-unseen-column-up-a-hill-or-down-a-valley-is-walked-to-and-arrived-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the column x 60 is 60 blocks off, out of sight (radius 16) from the start, its ground 20 above or below the
        ;; feet: no ground within approach-range of the feet-y estimate; walking toward it brings it into view
        (doseq [[label y0 dir to] [["hill" 64 1 84] ["valley" 84 -1 64]]]
          (let [{:keys [out p]} (await (go! {:blocks (slope y0 dir) :light [15 0] :ray-deg 3 :sight true :exact-eye true :bodyHitbox true :self {:pos {:x 0 :y y0 :z 0}}}
                                            {:pos [60 0] :range 0 :unknown true}))]
            (is (= {:arrived true :resolved {:x 60 :y to :z 0} :why :surface} @out) (str label " " (pr-str @out)))
            (is (= [60 to 0] (mapv #(js/Math.floor %) (at p))) (str label " at " (at p)))))))))

(deftest a-waypoint-is-the-seen-ground-on-the-line-nearest-the-column
  (let [land (slope 64 1)]
    (is (= {:x 44 :y 78 :z 0} (column/waypoint (column/seen-kind-at (hiding land (fn [[x _ _]] (> x 44)))) 60 0 [0 64 0]))
        "the farthest seen column toward x 60")
    (is (nil? (column/waypoint (column/seen-kind-at (hiding land (fn [[x _ _]] (> x 0)))) 60 0 [0 64 0]))
        "nothing seen ahead: no waypoint, never the unseen ground")))
