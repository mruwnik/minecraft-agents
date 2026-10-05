(ns engine.planner-partial-test
  "engine/js/path/planner-partial.test.mjs against the ClojureScript planner: the partial end of a plan, the node nearest
  the goal among those reached without a step the body cannot undo, and result :oneWay when a nearer node lies behind one."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.planner-tuned :as planner]
            [engine.planner-fixture :as pf :refer [near cells moves last-cell]]))

(def DROP (:drop pf/MOVE))
(def GAP (:gap pf/MOVE))

(defn run
  "plan over a hand-built world (no stone floor) with the goal flood off."
  ([fill from goal] (run fill from goal {}))
  ([fill from goal options]
   (pf/plan (pf/snapshot {:fill fill}) {:from from :goal goal} (assoc options :goalFlood 0))))

(defn stone
  ([x0 z0 x1 z1 feet] (stone x0 z0 x1 z1 feet 59))
  ([x0 z0 x1 z1 feet to] [x0 to z0 x1 (dec feet) z1 "stone"]))
(defn slab [x0 z0 x1 z1 feet] [x0 feet z0 x1 feet z1 "stone_slab" {:type "bottom"}])
(defn one-way-of [{:keys [oneWay]}] (select-keys oneWay [:move :x :y :z]))

(def island
  [(stone -2 -2 5 6 70)
   [6 60 -2 13 65 6 "water"] (stone 6 -2 13 6 60 55)
   (stone 14 -2 17 6 66)
   (stone 26 -2 33 6 66)])
(def plateau {:x 2 :y 70 :z 2})

(deftest island-reachable-only-by-a-drop-into-water
  (let [r (run island plateau (near 28 66 2))]
    (is (= "partial" (:status r)))
    (is (= [5 70 2] (last-cell r)))
    (is (not-any? #(= DROP %) (moves r)))
    (is (= {:move DROP :x 6 :y 65 :z 2} (one-way-of r)))
    (is (< (get-in r [:oneWay :distance]) 12))))

(deftest no-way-to-a-nearer-cell-by-the-drop-oneway-is-null
  (let [r (run [(stone -2 -2 5 6 70) (stone 26 -2 33 6 66)] plateau (near 28 66 2))]
    (is (= "partial" (:status r)))
    (is (nil? (:oneWay r)))))

(def ledge [(stone -2 -2 10 6 70) (stone 11 -2 12 6 69) (stone 13 -2 20 6 66) (stone 26 -2 33 6 66)])

(deftest goal-behind-a-3-drop-ends-on-the-returnable-ledge
  (let [r (run ledge plateau (near 28 66 2))]
    (is (= "partial" (:status r)))
    (is (= [12 69 2] (last-cell r)))
    (is (= [DROP] (filterv #(= DROP %) (moves r))))
    (is (= {:move DROP :x 13 :y 66 :z 2} (one-way-of r)))))

(def two-ways
  [(stone -2 -2 5 6 70)
   (stone -2 7 5 7 69) (stone -2 8 5 8 68) (stone -2 9 5 12 67)
   (stone 6 -2 20 12 67)
   (stone 26 -2 33 6 67)])

(deftest node-reached-by-a-cheap-3-drop-and-a-dearer-flight-is-the-partial-end-by-the-flight
  (let [r (run two-ways plateau (near 28 67 2))]
    (is (= "partial" (:status r)))
    (is (= [20 67 2] (last-cell r)))
    (is (= 1 (get-in r [:path :cost :maxDrop])))
    (is (nil? (:oneWay r)))))

(defn ledge-below [platform ledge-feet] (into (vec platform) [(stone 11 -2 12 6 ledge-feet) (stone 26 -2 33 6 70)]))
(def flat [(stone -2 -2 10 6 70)])

(deftest a-drop-of-exactly-1-is-undone-by-a-jump-up
  (let [r (run (ledge-below flat 69) plateau (near 28 70 2))]
    (is (= "partial" (:status r)))
    (is (= [12 69 2] (last-cell r)))
    (is (nil? (:oneWay r)))))

(deftest a-drop-of-1-5-off-a-slab-is-not-undone
  (let [r (run (ledge-below (conj flat (slab -2 -2 10 6 70)) 69) plateau (near 28 70 2))]
    (is (= "partial" (:status r)))
    (is (= [10 70 2] (last-cell r)))
    (is (= {:move DROP :x 11 :y 69 :z 2} (one-way-of r)))))

(defn pit [landing] [(stone -2 -2 5 6 70) (stone 8 -2 14 6 landing) (stone 26 -2 33 6 70)])

(deftest a-gap-jump-down-is-one-way-a-level-gap-jump-is-not
  (let [down (run (pit 69) plateau (near 28 70 2))
        level (run (pit 70) plateau (near 28 70 2))]
    (is (= [5 70 2] (last-cell down)))
    (is (= {:move GAP :x 9 :y 69 :z 2} (one-way-of down)))
    (is (= [14 70 2] (last-cell level)))
    (is (nil? (:oneWay level)))))

(deftest complete-plans-are-unchanged-a-goal-past-a-3-drop-is-found
  (let [r (run [(stone -2 -2 10 6 70) (stone 11 -2 20 6 67)] plateau (near 18 67 2))]
    (is (= "found" (:status r)))
    (is (some #(= DROP %) (moves r)))
    (is (nil? (:oneWay r)))))

(deftest no-returnable-node-2-blocks-nearer-is-none-with-the-one-way-step-reported
  (let [r (run [(stone 0 -2 3 6 70) [4 60 -2 13 65 6 "water"] (stone 4 -2 13 6 60 55) (stone 14 -2 17 6 66) (stone 26 -2 33 6 66)]
               plateau (near 28 66 2))]
    (is (= "none" (:status r)))
    (is (nil? (:path r)))
    (is (= {:move DROP :x 4 :y 65 :z 2} (one-way-of r)))))

;; a plateau (feet 70) with a 3-drop to a lower floor (feet 67) that runs to the edge of the loaded chunks (x 47; x 48 on is
;; unloaded), the goal far east in unloaded land: the land below leads on into land not yet loaded
(def cliff [(stone -2 -2 10 6 70) (stone 11 -2 47 6 67)])

(deftest a-one-way-step-to-land-that-runs-into-unloaded-land-is-open-with-its-path
  (let [r (run cliff plateau (near 100 67 2))
        beyond (:oneWay r)]
    (is (= [10 70 2] (last-cell r)) "the plan itself still ends before the drop")
    (is (= {:move DROP :x 11 :y 67 :z 2} (one-way-of r)))
    (is (true? (:open beyond)))
    (is (= 47 (first (peek (mapv (juxt :x :y :z) (get-in beyond [:path :steps]))))) "the path beyond ends at the loaded edge")
    (is (some #(= DROP %) (mapv :move (get-in beyond [:path :steps]))))))

;; the same plateau with a closed pit (floor feet 67, x 11..14) in front of a wall too high to climb (feet 71): every cell
;; round it is loaded, so the pit leads nowhere
(def pit-in-front [(stone -2 -2 10 6 70) (stone 11 -2 14 6 67) (stone 15 -2 16 6 71)])

(deftest a-one-way-step-into-a-closed-pit-is-not-open
  (let [r (run pit-in-front plateau (near 100 70 2))]
    (is (= {:move DROP :x 11 :y 67 :z 2} (one-way-of r)))
    (is (false? (get-in r [:oneWay :open])))))

(defn gap-of [cells][(stone -2 -2 5 6 70) (stone (+ 6 cells) -2 20 6 70)])

(deftest a-gap-wider-than-any-jump-is-exhausted
  (are [cells status reason end]
       (let [r (run (gap-of cells) plateau (near 12 70 2))]
         (is (= [status reason (last-cell r)] [(:status r) (:reason r) end]))
         (is (nil? (:oneWay r))))
    3 "found" nil [12 70 2]
    4 "partial" "exhausted" [5 70 2]))

;; ---- where an unfinished search has got to (create-plan's progress) ----

(defn progress-after
  "create-plan over fill from `from` to goal, stepped n expansions once: its progress as cljs data (nil when none)."
  [fill from goal n]
  (let [^js p (planner/create-plan (pf/snapshot {:fill fill}) (clj->js {:from from :goal goal}) (pf/options-js {}))]
    (.step p n)
    (js->clj (.progress p) :keywordize-keys true)))

;; a long field (feet 64) far from a goal at x 80; on the second, the field lies 3 below a ledge (feet 67) the start stands on
(def field [(stone -2 -20 80 20 64)])
(def ledge-field [(stone -2 -20 80 20 64) (stone -2 -2 3 2 67)])

(deftest an-unfinished-search-says-where-it-has-got-to
  (let [{:keys [distance startDistance] :as pr} (progress-after field {:x 0 :y 64 :z 0} (near 80 64 0) 30)]
    (is (< distance (- startDistance 10)) "well on its way")
    (is (= [0 64 0] (first (cells pr))))
    (is (= distance (let [[x _ z] (last (cells pr))] (- 80 x))) "its distance is its end's")))

(deftest the-progress-of-a-search-stops-before-a-step-the-body-cannot-undo
  (let [pr (progress-after ledge-field {:x 0 :y 67 :z 0} (near 80 64 0) 60)]
    (is (some? pr))
    (is (every? #(= 67 (second %)) (cells pr)) "the drop off the ledge is not taken")
    (is (not-any? #{DROP} (moves pr)))))

(deftest a-search-that-is-over-has-no-progress
  (let [^js p (planner/create-plan (pf/snapshot {:fill field}) (clj->js {:from {:x 0 :y 64 :z 0} :goal (near 10 64 0)})
                                   (pf/options-js {}))]
    (loop [] (when-not (.step p 1000) (recur)))
    (is (nil? (.progress p)))))

;; a start at the east edge of a ledge (feet 70, x 0..1, z 0..63) whose first move is a 3-drop to a floor (feet 67, x 2..47)
;; that runs to the loaded edge (x 48 on is unloaded), the goal far east in unloaded land (live: a gap jump down as go-to's
;; first move kept every unfinished search from walking)
(def first-move-cliff [(stone 0 0 1 63 70) (stone 2 0 47 63 67)])

(deftest the-progress-past-a-first-one-way-step-to-the-loaded-edge-is-open-with-its-path
  (let [pr (progress-after first-move-cliff {:x 1 :y 70 :z 32} (near 120 67 32) 600)
        beyond (:oneWay pr)]
    (is (some? pr))
    (is (nil? (:path pr)) "nothing before the drop is nearer")
    (is (= (:startDistance pr) (:distance pr)))
    (is (= {:move DROP :x 2 :y 67 :z 32} (select-keys beyond [:move :x :y :z])))
    (is (true? (:open beyond)))
    (is (<= 46 (first (last-cell beyond))) "the path beyond ends at the loaded edge")
    (is (< (:distance beyond) (- (:startDistance pr) 40)))))

;; the same ledge with only a closed pit below it (feet 67, x 2..13, z 30..34) and a wall too high to climb behind it
;; (feet 72, x 14..17): every cell round the pit is loaded, so it leads nowhere
(def first-move-pit [(stone 0 0 1 63 70) (stone 2 30 13 34 67) (stone 14 0 17 63 72)])

(deftest the-progress-past-a-one-way-step-into-a-closed-pit-is-not-open
  (let [pr (progress-after first-move-pit {:x 1 :y 70 :z 32} (near 120 67 32) 60)]
    (is (= DROP (get-in pr [:oneWay :move])))
    (is (false? (get-in pr [:oneWay :open])))))
