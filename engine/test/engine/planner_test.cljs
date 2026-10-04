(ns engine.planner-test
  "engine/js/path/planner.test.mjs against the ClojureScript planner (engine.path.planner-tuned)."
  (:require [cljs.test :refer [deftest is are]]
            [clojure.string :as str]
            [engine.planner-fixture :as pf :refer [world near xz run search create-search cells moves last-cell MOVE]]))

(def start pf/start)

(defn distance-to-goal [r [gx gz]]
  (let [[x _ z] (last-cell r)] (js/Math.hypot (- x gx) (- z gz))))

(defn index-of-start [r]
  (count (take-while (complement (fn [[x _ z]] (and (= x 2) (= z 2)))) (cells r))))

(defn cost [r k] (get-in r [:path :cost k]))
(defn status+reason [r] [(:status r) (:reason r)])

;; ---- basics ----

(deftest flat-walk-of-10-blocks
  (let [r (run (world {}) (near 12 64 2))]
    (is (= "found" (:status r)))
    (is (nil? (:reason r)))
    (is (= [12 64 2] (last-cell r)))
    (is (= 11 (count (cells r))))
    (is (< (js/Math.abs (- (cost r :seconds) (/ 10 4.317))) 0.01))
    (is (= {:seconds 0 :risk 0 :maxDrop 0 :jumps 0 :climbed 0 :opens 0 :unknown 0 :waterSeconds 0 :airMin 15 :waterDrop 0}
           (assoc (get-in r [:path :cost]) :seconds 0)))
    (is (and (> (:expanded r) 0) (>= (:ms r) 0)))))

(deftest flat-open-20-block-query-is-cheap
  (let [r (run (world {}) (near 22 64 2))]
    (is (= "found" (:status r)))
    (is (< (:expanded r) 40) (str "expanded " (:expanded r)))
    (is (= 0 (get-in r [:stats :flooded])))
    (is (= 0 (get-in r [:stats :masks])))))

(deftest near-goal-accepts-any-standable-cell-in-range
  (let [r (run (world {}) (near 12 64 2 3))]
    (is (= "found" (:status r)))
    (is (= 8 (count (cells r))))))

(deftest xz-goal-ignores-height
  (let [r (run (world {}) (xz 12 2 1))]
    (is (= "found" (:status r)))
    (is (= 64 (second (last-cell r))))
    (is (<= (js/Math.abs (- (first (last-cell r)) 12)) 1))))

(deftest diagonal-across-open-floor-uses-diagonal-moves
  (let [r (run (world {}) (near 7 64 7))]
    (is (= "found" (:status r)))
    (is (= (into [(:start MOVE)] (repeat 5 (:diagonal MOVE))) (moves r)))))

;; ---- corners ----

(defn check-diagonal-does-not-cut-corner [blocks]
  (let [r (run (world {:blocks blocks}) (near 3 64 3))]
    (is (not= [3 64 3] (get (cells r) (inc (index-of-start r)))))))

(deftest diagonal-does-not-cut-a-corner
  (are [blocks] (check-diagonal-does-not-cut-corner blocks)
    [[3 64 2 "stone"] [3 65 2 "stone"]]
    [[2 64 3 "stone"] [2 65 3 "stone"]]
    [[3 64 2 "oak_fence"]]))

(defn wall-at [x z] [x 64 z x 65 z "stone"])
(def notch [2 62 3 2 63 3 "air"])

(defn check-corner-slide [fill goal expected-move]
  (let [r (run (world {:fill fill}) goal)]
    (is (= [2 expected-move true]
           [(count (cells r)) (second (moves r)) (get-in r [:path :steps 1 :corner])]))
    (is (str/includes? (get-in r [:path :summary]) "1 corner slide"))))

(deftest corner-slide
  (are [fill goal expected-move] (check-corner-slide fill goal expected-move)
    [(wall-at 3 2) notch] (near 3 64 3) (:corner MOVE)
    [(wall-at 3 2) notch [3 64 3 3 64 3 "stone"]] (near 3 65 3) (:jump MOVE)))

(deftest corner-slide-not-chosen-when-l-route-cheaper
  (let [r (run (world {:fill [(wall-at 3 2)]}) (near 3 64 3))]
    (is (every? (fn [s] (and (not (:corner s)) (not= (:corner MOVE) (:move s)))) (get-in r [:path :steps])))))

(deftest both-sides-blocked-no-diagonal-between-them
  (let [r (run (world {:fill [(wall-at 3 2) (wall-at 2 3)]}) (near 3 64 3))
        at (index-of-start r)]
    (is (not= [3 64 3] (get (cells r) (inc at))))
    (is (every? (complement :corner) (get-in r [:path :steps])))))

(deftest lava-side-is-never-slid-past
  (let [r (run (world {:fill [[3 64 2 3 64 2 "lava"] notch]}) (near 3 64 3))]
    (is (every? (complement :corner) (get-in r [:path :steps])))))

(deftest diagonal-past-passable-sides-is-allowed
  (are [extra] (= [[2 64 2] [3 64 3]] (cells (run (world extra) (near 3 64 3))))
    {:fill [[3 63 2 3 64 2 "water"]]}
    {:fill [[3 60 2 3 63 2 "air"]]}
    {}))

(deftest diagonal-past-blocked-side-is-refused
  (are [blocks] (not= [[2 64 2] [3 64 3]] (cells (run (world {:blocks blocks}) (near 3 64 3))))
    [[3 64 2 "lava"]]
    [[3 64 2 "cobweb"]]
    [[3 64 2 "oak_fence"]]
    [[3 65 2 "stone"]]))

(deftest block-touching-diagonal-only-at-far-corner-does-not-stop-it
  (let [r (run (world {:blocks [[4 64 3 "stone"]]}) (near 3 64 3))]
    (is (= [[2 64 2] [3 64 3]] (cells r)))))

;; ---- jumps, walls ----

(deftest one-block-step-up-is-a-jump
  (let [r (run (world {:fill [[5 64 -2 9 64 40 "stone"]]}) (near 7 65 2))]
    (is (= "found" (:status r)))
    (is (= [7 65 2] (last-cell r)))
    (is (= 1 (count (filter #{(:jump MOVE)} (moves r)))))
    (is (= 1 (cost r :jumps)))))

(deftest jump-needs-headroom-over-the-start-cell
  (let [low (world {:fill [[5 64 -2 9 64 40 "stone"]] :blocks [[4 66 2 "stone"]]})
        r (run low (near 5 65 2) {} {:x 4 :y 64 :z 2})]
    (is (not= (:jump MOVE) (get-in r [:path :steps 1 :move])))))

(deftest two-block-wall-forces-a-detour-round-its-end
  (let [r (run (world {:fill [[5 64 -2 5 65 8 "stone"]]}) (near 8 64 2))]
    (is (= "found" (:status r)))
    (is (every? (fn [[x y z]] (or (not (and (= x 5) (<= z 8))) (> y 65))) (cells r)))
    (is (some (fn [[x _ z]] (and (= x 5) (= z 9))) (cells r)))))

(deftest two-block-wall-across-everything-none-when-no-closer
  (let [sealed (world {:fill [[5 64 -2 5 65 40 "stone"]]})
        r (search sealed (near 8 64 2) {} {:x 4 :y 64 :z 2})]
    (is (= ["none" "exhausted" nil] [(:status r) (:reason r) (:path r)]))))

(deftest exhausted-still-returns-partial-path-when-2-blocks-closer
  (let [sealed (world {:fill [[5 64 -2 5 65 40 "stone"]]})
        r (search sealed (near 8 64 2))]
    (is (= ["partial" "exhausted"] (status+reason r)))
    (is (= [4 64 2] (last-cell r)))))

(deftest slab-staircase-climbs-2-blocks-without-jumping
  (let [stairs (world {:fill [[5 64 -2 5 64 40 "stone"] [7 64 -2 40 65 40 "stone"]]
                       :blocks (concat (for [i (range 43)] [4 64 (- i 2) "oak_slab" {:type "bottom"}])
                                       (for [i (range 43)] [6 65 (- i 2) "oak_slab" {:type "bottom"}]))})
        r (run stairs (near 9 66 2))]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :jumps)))
    (is (= [9 66 2] (last-cell r)))
    (is (= [0 0 8 0 8 0] (vec (take 6 (map :h (get-in r [:path :steps]))))))))

(deftest top-slab-ceiling-blocks-the-walk-a-2-high-tunnel-does-not
  (let [blocked (search (world {:fill [[5 65 -2 7 65 40 "oak_slab" {:type "top"}]]}) (near 9 64 2))]
    (is (= ["partial" "exhausted"] (status+reason blocked)))
    (is (= [4 64 2] (last-cell blocked)))
    (is (= "found" (:status (run (world {:fill [[5 66 -2 7 66 40 "stone"]]}) (near 9 64 2)))))))

(deftest fence-line-is-not-stepped-over
  (let [sealed (search (world {:fill [[5 64 -3 5 64 41 "oak_fence"]]}) (near 8 64 2))
        open (run (world {:fill [[5 64 -3 5 64 8 "oak_fence"]]}) (near 8 64 2))]
    (is (= ["partial" "exhausted"] (status+reason sealed)))
    (is (= "found" (:status open)))
    (is (every? (fn [s] (>= (:z s) 8)) (filter (fn [s] (= 5 (:x s))) (get-in open [:path :steps]))))))

(deftest one-block-wall-is-jumped
  (let [r (run (world {:fill [[5 64 -2 5 64 40 "stone"]]}) (near 8 64 2))]
    (is (= "found" (:status r)))
    (is (= 1 (cost r :jumps)))))

;; ---- drops and gaps ----

(defn platform [d] (world {:fill [[0 64 0 4 (+ 63 d) 4 "stone"]]}))
(defn on-platform [d] {:x 2 :y (+ 64 d) :z 2})

(defn check-free-drop [d]
  (let [r (run (platform d) (near 8 64 2) {} (on-platform d))]
    (is (= "found" (:status r)))
    (is (= {:risk 0 :maxDrop d} {:risk (cost r :risk) :maxDrop (cost r :maxDrop)}))
    (is (some #{(:drop MOVE)} (moves r)))))

(deftest drops-of-1-to-3-blocks-are-free
  (are [d] (check-free-drop d) 1 2 3))

(deftest drop-of-4-refused-at-default-max-drop-and-costs-1-hp-when-allowed
  (let [refused (search (platform 4) (near 8 64 2) {} (on-platform 4))
        allowed (run (platform 4) (near 8 64 2) {:maxDrop 6} (on-platform 4))]
    (is (= ["partial" "exhausted"] (status+reason refused)))
    (is (every? (fn [[_ y]] (= y 68)) (cells refused)))
    (is (= "found" (:status allowed)))
    (is (= {:risk 1 :maxDrop 4} {:risk (cost allowed :risk) :maxDrop (cost allowed :maxDrop)}))))

(defn check-gap-jumped [gap]
  (let [r (run (world {:fill [[5 60 -2 (+ 4 gap) 63 40 "air"]]}) (near (+ 5 gap 3) 64 2))]
    (is (= "found" (:status r)))
    (is (= [1 1] [(cost r :jumps) (cost r :risk)]))
    (is (some #{(:gap MOVE)} (moves r)))))

(deftest gap-of-1-2-or-3-cells-is-jumped
  (are [gap] (check-gap-jumped gap) 1 2 3))

(deftest gap-of-4-is-not-jumped
  (let [wide (search (world {:fill [[5 60 -2 8 63 40 "air"]]}) (near 12 64 2))]
    (is (= ["partial" "exhausted"] (status+reason wide)))))

(deftest gap-jump-needs-headroom-over-the-gap
  (let [r (run (world {:fill [[5 60 -2 6 63 40 "air"] [5 65 -2 6 65 40 "stone"]]}) (near 10 64 2))]
    (is (not= "found" (:status r)))))

(deftest gap-jump-may-land-one-lower
  (let [r (run (world {:fill [[5 60 -2 6 63 40 "air"] [7 63 -2 40 63 40 "air"]]}) (near 10 63 2))]
    (is (= "found" (:status r)))))

;; ---- farmland ----

(defn farmland [x0 z0 x1 z1] [x0 63 z0 x1 63 z1 "farmland"])

(defn tramples [r]
  (filter (fn [s] (and (#{(:gap MOVE) (:drop MOVE)} (:move s)) (= 63 (:y s)) (= 15 (:h s))))
          (get-in r [:path :steps])))

(defn check-never-lands-on-farmland [w goal from]
  (let [r (run w goal {} from)]
    (is (= "found" (:status r)))
    (is (empty? (tramples r)))))

(deftest drop-and-gap-jump-never-land-on-farmland
  (are [w goal from] (check-never-lands-on-farmland w goal from)
    (world {:fill [[0 64 0 4 64 4 "stone"] (farmland 5 -2 7 40)]}) (near 9 64 2) {:x 2 :y 65 :z 2}
    (world {:fill [[5 60 -2 6 63 40 "air"] (farmland 7 -2 9 10)]}) (near 11 64 2) start))

(deftest jump-up-one-block-onto-farmland-is-kept
  (let [w (world {:fill [(farmland 1 1 3 3) [2 63 2 2 63 2 "air"] [2 63 1 2 63 1 "stone"]]})
        r (run w (near 8 64 2) {} {:x 2 :y 63 :z 2})]
    (is (= "found" (:status r)))
    (is (= [[(:start MOVE) 2 63 2 0] [(:jump MOVE) 3 63 2 15]]
           (mapv (juxt :move :x :y :z :h) (take 2 (get-in r [:path :steps])))))))

;; ---- gaps up ----

(defn gap-up [gap & [extra]]
  (world {:fill (into [[5 60 -2 (+ 4 gap) 63 40 "air"] [(+ 5 gap) 64 -2 40 64 40 "stone"]] extra)}))

(deftest gap-jump-landing-one-higher
  (are [gap extra status] (= status (:status (search (gap-up gap extra) (near (+ 5 gap 3) 65 2))))
    1 [] "found"
    2 [] "found"
    3 [] "partial"
    2 [[5 66 -2 6 66 40 "stone"]] "partial"))

(deftest summary-names-a-jump-up-over-a-gap
  (let [r (search (gap-up 2) (near 10 65 2))]
    (is (str/includes? (get-in r [:path :summary]) "1 jump up over a gap"))))

(deftest gap-jump-up-costs-0-3-s-more-than-the-level-one
  (let [up (search (gap-up 2) (near 10 65 2))
        level (search (world {:fill [[5 60 -2 6 63 40 "air"]]}) (near 10 64 2))]
    (is (< (js/Math.abs (- (cost up :seconds) (cost level :seconds) 0.3)) 0.02))))

;; ---- lava, cobweb, magma, soul sand ----

(deftest lava-pool-is-walked-round-when-a-detour-exists
  (let [r (run (world {:fill [[5 63 0 7 63 4 "lava"]]}) (near 10 64 2))]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :risk)))
    (is (not-any? #{(:gap MOVE)} (moves r)))))

(deftest lava-strip-across-everything-is-gap-jumped-with-risk
  (let [r (run (world {:fill [[5 63 -2 7 63 40 "lava"]]}) (near 10 64 2))]
    (is (= "found" (:status r)))
    (is (>= (cost r :risk) 1))
    (is (some #{(:gap MOVE)} (moves r)))))

(defn check-cobweb-never-entered [y]
  (let [r (run (world {:fill [[5 y -2 5 y 40 "cobweb"]]}) (near 8 64 2))]
    (is (not= "found" (:status r)))
    (is (every? (fn [[x]] (< x 5)) (cells r)))))

(deftest cobweb-is-never-entered-jumped-or-stood-in
  (are [y] (check-cobweb-never-entered y) 64 65))

(deftest magma-strip-is-avoided-when-the-detour-is-cheap
  (let [r (run (world {:fill [[5 63 -2 5 63 4 "magma_block"]]}) (near 8 64 2))]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :risk)))))

(deftest magma-strip-is-crossed-when-the-detour-costs-more-than-2-s
  (let [r (run (world {:fill [[5 63 -2 5 63 8 "magma_block"]]}) (near 8 64 2))]
    (is (= "found" (:status r)))
    (is (= 1 (cost r :risk)))))

(deftest magma-strip-across-everything-is-crossed-with-risk
  (let [r (run (world {:fill [[5 63 -2 5 63 40 "magma_block"]]}) (near 8 64 2))]
    (is (= "found" (:status r)))
    (is (> (cost r :risk) 0))))

(deftest soul-sand-is-slower-to-cross-than-stone
  (let [sand (run (world {:fill [[3 63 -2 8 63 40 "soul_sand"]]}) (near 10 64 2))
        stone (run (world {}) (near 10 64 2))]
    (is (> (cost sand :seconds) (* 1.5 (cost stone :seconds))))))

;; ---- goals and starts ----

(deftest goal-inside-a-block-is-not-standable-range-1-cell-above-serves
  (let [inside (run (world {}) (near 8 63 2))]
    (is (= ["none" "goal-not-standable" nil] [(:status inside) (:reason inside) (:path inside)]))
    (is (= "found" (:status (run (world {}) (near 8 63 2 1)))))))

(defn check-unloaded-goal [goal]
  (let [r (run (world {}) goal)]
    (is (= ["partial" "goal-unloaded"] (status+reason r)))
    (is (< (distance-to-goal r [200 200]) (- (js/Math.hypot 198 198) 20)))))

(deftest unloaded-goal-gives-partial-path-toward-it
  (are [goal] (check-unloaded-goal goal)
    (near 200 64 200 1)
    (xz 200 200 2)))

(deftest start-not-standable
  (are [from] (let [r (run (world {}) (near 8 64 2) {} from)]
                (= ["none" "start-not-standable" nil] [(:status r) (:reason r) (:path r)]))
    {:x 2 :y 63 :z 2}
    {:x 2 :y 70 :z 2}
    {:x 200 :y 64 :z 2}))

(deftest max-nodes-small-gives-budget-with-partial-path
  (let [r (run (world {}) (near 35 64 2) {:maxNodes 50})]
    (is (= ["partial" "budget"] (status+reason r)))
    (is (> (count (get-in r [:path :steps])) 1))
    (is (< (distance-to-goal r [35 2]) 33))))

(deftest same-snapshot-and-query-give-identical-path
  (let [w (world {:fill [[5 64 -2 5 65 30 "stone"] [12 64 5 12 64 40 "oak_fence"]]})
        a (run w (near 20 64 20))
        b (run w (near 20 64 20))]
    (is (= (:path a) (:path b)))
    (is (= (:expanded a) (:expanded b)))))

(defn check-sliced-search-matches-plan [slice]
  (let [w (world {:fill [[5 64 -2 5 65 30 "stone"]]})
        whole (run w (near 20 64 20))
        s (create-search w {:from start :goal (near 20 64 20)})
        done (some (fn [_] ((:step s) slice)) (range 200000))
        sliced ((:result s))]
    (is done)
    (is (= [(:status whole) (:reason whole) (:expanded whole) (:path whole)]
           [(:status sliced) (:reason sliced) (:expanded sliced) (:path sliced)]))))

(deftest create-search-in-slices-matches-plan
  (are [slice] (check-sliced-search-matches-plan slice) 1 7 100))

;; ---- summaries, weight ----

(deftest summary-a-step-up-and-a-drop-of-4
  (let [w (world {:fill [[4 64 -2 6 64 40 "stone"] [7 61 -2 40 63 40 "air"]]})
        r (run w (near 9 61 2) {:maxDrop 6})]
    (is (= "found" (:status r)))
    (is (= "7 blocks, 1 step up, 1 drop of 4" (get-in r [:path :summary])))))

(deftest summary-lava-beside-the-path-counts-the-risk-and-is-named
  (let [w (world {:fill [[4 64 -2 6 65 1 "stone"] [4 64 3 6 64 40 "lava"]]})
        r (run w (near 8 64 2))]
    (is (= "found" (:status r)))
    (is (= 1.5 (cost r :risk)))
    (is (= "6 blocks, passes 1 cell from lava" (get-in r [:path :summary])))))

(deftest weighted-search-still-finds-a-path-expanding-no-more-nodes
  (let [w (world {:fill [[5 64 -2 5 65 30 "stone"]]})
        exact (run w (near 20 64 20))
        greedy (run w (near 20 64 20) {:weight 3})]
    (is (= "found" (:status greedy)))
    (is (<= (:expanded greedy) (:expanded exact)))))

;; ---- partial blocks ----

(defn row
  ([x0 x1 name props] (row x0 x1 name props 64))
  ([x0 x1 name props y] (cond-> [x0 y -2 x1 y 40 name] props (conj props))))

(defn check-partial-row [y block props h]
  (let [r (run (world {:fill [(row 4 6 block props y)]}) (near 9 64 2))]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :jumps)))
    (is (= [[y h] [y h] [y h]]
           (mapv (juxt :y :h) (filter (fn [s] (<= 4 (:x s) 6)) (get-in r [:path :steps])))))))

(deftest partial-block-row-is-walked-across-at-its-height-without-jumping
  (are [y block props h] (check-partial-row y block props h)
    64 "red_bed" {:part "foot" :facing "east"} 9
    64 "oak_slab" {:type "bottom"} 8
    64 "snow" {:layers 4} 6
    63 "soul_sand" nil 14
    63 "chest" nil 14
    63 "farmland" nil 15
    63 "dirt_path" nil 15
    63 "enchanting_table" nil 12
    63 "snow" {:layers 8} 14))

(deftest soul-sand-is-crossed-slowly-but-is-found
  (let [sand (run (world {:fill [(row 3 8 "soul_sand" nil 63)]}) (near 10 64 2))]
    (is (= "found" (:status sand)))
    (is (> (cost sand :seconds) (* 1.5 (cost (run (world {}) (near 10 64 2)) :seconds))))))

(deftest staircase-of-snow-layers-is-climbed-without-a-jump
  (let [w (world {:fill (into (vec (map-indexed (fn [i n] (row (+ 4 i) (+ 4 i) "snow" {:layers n})) [2 4 6 8]))
                              [(row 8 12 "stone" nil)])})
        r (run w (near 10 65 2))]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :jumps)))
    (is (not-any? #{(:jump MOVE)} (moves r)))
    (is (= [2 6 10 14] (mapv :h (filter (fn [s] (<= 4 (:x s) 7)) (get-in r [:path :steps])))))))

(deftest from-a-full-block-onto-a-chest-and-off-again-no-jump
  (let [w (world {:fill [(row 3 3 "stone" nil) (row 4 4 "chest" nil) (row 5 6 "stone" nil)]})
        r (run w (near 6 65 2) {} {:x 3 :y 65 :z 2})
        on-chest (first (filter (fn [s] (= 4 (:x s))) (get-in r [:path :steps])))]
    (is (= "found" (:status r)))
    (is (not-any? #{(:jump MOVE)} (moves r)))
    (is (= [64 14] [(:y on-chest) (:h on-chest)]))))

(deftest standing-on-a-partial-block-needs-1-8-headroom
  (let [w (world {:fill [(row 5 5 "chest" nil) [5 66 -2 5 66 40 "stone"]]})
        r (run w (near 5 64 2) {} {:x 3 :y 64 :z 2})]
    (is (not= "found" (:status r)))))

;; ---- stairs ----

(defn stairs [x y facing] [x y -2 x y 40 "oak_stairs" {:facing facing :half "bottom" :shape "straight"}])

(def flight
  [(stairs 4 64 "east") (stairs 5 65 "east") (stairs 6 66 "east") (stairs 7 67 "east")
   [5 64 -2 5 64 40 "stone"] [6 64 -2 6 65 40 "stone"] [7 64 -2 7 66 40 "stone"]
   [8 64 -2 12 67 40 "stone"]])

(deftest four-step-stair-flight-is-climbed-with-walks-only
  (let [r (run (world {:fill flight}) (near 9 68 2))]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :jumps)))
    (is (= #{(:start MOVE) (:walk MOVE)} (set (moves r))))
    (is (= [65 66 67 68] (mapv second (filter (fn [[x]] (<= 4 x 7)) (cells r)))))))

(deftest descending-the-flight-is-walks-and-drops
  (let [r (run (world {:fill flight}) (near 2 64 2) {} {:x 9 :y 68 :z 2})]
    (is (= "found" (:status r)))
    (is (every? #{(:start MOVE) (:walk MOVE) (:drop MOVE)} (moves r)))))

(def sealed3 [[5 64 2 5 66 2 "stone"] [3 64 1 5 64 1 "stone"]])

(defn check-stairs-entered [facing from walls jumps]
  (let [w (world {:fill (into [[4 64 2 4 64 2 "oak_stairs" {:facing facing :half "bottom" :shape "straight"}]] walls)})
        r (run w (near 4 65 2) {} from)]
    (is (= "found" (:status r)))
    (is (= jumps (> (cost r :jumps) 0)))))

(deftest stairs-entered-from-each-approach
  (are [facing from walls jumps] (check-stairs-entered facing from walls jumps)
    "east" {:x 2 :y 64 :z 2} [[4 64 1 4 66 1 "stone"] [4 64 3 4 66 3 "stone"]] false
    "east" {:x 4 :y 64 :z 3} (into [[3 64 2 3 66 2 "stone"]] sealed3) true
    "west" {:x 4 :y 64 :z 3} (into [[3 64 2 3 66 2 "stone"]] sealed3) true))

(deftest top-half-stairs-block-is-a-full-obstacle-it-takes-a-jump
  (let [w (world {:fill [[4 64 -2 4 64 40 "oak_stairs" {:facing "east" :half "top" :shape "straight"}]]})
        r (run w (near 6 64 2))]
    (is (= "found" (:status r)))
    (is (= 1 (cost r :jumps)))))

;; ---- head room over the source column ----

(defn house [high inside]
  (world {:fill (into [[0 64 -2 0 75 40 "stone"] [0 64 2 0 (+ 63 high) 2 "air"]] inside)}))

(def outside {:x -1 :y 64 :z 2})

(defn behind [& rows] (vec (for [[name props] rows] [1 64 -2 1 64 40 name props])))

(def stair-inside (mapv (fn [[x0 y0 z0 x1 y1 z1 & rest]] (into [(- x0 3) y0 z0 (- x1 3) y1 z1] rest)) flight))
(def stair-goal (near 6 68 2))

(defn found-behind-doorway? [inside goal high]
  (= "found" (:status (run (house high inside) goal {} outside))))

(deftest riser-behind-a-doorway
  (are [inside goal high found] (= found (found-behind-doorway? inside goal high))
    stair-inside stair-goal 2 false
    stair-inside stair-goal 3 true
    (behind ["oak_slab" {:type "bottom"}]) (near 1 64 2) 2 false
    (behind ["oak_slab" {:type "bottom"}]) (near 1 64 2) 3 true
    (behind ["snow" {:layers 3}]) (near 1 64 2) 2 false
    (behind ["snow" {:layers 3}]) (near 1 64 2) 3 true))

(deftest free-standing-stair-flight-of-the-same-shape-is-found
  (is (= "found" (:status (run (world {:fill stair-inside}) stair-goal {} {:x -1 :y 64 :z 2})))))

(deftest low-risers-behind-a-2-high-doorway-are-found
  (are [block props] (= "found" (:status (run (house 2 (behind [block props])) (near 1 64 2) {} outside)))
    "snow" {:layers 2}
    "white_carpet" {}))

;; ---- goal side flood ----

(def floating (world {:fill [[10 66 10 14 66 14 "stone"]]}))
(def flooding {:floodAfter 20 :preFlood 0})

(deftest goal-on-platform-3-above-ground-is-goal-enclosed-cheaply
  (let [r (run floating (near 12 67 12) flooding)]
    (is (= "goal-enclosed" (:reason r)))
    (is (and (> (:expanded r) 0) (< (:expanded r) 4000)))))

(deftest easy-query-never-floods
  (let [r (run (world {}) (near 12 64 2) {:floodAfter 20 :goalFlood 1})]
    (is (= ["found" true] [(:status r) (< (:expanded r) 20)]))))

(deftest without-the-flood-a-sealed-platform-is-exhausted
  (is (= "exhausted" (:reason (run floating (near 12 67 12) {:preFlood 0})))))

(deftest goal-on-ground-start-on-pillar-it-can-drop-from-is-found
  (is (= "found" (:status (run (world {:fill [[2 64 2 2 66 2 "stone"]]}) (near 8 64 2) {} {:x 2 :y 67 :z 2})))))

(deftest flood-larger-than-its-budget-hands-over-to-the-main-search
  (let [small (run floating (near 12 67 12) (assoc flooding :goalFlood 10))]
    (is (not= "goal-enclosed" (:reason small)))
    (is (not= "found" (:status small)))
    (is (= "found" (:status (run (world {}) (near 20 64 20) {:goalFlood 10}))))))

(deftest goal-sealed-in-by-a-wall-is-goal-enclosed-with-a-partial-path
  (let [r (run (world {:fill [[5 64 -2 5 65 40 "stone"]]}) (near 8 64 2) flooding)]
    (is (= ["partial" "goal-enclosed"] (status+reason r)))
    (is (= [4 64 2] (last-cell r)))))

(deftest sealed-goal-no-nearer-than-the-start-is-none
  (let [r (run (world {:fill [[5 64 -2 5 65 40 "stone"]]}) (near 8 64 2) flooding {:x 4 :y 64 :z 2})]
    (is (= ["none" "goal-enclosed" nil] [(:status r) (:reason r) (:path r)]))))

(deftest xz-goal-does-not-flood
  (let [goal (xz 12 12 0)]
    (is (not= "goal-enclosed" (:reason (run floating goal flooding))))
    (is (not= "goal-enclosed" (:reason (run floating goal))))))

;; ---- early goal flood ----

(defn ring [cx cz half & [gap]]
  (into [[(- cx half) 64 (- cz half) (+ cx half) 66 (+ cz half) "stone"]
         [(+ (- cx half) 1) 64 (+ (- cz half) 1) (- (+ cx half) 1) 66 (- (+ cz half) 1) "air"]]
        (map (fn [[x z]] [x 64 z x 66 z "air"]) gap)))

(def sealed-goal (near 10 64 10))
(def big-floor [[-2 60 -2 62 63 62 "stone"]])

(defn check-walled-in-goal [fill]
  (let [r (run (world {:fill fill}) sealed-goal)]
    (is (= ["none" "goal-enclosed" nil 0] [(:status r) (:reason r) (:path r) (:expanded r)]))
    (is (= 0 (get-in r [:stats :flooded])))))

(deftest goal-walled-in-on-all-sides-is-goal-enclosed-at-once
  (are [fill] (check-walled-in-goal fill)
    (ring 10 10 1)
    (into big-floor (ring 10 10 1))))

(deftest pre-flood-0-gives-the-old-answer-the-search-exhausts
  (let [r (run (world {:fill (ring 10 10 1)}) sealed-goal {:preFlood 0})]
    (is (= "exhausted" (:reason r)))
    (is (> (:expanded r) 0))))

(deftest goal-flood-0-turns-the-early-flood-off-too
  (is (= "exhausted" (:reason (run (world {:fill (ring 10 10 1)}) sealed-goal {:goalFlood 0})))))

(defn check-not-claimed-early [fill goal]
  (let [r (run (world {:fill fill}) goal)]
    (is (> (:expanded r) 0) (str "expanded " (:expanded r)))
    (is (> (get-in r [:stats :preFlooded]) 0))))

(deftest unsealed-unreachable-goal-is-left-to-the-search
  (are [fill goal] (check-not-claimed-early fill goal)
    [[6 60 -2 40 63 40 "air"] [10 60 9 12 63 11 "stone"]] (near 11 64 10)
    [[9 64 9 11 71 11 "stone"]] (near 10 72 10)))

(deftest goal-in-a-ring-of-wall-with-a-one-cell-gap-is-found
  (let [r (run (world {:fill (ring 10 10 1 [[9 10]])}) sealed-goal)]
    (is (= "found" (:status r)))
    (is (= [10 64 10] (last-cell r)))))

(deftest goal-inside-solid-stone-is-goal-not-standable
  (let [r (run (world {:fill [[10 64 10 10 66 10 "stone"]]}) sealed-goal)]
    (is (= ["none" "goal-not-standable" 0] [(:status r) (:reason r) (:expanded r)]))))

(deftest enclosed-area-larger-than-early-budget-is-left-to-the-search
  (let [r (run (world {:fill (ring 24 24 15)}) (near 24 64 24))]
    (is (not= "found" (:status r)))
    (is (> (:expanded r) 0))
    (is (> (get-in r [:stats :preFlooded]) 24))))

;; ---- search box ----

(defn bar [z1] (world {:fill [[5 64 -2 5 65 z1 "stone"]]}))

(deftest box-limits-the-search
  (are [w options status reason] (= [status reason] (status+reason (search w (near 8 64 2) options)))
    (bar 10) {:margin 3} "partial" "box"
    (bar 10) {} "found" nil
    (bar 40) {:margin 3} "partial" "box"
    (bar 40) {} "partial" "exhausted"))

(def step-world (world {:fill [[5 64 -2 40 64 40 "stone"]]}))

(deftest y-margin-limits-the-search
  (are [goal options status reason] (= [status reason] (status+reason (run step-world goal options)))
    (near 8 65 2) {:yMargin 0} "found" nil
    (xz 8 2 0) {:yMargin 0} "partial" "box"
    (xz 8 2 0) {:yMargin 1} "found" nil
    (xz 8 2 0) {} "found" nil))
