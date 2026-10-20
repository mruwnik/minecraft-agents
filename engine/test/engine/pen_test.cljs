(ns engine.pen-test
  "jobs.lib.pen: the pen check, over a block grid that is stone up to y 63 and air above."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.pen :as pen]
            [jobs.lib.pen-blocks :as pen-blocks]
            [jobs.lib.pen-world :as pw]))

(defn block
  "A JS block from a cell value: a name, or [name {property value}]."
  [v]
  (let [[n props] (if (string? v) [v nil] v)]
    #js {:name n :properties (clj->js (or props {}))}))

(defn block-at-from
  "A block-at over {[x y z] value}: absent cells are stone at y 63 and below, air above; :unloaded reads as nil."
  [cells]
  (fn [{:keys [x y z]}]
    (let [v (get cells [x y z] (if (<= y 63) "stone" "air"))]
      (when-not (= :unloaded v) (block v)))))

(defn ring
  "The ring of cells round x 0..4, z 0..4 (a pen interior of 25), at heights ys, all of one value; dx moves it along x."
  ([value ys] (ring value ys 0))
  ([value ys dx]
   (into {} (for [x (range -1 6) z (range -1 6) y ys
                  :when (or (#{-1 5} x) (#{-1 5} z))]
              [[(+ x dx) y z] value]))))

(def start [2 64 2])
(def pen-at {:at {:x 2 :y 64 :z 2}})

(defn check [cells & [opts]]
  (pen/check (merge {:block-at (block-at-from cells)} pen-at opts)))

(def fence-ring (ring "oak_fence" [64]))

(deftest litter-inside-the-fence-is-no-step
  (let [floor (into {} (for [x (range 5) z (range 5)] [[x 64 z] "leaf_litter"]))]
    (is (:closed? (check (merge fence-ring floor))))))

(deftest ground-beside-the-fence-top-is-a-way-in
  (let [r (check (assoc fence-ring [6 64 2] "stone"))]
    (is (false? (:closed? r)))
    (is (= [{:pos {:x 5 :y 64 :z 2} :why :perch-in}] (:leaks r)))))

(def box-opts {:box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 4}}})

(deftest ground-beside-the-fence-top-is-a-way-in-for-a-box-too
  (let [r (check (assoc fence-ring [6 64 2] "stone") (assoc box-opts :at nil))]
    (is (false? (:closed? r)))
    (is (= [{:pos {:x 5 :y 64 :z 2} :why :perch-in}] (:leaks r)))))

(deftest a-walled-in-pocket-beside-the-fence-is-no-way-in
  (let [wall (fn [x z] (for [y (range 64 70)] [[x y z] "stone"]))
        pocket (into {[6 64 2] "stone"} (mapcat #(apply wall %)) [[7 2] [6 1] [6 3]])
        cells (merge fence-ring pocket)]
    (is (:closed? (check cells)))
    (is (:closed? (check cells (assoc box-opts :at nil))))))

(deftest unseen-ground-beside-the-fence-is-no-definite-way-in
  (let [cells (assoc fence-ring [6 64 2] "stone" [6 63 2] :unloaded [6 65 2] :unloaded)]
    (are [opts] (let [r (check cells opts)]
                  (not-any? #(= :perch-in (:why %)) (:leaks r)))
      {}
      (assoc box-opts :at nil))))

(defn without [cells & ks] (apply dissoc cells ks))

;; ------------------------------------------------------------------ what the ring is made of

(deftest a-ring-of-each-block-holds-or-leaks-by-its-height
  (are [value ys closed?] (= closed? (:closed? (check (ring value ys))))
    "oak_fence" [64] true
    "cobblestone_wall" [64] true
    ["oak_fence_gate" {:open false}] [64] true
    ["oak_fence_gate" {:open true}] [64] false
    "stone" [64] false
    "stone" [64 65] true
    ["oak_slab" {:type "bottom"}] [64] false
    ["oak_slab" {:type "top"}] [64] false
    ["oak_slab" {:type "double"}] [64] false
    ["oak_slab" {:type "double"}] [64 65] true
    ["oak_trapdoor" {:open false :half "bottom"}] [64] false
    ["oak_door" {:open false}] [64 65] true
    ["oak_door" {:open true}] [64 65] false
    "water" [64] false
    "lava" [64] true
    "short_grass" [64] false
    "short_grass" [64 65] false
    "water" [64 65] false
    "white_carpet" [64] false
    "oak_fence" [64 65] true))

(deftest a-carpet-on-a-fence-changes-nothing-for-an-animal
  (is (true? (:closed? (check (merge fence-ring (ring "white_carpet" [65])))))))

(deftest a-carpet-inside-the-pen-is-walked-over
  (let [r (check (assoc fence-ring [2 64 2] "white_carpet"))]
    (is (true? (:closed? r)))
    (is (= 25 (count (:inside r))))))

(deftest water-in-the-pen-floor-is-walked-through
  (let [r (check (assoc fence-ring [2 64 2] "water"))]
    (is (true? (:closed? r)))
    (is (contains? (:inside r) [2 64 2]))))

(deftest lava-in-the-pen-floor-is-never-entered
  (let [r (check (assoc fence-ring [3 64 2] "lava"))]
    (is (true? (:closed? r)))
    (is (= 24 (count (:inside r))))))

(deftest a-slab-in-the-pen-floor-is-walked-over
  (let [r (check (assoc fence-ring [3 64 2] ["oak_slab" {:type "bottom"}]))]
    (is (true? (:closed? r)))
    (is (= 25 (count (:inside r))))
    (is (contains? (:inside r) [3 64 2]))))

(deftest a-roof-of-top-slabs-leaves-room-to-walk-under
  (let [r (check (merge fence-ring (into {} (for [x (range 0 5) z (range 0 5)] [[x 65 z] ["oak_slab" {:type "top"}]]))))]
    (is (true? (:closed? r)))
    (is (= 25 (count (:inside r))))))

(deftest a-low-wall-with-a-ceiling-over-it-holds
  (is (true? (:closed? (check (merge (ring "stone" [64]) (ring ["oak_slab" {:type "top"}] [65])))))))

(deftest a-diagonal-step-past-a-post-needs-both-sides-open
  ;; from (0 0) the east cell is a step down, the south cell a fence; the block at (1 1) is one up from (0 0)
  ;; but two up from the east cell, and walled off from the rest: only a squeeze past the post would reach it
  (let [cells (merge fence-ring {[1 63 0] "air" [0 64 1] "oak_fence" [1 64 1] "stone" [2 64 1] "oak_fence" [1 64 2] "oak_fence"})
        r (check cells {:at {:x 0 :y 64 :z 0}})]
    (is (true? (:closed? r)))
    (is (not (contains? (:inside r) [1 65 1])))))

;; ------------------------------------------------------------------ the answer

(deftest a-closed-pen-is-closed
  (let [r (check fence-ring)]
    (is (= {:closed? true :reason nil :leaks [] :gates []} (select-keys r [:closed? :reason :leaks :gates])))
    (is (= 25 (count (:inside r))))))

(deftest a-closed-gate-is-listed-and-holds
  (let [r (check (assoc fence-ring [2 64 -1] ["oak_fence_gate" {:open false}]))]
    (is (true? (:closed? r)))
    (is (= [{:pos {:x 2 :y 64 :z -1} :open? false}] (:gates r)))))

(deftest an-open-gate-leaks-at-the-gate
  (let [r (check (assoc fence-ring [2 64 -1] ["oak_fence_gate" {:open true}]))]
    (is (false? (:closed? r)))
    (is (= :leak (:reason r)))
    (is (= [{:pos {:x 2 :y 64 :z -1} :why :open-gate}] (:leaks r)))
    (is (= [{:pos {:x 2 :y 64 :z -1} :open? true}] (:gates r)))))

(deftest a-leaky-pen-still-knows-what-it-would-enclose
  (are [cells] (= 25 (count (:inside (check cells))))
    (assoc fence-ring [2 64 -1] ["oak_fence_gate" {:open true}])
    (without fence-ring [5 64 3])
    (without fence-ring [5 64 2] [5 64 3])))

(deftest a-leaky-pen-counts-an-animal-inside-it-by-membership
  (let [r (check (assoc fence-ring [2 64 -1] ["oak_fence_gate" {:open true}]))]
    (are [pos expected] (= expected (pen/in-pen? r pos))
      {:x 2.5 :y 64.0 :z 2.5} true
      {:x 2.5 :y 64.0 :z -3.5} false)))

(deftest the-gates-listed-are-the-pens-own
  (let [r (check (merge (assoc fence-ring [2 64 -1] ["oak_fence_gate" {:open true}])
                        (assoc (ring "oak_fence" [64] 10) [12 64 -1] ["oak_fence_gate" {:open false}])))]
    (is (= [{:pos {:x 2 :y 64 :z -1} :open? true}] (:gates r)))))

(defn strip
  "block-at for a strip of ground x -2..40, z -3..7 with nothing beyond it and a wall closing the west end."
  [cells]
  (let [base (block-at-from cells)]
    (fn [{:keys [x y z] :as pos}]
      (cond
        (not (<= -3 x 40)) (block "air")
        (not (<= -3 z 7)) (block "air")
        (and (= x -3) (<= 64 y 66)) (block "stone")
        :else (base pos)))))

(deftest the-leak-named-is-in-the-pens-own-wall-not-a-neighbours
  ;; the only way on from the first pen is east, past a second low-walled pen: the walk's far end is beyond it
  (let [r (pen/check {:block-at (strip (merge (ring "stone" [64]) (ring "stone" [64] 10))) :at {:x 2 :y 64 :z 2} :max-cells 200})]
    (is (= [:climb] (distinct (map :why (:leaks r)))))
    (is (every? #(<= -1 (get-in % [:pos :x]) 5) (:leaks r)))))

(deftest a-missing-fence-leaks-there
  (let [r (check (without fence-ring [5 64 3]))]
    (is (false? (:closed? r)))
    (is (= [{:pos {:x 5 :y 64 :z 3} :why :gap}] (:leaks r)))))

(deftest a-gap-of-two-fences-is-still-a-gap
  (let [r (check (without fence-ring [5 64 2] [5 64 3]))]
    (is (= :leak (:reason r)))
    (is (= [:gap] (distinct (map :why (:leaks r)))))
    (is (= [5] (distinct (map #(get-in % [:pos :x]) (:leaks r)))))))

(deftest a-missing-corner-post-is-no-leak-between-two-fences
  (is (true? (:closed? (check (without fence-ring [-1 64 -1]))))))

(deftest a-cut-corner-with-no-posts-behind-it-is-no-way-out
  ;; the corner cell (4 4) is a fence; the ring cells behind it are gone: two posts touch only at their corners
  (let [cells (-> fence-ring (without [5 64 4] [4 64 5] [5 64 5]) (assoc [4 64 4] "oak_fence"))
        r (check cells)]
    (is (true? (:closed? r)))
    (is (= 24 (count (:inside r))))))

(deftest a-low-wall-leaks-and-says-where
  (let [r (check (without (ring "stone" [64]) [5 64 3]))]
    (is (false? (:closed? r)))
    (is (seq (:leaks r)))))

(defn unseen-border
  "Cells at x or z -3 and 7, y 63 and 64, unseen: the edge of what a body has looked at."
  [cells]
  (into cells (for [x (range -3 8) z (range -3 8) y [63 64] :when (or (#{-3 7} x) (#{-3 7} z))] [[x y z] :unloaded])))

(deftest a-low-wall-with-unseen-ground-beyond-waits-for-a-look
  (is (= :unloaded (:reason (check (unseen-border (ring "stone" [64])))))))

(def divider
  "A fence line across the closed ring at x 2, z 0..4, with its middle cell given by value (nil: a gap)."
  (fn [value]
    (merge fence-ring
           (into {} (for [z (range 0 5)] [[2 64 z] "oak_fence"]))
           {[2 64 2] (or value "air")})))

(deftest a-gap-or-open-gate-is-no-leak-evidence-while-a-cell-is-unseen
  ;; the unseen cell shows only after a look; which side is outside cannot be told from a gap or a gate alone
  (are [cells] (= :unloaded (:reason (check (assoc cells [4 64 4] :unloaded))))
    (divider nil)
    (divider ["oak_fence_gate" {:open true}])))

(deftest a-one-block-divider-inside-is-no-climb-while-a-cell-is-unseen
  (let [stone-line (into {} (for [z (range 1 4)] [[2 64 z] "stone"]))
        r (check (assoc (merge fence-ring stone-line) [4 64 4] :unloaded) {:at {:x 0 :y 64 :z 2}})]
    (is (= :unloaded (:reason r)))
    (is (= [:unloaded] (distinct (map :why (:leaks r)))))))

(deftest a-one-block-wall-all-round-is-climbed-over
  (let [r (check (ring "stone" [64]))]
    (is (false? (:closed? r)))
    (is (= :leak (:reason r)))
    (is (= [{:pos {:x 2 :y 65 :z -1} :why :climb}] (:leaks r)))))

(defn lowered
  "block-at for a bare plateau (interior x 0..4, z 0..4, ground top 64) with the ground lowered to ground-top all round it."
  [ground-top]
  (let [base (block-at-from {})]
    (fn [{:keys [x y z] :as pos}]
      (if (and (not (and (<= 0 x 4) (<= 0 z 4))) (<= ground-top y 63))
        (block "air")
        (base pos)))))

(deftest a-plateau-holds-past-a-drop-of-more-than-three
  (are [ground-top closed?] (= closed? (:closed? (pen/check {:block-at (lowered ground-top) :at {:x 2 :y 64 :z 2}})))
    63 false
    61 false
    60 true))

(deftest a-fence-top-reached-from-a-raised-floor-is-a-way-out
  (let [raised (into {} (for [x (range 0 5) z (range 0 5)] [[x 64 z] "stone"]))]
    (is (false? (:closed? (check (merge raised fence-ring) {:at {:x 2 :y 65 :z 2}}))))))

(deftest a-pen-bigger-than-the-bound-is-not-closed-and-says-so
  (let [r (check fence-ring {:max-cells 10})]
    (is (false? (:closed? r)))
    (is (= :unbounded (:reason r)))
    (is (= [] (:leaks r)))
    (is (= #{} (:inside r)))))

(deftest unloaded-leaks-list-the-cells-nearest-to-near-first
  (let [cells (reduce #(assoc %1 %2 :unloaded) fence-ring (map (fn [x] [x 64 3]) (range 0 6)))
        xs (fn [opts] (mapv (comp :x :pos) (:leaks (check cells opts))))]
    (is (= [0 1 2 3 4] (xs {})) "by coordinate without :near")
    (is (= [4 3 2 1 0] (xs {:near [4 64 3]})) "nearest first with :near")))

(deftest an-unloaded-wall-cell-cannot-be-proven-closed
  (let [r (check (assoc fence-ring [5 64 3] :unloaded))]
    (is (false? (:closed? r)))
    (is (= :unloaded (:reason r)))
    (is (= [{:pos {:x 5 :y 64 :z 3} :why :unloaded}] (:leaks r)))))

(deftest an-unseen-cell-under-a-fence-post-does-not-matter
  (let [r (check (merge fence-ring (ring :unloaded [63])))]
    (is (true? (:closed? r)) "nothing stands in or on a cell under a fence post")
    (is (= [] (:leaks r)))))

(deftest a-doorway-one-block-high-leaks-whatever-its-head-cell-reads
  (let [doorway (merge (ring "stone" [64 65 66]) {[2 64 -1] "air" [2 65 -1] "stone" [2 66 -1] "stone"})]
    (are [head] (= [{:pos {:x 2 :y 64 :z -1} :why :gap}] (:leaks (check (assoc doorway [2 65 -1] head) {:max-cells 100})))
      "stone" :unloaded "air")))

(deftest a-start-that-is-no-floor-answers-no-start
  (are [at] (= :no-start (:reason (check fence-ring {:at at})))
    {:x 2 :y 70 :z 2}
    {:x 2 :y 63 :z 2}
    {:x -1 :y 64 :z 2}))

(deftest an-unseen-start-floor-answers-unloaded-not-no-start
  (let [r (check fence-ring {:at {:x 2 :y 64 :z 2}})
        unseen (assoc fence-ring [2 63 2] :unloaded)
        r2 (check unseen)
        all-unseen (into {} (for [x (range 0 5) z (range 0 5) y [63 64 65]] [[x y z] :unloaded]))
        r3 (pen/check {:block-at (block-at-from all-unseen) :box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 4}}})]
    (is (true? (:closed? r)))
    (is (= :unloaded (:reason r2)) "the floor under the start was never seen: not a missing floor")
    (is (= :unloaded (:reason r3)) "a box of unseen cells: nothing to start from, but not :no-start")
    (is (seq (:leaks r3)) "the unseen cells are listed so a caller can look at them")))

;; ------------------------------------------------------------------ a box says what the pen is meant to be

(def box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 4}})

(deftest a-box-is-closed-when-nothing-leaves-it
  (let [r (pen/check {:block-at (block-at-from fence-ring) :box box})]
    (is (true? (:closed? r)))
    (is (= 25 (count (:inside r))))))

(deftest a-box-lists-every-crossing-out-of-it
  (let [r (pen/check {:block-at (block-at-from (without fence-ring [5 64 1] [5 64 3])) :box box})]
    (is (false? (:closed? r)))
    (is (= #{{:x 5 :y 64 :z 1} {:x 5 :y 64 :z 3}} (set (map :pos (:leaks r)))))))

(deftest a-box-lists-each-leak-position-once
  (let [r (pen/check {:block-at (block-at-from (without fence-ring [5 64 1] [5 64 2] [5 64 3])) :box box})
        positions (map :pos (:leaks r))]
    (is (= 3 (count positions)))
    (is (= 3 (count (distinct positions))))))

(deftest a-box-over-a-one-block-wall-says-climb
  (let [r (pen/check {:block-at (block-at-from (ring "stone" [64])) :box box})]
    (is (false? (:closed? r)))
    (is (= [:climb] (distinct (map :why (:leaks r)))))))

(deftest a-box-smaller-than-the-pen-says-open-ground
  (let [r (pen/check {:block-at (block-at-from fence-ring) :box {:min {:x 0 :y 64 :z 0} :max {:x 3 :y 64 :z 4}}})]
    (is (false? (:closed? r)))
    (is (= [:open] (distinct (map :why (:leaks r)))))))

(deftest a-box-ignores-the-tops-of-its-own-fences
  (let [tall {:min {:x -1 :y 64 :z -1} :max {:x 5 :y 66 :z 5}}
        r (pen/check {:block-at (block-at-from fence-ring) :box tall})]
    (is (true? (:closed? r)))))

(deftest a-box-taller-than-the-pen-ignores-unseen-cells-under-its-floor
  (let [deep {:min {:x 0 :y 55 :z 0} :max {:x 4 :y 80 :z 4}}
        buried (into {} (for [x (range 0 5) z (range 0 5) y [58 59 60]] [[x y z] :unloaded]))
        r (pen/check {:block-at (block-at-from (merge fence-ring buried)) :box deep})]
    (is (true? (:closed? r)))
    (is (empty? (:leaks r)))))

;; ------------------------------------------------------------------ membership

(deftest in-pen-reads-the-feet-cell-of-a-position
  (let [r (check fence-ring)]
    (are [pos expected] (= expected (pen/in-pen? r pos))
      {:x 2.5 :y 64.0 :z 2.5} true
      {:x 0.1 :y 64.0 :z 4.9} true
      {:x 2.5 :y 64.5 :z 2.5} true
      {:x 2.5 :y 63.995 :z 2.5} true
      {:x 2.5 :y 66.0 :z 2.5} true
      {:x 2.5 :y 67.0 :z 2.5} true
      {:x 2.5 :y 68.0 :z 2.5} false
      {:x 5.5 :y 64.0 :z 2.5} false
      {:x 2.5 :y 70.0 :z 2.5} false
      {:x -0.5 :y 64.0 :z 2.5} false)))

(deftest in-pen-is-false-for-an-open-field
  (is (false? (pen/in-pen? (check {} {:max-cells 50}) {:x 2.5 :y 64.0 :z 2.5}))))

(deftest a-fence-one-block-above-the-floor-leaks-under-it
  (let [floating (ring "oak_fence" [65])
        box {:box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 66 :z 4}}}]
    (is (false? (:closed? (check floating))) "from a cell")
    (is (false? (:closed? (check floating (assoc box :at nil)))) "from a box taller than the floor")
    (is (false? (:closed? (check floating (assoc-in (assoc box :at nil) [:box :min :y] 63)))) "box starting below the floor")))

;; ------------------------------------------------------------------ uneven ground

(deftest a-block-inside-beside-a-fence-names-the-fence-top-as-the-climb
  (let [r (pen/check {:block-at (block-at-from (assoc fence-ring [0 64 2] "stone")) :box box})]
    (is (false? (:closed? r)))
    (is (= {:pos {:x -1 :y 64 :z 2} :why :climb} (first (:leaks r))))))

(deftest a-gate-a-step-above-the-floor-inside-is-listed
  (let [inside-dug (into {} (for [x (range 5) z (range 5)] [[x 63 z] "air"]))
        r (check (merge fence-ring inside-dug {[2 64 -1] "oak_fence_gate"})
                 {:at {:x 2 :y 63 :z 2}})]
    (is (true? (:closed? r)))
    (is (= [{:pos {:x 2 :y 64 :z -1} :open? false}] (:gates r)))))

;; ------------------------------------------------------------------ over a 1-2 high wall

(def wall2 (ring "stone" [64 65]))

(deftest a-block-or-slope-inside-a-two-high-wall-is-a-way-out
  (let [slope (into {} (for [z (range 5)] [[0 64 z] "stone"]))]
    (are [inside opts] (false? (:closed? (check (merge wall2 inside) opts)))
      {[0 64 2] "stone"} nil
      {[0 64 2] "stone"} {:at nil :box box}
      {[0 64 0] "stone"} nil
      {[1 64 2] "stone" [0 64 2] "stone"} nil
      {[0 64 2] ["stone_slab" {:type "double"}]} nil
      slope nil
      slope {:at nil :box box})))

(deftest a-block-inside-a-two-high-wall-with-unseen-ground-beyond-is-a-climb-once-no-look-is-left
  (let [r (check (unseen-border (merge wall2 {[0 64 2] "stone"})) {:final? true})]
    (is (false? (:closed? r)))
    (is (= :leak (:reason r)))
    (is (= :climb (:why (first (:leaks r)))))))

(deftest a-three-high-wall-holds-past-a-block-inside
  (is (true? (:closed? (check (merge (ring "stone" [64 65 66]) {[0 64 2] "stone"}))))))

(deftest a-fence-on-a-raised-wall-block-holds-past-a-block-inside
  (is (true? (:closed? (check (merge (ring "stone" [64]) (ring "oak_fence" [65]) {[0 64 2] "stone"}))))))

(defn pool [depth]
  (into {} (for [x (range 0 5) z (range 0 5) y (range 64 (+ 64 depth))] [[x y z] "water"])))

(deftest water-two-deep-lifts-an-animal-over-a-two-high-wall
  (are [wall depth closed?] (= closed? (:closed? (check (merge wall (pool depth)))))
    wall2 1 true
    wall2 2 false
    wall2 3 false
    (ring "stone" [64 65 66 67]) 2 true
    fence-ring 2 false))

;; ------------------------------------------------------------------ a drop inside the gate

(defn sunk-floor
  "The ring with a gate at 2 64 -1 and the inside floor of rows z 1..4 dug down by depth below the row z 0."
  [depth]
  (merge (assoc fence-ring [2 64 -1] ["oak_fence_gate" {:open false}])
         (into {} (for [x (range 0 5) z (range 1 5) y (range (- 64 depth) 64)] [[x y z] "air"]))))

(deftest a-drop-of-more-than-one-just-inside-the-gate-is-a-finding
  (let [r (check (sunk-floor 2) {:at {:x 2 :y 64 :z 0}})]
    (is (true? (:closed? r)))
    (is (= [{:gate {:x 2 :y 64 :z -1} :drop 2}] (map #(select-keys % [:gate :drop]) (:gate-drops r))))
    (is (= [{:gate {:x 2 :y 64 :z -1} :drop 2}] (map #(select-keys % [:gate :drop]) (:gate-drops (pen/summary r)))))))

(deftest a-step-of-one-inside-the-gate-is-no-finding
  (are [depth] (empty? (:gate-drops (check (sunk-floor depth) {:at {:x 2 :y 64 :z 0}})))
    1
    0))

(deftest gates-near-lists-each-gate-once-in-position-order-with-its-open-state
  (let [cells (assoc fence-ring [-1 64 2] ["oak_fence_gate" {:open "true"}] [5 65 3] ["oak_fence_gate" {:open "false"}])
        r (check cells)]
    (is (= [{:pos {:x -1 :y 64 :z 2} :open? true} {:pos {:x 5 :y 65 :z 3} :open? false}] (:gates r)))))

;; ------------------------------------------------------------------ the fill against a plain reference

(defn reference-fill
  "The breadth-first fill written plainly (persistent queue, vectors): what pen/fill must answer."
  [world seeds box max-cells]
  (let [seen (volatile! (into {} (map (fn [{:keys [key s]}] [key {:s s :from nil}])) seeds))]
    (loop [queue (into cljs.core/PersistentQueue.EMPTY (map :key) seeds) order (mapv :key seeds) exits []]
      (cond
        (empty? queue) {:seen @seen :order order :exits exits :truncated? false}
        (>= (count @seen) max-cells) {:seen @seen :order order :exits exits :truncated? true :last (peek order)}
        :else
        (let [[x y z :as k] (peek queue)
              [order exits queue]
              (reduce (fn [[order exits queue] [[dx dz] {:keys [top perch?]}]]
                        (let [nk [(+ x dx) (pw/floor-cell top) (+ z dz)]]
                          (cond
                            (and box (pw/outside? box (nk 0) (nk 2))) [order (conj exits {:from k :to nk :top top :perch? perch?}) queue]
                            (contains? @seen nk) [order exits queue]
                            :else (do (vswap! seen assoc nk {:s top :from k :perch? perch?})
                                      [(conj order nk) exits (conj queue nk)]))))
                      [order exits (pop queue)]
                      (pw/moves world x z (:s (@seen k))))]
          (recur queue order exits))))))

(defn random-cells
  "A 9x9 patch of ground-level cells of fence, gate, stone, slab, water, air or fence-top blocks, from seed."
  [seed]
  (let [r (atom (+ 7 (* 7919 seed)))
        next! #(swap! r (fn [v] (mod (+ (* v 1103515245) 12345) 2147483648)))
        pick (fn [xs] (nth xs (mod (quot (next!) 8) (count xs))))]
    (into {} (for [x (range -2 7) z (range -2 7) y [63 64 65]
                   :let [v (pick [nil nil nil "oak_fence" "oak_fence" ["oak_fence_gate" {:open "true"}] ["oak_fence_gate" {:open "false"}]
                                  "stone" "air" "water" ["oak_slab" {:type "bottom"}]])]
                   :when v]
               [[x y z] v]))))

(deftest the-fill-equals-the-plain-reference-on-random-patches
  (is (< 20 (apply max (for [seed (range 40)
                             :let [world (pw/make-world (block-at-from (random-cells seed)) nil (pw/store))]]
                         (count (:order (pen/fill world (pw/seeds-at world {:x 2 :y 64 :z 2}) nil 2000)))))))
  (doseq [seed (range 40) max-cells [12 2000] box [nil {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 4}}]]
    (let [cells (random-cells seed)
          world (pw/make-world (block-at-from cells) nil (pw/store))
          seeds (if box (pw/seeds-in-box world box) (pw/seeds-at world {:x 2 :y 64 :z 2}))
          got (pen/fill world seeds box max-cells)
          want (reference-fill world seeds box max-cells)]
      (is (= want got) (str "fill seed " seed))
      (is (= (pen/gates-near world (:order want))
             (->> (:order want)
                  (mapcat (fn [[x y z]] (for [[dx dz] (cons [0 0] pw/orthogonal) dy [0 1 -1]] [(+ x dx) (+ y dy) (+ z dz)])))
                  distinct
                  (keep (fn [[x y z]] (let [b ((:block world) x y z)]
                                        (when (pen-blocks/gate? b) {:pos {:x x :y y :z z} :open? (pen-blocks/on? b "open")}))))
                  (sort-by (juxt (comp :x :pos) (comp :y :pos) (comp :z :pos)))
                  vec))
          (str "gates seed " seed)))))
