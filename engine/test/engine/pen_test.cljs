(ns engine.pen-test
  "jobs.lib.pen: the pen check, over a block grid that is stone up to y 63 and air above."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.pen :as pen]))

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

(deftest a-one-block-wall-all-round-is-climbed-over
  (let [r (check (ring "stone" [64]))]
    (is (false? (:closed? r)))
    (is (= :leak (:reason r)))
    (is (= :climb (:why (first (:leaks r)))))))

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

(deftest an-unloaded-wall-cell-cannot-be-proven-closed
  (let [r (check (assoc fence-ring [5 64 3] :unloaded))]
    (is (false? (:closed? r)))
    (is (= :unloaded (:reason r)))
    (is (= [{:pos {:x 5 :y 64 :z 3} :why :unloaded}] (:leaks r)))))

(deftest an-unseen-cell-under-a-fence-post-does-not-matter
  (let [r (check (merge fence-ring (ring :unloaded [63])))]
    (is (true? (:closed? r)) "nothing stands in or on a cell under a fence post")
    (is (= [] (:leaks r)))))

(deftest a-start-that-is-no-floor-answers-no-start
  (are [at] (= :no-start (:reason (check fence-ring {:at at})))
    {:x 2 :y 70 :z 2}
    {:x 2 :y 63 :z 2}
    {:x -1 :y 64 :z 2}))

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

;; ------------------------------------------------------------------ membership

(deftest in-pen-reads-the-feet-cell-of-a-position
  (let [r (check fence-ring)]
    (are [pos expected] (= expected (pen/in-pen? r pos))
      {:x 2.5 :y 64.0 :z 2.5} true
      {:x 0.1 :y 64.0 :z 4.9} true
      {:x 2.5 :y 64.5 :z 2.5} true
      {:x 2.5 :y 63.995 :z 2.5} true
      {:x 5.5 :y 64.0 :z 2.5} false
      {:x 2.5 :y 70.0 :z 2.5} false
      {:x -0.5 :y 64.0 :z 2.5} false)))

(deftest in-pen-is-false-for-an-open-field
  (is (false? (pen/in-pen? (check {} {:max-cells 50}) {:x 2.5 :y 64.0 :z 2.5}))))
