(ns engine.dig-route-test
  (:require [cljs.test :refer [deftest is testing]]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-route :as dr]))

(defn world
  "block-at over a stone mass below y 10 with the given cells overridden."
  [overrides]
  (fn [[x y z :as c]] (or (overrides c) (if (< y 10) "stone" "air"))))

(deftest a-pit-beside-the-body-needs-no-dig-when-open
  (is (= [] (:dig (dr/route (world {[1 9 0] "air" [1 8 0] "air"}) [0 10 0] [1 8 0] {})))))

(deftest a-sealed-pocket-is-dug-down-in-walking-order
  (let [r (dr/route (world {[3 8 0] "air"}) [0 10 0] [3 8 0] {:margin 3})]
    (is (seq (:dig r)))
    (is (not (some #{[3 8 0]} (:dig r))) "the drop's own cell is open already")
    (is (<= (count (:dig r)) 12))))

(deftest lava-on-the-way-is-not-dug-through
  (let [lava (into {} (for [x (range -5 10) z (range -5 5) y [8 9]] [[x y z] "lava"]))]
    (is (= {:problem :no-route} (dr/route (world (assoc lava [3 8 0] "air")) [0 10 0] [3 8 0] {:margin 3})))))

(deftest a-route-dearer-than-its-bound-is-none
  (let [deep (fn [c] (or ({[30 8 0] "air" [0 20 0] "air" [0 21 0] "air"} c) (if (< (c 1) 20) "stone" "air")))]
    (is (= {:problem :dearer} (dr/route deep [0 20 0] [30 8 0] {:max-s 5 :step-s 0.2 :max-nodes 100000})) "every route left is dearer: the search stops unbounded by any box")
    (is (seq (:dig (dr/route deep [0 20 0] [30 8 0] {:max-s 1000 :margin 3 :max-nodes 100000}))) "within the bound it is found")
    (is (= {:problem :no-route} (dr/route deep [0 20 0] [30 8 0] {:max-dig-s 5 :margin 3 :max-nodes 100000})) "a bound in seconds of digging")))

(deftest a-goal-walled-in-ends-without-a-flood-however-wide-the-wall
  (let [shell (fn [d name] (into {} (for [x (range (- 10 d) (+ 11 d)) y (range (- 10 d) (+ 11 d)) z (range (- d) (inc d))
                                          :when (= d (max (js/Math.abs (- x 10)) (js/Math.abs (- y 10)) (js/Math.abs z)))]
                                      [[x y z] name])))
        open-land (fn [walls] (fn [[_ y _ :as c]] (or (walls c) (if (< y 10) "stone" "air"))))
        opts {:max-s 1000 :step-s 0.3 :max-nodes 2000}]
    (is (= {:problem :no-route} (dr/route (open-land (shell 2 "bedrock")) [-20 10 0] [10 10 0] opts)) "a bedrock shell round the goal")
    (is (= {:problem :no-route} (dr/route (open-land (shell 2 "lava")) [-20 10 0] [10 10 0] opts)) "a lava one")
    (is (= {:problem :no-route} (dr/route (open-land (shell 10 "bedrock")) [-20 10 0] [10 10 0] (assoc opts :max-nodes 10000))) "one ten blocks out: its inside is searched, never the land round it")
    (is (= {:problem :dearer} (dr/route (open-land (shell 2 "obsidian")) [-20 10 0] [10 10 0] (assoc opts :max-s 30 :dig-s #(if (= "obsidian" %) 50 1))))
        "one dearer to dig through than the route's bound")
    (is (= [[8 11 0] [8 10 0]] (:dig (dr/route (open-land (assoc (shell 2 "bedrock") [8 10 0] "stone" [8 11 0] "stone")) [0 10 0] [10 10 0] opts)))
        "stone in the wall is dug through")))

(deftest a-step-down-does-not-dig-above-the-head
  (let [low (fn [c] (or ({[1 9 0] "air" [1 8 0] "air"} c) (if (or (< (c 1) 10) (>= (c 1) 12)) "stone" "air")))]
    (is (= [] (:dig (dr/route low [0 10 0] [1 8 0] {}))))))

(deftest the-fall-limit-is-an-option
  (let [w (world {[1 9 0] "air" [1 8 0] "air" [1 7 0] "air" [1 6 0] "air"})]
    (is (= [] (:dig (dr/route w [0 10 0] [1 6 0] {:max-fall 4}))))
    (is (seq (:dig (dr/route w [0 10 0] [1 6 0] {:max-fall 1 :margin 3}))))))

(deftest digs-are-priced-by-the-callers-seconds-per-block
  (let [hard (into {} (for [y (range 10 14) z (range -3 4)] [[1 y z] "obsidian"]))
        w (fn [c] (or ({[3 10 0] "air" [3 11 0] "air" [-1 10 0] "air" [-1 11 0] "air"} c)
                      (hard c)
                      (if (< (c 1) 10) "stone" "dirt")))]
    (is (some (comp #{"obsidian"} w) (:dig (dr/route w [-1 10 0] [3 10 0] {})))
        "by count the way through the obsidian wall is the shortest")
    (is (not-any? (comp #{"obsidian"} w) (:dig (dr/route w [-1 10 0] [3 10 0] {:dig-s #(if (= "obsidian" %) 50 1) :step-s 0.2})))
        "priced, it digs under the wall")))

(deftest a-fall-is-priced-by-the-callers-seconds
  (let [w (world {[1 9 0] "air" [1 8 0] "air" [1 7 0] "air" [1 6 0] "air"})]
    (is (= [] (:dig (dr/route w [0 10 0] [1 6 0] {:max-fall 4 :fall-s (constantly 0)}))))
    (is (seq (:dig (dr/route w [0 10 0] [1 6 0] {:max-fall 4 :fall-s #(if (> % 3) 100 0) :margin 3}))) "a dear fall is dug round")
    (is (= {:problem :no-route} (dr/route (world {[1 6 0] "air" [1 7 0] "air"}) [0 10 0] [1 6 0] {:max-fall 4 :fall-s #(if (> % 3) 1 0) :max-dig-s 2 :margin 2}))
        "no hole is dug to fall down it at a price in hp")))

(deftest a-pillar-climbs-out-of-a-pit-without-digging-when-blocks-are-carried
  (let [w (world {[0 8 0] "air" [0 9 0] "air"})
        moves #(map :move (:steps %))]
    (is (= [:pillar :walk] (moves (dr/route w [0 8 0] [1 10 0] {:pillar-s 0.5 :step-s 0.2 :margin 2}))))
    (is (= [] (:dig (dr/route w [0 8 0] [1 10 0] {:pillar-s 0.5 :step-s 0.2 :margin 2}))))
    (is (not-any? #{:pillar} (moves (dr/route w [0 8 0] [1 10 0] {:step-s 0.2 :margin 2}))) "no block, no pillar")))

(deftest a-pillar-fn-prices-each-block-by-its-place-in-the-run
  (let [w (world {[0 8 0] "air" [0 9 0] "air" [0 10 0] "air"})
        calls (atom #{})
        pillar-s (fn [n x y z] (swap! calls conj [n x y z]) 0.5)
        r (dr/route w [0 8 0] [0 10 0] {:pillar-s pillar-s :step-s 0.2 :margin 2})]
    (is (= [:pillar :pillar] (map :move (:steps r))))
    (is (contains? @calls [2 0 9 0]) "the second block of the run is priced as the second")))

(deftest a-step-up-lays-its-floor-against-a-face
  (let [w (fn [[_ y _ :as c]] (or ({[2 11 0] "stone"} c) (if (< y 10) "stone" "air")))
        r (dr/route w [0 10 0] [2 12 0] {:place-s 1 :step-s 0.2 :margin 2})]
    (is (= [[:laid [1 10 0]] [:walk nil]] (map (juxt :move :place) (:steps r))))
    (is (= {:problem :no-route} (dr/route w [0 10 0] [2 12 0] {:step-s 0.2 :margin 2})) "without a block the step has no floor")))

(deftest a-floor-of-the-bodys-own-stair-is-neither-dug-nor-built-on
  (let [w (world {[3 8 0] "air"})
        straight (dr/route w [0 10 0] [3 8 0] {:margin 3})
        way (set (take 1 (:dig straight)))]
    (is (not-any? way (:dig (dr/route w [0 10 0] [3 8 0] {:margin 3 :ways way}))))
    (is (not-any? #{[0 8 0]} (map :place (:steps (dr/route (world {[0 8 0] "air" [0 9 0] "air"}) [0 8 0] [1 10 0] {:pillar-s 0.5 :step-s 0.2 :margin 2 :ways #{[0 7 0]}}))))
        "the pit's floor is a step of its own stair: no block is put on it")))

(deftest a-cell-under-sand-or-gravel-is-not-dug-and-the-route-goes-round
  (let [at (fn [roof] (fn [[x y z :as c]] (or (get roof c) (if (< y 12) "stone" "air"))))
        roof {[1 11 0] "gravel" [2 11 0] "sand" [3 11 0] "gravel"}
        free {[0 9 0] "air" [0 10 0] "air" [3 9 0] "air" [3 10 0] "air"}
        opened (fn [r] (set (:dig r)))]
    (is (= {:problem :no-route} (dr/route (at (merge roof free)) [0 9 0] [3 9 0] {:margin 0})) "the straight line is all under falling blocks")
    (is (not-any? (comp #{[1 10 0] [2 10 0]} identity) (opened (dr/route (at (merge roof free)) [0 9 0] [3 9 0] {:margin 2}))) "the detour opens nothing under them")))

(deftest a-cell-is-priced-once-however-many-steps-reach-it
  (let [calls (atom [])
        dig-s (fn [n cell] (swap! calls conj [n cell]) 1)]
    (dr/route (world {[8 8 8] "air"}) [0 10 0] [8 8 8] {:dig-s dig-s :step-s 0.5})
    (is (= (count @calls) (count (set @calls))))))

(deftest a-walker-passes-a-gate-and-a-plant-but-not-a-fence-or-wall
  (is (every? dr/open? ["air" "oak_fence_gate" "oak_sapling" "poppy"]))
  (is (not-any? dr/open? ["oak_fence" "cobblestone_wall" "stone" nil])))

(deftest a-far-search-stops-at-the-node-budget
  (let [goal [76 52 96]
        reads (atom #{})
        w (fn [c] (swap! reads conj c) (cond (#{goal (update goal 1 inc)} c) "air" (< (c 1) 64) "stone" :else "air"))]
    (is (= {:problem :budget} (dr/route w [0 64 0] goal {:dig-s (constantly 0.5) :step-s 0.2 :max-nodes 500})))
    (is (< (count @reads) 12000) "cells read: about a dozen a node expanded, by both searches, not the whole box between")))

(deftest a-search-run-in-slices-ends-as-one-run-does
  (let [w (world {[20 4 6] "air" [20 5 6] "air"})
        opts {:dig-s (constantly 0.5) :step-s 0.2}
        run (dr/search w [0 10 0] [20 4 6] opts)
        slices (take-while #(nil? (first %)) (repeatedly #(run 40)))]
    (is (every? #(= 40 (second %)) slices) "a slice expands no more than it is given")
    (is (< 3 (count slices)) "the search spans slices")
    (is (seq (:dig (first (run 40)))))
    (is (= (dr/route w [0 10 0] [20 4 6] (assoc opts :max-nodes 100000)) (first (run 40))))))

(deftest a-search-that-spends-its-nodes-goes-on-at-the-next-call-with-its-key
  (let [w (world {[20 4 6] "air" [20 5 6] "air"})
        opts {:dig-s (constantly 0.5) :step-s 0.2 :max-nodes 40}
        asks (loop [rs []] (let [r (dr/route-resumed [:t 1] w [0 10 0] [20 4 6] opts)] (if (and (= {:problem :searching} r) (< (count rs) 300)) (recur (conj rs r)) (conj rs r))))
        done (peek asks)]
    (is (< 3 (count asks)) "each call is one slice")
    (is (= (dr/route w [0 10 0] [20 4 6] (assoc opts :max-nodes 100000)) done))))

(deftest a-resumed-search-past-a-dear-wall-prices-the-wall-first-and-keeps-one-search-a-caller
  ;; the goal in an obsidian shell in rock: every way digs obsidian, and a search that learns it only on reaching the
  ;; shell searches the ball of rock cheaper than that first
  (let [shell (into {} (for [x (range -1 2) y (range 4 7) z (range 5 8) :when (not= [x y z] [0 5 6])] [[x y z] "obsidian"]))
        w (world (assoc shell [0 5 6] "air"))
        opts {:dig-s (fn [n _] (if (= "obsidian" n) 50 1)) :step-s 0.3 :max-s 1000}
        to-end (fn [run] (loop [cells 0] (let [[a e] (run 500)] (if a [a (+ cells e)] (recur (+ cells e))))))
        [exact exact-cells] (to-end (dr/search w [0 10 0] [0 5 6] opts))
        _ (.clear dr/kept)
        [priced priced-cells] (to-end (dr/priced-search w [0 10 0] [0 5 6] opts))]
    (is (<= (:seconds priced) (+ (:seconds exact) (* (cost/search-cell-s) priced-cells)))
        (str "dearer than the cheapest by at most the CPU it spent (" priced-cells " cells, the exact search " exact-cells ")"))
    (is (< (* 2 priced-cells) exact-cells) "for less than half the cells")
    (doseq [i (range 20)] (dr/route-resumed [:t 2 i] w [0 10 0] [0 5 6] (assoc opts :max-nodes 1)))
    (is (= 1 (.-size dr/kept)) "round after round, each new question of the caller drops its old search")
    (.clear dr/kept)
    (with-redefs [dr/heap-full? (constantly true)]
      (is (= {:problem :too-big} (dr/route-resumed [:t 3] w [0 10 0] [0 5 6] opts)) "a full heap grows no search"))
    (.clear dr/kept)))

(deftest a-steps-guessed-cells-over-the-head-it-leaves-and-under-a-laid-block-are-priced
  (let [flat (world {})
        up (fn [unseen] (dr/route (world {[2 10 0] "stone"}) [0 10 0] [2 11 0] {:step-s 0.2 :unseen? unseen :unseen-step-s 5 :margin 2}))
        laid (fn [unseen] (dr/route (fn [c] (or ({[2 11 0] "stone"} c) (flat c))) [0 10 0] [2 12 0]
                                    {:step-s 0.2 :place-s 1 :unseen? unseen :unseen-step-s 5 :margin 2}))]
    (is (= [1 10 0] (:to (last (butlast (:steps (up #{})))))))
    (is (not= [1 10 0] (:to (last (butlast (:steps (up #{[1 12 0]})))))) "the cell over the head of the step up is a guess")
    (is (some #{[1 10 0]} (map :place (:steps (laid #{})))))
    (is (not-any? #{[1 10 0]} (map :place (:steps (laid #{[1 9 0]})))) "the only face of the laid block is a guess")))

(deftest a-stair-that-turns-back-never-stands-on-a-floor-it-dug-away
  (let [block-at (fn [[x y z]] (if (and (zero? x) (zero? z) (= y 10)) "air" (if (< y 30) "stone" "air")))]
    (doseq [goal [[0 13 0] [0 14 0] [0 15 0] [0 16 0]]
            :let [r (dr/route block-at [0 10 0] goal {:step-s 0.2 :margin 2 :max-nodes 100000})
                  stood (reductions (fn [dug s] (into dug (:dig s))) #{} (:steps r))]]
      (is (seq (:steps r)) "a route is found")
      (is (every? true? (map (fn [dug {[x y z] :to}] (not (dug [x (dec y) z]))) stood (:steps r)))))))

(deftest a-range-search-seeds-its-back-search-from-the-end-cells-nearest-the-start-only
  (let [reads (atom #{})
        at (world {})
        r (dr/route (fn [c] (swap! reads conj c) (at c)) [0 10 0] [20 10 0] {:range 8 :step-s 1})]
    (is (= [] (:dig r)))
    (is (not-any? (fn [[x]] (> x 22)) @reads) "end cells beyond the goal cannot be on the cheapest way: unread")))

(deftest a-range-search-asks-sees-only-for-the-end-cells-its-back-search-comes-to
  (let [asked (atom 0)
        r (dr/route (world {}) [0 10 0] [3 10 0] {:range 8 :step-s 1 :sees (fn [_ _ _] (swap! asked inc) true)})]
    (is (= [] (:dig r)))
    (is (< @asked (* 17 17 17)) "not every cell of the cube is asked")
    (is (< @asked 200))))

(deftest a-range-search-ends-dearer-and-with-the-same-path-as-before
  (let [at (world {})
        far {:range 4 :step-s 1}]
    (is (= {:problem :dearer} (dr/route at [0 10 0] [20 10 0] (assoc far :max-s 5))) "every way past the range is dearer")
    (is (= 16 (count (:steps (dr/route at [0 10 0] [20 10 0] (assoc far :max-s 100))))) "ends at the first cell within range of the goal")))

(deftest a-cell-is-dug-at-its-share-of-the-price
  (let [prices (dr/by-share {:dig-s (fn [name _] (when (= "stone" name) 10))} #(if (= [0 0 0] %) 0.25 1))]
    (is (= 2.5 ((:dig-s prices) "stone" [0 0 0])))
    (is (= 10 ((:dig-s prices) "stone" [1 0 0])))
    (is (nil? ((:dig-s prices) "bedrock" [0 0 0])) "a cell never dug stays so")))

(defn to-end
  "Run a search on in slices of 500 cells until it answers or has expanded 100000: [answer cells]."
  [run]
  (loop [cells 0] (let [[a e] (run 500) cells (+ cells e)] (if (or a (> cells 100000)) [a cells] (recur cells)))))

(defn unseen-opts
  "Prices over rock below y 10 never seen but for the cells of seen (a fn of a cell; never seen taken for stone): seen-steps the fewest steps to a cell
  whose step is seen, seen-cells the fewest cells to one seen (the air over y 10 or seen's)."
  [seen seen-steps seen-cells max-s]
  {:dig-s (fn [n _] (if (= "glass" n) 13 0.5)) :step-s 0.3 :unseen-step-s 1 :unseen-dig-s 0.5 :max-s max-s
   :seen-steps seen-steps :seen-cells (fn [x y z] (min (max 0 (- 10 y)) (seen-cells x y z)))
   :unseen? (fn [[_ y _ :as c]] (and (< y 10) (not (seen c)))) :guess (constantly "stone")})

(deftest a-priced-search-toward-a-far-goal-through-unseen-rock-prices-the-rock-and-searches-no-ball-of-it
  ;; rock below y 10 never seen (taken for stone): the walk's price alone (step-s a block) leaves a ball of rock open that a
  ;; way through the unseen costs far more than. No step is seen (the land's floor never was) but in the glass plane.
  (let [glass (into {} (for [z (range -300 301) y (range 0 10)] [[30 y z] "glass"]))
        none (constantly ##Inf)
        to-glass (fn [x _ _] (js/Math.abs (- x 30)))
        [far far-cells] (to-end (dr/priced-search (world {}) [0 10 0] [300 4 0] (unseen-opts {} none none 208)))
        [wall wall-cells] (to-end (dr/priced-search (world glass) [0 10 0] [60 4 0] (unseen-opts glass to-glass to-glass 208)))
        [deep deep-cells] (to-end (dr/priced-search (world {}) [0 10 0] [40 -60 10] (unseen-opts {} none none 208)))]
    (is (= {:problem :dearer} far))
    (is (< far-cells 100) (str "a goal 300 blocks off through unseen rock is dearer than 208 s at once (" far-cells " cells)"))
    (is (< (:seconds wall) 90) "the way over a glass plane 600 wide is found")
    (is (< wall-cells 100) (str wall-cells " cells"))
    (is (< (:seconds deep) 130))
    (is (< deep-cells 400) (str "a goal 70 blocks down (" deep-cells " cells)"))))

(defn same-price?
  "Whether search prices each [from goal opts] of rows the same with the unseen-priced bound, bounded by that price (a
  bound over the way's price anywhere on it ends :dearer), as with the walk bound alone, over the cells of seen (never seen
  below y 10 but for them), seen-steps and seen-cells measured to its cells: [row ok?]."
  [seen rows]
  (let [rho (fn [mf] (fn [x y z] (reduce min ##Inf (for [[[a b c]] seen] (max (+ (js/Math.abs (- x a)) (js/Math.abs (- z c))) (js/Math.ceil (/ (js/Math.abs (- y b)) mf)))))))
        price (fn [from goal opts prior?] (:seconds (first ((dr/search (world seen) from goal (assoc opts :prior-h? prior?)) 100000))))]
    (for [[from goal extra] rows
          :let [opts (merge (assoc (unseen-opts seen (memoize (rho 3)) (memoize (rho 1)) 1000) :max-fall 3 :margin 3) extra)
                walk (price from goal opts false)]]
      [[from goal] (and (number? walk) (< (js/Math.abs (- walk (price from goal (assoc opts :max-s (+ walk 1e-6)) true))) 1e-9))])))

(defn stand
  "A seen 2-high stand at feet cell: its floor stone, feet and head air."
  [[x y z]]
  {[x (dec y) z] "stone" [x y z] "air" [x (inc y) z] "air"})

(deftest the-unseen-priced-bound-finds-the-way-the-walk-bound-alone-finds
  ;; seen tunnels and seen rock in unseen stone, ways up, down, across, by pillar and to a range: a bound over any way's
  ;; price, or one that rose more than a step's price from cell to cell, would end on a dearer way
  (let [seen (into {} (concat (for [x (range -2 9) z [3]] [[x 4 z] "air"])
                              (for [x [6] y (range -2 4) z (range 0 4)] [[x y z] "air"])
                              (for [x (range -3 3) y [0 1] z [-4]] [[x y z] "air"])
                              (for [x (range 1 5) y [6] z (range -3 0)] [[x y z] "stone"])))]
    (is (= [] (remove second (same-price? seen [[[0 10 0] [6 -2 0]]
                                               [[0 10 0] [12 -14 5]]
                                               [[0 10 0] [-1 -16 0]]
                                               [[-20 10 0] [-30 -6 0]]
                                               [[0 10 0] [-3 0 -4]]
                                               [[6 -2 1] [0 10 0]]
                                               [[6 -2 1] [0 10 0] {:pillar-s 0.4 :place-s 0.4}]
                                               [[-3 0 -4] [8 4 3]]
                                               [[0 10 0] [2 -1 -6] {:range 1 :near [2 -1 -6]}]
                                               [[-2 4 3] [4 5 -2] {:pillar-s 0.4}]]))))))

(deftest the-unseen-priced-bound-prices-seen-steps-at-a-ways-start-end-or-both-as-the-walk-bound-alone
  ;; seen steps (floor, feet and head seen) at the start, the end or both, so the unseen steps before the first seen one and
  ;; after the last are added
  (let [floored (merge (into {} (mapcat #(stand [% 4 6]) (range -2 11)))
                       (into {} (for [x (range 12 17) z (range -2 3)] [[x -3 z] "stone"]))
                       (into {} (for [x (range 12 17) y [-2 -1] z (range -2 3)] [[x y z] "air"]))
                       {[3 -4 -6] "stone"}
                       (stand [-6 -10 0]) {[-6 -8 0] "stone"})]
    (is (= [] (remove second (same-price? floored [[[0 10 0] [8 4 6]]
                                                  [[-2 4 6] [14 -2 0]]
                                                  [[14 -2 0] [0 10 0]]
                                                  [[14 -2 0] [-2 4 6]]
                                                  [[0 10 0] [3 -7 -6]]
                                                  [[0 10 0] [3 -8 -6]]
                                                  [[-6 -10 0] [-6 2 0] {:pillar-s 0.4 :place-s 0.4}]
                                                  [[-6 -10 0] [8 4 6] {:pillar-s 0.4}]
                                                  [[14 -2 0] [8 4 6] {:range 1 :near [8 4 6]}]]))))
    ;; digs free, so only the unseen steps' count is left to price the way at all
    (is (= [] (remove second (same-price? floored (for [[from goal] [[[0 10 0] [8 4 6]] [[-2 4 6] [14 -2 0]] [[14 -2 0] [0 10 0]]
                                                                     [[0 10 0] [3 -8 -6]] [[-6 -10 0] [8 4 6]] [[8 4 6] [-6 -10 0]]]]
                                                    [from goal {:dig-s (constantly 0) :unseen-dig-s 0}])))))))

(deftest a-resumed-search-reads-the-memory-its-bound-was-made-from
  ;; a tunnel seen while the search goes on, where its bound (made at the start) priced unseen rock: read as seen, its cells
  ;; would cost less than the bound says, and the search could end on a dearer way
  (let [mem (atom {})
        w (fn [[_ y _ :as c]] (or (@mem c) (if (< y 10) "stone" "air")))
        ;; the memory's cells as the search's first call finds them, as seen-sections/steps-fn measures
        opts (fn [] (let [snap @mem
                          to (fn [mf] (memoize (fn [x y z] (reduce min ##Inf (for [[[a b c]] snap] (max (+ (js/Math.abs (- x a)) (js/Math.abs (- z c))) (js/Math.ceil (/ (js/Math.abs (- y b)) mf))))))))]
                      (assoc (unseen-opts #(@mem %) (to 3) (to 1) 1000) :max-fall 3 :max-nodes 3)))
        tunnel (into {} (for [x (range 1 21) y [3 4 5]] [[x y 0] (if (= 3 y) "stone" "air")]))
        resumed (fn [k] (loop [i 0] (let [r (dr/route-resumed k w [0 10 0] [20 4 0] (opts))]
                                      (when (zero? i) (reset! mem tunnel))
                                      (if (and (= {:problem :searching} r) (< i 1000)) (recur (inc i)) [r i]))))
        before (first ((dr/priced-search w [0 10 0] [20 4 0] (opts)) 100000))
        during (resumed [:t 4])
        after (resumed [:t 5])]
    (.clear dr/kept)
    (is (< 2 (second during)) "the search spans calls")
    (is (= (select-keys before [:seconds :dig]) (select-keys (first during) [:seconds :dig])) "the way the memory at its start gives")
    (is (< (:seconds (first after)) (:seconds before)) "a search begun once the tunnel is seen takes it")))

(deftest a-seen-cave-past-unseen-rock-is-priced-as-seen
  ;; a seen cave 60 long (its floor and walls seen) behind 5 blocks of unseen rock: priced as if the rest were unseen
  ;; past the first unseen step, every way would cost over 80 s
  (let [cave (into {} (for [x (range 5 66) y (range 3 7) z (range -1 2)]
                        [[x y z] (if (and (<= 4 y 5) (zero? z)) "air" "stone")]))
        gap (fn [a lo hi] (max 0 (- lo a) (- a hi)))
        steps (fn [x _ z] (+ (gap x 5 65) (js/Math.abs z)))
        cells (fn [x y z] (max (+ (gap x 5 65) (gap z -1 1)) (gap y 3 6)))
        [r n] (to-end (dr/priced-search (world cave) [0 10 0] [65 4 0] (unseen-opts cave steps cells 60)))]
    (is (< (:seconds r) 60) (str "the way down into the cave and along it is found under the worth: " r))
    (is (< n 5000) (str n " cells"))))
