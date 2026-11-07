(ns engine.dig-route-test
  (:require [cljs.test :refer [deftest is testing]]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.lib.dig-route :as dr]
            [jobs.lib.seen-sections :as seen-sections]))

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

(deftest a-cut-under-or-beside-lava-is-not-dug
  (let [lava (into {} (for [x (range 3 6) z (range -3 4) y [8 9]] [[x y z] "lava"]))
        w (world (assoc lava [8 7 0] "air" [8 8 0] "air"))
        r (dr/route w [0 10 0] [8 7 0] {:margin 6 :max-nodes 100000})
        next-to-lava? (fn [[x y z]] (some #(= "lava" (w (mapv + [x y z] %))) [[0 1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1]]))]
    (is (seq (:dig r)) "the way round or under it is found")
    (is (not-any? next-to-lava? (:dig r)) "no cell dug has lava over or beside it")))

(deftest a-cut-beside-lava-is-dug-when-its-seal-costs-less-than-the-way-round
  (let [wall (merge (into {} (for [z (range -5 6) y [10 11]] [[3 y z] "stone"])) {[3 10 1] "lava" [3 11 1] "lava"})
        w (world wall)
        route (fn [lava-s] (:dig (dr/route w [0 10 0] [6 10 0] (cond-> {:margin 8 :step-s 0.3 :max-nodes 100000} lava-s (assoc :lava-s lava-s)))))
        cut? (fn [dig] (some #(and (= 3 (% 0)) (= 0 (% 2))) dig))]
    (is (cut? (route (fn [faces] (* faces 0.01)))) "sealed at a small price, the cut at z 0 is taken")
    (is (not (cut? (route (fn [faces] (* faces 1000))))) "a dear seal sends the route round the wall")
    (is (not (cut? (route nil))) "with no seal priced the cut is refused")))

(deftest a-route-dearer-than-its-bound-is-none
  (let [deep (fn [c] (or ({[30 8 0] "air" [0 20 0] "air" [0 21 0] "air"} c) (if (< (c 1) 20) "stone" "air")))]
    (is (= {:problem :dearer} (dr/route deep [0 20 0] [30 8 0] {:max-s 5 :step-s 0.2 :max-nodes 100000})) "every route left is dearer: the search stops unbounded by any box")
    (is (seq (:dig (dr/route deep [0 20 0] [30 8 0] {:max-s 1000 :margin 3 :max-nodes 100000}))) "within the bound it is found")
    (let [run (dr/search deep [0 20 0] [30 8 0] {:max-s 1000 :margin 3})]
      (run 10)
      (is (= {:problem :dearer} (first (run 100000 5))) "a search begun under a larger bound goes on under the lower one it is handed"))
    (is (= {:problem :no-route :boxed true :searched-s 0} (dr/route deep [0 20 0] [30 8 0] {:max-dig-s 5 :margin 3 :max-nodes 100000})) "a bound in seconds of digging")))

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

;; every node of a route stands on a floor: with no block to lay, the goal's only way in is a fall down the open air over
;; it, which no floor beside leads into; the rock and air round it are endless (bedrock is the sky's limit), so only
;; that proof ends the search
(deftest a-goal-on-a-block-hung-in-open-air-is-no-route-without-a-block-to-lay
  (let [w (fn [[_ y _ :as c]] (cond (= c [10 20 0]) "stone" (> y 40) "bedrock" (< y 10) "stone" :else "air"))]
    (is (= {:problem :no-route} (dr/route w [0 10 0] [10 21 0] {:max-s 1000 :step-s 0.3 :max-nodes 20000})))))

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
    (is (= {:problem :no-route :boxed true :searched-s 0} (dr/route (world {[1 6 0] "air" [1 7 0] "air"}) [0 10 0] [1 6 0] {:max-fall 4 :fall-s #(if (> % 3) 1 0) :max-dig-s 2 :margin 2}))
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

(defn walls
  "block-at over bedrock with the given cells air."
  [open]
  (fn [c] (if (contains? open c) "air" "bedrock")))

;; a laid block up over the pit at x 1 reaches [2 11 0] cheapest (2.5 s); the stair round by z 1 (4 s) spends none, and
;; the goal is reached only by laying one more from there
(deftest a-cheaper-way-to-a-cell-that-placed-the-last-block-hides-no-way-that-did-not
  (let [w (walls #{[0 10 0] [0 11 0] [0 12 0] [1 9 0] [1 10 0] [1 11 0] [1 12 0] [2 11 0] [2 12 0] [2 13 0] [3 11 0] [3 12 0] [3 13 0]
                   [0 11 1] [0 12 1] [1 11 1] [1 12 1] [2 11 1] [2 12 1]})
        opts {:step-s 1 :place-s 0.5 :margin 3}]
    (is (= 4 (:seconds (dr/route w [0 10 0] [3 12 0] opts))) "two blocks carried: laid twice")
    (is (= 5.5 (:seconds (dr/route w [0 10 0] [3 12 0] (assoc opts :max-places 1)))) "one: round by the stair, then laid")))

;; a pillar's third block is dear: the cheapest way to the shaft's [0 12 0] is its first two (0.2 s), the dearer a laid
;; step in from the ledge at x 1 (2.5 s), after which the goal's block is the first of its run
(deftest a-cheaper-way-to-a-cell-up-a-longer-pillar-run-hides-no-way-up-a-shorter
  (let [w (walls #{[0 10 0] [0 11 0] [0 12 0] [0 13 0] [0 14 0] [1 11 0] [1 12 0] [1 13 0]})
        r (dr/route w [0 10 0] [0 13 0] {:step-s 1 :place-s 0.5 :pillar-s (fn [n _ _ _] (if (< n 3) 0.1 100)) :margin 3})]
    (is (= [:walk :laid :pillar] (map :move (:steps r))))
    (is (< (js/Math.abs (- 2.6 (:seconds r))) 1e-9))))

(deftest a-step-up-lays-its-floor-against-a-face
  (let [w (fn [[_ y _ :as c]] (or ({[2 11 0] "stone"} c) (if (< y 10) "stone" "air")))
        r (dr/route w [0 10 0] [2 12 0] {:place-s 1 :step-s 0.2 :margin 2})]
    (is (= [[:laid [1 10 0]] [:walk nil]] (map (juxt :move :place) (:steps r))))
    (is (= {:problem :no-route :boxed true :searched-s 0} (dr/route w [0 10 0] [2 12 0] {:step-s 0.2 :margin 2})) "without a block the step has no floor")))

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
    (is (= {:problem :no-route :boxed true :searched-s 0} (dr/route (at (merge roof free)) [0 9 0] [3 9 0] {:margin 0})) "the straight line is all under falling blocks")
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
    (is (< (count @reads) 20000) "cells read: about a dozen a node expanded, by both searches, not the whole box (400000 cells) between")))

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
    (is (= (dr/route w [0 10 0] [20 4 6] (assoc opts :max-nodes 100000)) (dissoc done :searched-s)) "the same route; the resumed search also reports its CPU")))

(deftest a-resumed-search-whose-read-cell-changed-meanwhile-begins-again
  (let [changes (atom {})
        w0 (world {[20 4 6] "air" [20 5 6] "air"})
        w (fn [c] (or (@changes c) (w0 c)))
        opts {:dig-s (constantly 0.5) :step-s 0.2 :max-nodes 40}
        ask #(dr/route-resumed [:t 4] w [0 10 0] [20 4 6] opts)
        whole (dr/route w [0 10 0] [20 4 6] (assoc opts :max-nodes 100000))
        lavaed (first (:dig whole))
        _ (.clear dr/kept)
        _ (ask)
        _ (swap! changes assoc lavaed "lava")
        end (loop [i 0] (let [r (ask)] (if (and (= {:problem :searching} r) (< i 300)) (recur (inc i)) r)))]
    (is (some? lavaed))
    (is (seq (:dig end)))
    (is (not-any? #{lavaed} (:dig end)) "the route is found again without the cell seen as lava")))

(deftest a-resumed-search-past-a-dear-wall-prices-the-wall-first-and-keeps-one-search-a-caller
  ;; the goal in an obsidian shell in rock: every way digs obsidian, and a search that learns it only on reaching the
  ;; shell searches the ball of rock cheaper than that first
  (let [shell (into {} (for [x (range -1 2) y (range 4 7) z (range 5 8) :when (not= [x y z] [0 5 6])] [[x y z] "obsidian"]))
        w (world (assoc shell [0 5 6] "air"))
        opts {:dig-s (fn [n _] (if (= "obsidian" n) 50 1)) :step-s 0.3 :max-s 1000}
        to-end (fn [run] (loop [cells 0 reads 0] (let [[a e _ rd] (run 500)] (if a [a (+ cells e) (+ reads rd)] (recur (+ cells e) (+ reads rd))))))
        [exact exact-cells] (to-end (dr/search w [0 10 0] [0 5 6] opts))
        _ (.clear dr/kept)
        [priced priced-cells reads] (to-end (dr/priced-search w [0 10 0] [0 5 6] opts))]
    ;; the back search and each expansion's neighbours are read too: the CPU goes by the cells read
    (is (= (:searched-s priced) (* (cost/search-read-s) reads)) "its CPU priced by the cells it read")
    (is (< priced-cells reads) "more than it expanded")
    (is (<= (:seconds priced) (+ (:seconds exact) (:searched-s priced)))
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
    (is (nil? ((:dig-s prices) "bedrock" [0 0 0])) "a cell never dug stays so")
    (is (zero? ((:dig-s (dr/by-share {:dig-s (constantly ##Inf)} (constantly 0))) "stone" [0 0 0])) "a cell sure to be open costs nothing, however dear its dig")))

(defn to-end
  "Run a search on in slices of 500 cells until it answers or has expanded 100000: [answer cells reads]."
  [run]
  (loop [cells 0 reads 0] (let [[a e _ r] (run 500) cells (+ cells e) reads (+ reads r)] (if (or a (> cells 100000)) [a cells reads] (recur cells reads)))))

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
        [deep deep-cells] (to-end (dr/priced-search (world {}) [0 10 0] [40 -60 10] (unseen-opts {} none none 208)))
        ;; every way over the land's top to the far corner bounds alike: summed in other orders, some bounds came out a bit lower
        [diag diag-cells] (to-end (dr/priced-search (world {}) [0 10 0] [150 4 150] (unseen-opts {} none none 600)))
        ;; a fall past 3 costs hp, so one through rock is dug 3 at most
        [falls falls-cells falls-reads] (to-end (dr/priced-search (world {}) [0 10 0] [40 -60 10] (assoc (unseen-opts {} none none 400) :max-fall 16 :fall-s (fn [dy] (if (> dy 3) (* 2 (- dy 3)) 0)))))]
    (is (= {:problem :dearer} far))
    (is (< far-cells 100) (str "a goal 300 blocks off through unseen rock is dearer than 208 s at once (" far-cells " cells)"))
    (is (< (:seconds wall) 90) "the way over a glass plane 600 wide is found")
    (is (< wall-cells 100) (str wall-cells " cells"))
    (is (< (:seconds deep) 130))
    (is (< deep-cells 400) (str "a goal 70 blocks down (" deep-cells " cells)"))
    (is (< (:seconds diag) 400))
    (is (< diag-cells 1000) (str "a goal 150 off on each axis (" diag-cells " cells)"))
    (is (< (:seconds falls) 130))
    (is (< falls-cells 5000) (str "a goal 70 blocks down, falls past 3 priced (" falls-cells " cells)"))
    ;; the back search's bound to the start prices the unseen cells a way down to them opens: without, 22831 cells read
    (is (< falls-reads 16000) (str falls-reads " cells read"))))

(defn steps-to-seen
  "A fn [x y z] of the fewest steps, falls of at most mf, from the cell to one of seen's."
  [seen mf]
  (fn [x y z] (reduce min ##Inf (for [[[a b c]] seen] (max (+ (js/Math.abs (- x a)) (js/Math.abs (- z c))) (js/Math.ceil (/ (js/Math.abs (- y b)) mf)))))))

(defn same-price?
  "Whether search prices each [from goal opts] of rows the same with the unseen-priced bound, bounded by that price (a
  bound over the way's price anywhere on it ends :dearer), as with the walk bound alone, over the cells of seen (never seen
  below y 10 but for them), seen-steps and seen-cells measured to its cells: [row ok?]."
  [seen rows]
  (let [rho #(steps-to-seen seen %)
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
                                               [[-2 4 3] [4 5 -2] {:pillar-s 0.4}]]))))
    (is (= [] (remove second (same-price? seen (for [[from goal] [[[0 10 0] [6 -2 0]] [[0 10 0] [12 -14 5]] [[0 10 0] [-1 -16 0]] [[6 -2 1] [0 10 0]] [[-3 0 -4] [8 4 3]]]]
                                                [from goal {:fall-s (fn [dy] (if (> dy 1) dy 0))}]))))
        "falls past 1 priced, so a dug fall is 1 at most")
    (is (= [] (remove second (same-price? seen (for [[from goal] [[[0 10 0] [6 -2 0]] [[0 10 0] [12 -14 5]] [[-2 4 3] [6 -2 1]] [[6 -2 1] [0 10 0]] [[-3 0 -4] [8 4 3]]]]
                                                [from goal {:max-fall 6 :fall-s (fn [dy] (if (> dy 1) dy 0)) :seen-steps (memoize (steps-to-seen seen 1))}]))))
        "seen steps counted with falls of 1, the longest free one, though a fall of 6 down the seen shaft is taken")))

(deftest a-bodys-seen-steps-count-its-falls-through-the-unseen-at-the-longest-that-costs-no-hp
  ;; a longer fall is taken only down cells seen open, so before the way meets the memory it falls no farther
  (let [w (tu/fake-on-floor {})
        per (perception/create (fake-raw/create w) {:radius 10 :ray-deg 2 :now (constantly 1000000) :rand (constantly 0.5)})
        w (perception/wrap w per)
        _ (perception/pass! per)
        {:keys [seen-steps max-fall fall-s]} (dr/body-prices w (constantly false))]
    (is (< 3 max-fall) "the body may spend hp on a fall")
    (is (= [true true true false] (mapv (comp zero? fall-s) [1 2 3 4])) "falls to 3 cost no hp")
    (is (= ((seen-sections/steps-fn w 3) 0 20 0) (seen-steps 0 20 0)))
    (is (< ((seen-sections/steps-fn w max-fall) 0 20 0) (seen-steps 0 20 0)))))

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

(deftest a-search-reads-the-same-cells-wherever-it-stands-in-y
  (let [reads (fn [dy] (let [seen (atom #{})
                             w (fn [[x y z]] (swap! seen conj [x y z]) (if (< y (+ 10 dy)) "stone" "air"))
                             r (dr/route w [0 (+ 10 dy) 0] [20 (+ 4 dy) 6] {:dig-s (constantly 0.5) :step-s 0.2 :max-nodes 100000})]
                         [(count @seen) (:dig r)]))
        [n0 d0] (reads 0)
        [n1 d1] (reads 2048)]
    (is (= n0 n1) "cells 1024 apart in y are different cells: no read is a cache hit for another")
    (is (= (map (fn [[x y z]] [x (- y 2048) z]) d1) d0))))

(deftest a-route-past-512-blocks-from-the-start-names-its-cells-as-a-near-one-does
  ;; a cell's key is a small integer within 512 of the start, another packing past it: the wall's cells (the way over it
  ;; digs one more) and the steps past the seam read back as the cells they are
  (let [wall (fn [x dx] (into {} (map #(vector % "stone")) [[x 10 0] [x 11 0] [x 12 0] [(- x dx) 12 0]]))
        opts {:dig-s (constantly 0.1) :step-s 1 :max-nodes 100000}
        r (dr/route (world (wall 550 1)) [0 10 0] [600 10 0] opts)
        back (dr/route (world (wall -600 -1)) [0 10 0] [-700 10 0] opts)]
    (is (= [[550 11 0] [550 10 0]] (:dig r)))
    (is (= (map #(vector % 10 0) (range 1 601)) (map :to (:steps r))))
    (is (< (js/Math.abs (- 600.2 (:seconds r))) 1e-9))
    (is (= [[-600 11 0] [-600 10 0]] (:dig back)))
    (is (= [-700 10 0] (:to (peek (:steps back)))))))

(deftest the-back-search-does-not-climb-open-air-above-the-start
  (let [top (atom 0)
        w (fn [[_ y _]] (swap! top max y) (if (< y 10) "stone" "air"))]
    (dr/route w [0 10 0] [20 4 6] {:dig-s (constantly 0.5) :step-s 0.2 :max-nodes 100000})
    (is (< @top 60) "a cell above the start costs a climb each block to reach, so the air over it is read only as far as the route's price allows, not for ever")))

(deftest a-dearer-dry-way-to-a-cell-is-kept-beside-a-cheaper-one-that-let-water-in
  ;; card 9a274838: one label a cell lost the dry detour to the cut under the pool, and the stair on was dug wet
  (let [open (into #{} (concat [[0 10 0] [0 11 0] [2 10 0] [2 11 0] [1 10 3] [1 11 3]]
                               (for [x [0 2] y [10 11] z [1 2 3]] [x y z])))
        stone (into #{[1 10 0] [1 11 0]} (for [x (range 3 7) y (range 6 12)] [x y 0]))
        w (fn [c] (cond (open c) "air" (stone c) "stone" (= c [1 10 -1]) "water" :else "bedrock"))
        flood {:level (fn [c] (when (= c [1 10 0]) 10))
               :open (delay [1 0])
               :dig (fn [under _] [100 (inc under)])
               :step (fn [under] [0 under])}
        r (dr/route w [0 10 0] [6 7 0] {:step-s 1 :flood flood :max-nodes 100000})]
    (is (< (:seconds r) 100) "the stair is dug dry")
    (is (not-any? #{[1 10 0] [1 11 0]} (:dig r)) "the way round the pool is walked, its cut left shut")))

(deftest a-dearer-flooded-way-with-lower-water-is-kept-beside-a-cheaper-one
  ;; card 4abc7f55: two cuts beside pools reach the cell [2 10 0]: the short one lets water in to 11 and leaves the head
  ;; under it, the detour's to 9. The cut on at y 10 and 11 is made under the first's water (drowning) and over the
  ;; second's; one wet label a cell kept only the cheaper
  (let [open (into #{} (for [[x z] [[0 0] [0 1] [0 2] [2 2] [2 1] [2 0] [4 0]] y [10 11]] [x y z]))
        stone #{[1 10 0] [1 11 0] [1 10 2] [1 11 2] [3 10 0] [3 11 0]}
        water #{[1 10 -1] [1 11 -1] [1 10 3] [1 11 3]}
        w (fn [c] (cond (open c) "air" (stone c) "stone" (water c) "water" :else "bedrock"))
        flood {:level (fn [[x _ z]] (when (= x 1) ({0 11 2 9} z)))
               :open (delay [1 0])
               :dig (fn [under _] [(if (pos? under) ##Inf 1) (+ under 5)])
               :step (fn [under] [0 under])}
        r (dr/route w [0 10 0] [4 10 0] {:step-s 1 :flood flood :max-nodes 100000})]
    (is (< (:seconds r) ##Inf) (str "the cut past the meeting cell is made over the detour's water: " r))
    (is (some #{[1 10 2]} (:dig r)) "the detour's cut, not the short one's")))

(deftest a-plug-is-priced-as-its-flood-though-the-search-reads-no-water-beside-it
  ;; a plug's unseen neighbour is read as water by body-prices' :level, never by the search's own reads
  (let [flood {:level (fn [c] (when (= c [1 10 0]) 10)) :open (delay [7 0]) :dig (fn [under _] [0 under]) :step (fn [under] [0 under])}
        route-s (fn [plugs] (:seconds (dr/route (world {[1 10 0] "stone" [1 11 0] "stone"}) [0 10 0] [3 10 0]
                                                {:step-s 1 :margin 0 :flood (assoc flood :plugs plugs)})))]
    (is (= 7 (- (route-s #{[1 10 0]}) (route-s #{}))))))

(deftest the-rise-priced-bound-finds-the-way-the-bound-without-it-finds
  ;; a rise through cells at heights where none reads open is a stair's step (or a pillar block) in the bounds: one over a
  ;; way's price, or one that fell by more than a move's price from cell to cell, would end on a dearer way
  (let [seen (into {} (concat (for [x (range -2 9) z [3]] [[x 4 z] "air"])
                              (for [x [6] y (range -2 4) z (range 0 4)] [[x y z] "air"])
                              (for [x (range -3 3) y [0 1] z [-4]] [[x y z] "air"])))
        ys (into-array (sort (into (set (range 10 40)) (keep (fn [[[_ y _] n]] (when (dr/open? n) y)) seen))))
        rho #(memoize (steps-to-seen seen %))
        price (fn [from goal opts] (:seconds (first ((dr/search (world seen) from goal opts) 100000))))]
    (is (= [] (for [[from goal extra] [[[6 -2 1] [0 10 0]]
                                       [[6 -2 1] [0 10 0] {:pillar-s 4 :place-s 0.4}]
                                       [[6 -2 1] [0 10 0] {:pillar-s 0.4 :place-s 0.4}]
                                       [[-3 0 -4] [8 4 3]]
                                       [[-2 4 3] [4 5 -2] {:pillar-s 4}]
                                       [[0 -6 0] [3 6 2]]
                                       [[6 -2 1] [0 10 0] {:range 1 :near [0 10 0]}]
                                       [[-3 0 -4] [6 3 2] {:range 1 :near [6 3 2]}]
                                       [[0 10 0] [6 -2 0]]]
                    prior? [false true]
                    :let [opts (merge (unseen-opts seen (rho 3) (rho 1) 1000) {:max-fall 3 :margin 3 :stair-s 3 :prior-h? prior?} extra)
                          bare (price from goal opts)
                          risen (price from goal (assoc opts :open-ys ys :max-s (+ bare 1e-6)))]
                    :when (not (and (number? bare) (number? risen) (< (js/Math.abs (- bare risen)) 1e-9)))]
                [from goal prior? bare risen])))))


(defn own-cut-floors
  "The floors route's walks step onto that its own earlier steps dug."
  [route]
  (first (reduce (fn [[out dug] {:keys [to move dig]}]
                   [(cond-> out (and (= :walk move) (dug (update to 1 dec))) (conj (update to 1 dec))) (into dug dig)])
                 [[] #{}] (:steps route))))

(deftest a-cheaper-way-to-a-cell-that-dug-a-floor-a-route-needs-on-hides-no-cheaper-route
  ;; the cheapest way to a cell found first had dug a floor the cheapest route on stands on, and each bound found a
  ;; dearer one (prior 54.9 s, plain 58.3 s) than the way through these feet cells
  (let [seen (into {} (concat (for [x (range -2 9) z [3]] [[x 4 z] "air"])
                              (for [x [6] y (range -2 4) z (range 0 4)] [[x y z] "air"])
                              (for [x (range -3 3) y [0 1] z [-4]] [[x y z] "air"])))
        rho #(memoize (steps-to-seen seen %))
        opts (merge (unseen-opts seen (rho 3) (rho 1) 1000) {:max-fall 3 :margin 3 :stair-s 3 :range 1 :near [6 2 2]})
        feet [[1 -6 0] [2 -6 0] [3 -6 0] [4 -5 0] [5 -4 0] [6 -3 0] [6 -2 1] [7 -1 1] [8 0 1] [8 1 2] [7 2 2]]
        way (first (dr/route-within (world seen) [0 -6 0] [6 2 2] {:steps (map #(hash-map :to %) feet)} opts))
        answers (for [bound [{:prior-h? false} {:prior-h? true} {:prior-h? true :back-h? true}]]
                  (first ((dr/search (world seen) [0 -6 0] [6 2 2] (merge opts bound)) 100000)))]
    (is (= [] (own-cut-floors way)) "the way through those cells stands on no floor it dug")
    (is (= [] (remove #(< (js/Math.abs (- (:seconds %) (:seconds way))) 1e-9) answers)) (str "each bound finds its price, " (:seconds way) " s"))
    (is (= [[] [] []] (map own-cut-floors answers)))))

(deftest a-cell-read-open-at-a-height-the-rise-bound-read-closed-is-read-as-never-seen
  ;; open-ys is the memory as the search began: a cell seen open since, at a height it held none at, would make a rise
  ;; there cut-free while the bounds price it a stair's step
  (let [opts {:dig-s (constantly 0.5) :step-s 0.3 :stair-s 3 :open-ys #js [10 11] :margin 2}
        rock (fn [open] (fn [[_ y _ :as c]] (cond (open c) "air" (< y 30) "stone" :else "air")))
        own #{[0 10 0] [0 11 0]}
        seen (dr/route (rock (conj own [2 12 0])) [0 10 0] [4 14 0] opts)
        never (dr/route (rock own) [0 10 0] [4 14 0] opts)
        ;; priced unseen: the cell seen open since is counted unseen, as one never seen is
        tolled (assoc opts :unseen-step-s 2)
        seen-u (dr/route (rock (conj own [2 12 0])) [0 10 0] [4 14 0] (assoc tolled :unseen? (constantly false)))
        never-u (dr/route (rock own) [0 10 0] [4 14 0] (assoc tolled :unseen? #(= [2 12 0] %)))]
    (is (seq (:dig never)))
    (is (= (select-keys never [:seconds :dig]) (select-keys seen [:seconds :dig])))
    (is (> (:seconds never-u) (:seconds never)) "the never-seen cell's toll is paid on the way")
    (is (= (select-keys never-u [:seconds :dig]) (select-keys seen-u [:seconds :dig])))))

(deftest unseen-digs-priced-by-their-distance-from-the-seen-find-the-plain-search-s-price-through-fewer-cells
  ;; rock below y 10 never seen, a dig there the dearer the farther from the seen plane (as the measured solid chance is)
  ;; and under y 4: the least by tier bounds it where the one least of all (the nearest's) leaves a wide ball searched
  (let [share (fn [[_ y _]] (let [d (- 10 y)] (min 1 (+ 0.2 (* 0.1 (js/Math.ceil (/ d 2))) (if (<= y 4) 0.2 0)))))
        row (fn [low?] (mapv #(* 0.5 (min 1 (+ 0.2 (* 0.1 (js/Math.ceil (/ % 2))) (if low? 0.2 0)))) (range 1 17)))
        opts (assoc (unseen-opts {} (constantly ##Inf) (constantly ##Inf) 1000)
                    :dig-s (fn [_ c] (* 0.5 (share c))) :unseen-dig-s 0.1 :max-fall 3)
        tiers {:low-y 4 :low (row true) :any (row false)}
        run (fn [from goal o] (let [[a n] (to-end (dr/search (world {}) from goal o))] [(:seconds a) n]))]
    (doseq [[from goal] [[[0 10 0] [12 -4 4]] [[0 10 0] [3 -10 0]] [[0 10 0] [16 3 0]]]
            :let [[plain-s] (run from goal (assoc opts :prior-h? false))
                  [flat-s flat-n] (run from goal (assoc opts :prior-h? true))
                  [tier-s tier-n] (run from goal (assoc opts :prior-h? true :unseen-digs tiers))]]
      (is (< (js/Math.abs (- plain-s tier-s)) 1e-9) (str from "->" goal ": " tier-s " s, plain " plain-s))
      (is (< (js/Math.abs (- plain-s flat-s)) 1e-9))
      (is (< tier-n flat-n) (str from "->" goal ": " tier-n " cells, " flat-n " at the least of all")))))

(defn holed-climb
  "Stone below y 30 with about one cell in eight air round a climb from [0 10 0], seeded: [block-at goal]."
  [seed]
  (let [state (atom seed)
        rnd (fn [] (/ (swap! state #(mod (+ (* % 1103515245) 12345) 2147483648)) 2147483648))
        holes (into #{} (for [x (range -3 12) y (range 8 26) z (range -3 4) :when (< (rnd) 0.12)] [x y z]))
        goal [(js/Math.floor (* 9 (rnd))) (+ 14 (js/Math.floor (* 9 (rnd)))) (dec (js/Math.floor (* 3 (rnd))))]
        open (into holes [[0 10 0] [0 11 0] goal (update goal 1 inc)])]
    [(fn [[_ y _ :as c]] (if (or (open c) (>= y 30)) "air" "stone")) goal]))

(deftest a-way-that-refuses-more-contested-floors-at-no-lower-price-is-not-searched-on
  ;; each floor contested labels the ways that dug it apart; one that dug a superset of another's and costs no less is no
  ;; better (without that the climb expanded over 100000 cells)
  (let [[w goal] (holed-climb 13)
        r (dr/route w [0 10 0] goal {:step-s 0.3 :dig-s (constantly 0.5) :stair-s 3 :margin 2 :max-fall 3 :max-nodes 20000})]
    (is (< (js/Math.abs (- 33.3 (:seconds r))) 1e-9) (pr-str (dissoc r :steps :dig)))))
