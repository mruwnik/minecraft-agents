(ns engine.planner-unknown-test
  "options.unknown: a cell the body has not seen reads as void_air (air for the body); a stand on an unseen floor is a
  phantom floor, allowed only where the walk can check it first (gate-checkable) and see it (seeDepth, seeLit, or a torch
  at a lightable takeoff), and priced. No proof rests on an unseen cell. Off (nil): plans as before."
  (:require [cljs.test :refer [are deftest is testing]]
            [engine.path.blocks :as blocks]
            [engine.path.fixture :as fx]
            [engine.path.planner-tuned :as planner]
            [engine.planner-fixture :as pf]
            [jobs.lib.cost :as cost]))

(def U "void_air")
(def from {:x 0 :y 64 :z 2})

(defn line
  "A world of single blocks along z 2: [x y name] or [x y name props]."
  [& cells]
  (pf/snapshot {:blocks (mapv (fn [[x y name props]] (cond-> [x y 2 name] props (conj props))) cells)}))

(def prices {:cell 0.4 :peekS 0.7 :aimS 0.1})

(defn unk [m] {:unknown (merge prices m)})

(defn run
  ([w goal options] (run w goal options from))
  ([w goal options start] (pf/run w goal options start)))

(defn step-secs [r] (get-in r [:path :cost :seconds]))
(defn dark-secs [r] (get-in r [:path :cost :darkSeconds]))
(defn last-step [r] (peek (get-in r [:path :steps])))
(defn found? [r] (= "found" (:status r)))
(defn near? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest void-air-reads-as-air-in-every-table
  (let [^js t @pf/table
        air (first (blocks/state-ids t "air"))
        void (first (blocks/state-ids t "void_air"))]
    ;; (boxes is the shapes' flat array, not per state; boxStart points into it and matters only with a box)
    (is (zero? (aget (.-boxCount t) void)))
    (doseq [k (js/Object.keys t)
            :when (not (#{"nameIds" "boxes" "boxStart"} k))
            :let [^js a (aget t k)]]
      (is (= (aget a air) (aget a void)) k))))

(deftest off-plans-as-before
  (let [w (line [0 63 "stone"] [1 63 U] [2 63 "stone"])
        off (run w (pf/near 2 64 2) {})]
    (is (= [[0 64 2] [2 64 2]] (pf/cells off)) "void_air is air: a gap jump over it")
    (is (= (:gap pf/MOVE) (:move (last-step off))))
    (is (= (dissoc off :ms) (dissoc (run (line [0 63 "stone"] [2 63 "stone"]) (pf/near 2 64 2) {}) :ms)) "as over plain air")
    (is (= (dissoc off :ms) (dissoc (run w (pf/near 2 64 2) {:unknown nil}) :ms)))))

;; ---- phantom floors ----

(deftest a-level-walk-over-an-unseen-floor-costs-the-aim-and-the-unseen-share
  (let [w (line [0 63 "stone"] [1 63 U])
        r (run w (pf/near 1 64 2) (unk {}))]
    (is (found? r))
    (is (= [[0 64 2] [1 64 2]] (pf/cells r)))
    (is (near? (+ 0.1 (* 0.4 (step-secs r))) (dark-secs r)))
    (testing "off: void_air is air, nothing to stand on"
      (is (not (found? (run w (pf/near 1 64 2) {})))))))

(deftest a-move-into-unseen-cells-costs-the-unseen-share-once
  (doseq [unseen [[[1 64 U] [1 65 U]] [[1 65 U]]]]
    (let [r (run (apply line [0 63 "stone"] [1 63 "stone"] unseen) (pf/near 1 64 2) (unk {}))]
      (is (found? r))
      (is (near? (* 0.4 (step-secs r)) (dark-secs r))))))

(deftest a-jump-up-onto-an-unseen-floor-costs-the-aim
  (let [w (line [0 63 "stone"] [1 64 U])
        r (run w (pf/near 1 65 2) (unk {}))]
    (is (found? r))
    (is (= (:jump pf/MOVE) (:move (last-step r))))
    (is (near? (+ 0.1 (* 0.4 (step-secs r))) (dark-secs r)))))

(def drop-1 (line [0 63 "stone"] [1 62 U]))
(def drop-2 (line [0 63 "stone"] [1 61 U]))

(deftest a-drop-of-one-onto-an-unseen-floor-is-seen-from-the-peek
  (let [r (run drop-1 (pf/near 1 63 2) (unk {:seeDepth 1}))]
    (is (found? r))
    (is (= (:drop pf/MOVE) (:move (last-step r))))
    (is (near? (+ 0.7 (* 0.4 (step-secs r))) (dark-secs r)))
    (is (not (:darkRefused r)))))

(deftest a-drop-past-the-see-depth-is-refused-in-the-dark
  (doseq [[w goal depth] [[drop-1 (pf/near 1 63 2) 0] [drop-2 (pf/near 1 62 2) 1]]]
    (let [r (run w goal (unk {:seeDepth depth}))]
      (is (not (found? r)))
      (is (true? (:darkRefused r)))
      (is (= (select-keys goal [:x :y :z]) (select-keys (:darkBest r) [:x :y :z]))))))

(deftest a-deeper-drop-is-seen-with-a-torch-held-or-in-light
  (testing "seeDepth 4 (a torch in hand)"
    (is (found? (run drop-2 (pf/near 1 62 2) (unk {:seeDepth 4})))))
  (testing "seeLit: the landing's air is lit"
    (let [asked (atom [])
          lit (fn [x y z] (swap! asked conj [x y z]) 1)]
      (is (found? (run drop-2 (pf/near 1 62 2) (unk {:seeDepth 1 :seeLit lit}))))
      (is (= #{[1 62 2]} (set @asked)))))
  (testing "seeLit 0: still dark"
    (is (not (found? (run drop-2 (pf/near 1 62 2) (unk {:seeDepth 1 :seeLit (fn [_ _ _] 0)})))))))

(deftest a-lit-drop-is-seen-only-as-deep-as-the-peek-sees
  ;; floor-sight-pin-test: the walk's worst peek stop (x .87) sees a lit floor to d 10
  (let [deep (fn [d] (pf/snapshot {:blocks [[0 63 2 "stone"] [1 (- 63 d) 2 U]] :fill [[0 40 2 0 62 2 "stone"]]}))
        opts (merge {:maxDrop 12} (unk {:seeDepth 1 :seeLit (fn [_ _ _] 1)}))]
    (is (found? (run (deep 10) (pf/near 1 (- 64 10) 2) opts)) "d 10")
    (let [r (run (deep 11) (pf/near 1 (- 64 11) 2) opts)]
      (is (not (found? r)) "d 11")
      (is (true? (:darkRefused r))))))

(deftest see-lit-depth-is-an-option
  (let [deep (fn [d] (pf/snapshot {:blocks [[0 63 2 "stone"] [1 (- 63 d) 2 U]] :fill [[0 40 2 0 62 2 "stone"]]}))
        opts (fn [depth] (merge {:maxDrop 12} (unk {:seeDepth 1 :seeLit (fn [_ _ _] 1) :seeLitDepth depth})))]
    (is (found? (run (deep 6) (pf/near 1 (- 64 6) 2) (opts 6))) "d 6 with seeLitDepth 6")
    (is (not (found? (run (deep 3) (pf/near 1 (- 64 3) 2) (opts 2)))) "d 3 with seeLitDepth 2")))

(deftest no-descent-into-unseen-land
  (let [w (line [0 63 "stone"] [1 62 U] [1 61 U] [1 60 "stone"])]
    (is (found? (run w (pf/near 1 61 2) {})) "off: void_air is air, a drop of 3")
    (is (not (found? (run w (pf/near 1 61 2) (unk {:seeDepth 4})))))
    (is (found? (run w (pf/near 1 63 2) (unk {:seeDepth 4}))) "the drop lands on the first unseen cell, a phantom floor")))

;; ---- light ----

(def torch-base
  (delay (let [^js t @pf/table
               a (js/Uint8Array. (.-length (.-top t)))]
           (doseq [id (blocks/state-ids t "stone")] (aset a id 1))
           a)))

(defn lights [m] (unk (merge {:seeDepth 1 :lightCost 5 :torchBase @torch-base} m)))

(deftest a-dark-drop-is-lit-from-a-lightable-takeoff
  (let [r (run drop-2 (pf/near 1 62 2) (lights {}))]
    (is (found? r))
    (is (true? (:light (last-step r))))
    (is (near? (+ 5 0.7 (* 0.4 (step-secs r))) (dark-secs r)))))

(deftest light-only-at-a-lightable-takeoff
  (let [deep (fn [d] (pf/snapshot {:blocks [[0 63 2 "stone"] [1 (- 63 d) 2 U]] :fill [[0 40 2 0 62 2 "stone"]]}))
        goal (fn [d] (pf/near 1 (- 64 d) 2))
        deep-opts {:maxDrop 12}]
    (is (found? (run (deep 5) (goal 5) (merge deep-opts (lights {})))) "d 5")
    (doseq [[why w g opts] [["d 6: past what the peek sees of a lit floor" (deep 6) (goal 6) (merge deep-opts (lights {}))]
                            ["the takeoff's floor is no torch base" drop-2 (pf/near 1 62 2) (lights {:torchBase (js/Uint8Array. 1)})]
                            ["lightOk says no" drop-2 (pf/near 1 62 2) (lights {:lightOk (fn [_ _ _] false)})]
                            ["a torch failed there" drop-2 (pf/near 1 62 2) (lights {:noLight (js/Set. [(planner/cell-key 0 64 2)])})]
                            ["the takeoff's feet cell holds a carpet" (line [0 63 "stone"] [0 64 "white_carpet"] [1 61 U]) (pf/near 1 62 2) (lights {})]]]
      (let [r (run w g opts)]
        (is (not (found? r)) why)
        (is (true? (:darkRefused r)) why)))))

;; ---- where the walk can check the floor first ----

(deftest a-phantom-after-a-gap-jump-is-refused
  (let [gap (line [0 63 "stone"] [1 63 "stone"] [3 63 "stone"] [4 63 U])
        walk (line [0 63 "stone"] [1 63 "stone"] [2 63 "stone"] [3 63 "stone"] [4 63 U])
        r (run gap (pf/near 4 64 2) (unk {}))]
    (is (not (found? r)))
    (is (true? (:unseenRefused r)))
    (is (not (:darkRefused r)))
    (is (found? (run walk (pf/near 4 64 2) (unk {}))) "after a walk")))

(deftest a-phantom-after-an-exit-from-water-is-refused
  (let [pool (fn [floor] (pf/snapshot {:fill [[0 60 2 3 60 2 "stone"] [0 61 2 3 63 2 "water"]]
                                       :blocks [[4 63 2 floor] [5 63 2 "stone"]]}))
        start {:x 1 :y 63 :z 2}]
    (is (found? (run (pool "stone") (pf/near 5 64 2) (unk {}) start)) "a seen floor")
    (let [r (run (pool U) (pf/near 5 64 2) (unk {}) start)]
      (is (not (found? r)))
      (is (true? (:unseenRefused r))))))

(deftest a-phantom-after-a-climb-is-refused
  ;; a ladder at x 1 (on a wall at z 3) up to y 67; the step off it east onto (2 67), whose floor (2 66) is the
  ;; top of a stone column
  (let [shaft (fn [top] (pf/snapshot {:fill [[0 63 2 0 63 2 "stone"] [1 63 3 1 68 3 "stone"] [2 63 2 2 65 2 "stone"]
                                             [1 64 2 1 67 2 "ladder" {:facing "north"}]]
                                      :blocks [[2 66 2 top]]}))]
    (is (found? (run (shaft "stone") (pf/near 2 67 2) (unk {}))) "a seen floor")
    (let [r (run (shaft U) (pf/near 2 67 2) (unk {}))]
      (is (not (found? r)))
      (is (true? (:unseenRefused r))))))

(deftest a-phantom-off-a-vine-with-no-floor-under-it-is-refused
  ;; the body hangs in a lone vine (on a stone block at z 1) 8 above the ground; the drop east lands on (1 63), whose floor
  ;; (1 62) is unseen: hanging, the body slides down the vine while it looks, so it cannot stop to check that floor
  (let [vine (fn [under floor] (pf/snapshot {:fill [[0 55 2 1 55 2 "stone"] [0 64 1 0 64 1 "stone"]]
                                             :blocks [[0 64 2 "vine" {:north "true"}] [0 63 2 under] [1 62 2 floor]]}))
        start {:x 0 :y 64 :z 2}]
    (is (found? (run (vine "stone" U) (pf/near 1 63 2) (unk {:seeDepth 2}) start)) "a vine standing on a floor: the body stops there")
    (let [r (run (vine "air" U) (pf/near 1 63 2) (unk {:seeDepth 2}) start)]
      (is (not (found? r)) (pr-str (pf/cells r)))
      (is (true? (:unseenRefused r))))))

(deftest a-drop-into-the-middle-of-a-vine-column-is-planned
  ;; a vine column (0 62..64 2) over ground at y 55; the body stands in its top cell and the drop east lands on (1 63),
  ;; whose floor (1 62) is unseen. A vine below holds the body (a column, climbed on down), so it is no lone climbable
  (let [w (pf/snapshot {:fill [[0 55 2 1 55 2 "stone"] [0 62 1 0 64 1 "stone"]]
                        :blocks [[0 64 2 "vine" {:north "true"}] [0 63 2 "vine" {:north "true"}] [0 62 2 "vine" {:north "true"}]
                                 [1 62 2 U]]})
        r (run w (pf/near 1 63 2) (unk {:seeDepth 2}) {:x 0 :y 64 :z 2})]
    (is (found? r))
    (is (some #{[1 63 2]} (pf/cells r)) (pr-str (pf/cells r)))))

(deftest a-swim-over-an-unseen-bed-is-planned
  (let [pond (fn [bed] (pf/snapshot {:fill [[0 62 2 5 62 2 "stone"] [0 63 2 5 63 2 "water"]]
                                     :blocks [[2 62 2 bed] [3 62 2 bed] [6 63 2 "stone"]]}))
        start {:x 0 :y 63 :z 2}]
    (is (found? (run (pond "stone") (pf/near 6 64 2) (unk {}) start)) "a seen bed")
    (let [r (run (pond U) (pf/near 6 64 2) (unk {}) start)]
      (is (found? r))
      (is (not (:unseenRefused r)))
      (is (some #{[2 63 2]} (pf/cells r))))))

(deftest a-swim-under-unseen-cells-uses-the-air-like-a-dive
  (let [pond (fn [over] (pf/snapshot {:fill [[0 62 2 10 62 2 "stone"] [0 63 2 10 63 2 "water"] [0 64 2 10 64 2 over]]
                                      :blocks [[11 63 2 "stone"]]}))
        start {:x 0 :y 63 :z 2}
        options (assoc (unk {}) :costs {:airLimit 3})]
    (is (found? (run (pond "air") (pf/near 11 64 2) options start)) "seen air over the pond: swum head out")
    (is (not (found? (run (pond U) (pf/near 11 64 2) options start))) "unseen cells over it are not air: the swim is a dive past the limit")))

(deftest a-drop-into-water-over-an-unseen-bed-is-planned
  (let [w (line [0 63 "stone"] [1 62 "water"] [1 61 U])
        r (run w (pf/near 1 62 2) (unk {:seeDepth 0}))]
    (is (found? r))
    (is (= (:drop pf/MOVE) (:move (last-step r))))
    (is (not (:darkRefused r)))))

(deftest a-gap-jump-over-a-refused-phantom-floor
  (testing "the phantom stand after a gap jump is refused, so the body jumps over it to the seen floor beyond"
    (let [w (line [0 63 "stone"] [1 63 "stone"] [3 63 "stone"] [4 63 U] [5 63 "stone"])
          r (run w (pf/near 5 64 2) (unk {}))]
      (is (found? r))
      (is (= [[0 64 2] [1 64 2] [3 64 2] [5 64 2]] (pf/cells r)))
      (is (= (:gap pf/MOVE) (:move (last-step r))))))
  (testing "a missed floor: jumped over"
    (let [w (line [0 63 "stone"] [1 63 U] [2 63 "stone"])
          r (run w (pf/near 2 64 2) (unk {:missed (js/Set. [(planner/cell-key 1 63 2)])}))]
      (is (found? r))
      (is (= [[0 64 2] [2 64 2]] (pf/cells r)))))
  (testing "two phantom cells: a gap of two"
    (let [w (line [0 63 "stone"] [1 63 U] [2 63 U] [3 63 "stone"] [4 55 "stone"])
          r (run w (pf/near 3 64 2) (unk {:missed (js/Set. [(planner/cell-key 1 63 2) (planner/cell-key 2 63 2)])}))]
      (is (found? r))
      (is (= [[0 64 2] [3 64 2]] (pf/cells r))))))

(deftest a-gap-jump-through-an-unseen-feet-cell-is-not-planned
  (testing "a 2-gap whose second cell has an unseen feet cell"
    (let [w (line [0 63 "stone"] [2 64 U] [3 63 "stone"])
          r (run w (pf/near 3 64 2) (unk {}))]
      (is (not (found? r)))))
  (testing "a 2-gap with an unseen head cell in its arc"
    (doseq [x [1 2]]
      (let [w (line [0 63 "stone"] [x 65 U] [3 63 "stone"])
            r (run w (pf/near 3 64 2) (unk {}))]
        (is (not (found? r)) (str "head cell at x " x)))))
  (testing "an unseen floor under the arc is allowed"
    (let [w (line [0 63 "stone"] [1 63 U] [2 63 U] [3 63 "stone"] [4 55 "stone"])
          r (run w (pf/near 3 64 2) (unk {:missed (js/Set. [(planner/cell-key 1 63 2) (planner/cell-key 2 63 2)])}))]
      (is (found? r))
      (is (= (:gap pf/MOVE) (:move (last-step r)))))))

(deftest a-gap-jump-under-an-unseen-cell-two-over-the-feet-is-not-planned
  (testing "a jump up over a 2-gap: its arc reaches y+2"
    (let [seen (line [0 63 "stone"] [3 64 "stone"] [4 55 "stone"])
          hidden (line [0 63 "stone"] [3 64 "stone"] [4 55 "stone"] [1 66 U])]
      (is (= (:gap pf/MOVE) (:move (last-step (run seen (pf/near 3 65 2) (unk {}))))))
      (is (not (found? (run hidden (pf/near 3 65 2) (unk {})))))))
  (testing "a level gap from a bottom slab (feet 0.5 up): its arc reaches y+2"
    (let [slab {:type "bottom"}
          seen (line [0 64 "oak_slab" slab] [3 64 "oak_slab" slab] [4 55 "stone"])
          hidden (line [0 64 "oak_slab" slab] [3 64 "oak_slab" slab] [4 55 "stone"] [2 66 U])]
      (is (= (:gap pf/MOVE) (:move (last-step (run seen (pf/near 3 64 2) (unk {}))))))
      (is (not (found? (run hidden (pf/near 3 64 2) (unk {}))))))))

(deftest a-running-jump-onto-a-one-cell-landing-with-unseen-depth-past-it-is-not-planned
  (let [seen (line [0 63 "stone"] [3 63 "stone"] [4 55 "stone"])]
    (is (= (:gap pf/MOVE) (:move (last-step (run seen (pf/near 3 64 2) (unk {}))))) "ground seen 8 below the overshoot")
    (is (not (found? (run (line [0 63 "stone"] [3 63 "stone"] [4 62 U] [4 55 "stone"]) (pf/near 3 64 2) (unk {}))))
        "an unseen cell under the overshoot hides how far it falls")))

(deftest a-walk-through-a-closed-gate-onto-a-phantom-floor-is-checkable
  ;; (a gate or door is crossed by a walk that opens it; MOVE-OPEN is the climb through a shut trapdoor, never a takeoff)
  (let [w (line [0 63 "stone"] [1 63 U] [1 64 "oak_fence_gate" {:facing "east" :open "false"}])
        r (run w (pf/near 1 64 2) (unk {}))]
    (is (found? r))
    (is (not (:unseenRefused r)))
    (is (= (:walk pf/MOVE) (:move (last-step r))))))

(deftest a-water-takeoff-is-not-lightable
  (let [w (fn [feet] (line [0 63 "stone"] [0 64 feet] [1 61 U]))]
    (is (found? (run (w "air") (pf/near 1 62 2) (lights {}))) "a dry takeoff")
    (let [r (run (w "water") (pf/near 1 62 2) (lights {}) {:x 0 :y 64 :z 2})]
      (is (not (found? r)))
      ;; (a swim node expands no drop at all, so the takeoff is never offered to lightable)
      (is (= "exhausted" (:reason r)))
      (is (not (:darkRefused r)))
      (is (not (:unseenRefused r))))))

(deftest a-phantom-after-a-trapdoor-climb-is-refused
  ;; a ladder up to a shut trapdoor (MOVE-OPEN); the stand beside the trapdoor cell has an unseen floor
  (let [w (pf/world {:fill [[8 64 4 10 72 6 "stone"]
                            [9 64 5 9 72 5 "air"]
                            [9 64 5 9 71 5 "ladder" {:facing "west"}]
                            [9 72 5 9 72 5 "oak_trapdoor" {:half "top"}]
                            [10 71 5 10 71 5 "void_air"]
                            [10 72 5 10 72 5 "air"]]})
        r (run w (pf/near 10 72 5) (unk {}) {:x 9 :y 64 :z 5})]
    (is (not (found? r)))
    (is (:unseenRefused r))))

(deftest a-phantom-after-a-drop-is-allowed
  (let [w (line [0 63 "stone"] [1 62 "stone"] [2 62 U])]
    (is (found? (run w (pf/near 2 63 2) (unk {}))))))

(deftest the-floor-under-a-door-is-implied-and-the-floor-after-it-checked-from-the-door
  (let [w (line [0 63 "stone"] [1 63 U] [1 64 "oak_door" {:half "lower" :facing "east"}] [1 65 "oak_door" {:half "upper" :facing "east"}]
                [2 63 U])
        r (run w (pf/near 2 64 2) (unk {}))]
    (is (found? r))
    (is (= [[0 64 2] [1 64 2] [2 64 2]] (pf/cells r)))))

(deftest a-missed-floor-is-never-seeable
  (let [w (line [0 63 "stone"] [1 63 U])
        r (run w (pf/near 1 64 2) (unk {:missed (js/Set. [(planner/cell-key 1 63 2)])}))]
    (is (not (found? r)))
    (is (true? (:darkRefused r)))))

;; ---- proofs ----

(deftest a-goal-over-an-unseen-floor-is-standable
  (let [w (line [0 63 "stone"] [1 63 "stone"] [2 63 U])]
    (is (= "goal-not-standable" (:reason (run w (pf/near 2 64 2) {}))))
    (is (found? (run w (pf/near 2 64 2) (unk {}))))))

(deftest no-goal-flood-proof-through-unseen-cells
  (let [w (pf/snapshot {:fill [[5 55 5 15 65 15 "stone"]]
                        :blocks [[10 60 10 "air"] [10 61 10 "air"] [10 62 10 U]]})
        goal (pf/near 10 60 10)
        top {:x 6 :y 66 :z 6}]
    (is (= "goal-enclosed" (:reason (run w goal {} top))) "off: void_air is air, the pocket is sealed")
    (is (not (#{"goal-enclosed" "goal-cut-off"} (:reason (run w goal (unk {}) top)))) "on: its roof is unseen")))

(def stone-room-fill
  "A floor, and a closed room of stone (x 10..12, z 0..1, two high) under a roof."
  [[-2 60 -2 30 63 4 "stone"] [9 64 -1 13 65 2 "stone"] [10 66 0 12 66 1 "stone"] [10 64 0 12 65 1 "air"]])

;; the floor under a wall (its head cell is the wall) and a wall corner (both cells beside the diagonal are walls) hold
;; no body, as the inside of a hut a body has seen leaves them
(deftest a-room-walled-in-by-seen-blocks-is-proved-whatever-lies-unseen-behind-them
  (doseq [cells [[[10 62 0]] [[8 64 0]] [[8 63 0]] [[10 66 -1]] [[10 67 0]] [[10 68 0]] [[9 63 0]] [[9 64 -1] [9 65 -1]]]
          :let [w (pf/snapshot {:fill stone-room-fill :blocks (mapv #(conj % U) cells)})
                r (run w (pf/near 10 64 0) (unk {}) {:x 0 :y 64 :z 0})]]
    (is (= "goal-enclosed" (:reason r)) (str "unseen cells " cells))))

(defn- door [x z facing]
  [[x 64 z "iron_door" {:half "lower" :facing facing}] [x 65 z "iron_door" {:half "upper" :facing facing}]])

;; the proof rests on every door at once: it holds only while the chance that none opened is over 1/2
(deftest a-room-shut-by-doors-likelier-some-changed-than-not-is-not-proved-enclosed
  (let [reason (fn [doors q]
                 (let [w (pf/snapshot {:fill stone-room-fill :blocks (into [] (mapcat #(apply door %)) doors)})
                       changed (fn [x y z] (if (some (fn [[dx dz]] (and (== x dx) (== z dz) (<= 64 y 65))) doors) q 0))]
                   (:reason (run w (pf/near 10 64 0) (unk {:changed changed}) {:x 0 :y 64 :z 0}))))
        one [[9 0 "west"]]
        three [[9 0 "west"] [9 1 "west"] [13 0 "east"]]]
    (is (= "goal-enclosed" (reason one 0.4)) "one door likely still shut: the proof holds")
    (is (not= "goal-enclosed" (reason one 0.5)) "one door likelier opened than not")
    (is (= "goal-enclosed" (reason three 0.2)) "three doors at 0.2: none opened at 0.51")
    (is (not= "goal-enclosed" (reason three 0.4)) "three doors at 0.4: some opened at 0.78")))

(deftest a-start-ringed-by-dark-floors-is-not-start-enclosed
  (let [w (pf/snapshot {:blocks [[0 63 2 "stone"] [1 62 2 U] [-1 62 2 U] [0 62 3 U] [0 62 1 U]]})
        goal (pf/near 200 64 2)]
    (is (= "start-enclosed" (:reason (run w goal {}))) "off")
    (let [r (run w goal (unk {:seeDepth 0}))]
      (is (true? (:darkRefused r)))
      (is (not= "start-enclosed" (:reason r))))))

(deftest risk-looks-stop-at-an-unseen-cell
  (let [w (line [0 63 "stone"] [1 63 "stone"] [2 62 U] [2 61 "lava"] [3 63 "stone"])
        goal (pf/near 3 64 2)
        off (run w goal {})
        ;; (gapMiss 0: the unseen depth under the gap is also priced as a fall that may kill)
        on (run w goal (assoc (unk {:seeDepth 0}) :costs {:gapMiss 0}))]
    (is (pos? (get-in off [:path :cost :risk])) "off: the gap's column falls into lava")
    (is (found? on))
    (is (= (:gap pf/MOVE) (:move (last-step on))))
    (is (zero? (get-in on [:path :cost :risk])) "on: the look stops at the unseen cell")
    (is (> (get-in (run w goal (unk {:seeDepth 0})) [:path :cost :risk]) 1000) "the unseen depth is a miss that may kill")))

(defn column-snapshot
  "A snapshot of one loaded column (0 0) reading (f y) at every y of -64..319."
  [f]
  #js {:minY -64 :height 384 :stateAt (fn [x _y z] (if (and (zero? x) (zero? z)) (f _y) planner/UNLOADED))})

(deftest ground-y-reads-the-masked-column
  (let [^js t @pf/table
        ground-y #'planner/ground-y
        void (first (blocks/state-ids t "void_air"))
        stone (first (blocks/state-ids t "stone"))
        air (first (blocks/state-ids t "air"))]
    (is (nil? (ground-y (column-snapshot (constantly void)) t 0 0)) "all unknown: no ground (the start's y bounds the box)")
    (is (= 61 (ground-y (column-snapshot #(if (= 60 %) stone void)) t 0 0)) "known stone at 60 under unknown")
    (is (= 64 (ground-y (column-snapshot #(if (<= % 63) stone air)) t 0 0)) "unmasked: as before")))

(deftest light-depth-is-an-option
  (is (found? (run drop-2 (pf/near 1 62 2) (lights {:lightDepth 2}))))
  (is (not (found? (run drop-2 (pf/near 1 62 2) (lights {:lightDepth 1}))))))

(deftest a-goal-with-no-y-is-refused-at-once
  (let [^js w (line [0 63 "stone"] [1 63 "stone"] [2 63 "stone"])
        void (first (blocks/state-ids @pf/table "void_air"))
        ;; (a masked snapshot, as the go-to bench's: every cell the body has not seen reads void_air)
        masked (doto (js/Object.create w) (set! -stateAt (fn [x y z] (if (js/Number.isFinite y) (.stateAt w x y z) void))))]
    (are [world options] (= "goal-not-standable" (:reason (run world (dissoc (pf/near 2 64 2 2) :y) options)))
      w {}
      masked {}
      masked (unk {:id void :seeDepth 4 :seeLit (fn [_ _ _] 1) :known (fn [_ _ _] nil)}))))

;; ---- shafts ----

(def seagrass-id (delay (first (blocks/state-ids @pf/table "seagrass"))))

(deftest a-climb-is-planned-up-the-unseen-cells-above-a-seen-ladder
  (are [mask props wet-start] (let [w (pf/snapshot {:fill (cond-> [[0 63 2 2 63 2 "stone"]
                                                                   [1 64 2 1 67 2 "ladder" props]
                                                                   [1 68 2 1 79 2 mask]
                                                                   [2 80 2 2 80 2 mask]]
                                                            wet-start (conj [0 64 2 0 65 2 "water"]))})
                                    r (run w (pf/near 2 80 2 1) (unk (when (= mask "seagrass") {:id @seagrass-id})))]
                                (= 79 (apply max (map second (pf/cells r)))))
    U {:facing "west"} false
    "seagrass" {:facing "west" :waterlogged "true"} true))

(deftest a-way-down-is-planned-through-the-unseen-water-above-a-seen-ladder
  (are [start-ladder? y] (let [w (pf/snapshot {:fill (cond-> [[0 63 2 11 63 2 "stone"] [0 64 1 11 80 1 "stone"] [0 64 3 11 80 3 "stone"]
                                                              [11 64 2 11 80 2 "stone"] [-1 64 2 -1 80 2 "stone"] [0 68 2 9 81 2 "stone"] [1 67 2 9 67 2 "stone"]
                                                              [0 64 2 9 65 2 "water"] [0 66 2 0 67 2 "air"]
                                                              [10 64 2 10 65 2 "ladder" {:facing "west" :waterlogged "true"}]
                                                              [10 66 2 10 80 2 "seagrass"]]
                                                       start-ladder? (conj [10 70 2 10 70 2 "ladder" {:facing "west" :waterlogged "true"}]))})]
                           (found? (run w (pf/near 0 65 2 0) (unk {:id @seagrass-id}) {:x 10 :y y :z 2})))
    false 70
    true 70))

;; ---- the unseen price decides a way round ----

(def pond-across
  "Stone ground x 0..14, z 0..8 (stand y 64), a pond 3 deep across all of it at x 4..10; past its end, at z -1, a row of
  cells the body has not seen (ground and air): the only dry way round is a guess."
  (pf/snapshot {:fill [[0 60 0 14 63 8 "stone"] [4 61 0 10 63 8 "water"] [3 60 -1 11 66 -1 U]]}))

(deftest a-way-round-over-unseen-cells-against-a-seen-swim-is-decided-by-the-unseen-price
  (let [kit {:weapon "stone_sword" :equipment nil}
        round? (fn [counts]
                 (let [r (run pond-across (pf/near 12 64 2) (unk {:cell (cost/unseen-share counts kit)}) {:x 2 :y 64 :z 2})]
                   (is (found? r))
                   (boolean (some #(neg? (:z %)) (get-in r [:path :steps])))))]
    (is (true? (round? {:k 98 :n 100})) "past open ground the unseen way round likely goes on: shorter than the swim")
    (is (false? (round? {:k 50 :n 100})) "among dead ends it likely ends: the swim is taken")))
