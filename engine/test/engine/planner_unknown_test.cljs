(ns engine.planner-unknown-test
  "options.unknown: a cell the body has not seen reads as void_air (air for the body); a stand on an unseen floor is a
  phantom floor, allowed only where the walk can check it first (gate-checkable) and see it (seeDepth, seeLit, or a torch
  at a lightable takeoff), and priced. No proof rests on an unseen cell. Off (nil): plans as before."
  (:require [cljs.test :refer [deftest is testing]]
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

(def prices {:cell 0.1 :floor 0.3 :peekS 0.7 :aimS 0.1})

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

(deftest default-prices-live-in-jobs-lib-cost
  (is (= {:cell 0.1 :floor 0.3 :peek-s 0.7 :aim-s 0.1} cost/unknown-prices)))

(deftest off-plans-as-before
  (let [w (line [0 63 "stone"] [1 63 U] [2 63 "stone"])]
    (is (= (dissoc (run w (pf/near 2 64 2) {}) :ms) (dissoc (run w (pf/near 2 64 2) {:unknown nil}) :ms)))))

;; ---- phantom floors ----

(deftest a-level-walk-over-an-unseen-floor-costs-the-aim-and-the-floor
  (let [w (line [0 63 "stone"] [1 63 U])
        r (run w (pf/near 1 64 2) (unk {}))]
    (is (found? r))
    (is (= [[0 64 2] [1 64 2]] (pf/cells r)))
    (is (near? (+ 0.1 (* 0.3 (step-secs r))) (dark-secs r)))
    (testing "off: void_air is air, nothing to stand on"
      (is (not (found? (run w (pf/near 1 64 2) {})))))))

(deftest unseen-feet-and-head-cells-cost-the-cell-price-each
  (let [w (line [0 63 "stone"] [1 63 "stone"] [1 64 U] [1 65 U])
        r (run w (pf/near 1 64 2) (unk {}))]
    (is (found? r))
    (is (near? (* 0.1 2 (step-secs r)) (dark-secs r)))))

(deftest a-jump-up-onto-an-unseen-floor-costs-the-aim
  (let [w (line [0 63 "stone"] [1 64 U])
        r (run w (pf/near 1 65 2) (unk {}))]
    (is (found? r))
    (is (= (:jump pf/MOVE) (:move (last-step r))))
    (is (near? (+ 0.1 (* 0.3 (step-secs r))) (dark-secs r)))))

(def drop-1 (line [0 63 "stone"] [1 62 U]))
(def drop-2 (line [0 63 "stone"] [1 61 U]))

(deftest a-drop-of-one-onto-an-unseen-floor-is-seen-from-the-peek
  (let [r (run drop-1 (pf/near 1 63 2) (unk {:seeDepth 1}))]
    (is (found? r))
    (is (= (:drop pf/MOVE) (:move (last-step r))))
    (is (near? (+ 0.7 (* 0.3 (step-secs r))) (dark-secs r)))
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
    (is (near? (+ 5 0.7 (* 0.3 (step-secs r))) (dark-secs r)))))

(deftest light-only-at-a-lightable-takeoff
  (let [deep (fn [d] (pf/snapshot {:blocks [[0 63 2 "stone"] [1 (- 63 d) 2 U]] :fill [[0 40 2 0 62 2 "stone"]]}))
        goal (fn [d] (pf/near 1 (- 64 d) 2))
        deep-opts {:maxDrop 12}]
    (is (found? (run (deep 10) (goal 10) (merge deep-opts (lights {})))) "d 10")
    (doseq [[why w g opts] [["d 11" (deep 11) (goal 11) (merge deep-opts (lights {}))]
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
        on (run w goal (unk {:seeDepth 0}))]
    (is (pos? (get-in off [:path :cost :risk])) "off: the gap's column falls into lava")
    (is (found? on))
    (is (= (:gap pf/MOVE) (:move (last-step on))))
    (is (zero? (get-in on [:path :cost :risk])) "on: the look stops at the unseen cell")))
