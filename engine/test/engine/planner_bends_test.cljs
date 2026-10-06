(ns engine.planner-bends-test
  "engine.path.planner.bends over hand-made 17x17 masks: the string-pulled route round a post, and withBends on a path."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.planner.base :refer [GRID]]
            [engine.path.planner.bends :as bends]
            [engine.path.planner.search :refer [Search]]))

(defn mask-of
  "A mask of free positions with the (i j) cells in blocked set to 0."
  [blocked]
  (let [m (js/Uint8Array. (* GRID GRID))]
    (.fill m 1)
    (doseq [[i j] blocked] (aset m (+ (* j GRID) i) 0))
    m))

(defn idx [i j] (+ (* j GRID) i))

(def stalk (for [i (range 7 10) j (range 6 11)] [i j]))
(def wall (for [j (range GRID)] [8 j]))

(deftest line-free-follows-the-segment
  (let [open (mask-of [])
        post (mask-of stalk)]
    (is (true? (bends/line-free? open 0 8 16 8)))
    (is (false? (bends/line-free? post 0 8 16 8)) "the stalk stands on the line")
    (is (true? (bends/line-free? post 0 2 16 2)) "a line clear of it")
    (is (true? (bends/line-free? post 0 8 7 8)) "the end points are not tested")))

(deftest route-is-empty-when-the-leg-is-straight
  (is (= [] (bends/route (mask-of stalk) (idx 0 2) (idx 16 2)))))

(deftest route-corners-lead-round-a-post
  (let [mask (mask-of stalk)
        a (idx 0 8)
        b (idx 16 8)
        corners (bends/route mask a b)
        ij (fn [k] [(js-mod k GRID) (js/Math.floor (/ k GRID))])
        pts (mapv ij (concat [a] corners [b]))]
    (is (seq corners) "the straight line is blocked: at least one corner")
    (is (every? (fn [k] (let [[i j] (ij k)] (pos? (aget mask (idx i j))))) corners) "every corner is a free position")
    (is (every? (fn [[[ai aj] [bi bj]]] (bends/line-free? mask ai aj bi bj)) (partition 2 1 pts))
        "every leg between corners is free")))

(deftest route-is-nil-when-b-cannot-be-reached
  (is (nil? (bends/route (mask-of wall) (idx 0 8) (idx 16 8)))))

(defn search-over
  "A Search whose every cell is tight with the given mask."
  [mask]
  (doto (js/Object.create (.-prototype Search))
    (aset "isTight" (fn [& _] true))
    (aset "shapeOf" (fn [& _] #js {:mask mask}))))

(defn walk [x px pz extra]
  (let [s #js {:x x :y 64 :z 0 :h 0 :move 1 :corner false :px px :pz pz}]
    (doseq [[k v] extra] (aset s k v))
    s))

(deftest with-bends-on-a-last-step-bends-then-comes-again-at-its-stand-point
  (let [only (walk 0 1.0 0.5 {"cx" 0.0 "cz" 0.5})
        out (.withBends (search-over (mask-of stalk)) #js [only])
        [head & more] (vec out)
        bends (butlast more)
        final (last more)]
    (is (identical? only head))
    (is (seq bends))
    (is (every? #(true? (.-bend %)) bends))
    (is (not (.-bend final)))
    (is (= [1.0 0.5] [(.-px final) (.-pz final)]) "the final step is at the stand point")
    (is (undefined? (.-cx final)) "and no longer comes in by a crossing")
    (is (= [(.-px (first bends)) (.-pz (first bends))] [(.-px head) (.-pz head)]) "the step heads for its first bend")))

(deftest with-bends-on-a-free-leg-heads-for-the-next-crossing
  (let [a (walk 0 0.5 0.5 {"cx" 0.0 "cz" 0.125})
        b (walk 1 1.5 0.5 {"cx" 1.0 "cz" 0.125})
        out (.withBends (search-over (mask-of [])) #js [a b])]
    (is (= 2 (.-length out)) "no bends added")
    (is (= [1.0 0.125] [(.-px a) (.-pz a)]))))

(deftest with-bends-leaves-steps-that-are-not-walked-alone
  (let [a (walk 0 0.5 0.5 {"cx" 0.0 "cz" 0.5})
        b (walk 1 1.5 0.5 {"cx" 1.0 "cz" 0.5})
        jump (doto (walk 2 2.5 0.5 {"cx" 2.0 "cz" 0.5}) (aset "move" 3))
        out (.withBends (search-over (mask-of stalk)) #js [a b jump])]
    (is (identical? jump (aget out (dec (.-length out)))))
    (is (= [1.5 0.5] [(.-px b) (.-pz b)]) "b is followed by a jump: no business of bends, its point stays")))
