(ns engine.planner-fence-gap-test
  "A fence pen with a post or two removed (live, ProbePen): every leg the executor walks (one step's stand point, the next
  step's crossing point, its stand point) must be free for the body, and the body must get in through the gap. A fence
  post beside a gap leaves its cell a U-shaped free region (a strip each side of the line, joined along the gap); its
  stand point sits on one strip against the post, so a crossing out on the other strip lies behind the post (live:
  step 1 :walk to the cell past the post, body pressed on the post at x .065, :stuck, breed :unreachable)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.planner-fixture :as pf :refer [world near run]]
            [engine.stairs-physics-test :as sp]))

(def x0 10)
(def x1 21)
(def z0 5)
(def z1 16)

(defn ring-cells
  "The [x z] of a 12x12 ring round x 10..21, z 5..16, less the gaps."
  [gaps]
  (for [x (range x0 (inc x1)) z (range z0 (inc z1))
        :when (or (= x x0) (= x x1) (= z z0) (= z z1))
        :when (not (contains? (set gaps) [x z]))]
    [x z]))

(defn pen
  "The planner's world: the ring of oak fence at y 64 (the fixture joins the posts)."
  [gaps]
  (world {:blocks (mapv (fn [[x z]] [x 64 z "oak_fence"]) (ring-cells gaps))}))

(defn stand-lo [{:keys [y h]}] (+ y (/ h 16)))

(defn leg-free?
  "Whether the body can walk in a straight line from a to b ({:x :z} world) standing at lo."
  [snapshot lo a b]
  (.segmentFree ^js @pf/space snapshot @pf/table (clj->js a) (clj->js b) lo (+ lo 1.8)))

(defn blocked-legs
  "The legs of the plan the body cannot walk straight: [step-index from to]."
  [snapshot r]
  (let [steps (get-in r [:path :steps])]
    (vec (for [[i [prev step]] (map-indexed vector (partition 2 1 steps))
               :let [lo (max (stand-lo prev) (stand-lo step))
                     from {:x (:px prev) :z (:pz prev)}
                     to {:x (:px step) :z (:pz step)}
                     via (when (some? (:cx step)) {:x (:cx step) :z (:cz step)})
                     legs (if via [[from via] [via to]] [[from to]])
                     [a b] (first (remove (fn [[a b]] (leg-free? snapshot lo a b)) legs))]
               :when a]
           [(inc i) a b]))))

(def cases
  "[gaps from goal]: a 1-wide gap in the west or east side approached from outside or across the pen, a 2-wide gap, a
  gap in the north side; goals in the rows of the posts beside the gap (the live failure) and in the gap's row"
  [[[[10 11]] [6 64 11] [14 64 10]]
   [[[10 11]] [6 64 10] [14 64 10]]
   [[[10 11]] [6 64 12] [14 64 12]]
   [[[10 11]] [6 64 11] [14 64 11]]
   [[[21 11]] [25 64 11] [17 64 10]]
   [[[21 11]] [25 64 10] [17 64 10]]
   [[[21 11]] [25 64 12] [17 64 12]]
   [[[21 11]] [15 64 11] [25 64 10]]
   [[[21 11]] [15 64 10] [25 64 10]]
   [[[10 11] [10 12]] [6 64 11] [14 64 10]]
   [[[10 11] [10 12]] [6 64 12] [14 64 13]]
   [[[21 11] [21 12]] [25 64 11] [17 64 10]]
   [[[15 5]] [15 64 1] [14 64 9]]
   [[[15 5]] [15 64 1] [16 64 6]]])

(deftest every-leg-into-a-pen-through-its-gap-is-free
  (doseq [[gaps [x y z] [gx gy gz]] cases]
    (let [w (pen gaps)
          r (run w (near gx gy gz) {} {:x x :y y :z z})]
      (is (= "found" (:status r)) (pr-str gaps [x z] [gx gz]))
      (is (= [] (blocked-legs w r)) (pr-str gaps [x z] [gx gz])))))

(deftest the-live-layout-plans-a-free-way-in-with-range-2
  (let [w (pen [[10 11]])
        r (run w (near 14 64 10 2) {} {:x 6 :y 64 :z 11})]
    (is (= "found" (:status r)))
    (is (= [] (blocked-legs w r)))))

(deftest a-pen-with-no-gap-has-no-way-in
  (is (not= "found" (:status (run (pen []) (near 14 64 10) {} {:x 6 :y 64 :z 11})))))

;; ---- the body walking it, in prismarine-physics ----

(def SIDES {"east" [1 0] "west" [-1 0] "south" [0 1] "north" [0 -1]})

(defn physics-fills
  "Fills for the physics walk: a stone floor (top y 64) over x 0..30, z -2..24 and the ring, each post joined to its
  neighbours on the ring as the server joins them."
  [gaps]
  (let [ring (set (ring-cells gaps))]
    (into [[0 63 -2 30 63 24 "stone" {}]]
          (map (fn [[x z]]
                 [x 64 z x 64 z "oak_fence"
                  (into {} (map (fn [[side [dx dz]]] [side (contains? ring [(+ x dx) (+ z dz)])])) SIDES)]))
          ring)))

(deftest the-body-walks-into-a-pen-through-its-gap
  (doseq [[gaps from goal] cases]
    (let [{:keys [done pose]} (sp/walk (physics-fills gaps) from goal)]
      (is (= :arrived (:status done)) (pr-str gaps from goal done (select-keys pose [:x :z]))))))
