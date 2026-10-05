(ns engine.jobs.reach
  "Whether a hostile mob is a real danger to the body: a melee mob that has a
  walkable way to the body (a bounded search over the blocks as a zombie
  walks: one step up, up to three down, doors only when open, water swum),
  and a ranged mob (skeleton and the like) that has a line of fire (it is in
  sight). A mob walled off, across a pit it cannot climb out of, or with the
  body sealed in, is not."
  (:require [clojure.string :as str]
            [engine.jobs.combat :as combat]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]))

(def node-budget
  "Cells one search expands before it gives up and answers 'unknown'."
  1500)

(def max-drop 3)

(defn openable? [name]
  (or (str/ends-with? name "_door") (str/ends-with? name "_fence_gate") (str/ends-with? name "_trapdoor")))

(defn open-prop? [b]
  (let [v (some-> b .-properties .-open)]
    (or (true? v) (= "true" v))))

(defn kind-of
  "What a block block is to a walker: :open, :water or :solid. An unloaded cell (nil) is open."
  [b]
  (let [name (some-> b .-name)]
    (cond
      (nil? name) :open
      (= "water" name) :water
      (#{"lava" "fire" "soul_fire" "magma_block" "cactus" "sweet_berry_bush"} name) :solid
      (openable? name) (if (open-prop? b) :open :solid)
      (str/ends-with? name "_leaves") :solid
      (sh/solid? name) :solid
      :else :open)))

(defn lookup
  "A function cell -> kind over primitives p, caching each cell for one query."
  [p]
  (let [cache (volatile! {})]
    (fn [[x y z]]
      (let [k [x y z]]
        (if-let [hit (get @cache k)]
          hit
          (let [v (kind-of (.blockAt p #js {:x x :y y :z z}))]
            (vswap! cache assoc k v)
            v))))))

(defn passable? [kind-at c] (not= :solid (kind-at c)))

(defn standable?
  "Feet and head cells free and something to stand on (a floor, or water)."
  [kind-at [x y z]]
  (and (passable? kind-at [x y z]) (passable? kind-at [x (inc y) z])
       (or (= :water (kind-at [x y z]))
           (#{:solid :water} (kind-at [x (dec y) z])))))

(def dirs [[1 0] [-1 0] [0 1] [0 -1]])

(defn forward
  "The cells a walker standing at [x y z] steps to in one move."
  [kind-at [x y z]]
  (mapcat
   (fn [[dx dz]]
     (let [nx (+ x dx) nz (+ z dz)]
       (cond
         (and (= :solid (kind-at [nx y nz])) (passable? kind-at [x (+ y 2) z]) (standable? kind-at [nx (inc y) nz]))
         [[nx (inc y) nz]]
         (and (passable? kind-at [nx y nz]) (passable? kind-at [nx (inc y) nz]))
         (loop [k 0]
           (cond
             (> k max-drop) []
             (standable? kind-at [nx (- y k) nz]) [[nx (- y k) nz]]
             (passable? kind-at [nx (- y k) nz]) (recur (inc k))
             :else []))
         :else [])))
   dirs))

(defn backward
  "The standable cells from which a walker steps to c (forward's edges reversed)."
  [kind-at [x y z :as c]]
  (for [[dx dz] dirs
        ny (range (- y 1) (+ y max-drop 2))
        :let [n [(+ x dx) ny (+ z dz)]]
        :when (and (standable? kind-at n) (some #{c} (forward kind-at n)))]
    n))

(defn heuristic [[x y z] [tx ty tz]]
  (+ (js/Math.abs (- x tx)) (js/Math.abs (- z tz)) (* 0.5 (js/Math.abs (- y ty)))))

(defn near-cell?
  "Whether c is within one block horizontally (diagonal included) and one vertically of target."
  [[x y z] [tx ty tz]]
  (and (<= (+ (* (- x tx) (- x tx)) (* (- z tz) (- z tz))) 2) (<= (js/Math.abs (- y ty)) 1)))

(defn search
  "Best-first search from start over (next-cells cell) to a cell near target:
  :found, :closed (every reachable cell expanded, no way) or :budget."
  [next-cells start target]
  (loop [open (sorted-set [(heuristic start target) 0 start])
         seen #{start}
         n 0]
    (cond
      (empty? open) :closed
      (>= n node-budget) :budget
      :else
      (let [[_ g c :as top] (first open)
            open (disj open top)]
        (if (near-cell? c target)
          :found
          (let [fresh (remove seen (next-cells c))]
            (recur (into open (map (fn [x] [(+ (inc g) (heuristic x target)) (inc g) x])) fresh)
                   (into seen fresh)
                   (inc n))))))))

(defn walkable-way?
  "Whether a walker at the mob's cell can reach the body's. A search from the
  mob; if it runs out of budget, one from the body (which settles a body
  sealed in or on a small island); unknown counts as a way. With a set of
  cells solid, those cells count as solid blocks (what the way would be were
  they filled)."
  ([p mob-pos body-pos] (walkable-way? p mob-pos body-pos #{}))
  ([p mob-pos body-pos solid]
   (let [base (lookup p)
         kind-at (if (empty? solid) base (fn [c] (if (contains? solid c) :solid (base c))))
         mob (let [{:keys [x y z]} (sh/cell mob-pos)] [x y z])
         body (let [{:keys [x y z]} (sh/cell body-pos)] [x y z])
         r (search (partial forward kind-at) mob body)]
     (case r
       :found true
       :closed false
       (not= :closed (search (partial backward kind-at) body mob))))))

(defn danger?
  "Whether hostile e (JS entity) is a real danger to the body of primitives p.
  A ranged mob needs a line of fire (in sight); a melee mob needs a walkable
  way to the body and, unless :sight? is false, to be in sight."
  ([p e] (danger? p e {}))
  ([p e {:keys [sight?] :or {sight? true}}]
   (let [seen? (true? (.-visible e))]
     (if (combat/ranged? e)
       seen?
       (and (or seen? (not sight?))
            (walkable-way? p (u/pos-of (.-pos e)) (u/pos-of (.-pos (.self p)))))))))

(defn dangers
  "The hostiles of combat/hostiles (same opts) that are real dangers, nearest first."
  ([p radius opts] (dangers p radius opts {}))
  ([p radius opts danger-opts]
   (filterv #(danger? p % danger-opts) (combat/hostiles p radius (dissoc opts :sight)))))
