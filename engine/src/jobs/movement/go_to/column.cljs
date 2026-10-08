(ns jobs.movement.go-to.column
  "go-to's goal y. A column goal ([x z] or {:x :z}, no y) stands on the ground of that column nearest the body's feet. A
  y with no standable cell within the range is re-aimed to its column's standable cell nearest it, at most :y-slack
  blocks off. Cells read as the body sees or remembers them; a cell it never saw reads from the world the plans read
  (the raw world with :unknown off, nothing with :unknown on). A column with no ground known keeps a stand-in y (the
  body's feet, or the given y) and go-to walks toward it, aimed again every step: an unseen column resolves on approach."
  (:require [engine.args :as a]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(def column-reach "Blocks above and below the body's feet a column goal looks for ground in." 64)

(defn column-xz
  "[x z] of a column pos ([x z] or {:x :z} with no :y), else nil."
  [pos]
  (cond
    (and (sequential? pos) (= 2 (count pos))) (vec pos)
    (and (map? pos) (nil? (:y pos)) (contains? pos :x) (contains? pos :z)) [(:x pos) (:z pos)]))

(def pos-or-column?
  (a/pred "[x y z], {:x :y :z}, [x z] or {:x :z} of numbers (no y: the ground of that column)"
          #(or (some? (a/cell %)) (boolean (some->> (column-xz %) (every? a/finite?))))))

(defn parse
  "{:pos {:x :z}} (floored) for a column pos, else nil."
  [pos]
  (when-let [[x z] (column-xz pos)]
    (let [parsed (places/parse-pos [x 0 z])]
      (if (:reason parsed) parsed {:pos (dissoc (:pos parsed) :y)}))))

(defn kind
  "A block as a stand test reads it: :unknown (nil: unseen or unloaded), :hazard, else reach/kind-of."
  [b]
  (cond (nil? b) :unknown
        (reach/hazard-blocks (.-name b)) :hazard
        :else (reach/kind-of b)))

(defn kind-at
  "(fn [x y z]) -> kind of the block the body of primitives p sees or remembers there; an unseen one from the raw world
  (reach/lookup) when raw?, else :unknown."
  [p raw?]
  (let [raw (when raw? (reach/lookup p kind))
        sensing? (some? (.-sensedAt p))]
    (fn [x y z]
      (let [k (if sensing? (kind (u/seen-block p {:x x :y y :z z})) :unknown)]
        (if (and raw (keyword-identical? :unknown k)) (raw x y z) k)))))

(defn free? [k] (or (keyword-identical? :open k) (keyword-identical? :water k)))

(defn stands?
  "Feet and head cells open or water, and a solid floor under open feet."
  [kind-at x y z]
  (let [feet (kind-at x y z)]
    (and (free? feet) (free? (kind-at x (inc y) z))
         (or (keyword-identical? :water feet) (keyword-identical? :solid (kind-at x (dec y) z))))))

(defn ground
  "The y within reach of y (nearest first, the higher of two) where the column x z stands, else nil."
  [kind-at x z y within]
  (some (fn [d] (cond (stands? kind-at x (+ y d) z) (+ y d)
                      (and (pos? d) (stands? kind-at x (- y d) z)) (- y d)))
        (range 0 (inc within))))

(defn stands-near?
  "Whether a cell within range (a sphere, as the planner's goal test) of pos stands."
  [kind-at {:keys [x y z]} radius]
  (let [r (js/Math.ceil radius)]
    (boolean (some (fn [[dx dy dz]] (stands? kind-at (+ x dx) (+ y dy) (+ z dz)))
                   (for [dx (range (- r) (inc r)) dy (range (- r) (inc r)) dz (range (- r) (inc r))
                         :when (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* radius radius))]
                     [dx dy dz])))))

(defn aim
  "{:pos cell} go-to walks to for goal ({:x :y :z}, or {:x :z} for a column), with :resolved true when the y was
  chosen here. body-y: the body's feet y; slack: the :y-slack arg; raw?: whether unseen cells read from the raw world."
  [p {:keys [x y z] :as goal} radius slack body-y raw?]
  (let [ka (kind-at p raw?)]
    (cond
      (nil? y) (let [g (ground ka x z body-y column-reach)]
                 (cond-> {:pos {:x x :y (or g body-y) :z z}} g (assoc :resolved true)))
      (or (zero? slack) (stands-near? ka goal radius)) {:pos goal}
      :else (if-let [g (ground ka x z y slack)]
              {:pos (assoc goal :y g) :resolved true}
              {:pos goal}))))
