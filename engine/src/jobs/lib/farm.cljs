(ns jobs.lib.farm
  "Helpers the farm jobs share: which cells are bare, which seeds may be sown, and the hoe and bone meal a body carries."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.crops :as crops]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.util :as u]))

(declare settings)

(a/defargs settings
  {::water-reach {:default 4 :doc "Blocks from water within which farmland is hydrated." :spec (a/int-in 0 nil)}})

(defn water-reach [] (settings/get settings ::water-reach))

(defn hydrated?
  "True when a water cell of water-set (a set of [x y z]) lies within ::water-reach in x and
  z of the farmland at x y z, at its y or one above."
  [water-set x y z]
  (boolean (some (fn [[wx wy wz]]
                   (and (<= (js/Math.abs (- wx x)) (water-reach))
                        (<= (js/Math.abs (- wz z)) (water-reach))
                        (or (= wy y) (= wy (inc y)))))
                 water-set)))

(defn wet?
  "Whether the farmland at {:x :y :z} is hydrated by water the body has seen (see hydrated?)."
  [p {:keys [x y z]}]
  (let [r (water-reach)]
    (boolean (some #(= "water" (u/seen-name p %))
                   (for [dx (range (- r) (inc r)) dz (range (- r) (inc r)) dy [0 1]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))))

(defn bare-cells
  "The bare cells of box (farmland with air above, ground layer y = min y) not in skipped, in x then z order."
  [p box skipped]
  (let [skipped (set skipped)
        {:keys [min max]} box
        y (:y min)]
    (->> (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))] {:x x :y y :z z})
         (remove skipped)
         (filter #(and (= "farmland" (u/seen-name p %))
                       (= "air" (u/seen-name p (update % :y inc))))))))

(defn sowable
  "{name count} of the carried items that may be sown: what the reserve register leaves for :sow (food crops only
  above the food reserve, nothing an agent entry holds). A cell just harvested is replanted by jobs.farm.harvest and ignores the reserve."
  [c]
  (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} (reserve/free c :sow)))

(defn pick-seed
  "The seed to sow given the inventory: seed when it may be sown, else the largest sowable stack of a known seed;
  nil when none. Only what sowable leaves counts."
  [seed c]
  (let [counts (sowable c)
        carried #(get counts % 0)]
    (if seed
      (when (pos? (carried seed)) seed)
      (->> (vals crops/seed-of)
           (filter #(pos? (carried %)))
           (sort-by #(- (carried %)))
           first))))

(defn hoe-of
  "The name of a carried item ending in _hoe, or nil."
  [p]
  (some #(when (.endsWith (:name %) "_hoe") (:name %)) (u/inventory p)))

(defn has-meal?
  "Whether bone meal is carried that the reserve register leaves for :fertilize."
  [c]
  (some #(= "bone_meal" (:name %)) (reserve/free c :fertilize)))
