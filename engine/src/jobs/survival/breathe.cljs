(ns jobs.survival.breathe
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.triggers.suffocating :as s]))

(def doc
  "Get air. Drowning (in water, oxygen below :min-oxygen, head under water):
  swim to the nearest cell whose head is in air, own column first, then
  columns up to :radius away; at most :reach blocks up. Enclosed (head cell
  holds a suffocating block, see engine.triggers.suffocating): dig the head
  block, dig the block above it if solid, and step up. One move per round;
  done as soon as the situation is gone: the head is clear, and in water
  either oxygen is back at :min-oxygen or the head is in air. Gives up after
  three rounds with no way out; the suffocating trigger then fires it again.")

(def args
  {:min-oxygen {:doc "oxygen (of 20) below which being in water with the head submerged is drowning"
                :default s/default-min-oxygen}
   :radius {:doc "columns this far sideways are searched for air" :default 2}
   :reach {:doc "blocks above the feet the search climbs" :default 10}})

(def breathe-policy {:cap 20 :ttl (* 60 60 1000)})

(defn check [c]
  (some? (s/situation (:primitives c) (:min-oxygen (:args c)))))

(defn columns
  "Column offsets within radius, the own column first, then by distance."
  [radius]
  (->> (for [dx (range (- radius) (inc radius))
             dz (range (- radius) (inc radius))
             :when (<= (+ (* dx dx) (* dz dz)) (* radius radius))]
         [dx dz])
       (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz))))))

(defn passable-water-or-air? [name]
  (or (s/air? name) (= "water" name)))

(defn surface-in-column
  "The feet cell in column x z, at or above fy and within reach, whose head
  cell is air and which is reached through water or air only; nil if the
  column is capped or leaves the loaded world."
  [p x z fy reach]
  (loop [k 0]
    (when (<= k reach)
      (let [y (+ fy k)
            feet (s/block-name p {:x x :y y :z z})
            head (s/block-name p {:x x :y (inc y) :z z})]
        (cond
          (not (and feet head (passable-water-or-air? feet))) nil
          (s/air? head) {:x x :y y :z z}
          (= "water" head) (recur (inc k))
          :else nil)))))

(defn nearest-air [p self-pos radius reach]
  (let [fx (js/Math.floor (:x self-pos))
        fy (js/Math.floor (:y self-pos))
        fz (js/Math.floor (:z self-pos))]
    (some (fn [[dx dz]] (surface-in-column p (+ fx dx) (+ fz dz) fy reach))
          (columns radius))))

(defn solid-at? [p cell]
  (let [n (s/block-name p cell)]
    (and (some? n) (not (s/passable? n)))))

(defn ^:async swim-up! [c]
  (let [{:keys [radius reach]} (:args c)
        p (:primitives c)
        target (nearest-air p (u/self-pos c) radius reach)]
    (if (nil? target)
      (u/fail! c :no_air "drowning and no air within reach")
      (do (await (ctx/act c :moveTo (clj->js {:pos target :range 0})))
          :moved))))

(defn ^:async dig-out! [c]
  (let [p (:primitives c)
        head (s/head-cell (.self p))
        above (update head :y inc)
        dug (await (ctx/act c :dig (clj->js {:pos head})))]
    (if (= "cannot" (.-status dug))
      (u/fail! c :no_way_out "the head block cannot be dug")
      (do (when (solid-at? p above)
            (await (ctx/act c :dig (clj->js {:pos above}))))
          (await (ctx/act c :moveTo (clj->js {:pos head :range 0})))
          :moved))))

(defn note!
  "Write the :breathe entry once per job instance."
  [c why]
  (when-not (:noted (ctx/mem c))
    (let [self (.self (:primitives c))]
      (ctx/remember! c :breathe {:why why :pos (u/pos-of (.-pos self)) :oxygen (.-oxygen self)} breathe-policy)
      (ctx/update-mem! c assoc :noted true))))

(defn ^:async round [c]
  (let [min-oxygen (:min-oxygen (:args c))
        why (s/situation (:primitives c) min-oxygen)]
    (if (nil? why)
      :done
      (do (note! c why)
          (let [moved (await (if (= :drowning why) (swim-up! c) (dig-out! c)))]
            (if (and (= :moved moved) (nil? (s/situation (:primitives c) min-oxygen)))
              :done
              (if (= :moved moved) :continue moved)))))))
