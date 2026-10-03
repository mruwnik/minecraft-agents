(ns jobs.survival.breathe
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.triggers.suffocating :as s]))

(def doc
  "Get air. Drowning (in water, oxygen below :min-oxygen, head under water):
  when the own column reaches air within :reach blocks up, swim (the swim
  primitive rises to the surface); else walk sideways at the feet's height to
  the nearest column within :radius that does, and swim from there next round.
  Enclosed (head cell
  holds a suffocating block, see engine.triggers.suffocating): dig the head
  block, dig the block above it if solid, and step up; a dig that is not
  dug/missing (cannot, timeout, unreachable) is a failed round and nothing
  moves. One action per round;
  done as soon as the situation is gone: the head is clear, and in water
  either oxygen is back at :min-oxygen or the head is in air. Gives up (:no_air or
  :no_way_out warn) after three failed rounds; the suffocating trigger then fires it again.")

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

(defn status [r] (.-status r))

(defn ^:async swim-up!
  "Drowning: swim up the own column when it reaches air, else walk (through
  water, at the feet's height) to the nearest column that does. True when the
  action succeeded; nil when there is no air in reach (failure counted)."
  [c]
  (let [{:keys [radius reach]} (:args c)
        p (:primitives c)
        pos (u/self-pos c)
        fx (js/Math.floor (:x pos))
        fy (js/Math.floor (:y pos))
        fz (js/Math.floor (:z pos))
        target (nearest-air p pos radius reach)]
    (cond
      (nil? target) nil
      (surface-in-column p fx fz fy reach)
      (= "surfaced" (status (await (ctx/act c :swim #js {}))))

      :else
      (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos {:x (:x target) :y fy :z (:z target)}
                                                                :range 0}))))))))

(defn ^:async dig-out!
  "Enclosed: dig the head block (and the one above if solid), step up. True
  when the dig worked; false when it did not (nothing moved)."
  [c]
  (let [p (:primitives c)
        head (s/head-cell (.self p))
        above (update head :y inc)
        dug (status (await (ctx/act c :dig (clj->js {:pos head}))))]
    (if-not (contains? #{"dug" "missing"} dug)
      false
      (do (when (solid-at? p above)
            (await (ctx/act c :dig (clj->js {:pos above}))))
          (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos head :range 0})))))))))

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
          (let [drowning? (= :drowning why)
                ok (await (if drowning? (swim-up! c) (dig-out! c)))]
            (cond
              (nil? (s/situation (:primitives c) min-oxygen)) :done
              (and drowning? ok) :continue
              drowning? (u/fail! c :no_air "drowning and no air within reach")
              :else (u/fail! c :no_way_out "could not dig out of the block")))))))
