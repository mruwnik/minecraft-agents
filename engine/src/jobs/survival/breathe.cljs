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
  holds a suffocating block, see engine.triggers.suffocating): first, once per
  job, step to a horizontal neighbour at the same feet height whose feet and
  head cells are passable and which has something to stand on (not air, water
  or lava); if that does not clear the situation (:side-tried in job memory)
  the next round digs: dig the head
  block, dig the block above it if solid, and step up; a dig that is not
  dug/missing (cannot, timeout, unreachable) is a failed round and nothing
  moves. One action per round;
  the situation is gone once the head is clear, and in water either oxygen is
  back at :min-oxygen or the head is in air. After a swim that surfaced
  (:surfaced in job memory) a body still in water heads for land instead of
  bobbing: it swims toward the nearest land cell within :shore-radius (feet y
  from one below to two above the own; feet and head cells air, the cell below solid,
  i.e. not air, water, lava, fire or magma); done when it stands out of the
  water, a failed round (:no_shore warn after three) when the swim does not
  get it out, and a clean decline (info :no_shore_near, no warn) when no land
  is in reach. Gives up (:no_air or :no_way_out warn) after three failed
  rounds; the suffocating trigger then fires it again.")

(def args
  {:min-oxygen {:doc "oxygen (of 20) below which being in water with the head submerged is drowning"
                :default s/default-min-oxygen}
   :radius {:doc "columns this far sideways are searched for air" :default 2}
   :reach {:doc "blocks above the feet the search climbs" :default 10}
   :shore-radius {:doc "after surfacing, land this many blocks sideways is walked to" :default 6}})

(def breathe-policy {:cap 20 :ttl (* 60 60 1000)})

(defn surfaced-in-water?
  "A swim surfaced earlier in this job and the body is still in water. A ctx
  without job memory (no :view) has not surfaced."
  [c]
  (boolean (and (:view c)
                (:surfaced (ctx/mem c))
                (.-inWater (.self (:primitives c))))))

(defn check [c]
  (or (some? (s/situation (:primitives c) (:min-oxygen (:args c))))
      (surfaced-in-water? c)))

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
            feet (u/block-name p {:x x :y y :z z})
            head (u/block-name p {:x x :y (inc y) :z z})]
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

(defn solid-at? [p cell] (s/suffocates? p cell))

(defn status [r] (.-status r))

(def unsafe-below #{"water" "lava" "fire" "soul_fire" "magma_block"})

(defn land-cell?
  "A feet cell on land: feet and head air, and a solid block below (anything
  but air, water, lava, fire or magma; an unloaded cell is not land)."
  [p cell]
  (let [feet (u/block-name p cell)
        head (u/block-name p (update cell :y inc))
        below (u/block-name p (update cell :y dec))]
    (boolean (and feet head below
                  (s/air? feet) (s/air? head)
                  (not (s/air? below)) (not (contains? unsafe-below below))))))

(defn nearest-land
  "The nearest land cell within radius sideways of self-pos, feet y from one
  below to two above; nil if none."
  [p self-pos radius]
  (let [fx (js/Math.floor (:x self-pos))
        fy (js/Math.floor (:y self-pos))
        fz (js/Math.floor (:z self-pos))]
    (->> (for [[dx dz] (columns radius)
               dy [0 -1 1 2]]
           {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)})
         (filter #(land-cell? p %))
         first)))

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
      (let [surfaced? (= "surfaced" (status (await (ctx/act c :swim #js {}))))]
        (when surfaced? (ctx/update-mem! c assoc :surfaced true))
        surfaced?)

      :else
      (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos {:x (:x target) :y fy :z (:z target)}
                                                                :range 0}))))))))

(defn side-cell
  "A horizontal neighbour of the feet cell where the body fits (feet and head
  cells passable) and can stand (the cell below is neither air, water nor
  lava); nil if none. Unloaded cells do not count."
  [p self]
  (let [fx (js/Math.floor (.. self -pos -x))
        fy (js/Math.floor (.. self -pos -y))
        fz (js/Math.floor (.. self -pos -z))
        fits? (fn [cell]
                (let [feet (u/block-name p cell)
                      head (u/block-name p (update cell :y inc))
                      below (u/block-name p (update cell :y dec))]
                  (and feet head below
                       (not (s/suffocates? p cell)) (not (s/suffocates? p (update cell :y inc)))
                       (not (s/air? below)) (not (contains? #{"water" "lava"} below)))))]
    (->> [[1 0] [-1 0] [0 1] [0 -1]]
         (map (fn [[dx dz]] {:x (+ fx dx) :y fy :z (+ fz dz)}))
         (filter fits?)
         first)))

(defn ^:async dig-out!
  "Enclosed: dig the head block (and the one above if solid), step up. True
  when the dig worked; false when it did not (nothing moved)."
  [c]
  (let [p (:primitives c)
        head (s/eye-cell (.self p))
        above (update head :y inc)
        dug (status (await (ctx/act c :dig (clj->js {:pos head}))))]
    (if-not (contains? #{"dug" "missing"} dug)
      false
      (do (when (solid-at? p above)
            (await (ctx/act c :dig (clj->js {:pos above}))))
          (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos head :range 0})))))))))

(defn ^:async head-for-land!
  "Surfaced and still in water: swim toward the nearest land cell within
  :shore-radius (the swim primitive with toward climbs out onto a rim the
  pathfinder cannot path to). :done when out of the water or when no land is in reach
  (info :no_shore_near); a failed round otherwise."
  [c]
  (let [p (:primitives c)
        radius (:shore-radius (:args c))
        target (nearest-land p (u/self-pos c) radius)]
    (if-not target
      (do (ctx/emit! c :no_shore_near :info {:radius radius :text "no land within reach of the surfaced body"})
          :done)
      (let [r (await (ctx/act c :swim (clj->js {:toward target})))]
        (if (or (= "landed" (status r)) (not (.-inWater (.self p))))
          :done
          (u/fail! c :no_shore "could not reach the nearest shore"))))))

(defn note!
  "Write the :breathe entry once per job instance."
  [c why]
  (when-not (:noted (ctx/mem c))
    (let [self (.self (:primitives c))]
      (ctx/remember! c :breathe {:why why :pos (u/pos-of (.-pos self)) :oxygen (.-oxygen self)} breathe-policy)
      (ctx/update-mem! c assoc :noted true))))

(defn after-situation
  "The situation is gone: keep going while surfaced in water (next round heads
  for land), else done."
  [c]
  (if (surfaced-in-water? c) :continue :done))

(defn ^:async round [c]
  (let [min-oxygen (:min-oxygen (:args c))
        why (s/situation (:primitives c) min-oxygen)]
    (cond
      (and (nil? why) (surfaced-in-water? c)) (await (head-for-land! c))
      (nil? why) :done

      :else
      (let [p (:primitives c)
            side (when (and (= :enclosed why) (not (:side-tried (ctx/mem c))))
                   (side-cell p (.self p)))]
        (note! c why)
        (if side
          (do (ctx/update-mem! c assoc :side-tried true)
              (await (ctx/act c :moveTo (clj->js {:pos side :range 0})))
              (if (nil? (s/situation p min-oxygen)) (after-situation c) :continue))
          (let [drowning? (= :drowning why)
                ok (await (if drowning? (swim-up! c) (dig-out! c)))]
            (cond
              (nil? (s/situation p min-oxygen)) (after-situation c)
              (and drowning? ok) :continue
              drowning? (u/fail! c :no_air "drowning and no air within reach")
              :else (u/fail! c :no_way_out "could not dig out of the block"))))))))
