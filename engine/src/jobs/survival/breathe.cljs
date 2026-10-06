(ns jobs.survival.breathe
  (:require [jobs.lib.tidy :as tidy]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [triggers.survival.suffocating :as s]))

(def doc
  "Get air when drowning or stuck inside a block. One action per round.
  Drowning (in water, head under, oxygen below :min-oxygen):
  - If the own column reaches air within :reach blocks up, swim (the swim primitive rises to the surface).
  - Otherwise walk sideways, at the feet's height, to the nearest column within :radius that does.
  Enclosed (head cell holds a suffocating block, see triggers.survival.suffocating):
  - First, once per job, step to a side cell with room to stand.
  - If that does not help, dig the head block, dig the block above it if solid, and step up.
  - A dig that fails (cannot, timeout, unreachable) is a failed round.
  After a swim that surfaced, a body still in water heads for land instead of bobbing:
  it swims to the nearest land cell within :shore-radius, else walks to land within :far-radius.
  Ends when the head is clear and the body stands out of the water.
  A body that cannot get out stays afloat (it holds jump for a few seconds each round) instead of ending,
  because a body with no job sinks and the trigger would fire again for ever. One :afloat warning says so.
  Gives up with a :no_air, :no_way_out or :no_shore warning after three failed rounds.
  The suffocating trigger then fires it again.
  Memory: writes one :breathe entry per job.")

(def args
  {:min-oxygen {:doc "oxygen (of 20) below which being in water with the head submerged is drowning"
                :default s/default-min-oxygen}
   :radius {:doc "columns this far sideways are searched for air" :default 2}
   :reach {:doc "blocks above the feet the search climbs" :default 10}
   :shore-radius {:doc "after surfacing, land this many blocks sideways is swum to" :default 6}
   :far-radius {:doc "after surfacing, land beyond :shore-radius up to this many blocks sideways is walked to with the walk driver"
                :default 24}})

(def hold-ticks "Physics ticks (20 per second) one afloat hold keeps jump pressed." 100)
(def hold-timeout-s "Bound of one hold's steer act, a little over its ticks." 8)
(def far-timeout-s "Bound of one walk to far land." 60)

(def breathe-policy {:cap 20 :ttl (* 60 60 1000)})

(defn surfaced-in-water?
  "A swim surfaced earlier in this job and the body is still in water, or not standing yet: a body bobbing at the
  surface or climbing out is out of the water for a moment on every crest. A ctx without job memory (no :view) has
  not surfaced."
  [c]
  (boolean (and (:view c)
                (:surfaced (ctx/mem c))
                (let [self (.self (:primitives c))]
                  (or (.-inWater self) (not (.-onGround self)))))))

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

(defn surface-pos
  "pos with y raised to the feet cell at the top of the own water column (surface-in-column), so a body that has
  sunk a few blocks below the surface still finds a bank flush with it; pos itself when the column has no surface."
  [p pos reach]
  (let [cell (surface-in-column p (js/Math.floor (:x pos)) (js/Math.floor (:z pos)) (js/Math.floor (:y pos)) reach)]
    (if cell (assoc pos :y (:y cell)) pos)))

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
      ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
      (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos {:x (:x target) :y fy :z (:z target)}
                                                                :range 0}))))))))

(def harmful-in-cell #{"lava" "fire" "soul_fire" "cactus" "sweet_berry_bush" "campfire" "soul_campfire" "magma_block"})

(defn side-cell
  "A horizontal neighbour of the feet cell where the body fits (feet and head
  cells passable and not harmful: lava, fire, cactus...) and can stand (the cell below is neither air, water, lava
  nor magma); nil if none. Unloaded cells do not count."
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
                       (not (contains? harmful-in-cell feet)) (not (contains? harmful-in-cell head))
                       (not (s/air? below)) (not (contains? #{"water" "lava" "magma_block"} below)))))]
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
        _ (access/trespass! c "breathe" (:trespass (access/choose c :dig [(cond-> [head] (solid-at? p above) (conj above))] identity)))
        dug (status (await (tidy/dig! c head true)))]
    (if-not (contains? #{"dug" "missing"} dug)
      false
      (do (when (solid-at? p above)
            (await (tidy/dig! c above true)))
          ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
          (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos head :range 0})))))))))

(defn hold-decider
  "A steer decide function for one hold: jump pressed on every tick, done after hold-ticks ticks, or at once when
  the body stands out of the water (on a bank, jump held would only make it hop for ever)."
  []
  (let [ticks (volatile! 0)]
    (fn [pose]
      (if (or (>= (vswap! ticks inc) hold-ticks)
              (and (.-onGround pose) (not (.-inWater pose))))
        #js {:done #js {}}
        #js {:controls #js {:jump true}}))))

(defn ^:async hold-afloat!
  "Keep jump pressed for hold-ticks physics ticks (one steer act), which holds a body at the surface: with no input
  it sinks, and the drowning trigger would fire again. Returns :continue."
  [c]
  (await (ctx/act c :steer (walk/steer-args hold-timeout-s (hold-decider))))
  :continue)

(defn afloat!
  "The body cannot get out of the water: remember it (later rounds only hold) and, when why is given, warn once."
  [c why]
  (ctx/update-mem! c assoc :afloat true)
  (when why
    (ctx/emit! c :afloat :warn {:why why :text (str "afloat in water, no way out: " (name why))})))

(defn ^:async walk-to-far-land!
  "Walk to the nearest land within :far-radius with the walk driver. :done when it arrived out of the water, else
  :continue after marking the body afloat (no land, no pathWorld, or the driver found no way)."
  [c]
  (let [p (:primitives c)
        target (when (walk/path-world p) (nearest-land p (surface-pos p (u/self-pos c) (:reach (:args c))) (:far-radius (:args c))))]
    (if-not target
      (do (afloat! c :no-land-in-reach) :continue)
      (let [{:keys [result]} (await (walk/walk-to! c {:to [(:x target) (:y target) (:z target)] :range 0
                                                      :weight walk/default-weight :timeout-s far-timeout-s}))]
        (cond
          (not (.-inWater (.self p))) :done
          (= :arrived (:status result)) :continue
          :else (do (afloat! c (or (:reason result) (:status result))) :continue))))))

(defn ^:async head-for-land!
  "Surfaced and still in water. Afloat already: hold. Else swim toward the nearest land cell within :shore-radius (the
  swim primitive with toward climbs out onto a rim the pathfinder cannot path to); with none, walk to land within
  :far-radius; a body that cannot get out stays afloat. :done when out of the water."
  [c]
  (let [p (:primitives c)
        target (nearest-land p (surface-pos p (u/self-pos c) (:reach (:args c))) (:shore-radius (:args c)))]
    (cond
      (:afloat (ctx/mem c)) (await (hold-afloat! c))
      (not target) (let [r (await (walk-to-far-land! c))]
                     (if (= :continue r) (await (hold-afloat! c)) r))
      :else
      (let [r (await (ctx/act c :swim (clj->js {:toward target})))]
        (if (or (= "landed" (status r)) (not (.-inWater (.self p))))
          :done
          (let [outcome (u/fail! c :no_shore "could not reach the nearest shore")]
            (if (= :done outcome)
              (do (afloat! c nil) (await (hold-afloat! c)))
              outcome)))))))

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
              ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
              (await (ctx/act c :moveTo (clj->js {:pos side :range 0})))
              (if (nil? (s/situation p min-oxygen)) (after-situation c) :continue))
          (let [drowning? (= :drowning why)
                ok (await (if drowning? (swim-up! c) (dig-out! c)))]
            (cond
              (nil? (s/situation p min-oxygen)) (after-situation c)
              (and drowning? ok) :continue
              drowning? (u/fail! c :no_air "drowning and no air within reach")
              :else (u/fail! c :no_way_out "could not dig out of the block"))))))))
