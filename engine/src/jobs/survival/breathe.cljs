(ns jobs.survival.breathe
  (:require [jobs.lib.tidy :as tidy]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [triggers.survival.suffocating :as s]))

(def doc
  "Get air when drowning or stuck inside a block, then out of the water, in one run (a reflex; never yields).
  Each pass reads the world again:
  - Drowning (in water, head under, oxygen below :min-oxygen): swim up the own column when it reaches air within
    :reach blocks, else step sideways, at the feet's height, to the nearest column within :radius that does.
  - Enclosed (head cell holds a suffocating block, see triggers.survival.suffocating): step to a side cell with room
    to stand, once; then dig the head block, the block above it if solid, and step up.
  - Surfaced and still in water: swim to the nearest shore cell (land with its rim at most one block above the water)
    within :shore-radius, another direction after each failed swim; else walk to land within :far-radius.
  - Afloat (no way out found): hold jump (job.holding :afloat) for afloat-holds holds, then stopped :no_land; a body
    with no job sinks and the trigger fires again. One :afloat or :no_shore warning per spot (body memory
    :breathe-afloat, 5 min).
  Completed when the head is clear and, after a swim, the body stands on solid ground out of the water. Stopped
  :no_air or :no_way_out (with a warn) after three failed tries in the run.
  Memory: one :breathe entry per run.")

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
(def far-walks "Walks to far land one run makes before the body counts as afloat." 2)
(def afloat-holds "Holds one run keeps an afloat body up (about 20 s) before it stops :no_land." 4)
(def max-passes "Passes one run makes before it stops :no_way_out, against a world that never changes." 40)
(def afloat-near "Blocks around a spot warned afloat about in which a later run does not warn again." 16)
(def afloat-policy {:cap 5 :ttl (* 5 60 1000)})

(def breathe-policy {:cap 20 :ttl (* 60 60 1000)})

(declare unsafe-below)

(defn land-footing?
  "The block under the feet cell is solid ground: not air, water, lava, fire or magma, and loaded. A body pressed
  against a wall over water is onGround for a tick without it."
  [p self]
  (let [below (u/block-name p {:x (js/Math.floor (.. self -pos -x)) :y (dec (js/Math.floor (.. self -pos -y)))
                               :z (js/Math.floor (.. self -pos -z))})]
    (boolean (and below (not (s/air? below)) (not (contains? unsafe-below below))))))

(defn on-land?
  "Out of the water and standing on solid ground. A swim or walk that ends on a jump crest against a wall over water
  is out of the water for that moment, without a footing."
  [p]
  (let [self (.self p)]
    (and (not (.-inWater self)) (.-onGround self) (land-footing? p self))))

(defn surfaced-in-water?
  "A swim surfaced earlier in this job and the body is still in water, or not standing on land yet: a body bobbing
  at the surface or climbing out is out of the water for a moment on every crest, and one pressed against a wall
  over water is onGround without a footing. A ctx without job memory (no :view) has not surfaced."
  [c]
  (boolean (and (:view c)
                (:surfaced (ctx/mem c))
                (not (on-land? (:primitives c))))))

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

(defn same-way?
  "cell lies within 45 degrees of a failed target as seen from the offset (dx, dz): a swim toward that way already
  failed (a wall), so its neighbours would too."
  [dx dz failed fx fz]
  (some (fn [t]
          (let [tx (- (:x t) fx) tz (- (:z t) fz)
                dot (+ (* dx tx) (* dz tz))]
            (and (pos? dot)
                 (>= (* dot dot) (* 0.5 (+ (* dx dx) (* dz dz)) (+ (* tx tx) (* tz tz)))))))
        failed))

(defn shore-cell?
  "A land cell a swimming body can climb onto: a side neighbour has room (feet and head cells air or water) over water
  one or two blocks below the land cell's feet, so its rim is at most one block above the water. Land behind a wall,
  or on a wall two above the water, is not one."
  [p cell]
  (some (fn [[dx dz]]
          (let [n (-> cell (update :x + dx) (update :z + dz))
                at (fn [dy] (u/block-name p (update n :y + dy)))]
            (and (passable-water-or-air? (at 0)) (passable-water-or-air? (at 1))
                 (or (= "water" (at -1)) (= "water" (at -2))))))
        [[1 0] [-1 0] [0 1] [0 -1]]))

(defn nearest-land
  "The nearest land cell within radius sideways of self-pos, feet y from one below to two above, that keep? accepts;
  nil if none. Cells in the direction of a failed target (a seq of cells) are skipped."
  ([p self-pos radius] (nearest-land p self-pos radius nil (constantly true)))
  ([p self-pos radius failed keep?]
   (let [fx (js/Math.floor (:x self-pos))
         fy (js/Math.floor (:y self-pos))
         fz (js/Math.floor (:z self-pos))]
     (->> (for [[dx dz] (columns radius)
                :when (not (same-way? dx dz failed fx fz))
                dy [0 -1 1 2]]
            {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)})
          (filter #(and (land-cell? p %) (keep? p %)))
          first))))

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
  "One hold: declare the :afloat hold and keep jump pressed for hold-ticks physics ticks (one steer act), which holds
  a body at the surface (with no input it sinks). Stopped :no_land after afloat-holds holds in the run, else :held."
  [c]
  (ctx/hold-still! c :afloat)
  (await (ctx/act c :steer (walk/steer-args hold-timeout-s (hold-decider))))
  (let [holds (inc (:holds (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :holds holds)
    (cond
      (on-land? (:primitives c)) :again
      (< holds afloat-holds) :held
      :else (result/stop! c :no_land "afloat with no land in reach; fires again if it sinks"))))

(defn near-afloat-spot?
  "An earlier run warned about being afloat within afloat-near blocks of pos, within afloat-policy's ttl."
  [c pos]
  (some (fn [{:keys [data]}]
          (let [q (:pos data)
                dx (- (:x pos) (:x q)) dz (- (:z pos) (:z q))]
            (<= (+ (* dx dx) (* dz dz)) (* afloat-near afloat-near))))
        (ctx/entries c :breathe-afloat)))

(defn warn-afloat!
  "Warn kind with fields once per spot: not when an earlier run warned near here (body memory :breathe-afloat)."
  [c kind fields]
  (let [pos (u/self-pos c)]
    (when-not (near-afloat-spot? c pos)
      (ctx/emit! c kind :warn fields)
      (ctx/remember! c :breathe-afloat {:pos pos :kind kind} afloat-policy))))

(defn afloat!
  "The body cannot get out of the water: remember it in job memory (later passes only hold) and, when why is given,
  warn :afloat once per spot."
  [c why]
  (ctx/update-mem! c assoc :afloat true)
  (when why
    (warn-afloat! c :afloat {:why why :text (str "afloat in water, no way out: " (name why))})))

(defn ^:async walk-to-far-land!
  "Walk to the nearest land within :far-radius with the walk driver. :done when it ended on land; :again for another
  pass after a walk that arrived in water (far-walks per run), else after marking the body afloat (no land, no
  pathWorld, or the driver found no way)."
  [c]
  (let [p (:primitives c)
        target (when (walk/path-world p) (nearest-land p (surface-pos p (u/self-pos c) (:reach (:args c))) (:far-radius (:args c))))]
    (if-not target
      (do (afloat! c :no-land-in-reach) :again)
      (let [{:keys [result]} (await (walk/walk-to! c {:to [(:x target) (:y target) (:z target)] :range 0
                                                      :weight walk/default-weight :timeout-s far-timeout-s}))
            walks (inc (:far-walks (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :far-walks walks)
        (cond
          (on-land? p) :done
          (and (= :arrived (:status result)) (< walks far-walks)) :again
          :else (do (afloat! c (or (:reason result) (:status result))) :again))))))

(defn give-up-shore!
  "Every shore in reach failed (each failed swim excluded its direction): warn :no_shore once per spot; the body is
  afloat."
  [c]
  (warn-afloat! c :no_shore {:tries (count (:failed-shores (ctx/mem c))) :text "could not reach a shore"})
  (afloat! c nil)
  :again)

(defn ^:async head-for-land!
  "Surfaced and still in water. Afloat already: hold. Else swim toward the nearest shore cell within :shore-radius,
  then the next nearest in another direction after each failed swim, until every shore failed (the swim primitive
  with toward climbs out onto a rim the pathfinder cannot path to); with none, walk to land within :far-radius; a
  body that cannot get out is afloat. :done when out of the water."
  [c]
  (let [p (:primitives c)
        failed (:failed-shores (ctx/mem c))
        target (when-not (:afloat (ctx/mem c))
                 (nearest-land p (surface-pos p (u/self-pos c) (:reach (:args c))) (:shore-radius (:args c)) failed shore-cell?))]
    (cond
      (:afloat (ctx/mem c)) (await (hold-afloat! c))
      (and (not target) (seq failed)) (give-up-shore! c)
      (not target) (await (walk-to-far-land! c))
      :else
      (let [r (await (ctx/act c :swim (clj->js {:toward target})))]
        (if (or (= "landed" (status r)) (on-land? p))
          :done
          (do (ctx/update-mem! c update :failed-shores (fnil conj []) target)
              :again))))))

(defn note!
  "Write the :breathe entry once per job instance."
  [c why]
  (when-not (:noted (ctx/mem c))
    (let [self (.self (:primitives c))]
      (ctx/remember! c :breathe {:why why :pos (u/pos-of (.-pos self)) :oxygen (.-oxygen self)} breathe-policy)
      (ctx/update-mem! c assoc :noted true))))

(defn fail!
  "One failed try in this run: :again until u/max-failures, then the warn kind and stopped with reason kind."
  [c kind text]
  (let [tries (inc (:failures (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :failures tries)
    (if (< tries u/max-failures)
      :again
      (do (ctx/emit! c kind :warn {:tries tries :text text})
          (result/stop! c kind text)))))

(defn ^:async pass!
  "One look at the world and one try. :again or :held for another pass, else :done (perhaps stopped)."
  [c]
  (let [p (:primitives c)
        min-oxygen (:min-oxygen (:args c))
        why (s/situation p min-oxygen)]
    (cond
      (and (nil? why) (surfaced-in-water? c)) (await (head-for-land! c))
      (nil? why) :done

      :else
      (let [side (when (and (= :enclosed why) (not (:side-tried (ctx/mem c))))
                   (side-cell p (.self p)))]
        (ctx/hold-still! c nil)
        (note! c why)
        (if side
          (do (ctx/update-mem! c assoc :side-tried true)
              ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
              (await (ctx/act c :moveTo (clj->js {:pos side :range 0})))
              :again)
          (let [drowning? (= :drowning why)
                ok (await (if drowning? (swim-up! c) (dig-out! c)))]
            (cond
              (nil? (s/situation p min-oxygen)) :again
              (and drowning? ok) :again
              drowning? (fail! c :no_air "drowning and no air within reach")
              :else (fail! c :no_way_out "could not dig out of the block"))))))))

(defn ^:async round [c]
  (loop [i 0]
    (cond
      (not (ctx/alive? c)) :done
      (<= max-passes i) (result/stop! c :no_way_out "still not out after many tries")
      :else (let [r (await (pass! c))]
              (case r
                :held (recur (inc i))
                :again (do (await (pace/pace!)) (recur (inc i)))
                r)))))
