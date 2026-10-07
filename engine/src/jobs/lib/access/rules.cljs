(ns jobs.lib.access.rules
  "Pure rules: may the body dig, or place a block, at a cell. No sensing, no side effects. The caller reads
  the world into a lookup and acts on the verdict.

  Input, one map:
    :block-at    fn [x y z] -> block name, or nil when the cell is not loaded. The caller's read: stair and
                 tunnel give what the body senses and a guess for a cell it has not seen; other jobs still read the world
    :cell        [x y z] the cell to dig or fill
    :feet        [x y z] the body's feet cell (its head cell is one above)
    :zones       nil (no zone list loaded) or a vector of zones {:name :min :max :owner :allow}
    :footprints  cells other plans claim: a set, or a map {cell plan-id} (what jobs.lib.world/footprints gives),
                 in which case a :footprint refusal names the :plan
    :claims      the active area claims (jobs.lib.world/claims)
    :self        the body's name
    :now         the clock in ms
    :ignore-zones?  true skips the zone, claim, footprint and no-zone-list checks (a job's opt-out). The
                 physical rules still apply.
    :ledger      set of [x y z] cells holding this body's own scaffold blocks (may be empty)
  The social part (zones, claims, footprints, no zone list) is jobs.lib.access.zones/verdict.

  Output: {:ok true}, or {:ok false :reason kw ...detail}. A dig with hazards is {:ok true :hazards [{:reason
  kw ...detail} ...]}; :hazards is absent when empty.

  The rules say what is impossible or not permitted and only report what is dangerous. The job decides which
  danger it accepts (see accepts?).
  - Impossible or unknown: :not-loaded, :own-body, :not-replaceable (+ :block)
  - Not permitted (jobs.lib.access.zones): :footprint (+ :plan), :zone (+ :zone :owner), :claim (+ :claim
    :owner), :no-zones
  - Hazards of a dig, in this order: one :fluid-adjacent (+ :fluid :at) per neighbouring fluid cell,
    :falling-block (+ :block :at), :under-feet

  The first failing check is the verdict, so the most specific reason wins and :no-zones, the general one,
  shows only when nothing else is wrong. A caller that overrides it for an emergency can rely on that.
    dig:   :not-loaded, :footprint, :zone, :no-zones, then the hazards
    place: :not-loaded, :footprint, :zone, :own-body, :not-replaceable, :no-zones
  A cell holding air, water, lava or a bubble column can be placed into."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [jobs.lib.access.zones :as zones]))

(def air #{"air" "cave_air" "void_air"})

(def fluids #{"water" "lava" "bubble_column"})

(def replaceable
  "Names a placed block takes the place of: air, fluids (plugging a source, bridging water, sealing a leak) and the
  plants and snow a placement overwrites."
  (into (into air fluids) #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow" "vine" "glow_lichen" "leaf_litter" "hanging_roots"}))

(def item-block
  "Items whose placed block is not the block of the same name: seeds and root crops place a crop block, and the
  wheat item is no block (nil) though a wheat crop block exists."
  {"wheat" nil "wheat_seeds" "wheat" "carrot" "carrots" "potato" "potatoes" "beetroot_seeds" "beetroots"
   "melon_seeds" "melon_stem" "pumpkin_seeds" "pumpkin_stem" "torchflower_seeds" "torchflower_crop"
   "pitcher_pod" "pitcher_crop"})

(defn no-collision?
  "True when the block that item places has an empty collision shape in minecraft-data for version, so the body
  may stand in the cell: seeds, saplings, torches, flowers, crops. Carpets count too (a 1/16 layer). False for an
  item that places no block."
  [version item]
  (let [block (if (contains? item-block item) (item-block item) item)
        shape (some-> block (->> (aget (.-blocksByName (minecraft-data version)))) .-boundingBox)]
    (boolean (and block (or (= "empty" shape) (and shape (str/ends-with? block "_carpet")))))))

(def not-a-floor
  "Non-fluid, non-replaceable names that are no floor to stand on after the block above is gone."
  #{"magma_block" "powder_snow" "cobweb" "fire" "soul_fire" "cactus" "sweet_berry_bush"})

(def falling-names #{"sand" "red_sand" "gravel"})

(defn falling? [n]
  (boolean (or (falling-names n) (some-> n (str/ends-with? "_concrete_powder")))))

(defn offset [[x y z] dx dy dz] [(+ x dx) (+ y dy) (+ z dz)])

(def neighbour-deltas [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn solid-floor?
  "True when the cell holds a known block that is solid and safe to stand on."
  [block-at pos]
  (let [n (block-at pos)]
    (boolean (and n (not (replaceable n)) (not (fluids n)) (not (not-a-floor n))))))

(defn in-box? [[x y z] {[x0 y0 z0] :min [x1 y1 z1] :max}]
  (and (<= x0 x x1) (<= y0 y y1) (<= z0 z z1)))

(defn refuse [reason & {:as detail}]
  (assoc detail :ok false :reason reason))

(defn fluid-neighbours
  "[name pos] of every water, lava or bubble column among the six neighbours of cell, in neighbour-deltas order."
  [block-at cell]
  (keep (fn [d] (let [pos (apply offset cell d) n (block-at pos)] (when (fluids n) [n pos])))
        neighbour-deltas))

(defn falls-on-body?
  "A falling block directly above cell drops through it. Returns [name pos] of that block when the fall runs down the
  body's column to its head: the cell is in the body's column at or above the head, and every cell between is
  open (air or an unloaded cell)."
  [block-at [x y z :as cell] [fx fy fz]]
  (let [above (offset cell 0 1 0)
        n (block-at above)
        head (inc fy)]
    (when (and (falling? n) (= x fx) (= z fz) (>= y head)
               (every? #(let [b (block-at [x % z])] (or (nil? b) (air b))) (range (inc head) y)))
      [n above])))

(defn verdict
  "The first failing check among checks (fns returning a refusal map or nil), else {:ok true}."
  [checks]
  (or (some #(%) checks) {:ok true}))

(defn social-verdict
  "jobs.lib.access.zones/verdict for action over the rules input. A nil zone list counts as empty here (the
  no-zones refusal comes last, see no-zones-check). A footprint given as a set refuses without a :plan.
  :plan-cells, the cells of the plan the job builds, let it work over a foreign zone or claim (ok :plan)."
  [action {:keys [zones footprints cell] :as in}]
  (let [v (zones/verdict (assoc (select-keys in [:claims :self :now :plan-cells]) :zones (or zones []) :footprints footprints
                                :action action :cell cell))]
    (cond-> v
      (set? footprints) (dissoc :plan))))

(defn common-checks [action {:keys [block-at cell ignore-zones?] :as in}]
  [#(when (nil? (block-at cell)) (refuse :not-loaded))
   #(when-not ignore-zones?
      (let [v (social-verdict action in)]
        (when-not (:ok v) v)))])

(defn no-zones-check [{:keys [zones ignore-zones?]}]
  #(when (and (nil? zones) (not ignore-zones?)) (refuse :no-zones)))

(defn dig-hazards
  "Every hazard of digging :cell, as maps {:reason kw ...detail}, in check order. Hazards are reported, never refused:
  the job decides which it accepts. The cell under the body's feet is a hazard unless the cell below it is a known
  solid floor (not magma) or the cell is in the body's own ledger: stairs, not shafts. The floor is read by
  :floor-at (a cell not seen is nil) when given, else block-at."
  [{:keys [block-at floor-at cell feet ledger]}]
  (let [[fx fy fz] feet]
    (vec (concat
          (for [[n at] (fluid-neighbours block-at cell)] {:reason :fluid-adjacent :fluid n :at at})
          (filter some?
                  [(when-let [[n at] (falls-on-body? block-at cell feet)] {:reason :falling-block :block n :at at})
                   (when (and (= cell [fx (dec fy) fz])
                              (not (contains? ledger cell))
                              (not (solid-floor? (or floor-at block-at) (offset cell 0 -1 0))))
                     {:reason :under-feet})])))))

(defn may-dig?
  "Verdict for digging the block at :cell. See the namespace docstring for the input and the output. Refusals are
  :not-loaded, :footprint, :zone, :claim and :no-zones; hazards come back in :hazards on an {:ok true} verdict."
  [in]
  (let [refusal (verdict (concat (common-checks :dig in) [(no-zones-check in)]))]
    (if-not (:ok refusal)
      refusal
      (let [hazards (dig-hazards in)]
        (cond-> {:ok true}
          (seq hazards) (assoc :hazards hazards))))))

(defn accepts?
  "True when verdict is ok and every hazard it reports has a reason in accepted (a set of reason keywords):
  (accepts? v #{:fluid-adjacent}); (accepts? v #{}) accepts no hazard at all."
  [verdict accepted]
  (boolean (and (:ok verdict) (every? (comp accepted :reason) (:hazards verdict)))))

(defn may-place?
  "Verdict for placing a block at :cell. See the namespace docstring."
  [{:keys [block-at cell feet] :as in}]
  (let [[fx fy fz] feet]
    (verdict
     (concat
      (common-checks :place in)
      [#(when (contains? #{[fx fy fz] [fx (inc fy) fz]} cell) (refuse :own-body))
       #(let [n (block-at cell)] (when-not (replaceable n) (refuse :not-replaceable :block n)))
       (no-zones-check in)]))))
