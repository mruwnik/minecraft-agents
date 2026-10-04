(ns engine.access.rules
  "Pure rules: may the body dig, or place a block, at a cell. No sensing, no side effects; the caller reads the world
  into a lookup and acts on the verdict.

  Input, one map:
    :block-at    fn [x y z] -> block name string, or nil when the cell is not loaded
    :cell        [x y z] the cell to dig or fill
    :feet        [x y z] the body's feet cell (its head cell is one above)
    :zones       nil (no zone list loaded) or a vector of zones. Assumed zone shape, to be adapted when the zone file
                 format is decided: {:name \"farm\" :min [x y z] :max [x y z] :allow #{:dig :place}}. The box is
                 inclusive. :allow is the set of actions the zone permits inside it; a zone without :allow permits
                 none. A zone is a keep-out box for every action it does not allow.
    :footprints  the [x y z] cells that other plans claim (may be empty): a set, or a map {cell plan-id} (what
                 engine.ctx/footprints gives), whose :footprint refusal then names the :plan
    :ledger      set of [x y z] cells holding this body's own scaffold blocks (may be empty)
  Output: {:ok false :reason kw ...detail} for a refusal, else {:ok true}, and for a dig that has hazards
  {:ok true :hazards [{:reason kw ...detail} ...]} (:hazards is absent when empty).

  The rules say what is impossible or not permitted and only report what is dangerous; the job decides which
  danger it accepts (see accepts?).
  Refused, impossible or unknown: :not-loaded, :own-body, :not-replaceable (+ :block).
  Refused, not permitted: :footprint (+ :plan id, for a footprint map), :zone (+ :zone name), :no-zones.
  Hazards of a dig, all that apply, in this order: one :fluid-adjacent (+ :fluid :at) per neighbouring fluid cell, :falling-block (+ :block :at),
  :under-feet.
  Refusals run in this order and the first is the verdict, so the most specific reason wins and :no-zones, the
  general one, only shows when nothing else is wrong (a caller that overrides it for an emergency can rely on
  that):
    dig:   :not-loaded, :footprint, :zone, :no-zones, then the hazards
    place: :not-loaded, :footprint, :zone, :own-body, :not-replaceable, :no-zones
  A cell holding air, water, lava or a bubble column is placeable."
  (:require [clojure.string :as str]))

(def air #{"air" "cave_air" "void_air"})

(def fluids #{"water" "lava" "bubble_column"})

(def replaceable
  "Names a placed block takes the place of: air, fluids (plugging a source, bridging water, sealing a leak) and the
  plants and snow a placement overwrites."
  (into (into air fluids) #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow" "vine" "glow_lichen"}))

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

(defn blocking-zone
  "The first zone holding cell that does not allow action, or nil."
  [zones cell action]
  (first (filter #(and (in-box? cell %) (not (contains? (:allow %) action))) zones)))

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

(defn common-checks [action {:keys [block-at cell footprints zones]}]
  [#(when (nil? (block-at cell)) (refuse :not-loaded))
   #(when (contains? footprints cell)
      (cond-> (refuse :footprint) (map? footprints) (assoc :plan (get footprints cell))))
   #(when-let [z (blocking-zone zones cell action)] (refuse :zone :zone (:name z)))])

(defn no-zones-check [{:keys [zones]}]
  #(when (nil? zones) (refuse :no-zones)))

(defn dig-hazards
  "Every hazard of digging :cell, as maps {:reason kw ...detail}, in check order. Hazards are reported, never refused:
  the job decides which it accepts. The cell under the body's feet is a hazard unless the cell below it is a known
  solid floor (not magma) or the cell is in the body's own ledger: stairs, not shafts."
  [{:keys [block-at cell feet ledger]}]
  (let [[fx fy fz] feet]
    (vec (concat
          (for [[n at] (fluid-neighbours block-at cell)] {:reason :fluid-adjacent :fluid n :at at})
          (filter some?
                  [(when-let [[n at] (falls-on-body? block-at cell feet)] {:reason :falling-block :block n :at at})
                   (when (and (= cell [fx (dec fy) fz])
                              (not (contains? ledger cell))
                              (not (solid-floor? block-at (offset cell 0 -1 0))))
                     {:reason :under-feet})])))))

(defn may-dig?
  "Verdict for digging the block at :cell. See the namespace docstring for the input and the output. Refusals are
  :not-loaded, :footprint, :zone and :no-zones; hazards come back in :hazards on an {:ok true} verdict."
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
