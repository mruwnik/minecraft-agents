(ns jobs.farm.tend-plan
  "Plan mode of jobs.farm.tend: the plan's crop and ground cells, the facts and census read from them, and the warns
  for what the access rules refuse and the seeds that are short."
  (:require [clojure.string :as str]
            [jobs.farm.permit :as permit]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]
            [jobs.lib.crops :as crops]
            [jobs.farm.field :as fld]
            [jobs.farm.plant :as plant]
            [jobs.farm.till :as till]
            [jobs.lib.farm :as farm]
            [jobs.lib.reserve :as reserve]
            [jobs.farm.tidy :as tidy]
            [jobs.farm.tend-stock :as stock]
            [jobs.farm.tend-box :as box]
            [plan.shape :as shape]
            [jobs.lib.plan-field :as pf]
            [jobs.lib.world :as known]))

(defn farmland-want?
  "Whether a plan want accepts farmland: the block, a block map of it, or an :any naming it."
  [want]
  (cond
    (string? want) (= "farmland" want)
    (map? want) (= "farmland" (:block want))
    (vector? want) (boolean (some farmland-want? (rest want)))
    :else false))

(defn ground-cells
  "{ground-pos crop}: the plan's farmland under the crop cells crops {pos crop}, the cell below each unless the plan
  (answer) names it with a want that is not farmland."
  [answer crops]
  (let [wants (into {} (map (fn [{:keys [pos want]}] [(u/cell-pos pos) want])) (:cells answer))]
    (into {} (keep (fn [[pos crop]]
                     (let [g (update pos :y dec)]
                       (when (or (not (contains? wants g)) (farmland-want? (wants g)))
                         [g crop]))))
          crops)))

(defn planned
  "With :plan, {:answer :crops {pos crop}} when the plan can be worked, else {:trouble text} (warned once per reason);
  nil without :plan."
  [c]
  (when (:plan (:args c))
    (let [{:keys [cells] :as r} (pf/read-plan c {:cells fld/crop-cells :what "crop cells" :zones? true
                                                  :event :farm-tend.declined :label "tend"})]
      (cond-> r cells (assoc :crops cells)))))

(defn ground-cell
  "{:pos :name :above} of a ground cell."
  [p pos]
  {:pos pos :name (u/seen-name p pos) :above (u/seen-name p (update pos :y inc))})

(defn unripe-planned
  "The planned cells holding their crop, not yet ripe."
  [p cells]
  (filterv (fn [[pos crop]]
             (let [b (u/seen-block p pos)]
               (and b (= crop (.-name b)) (some-> (.-age b) (< (crops/ripe-age crop))))))
           cells))

(defn wrong-crops
  "[{:pos :found :want}] of the crop cells that hold a crop of another kind."
  [p crops]
  (vec (keep (fn [[{:keys [x y z]} crop]]
               (let [found (u/seen-name p {:x x :y y :z z})]
                 (when (and found (tidy/crop-blocks found) (not (contains? (shape/crop-names crop) found)))
                   {:pos [x y z] :found found :want (shape/want-text {:crop crop})})))
             crops)))

(defn seed-reserve
  "{seed count}: twice the planned cells of each crop."
  [cells]
  (into {} (for [[crop n] (frequencies (vals cells))] [(crops/seed-of crop) (* 2 n)])))

(defn refusers
  "What refuses the hoe on untilled ground cells of the plan for a social reason: {:refused n :zones :claims :plans
  :owners}, or nil when nothing is refused. It names them so a run with nothing else to do says why."
  [c plan cells]
  (let [vs (keep (fn [pos] (let [v (permit/permit c plan :dig pos)] (when (gate/refused? v) v))) cells)]
    (when (seq vs)
      {:refused (count vs) :zones (vec (distinct (keep :zone vs))) :claims (vec (distinct (keep :claim vs)))
       :plans (vec (distinct (keep :plan vs))) :owners (vec (distinct (keep :owner vs)))})))

(defn reserve-held
  "What the food reserve keeps of the seeds of the crops of cells {pos crop}: {:seed :held :kept :reserve-points :cells}
  for the first seed carried but kept, else nil."
  [c cells]
  (some (fn [seed]
          (when-let [h (plant/held-back seed c)]
            (assoc h :cells (count (filter #(= seed (crops/seed-of (val %))) cells)))))
        (distinct (map (comp crops/seed-of val) cells))))

(defn untilled-cells
  "The [pos crop] ground cells of the plan's crop cells that are untilled and not yet tried by a till child."
  [c]
  (let [p (:primitives c)
        {:keys [answer crops]} (planned c)
        tried (:till-tried (ctx/mem c) #{})]
    (->> (ground-cells answer crops)
         (remove (fn [[pos _]] (tried pos)))
         (filter (fn [[pos _]] (stock/untilled? (ground-cell p pos)))))))

(defn plan-ground-boxes
  "The boxes (as box/ground-box) irrigate works over for the plan's ground cells: per y layer, tiles of at most
  till/max-cells cells that hold a ground cell."
  [answer crops]
  (let [side (max 1 (long (js/Math.floor (js/Math.sqrt (till/max-cells)))))]
    (vec (for [[y ps] (sort-by key (group-by :y (keys (ground-cells answer crops))))
               :let [x0 (apply min (map :x ps)) z0 (apply min (map :z ps))]
               [_ cs] (sort-by key (group-by (fn [{:keys [x z]}] [(quot (- x x0) side) (quot (- z z0) side)]) ps))]
           {:min {:x (apply min (map :x cs)) :y y :z (apply min (map :z cs))}
            :max {:x (apply max (map :x cs)) :y y :z (apply max (map :z cs))}}))))

(defn dry-box
  "[box dry-count] of the first plan ground box not yet tried this pass with a cell irrigate would wet, nil when none."
  [c answer crops]
  (some (fn [box] (let [n (box/irrigate-need c box)] (when (pos? n) [box n])))
        (remove #(contains? (:irrigate-tried (ctx/mem c) #{}) (:min %)) (plan-ground-boxes answer crops))))

(defn plan-facts
  "What the decisions are made from for a plan, read live."
  [c]
  (let [p (:primitives c)
        {:keys [plan fertilize keep]} (:args c)
        {:keys [answer crops]} (planned c)
        inventory (u/inventory p)
        have (farm/sowable c)
        me (u/self-pos c)
        bare (fld/planned-bare p crops)
        bare-of (frequencies (map :seed bare))
        sowing (plant/sowing c crops)
        [mid R] (fld/plan-field crops)
        keep (stock/keeps (seed-reserve crops) (reserve/held c :tend) keep)
        untilled (untilled-cells c)
        tillable (filter (fn [[pos _]] (permit/ok? c plan :dig pos)) untilled)
        seeded (filter (fn [[_ crop]] (let [seed (crops/seed-of crop)] (> (get have seed 0) (get bare-of seed 0)))) tillable)
        seeded-set (set (map key seeded))
        unseeded (remove (comp seeded-set key) tillable)
        wet (filter (fn [[pos _]] (farm/wet? p pos)) unseeded)
        dry (remove (set wet) unseeded)
        tilled-cells (concat seeded wet)
        [ground-box dry-n] (when (:irrigate (:args c)) (dry-box c answer crops))]
    {:mid mid
     :ground-box ground-box
     :dry (or dry-n 0)
     :radius R
     :ripe (count (fld/planned-ripe p {} crops []))
     :hoe (some? (farm/hoe-of p))
     :untilled (->> tilled-cells (map key) (sort-by #(u/dist me %)) vec)
     :till-short (count dry)
     :till-held (reserve-held c dry)
     :refusers (refusers c plan (map key untilled))
     :bare (- (count bare) (count (:refused sowing)))
     :seed (boolean (seq (:ready sowing)))
     :short (:short sowing)
     :meal (boolean (farm/has-meal? c))
     :unripe (if fertilize (count (unripe-planned p crops)) 0)
     :keep keep
     :waste (stock/surplus inventory keep stock/waste-seeds)
     :stored (stock/surplus inventory keep stock/farm-goods)}))

(defn note-short!
  "One farm-tend.short-seed warn per seed the plan wants sown that is not carried (once per job and seed)."
  [c seeds]
  (doseq [seed (sort seeds)]
    (ctx/warn-once! c [(:plan (:args c)) :short seed] :farm-tend.short-seed
                    {:plan (:plan (:args c)) :seed seed
                     :text (str "tend of " (:plan (:args c)) ": no " seed " carried for the bare cells that want it")})))

(defn plan-census
  "The live {:crops :bare :untilled :wrong} of the plan's field, and the seeds short for its bare cells as :short."
  [c]
  (let [p (:primitives c)
        {:keys [answer crops]} (planned c)
        crops (or crops {})
        have (set (map :name (u/inventory p)))]
    {:crops (count (filter (fn [[pos crop]] (contains? (shape/crop-names crop) (u/seen-name p pos))) crops))
     :bare (count (fld/planned-bare p crops))
     :untilled (count (filter #(stock/untilled? (ground-cell p %)) (keys (ground-cells answer crops))))
     :wrong (wrong-crops p crops)
     :short (into #{} (comp (map :seed) (remove have)) (fld/planned-bare p crops))}))
