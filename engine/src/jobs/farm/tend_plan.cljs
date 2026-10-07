(ns jobs.farm.tend-plan
  "Plan mode of jobs.farm.tend: the plan's crop and ground cells, the facts and census read from them, and the warns
  for what the access rules refuse and the seeds that are short."
  (:require [clojure.string :as str]
            [jobs.farm.permit :as permit]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]
            [jobs.farm.fertilize :as fertilize]
            [jobs.lib.crops :as crops]
            [jobs.farm.harvest :as harvest]
            [jobs.farm.plant :as plant]
            [jobs.farm.tidy :as tidy]
            [jobs.farm.till :as till]
            [jobs.farm.tend-stock :as stock]
            [plan.shape :as shape]
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
  (let [wants (into {} (map (fn [{:keys [pos want]}] [(harvest/cell-pos pos) want])) (:cells answer))]
    (into {} (keep (fn [[pos crop]]
                     (let [g (update pos :y dec)]
                       (when (or (not (contains? wants g)) (farmland-want? (wants g)))
                         [g crop]))))
          crops)))

(defn planned
  "With :plan, {:answer :crops {pos crop}} when the plan can be worked, else {:trouble text} (warned once per reason);
  nil without :plan."
  [c]
  (when-let [id (:plan (:args c))]
    (let [part (:part (:args c))
          answer (known/plan c id)
          crops (harvest/crop-cells answer part)
          trouble (or (harvest/plan-trouble answer crops)
                      (when (nil? (known/zones c)) "no zone list has been read"))]
      (if-not trouble
        {:answer answer :crops crops}
        (do (ctx/warn-once! c [id trouble] :farm-tend.declined
                            {:plan id :part part :reason trouble
                             :text (str "tend declines plan " id (when part (str " part " part)) ": " trouble)})
            {:trouble trouble})))))

(defn ground-cell
  "{:pos :name :above} of a ground cell."
  [p pos]
  {:pos pos :name (u/block-name p pos) :above (u/block-name p (update pos :y inc))})

(defn unripe-planned
  "The planned cells holding their crop, not yet ripe."
  [p cells]
  (filterv (fn [[pos crop]]
             (let [b (u/block-at p pos)]
               (and b (= crop (.-name b)) (some-> (.-age b) (< (crops/ripe-age crop))))))
           cells))

(defn wrong-crops
  "[{:pos :found :want}] of the crop cells that hold a crop of another kind."
  [p crops]
  (vec (keep (fn [[{:keys [x y z]} crop]]
               (let [found (u/block-name p {:x x :y y :z z})]
                 (when (and found (tidy/crop-blocks found) (not (contains? (shape/crop-names crop) found)))
                   {:pos [x y z] :found found :want (shape/want-text {:crop crop})})))
             crops)))

(defn seed-reserve
  "{seed count}: twice the planned cells of each crop."
  [cells]
  (into {} (for [[crop n] (frequencies (vals cells))] [(crops/seed-of crop) (* 2 n)])))

(defn note-refused!
  "One farm-tend.refused warn per job when the rules refuse the hoe on untilled ground cells of the plan for a
  social reason: names the zones, claims and plans and the owners that refuse, so a run with nothing else to do is
  never silent. Returns the refused positions."
  [c plan cells]
  (let [verdicts (into [] (keep (fn [pos] (let [v (permit/permit c plan :dig pos)]
                                            (when (gate/refused? v) [pos v]))))
                       cells)
        vs (map second verdicts)
        fields {:zones (vec (distinct (keep :zone vs))) :claims (vec (distinct (keep :claim vs)))
                :plans (vec (distinct (keep :plan vs))) :owners (vec (distinct (keep :owner vs)))}]
    (when (seq verdicts)
      (ctx/warn-once! c [plan :refused] :farm-tend.refused
                      (assoc fields :plan plan :count (count verdicts)
                             :text (str "tend of " plan " leaves " (count verdicts) " untilled cells: refused by "
                                        (access/refusal-text fields)
                                        (when (seq (:owners fields)) (str " (owner " (str/join ", " (:owners fields)) ")"))))))
    (mapv first verdicts)))

(defn plan-facts
  "What the decisions are made from for a plan, read live."
  [c]
  (let [p (:primitives c)
        {:keys [plan fertilize keep]} (:args c)
        {:keys [answer crops]} (planned c)
        inventory (u/inventory p)
        have (plant/sowable inventory)
        me (u/self-pos c)
        tried (:till-tried (ctx/mem c) #{})
        bare (harvest/planned-bare p crops)
        bare-of (frequencies (map :seed bare))
        sowing (plant/sowing c crops)
        [mid R] (harvest/plan-field crops)
        keep (stock/keeps (seed-reserve crops) inventory keep)
        untilled-cells (->> (ground-cells answer crops)
                            (remove (fn [[pos _]] (tried pos)))
                            (filter (fn [[pos _]] (stock/untilled? (ground-cell p pos)))))
        _ (note-refused! c plan (map key untilled-cells))
        tillable (filter (fn [[pos _]] (permit/ok? c plan :dig pos)) untilled-cells)
        seeded (filter (fn [[_ crop]] (let [seed (harvest/seed-of crop)] (> (get have seed 0) (get bare-of seed 0)))) tillable)]
    {:mid mid
     :radius R
     :ripe (count (harvest/planned-ripe p {} crops []))
     :hoe (some? (till/hoe-of p))
     :untilled (->> seeded (map key) (sort-by #(u/dist me %)) vec)
     :till-short (- (count tillable) (count seeded))
     :bare (- (count bare) (count (:refused sowing)))
     :seed (boolean (seq (:ready sowing)))
     :short (:short sowing)
     :meal (boolean (fertilize/has-meal? p))
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
    {:crops (count (filter (fn [[pos crop]] (contains? (shape/crop-names crop) (u/block-name p pos))) crops))
     :bare (count (harvest/planned-bare p crops))
     :untilled (count (filter #(stock/untilled? (ground-cell p %)) (keys (ground-cells answer crops))))
     :wrong (wrong-crops p crops)
     :short (into #{} (comp (map :seed) (remove have)) (harvest/planned-bare p crops))}))
