(ns jobs.lib.strays
  "What a plan does not want in its cells: the strays a tidy job clears, read over a block lookup. Pure."
  (:require [engine.contract :as contract]
            [jobs.lib.crops :as crops]
            [jobs.lib.tidy-rules :as tr]
            [plan.shape :as shape]))

(def crop-blocks
  (into (set (keys crops/ripe-age))
        #{"melon_stem" "pumpkin_stem" "attached_melon_stem" "attached_pumpkin_stem"
    "melon" "pumpkin" "sugar_cane" "bamboo" "bamboo_sapling" "sweet_berry_bush" "nether_wart" "cocoa" "torchflower_crop"
    "pitcher_crop"}))

(def ground-blocks #{"grass_block" "podzol" "mycelium"})

(defn crop-want? [want] (and (map? want) (contains? want :crop)))
(defn tree-want? [want] (and (map? want) (contains? want :tree)))

(defn classify
  "What the world's block (nil: nobody has seen it) is in a cell with this want: nil (nothing for this job), :dig,
  :wrong (reported, never dug) or [:kept why]. A want is a plan want, or :headroom for the air above a crop cell."
  [want block]
  (let [n (:name block)]
    (cond
      (or (nil? block) (shape/air? n)) nil
      (tree-want? want) nil
      (and (crop-want? want) (contains? (shape/crop-names (:crop want)) n)) nil
      (or (= :clear want) (= :headroom want) (crop-want? want))
      (if-let [why (tr/keep-why n)]
        [:kept why]
        (cond
          (= "farmland" n) :wrong
          (and (crop-blocks n) (= :headroom want)) nil
          (and (crop-blocks n) (crop-want? want)) :wrong
          (and (ground-blocks n) (crop-want? want)) :wrong
          :else :dig))
      :else (when (= :wrong (shape/judge want block)) :wrong))))

(defn work-cells
  "The cells the job looks at: the plan's cells (of part when given) [{:pos :want ...}] and the cell above each crop
  cell that no cell of the plan names, as {:pos :want :headroom}."
  [cells part]
  (let [claimed (set (map :pos cells))
        mine (filterv #(or (nil? part) (= part (:part %))) cells)
        heads (->> mine (filter #(crop-want? (:want %))) (map #(update (:pos %) 1 inc)) (remove claimed) distinct)]
    (into mine (map (fn [pos] {:pos pos :want :headroom})) heads)))

(defn want-text [want] (if (= :headroom want) "air" (shape/want-text want)))

(defn strays
  "{:dig [{:pos :block}] :kept [{:pos :block :why}] :wrong [{:pos :found :want}]} of the cells, read through block-at
  (a fn [pos] -> {:name n :state s} or nil)."
  [cells block-at]
  (reduce (fn [acc {:keys [pos want]}]
            (let [block (block-at pos)
                  verdict (classify want block)]
              (cond
                (nil? verdict) acc
                (= :dig verdict) (update acc :dig conj {:pos pos :block (:name block)})
                (= :wrong verdict) (update acc :wrong conj {:pos pos :found (:name block) :want (want-text want)})
                :else (update acc :kept conj {:pos pos :block (:name block) :why (second verdict)}))))
          {:dig [] :kept [] :wrong []}
          cells))

(defn skips
  "[[pos reason] ...] of the cells (maps {:x :y :z}) a jobs.blocks.clear-cells child ending e left alone: the cells it
  skipped, or all of them when it declined to start (a zone or plan footprint refusing them is named)."
  [e cells]
  (if (:data e)
    (map (fn [[pos reason]] [(select-keys pos [:x :y :z]) reason]) (get-in e [:data :skipped]))
    (let [cause (:cause e)
          reason (cond (seq (:plans cause)) :footprint
                       (seq (:zones cause)) :zone
                       :else (or (contract/cause-key e) :refused))]
      (map (fn [pos] [pos reason]) cells))))
