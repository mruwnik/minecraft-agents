(ns jobs.lib.forestry
  "Helpers the forestry jobs share: reading the tree cells of a forest plan, the soil a species takes and the plan's status."
  (:require [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.plan-field :as pf]))

(defn cell-vec [{:keys [x y z]}] [x y z])

(defn feet-cell [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn tree-cells
  "{pos species} of the cells of a plan answer (of part, when given) that want a tree."
  [answer part]
  (into {} (keep (fn [{:keys [pos want] :as cell}]
                   (when (and (map? want) (string? (:tree want)) (or (nil? part) (= part (:part cell))))
                     [(u/cell-pos pos) (:tree want)])))
        (:cells answer)))

(def soil #{"dirt" "grass_block" "coarse_dirt" "podzol" "rooted_dirt" "moss_block"})

(defn soil-for [species]
  (if (= "mangrove" species) (into soil ["mud" "clay" "muddy_mangrove_roots"]) soil))

(defn down [pos] (update pos :y dec))

(def trouble-status pf/plan-can-run?)

(defn field-cells
  "The planned tree cells of field and the ground under them, as {:x :y :z}."
  [field]
  (mapcat (fn [pos] [pos (update pos :y dec)]) (keys (:trees field))))

(defn ^:async survey-field!
  "Before the first step, look around once when a cell of field was never seen: :continue when it looked, else nil."
  [c field]
  (when (and (not (:begun (ctx/mem c))) (look/unseen? (:primitives c) (field-cells field)))
    (await (look/survey! c))))

(defn planned
  "{:trees {pos species} :planned #{[x y z]}} for the plan of c's args, or {:trouble text :later?}."
  [c]
  (let [{:keys [answer cells] :as r} (pf/read-plan c {:cells tree-cells :what "tree cells" :zones? true})]
    (if cells
      {:trees cells :planned (set (map (comp vec :pos) (:cells answer)))}
      r)))
