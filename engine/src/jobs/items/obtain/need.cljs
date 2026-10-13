(ns jobs.items.obtain.need
  "Whether a source would start: viability of the gather and tool sources, and the tool a mined need wants."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.items.get-tool :as get-tool]
            [jobs.items.obtain.base :refer [explore-setting limits]]
            [jobs.items.obtain.chest :refer [candidates]]
            [jobs.items.obtain.plan :refer [craft-plan gather-plan]]
            [jobs.items.obtain.seen :refer [gather-slot seen-of with-block]]
            [jobs.items.obtain.table :refer [gather-needs]]
            [jobs.lib.tools :as tools]
            [jobs.lib.trees :as trees]))

(declare tools-wanted explorable? gather-viable?)

(defn tool-viable?
  "Whether a source of the obtain child that gets a tool for need would start: a usable seen chest, a craft plan, or a
  gather of its own (recursion ends at the wooden pickaxe, whose logs need no tool)."
  [c names need]
  (let [tools (tools-wanted c names need)
        how (:how (limits c))]
    (boolean (or (and (contains? how :chest) (seq (candidates c tools)))
                 (and (contains? how :craft) (craft-plan c tools 1))
                 (and (contains? how :gather) (or (gather-viable? c tools 1) (explorable? c tools 1)))))))

(defn gather-viable?
  "Whether a chain is craftable once raw items are gathered, and a block for every raw need has been seen and
  the child of the first would run (or, when no carried tool harvests its block, the tool's obtain child would)."
  [c names n]
  (when-let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))) seq)]
    (and (every? #(seen-of c %) needs)
         (let [need (with-block c (first needs))]
           (boolean (if (tools-wanted c names need)
                      (tool-viable? c names need)
                      (ctx/check-child c (gather-slot need) (:job need) (:args need))))))))

(defn explore-opts
  "The search options of :explore, or nil when it is off."
  [c]
  (let [e (:explore (:args c))]
    (cond (map? e) (merge (explore-setting) e)
          e (explore-setting))))

(defn search-names
  "The block names a search for a need looks for."
  [need]
  (if (set? (:seen need)) (vec (sort (:seen need))) trees/log-names))

(defn explorable?
  "Whether :explore is on and a raw need of the chain is not seen: the search child solves it, when the tool that
  harvests its block (an unseen ore is mined by the pickaxe for its block) can be had (tool-viable?)."
  [c names n]
  (boolean (and (explore-opts c)
                (some #(and (not (:sheep %)) (not (seen-of c %))
                            (or (not (tools-wanted c names %)) (tool-viable? c names %)))
                      (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))))))))

(defn tools-wanted
  "For a mined need (dirt is dug by hand) whose block the carried tools do not harvest, with :craft allowed: the tools
  that do, cheapest first (wooden_pickaxe for stone, stone_pickaxe for iron_ore), less those being fetched above; else nil."
  [c names need]
  (let [p (:primitives c)
        block (:block (:args need))
        tools (when (and (= 'jobs.gather.mine (:job need)) (not= "dirt" (:key need)) block
                         (contains? (:how (limits c)) :craft) (not (tools/can-harvest? p block)))
                (get-tool/tools-for p {:block block}))]
    (when (vector? tools)
      (not-empty (vec (remove (set (concat names (:chain (:args c)))) tools))))))

(defn pickaxe-first? [c names need] (boolean (tools-wanted c names need)))

(defn gather-held?
  "Whether every raw need of the chain is seen and the first one's child declined with a wait reason of its own
  (night in a shelter ..), left standing as the job's: the source is there, only not now."
  [c names n]
  (let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))) seq)]
    (boolean (and needs (every? #(seen-of c %) needs) (some-> (:wait c) deref)))))
