(ns jobs.items.obtain.need
  "Whether a source would start: viability of the gather and tool sources, and the tool a mined need wants."
  (:require [engine.composite :as composite]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.items.get-tool :as get-tool]
            [jobs.items.obtain.base :refer [gathers? makes? names-of way?]]
            [jobs.items.obtain.chest :refer [candidates]]
            [jobs.items.obtain.plan :refer [craft-plan gather-plan]]
            [jobs.items.obtain.seen :refer [gather-slot seen-of with-block]]
            [jobs.items.obtain.table :refer [gather-needs searchable?]]
            [jobs.lib.cost :as cost]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.trees :as trees]))

(declare tools-wanted explorable? gather-viable?)

(defn tool-viable?
  "Whether a source of the obtain child that gets a tool for need would start: a usable seen chest, a craft plan, or a
  gather of its own (recursion ends at the wooden pickaxe, whose logs need no tool)."
  [c names need]
  (let [tools (tools-wanted c names need)]
    (boolean (or (and (way? c :chest) (seq (candidates c tools)))
                 (and (makes? c) (craft-plan c tools 1))
                 (and (gathers? c) (or (gather-viable? c tools 1) (explorable? c tools 1)))))))

(defn hall-cell
  "The cell of a dark hall refusal a torch ends (jobs.lib.ready/hall-status: :exposed, lowered by :torch, :at), else nil."
  [cause]
  (let [{:keys [reason lowered-by at]} (when (map? cause) cause)]
    (when (and (= :exposed reason) (contains? lowered-by :torch)) at)))

(def light-settle-ms "How long after a torch is hung its light may take to reach what the body perceives." 3000)

(defn torch-due
  "The cell to light when a refusal of the need's child (its ending's or its start check's cause) is a dark hall a torch
  ends and no torch was hung there yet this gather; nil else. A player lights the hall, not leaves the ore."
  [c cause]
  (let [at (hall-cell cause)]
    (when (and at (not (contains? (get-in (ctx/mem c) [:gather :lit]) at))) at)))

(defn light-settling?
  "Whether the refusal is a dark hall at a cell lit less than light-settle-ms ago: the light is not perceived yet."
  [c cause]
  (let [t (some->> (hall-cell cause) (get (get-in (ctx/mem c) [:gather :lit])))]
    (boolean (and t (< (- (ctx/now c) t) light-settle-ms)))))

(defn lights-itself?
  "Whether the gather source deals with the refusal itself: a torch is due or its light settling."
  [c cause]
  (or (some? (torch-due c cause)) (light-settling? c cause)))

(defn gather-viable?
  "Whether a chain is craftable once raw items are gathered, and a block for every raw need has been seen and
  the child of the first would run (or, when no carried tool harvests its block, the tool's obtain child would)."
  [c names n]
  (when-let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))) seq)]
    (and (every? #(seen-of c %) needs)
         (let [need (with-block c (first needs))]
           (boolean (if (tools-wanted c names need)
                      (tool-viable? c names need)
                      (let [s (ctx/child-status c (gather-slot need) (:job need) (:args need))]
                        (or (composite/startable? s) (lights-itself? c (contract/status-cause s))))))))))

(defn own-worth
  "What n of names is worth to the body here, less a trip (jobs.lib.cost/find-worth)."
  [c names n]
  (cost/find-worth (cost/death-body (:primitives c) (ctx/view c)) names (u/self-pos c) n))

(defn search-worth
  "Seconds a search for the call's need is worth: :search-worth (the goal of the caller, or of the obtain whose need asked
  for this one), else :worth, else for a bill all of it (jobs.lib.cost/items-find-worth), else own-worth of :count."
  [c]
  (let [{:keys [search-worth worth bill] :as a} (:args c)]
    (or search-worth worth
        (when bill (cost/items-find-worth bill))
        (own-worth c (names-of a) (:count a 1)))))

(defn priced-search
  "The search options: search-worth as the search's :worth, which prices each walk, and that many seconds to spend. A
  worth that pays for no more than the trip is 0: the search ends :price at its first walk."
  [c]
  (let [worth (search-worth c)]
    {:worth (max 0 worth)
     :spend {:job-time (max 1 worth)}}))

(defn explore-opts
  "The search options, or nil when the :search way is off (or :explore false)."
  [c]
  (when (and (:explore (:args c)) (way? c :search))
    (priced-search c)))

(defn search-names
  "The block or animal names a search for a need looks for."
  [need]
  (cond (:sheep need) ["sheep"]
        (:mob need) [(:mob need)]
        (set? (:seen need)) (vec (sort (:seen need)))
        :else trees/log-names))

(defn trees-unreachable?
  "Whether a seen log need's child declines :no-tree (the logs seen are not ones it can fell): the search looks
  elsewhere."
  [c need]
  (and (= 'jobs.forestry.harvest-wood (:job need))
       (let [s (ctx/child-status c (gather-slot need) (:job need) (:args need))]
         (and (#{:later :look} (first s))
              (= :no-tree (:reason (contract/status-cause s)))))))

(defn explorable?
  "Whether :explore is on and a raw need of the chain is not seen, or its logs are seen but not fellable: the search
  child solves it, when the tool that harvests its block (an unseen ore is mined by the pickaxe for its block) can be
  had (tool-viable?)."
  [c names n]
  (boolean (and (explore-opts c)
                (some #(and (searchable? %) (or (not (seen-of c %)) (trees-unreachable? c (with-block c %)))
                            (or (not (tools-wanted c names %)) (tool-viable? c names %)))
                      (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))))))))

(defn tools-wanted
  "For a mined need (dirt is dug by hand) whose block the carried tools do not harvest, with :craft allowed: the tools
  that do, cheapest first (wooden_pickaxe for stone, stone_pickaxe for iron_ore), less those wanted; for a water bucket
  with no bucket carried, the bucket; else nil."
  [c names need]
  (let [p (:primitives c)
        block (:block (:args need))
        tools (when (and (= 'jobs.gather.mine (:job need)) (not= "dirt" (:key need)) block
                         (way? c :craft) (not (tools/can-harvest? p block)))
                (get-tool/tools-for p {:block block}))]
    (if (and (= "water_bucket" (:key need)) (not (some #(= "bucket" (:name %)) (u/inventory p))))
      ["bucket"]
      (when (vector? tools)
        (not-empty (vec (remove (set names) tools)))))))

(defn pickaxe-first? [c names need] (boolean (tools-wanted c names need)))

(defn gather-held
  "When every raw need of the chain is seen and the first one's child would wait for a reason of its own (night in
  a shelter ..): that reason as a cause, the source there, only not now; else nil. A need that wants a tool first
  has the wait of the tool's own chain (a pickaxe from logs held by the night shelter); a dark hall a torch ends is no
  hold (the gather lights it, lights-itself?). The cause's metadata :kind is
  the child's status kind (:later or :look)."
  [c names n]
  (let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))) seq)
        need (when (and needs (every? #(seen-of c %) needs)) (with-block c (first needs)))]
    (when need
      (if-let [tools (tools-wanted c names need)]
        (gather-held c tools 1)
        (let [s (ctx/child-status c (gather-slot need) (:job need) (:args need))]
          (when-not (or (composite/startable? s) (lights-itself? c (contract/status-cause s)))
            (with-meta (contract/status-cause s) {:kind (first s)})))))))
