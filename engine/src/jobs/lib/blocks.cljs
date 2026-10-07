(ns jobs.lib.blocks
  "Shared parts of jobs.blocks.dig and jobs.blocks.place, and what a parent needs to run them as children.
  child-wait reads why a child's check would wait. body-wait? tells a reason about the body (tool, item, free slot:
  the parent waits too) from one about the cell (the parent skips that cell)."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.set :as set]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.forestry.trees :as forestry]
            [jobs.lib.util :as u]
            [jobs.lib.places :as places]
            [engine.game :as game]))

(def air #{"air" "cave_air" "void_air"})
(def fluids #{"water" "lava" "bubble_column"})

(def flowers
  #{"dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip" "white_tulip" "pink_tulip"
    "oxeye_daisy" "cornflower" "lily_of_the_valley" "torchflower" "pink_petals" "wildflowers" "short_dry_grass"
    "tall_dry_grass" "bush" "firefly_bush"})

(def clearable
  "What jobs.blocks.place digs out of a cell before placing: the replaceable plants and snow layer
  (jobs.lib.access.rules/replaceable, less air and fluids) and small flowers, which a placement does not overwrite."
  (into (set/difference rules/replaceable air fluids) flowers))

(def drops-table
  "{block name [item name ..]} of what minecraft-data lists each block drops for a version (memoised)."
  (memoize
   (fn [version]
     (let [data (minecraft-data version)]
       (into {} (map (fn [b] [(.-name b) (vec (keep #(some-> (aget (.-items data) %) .-name) (array-seq (.-drops b))))]))
             (array-seq (.-blocksArray data)))))))

(defn drops-of
  "The item names block-name drops by minecraft-data (tool and enchantment ignored), or the block's own name when
  the data does not list it."
  [p block-name]
  (get (drops-table (game/version-of p)) block-name [block-name]))

(defn parse
  "{:pos {:x :y :z}} of args' :pos (a cell, fractions floored), or {:error text}."
  [{:keys [pos]}]
  (let [parsed (when (some? pos) (places/parse-pos pos))]
    (cond
      (nil? pos) {:error "needs :pos, [x y z] or {:x :y :z}"}
      (:reason parsed) {:error (:message parsed)}
      :else {:pos (:pos parsed)})))

(defn cell [{:keys [x y z]}] [x y z])

(defn feet-cell
  "The body's feet cell [x y z]."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn in-reach?
  "Whether the block at pos can be dug or placed from where the body stands (eye to cell centre, within
  forestry/dig-reach, a margin under the primitive's 4.5)."
  [c pos]
  (<= (u/eye-dist (u/self-pos c) pos) forestry/dig-reach))

(defn rules-in
  "The rules input for this job: zones, claims, every plan's footprint but :for-plan's and the
  body's own plans', and :ignore-zones?."
  [c]
  (access/rules-input c {:except (:for-plan (:args c)) :own-plans-ok? true}))

(def social #{:zone :claim :footprint :no-zones})

(defn not-allowed
  "The wait reason for a verdict v refused by a zone, claim, plan footprint or missing zone list:
  {:reason :not-allowed :pos :by kw} plus whichever of :zone :claim :plan :owner refused. nil for any other verdict."
  [pos v]
  (when (contains? social (:reason v))
    (merge {:reason :not-allowed :pos pos :by (:reason v)} (select-keys v [:zone :claim :plan :owner]))))

(defn unreachable-wait
  "The wait reason {:reason :unreachable :pos :why} for the failed walk remembered in memory m, while the body still
  stands in the cell it gave up from. Once the body has moved, nil (it tries again)."
  [c m pos]
  (when-let [{:keys [from why]} (:unreachable m)]
    (when (= from (feet-cell c))
      {:reason :unreachable :pos pos :why why})))

(defn fresh-mem
  "Job memory m for target pos: m when it is about pos, else emptied (children too) and marked for pos. Gives a
  parent that reuses one slot a clean child per cell."
  [m pos]
  (if (= pos (:for m)) m (assoc (select-keys m [:args]) :for pos :children {})))

(defn mem-for
  "This job's memory as it is for pos (see fresh-mem), without writing it."
  [c pos]
  (fresh-mem (ctx/mem c) pos))

(def cell-reasons
  "Wait reasons about the cell that can appear mid-call (a zone added, a block moved in): the call declines."
  #{:not-allowed :hazard :not-loaded :own-body :no-support})

(defn refused-now
  "Whether wait reason w (a job's problem, or nil) is one the cell turned into during the call; the wait is noted for
  the caller's check."
  [c w]
  (when (contains? cell-reasons (:reason w))
    (ctx/wait c w)
    true))

(defn unreachable!
  "Remember the failed walk (why a keyword) as :unreachable {:from feet :why}, which the check waits on."
  [c why]
  (ctx/update-mem! c assoc :unreachable {:from (feet-cell c) :why why}))

(defn ^:async walk!
  "The walk to pos: one go-to call (child :walk, range 3), the whole walk. Resolves to :arrived (in reach), :continue
  (go-to waits on the world) or {:unreachable why} (it gave up, or arrived with the block still out of reach)."
  [c pos]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range 3 :escalate false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))
        res (ctx/child-result c :walk)]
    (cond
      (= :continue r) :continue
      (and (= :done r) (:arrived res) (in-reach? c pos)) :arrived
      :else {:unreachable (cond (not= :done r) :declined
                                (:arrived res) :out-of-reach
                                :else (:why res :unreachable))})))

;; ------------------------------------------------------------------ for parents

(def body-reasons
  "Wait reasons about the body, not the cell: a parent waits on these too instead of skipping the cell."
  #{:no-tool :inventory-full :need})

(defn body-wait? [reason] (contains? body-reasons (:reason reason)))

(defn child-wait
  "Why the child job (a registry symbol) in slot with args would wait now: its check's ctx/wait reason as a map
  {:reason kw ...} ({:reason :not-ready} for a plain false), or nil when its check passes."
  [c slot job args]
  (let [a (atom nil)]
    (when-not (ctx/check-child (assoc c :wait a) slot job args)
      (let [r @a]
        (cond (map? r) r
              (some? r) {:reason r}
              :else {:reason :not-ready})))))

(defn dig-outcome
  "What one round of a jobs.blocks.dig child (its return r, result res, the reason its check waits with) means for the
  cell: :dug, :missing (nothing there), :continue (the child waits on the world), :refused (a zone, claim, plan or
  hazard), :unreachable, :cannot, or the failed dig's :reason (:failed, :fluid-adjacent ...)."
  [r res waits]
  (case r
    :continue :continue
    :declined (if (#{:not-allowed :hazard} (:reason waits)) :refused :unreachable)
    (case (:reason res)
      :dug :dug
      :already-clear :missing
      (:cannot :fluid) :cannot
      (or (:reason res) :failed))))

(defn ^:async dig-cell!
  "Dig the one block at pos with a jobs.blocks.dig child in slot :dig (args merged over: no drops collected, no tool
  needed, no fetch; the caller picks drops up) and say what came of it (dig-outcome)."
  [c pos args]
  (let [args (merge {:collect false :need-drop false :fetch false} args {:pos pos})
        r (await (ctx/call-child c :dig 'jobs.blocks.dig args))
        res (ctx/child-result c :dig)
        waits (when (= :declined r) (child-wait c :dig 'jobs.blocks.dig args))]
    (dig-outcome r res waits)))

(defn chosen
  "The first of items carried, or nil."
  [c items]
  (let [carried (set (map :name (u/inventory (:primitives c))))]
    (first (filter carried items))))

(def neighbours [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])

(defn support?
  "Whether a neighbour of pos is a block to place against (not air, a fluid or a plant)."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]]
                   (let [n (u/block-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})]
                     (and n (not (air n)) (not (fluids n)) (not (clearable n)))))
                 neighbours)))

(defn place-outcome
  "What one round of a jobs.blocks.place child (its return r, result res, the reason its check waits with) means for the
  cell: :placed, :already (it holds the block), :continue, :refused (a zone, claim or plan), :need (the item is not
  carried), :no-support, :unreachable, :occupied, or the failed place's :reason (:failed, :clear-failed ...)."
  [r res waits]
  (case r
    :continue :continue
    :declined (case (:reason waits)
                (:not-allowed :own-body) :refused
                :need :need
                :no-support :no-support
                :unreachable)
    (case (:reason res)
      :placed :placed
      :already :already
      :occupied :occupied
      (or (:reason res) :failed))))

(defn ^:async place-cell!
  "Place item at pos with a jobs.blocks.place child in slot :place (args merged over: no fetch) and say what came of it
  (place-outcome)."
  [c pos item args]
  (let [args (merge {:fetch false} args {:pos pos :item item})
        r (await (ctx/call-child c :place 'jobs.blocks.place args))
        res (ctx/child-result c :place)
        waits (when (= :declined r) (child-wait c :place 'jobs.blocks.place args))]
    (place-outcome r res waits)))
