(ns engine.jobs.blocks
  "Shared parts of the leaf jobs jobs.blocks.dig and jobs.blocks.place, and what a parent needs to run them as
  children: child-wait reads the reason a child's check would wait with, and body-wait? tells a reason about the body
  (a tool, an item, a free slot: the parent should wait on it too) from one about the cell (skip that cell)."
  (:require [clojure.set :as set]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.forestry :as forestry]
            [engine.jobs.util :as u]
            [engine.places :as places]))

(def air #{"air" "cave_air" "void_air"})
(def fluids #{"water" "lava" "bubble_column"})

(def flowers
  #{"dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip" "white_tulip" "pink_tulip"
    "oxeye_daisy" "cornflower" "lily_of_the_valley" "torchflower" "pink_petals" "wildflowers" "short_dry_grass"
    "tall_dry_grass" "bush" "firefly_bush"})

(def clearable
  "What jobs.blocks.place digs out of a cell before placing: the plants and snow layer a placement would overwrite
  (engine.access.rules/replaceable less air and fluids) and the small flowers, which a placement does not overwrite."
  (into (set/difference rules/replaceable air fluids) flowers))

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
  "Whether the block at pos can be dug or placed from where the body stands (eye to cell centre, with a margin under
  the primitive's 4.5)."
  [c pos]
  (<= (forestry/eye-dist (u/self-pos c) pos) forestry/dig-reach))

(defn rules-in
  "The rules input for this job: zones, claims, the footprints of every plan but :for-plan, and its :ignore-zones?."
  [c]
  (access/rules-input c {:except (:for-plan (:args c))}))

(def social #{:zone :claim :footprint :no-zones})

(defn not-allowed
  "The wait reason for a verdict v refused by zones, claims, a plan's footprint or no zone list:
  {:reason :not-allowed :pos :by kw} plus the :zone/:claim/:plan/:owner that refused; nil for any other verdict."
  [pos v]
  (when (contains? social (:reason v))
    (merge {:reason :not-allowed :pos pos :by (:reason v)} (select-keys v [:zone :claim :plan :owner]))))

(defn unreachable-wait
  "The wait reason for job memory m's remembered failed walk to pos: {:reason :unreachable :pos :why}, while the body
  still stands in the cell it gave up from (a body moved by anyone else tries again); else nil."
  [c m pos]
  (when-let [{:keys [from why]} (:unreachable m)]
    (when (= from (feet-cell c))
      {:reason :unreachable :pos pos :why why})))

(defn fresh-mem
  "Job memory m for target pos: m when it is about pos, else emptied (children too) and marked for pos. A parent
  that reuses one slot for a run of cells gets a clean child per cell."
  [m pos]
  (if (= pos (:for m)) m (assoc (select-keys m [:args]) :for pos :children {})))

(defn mem-for
  "This job's memory as it is for pos (see fresh-mem), without writing it."
  [c pos]
  (fresh-mem (ctx/mem c) pos))

(defn ^:async walk!
  "One go-to round toward pos (child :walk, range 2). Resolves to :continue; a walk that gives up, or arrives with the
  block still out of reach, is remembered as :unreachable {:from feet :why}, which the check waits on."
  [c pos]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range 2}))
        res (ctx/child-result c :walk)]
    (when (and (= :done r) (not (and (:arrived res) (in-reach? c pos))))
      (ctx/update-mem! c assoc :unreachable {:from (feet-cell c) :why (if (:arrived res) :out-of-reach (:why res :unreachable))}))
    :continue))

;; ------------------------------------------------------------------ for parents

(def body-reasons
  "Wait reasons about the body, not the cell: a parent waits on these too instead of skipping the cell."
  #{:no-tool :inventory-full :need})

(defn body-wait? [reason] (contains? body-reasons (:reason reason)))

(defn child-wait
  "Why the child job (a registry symbol) in slot with args would wait now: its check's ctx/wait reason as a map
  ({:reason kw ...}; {:reason :not-ready} for a plain false), or nil when its check passes. Usable in a round or a
  check."
  [c slot job args]
  (let [a (atom nil)]
    (when-not (ctx/check-child (assoc c :wait a) slot job args)
      (let [r @a]
        (cond (map? r) r
              (some? r) {:reason r}
              :else {:reason :not-ready})))))
