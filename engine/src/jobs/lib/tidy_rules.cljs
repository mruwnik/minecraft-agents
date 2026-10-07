(ns jobs.lib.tidy-rules
  "The rule the tidy-style jobs (jobs.farm.tidy, jobs.forestry.prepare-field and -rule) share: the blocks never dug, and the
  verdict of the access rules for a cell, judged against the accepted hazards."
  (:require [clojure.string :as str]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.util :as u]))

(defn access-world
  "The social half of the rules' input, read now (jobs.lib.access/zone-input): zones nil when the zone file was never
  read, claims, footprints of every plan but this one, the body's name, the clock and the job's :ignore-zones? arg."
  [c]
  (access/zone-input c {:except (:plan (:args c)) :ignore-zones? (:ignore-zones? (:args c))}))

;; ------------------------------------------------------------------ the rule, pure

(def fluids #{"water" "lava" "bubble_column"})

(def lights
  #"(^|_)(torch|lantern|candle)$|^(campfire|soul_campfire|end_rod|sea_lantern|glowstone|shroomlight|jack_o_lantern)$")

(def containers
  #{"chest" "trapped_chest" "ender_chest" "barrel" "hopper" "dispenser" "dropper" "furnace" "blast_furnace" "smoker"
    "crafting_table" "composter" "brewing_stand" "enchanting_table" "cauldron" "water_cauldron" "grindstone" "loom"
    "smithing_table" "stonecutter" "cartography_table" "fletching_table" "lectern" "bell" "jukebox" "note_block"
    "beacon" "conduit" "lodestone" "respawn_anchor" "spawner" "beehive" "bee_nest" "flower_pot" "decorated_pot"
    "anvil" "chipped_anvil" "damaged_anvil" "crafter" "chiseled_bookshelf"})

(def owned #"_bed$|_sign$|_banner$|_head$|_skull$|_shulker_box$|^shulker_box$")

(defn keep-why
  "Why a block is never dug (:fluid :light :container :owned), or nil."
  [n]
  (cond
    (fluids n) :fluid
    (re-find lights n) :light
    (containers n) :container
    (or (re-find owned n) (str/ends-with? n "_shulker_box")) :owned))

(defn hazard-reason
  "The reason a dig hazard is accepted by: lava beside is :lava-adjacent, kept apart from water."
  [{:keys [reason fluid]}]
  (if (and (= :fluid-adjacent reason) (= "lava" fluid)) :lava-adjacent reason))

(defn judge-verdict
  "What to do with the may-dig? verdict v: :dig, :skip (not loaded), [:refuse {:reason ...}] or [:hazard [reasons]]."
  [v accept]
  (cond
    (= :not-loaded (:reason v)) :skip
    (not (:ok v)) [:refuse (select-keys v [:reason :zone :claim :plan])]
    (rules/accepts? (update v :hazards (fn [hs] (mapv #(assoc % :reason (hazard-reason %)) hs))) accept) :dig
    :else [:hazard (mapv hazard-reason (:hazards v))]))

(defn world-block
  "The block at [x y z] in plan.shape's shape: nil when unloaded, else {:name n} with :state when it has properties."
  [p [x y z]]
  (when-let [b (u/seen-block p {:x x :y y :z z})]
    (cond-> {:name (.-name b)}
      (.-properties b) (assoc :state (js->clj (.-properties b) :keywordize-keys true)))))

(defn feet-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn permit
  "The may-dig? verdict for pos now: zones and footprints read afresh, the body's feet where they are."
  [c pos]
  (let [p (:primitives c)]
    (rules/may-dig? (merge {:block-at (fn [cell] (:name (world-block p cell))) :cell pos :feet (feet-of c) :ledger #{}}
                           (access-world c)))))

(defn decide [c pos] (judge-verdict (permit c pos) (:accept (:args c))))
