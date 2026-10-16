(ns jobs.lib.blocks
  "Shared parts of jobs.blocks.dig and jobs.blocks.place, and what a parent needs to run them as children.
  body-wait? tells a reason about the body (tool, item, free slot:
  the parent waits too) from one about the cell (the parent skips that cell)."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.set :as set]
            [clojure.string :as str]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.aim :as aim]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.access :as access]
            [jobs.lib.game-facts :as facts]
            [jobs.lib.pen-entry :as pen-entry]
            [jobs.lib.dig-look :as look]
            [jobs.lib.trees :as forestry]
            [jobs.lib.util :as u]
            [jobs.lib.places :as places]
            [engine.game :as game]))

(def air #{"air" "cave_air" "void_air"})
(def fluids #{"water" "lava" "bubble_column"})

(def ores
  {"coal_ore" "coal" "iron_ore" "raw_iron" "copper_ore" "raw_copper" "gold_ore" "raw_gold"
   "diamond_ore" "diamond" "redstone_ore" "redstone" "lapis_ore" "lapis_lazuli" "emerald_ore" "emerald"})

(def drop-item
  (merge {"stone" "cobblestone" "grass_block" "dirt" "deepslate" "cobbled_deepslate"
          "clay" "clay_ball" "snow_block" "snowball"}
         ores
         (into {} (map (fn [[k v]] [(str "deepslate_" k) v])) ores)))

(defn item-name
  "The item the mined block drops."
  [{:keys [block item]}]
  (or item (get drop-item block) block))

(defn room-for?
  "A free slot, or a carried stack of item with room."
  [p item]
  (or (pos? (u/free-slots p))
      (some #(and (= item (:name %)) (< (:count %) facts/stack-size)) (u/inventory p))))

(defn carried
  "The carried [{:name :count}] whose name is in blocks, in the order of blocks."
  [c blocks]
  (let [have (into {} (map (juxt :name :count)) (u/inventory (:primitives c)))]
    (vec (for [b blocks :let [n (get have b 0)] :when (pos? n)] {:name b :count n}))))

(defn pick [c blocks] (:name (first (carried c blocks))))

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

(def hidden-guess access/hidden-guess)

(defn target-name
  "The block name at the job's cell pos: nil when not loaded, guess when the body has not sensed it."
  [p pos guess]
  ((access/sensed-at p guess) (cell pos)))

(defn rules-in
  "The rules input for this job: zones, claims, every plan's footprint but :for-plan's and the
  body's own plans', and :ignore-zones?. A cell the body has not sensed reads as stone (dig to see)."
  [c]
  (assoc (access/rules-input c {:except (:for-plan (:args c)) :own-plans-ok? true})
         :block-at (access/sensed-at (:primitives c) hidden-guess)))

(def social #{:zone :claim :footprint :no-zones})

(defn not-allowed
  "The wait reason for a verdict v refused by a zone, claim, plan footprint or missing zone list:
  {:reason :not-allowed :pos :by kw} plus whichever of :zone :claim :plan :owner refused. nil for any other verdict."
  [pos v]
  (when (contains? social (:reason v))
    (merge {:reason :not-allowed :pos pos :by (:reason v)} (select-keys v [:zone :claim :plan :owner]))))

(defn feet-cell
  "The body's feet cell [x y z]."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn in-reach?
  "Whether the block at pos can be dug or placed from where the body stands (eye to cell centre, within
  forestry/dig-reach, a margin under the primitive's 4.5); with see? also whether the eye sees it (jobs.lib.aim: nothing
  seen solid in the way), as a player must to aim at it."
  [c pos & [see?]]
  (and (<= (u/eye-dist (u/self-pos c) pos) forestry/dig-reach)
       (or (not see?) (aim/body-sees? c (if (vector? pos) pos (u/cell-vec pos))))))

(defn fresh-mem
  "Job memory m for target pos: m when it is about pos, else emptied (children too) and marked for pos. Gives a
  parent that reuses one slot a clean child per cell."
  [m pos]
  (if (= pos (:for m)) m (assoc (select-keys m [:args]) :for pos :children {})))

(defn mem-for
  "This job's memory as it is for pos (see fresh-mem), without writing it."
  [c pos]
  (fresh-mem (ctx/mem c) pos))

(def neighbours [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])

(defn unseen-near?
  "Whether the body has not sensed the cell pos or one of its six neighbours."
  [p {:keys [x y z]}]
  (boolean (some #(look/unknown? p %) (cons [x y z] (map (fn [[dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)]) neighbours)))))

(defn to-see?
  "Whether the job has yet to look at pos: the body has not sensed it or a face of it, and has not turned to it (memory
  :looked)."
  [c pos]
  (and (unseen-near? (:primitives c) pos) (not= pos (:looked (mem-for c pos)))))

(defn ^:async see-target!
  "Turn the head to the job's cell pos once when it has yet to be looked at (to-see?), so the next read is of what is
  there."
  [c pos]
  (when (to-see? c pos)
    (ctx/update-mem! c #(assoc (fresh-mem % pos) :looked pos))
    (await (look/look-at! c (cell pos)))))

(def cell-reasons
  "Wait reasons about the cell that can appear mid-call (a zone added, a block moved in): the call declines."
  #{:not-allowed :hazard :not-loaded :own-body :no-support})

(defn kept-out-text [pens]
  (str "the walk failed while keeping out of the gate of a pen holding animals: "
       (str/join "; " (map (fn [{:keys [gates animals]}] (str animals " animals, gate " (str/join " " (map (fn [{:keys [x y z]}] [x y z]) gates)))) pens))
       "; :enter-pens true allows the walk through"))

(defn ^:async walk-request
  "[pens go-to-args] of the walk to pos: range 3, no escalation, and (unless the job's :enter-pens) keeping out of the
  gates of the pens holding animals, after a look at the cells of those not yet seen."
  [c pos see? at-night]
  (let [pens (when-not (:enter-pens (:args c)) (await (pen-entry/pens! c)))
        keep-out (vec (mapcat :gates pens))]
    [pens (cond-> {:pos pos :range 3 :see (boolean see?) :ways #{:walk :clear-seal} :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}
            at-night (assoc :at-night at-night)
            (seq keep-out) (assoc :keep-out keep-out))]))

(defn warn-kept-out! [c pens]
  (when (seq pens)
    (ctx/emit! c :blocks.pen-kept-out :warn {:pens pens :text (kept-out-text pens)})))

(defn ^:async walk!*
  "walk! through ctx/call: :arrived, :continue (go-to waits on the world; the round holds still with its reason), else
  an ending: go-to's when it did not end :done, else nothing-possible :out-of-reach, or go-to's :why (:unreachable)
  when it ended not having arrived."
  [c pos & [see? at-night]]
  (let [[pens args] (await (walk-request c pos see? at-night))
        [r] (await (child/call-walk-reason* c :walk args))
        arrived? (and (= :done (:kind r)) (:arrived (:data r)))]
    (cond
      (= :continue r) :continue
      (and arrived? (in-reach? c pos see?)) :arrived
      :else (do (warn-kept-out! c pens)
                (if (= :done (:kind r))
                  (ctx/end c :nothing-possible (if arrived? :out-of-reach (:why (:data r) :unreachable)) (:data r))
                  r)))))

;; ------------------------------------------------------------------ for parents

(def body-reasons
  "Wait reasons about the body, not the cell: a parent waits on these too instead of skipping the cell."
  #{:no-tool :inventory-full :need})

(defn body-wait? [reason] (contains? body-reasons (:reason reason)))

(defn dig-outcome
  "What one round of a jobs.blocks.dig child (:continue, or its ending e) means for the cell: :dug, :missing (nothing
  there), :continue (the child waits on the world), :refused (a zone, claim, plan or hazard), :unreachable, :cannot, or
  the failed dig's primitive status (a keyword) or reason (:fluid-adjacent ...)."
  [e]
  (if (= :continue e)
    :continue
    (let [k (contract/cause-key e)
          data (:data e)]
      (case (:kind e)
        :done (if (= :already-clear (:reason data)) :missing :dug)
        :condition-failed (case k
                            (:not-allowed :hazard) :refused
                            (:cannot :fluid) :cannot
                            :fluid-adjacent :fluid-adjacent
                            :unreachable)
        :impossible :unreachable
        (:cut-off :limit-reached) :failed
        (case k
          (:cannot :fluid) :cannot
          (:not-allowed :hazard) :refused
          (:unreachable :no-tool :inventory-full :not-loaded) :unreachable
          (or (some-> (:primitive data) keyword) (when (not= k (:kind e)) k) :failed))))))

(defn ^:async dig-cell!
  "Dig the one block at pos with a jobs.blocks.dig child in slot :dig (args merged over: no drops collected, no tool
  needed, no fetch; the caller picks drops up) and say what came of it (dig-outcome). :fetch false does not stop
  the lava seal's fetch (:seal-fetch, default true); the jobs digging through here do not expose :seal-fetch."
  [c pos args]
  (let [args (merge {:collect false :need-drop false :fetch false} args {:pos pos})
        e (await (ctx/call c :dig 'jobs.blocks.dig args))]
    ;; the dig child told any wear of the tool; this job's older snapshot would tell it again
    (ctx/update-mem! c dissoc :tool-wear)
    (dig-outcome e)))

(defn chosen
  "The first of items carried, or nil."
  [c items]
  (let [carried (set (map :name (u/inventory (:primitives c))))]
    (first (filter carried items))))

(defn support?
  "Whether a neighbour of pos is a block to place against (not air, a fluid or a plant); a face the body has not
  seen is not one."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]]
                   (let [n (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})]
                     (and n (not (air n)) (not (fluids n)) (not (clearable n)))))
                 neighbours)))

(defn place-outcome
  "What one round of a jobs.blocks.place child (:continue, or its ending e) means for the cell: :placed, :already (it
  holds the block), :continue, :refused (a zone, claim or plan), :need (the item is not carried), :no-support,
  :occupied, :unreachable, or the failed place's primitive status (a keyword) or reason (:clear-failed ...)."
  [e]
  (if (= :continue e)
    :continue
    (let [k (contract/cause-key e)
          data (:data e)]
      (case (:kind e)
        :done (if (= :already (:reason data)) :already :placed)
        :condition-failed (case k
                            (:not-allowed :own-body) :refused
                            (:need :no-support :occupied) k
                            :unreachable)
        :impossible :unreachable
        (:cut-off :limit-reached) :failed
        (or (some-> (:primitive data) keyword) (:reason data) (when (not= k (:kind e)) k) :failed)))))

(defn ^:async place-call!
  "Place item at pos with a jobs.blocks.place child in slot :place (args merged over: no fetch): :continue or its ending
  (its :data has the placed :block)."
  [c pos item args]
  (await (ctx/call c :place 'jobs.blocks.place (merge {:fetch false} args {:pos pos :item item}))))

(defn ^:async place-cell!
  "place-call!, said as what came of it (place-outcome)."
  [c pos item args]
  (place-outcome (await (place-call! c pos item args))))
