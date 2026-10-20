(ns jobs.lib.blocks
  "Shared parts of jobs.blocks.dig and jobs.blocks.place, and what a parent needs to run them as children.
  body-wait? tells a reason about the body (tool, item, free slot:
  the parent waits too) from one about the cell (the parent skips that cell)."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.set :as set]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.aim :as aim]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.waits :as waits]
            [jobs.lib.need :as need]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.access :as access]
            [jobs.lib.game-facts :as facts]
            [jobs.lib.pen-entry :as pen-entry]
            [jobs.lib.dig-look :as look]
            [jobs.lib.trees :as forestry]
            [jobs.lib.util :as u]
            [jobs.lib.places :as places]
            [engine.game :as game]))

(def ores
  {"coal_ore" "coal" "iron_ore" "raw_iron" "copper_ore" "raw_copper" "gold_ore" "raw_gold"
   "diamond_ore" "diamond" "redstone_ore" "redstone" "lapis_ore" "lapis_lazuli" "emerald_ore" "emerald"})

(def drop-item
  (merge {"stone" "cobblestone" "grass_block" "dirt" "deepslate" "cobbled_deepslate" "podzol" "dirt" "mycelium" "dirt"
          "clay" "clay_ball" "snow_block" "snowball"}
         ores
         (into {} (map (fn [[k v]] [(str "deepslate_" k) v])) ores)))

(defn item-name
  "The item the mined block drops."
  [{:keys [block item]}]
  (or item (get drop-item block) block))

(defn blocks-dropping
  "The blocks that drop item: the drop table's, else a block of the item's own name (sand)."
  [item]
  (let [from-table (into #{} (keep (fn [[b i]] (when (= i item) b))) drop-item)]
    (if (and (empty? from-table) (not (contains? drop-item item))) #{item} from-table)))

(defn room-for?
  "A free slot, or a carried stack of item with room."
  [p item]
  (or (pos? (u/free-slots p))
      (some #(and (= item (:name %)) (< (:count %) facts/stack-size)) (u/inventory p))))

(def clearable
  "What jobs.blocks.place digs out of a cell before placing: the replaceable plants and snow layer
  (block-sets/replaceable, less air and fluids) and small flowers, which a placement does not overwrite."
  (into (set/difference bs/replaceable bs/air bs/fluids) bs/flowers))

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

(defn recoverable?
  "Whether the placed block can be dug out again by this body for the same item: it drops itself and the carried
  tools harvest it. Glass, ice and leaves drop nothing without silk touch; stone drops cobblestone."
  [p block]
  (let [need (some-> (.harvestTools p block) js->clj)]
    (and (some #{block} (drops-of p block))
         (or (empty? need) (boolean (some (set need) (map :name (u/inventory p))))))))

(def falling-names #{"sand" "red_sand" "gravel" "suspicious_sand" "suspicious_gravel" "anvil" "dragon_egg"})

(defn falling? [n] (boolean (and n (or (falling-names n) (re-find #"_concrete_powder$" n)))))

(defn safe-to-break?
  "Whether taking a block out of cell later is safe, as far as the body has seen: no fluid beside or above it
  (it would flow in) and no falling block directly above it (the column would drop). An unseen neighbour counts as safe."
  [p {:keys [x y z]}]
  (let [at (fn [dx dy dz] (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)}))]
    (not (or (some #(contains? bs/fluids %) [(at 0 1 0) (at 1 0 0) (at -1 0 0) (at 0 0 1) (at 0 0 -1)])
             (falling? (at 0 1 0))))))

(defn held-fillers
  "{name count} the fillers chooser may not use: the reserve register's hard units (strength 1, an :avoid), and with
  soft? false the soft ones too; with soft? true the soft ones the body could not dig back out (recoverable?)."
  [c soft?]
  (reduce (fn [m {:keys [name count strength]}]
            (if (and soft? (< strength 1) (recoverable? (:primitives c) name))
              m
              (update m name (fnil + 0) count)))
          {} (reserve/rows c :fillers)))

(defn carried
  "The carried [{:name :count}] whose name is in blocks, less what the reserve register holds off limits to the
  fillers chooser (held-fillers). With soft? (default true: a body in danger), soft-reserved units that can be dug back
  out count as free, and names with unreserved units come first, then ones spending soft reserves, each in the order of blocks."
  ([c blocks] (carried c blocks true))
  ([c blocks soft?]
   (let [have (into {} (map (juxt :name :count)) (u/inventory (:primitives c)))
         held (held-fillers c soft?)
         all (reserve/held c :fillers)
         rows (vec (for [b blocks :let [n (- (get have b 0) (get held b 0))] :when (pos? n)] {:name b :count n}))
         free? #(> (- (get have (:name %) 0) (get all (:name %) 0)) 0)]
     (vec (concat (filter free? rows) (remove free? rows))))))

(defn pick
  ([c blocks] (pick c blocks true))
  ([c blocks soft?] (:name (first (carried c blocks soft?)))))

(defn borrowed?
  "Whether placing item spends a soft-reserved unit: fewer unreserved units are carried than the reserve register leaves free."
  [c item]
  (let [have (into {} (map (juxt :name :count)) (u/inventory (:primitives c)))]
    (<= (- (get have item 0) (get (reserve/held c :fillers) item 0)) 0)))

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
       "; the setting jobs.lib.blocks/enter-pens allows the walk through"))

(a/defargs settings
  {::enter-pens {:default false :doc "true: the walk of blocks.dig, place and use-on may go through the gate of a pen holding animals; false: it keeps out of those gates and, with no other way, declines (a :blocks.pen-kept-out warn names the pen)." :spec boolean?}
   ::max-walks {:default 2 :doc "Failed walks into reach of one blocks.dig, place or use-on call before it ends :unreachable." :spec (a/int-in 0 nil)}
   ::seal-fetch {:default true :doc "How blocks.dig gets a building block to seal lava with when none is carried, whatever its :ways say; false seals only with a carried block." :spec boolean?}})

(def low-floor-radius 4)

(defn low-floor-cells
  "The seen beds and bottom slabs round pos ({:x :y :z}), as cells a walk never enters (a go-to ##Inf toll): a build's walk stands half a block high on
  one, its head inside the roof cell the build still has to fill."
  [c pos]
  (let [p (:primitives c) r low-floor-radius]
    (vec (for [dx (range (- r) (inc r)) dz (range (- r) (inc r)) dy (range -2 3)
               :let [cell {:x (+ (js/Math.floor (:x pos)) dx) :y (+ (js/Math.floor (:y pos)) dy) :z (+ (js/Math.floor (:z pos)) dz)}]
               :when (u/low-floor? (u/seen-block p cell))]
           cell))))

(defn ^:async walk-request
  "[pens go-to-args] of the walk to pos: range (default 3), no escalation, and (unless the :enter-pens setting) keeping out of the
  gates of the pens holding animals, after a look at the cells of those not yet seen."
  [c pos see? & [range]]
  (let [pens (when-not (settings/get settings ::enter-pens) (await (pen-entry/pens! c)))
        keep-out (mapv #(assoc % :factor ##Inf) (into (vec (mapcat :gates pens)) (when (:for-plan (:args c)) (low-floor-cells c pos))))]
    [pens (cond-> {:pos pos :range (or range 3) :see (boolean see?) :ways #{:walk :clear-seal} :ignore-zones? (boolean (:ignore-zones? (:args c)))}
            (seq keep-out) (assoc :tolls keep-out))]))

(defn warn-kept-out! [c pens]
  (when (seq pens)
    (ctx/emit! c :blocks.pen-kept-out :warn {:pens pens :text (kept-out-text pens)})))

(defn ^:async walk!*
  "walk! through ctx/call: :arrived, :continue (go-to waits on the world; the round holds still with its reason), else
  an ending: go-to's when it did not end :done, else nothing-possible :out-of-reach, or go-to's :why (:unreachable)
  when it ended not having arrived."
  [c pos & [see? range]]
  (let [[pens args] (await (walk-request c pos see? range))
        r (await (child/call-walk* c :walk args))
        arrived? (and (= :done (:kind r)) (:arrived (:data r)))]
    (cond
      (= :continue r) :continue
      (and arrived? (in-reach? c pos see?)) :arrived
      :else (do (warn-kept-out! c pens)
                (if (= :done (:kind r))
                  (ctx/end c :nothing-possible (if arrived? :out-of-reach (:why (:data r) :unreachable)) (:data r))
                  r)))))

(defn max-walks [] (settings/get settings ::max-walks))

(defn ^:async walk-into-reach!
  "One walk into reach of pos for blocks.dig, place and use-on: :continue (go-to waits), :again (arrived: the caller re-checks),
  a child's ending that is not a failed walk (pass it up), else a failed walk, counted in the job memory across rounds:
  :again until max-walks have failed, then {:unreachable why :text go-to's words}."
  [c pos & [see? range]]
  (let [w (await (walk!* c pos see? range))]
    (cond
      (= :continue w) :continue
      (= :arrived w) :again
      (not (#{:nothing-possible :condition-failed} (:kind w))) w
      :else (let [n (inc (:walk-fails (ctx/mem c) 0))]
              (ctx/update-mem! c assoc :walk-fails n)
              (if (< n (max-walks))
                :again
                {:unreachable (or (:why (:data w)) (when (map? (:cause w)) (:why (:cause w))) (contract/cause-key w))
                 :text (get-in w [:data :text])})))))

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
                            :no-tool :no-tool
                            :unreachable)
        :impossible :unreachable
        (:cut-off :limit-reached) :failed
        (case k
          (:cannot :fluid) :cannot
          (:not-allowed :hazard) :refused
          (:unreachable :inventory-full :not-loaded) :unreachable
          :no-tool :no-tool
          (or (some-> (:primitive data) keyword) (when (not= k (:kind e)) k) :failed))))))

(defn ^:async dig-cell-ending!
  "Dig the one block at pos with a jobs.blocks.dig child in slot :dig (args merged over: no drops collected, no tool
  needed, no fetch; the caller picks drops up): :continue (also when the child waits out the world) or the child's ending. :ways #{:carried} does not stop
  the lava seal's fetch (the seal-fetch setting, default true)."
  [c pos args]
  (let [args (merge {:collect false :need-drop false :ways #{:carried}} args {:pos pos})
        e (waits/waiting-out c (await (ctx/call c :dig 'jobs.blocks.dig args)))]
    ;; the dig child told any wear of the tool; this job's older snapshot would tell it again
    (ctx/update-mem! c dissoc :tool-wear)
    e))

(defn ^:async dig-cell!
  "dig-cell-ending!, said as what came of it (dig-outcome)."
  [c pos args]
  (dig-outcome (await (dig-cell-ending! c pos args))))

(defn chosen
  "The first of items carried, or nil."
  [c items]
  (let [carried (set (map :name (u/inventory (:primitives c))))]
    (first (filter carried items))))

(defn support-name?
  "Whether a block name is one to place against (not air, a fluid or a plant)."
  [n]
  (boolean (and n (not (bs/air n)) (not (bs/fluids n)) (not (clearable n)))))

(defn support?
  "Whether a neighbour of pos is a block to place against; a face the body has not seen is not one."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]] (support-name? (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)}))) neighbours)))

(def stale-ms "Age of a sensed cell past which the body looks again before it gives up on what it holds." 1000)

(defn stale-support?
  "Whether a neighbour of pos that is no support (as far as the body knows) was last sensed over stale-ms ago and the
  job has not yet looked at pos: it may have been filled since."
  [c pos]
  (let [p (:primitives c) {:keys [x y z]} pos]
    (and (not= pos (:looked (mem-for c pos)))
         (boolean (some (fn [[dx dy dz]]
                          (let [s (u/sensed p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})]
                            (and s (not (true? (.-unknown s))) (not (support-name? (.-name s))) (> (or (.-ageMs s) 0) stale-ms))))
                        neighbours)))))

(defn refusing-mobs
  "The names of the entities a refused place reports near its cell (the mob in the way), nil when none."
  [r]
  (seq (map #(or (.-name %) (.-kind %)) (some-> r .-refusal .-entities))))

(defn place-outcome
  "What one round of a jobs.blocks.place child (:continue, or its ending e) means for the cell: :placed, :already (it
  holds the block), :continue, :refused (a zone, claim or plan), :need (the item is not carried), :no-support,
  :occupied, :unreachable, :mob (the failed place names a mob in the cell), or the failed place's primitive status (a keyword) or reason (:clear-failed ...)."
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
        (or (when (seq (:mobs data)) :mob) (some-> (:primitive data) keyword) (:reason data) (when (not= k (:kind e)) k) :failed)))))

(defn ^:async place-call!
  "Place item at pos with a jobs.blocks.place child in slot :place (args merged over: no fetch): :continue or its ending
  (its :data has the placed :block)."
  [c pos item args]
  (waits/waiting-out c (await (ctx/call c :place 'jobs.blocks.place (merge {:ways #{:carried}} args {:pos pos :item item})))))

(defn ^:async place-cell!
  "place-call!, said as what came of it (place-outcome)."
  [c pos item args]
  (place-outcome (await (place-call! c pos item args))))
