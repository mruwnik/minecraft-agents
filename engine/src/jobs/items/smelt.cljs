(ns jobs.items.smelt
  (:require [clojure.string :as str]
            [jobs.lib.game-facts :as facts]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.items.craft :as craft]
            [jobs.items.smelt.plan :as sp]
            [jobs.lib.access :as access]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.places :as places]
            [jobs.lib.storage :as storage]))

(def doc
  "Smelt :count of an item in a furnace, blast furnace or smoker without standing by it.
  :item may be a list, and with :item nil and two or more raw foods or ores carried every one of them is cooked: one child
  smelt per item, in order, each with the same :count; the result is {:smelted n :wanted n :items {item result}} (stopped,
  with the first item's reason, when nothing was smelted; :failed names the items that gave up).
  The first round walks to the furnace, reads it, takes any output already there (it does not count) and loads
  the input and the fuel. Fuel is worked out from the count: in a furnace one fuel item smelts 8 items for coal
  or charcoal, 1.5 for overworld wood (crimson and warped do not burn), 0.5 for a stick (twice that in a blast furnace or smoker). It is taken from what is carried when :fuel is nil, coal and charcoal first.
  The job then ends its round and waits (check reason :cooking, with :furnace and :ready-at) so the body can do
  other jobs. The next round takes the output.
  The wait is decided by the clock: ready-at is now plus the cook time of the items loaded (200 ticks each in a
  furnace, 100 in the other two) plus a margin. The check wakes the job early once :unlit-from has passed and
  the furnace block is no longer lit or no longer a furnace. It reads only memory, the clock and one block. A
  woken round that finds the cook unfinished waits again from the furnace's own bars.
  What is owed is kept in memory before the load, so a restart or cut neither loses the output nor loads twice.
  With :fetch (default true; jobs.lib.fetch) what is missing is fetched first by obtain children: the input, the
  fuel (the :fuel item, else coal or charcoal, else planks of any wood, sized by what one item of it smelts), and, when no furnace
  is seen, a furnace (a carried one is put down beside the body, else one is fetched and put down). A fetch that
  finds nothing leaves the reason below (no-item, no-fuel, no-furnace-seen). :fetch false skips all of it.
  Result: {:smelted n :wanted n}: :wanted is the count this job loaded and :smelted what came of it; input
  already in the furnace and the output already there do not count. Plus :reason and :status :stopped after a warn (smelt.gave-up) when it gives up:
  - no-furnace-seen (no :furnace given and none seen), furnace-not-placed (one is carried and could not be put down; :why says why), no-furnace, not-a-furnace, furnace-gone (broken or
    replaced meanwhile), output-gone (input and output both gone), unreachable.
  - no-item, nothing-smeltable, not-smeltable (this kind of furnace cannot cook it, also found when it never lit).
  - no-fuel, unknown-fuel, rejected-fuel, out-of-fuel (leftover input is taken back), fuel-busy (the fuel slot
    holds another fuel).
  - furnace-busy (the input slot holds another item that is not cooking; one that is cooking is waited out, its output taken, then the load goes in), furnace-full, inventory-full (the output stays in the
    furnace).
  - \"refused\" (with :zones, :claims): the furnace is in another owner's zone or claim and does not allow
    :put (loading) or :take (collecting, or the output already there). Nothing is loaded or taken. :ignore-zones? true skips the check.")

(a/defargs args
  {:furnace {:doc "furnace, blast furnace or smoker position [x y z] or {:x :y :z}; when nil the nearest one the body has seen within 32 blocks that cooks :item (or anything carried) is chosen (smelt.furnace says which), or the job ends with no-furnace-seen" :spec ::a/pos :default nil}
   :item {:doc "what to smelt, or a list of them (an input; a product such as cooked_chicken or iron_ingot means its input, charcoal the carried log there is most of; an item no furnace cooks ends not-smeltable at once); the first smeltable thing carried when nil, or every raw food and ore carried when there are several" :spec (a/or-of a/item? (a/coll-of a/item?)) :default nil}
   :count {:doc "how many; all carried (at most one stack) when nil" :spec (a/int-in 0 nil) :default nil}
   :fuel {:doc "fuel item to load; the best carried when nil" :spec a/item? :default nil}
   :fetch {:doc "get a missing input, fuel or furnace (jobs.lib.fetch): true, a set of kinds or a map of limits; false ends with the reason" :spec fetch/option? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(a/defargs settings
  {::slack-ms {:default 2000 :doc "Slack added to a wait for the furnace, in ms." :spec (a/int-in 0 nil)}
   ::grace-ms {:default 6000 :doc "How long a furnace may look unlit after a load before it counts as stuck, in ms." :spec (a/int-in 0 nil)}
   ::recheck-ms {:default 6000 :doc "How often the furnace is looked at again, in ms." :spec (a/int-in 0 nil)}
   ::fetch-fuels {:default ["coal" "charcoal"] :doc "Fuel items fetched first when :fuel is nil, the first preferred." :spec (a/coll-of a/item?)}
   ::fetch-wood-fuels {:default ["oak_planks" "spruce_planks" "birch_planks" "jungle_planks" "acacia_planks" "dark_oak_planks"] :doc "Fuel items fetched when the ::fetch-fuels fetch found too little, the first preferred." :spec (a/coll-of a/item?)}
   ::seen-radius {:default 32 :doc "Blocks within which a furnace counts as seen." :spec (a/int-in 0 nil)}})
(defn slack-ms [] (settings/get settings ::slack-ms))
(defn grace-ms [] (settings/get settings ::grace-ms))
(defn recheck-ms [] (settings/get settings ::recheck-ms))
(defn seen-radius [] (settings/get settings ::seen-radius))
(defn fetch-fuels [] (settings/get settings ::fetch-fuels))
(defn fetch-wood-fuels [] (settings/get settings ::fetch-wood-fuels))

(defn wait-ms
  "How long until the items in the input are cooked: what the cook bar still has to count, from the bar when it is
  known, plus a margin."
  [kind state]
  (let [per (sp/cook-ticks kind)
        left (- (* (get-in state [:input :count] 0) per) (get-in state [:cook :done] 0))]
    (+ (slack-ms) (settings/ticks->ms (max 0 left)))))

;; ------------------------------------------------------------------ the check

(def furnace-block? #{"furnace" "blast_furnace" "smoker"})

(defn furnace-of
  "The furnace this job uses: the :furnace arg, else the one its first round chose (job memory), else nil."
  [c]
  (or (:furnace (:args c)) (:furnace (ctx/mem c))))

(defn carried [c] (u/inventory (:primitives c)))

(defn nearest-furnace
  "The nearest furnace, blast furnace or smoker the body has seen within seen-radius, still that block now, that
  cooks item (or, when item is nil, anything carried; any kind when nothing carried is smeltable); nil when none."
  [c item]
  (let [names (if item [item] (map :name (carried c)))
        kinds (filter (fn [kind] (some #(sp/smelts? kind %) names)) (keys sp/cook-ticks))
        ok? (if (or item (seq kinds)) (set kinds) furnace-block?)]
    (->> (look/seen-blocks (:primitives c) {:names furnace-block? :radius (seen-radius) :max 16 :live? true})
         (some #(when (ok? (:name %)) (:pos %))))))

(defn needs-attention?
  "Whether the block at pos is loaded and is no longer a lit furnace: it is not lit (the fuel ran out, or the cook
  is over) or it is not a furnace any more (broken, replaced)."
  [p pos]
  (let [block (u/seen-block p pos)]
    (and (some? block)
         (or (not (furnace-block? (.-name block)))
             (false? (some-> (.-properties block) .-lit))))))

(defn due?
  "Whether the cook of the job memory m is due: see check."
  [c m]
  (let [furnace (or (:furnace (:args c)) (:furnace m))
        now (ctx/now c)]
    (or (nil? furnace)
        (nil? (:ready-at m))
        (>= now (:ready-at m))
        (and (>= now (:unlit-from m)) (needs-attention? (:primitives c) furnace)))))

(defn check
  "The cook is due: no furnace chosen yet (the round chooses one), nothing loaded yet, or the clock passed ready-at,
  or since :unlit-from the furnace block is unlit or gone. Else it waits with reason :cooking. Memory, the clock and
  one block: no window is opened. A job smelting several items asks the same of the child that is cooking."
  [c]
  (let [m (ctx/mem c)
        m (if-let [slot (:active m)] (get-in m [:children slot]) m)]
    (or (due? c m)
        (ctx/wait c {:reason :cooking :furnace (or (:furnace (:args c)) (:furnace m)) :ready-at (:ready-at m)}))))

;; ------------------------------------------------------------------ the round

(defn ^:async visit!
  "One furnace visit through act, as a cljs map."
  [c op extra]
  (let [r (await (ctx/act c :furnace (clj->js (merge {:pos (furnace-of c) :op op} extra))))]
    (js->clj r :keywordize-keys true)))

(defn wait-until!
  "Remember when to look again and end the round."
  [c ms]
  (let [now (ctx/now c)]
    (ctx/update-mem! c assoc :ready-at (+ now ms) :unlit-from (+ now (grace-ms)))
    :continue))

(defn finish!
  "Hand the parent what was smelted and return :done."
  [c extra]
  (let [m (ctx/mem c)]
    (ctx/result! c (merge {:smelted (max 0 (- (:got m 0) (:base (:owed m) 0)))
                           :wanted (- (:count (:owed m) 0) (:base (:owed m) 0))}
                          extra))
    :done))

(defn stop!
  "Warn and finish with a reason; block is the block the body saw where the furnace should be, why the cause when the reason alone misleads."
  ([c reason] (stop! c reason nil nil))
  ([c reason block] (stop! c reason block nil))
  ([c reason block why]
   (ctx/emit! c :smelt.gave-up :warn (cond-> {:reason reason :text (str "smelt gave up: " reason (when block (str " (saw " block ")")) (when why (str ": " why)))}
                                       block (assoc :block block)
                                       why (assoc :why why)))
   (finish! c (cond-> {:reason reason :status :stopped} why (assoc :why why)))))

(defn ^:async stop-back!
  "Give up after a load: leftover input goes back to the pockets first."
  [c reason]
  (await (visit! c "take" {:output false :input true}))
  (stop! c reason))

(defn refuse!
  "End with reason \"refused\": the furnace is in another's zone or claim; one smelt.gave-up warn names them."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])]
    (ctx/emit! c :smelt.gave-up :warn (assoc fields :reason "refused" :text (str "smelt gave up: refused by " (access/refusal-text fields))))
    (finish! c (assoc (select-keys fields [:zones :claims :reason]) :status :stopped))))

(defn give-up!
  "Count a failed round in a row: :continue until u/max-failures, then stop with the reason."
  [c reason]
  (if (u/count-fail! c)
    (stop! c reason)
    :continue))

(defn ^:async load!
  "Put the planned input and fuel in. :continue when they are in, else the reason it gives up."
  [c state plan]
  (let [{:keys [item count fuel]} plan
        m (ctx/mem c)
        e (if (= item (get-in state [:input :name])) (get-in state [:input :count]) 0)
        target (or (:target m) (+ e count))
        base (if (:target m) (:base (:owed m) 0) e)
        _ (ctx/update-mem! c assoc :target target :owed {:item item :count target :base base})
        load (cond-> {}
               (pos? count) (assoc :input {:item item :count count})
               fuel (assoc :fuel {:item (:item fuel) :count (:count fuel)}))
        r (if (empty? load) {:status "ok"} (await (visit! c "load" load)))]
    (case (:status r)
      "ok" (do (u/progress! c)
               (ctx/emit! c :smelt.loaded :info {:text (str "smelting " target " " item) :item item :count target :fuel (:item fuel)})
               (wait-until! c (+ (slack-ms) (settings/ticks->ms (* target (sp/cook-ticks (:kind state)))))))
      "busy" (stop! c (if (= "input" (:slot r)) "furnace-busy" "fuel-busy"))
      "no-item" (stop! c "no-item")
      "rejected" (stop! c (if (= "fuel" (:slot r)) "rejected-fuel" "furnace-full"))
      (stop! c (str "load " (:status r))))))

(defn ^:async start!
  "The first rounds: plan, take what the furnace holds already, load."
  [c state]
  (let [m (ctx/mem c)
        {:keys [count fuel]} (:args c)
        item (sp/input-of (:item (:args c)) (carried c))
        plan (sp/plan-load {:state state :carried (carried c) :item item :count count :fuel fuel :target (:target m)})]
    (cond
      (and (= "furnace-busy" (:give-up plan)) (= :wait (sp/judge state))) (wait-until! c (wait-ms (:kind state) state))
      (:give-up plan) (stop! c (:give-up plan))
      :else
      (if-let [v (when (:output state) (access/container-refusal c :take (furnace-of c)))]
        (refuse! c v)
        (let [freed (when (:output state) (await (visit! c "take" {})))]
          (if (= "full" (:status freed))
            (stop! c "inventory-full")
            (await (load! c state plan))))))))

(defn ^:async take!
  "Take the output with one furnace visit and add what was taken to :got. Returns the visit result: status \"full\" when the inventory has no room, :input while the furnace still holds input."
  [c]
  (let [r (await (visit! c "take" {}))
        got (reduce + 0 (map :count (:taken r)))]
    (ctx/update-mem! c update :got (fnil + 0) got)
    (when (pos? got) (u/progress! c))
    r))

(defn ^:async feed!
  "The fire is out with input left: put fuel in for what remains, or give the input back."
  [c state]
  (let [m (ctx/mem c)
        item (:item (:owed m))
        remaining (get-in state [:input :count])
        plan (sp/plan-load {:state state :carried (carried c) :item item :fuel (:fuel (:args c)) :target remaining})]
    (if (or (:give-up plan) (nil? (:fuel plan)))
      (await (stop-back! c "out-of-fuel"))
      (let [r (await (visit! c "load" {:fuel {:item (:item (:fuel plan)) :count (:count (:fuel plan))}}))]
        (if (= "ok" (:status r))
          (wait-until! c (wait-ms (:kind state) state))
          (await (stop-back! c "out-of-fuel")))))))

(defn ^:async collect!
  "The cook is due: take the output, then wait again, feed the fire, give up, or finish."
  [c state]
  (case (sp/judge state)
    :take (let [r (await (take! c))]
            (cond
              (= "full" (:status r)) (stop! c "inventory-full")
              (:input r) (wait-until! c (recheck-ms))
              :else (do (ctx/emit! c :smelt.done :info {:text (str "smelted " (:got (ctx/mem c))) :smelted (:got (ctx/mem c))})
                        (finish! c {}))))
    :wait (wait-until! c (wait-ms (:kind state) state))
    :out-of-fuel (if-let [v (access/container-refusal c :put (furnace-of c))]
                   (refuse! c v)
                   (await (feed! c state)))
    :not-smeltable (await (stop-back! c "not-smeltable"))
    :done (let [m (ctx/mem c)]
            (if (< (:got m 0) (:count (:owed m) 0))
              (stop! c "output-gone")
              (do (ctx/emit! c :smelt.done :info {:text (str "smelted " (:got m)) :smelted (:got m)})
                  (finish! c {}))))))

(defn ^:async round-at!
  "Reach the furnace, read it, then load (first) or collect."
  [c furnace owed?]
  (case (await (near/go-near! c furnace 3 {:zone-tolls true}))
    :partial :continue
    :blocked (give-up! c "unreachable")
    (let [state (await (visit! c "read" {}))]
      (case (:status state)
        "ok" (if (and owed? (:ready-at (ctx/mem c)))
               (await (collect! c state))
               (await (start! c state)))
        "missing" (stop! c (if owed? "furnace-gone" "no-furnace") (:block state))
        "cannot" (stop! c (if owed? "furnace-gone" "not-a-furnace") (:block state))
        "unreachable" (give-up! c "unreachable")
        (stop! c (str "furnace " (:status state)))))))

(defn ^:async round-with
  "The round once the furnace is known."
  [c furnace]
  (let [m (ctx/mem c)
        owed? (some? (:owed m))
        action (if (and owed? (:ready-at m)) :take :put)]
    (if-let [v (access/container-refusal c action furnace)]
      (refuse! c v)
      (await (round-at! c furnace owed?)))))

(defn refuse-item!
  "End stopped: no furnace cooks :item, so no furnace is looked for. One smelt.gave-up warn names it."
  [c item]
  (ctx/emit! c :smelt.gave-up :warn {:reason "not-smeltable" :item item
                                     :text (str "smelt gave up: not-smeltable, no furnace cooks " item)})
  (finish! c {:reason "not-smeltable" :item item :status :stopped}))

;; ------------------------------------------------------------------ fetch: input, fuel, furnace

(defn want-count
  "How many items are to be smelted: :count, else what is carried of the input (at least 1 when nothing is)."
  [c item]
  (let [have (storage/carried (carried c) item)]
    (or (:count (:args c)) (max 1 have))))

(defn fuel-capacity
  "How many items the carried fuel (only :fuel when given) would smelt."
  [c item]
  (let [{:keys [fuel]} (:args c)]
    (transduce (comp (filter #(and (not= item (:name %)) (or (nil? fuel) (= fuel (:name %)))))
                     (keep #(some-> (sp/fuel-per-unit "furnace" (:name %)) (* (:count %)))))
               + 0 (carried c))))

(defn stage-problem
  "The :need wait of the fetch stage in memory (:input or :fuel) while it is unmet, else nil."
  [c]
  (let [item (sp/input-of (:item (:args c)) (carried c))
        n (when item (want-count c item))]
    (when item
      (case (:fetch-stage (ctx/mem c))
        :input (let [need (- n (storage/carried (carried c) item))]
                 (when (pos? need) {:reason :need :item item :count need}))
        (:fuel :wood-fuel)
        (let [fuel (:fuel (:args c))
              names (cond fuel [fuel] (= :fuel (:fetch-stage (ctx/mem c))) (fetch-fuels) :else (fetch-wood-fuels))
              short (- n (fuel-capacity c item))
              need (js/Math.ceil (/ short (sp/fuel-per-unit "furnace" (first names))))]
          (when (and (pos? short) (not (and fuel (= :wood-fuel (:fetch-stage (ctx/mem c))))))
            (if fuel
              {:reason :need :item fuel :count need}
              {:reason :need :any-of names :count need})))
        nil))))

(defn ^:async fetch-stages!
  "Before the first load: fetch the missing input, then the missing fuel, once each. :continue while a fetch child
  waits, :done for a bad :fetch arg, else nil."
  [c]
  (when (and (fetch/opts c 'jobs.items.smelt) (nil? (:owed (ctx/mem c))))
    (loop [[stage & more] (remove #(contains? (:fetch-done (ctx/mem c)) %) [:input :fuel :wood-fuel])]
      (when stage
        (ctx/update-mem! c assoc :fetch-stage stage)
        (let [r (await (fetch/fetch! c 'jobs.items.smelt stage-problem))]
          (if (nil? r)
            (do (ctx/update-mem! c update :fetch-done (fnil conj #{}) stage)
                (recur more))
            r))))))

(defn furnace-problem
  "The :need wait for a furnace while one is wanted and none is carried, else nil."
  [c]
  (when (and (:want-furnace (ctx/mem c)) (not (pos? (storage/carried (carried c) "furnace"))))
    {:reason :need :item "furnace" :count 1}))

(defn ^:async place-furnace!
  "Put the carried furnace down beside the body (jobs.blocks.place child; craft/spot! finds the cell) and make it the
  furnace of this job. :again once it stands, :continue while a child waits, nil when it could not be put down (memory
  :furnace-refusal says why)."
  [c]
  (let [spot (or (:furnace-spot (ctx/mem c)) (await (craft/spot! c)))]
    (cond
      (= :continue spot) :continue
      (nil? spot) (do (ctx/update-mem! c assoc :furnace-refusal "no free cell beside the body to put it on")
                      nil)
      :else
      (do (ctx/update-mem! c assoc :furnace-spot spot)
          (let [r (await (ctx/call-child c :place 'jobs.blocks.place {:item "furnace" :pos spot :fetch false}))]
            (if (= :continue r)
              :continue
              (let [res (ctx/child-result c :place)]
                (ctx/update-mem! c dissoc :furnace-spot)
                (if (and (= :done r) (:placed res))
                  (do (ctx/update-mem! c #(-> % (assoc :furnace spot) (dissoc :want-furnace)))
                      (places/offer! c :furnace spot)
                      (ctx/emit! c :smelt.furnace :info {:furnace spot :text (str "put a furnace down at " (:x spot) " " (:y spot) " " (:z spot))})
                      :again)
                  (do (ctx/update-mem! c assoc :furnace-refusal (str (or (:why res) (some-> (:reason res) name) "it was not placed")))
                      nil)))))))))

(defn ^:async furnace-fetch!
  "With no furnace seen: put a carried one down, else fetch one and put it down. Once per smelt. :again / :continue,
  nil when there is nothing to do."
  [c]
  (when (and (fetch/opts c 'jobs.items.smelt) (not (:furnace-tried (ctx/mem c))))
    (when-not (pos? (storage/carried (carried c) "furnace")) (ctx/update-mem! c assoc :want-furnace true))
    (let [f (when (:want-furnace (ctx/mem c)) (await (fetch/fetch! c 'jobs.items.smelt furnace-problem)))
          r (when (and (nil? f) (pos? (storage/carried (carried c) "furnace"))) (await (place-furnace! c)))]
      (when-not (or f r) (ctx/update-mem! c #(-> % (dissoc :want-furnace) (assoc :furnace-tried true))))
      (or f r))))

(defn ^:async round-fetched
  "The round once the missing input and fuel are fetched."
  [c]
  (cond
    (furnace-of c) (await (round-with c (furnace-of c)))
    :else
    (if-let [pos (nearest-furnace c (sp/input-of (:item (:args c)) (carried c)))]
      (do (ctx/update-mem! c assoc :furnace pos)
          (ctx/emit! c :smelt.furnace :info {:furnace pos :text (str "smelting at the furnace seen at " (:x pos) " " (:y pos) " " (:z pos))})
          (await (round-with c pos)))
      (or (await (look/survey! c :cell))
          (let [f (await (furnace-fetch! c))]
            (case f
              nil (if-let [why (and (pos? (storage/carried (carried c) "furnace")) (:furnace-refusal (ctx/mem c)))]
                    (stop! c "furnace-not-placed" nil (str "a furnace is carried but could not be put down: " why))
                    (stop! c "no-furnace-seen" nil (when (:furnace-tried (ctx/mem c)) "none could be made either: a furnace is 8 cobblestone mined with a pickaxe")))
              :again (await (round-with c (:furnace (ctx/mem c))))
              f))))))

(defn ^:async item-round
  "One bounded step: reach the furnace, read it, then load (first) or collect. Without a :furnace the first round
  chooses the nearest seen one (kept in memory); with none in view it looks around once from where it stands (the
  four headings, level and down, a sight pass after each) and chooses again; none seen after that ends no-furnace-seen."
  [c]
  (cond
    (and (nil? (:owed (ctx/mem c))) (:item (:args c)) (not (sp/smeltable? (sp/input-of (:item (:args c)) (carried c))))) (refuse-item! c (:item (:args c)))
    :else (or (await (fetch-stages! c)) (await (round-fetched c)))))

(defn raw-carried
  "The raw foods and ores carried, by name."
  [c]
  (->> (carried c) (map :name) distinct (filter #(or (sp/foods %) (sp/ore? %))) vec))

(defn batch-items
  "The items of a batch job, kept in memory once chosen: the :item list, or with :item nil the raw foods and ores
  carried when there are two or more; nil for a one-item job."
  [c]
  (let [item (:item (:args c))
        items (or (:items (ctx/mem c))
                  (cond (coll? item) (vec (distinct item))
                        (nil? item) (let [r (raw-carried c)] (when (> (count r) 1) r))))]
    (when items (ctx/update-mem! c assoc :items items))
    items))

(defn finish-batch!
  "End the batch: the sums of the children, each item's result and the items that gave up."
  [c items]
  (let [results (into {} (map (fn [i] [i (get-in (ctx/mem c) [:batch i])])) items)
        smelted (reduce + (map #(:smelted % 0) (vals results)))
        wanted (reduce + (map #(:wanted % 0) (vals results)))
        failed (into {} (keep (fn [[i r]] (when (:reason r) [i (:reason r)]))) results)
        out (cond-> {:smelted smelted :wanted wanted :items results}
              (seq failed) (assoc :failed failed)
              (zero? smelted) (assoc :status :stopped :reason (get failed (first items) "nothing-smeltable")))]
    (ctx/emit! c :smelt.batch-done (if (zero? smelted) :warn :info)
               {:smelted smelted :wanted wanted :failed failed
                :text (str "smelted " smelted " of " wanted " (" (str/join ", " items) ")")})
    (ctx/result! c out)
    :done))

(defn ^:async batch-round
  "One child smelt after another, one per item; :continue while a child waits on its cook."
  [c items]
  (loop []
    (let [done (:batch (ctx/mem c))
          item (first (remove #(contains? done %) items))]
      (if-not item
        (do (ctx/update-mem! c dissoc :active) (finish-batch! c items))
        (let [slot (keyword (str "item-" item))
              _ (ctx/update-mem! c assoc :active slot)
              out (await (ctx/call-child c slot 'jobs.items.smelt (assoc (:args c) :item item)))]
          (if (= :done out)
            (do (ctx/update-mem! c assoc-in [:batch item] (ctx/child-result c slot))
                (recur))
            :continue))))))

(defn ^:async round
  "One bounded step of the one item, or one child smelt after another for several items."
  [c]
  (if-let [items (batch-items c)]
    (await (batch-round c items))
    (await (item-round c))))
