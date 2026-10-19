(ns jobs.items.smelt
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.items.smelt.fetch :as sf]
            [jobs.items.smelt.plan :as sp]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.furnace :as furnace]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(def doc
  "Smelt :count of an item in a furnace, blast furnace or smoker without standing by it.
  :item may be a list, and with :item nil and two or more raw foods or ores carried every one of them is cooked: one child
  smelt per item, in order, each with the same :count; the data is {:smelted n :wanted n :items {item result}} (:failed
  names the items that gave up; ended with the first item's cause when nothing was smelted).
  The first round walks to the furnace, reads it, takes any output already there (it does not count) and loads
  the input and the fuel. Fuel is worked out from the count: in a furnace one fuel item smelts 8 items for coal
  or charcoal, 1.5 for overworld wood (crimson and warped do not burn), 0.5 for a stick (twice that in a blast furnace or smoker). It is taken from what is carried when :fuel is nil, coal and charcoal first.
  The job then yields (:continue) until the cook is due, so the body can do other jobs; the next round takes the output.
  Due is decided by the clock: ready-at is now plus the cook time of the items loaded (200 ticks each in a
  furnace, 100 in the other two) plus a margin. It is also due once :unlit-from has passed and the furnace block is
  no longer lit or no longer a furnace. A cooking job waits on can-run? (:later :cooking with :loaded n), which reads only memory, the clock and one block. A
  woken round that finds the cook unfinished waits again from the furnace's own bars.
  What is owed is kept in memory before the load, so a restart or cut neither loses the output nor loads twice.
  Needs: the input, the fuel (the :fuel item, else coal or charcoal, else planks of any wood, sized by what one item of it
  smelts) and a furnace. With :fetch (default true; jobs.lib.need) a furnace comes first (when none is seen, a carried one is put down beside the body, else one is fetched and put
  down; none to be had ends no-furnace-seen before any fuel is fetched), then what else is missing is fetched by obtain
  children. A fetch that finds nothing leaves the cause below (no-item, no-fuel, no-furnace-seen), the fetch's ending
  its :because. :fetch false skips all of it. Starts only with an item some furnace cooks (impossible :not-smeltable, the job leaves the queue;
  an output such as cooked_chicken names its input). Quote: the cook time of the count (a furnace; half in the other
  two), the input and fuel items, the product; open-ended when no furnace is given or seen.
  The default :spend :job-time is ::job-time-s for the walk and fetches plus the cook of the count (10 s an item, a stack when nil);
  the default :wall-time is that plus ::away-s, for other jobs done while it cooks.
  Ends :done with {:smelted n :wanted n}: :wanted is the count this job loaded and :smelted what came of it; input
  already in the furnace and the output already there do not count. A give-up ends with the
  cause named and the same data (plus :text) (0 of N is never :done):
  - condition-failed: no-furnace-seen (no :furnace given and none seen), no-furnace, not-a-furnace, furnace-gone (broken or
    replaced meanwhile), no-item, nothing-smeltable, not-smeltable (this kind of furnace cannot cook it, also found when
    it never lit), no-fuel, unknown-fuel, rejected-fuel, fuel-busy (the fuel slot holds another fuel), furnace-busy (the
    input slot holds another item that is not cooking; one that is cooking is waited out, its output taken, then the
    load goes in), furnace-full, inventory-full (the output stays in the furnace), refused (with :zones, :claims: the
    furnace is in another owner's zone or claim and does not allow :put or :take; nothing is loaded or taken;
    :ignore-zones? true skips the check).
  - nothing-possible: furnace-not-placed (one is carried and could not be put down; :why says why), out-of-fuel
    (leftover input is taken back), output-gone (input and output both gone), unreachable, or the furnace's own status.")

(a/defargs args
  {:furnace {:doc "furnace, blast furnace or smoker position [x y z] or {:x :y :z}; when nil the nearest one the body has seen within 32 blocks that cooks :item (or anything carried) is chosen (smelt.furnace says which), or the job ends with no-furnace-seen" :spec ::a/pos :default nil}
   :item {:doc "what to smelt, or a list of them (an input; a product such as cooked_chicken or iron_ingot means its input, charcoal the carried log there is most of; an item no furnace cooks ends not-smeltable at once); the first smeltable thing carried when nil, or every raw food and ore carried when there are several" :spec (a/or-of a/item? (a/coll-of a/item?)) :default nil}
   :count {:doc "how many; all carried (at most one stack) when nil" :spec (a/int-in 0 nil) :default nil}
   :fuel {:doc "fuel item to load; the best carried when nil" :spec a/item? :default nil}
   :fetch {:doc "get a missing input, fuel or furnace (jobs.lib.need): true, a set of kinds or a map of limits; false ends with the reason" :spec need/option? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds the call may take besides the cook of its count (the walk and the fetches); the default time spend is this plus the cook, 10 s an item." :spec (a/num-in 0 nil)}
   ::away-s {:default 1800 :doc "Seconds the call may last from its start beyond its time spend: other jobs the body does while the furnace cooks (the default :wall-time spend is the time spend plus this)." :spec (a/num-in 0 nil)}
   ::slack-ms {:default 2000 :doc "Slack added to a wait for the furnace, in ms." :spec (a/int-in 0 nil)}
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

;; ------------------------------------------------------------------ the call

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
  "Whether the cook of the job memory m is due: no furnace chosen yet, nothing loaded yet, the clock past ready-at, or since
  :unlit-from the furnace block unlit or gone."
  [c m]
  (let [furnace (or (:furnace (:args c)) (:furnace m))
        now (ctx/now c)]
    (or (nil? furnace)
        (nil? (:ready-at m))
        (>= now (:ready-at m))
        (and (>= now (:unlit-from m)) (needs-attention? (:primitives c) furnace)))))

;; ------------------------------------------------------------------ the round

(defn ^:async visit!
  "One furnace visit through act, as a cljs map."
  [c op extra]
  (let [r (await (ctx/act c :furnace (clj->js (merge {:pos (furnace-of c) :op op} extra))))]
    (js->clj r :keywordize-keys true)))

(defn wait-until!
  "Remember when to look again and yield."
  [c ms]
  (let [now (ctx/now c)]
    (ctx/update-mem! c assoc :ready-at (+ now ms) :unlit-from (+ now (grace-ms)))
    :continue))

(defn cooking?
  "Whether a load is owed and its cook is not due: the job only waits."
  [c]
  (let [m (ctx/mem c)]
    (and (furnace/loaded? m) (some? (:ready-at m)) (not (due? c m)))))

(defn can-run?
  "Later :cooking with the count loaded while the load cooks; the parent reads it as the child's status."
  [c]
  (if (cooking? c)
    (let [{:keys [count base] :or {count 0 base 0}} (:owed (ctx/mem c))]
      (contract/later :cooking "cooking" :loaded (- count base)))
    (checks/holds)))

(def condition-reasons
  "Give-ups that name a missing or wrong fact; any other ends nothing-possible."
  #{"no-furnace-seen" "no-furnace" "not-a-furnace" "furnace-gone" "no-item" "nothing-smeltable" "not-smeltable" "no-fuel"
    "unknown-fuel" "rejected-fuel" "fuel-busy" "furnace-busy" "furnace-full" "inventory-full" "refused"})

(defn results
  "What came of the load: {:smelted n :wanted n} plus extra."
  [c extra]
  (let [m (ctx/mem c)]
    (merge {:smelted (max 0 (- (:got m 0) (:base (:owed m) 0)))
            :wanted (- (:count (:owed m) 0) (:base (:owed m) 0))}
           extra)))

(defn end-with
  "The ending for give-up reason (a string), cause the keyword of it unless given, data the job's result fields."
  ([c reason data] (end-with c reason (keyword (str/replace reason " " "-")) data))
  ([c reason cause data]
   (ctx/end c (if (condition-reasons reason) :condition-failed :nothing-possible) cause data)))

(defn finish!
  "End :done with what was smelted; a load of which nothing came is output-gone, never done."
  [c extra]
  (let [r (results c extra)]
    (if (and (pos? (:wanted r)) (zero? (:smelted r)))
      (end-with c "output-gone" (assoc r :text "smelt gave up: output-gone"))
      (ctx/end c :done nil (assoc r :text (str "smelted " (:smelted r)))))))

(defn stop!
  "End with a cause; block is the block the body saw where the furnace should be, why the cause when the reason alone misleads."
  ([c reason] (stop! c reason nil nil))
  ([c reason block] (stop! c reason block nil))
  ([c reason block why]
   (end-with c reason (results c (cond-> {:text (str "smelt gave up: " reason (when block (str " (saw " block ")")) (when why (str ": " why)))}
                                   block (assoc :block block)
                                   why (assoc :why why))))))

(defn ^:async stop-back!
  "Give up after a load: leftover input goes back to the pockets first."
  [c reason]
  (await (visit! c "take" {:output false :input true}))
  (stop! c reason))

(defn refuse!
  "End refused: the furnace is in another's zone or claim; the ending names them."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])]
    (end-with c "refused" (assoc (select-keys fields [:zones :claims]) :reason :refused)
              (results c {:text (str "smelt gave up: refused by " (access/refusal-text fields))}))))

(defn give-up!
  "Count a failed round in a row: :continue until u/max-failures, then stop with the reason."
  [c reason]
  (if (u/count-fail! c)
    (stop! c reason)
    :continue))

(defn ^:async load!
  "Put the planned input and fuel in. :continue when they are in, else the ending it gives up with."
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
      "busy" (if (= "input" (:slot r))
               (do (ctx/update-mem! c dissoc :owed :target)
                   (give-up! c "furnace-busy"))
               (stop! c "fuel-busy"))
      "no-item" (stop! c "no-item")
      "rejected" (stop! c (if (= "fuel" (:slot r)) "rejected-fuel" "furnace-full"))
      (stop! c (str "load " (:status r))))))

(defn ^:async start!
  "The first rounds: plan, take what the furnace holds already, load."
  [c state]
  (let [m (ctx/mem c)
        {:keys [count fuel]} (:args c)
        item (sp/input-of (:item (:args c)) (carried c))
        plan (sp/plan-load {:state state :carried (carried c) :held (reserve/held c :smelt) :item item :count count :fuel fuel :target (:target m)})]
    (cond
      (and (= "furnace-busy" (:give-up plan)) (= :wait (sp/judge state))) (wait-until! c (wait-ms (:kind state) state))
      (and (= "furnace-busy" (:give-up plan)) (= :take (sp/judge state)))
      (if-let [v (access/container-refusal c :take (furnace-of c))]
        (refuse! c v)
        (do (await (visit! c "take" {})) :continue))
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
        plan (sp/plan-load {:state state :carried (carried c) :held (reserve/held c :smelt) :item item :fuel (:fuel (:args c)) :target remaining})]
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
              :else (finish! c {})))
    :wait (wait-until! c (wait-ms (:kind state) state))
    :out-of-fuel (if-let [v (access/container-refusal c :put (furnace-of c))]
                   (refuse! c v)
                   (await (feed! c state)))
    :not-smeltable (await (stop-back! c "not-smeltable"))
    :done (let [m (ctx/mem c)]
            (if (< (:got m 0) (:count (:owed m) 0))
              (stop! c "output-gone")
              (finish! c {})))))

(defn ^:async reach!
  "Walk until the body is within 3 cells of the furnace, as a go-to child: :there, :partial (it waits on the world, or
  got nearer: yield) or :blocked (it gave up)."
  [c pos]
  (let [cell (near/cell-of pos)]
    (cond
      (nil? cell) (do (ctx/emit! c :refused :warn {:reason :bad-pos :text (:message (places/parse-pos pos))})
                      :blocked)
      (u/within? (u/self-pos c) cell 3) :there
      :else (let [r (await (child/call-walk* c :walk {:pos cell :range 3 :doors :shut
                                                      :one-way :closed :spend {:tries 1}
                                                      :ignore-zones? (boolean (:ignore-zones? (:args c)))}))
                  res (:data r)]
              (cond
                (= :continue r) :partial
                (not= :done (:kind r)) :blocked
                (:arrived res) :there
                (:leg res) :partial
                :else :blocked)))))

(defn ^:async round-at!
  "Reach the furnace, read it, then load (first) or collect."
  [c furnace owed?]
  (case (await (reach! c furnace))
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

;; ------------------------------------------------------------------ the round, with the fetches

(defn ^:async acquire-furnace!
  "With no furnace chosen: the nearest one seen is chosen (nil), else look around once, then put a carried or
  fetched one down. nil once a furnace is chosen; :continue while that goes on; a stop when none can be had."
  [c input]
  (if-let [pos (nearest-furnace c input)]
    (do (ctx/update-mem! c assoc :furnace pos)
        (ctx/emit! c :smelt.furnace :info {:furnace pos :text (str "smelting at the furnace seen at " (:x pos) " " (:y pos) " " (:z pos))})
        nil)
    (or (await (look/survey! c :cell))
        (let [f (await (sf/furnace-fetch! c))]
          (case f
            nil (if-let [why (and (pos? (storage/carried (carried c) "furnace")) (:furnace-refusal (ctx/mem c)))]
                  (stop! c "furnace-not-placed" nil (str "a furnace is carried but could not be put down: " why))
                  (stop! c "no-furnace-seen" nil (when (:furnace-tried (ctx/mem c)) "none could be made either: a furnace is 8 cobblestone mined with a pickaxe")))
            :again nil
            f)))))

(defn ^:async item-round
  "One bounded step: yield while the cook is not due, else get a furnace (the nearest one seen, else one put down:
  a body that cannot have one stops before it spends logs on fuel), fetch the missing input and fuel, reach the
  furnace, read it, then load (first) or collect. With none in view it looks around once from where it stands (the
  four headings, level and down, a sight pass after each) first; none after that ends no-furnace-seen."
  [c]
  (or (when-not (or (furnace-of c) (some? (:owed (ctx/mem c))))
        (await (acquire-furnace! c (sp/input-of (:item (:args c)) (carried c)))))
      (await (sf/fetch-stages! c (fetch-fuels) (fetch-wood-fuels)))
      (await (round-with c (furnace-of c)))))

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
        data (cond-> {:smelted smelted :wanted wanted :items results
                      :text (str "smelted " smelted " of " wanted " (" (str/join ", " items) ")")}
               (seq failed) (assoc :failed failed))]
    (if (zero? smelted)
      (end-with c (get failed (first items) "nothing-smeltable") data)
      (ctx/end c :done nil data))))

(defn batch-entry
  "The result kept for one item of a batch from its child's ending e: the data, with :reason when it did not end done."
  [e]
  (cond-> (merge {:smelted 0 :wanted 0} (:data e))
    (not= :done (:kind e)) (assoc :reason (name (contract/cause-key e)))))

(defn ^:async batch-step!
  "One child smelt for the next item without a result: :again, or :continue while it waits on its cook; the end after the last, before done? can cut it."
  [c items]
  (let [done (:batch (ctx/mem c))
        item (first (remove #(contains? done %) items))]
    (if-not item
      (finish-batch! c items)
      (let [e (await (ctx/call c (keyword (str "item-" item)) 'jobs.items.smelt (assoc (:args c) :item item)))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c assoc-in [:batch item] (batch-entry e))
              (if (every? #(contains? (:batch (ctx/mem c)) %) items) (finish-batch! c items) :again)))))))

(defn ^:async batch-round
  "One child smelt after another, one per item; :continue while a child waits on its cook."
  [c items]
  (await (pace/steps! c #(batch-step! c items))))

(defn ^:async round
  "One bounded step of the one item, or one child smelt after another for several items."
  [c]
  (if-let [items (batch-items c)]
    (await (batch-round c items))
    (await (item-round c))))

;; ------------------------------------------------------------------ the contract

(def target [:item])

(def safe? (ready/safe-at-by-night furnace-of))
(def fit? ready/ok)

(defn default-time
  "The call's default :spend :job-time: ::job-time-s for the walk and fetch plus the furnace's cook of the count (a stack when
  nil) of each item."
  [{:keys [item count]}]
  (let [n (* (or count 64) (if (coll? item) (clojure.core/count item) 1))]
    (+ (settings/get settings ::job-time-s) (/ (settings/ticks->ms (* n (sp/cook-ticks "furnace"))) 1000))))

(defn default-wall-time
  "The call's default :spend :wall-time: its default time plus ::away-s."
  [args]
  (+ (default-time args) (settings/get settings ::away-s)))

(def defaults {:spend {:job-time default-time :wall-time default-wall-time}})

(defn can-start?
  "Never when the :item is one no furnace cooks."
  [c]
  (let [item (:item (:args c))]
    (if (and item (not (coll? item)) (not (sp/smeltable? (sp/input-of item (carried c)))))
      (contract/never :not-smeltable (str item " does not smelt") :item item)
      (checks/holds))))

(defn done?
  "Done once the load is smelted and taken: :got reaches the count the load owes, or every item of a batch has its result
  without a give-up. The goal is relative (N more), so it is read from the call's own load record."
  [c]
  (let [m (ctx/mem c)
        batch (:batch m)
        entries (map #(get batch %) (:items m))
        smelted (if batch
                  (transduce (map #(:smelted % 0)) + 0 (vals batch))
                  (max 0 (- (:got m 0) (:base (:owed m) 0))))
        wanted (if batch
                 (transduce (map #(:wanted % 0)) + 0 (vals batch))
                 (- (:count (:owed m) 0) (:base (:owed m) 0)))
        data [:units :items :done smelted :left (max 0 (- wanted smelted)) :result {:smelted smelted}]
        text (str "smelt: " smelted " of " wanted " smelted")]
    (if (if batch
          (and (seq entries) (every? #(and % (not (:reason %))) entries) (pos? smelted))
          (and (:owed m) (pos? wanted) (>= smelted wanted)))
      (apply contract/ok :smelted text data)
      (apply contract/later :smelting text data))))

(defn quote-items
  "The items a quote is made for: the :item list, the one item, or the raw foods and ores carried."
  [c]
  (let [item (:item (:args c))]
    (cond (coll? item) item
          item [item]
          :else (let [r (raw-carried c)] (if (seq r) r [nil])))))

(defn quote-of
  "The cook time of the items a load puts in, those items, the fuel and the product; open-ended when no furnace is given
  or seen."
  [c]
  (let [{:keys [furnace count fuel]} (:args c)
        figs (keep #(let [f (sp/load-figures {:carried (carried c) :item % :count count :fuel fuel})]
                      (when-not (:give-up f) f))
                   (quote-items c))
        n (if (seq figs) (transduce (map :count) + 0 figs) (or count 1))
        cook (/ (settings/ticks->ms (* n (sp/cook-ticks "furnace"))) 1000)
        walk (if furnace 20 60)
        figure (fn [k] [k k k])
        add (fn [m item k] (cond-> m item (update item #(mapv + (or % [0 0 0]) (figure k)))))
        items (reduce (fn [m f] (-> m (add (:item f) (:count f)) (add (get-in f [:fuel :item]) (get-in f [:fuel :count] 0)))) {} figs)
        yields (reduce (fn [m f] (add m (:product f) (:count f))) {} figs)]
    (cond-> {:uses {:job-time [(+ walk (/ cook 2)) (+ walk cook) (if (or furnace (nearest-furnace c nil)) (+ walk cook 120) ##Inf)]}}
      (seq items) (assoc-in [:uses :items] items)
      (seq yields) (assoc :yields {:items yields}))))

(def meta {:tags [:smelt :fuel]})
