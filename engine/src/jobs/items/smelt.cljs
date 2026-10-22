(ns jobs.items.smelt
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.waits :as waits]
            [jobs.lib.checks :as checks]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.items.smelt.fetch :as sf]
            [jobs.items.smelt.plan :as sp]
            [jobs.lib.furnace :as furnace]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.storage :as storage]
            [jobs.lib.cost :as cost]
            [jobs.lib.price :as price]
            [jobs.lib.util :as u]))

(def doc
  "Smelt :count of an item in a furnace, blast furnace or smoker without standing by it: walk there, take any output, load input and fuel,
  yield (:continue) until the cook is due, take the output. :item may be a list, and then
  every one is cooked: one child smelt per item, each with the same :count (data {:smelted n :wanted n :items {item result}},
  :failed the items that gave up). What is owed is kept in memory before the load, so a restart loads nothing twice.
  Needs: the input, the fuel (the :fuel item, else coal, charcoal or planks, sized by what one item smelts; a wood input with no other fuel burns some of itself) and a furnace. With fetching ways
  (the call's :ways minus :carried, default all; jobs.lib.need) a furnace comes first (a seen one, a carried one put down, else one fetched), then what else is
  missing is fetched by obtain children; the fetch's ending is the :because. :ways #{:carried} skips it.
  can-start?: never :not-smeltable for an item no furnace cooks. can-start? and can-run?: later :refused {:zones :claims} while a zone
  or claim of another bars :put or :take at the furnace (:ignore-zones? skips it); a cooking job yields until the cook is due,
  without touching the furnace.
  Quote: the cook time of the count, the input, fuel and product items; open-ended with no furnace given or seen. Default :spend
  :job-time is ::job-time-s plus the cook (10 s an item); :wall-time adds ::away-s for other jobs done while it cooks.
  A load is noted in memory (jobs.lib.furnace/cooking-kind) until the furnace is taken empty, so a cut leaves it for get-food to collect.
  Ends :done {:smelted n :wanted n}: :wanted is the count this job loaded, :smelted what came of it (input and output already in the
  furnace do not count; 0 of N is never :done). A give-up carries the same data plus :text, with the cause:
  - condition-failed: no-furnace-seen, no-furnace, not-a-furnace, furnace-gone, no-item, nothing-smeltable, not-smeltable, no-fuel,
    unknown-fuel, rejected-fuel, fuel-busy, furnace-busy, furnace-full, inventory-full (the output stays in the furnace).
  - nothing-possible: furnace-not-placed (:why says why), out-of-fuel (leftover input is taken back), output-gone, unreachable (a :furnace arg, or one holding a load, that cannot be reached; a furnace the job chose itself and cannot reach is dropped, and another is taken or its own put down), or the
    furnace's own status.")

(a/defargs args
  {:furnace {:doc "furnace, blast furnace or smoker position [x y z] or {:x :y :z}; when nil the nearest one the body has seen within 32 blocks that cooks :item (or anything carried) is chosen (smelt.furnace says which), or the job ends with no-furnace-seen" :spec ::a/pos :default nil}
   :item {:doc "what to smelt, or a list of them (an input; a product such as cooked_chicken or iron_ingot means its input, charcoal the carried log there is most of; an item no furnace cooks ends not-smeltable at once); the first smeltable thing carried when nil, " :spec (a/or-of a/item? (a/coll-of a/item?)) :default nil}
   :count {:doc "how many; all carried (at most one stack) when nil" :spec (a/int-in 0 nil) :default nil}
   :fuel {:doc "fuel item to load; the best carried when nil" :spec a/item? :default nil}
   :explore {:doc "look beyond what is seen for a missing input, fuel or furnace (passed to the fetches)" :spec boolean? :default false}
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

(defn can-put-own?
  "Whether the body carries a furnace or the 8 cobblestone to make one."
  [c]
  (let [inv (carried c)]
    (or (pos? (storage/carried inv "furnace")) (>= (storage/carried inv "cobblestone") 8))))

(defn cook-s
  "Seconds the cook of the count (a stack when nil) of each item takes at the furnace."
  [c]
  (let [{:keys [item count]} (:args c)
        n (* (or count 64) (if (coll? item) (clojure.core/count item) 1))]
    (/ (settings/ticks->ms (* n (sp/cook-ticks "furnace"))) 1000)))

(defn exposed-furnace?
  "Whether a seen furnace at pos is one a player would not walk to: it lies in the dark (readiness :exposed, priced by the whole cook there) and the body can put one down where it stands."
  [c pos]
  (and (can-put-own? c) (not= :ok (first (ready/exposed (assoc c ::price/stay-s (cook-s c)) pos)))))

(defn nearest-furnace
  "The nearest furnace, blast furnace or smoker the body has seen within seen-radius, still that block now, that
  cooks item (or, when item is nil, anything carried; any kind when nothing carried is smeltable) and that a walk has not
  found unreachable (mem :unreachable-furnaces); nil when none."
  [c item]
  (let [names (if item [item] (map :name (carried c)))
        kinds (filter (fn [kind] (some #(sp/smelts? kind %) names)) (keys sp/cook-ticks))
        ok? (if (or item (seq kinds)) (set kinds) furnace-block?)]
    (->> (look/seen-blocks (:primitives c) {:names furnace-block? :radius (seen-radius) :max 16 :live? true})
         (some #(when (and (ok? (:name %)) (not (contains? (:unreachable-furnaces (ctx/mem c)) (:pos %))) (not (exposed-furnace? c (:pos %)))) (:pos %))))))

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
  (let [r (js->clj (await (ctx/act c :furnace (clj->js (merge {:pos (furnace-of c) :op op} extra)))) :keywordize-keys true)]
    (when (and (= "take" op) (= "ok" (:status r)) (not (:input r)))
      (furnace/forget-cooking! c (furnace-of c)))
    r))

(defn wait-until!
  "Remember when to look again and yield."
  [c ms]
  (let [now (ctx/now c)]
    (ctx/update-mem! c assoc :ready-at (+ now ms) :unlit-from (+ now (grace-ms)))
    :continue))

(defn cooking-in?
  "Whether job memory m owes a load whose cook is not due: the job only waits."
  [c m]
  (and (furnace/loaded? m) (some? (:ready-at m)) (not (due? c m))))

(defn furnace-to-reach
  "The furnace the job walks to, nil when it will put its own down where it stands: a furnace it chose for itself, with no load in it, that is exposed (the arg and a load in it stay)."
  [c]
  (let [f (furnace-of c)]
    (when-not (and f (not (:furnace (:args c))) (not (furnace/loaded? (ctx/mem c))) (exposed-furnace? c f))
      f)))

(defn furnace-positions [c] (keep identity [(furnace-of c)]))

(defn furnace-refused
  "Later :refused (zones, claims) while the furnace refuses :put (the load) or :take (the output): a load that cannot be collected is not started."
  [c]
  (contract/all [((checks/container-refused every? :put furnace-positions) c)
                 ((checks/container-refused every? :take furnace-positions) c)]))

(defn can-run?
  "Refused while a zone or claim bars the furnace, else ok."
  [c]
  (furnace-refused c))

(def condition-reasons
  "Give-ups that name a missing or wrong fact; any other ends nothing-possible."
  #{"no-furnace-seen" "no-furnace" "not-a-furnace" "furnace-gone" "no-item" "nothing-smeltable" "not-smeltable" "no-fuel"
    "unknown-fuel" "rejected-fuel" "fuel-busy" "furnace-busy" "furnace-full" "inventory-full"})

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
               (furnace/note-cooking! c (furnace-of c) item target)
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
      (do (await (visit! c "take" {})) :continue)
      (:give-up plan) (stop! c (:give-up plan))
      :else
      (let [freed (when (:output state) (await (visit! c "take" {})))]
        (if (= "full" (:status freed))
          (stop! c "inventory-full")
          (await (load! c state plan)))))))

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
              (or (:input r) (:output r)) (wait-until! c (recheck-ms))
              :else (finish! c {})))
    :wait (wait-until! c (wait-ms (:kind state) state))
    :out-of-fuel (await (feed! c state))
    :not-smeltable (await (stop-back! c "not-smeltable"))
    :done (let [m (ctx/mem c)]
            (if (< (:got m 0) (:count (:owed m) 0))
              (stop! c "output-gone")
              (finish! c {})))))

(defn leave-furnace!
  "A furnace the job chose for itself and cannot reach, with no load in it, is dropped for this job (the next round takes another or puts its own down): :continue. Else give-up! \"unreachable\"."
  [c pos]
  (if (or (:furnace (:args c)) (furnace/loaded? (ctx/mem c)))
    (give-up! c "unreachable")
    (do (ctx/update-mem! c #(-> % (update :unreachable-furnaces (fnil conj #{}) pos) (dissoc :furnace)))
        (ctx/emit! c :smelt.furnace-unreachable :info {:furnace pos :text (str "the furnace at " (:x pos) " " (:y pos) " " (:z pos) " cannot be reached: taking another")})
        :continue)))

(defn ^:async round-at!
  "Reach the furnace, read it, then load (first) or collect."
  [c furnace owed?]
  (case (await (near/go-near! c furnace 3))
    :partial :continue
    :blocked (leave-furnace! c furnace)
    (let [state (await (visit! c "read" {}))]
      (case (:status state)
        "ok" (if (and owed? (:ready-at (ctx/mem c)))
               (await (collect! c state))
               (await (start! c state)))
        "missing" (stop! c (if owed? "furnace-gone" "no-furnace") (:block state))
        "cannot" (stop! c (if owed? "furnace-gone" "not-a-furnace") (:block state))
        "unreachable" (leave-furnace! c furnace)
        (stop! c (str "furnace " (:status state)))))))

(defn ^:async round-with
  "The round once the furnace is known."
  [c furnace]
  (await (round-at! c furnace (some? (:owed (ctx/mem c))))))

;; ------------------------------------------------------------------ the round, with the fetches

(defn choose-furnace!
  "Make the furnace at pos this job's: nil."
  [c pos]
  (ctx/update-mem! c assoc :furnace pos)
  (ctx/emit! c :smelt.furnace :info {:furnace pos :text (str "smelting at the furnace seen at " (:x pos) " " (:y pos) " " (:z pos))})
  nil)

(defn ^:async acquire-furnace!
  "With no furnace chosen: the nearest one seen, else the remembered one when walking there is cheaper than making one, is
  chosen (nil), else put a carried or fetched one down. nil once a furnace is chosen; :continue while that goes on; a
  stop when none can be had, after the remembered one at any distance."
  [c input]
  (if-let [pos (or (nearest-furnace c input) (sf/home-furnace c false))]
    (choose-furnace! c pos)
    (let [f (await (sf/furnace-fetch! c))]
      (case f
        nil (if-let [pos (sf/home-furnace c true)]
              (choose-furnace! c pos)
              (if-let [why (and (pos? (storage/carried (carried c) "furnace")) (:furnace-refusal (ctx/mem c)))]
                (stop! c "furnace-not-placed" nil (str "a furnace is carried but could not be put down: " why))
                (stop! c "no-furnace-seen" nil (when (:furnace-tried (ctx/mem c)) (str "none could be made either: " (or (:furnace-why (ctx/mem c)) "nothing can be fetched"))))))
        :again nil
        f))))

(defn ^:async item-round
  "One bounded step: yield while the cook is not due, else get a furnace (the nearest one seen, else one put down:
  a body that cannot have one stops before it spends logs on fuel), fetch the missing input and fuel, reach the
  furnace, read it, then load (first) or collect. A furnace out of sight is looked for once by can-start? (a :look status); none after that ends no-furnace-seen."
  [c]
  (or (when (cooking-in? c (ctx/mem c)) (ctx/hold-still! c :cooking) :continue)
      (when (and (:furnace (ctx/mem c)) (nil? (furnace-to-reach c)))
        (ctx/update-mem! c dissoc :furnace)
        nil)
      (when-not (or (furnace-of c) (some? (:owed (ctx/mem c))))
        (await (acquire-furnace! c (sp/input-of (:item (:args c)) (carried c)))))
      (await (sf/fetch-stages! c (fetch-fuels) (fetch-wood-fuels)))
      (await (round-with c (furnace-of c)))))

(defn batch-items
  "The items of a batch job, kept in memory once chosen: the :item list; nil for a one-item job."
  [c]
  (let [item (:item (:args c))
        items (or (:items (ctx/mem c))
                  (when (coll? item) (vec (distinct item))))]
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
      (let [e (waits/waiting-out c (await (ctx/call c (keyword (str "item-" item)) 'jobs.items.smelt (assoc (:args c) :item item))))]
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
(def ways need/fetch-ways)

(defn stay-s
  "Seconds the smelt keeps the body at the furnace: the cook and the walk there and back."
  [c]
  (let [f (furnace-to-reach c)]
    (+ (cook-s c)
       (if f (/ (* 2 (u/dist (u/self-pos c) f)) (cost/walk-blocks-per-s)) 0))))

(def safe? (price/stays (ready/safe-at-by-night furnace-to-reach) stay-s))
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
  "Never when the :item is one no furnace cooks; later :refused while a zone or claim bars the furnace; look (once) when a
  single item is to be smelted and no furnace is known or carried: the round puts one down or fetches one after that."
  [c]
  (let [item (:item (:args c))]
    (cond
      (and item (not (coll? item)) (not (sp/smeltable? (sp/input-of item (carried c)))))
      (contract/never :not-smeltable (str item " does not smelt") :item item)

      (and item (not (coll? item)))
      (contract/all [(furnace-refused c)
                     (checks/seen-or-look c (boolean (or (furnace-of c) (pos? (storage/carried (carried c) "furnace")) (nearest-furnace c (sp/input-of item (carried c))))) :no-furnace
                                          {:known "a furnace is known" :missing "no furnace in sight" :fetches? true})])

      :else (furnace-refused c))))

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
  "The items a quote is made for: the :item list, or the one item (nil: the first smeltable thing carried)."
  [c]
  (let [item (:item (:args c))]
    (if (coll? item) item [item])))

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
