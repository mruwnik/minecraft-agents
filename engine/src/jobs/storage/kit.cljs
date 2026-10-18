(ns jobs.storage.kit
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.food :as food]
            [jobs.lib.stock :as stock]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.lib.need :as getting]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.storage :as storage]
            [jobs.lib.least :as least]
            [jobs.storage.withdraw]
            [jobs.lib.foods :as foods]))

(def doc
  "Take a kit out of the chest: :spare + 1 of each tool kind in :tools, and :food food items (by default enough to
  hold the 3-day food reserve, jobs.lib.cost/food-reserve, counted by the real hunger points of the chest food).
  A name is of a tool kind when it equals it or ends in _kind. Any tier counts, the best tier and best food are
  taken first. Food is
  any name in the eat table.
  One call is the whole kit: it yields :continue only while a child waits on the world. Each step works out the needs from the inventory and the plan from the inspected chest, and hands the plan to
  jobs.storage.withdraw as carry-at-least targets.
  Needs (can-start?): a known chest unless the kit is complete, else :no-chest. Ends done with data {:gave-up false :short {}} when the kit is
  complete, or at any other ending when at least the call's :least kinds are complete (:all wants every one). Every other ending has :short, what is still lacking keyed by the kind string or :food, and :gave-up:
  - condition-failed :chest-lacks {:gave-up false}: the chest could not supply it (and :missing {kind text} when the craft
    phase found no way);
  - after failed attempts {:gave-up true}: condition-failed :missing, or nothing-possible with the
    inspect status or the withdraw's cause; a withdraw or craft child that stopped passes its kind and cause up;
    a chest that refuses (zone or claim) ends at once condition-failed :refused {:zones :claims}.
  Quote: a walk and a visit per kind; done? counts the kinds complete.
  With :craft (the default) a short does not end the job. It enters a craft phase. Each craft step makes
  exactly one child call:
  - For a short tool kind K, the tiers of :craft-tiers are tried in order for tier_K (jobs.items.craft).
  - When the craft comes back short of a material, the chest is asked for it (jobs.storage.withdraw). Failing
    that, a stick or planks is crafted as a sub-step (at most two deep). Failing that, the tier is ruled out and
    the next is tried.
  - Food is made as bread from wheat (chest or body), in batches of 3.
  A craft that finds no crafting table in reach first gets one (jobs.lib.need, unless :fetch is false) and the craft puts
  it down; a table that cannot be had is \"no-table\".
  The result gets :missing {kind text} for what could not be made (only when non-empty). \"no-table\" and
  \"full\" end the phase for every short kind.
  Every inspect books what the chest holds in body memory :fetch/stock (jobs.lib.stock).")

(a/defargs args
  {:tools {:doc "tool kinds to carry, e.g. [\"hoe\" \"pickaxe\"]" :spec (a/coll-of (a/or-of string? keyword?)) :default ["hoe"]}
   :spare {:doc "extra of each tool kind beyond the one in use" :spec (a/int-in 0 nil) :default 1}
   :food {:doc "how many food items to carry; enough to hold the body's food reserve (jobs.lib.cost/food-reserve) when nil" :spec (a/int-in 0 nil) :default nil}
   :chest {:doc "chest position; the known :chest place when nil" :spec ::a/pos :default nil}
   :craft {:doc "craft what the chest cannot supply" :spec boolean? :default true}
   :fetch {:doc "get a crafting table (jobs.lib.need) when a craft finds none; false ends no-table" :spec boolean? :default true}
   :craft-tiers {:doc "tool tiers to craft, in order; iron or diamond only when listed" :spec (a/coll-of (a/or-of string? keyword?)) :default ["stone" "wooden"]}
   :radius {:doc "how far to look for a crafting table" :spec (a/num-in 0 nil) :default 32}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds a kit may last: the walks, the chest visits and the crafts." :spec (a/num-in 0 nil)}
   ::kind-s {:default 20 :doc "Seconds a quote counts for each kind of the kit." :spec (a/num-in 0 nil)}})

(def target [:chest])

(def safe? ready/safe-at-chest)
(def fit? ready/ok)

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn kind-of?
  "Does item name belong to tool kind: equal to it or ending in _kind."
  [kind name]
  (or (= kind name) (str/ends-with? name (str "_" kind))))

(defn matcher
  "Predicate on item names for a kind: a tool kind string, or :food."
  [kind]
  (if (= :food kind)
    foods/edible?
    #(kind-of? kind %)))

(defn count-of
  "Total of the items (maps with :name :count) whose name matches kind."
  [items kind]
  (let [match? (matcher kind)]
    (transduce (comp (filter #(match? (:name %))) (map :count)) + 0 items)))

(defn food-items
  "How many items bring the inventory to the food reserve: the best food of chest-items first, counted by its real
  hunger points (bread's 5 each while the chest is unknown or runs out)."
  ([inventory] (food-items inventory nil))
  ([inventory chest-items]
   (let [stocks (->> (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} chest-items)
                     (filter (fn [[name _]] (food/reserve-food? name)))
                     (sort-by (fn [[name _]] (- (foods/points name)))))
         [items left] (reduce (fn [[items left] [name n]]
                                (if (pos? left)
                                  (let [k (min n (js/Math.ceil (/ left (foods/points name))))]
                                    [(+ items k) (- left (* k (foods/points name)))])
                                  (reduced [items left])))
                              [0 (cost/food-short inventory)]
                              stocks)]
     (+ items (js/Math.ceil (/ left (foods/points "bread")))))))

(defn needs
  "[[kind n] ...] still needed: the tools kinds in order, then :food; n above 0 only. Without :food the food need
  is what brings the inventory to the reserve; chest-items (when known) give the points of what can be taken."
  ([inventory args] (needs inventory args nil))
  ([inventory {:keys [tools spare food] :or {tools ["hoe"] spare 1}} chest-items]
   (->> (concat (map (fn [k] [k (inc spare)]) tools)
                [[:food (or food (+ (count-of inventory :food) (food-items inventory chest-items)))]])
        (map (fn [[kind want]] [kind (- want (count-of inventory kind))]))
        (filter (fn [[_ n]] (pos? n)))
        vec)))

(defn rank
  "Sort key, higher is better: material tier of a tool name, food points of a food."
  [kind name]
  (if (= :food kind)
    (or (foods/points name) 0)
    (weapon/weapon-rank name)))

(defn plan
  "{:take {name target} :short {kind n}} for needs against chest-items (maps
  with :name :count). Per kind the best matching names first, up to n in all;
  a taken name's target is what is carried of it plus what is taken."
  [needs inventory chest-items]
  (let [held (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} chest-items)]
    (reduce
     (fn [acc [kind n]]
       (let [match? (matcher kind)
             names (->> (keys held)
                        (filter match?)
                        (sort-by #(- (rank kind %))))
             [taken left] (reduce (fn [[taken left] name]
                                    (let [k (min left (get held name))]
                                      [(if (pos? k) (assoc taken name k) taken) (- left k)]))
                                  [{} n] names)
             targets (into {} (map (fn [[name k]] [name (+ k (storage/carried inventory name))])) taken)]
         (-> acc
             (update :take merge targets)
             (update :short #(if (pos? left) (assoc % kind left) %)))))
     {:take {} :short {}}
     needs)))

(defn can-start?
  "A chest is known, or nothing is needed: a complete kit then ends at once without walking or inspecting."
  [c]
  (if (seq (needs (u/inventory (:primitives c)) (:args c)))
    (checks/chest-known c)
    (contract/ok :complete "the kit is complete")))

(defn chest-of!
  "The chest of this call, kept in memory at the first look so a place forgotten meanwhile does not lose it."
  [c]
  (or (:chest (ctx/mem c))
      (when-let [chest (storage/chest-of (ctx/view c) (:args c))]
        (ctx/update-mem! c assoc :chest chest)
        chest)))

(defn done?
  "Done once every kind of the kit (tools and food) is carried."
  [c]
  (let [short (into {} (needs (u/inventory (:primitives c)) (:args c)))
        left (count short)
        all (inc (count (:tools (:args c))))
        data [:units :kinds :done (- all left) :left left :result {:short short}]]
    (if (zero? left)
      (apply contract/ok :stocked "kit: complete" data)
      (apply contract/later :stocking (str "kit: " left " kind(s) short") data))))

(defn quote-of
  "A walk and a visit per kind."
  [c]
  (let [kinds (inc (count (:tools (:args c))))
        t (* kinds (settings/get settings ::kind-s))]
    {:uses {:job-time [0 t (max t (settings/get settings ::job-time-s))]}}))

(defn end!
  "ctx/end, but done when the units of done? meet the call's :least."
  [c kind cause data]
  (let [{:keys [done left]} (nth (done? c) 2)]
    (least/end! c kind cause data done (+ done left))))

(defn give-up!
  "u/fail!, and when it gives up end with the reason (a chest that is not there is a failed condition, no route is nothing-possible)
  and what is short."
  [c reason short]
  (let [r (u/fail! c :kit.gave-up (str "kit gave up: " reason))]
    (if (= :done r)
      (end! c (if (= "missing" reason) :condition-failed :nothing-possible) (keyword reason)
               {:gave-up true :short short})
      :again)))

(defn refused!
  "End at once with the withdraw child's refusal e and what is short: another's chest does not get asked three times."
  [c e short]
  (end! c :condition-failed {:reason :refused} (merge {:gave-up true :short short} (select-keys (:data e) [:zones :claims]))))

(defn child-stop!
  "End like the child that stopped, with what is short."
  [c e short]
  (if (= :refused (contract/cause-key e))
    (refused! c e short)
    (end! c (:kind e) (:cause e) {:gave-up true :short short})))

(defn finish!
  "Done when nothing is short, else condition-failed :chest-lacks; more is data."
  ([c short] (finish! c short nil))
  ([c short more]
   (let [data (merge {:gave-up false :short short} more)]
     (if (empty? short)
       (ctx/end c :done nil data)
       (end! c :condition-failed :chest-lacks data)))))

(defn result-of
  "A child's ending e as the result absorb reads: its data, with :reason the cause when it did not end done."
  [e]
  (cond-> (:data e)
    (not= :done (:kind e)) (assoc :reason (contract/cause-key e))))

(defn stacks-of
  "The inspected container items as cljs maps {:name :count}."
  [items]
  (mapv (fn [i] {:name (.-name i) :count (.-count i)}) (array-seq items)))

(def craft-keys [:phase :try :ruled :why :missing :steps :chest-items])

(def table-reasons #{"no-table" "not-a-table" "unreachable" "full"})

(defn mkey
  "Memory key of a kind: the keyword :food is kept as the string \"food\"."
  [kind]
  (if (= :food kind) "food" kind))

(defn tiers-of
  "The tiers to try, in order; golden is never used."
  [a]
  (vec (remove #{"golden" "gold"} (:craft-tiers a))))

(defn chest-count
  "How many of name the inspected stacks hold."
  [stacks name]
  (storage/carried stacks name))

(defn rule-out
  "Drop the tool being made for its kind: remember the tier and why."
  [mem text]
  (let [{:keys [kind tier]} (:try mem)]
    (-> mem
        (assoc :steps nil :try nil)
        (update-in [:ruled kind] (fnil conj []) tier)
        (update-in [:why kind] #(if % (str % ", " text) text)))))

(defn mark-missing [mem kind text]
  (assoc-in mem [:missing (mkey kind)] text))

(defn plan-call
  "[mem call] for the first still-short kind not given up on; call is
  {:slot :job :args :for} or nil when nothing more can be tried. Pure."
  [mem a still stacks inv chest]
  (let [[kind n] (first (remove #(contains? (:missing mem) (mkey (first %))) still))
        wheat-held (chest-count stacks "wheat")
        wheat-body (storage/carried inv "wheat")]
    (cond
      (nil? kind) [mem nil]
      (= :food kind)
      (let [loaves (min n (quot (+ wheat-held wheat-body) 3))]
        (cond
          (zero? loaves) (recur (mark-missing mem kind "wheat") a still stacks inv chest)
          (< wheat-body (* 3 loaves)) [mem {:slot :get :job 'jobs.storage.withdraw :for :food-get
                                            :args (merge (select-keys a [:ignore-zones?])
                                                         {:chest chest :items {"wheat" (* 3 loaves)}})}]
          :else [mem {:slot :craft :job 'jobs.items.craft :for :food
                      :args {:item "bread" :count (storage/plus-carried inv "bread" loaves) :radius (:radius a) :fetch false}}]))
      :else
      (let [steps (when (= kind (:kind (:try mem))) (:steps mem))
            tier (first (remove (set (get-in mem [:ruled kind])) (tiers-of a)))]
        (cond
          (seq steps)
          (let [{:keys [item get count]} (peek steps)]
            [mem (if get
                   {:slot :get :job 'jobs.storage.withdraw :for :get
                    :args (merge (select-keys a [:ignore-zones?]) {:chest chest :items {get count}})}
                   {:slot :craft :job 'jobs.items.craft :for :tool
                    :args {:item item :count (storage/plus-carried inv item count) :radius (:radius a) :fetch false}})])
          (nil? tier) (recur (mark-missing mem kind (or (get (:why mem) kind) "no tier")) a still stacks inv chest)
          :else (recur (assoc mem :try {:kind kind :tier tier}
                              :steps [{:item (str tier "_" kind) :count 1}])
                       a still stacks inv chest))))))

(defn classify-short
  "How to get one missing name: [:get held-name], [:craft name] or [:none text]."
  [name alts stacks depth]
  (let [held (first (filter #(pos? (chest-count stacks %)) (cons name alts)))]
    (cond
      held [:get held]
      (and (or (= "stick" name) (str/ends-with? name "_planks")) (< depth 2)) [:craft name]
      :else [:none (str/join " or " (cons name alts))])))

(defn absorb-short
  "Mem after a tool craft step came back short of materials."
  [mem res stacks inv]
  (let [steps (:steps mem)
        top (peek steps)
        made (:made res 0)
        steps (if (< made (:count top)) (conj (pop steps) (update top :count - made)) (pop steps))
        depth (dec (count steps))
        alts (:alternatives res)
        hows (map (fn [[name n]] [name n (classify-short name (get alts name) stacks depth)]) (:short res))
        none (first (filter #(= :none (first (nth % 2))) hows))
        [name n [how arg]] (first hows)]
    (cond
      none (rule-out mem (second (nth none 2)))
      (= :get how) (assoc mem :steps (conj steps {:get arg :count (+ n (storage/carried inv arg))}))
      :else (assoc mem :steps (conj steps {:item arg :count n})))))

(defn absorb
  "{:mem m :end reason :fail reason} after the child call finished with res. Pure."
  [mem call res stacks inv]
  (let [reason (when-not (:short res) (some-> (:reason res) name))
        top (peek (:steps mem))]
    (case (:for call)
      (:get :food-get)
      (if (:gave-up res)
        {:mem mem :fail reason}
        {:mem (cond-> (assoc mem :chest-items nil)
                (= :get (:for call)) (assoc :steps (pop (:steps mem))))})
      (cond
        (contains? table-reasons reason) {:mem mem :end reason}
        reason {:mem mem :fail reason}
        (and (:short res) (= :food (:for call))) {:mem (mark-missing mem :food "wheat")}
        (:short res) {:mem (assoc (absorb-short mem res stacks inv) :chest-items nil)}
        (not (pos? (:made res 0))) {:mem mem :fail "nothing-made"}
        (= :food (:for call)) {:mem mem}
        :else (let [steps (pop (:steps mem))]
                {:mem (cond-> (assoc mem :steps steps) (empty? steps) (assoc :try nil))})))))

(defn finish-craft!
  "End the craft phase with the result: what is still short and why."
  [c still end]
  (let [m (ctx/mem c)
        kinds (map first still)
        missing (cond-> (:missing m) end (merge (zipmap (map mkey kinds) (repeat end))))
        missing (into {} (keep (fn [k] (when-let [t (get missing (mkey k))] [k t]))) kinds)
        short (into {} still)]
    (finish! c short (when (seq missing) {:missing missing}))))

(defn ^:async reach-chest!
  "Walk to within 3 of the chest (a go-to child) unless already there: nil when there, :continue while go-to waits,
  else the give-up for an unreachable chest."
  [c chest still]
  (if (u/within? (u/self-pos c) chest 3)
    nil
    (let [r (await (child/call-walk* c :walk {:pos chest :range 3 :ways #{:walk :clear-seal} :warn false :retry false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
      (cond
        (= :continue r) :continue
        (:arrived (:data r)) nil
        :else (give-up! c "unreachable" (into {} still))))))

(defn ^:async load-chest!
  "The inspected chest stacks, from memory or by walking near and inspecting
  (stored in memory); a keyword (:continue, or the give-up's) when that failed."
  [c chest still]
  (if-let [stacks (:chest-items (ctx/mem c))]
    stacks
    (or (await (reach-chest! c chest still))
        (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
          (if (not= "ok" (.-status seen))
            (give-up! c (.-status seen) (into {} still))
            (let [stacks (stacks-of (.-items seen))]
              (stock/note-stock! c chest stacks)
              (ctx/update-mem! c assoc :chest-items stacks)
              stacks))))))

(defn ^:async craft-round
  "One bounded craft-phase round: one child call, then fold its result into memory."
  [c]
  (let [a (:args c)
        chest (chest-of! c)
        still (needs (u/inventory (:primitives c)) a)]
    (if (empty? still)
      (finish-craft! c still nil)
      (let [stacks (await (load-chest! c chest still))]
        (if (keyword? stacks)
          stacks
          (let [inv (u/inventory (:primitives c))
                need (needs inv a stacks)
                [mem call] (plan-call (ctx/mem c) a need stacks inv chest)]
            (ctx/update-mem! c merge (select-keys mem craft-keys))
            (if (nil? call)
              (finish-craft! c need nil)
              (let [t (when (and (= :craft (:slot call)) (:fetch a) (not (:table-fetched (ctx/mem c))))
                        (await (getting/table! c (some? (crafting-table/nearest (:primitives c) (:radius a))))))
                    _ (when (= :done (:kind t)) (ctx/update-mem! c assoc :table-fetched true))
                    e (when-not t (await (ctx/call c (:slot call) (:job call) (:args call))))]
                (cond
                  (= :continue (or t e)) :continue
                  (and t (not= :done (:kind t))) (finish-craft! c need "no-table")
                  t :again
                  :else
                  (let [{:keys [mem end fail]} (absorb mem call (result-of e) stacks inv)]
                    (ctx/update-mem! c merge (select-keys mem craft-keys))
                    (cond
                      (= "refused" fail) (refused! c e (into {} need))
                      fail (give-up! c fail (into {} need))
                      end (finish-craft! c need end)
                      :else (do (u/progress! c) :again))))))))))))

(defn ^:async step!
  "One piece; see doc. Early returns: nothing needed, walk, inspect, nothing takeable, then one withdraw call.
  :again, :continue (a child waits) or :done."
  [c]
  (let [a (:args c)
        chest (chest-of! c)
        still (needs (u/inventory (:primitives c)) a)]
    (cond
      (= :craft (:phase (ctx/mem c))) (await (craft-round c))
      (empty? still) (finish! c {})
      :else
      (or (await (reach-chest! c chest still))
          (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
            (if (not= "ok" (.-status seen))
              (give-up! c (.-status seen) (into {} still))
              (let [_ (stock/note-stock! c chest (.-items seen))
                    inv (u/inventory (:primitives c))
                    stacks (stacks-of (.-items seen))
                    {:keys [take short]} (plan (needs inv a stacks) inv stacks)]
                (cond
                  (and (empty? take) (:craft a))
                  (do (ctx/update-mem! c assoc :phase :craft :chest-items (stacks-of (.-items seen)))
                      :again)
                  (empty? take) (finish! c short)
                  :else
                  (let [e (await (ctx/call c :take 'jobs.storage.withdraw
                                           (merge (select-keys a [:ignore-zones?]) {:chest chest :items take})))]
                    (cond
                      (= :continue e) :continue
                      (true? (get-in e [:data :gave-up])) (child-stop! c e (merge (into {} (needs inv a stacks)) short))
                      :else (do (u/progress! c) :again)))))))))))

(defn ^:async round
  "The whole kit in one call: step! again until it is done or stopped."
  [c]
  (await (pace/steps! c (fn ^:async kit-step [] (await (step! c))))))
