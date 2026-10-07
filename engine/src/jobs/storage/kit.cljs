(ns jobs.storage.kit
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.food :as food]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.items.craft]
            [jobs.storage.deposit :as deposit]
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
  Ends with {:gave-up false :short {kind n}}. :short is empty when the kit is complete, else what the chest could
  not supply, keyed by the kind string or :food. After failed attempts it ends {:gave-up true :reason r :short
  {...}}, with r \"unreachable\", the inspect status or the withdraw's reason. A chest that refuses (zone or
  claim) ends at once with :reason :refused.
  With :craft (the default) a short does not end the job. It enters a craft phase. Each craft step makes
  exactly one child call:
  - For a short tool kind K, the tiers of :craft-tiers are tried in order for tier_K (jobs.items.craft).
  - When the craft comes back short of a material, the chest is asked for it (jobs.storage.withdraw). Failing
    that, a stick or planks is crafted as a sub-step (at most two deep). Failing that, the tier is ruled out and
    the next is tried.
  - Food is made as bread from wheat (chest or body), in batches of 3.
  The result gets :missing {kind text} for what could not be made (only when non-empty). \"no-table\" and
  \"full\" end the phase for every short kind.
  A craft phase that ends with a non-empty :missing is remembered as :no-craft in body memory for 10 minutes.
  While that lives, a take phase that would enter the craft phase ends at once with the short and the remembered
  :missing.
  Every inspect books what the chest holds in body memory :fetch/stock (jobs.lib.fetch).")

(def args
  {:tools {:doc "tool kinds to carry, e.g. [\"hoe\" \"pickaxe\"]" :default ["hoe"]}
   :spare {:doc "extra of each tool kind beyond the one in use" :default 1}
   :food {:doc "food items to carry; enough to hold the body's food reserve (jobs.lib.cost/food-reserve) when nil" :default nil}
   :chest {:doc "chest position; the known :chest place when nil" :type :pos :default nil}
   :craft {:doc "craft what the chest cannot supply" :default true}
   :craft-tiers {:doc "tool tiers to craft, in order; iron or diamond only when listed" :default ["stone" "wooden"]}
   :radius {:doc "how far to look for a crafting table" :default 32}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

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
    (get combat/material-rank (first (str/split name #"_")) 0)))

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
             targets (into {} (map (fn [[name k]] [name (+ k (deposit/carried inventory name))])) taken)]
         (-> acc
             (update :take merge targets)
             (update :short #(if (pos? left) (assoc % kind left) %)))))
     {:take {} :short {}}
     needs)))

(defn check
  "A chest is known. A complete kit is not a false check: the job must stay
  admitted to report {:short {}} (a parent reads it), and its first round
  then ends at once without walking or inspecting."
  [c]
  (boolean (deposit/chest-of (ctx/view c) (:args c))))

(defn give-up!
  "u/fail!, and when it gives up hand the parent the reason and what is short."
  [c reason short]
  (let [r (u/fail! c :kit.gave-up (str "kit gave up: " reason))]
    (if (= :done r)
      (do (ctx/result! c {:gave-up true :reason reason :short short})
          :done)
      :again)))

(defn refused!
  "End at once with the withdraw child's refusal res ({:reason :refused :zones :claims}) and what is short: another's
  chest does not get asked three times."
  [c res short]
  (ctx/result! c (merge {:gave-up true :reason :refused :short short} (select-keys res [:zones :claims])))
  :done)

(defn finish!
  [c short]
  (ctx/result! c {:gave-up false :short short})
  :done)

(defn stacks-of
  "The inspected container items as cljs maps {:name :count}."
  [items]
  (mapv (fn [i] {:name (.-name i) :count (.-count i)}) (array-seq items)))

(def craft-keys [:phase :try :ruled :why :missing :steps :chest-items])

(def table-reasons #{"no-table" "not-a-table" "unreachable" "full"})

(def no-craft-ms (* 10 60 1000))

(def no-craft-policy {:cap 1 :ttl no-craft-ms})

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
  (deposit/carried stacks name))

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
        wheat-body (deposit/carried inv "wheat")]
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
                      :args {:item "bread" :count loaves :radius (:radius a)}}]))
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
                    :args {:item item :count count :radius (:radius a)}})])
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
      (= :get how) (assoc mem :steps (conj steps {:get arg :count (+ n (deposit/carried inv arg))}))
      :else (assoc mem :steps (conj steps {:item arg :count n})))))

(defn absorb
  "{:mem m :end reason :fail reason} after the child call finished with res. Pure."
  [mem call res stacks inv]
  (let [reason (:reason res)
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
    (when (seq short)
      (ctx/emit! c :kit.short :info {:short short :text (str "kit still short " (pr-str short) (when (seq missing) (str ", missing " (pr-str missing))))}))
    (when (seq missing) (ctx/remember! c :no-craft {:missing missing} no-craft-policy))
    (ctx/result! c (cond-> {:gave-up false :short short} (seq missing) (assoc :missing missing)))
    :done))

(defn remembered-missing
  "The :missing of the live :no-craft entry restricted to the kinds in short, or nil."
  [c short]
  (when (pos? (ctx/count-in c :no-craft no-craft-ms))
    (let [missing (:missing (:data (ctx/latest c :no-craft)))]
      (not-empty (into {} (filter (fn [[k _]] (contains? short k))) missing)))))

(defn finish-uncrafted!
  "Finish without a craft phase: the short, plus what the remembered failure says."
  [c short]
  (ctx/emit! c :kit.short :info {:short short :text (str "chest lacks " (pr-str short))})
  (let [missing (remembered-missing c short)]
    (ctx/result! c (cond-> {:gave-up false :short short} missing (assoc :missing missing)))
    :done))

(defn ^:async reach-chest!
  "Walk to within 3 of the chest (a go-to child) unless already there: nil when there, :continue while go-to waits,
  else the give-up for an unreachable chest."
  [c chest still]
  (if (u/within? (u/self-pos c) chest 3)
    nil
    (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos chest :range 3 :escalate false :warn false :retry false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
      (cond
        (= :continue r) :continue
        (:arrived (ctx/child-result c :walk)) nil
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
              (fetch/note-stock! c chest stacks)
              (ctx/update-mem! c assoc :chest-items stacks)
              stacks))))))

(defn ^:async craft-round
  "One bounded craft-phase round: one child call, then fold its result into memory."
  [c]
  (let [a (:args c)
        chest (deposit/chest-of (ctx/view c) a)
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
              (let [r (await (ctx/call-child c (:slot call) (:job call) (:args call)))
                    res (when (= :done r) (ctx/child-result c (:slot call)))]
                (if-not (= :done r)
                  (if (= :continue r) :continue (give-up! c "craft child declined" (into {} need)))
                  (let [{:keys [mem end fail]} (absorb mem call res stacks inv)]
                    (ctx/update-mem! c merge (select-keys mem craft-keys))
                    (cond
                      (= :refused fail) (refused! c res (into {} need))
                      fail (give-up! c fail (into {} need))
                      end (finish-craft! c need end)
                      :else (do (u/progress! c) :again))))))))))))

(defn ^:async step!
  "One piece; see doc. Early returns: nothing needed, walk, inspect, nothing takeable, then one withdraw call.
  :again, :continue (a child waits) or :done."
  [c]
  (let [a (:args c)
        chest (deposit/chest-of (ctx/view c) a)
        still (needs (u/inventory (:primitives c)) a)]
    (cond
      (= :craft (:phase (ctx/mem c))) (await (craft-round c))
      (empty? still) (finish! c {})
      :else
      (or (await (reach-chest! c chest still))
          (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
            (if (not= "ok" (.-status seen))
              (give-up! c (.-status seen) (into {} still))
              (let [_ (fetch/note-stock! c chest (.-items seen))
                    inv (u/inventory (:primitives c))
                    stacks (stacks-of (.-items seen))
                    {:keys [take short]} (plan (needs inv a stacks) inv stacks)]
                (cond
                  (and (empty? take) (:craft a) (pos? (ctx/count-in c :no-craft no-craft-ms)))
                  (finish-uncrafted! c short)
                  (and (empty? take) (:craft a))
                  (do (ctx/update-mem! c assoc :phase :craft :chest-items (stacks-of (.-items seen)))
                      :again)
                  (empty? take) (do (ctx/emit! c :kit.short :info {:short short :text (str "chest lacks " (pr-str short))})
                                    (finish! c short))
                  :else
                  (let [r (await (ctx/call-child c :take 'jobs.storage.withdraw
                                                 (merge (select-keys a [:ignore-zones?]) {:chest chest :items take})))
                        res (when (= :done r) (ctx/child-result c :take))]
                    (cond
                      (= :continue r) :continue
                      (= :declined r) (give-up! c "withdraw declined" (merge (into {} (needs inv a stacks)) short))
                      (= :refused (:reason res)) (refused! c res (merge (into {} (needs inv a stacks)) short))
                      (:gave-up res) (do (ctx/result! c {:gave-up true :reason (:reason res) :short (merge (into {} (needs inv a stacks)) short)})
                                         :done)
                      :else (do (u/progress! c) :again)))))))))))

(defn ^:async round
  "The whole kit in one call: step! again until it is done or stopped."
  [c]
  (await (pace/steps! c (fn ^:async kit-step [] (await (step! c))))))
