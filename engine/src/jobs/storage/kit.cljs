(ns jobs.storage.kit
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]
            [jobs.storage.deposit :as deposit]
            [jobs.survival.eat :as eat]))

(def doc
  "Take a kit out of the chest: :spare + 1 of each tool kind in :tools (any tier;
  a name is of a kind when it equals it or ends in _kind) and :food food items
  (any name in the eat table). Each round the needs are re-derived from the
  inventory at its start and the plan from the inspected chest (best tier and best food
  first), and the plan is handed to jobs.storage.withdraw as carry-at-least
  targets. Nothing is stored between rounds. Ends with a result
  {:gave-up false :short {kind n}} (short is empty when the kit is complete,
  else what the chest could not supply, keyed by the kind string or :food), or
  {:gave-up true :reason r :short {...}} (\"unreachable\", the inspect status, or
  the withdraw's reason) when the failed attempts used it up and the warn was
  emitted.")

(def args
  {:tools {:doc "tool kinds to carry, e.g. [\"hoe\" \"pickaxe\"]" :default ["hoe"]}
   :spare {:doc "extra of each tool kind beyond the one in use" :default 1}
   :food {:doc "food items to carry" :default 12}
   :chest {:doc "chest position; the known :chest place when nil" :default nil}})

(defn kind-of?
  "Does item name belong to tool kind: equal to it or ending in _kind."
  [kind name]
  (or (= kind name) (str/ends-with? name (str "_" kind))))

(defn matcher
  "Predicate on item names for a kind: a tool kind string, or :food."
  [kind]
  (if (= :food kind)
    #(contains? eat/food-points %)
    #(kind-of? kind %)))

(defn count-of
  "Total of the items (maps with :name :count) whose name matches kind."
  [items kind]
  (let [match? (matcher kind)]
    (transduce (comp (filter #(match? (:name %))) (map :count)) + 0 items)))

(defn needs
  "[[kind n] ...] still needed: the tools kinds in order, then :food; n above 0 only."
  [inventory {:keys [tools spare food] :or {tools ["hoe"] spare 1 food 12}}]
  (->> (concat (map (fn [k] [k (inc spare)]) tools) [[:food food]])
       (map (fn [[kind want]] [kind (- want (count-of inventory kind))]))
       (filter (fn [[_ n]] (pos? n)))
       vec))

(defn rank
  "Sort key, higher is better: material tier of a tool name, food points of a food."
  [kind name]
  (if (= :food kind)
    (get eat/food-points name 0)
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
  "A chest is known."
  [c]
  (boolean (deposit/chest-of (ctx/view c) (:args c))))

(defn give-up!
  "u/fail!, and when it gives up hand the parent the reason and what is short."
  [c reason short]
  (let [r (u/fail! c :kit.gave-up (str "kit gave up: " reason))]
    (when (= :done r) (ctx/result! c {:gave-up true :reason reason :short short}))
    r))

(defn finish!
  [c short]
  (ctx/result! c {:gave-up false :short short})
  :done)

(defn stacks-of
  "The inspected container items as cljs maps {:name :count}."
  [items]
  (mapv (fn [i] {:name (.-name i) :count (.-count i)}) (array-seq items)))

(defn ^:async round
  "One bounded step; see doc. Early returns: nothing needed, walk, inspect,
  nothing takeable, then one withdraw round."
  [c]
  (let [a (:args c)
        chest (deposit/chest-of (ctx/view c) a)
        still (needs (u/inventory (:primitives c)) a)]
    (if (empty? still)
      (finish! c {})
      (let [w (await (u/walk-near! c chest 3))]
        (case w
          :partial :continue
          :blocked (give-up! c "unreachable" (into {} still))
          (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
            (if (not= "ok" (.-status seen))
              (give-up! c (.-status seen) (into {} still))
              (let [inv (u/inventory (:primitives c))
                    {:keys [take short]} (plan still inv (stacks-of (.-items seen)))]
                (cond
                  (empty? take) (do (ctx/emit! c :kit.short :info {:short short :text (str "chest lacks " (pr-str short))})
                                    (finish! c short))
                  :else
                  (let [r (await (ctx/call-child c :take 'jobs.storage.withdraw {:chest chest :items take}))
                        res (when (= :done r) (ctx/child-result c :take))]
                    (if (:gave-up res)
                      (do (ctx/result! c {:gave-up true :reason (:reason res) :short (merge (into {} still) short)})
                          :done)
                      :continue)))))))))))
