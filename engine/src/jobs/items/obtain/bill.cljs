(ns jobs.items.obtain.bill
  "A bill {item n}: one obtain child per item, the furnace material several items need together first. When that material fails but some of it (or its raw input) is carried, the items are made in order from what is carried and the bill stops at the first it cannot make."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.items.obtain.base :refer [bill-item-s bill-of carried-of]]
            [jobs.lib.armour :as armour]
            [jobs.items.recipes :as recipes]
            [jobs.items.smelt.plan :as smelt-plan]
            [jobs.lib.need :as need]
            [jobs.lib.next-call :as next-call]
            [jobs.lib.pace :as pace]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(defn carried [c] (storage/usable-totals (u/inventory (:primitives c))))

(defn todo
  "The bill's entries [item n] not yet carried or worn, in order."
  [c]
  (let [have (carried c)
        worn (set (vals (armour/worn-of (.-equipment (.self (:primitives c))))))]
    (filterv (fn [[item n]] (and (< (get have item 0) n) (not (worn item)))) (bill-of (:args c)))))

(defn furnace-short
  "{material n}: the furnace products (iron_ingot ..) that the first recipes of items need together, beyond carried
  ({name count})."
  [version items carried]
  (let [rs (recipes/recipes-for version)
        need (apply merge-with + {} (for [item items
                                          [k v] (:needs (first (get rs item)))
                                          :when (contains? smelt-plan/made-from k)]
                                      {k v}))]
    (into {} (keep (fn [[k v]] (let [n (- v (get carried k 0))] (when (pos? n) [k n])))) need)))

(defn kit-stock
  "[[item n why not]] of the fuel and sticks the items need: coal or charcoal for the smelt (one per 8 ingots), the sticks of the recipes and a carried table to craft a spare pickaxe with."
  [version items]
  (let [rs (recipes/recipes-for version)
        sticks (reduce + (for [item items] (get (:needs (first (get rs item))) "stick" 0)))
        ingots (reduce + (vals (furnace-short version items {})))
        fuel (long (js/Math.ceil (/ ingots 8)))]
    (concat (when (pos? sticks) [["stick" sticks #{:craft}] ["crafting_table" 1 #{:craft}]])
            (when (pos? fuel) (for [f ["coal" "charcoal"]] [f fuel #{:craft :smelt}])))))

(defn reserve!
  "Replace this job's reserve entries (strength 0.9) with the furnace material the missing items need together and the
  raw input it is smelted from (a deposit leaves those carried units alone; the craft and smelt children spend them), and the
  fuel and sticks they need (torch-stock and spare-tool crafts leave those alone)."
  [c items]
  (let [version (game/version-of (:primitives c))]
    (reserve/set-job-entries!
     c (vec (concat (for [[mat n] (furnace-short version items {})
                          item [mat (get smelt-plan/made-from mat)]]
                      {:id (str (:id c) "-bill-" item) :item item :count n :strength 0.9 :by (:id c)
                       :why "material for the bill" :not #{:craft :smelt}})
                    (for [[item n not] (kit-stock version items)]
                      {:id (str (:id c) "-bill-" item) :item item :count n :strength 0.9 :by (:id c)
                       :why "fuel and sticks for the bill" :not not}))))))

(defn child-args
  "The obtain child's args for item, carrying total in all."
  [c item total]
  (let [a (:args c)]
    (into {:item item :count total :spend {:job-time (bill-item-s)}}
          (remove (comp nil? val))
          (select-keys a [:ways :explore :for-plan :avoid]))))

(defn next-call
  "{:slot :item :n} of the child to run: when the first missing item needs furnace material, the material all missing
  items need together (if short), else the first missing item."
  [c]
  (let [items (mapv first (todo c))
        version (game/version-of (:primitives c))
        short (when (and (not (:partial (ctx/mem c))) (seq (furnace-short version [(first items)] {})))
                (furnace-short version items (carried c)))]
    (if-let [[mat n] (first short)]
      {:slot :materials :item mat :n n}
      {:slot (keyword (first items)) :item (first items) :n (second (first (todo c)))})))

(defn material-carried?
  "Whether any of the furnace material mat, or the raw input it is smelted from, is carried."
  [c mat]
  (let [have (carried c)]
    (pos? (+ (get have mat 0) (get have (get smelt-plan/made-from mat) 0)))))

(defn stop-data
  "The data of the bill's stop at item for child ending e: its :why, :tried and :next."
  [item e]
  (cond-> {:item item :got 0}
    (get-in e [:data :tried]) (assoc :tried (get-in e [:data :tried]))
    (next-call/of e) (assoc :next (next-call/of e))
    (or (get-in e [:data :why]) (when (map? (:cause e)) (:why (:cause e))))
    (assoc :why (or (get-in e [:data :why]) (:why (:cause e))))))

(defn ^:async step!
  "One obtain child round: :continue, :again once it ended with the item carried (progress booked), else the child's
  ending, which ends the bill with its kind and cause and the item it stopped at."
  [c]
  (let [{:keys [slot item n]} (next-call c)
        have (carried-of (u/inventory (:primitives c)) [item])
        e (await (need/call! c slot 'jobs.items.obtain
                           (child-args c item (need/carry-count c slot [item] (max 1 (- n (if (= :materials slot) 0 have)))))))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/emit! c :obtain.bill-progress :info
                                         {:item item :left (count (todo c))
                                          :text (str "bill: got " item ", " (count (todo c)) " item(s) to go")})
                              :again)
      (and (= :materials slot) (material-carried? c item))
      (do (ctx/update-mem! c assoc :partial {:reason (contract/cause-key e)}) :again)

      :else (do (reserve! c [])
                (ctx/end c (:kind e) (:cause e) (stop-data item e))))))

(defn ^:async run!
  "The whole bill in one call: one child after the other until everything is carried or one fails."
  [c]
  (await (pace/steps! c (fn ^:async step []
                          (let [items (mapv first (todo c))]
                            (reserve! c items)
                            (if (empty? items)
                              (ctx/end c :done nil {:have (vec (keys (:bill (:args c))))})
                              (await (step! c))))))))
