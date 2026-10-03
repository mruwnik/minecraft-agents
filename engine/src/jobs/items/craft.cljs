(ns jobs.items.craft
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Craft count more of an item: 2x2 recipes anywhere, bigger ones at a crafting
  table, walking to the given or the nearest one within the radius when the
  body is not in reach of one. Target-based: the count carried when the job
  started is kept in memory and the target is that plus count, so a cut or a
  partial batch loses nothing. Ends with a result {:made n} (n is how many of
  the item were gained), plus :short {name n} when an ingredient ran out, or
  :reason (\"no-table\", \"unreachable\", \"full\", the cannot reason, or the
  failed status) when it stopped short, after emitting a warn.")

(def args
  {:item {:doc "item name to craft" :default nil}
   :count {:doc "how many more to end up with" :default 1}
   :table {:doc "crafting table position; the nearest within :radius when nil and the recipe needs one" :default nil}
   :radius {:doc "how far to look for a crafting table" :default 32}})

(defn check
  "An item name is given."
  [c]
  (string? (:item (:args c))))

(defn carried
  "How many of name the inventory holds over all stacks."
  [p name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 (u/inventory p)))

(defn nearest-table
  "The position of the nearest crafting table within radius, or nil."
  [p radius]
  (some-> (.blocks p #js {:radius radius :names #js ["crafting_table"] :max 1})
          array-seq
          first
          .-pos
          u/pos-of))

(defn finish!
  "Hand the parent a result, made so far plus extra, and return :done."
  [c made extra]
  (ctx/result! c (merge {:made made} extra))
  :done)

(defn give-up!
  "u/fail!, and when it gives up hand the parent the made count and the reason."
  [c made reason]
  (let [r (u/fail! c :craft.gave-up (str "craft gave up: " reason))]
    (when (= :done r) (finish! c made {:reason reason}))
    r))

(defn no-table!
  "Warn that no crafting table is within the radius and finish."
  [c made]
  (ctx/emit! c :craft.no-table :warn {:text (str "no crafting table within " (:radius (:args c)))})
  (finish! c made {:reason "no-table"}))

(defn ^:async reach-table!
  "The craft was unreachable: walk to the remembered or the nearest table.
  :continue to craft again, :done when there is none or the walk was given up."
  [c made]
  (let [{:keys [table radius]} (merge (:args c) (ctx/mem c))
        p (:primitives c)
        table (or table (nearest-table p radius))]
    (if (nil? table)
      (no-table! c made)
      (do (ctx/update-mem! c assoc :table table)
          (if (u/within? (u/self-pos c) table 3)
            (give-up! c made "unreachable")
            (case (await (u/walk-near! c table 3))
              :blocked (give-up! c made "unreachable")
              :continue))))))

(defn short!
  "A craft ran out of an ingredient: emit the info and finish with what is
  missing, and the alternatives (name [cousins]) when the primitive offers some."
  [c item r made]
  (let [short (js->clj (.-short r))
        alternatives (some-> (.-alternatives r) js->clj)]
    (ctx/emit! c :craft.short :info {:text (str "craft " item " is missing " (pr-str short))})
    (finish! c made (cond-> {:short short} alternatives (assoc :alternatives alternatives)))))

(defn ^:async round
  "One bounded step: stop when the target is carried, else craft the rest and
  act on the status."
  [c]
  (let [p (:primitives c)
        {:keys [item count]} (:args c)
        _ (when-not (contains? (ctx/mem c) :start)
            (ctx/update-mem! c assoc :start (carried p item)))
        start (:start (ctx/mem c))
        target (+ start count)
        have (carried p item)]
    (if (>= have target)
      (finish! c (- have start) {})
      (let [table (or (:table (ctx/mem c)) (:table (:args c)))
            r (await (ctx/act c :craft (clj->js {:item item :count (- target have) :table table})))
            status (.-status r)
            made (- (carried p item) start)]
        (when table (ctx/update-mem! c assoc :table table))
        (case status
          "crafted" (finish! c made {})
          "no-item" (short! c item r made)
          "partial" (if (= "no-item" (.-reason r))
                      (short! c item r made)
                      :continue)
          "out-of-reach" (let [handed (u/pos-of (.-table r))]
                           (cond
                             (:table (:args c)) (await (reach-table! c made))
                             (> (u/dist (u/self-pos c) handed) (:radius (:args c))) (no-table! c made)
                             :else (do (ctx/update-mem! c assoc :table handed)
                                       (await (reach-table! c made)))))
          "unreachable" (cond
                          (= "no-table" (.-reason r)) (do (ctx/update-mem! c dissoc :table)
                                                          (await (reach-table! c made)))
                          (not= "not-a-table" (.-reason r)) (await (reach-table! c made))
                          (:table (:args c)) (do (ctx/emit! c :craft.no-table :warn {:text (str "not a crafting table: " (pr-str table))})
                                                 (finish! c made {:reason "not-a-table"}))
                          :else (do (ctx/update-mem! c dissoc :table) :continue))
          "full" (do (ctx/emit! c :craft.full :warn {:text "inventory is full"})
                     (finish! c made {:reason "full"}))
          "cannot" (do (ctx/emit! c :craft.cannot :warn {:text (str "cannot craft " item ": " (.-reason r))})
                       (finish! c 0 {:reason (.-reason r)}))
          (give-up! c made status))))))
