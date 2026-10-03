(ns jobs.storage.withdraw
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [jobs.storage.deposit :as deposit]))

(def doc
  "Walk to the chest and take items out, one name per round, until at least the
  wanted count of each name is carried. Target-based: what is short is
  re-derived from the inventory every round, so a cut or restart loses nothing.
  The shortfall that decides a transfer is read after the chest is inspected,
  since the inventory view can lag a withdraw until the chest is opened again.
  Ends with a result {:gave-up false :short {name n}} (short is empty when
  everything is carried, else what the chest could not supply), or
  {:gave-up true :reason r :short {...}} (the inspect or transfer status,
  \"unreachable\" for a blocked walk, \"nothing-moved\") when the failed attempts
  used it up and the warn was emitted.")

(def args
  {:chest {:doc "chest position; the known :chest place when nil" :default nil}
   :items {:doc "{item-name count}: carry at least this many of each name" :default {}}})

(defn shortfall
  "[[name n] ...] in the order of items: n is the target minus the total
  carried over all stacks, for the names with n above 0."
  [inventory items]
  (->> items
       (map (fn [[name target]] [name (- target (deposit/carried inventory name))]))
       (filter (fn [[_ n]] (pos? n)))
       vec))

(defn check
  "A chest is known."
  [c]
  (boolean (deposit/chest-of (ctx/view c) (:args c))))

(defn give-up!
  "u/fail!, and when it gives up hand the parent the reason and the shortfall."
  [c reason short]
  (let [r (u/fail! c :withdraw.gave-up (str "withdraw gave up: " reason))]
    (when (= :done r) (ctx/result! c {:gave-up true :reason reason :short (into {} short)}))
    r))

(defn held-in
  "How many of name the inspected container items hold."
  [stacks name]
  (transduce (comp (filter #(= name (.-name %))) (map #(.-count %))) + 0 (array-seq stacks)))

(defn ^:async round
  "One bounded step: walk in reach, then move one name's stack. Done when
  nothing is short or the chest holds none of what is short. The early exit and
  the give-ups before the inspect use the shortfall read at the start; the
  shortfall that picks the transfer and the :short result is re-read once the
  chest is open, because the client's inventory view can lag a withdraw until
  the chest is opened again."
  [c]
  (let [{:keys [items]} (:args c)
        chest (deposit/chest-of (ctx/view c) (:args c))
        early (shortfall (u/inventory (:primitives c)) items)]
    (if (empty? early)
      (do (ctx/result! c {:gave-up false :short {}}) :done)
      (let [w (await (u/walk-near! c chest 3))]
        (case w
          :partial :continue
          :blocked (give-up! c "unreachable" early)
          (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
            (if (not= "ok" (.-status seen))
              (give-up! c (.-status seen) early)
              (let [short (shortfall (u/inventory (:primitives c)) items)
                    pick (some (fn [[name n]] (let [held (held-in (.-items seen) name)]
                                                (when (pos? held) [name (min n held)])))
                               short)]
                (cond
                  (empty? short) (do (ctx/result! c {:gave-up false :short {}}) :done)
                  (nil? pick) (do (ctx/emit! c :withdraw.short :info {:short (into {} short)
                                                                     :text (str "chest lacks " (pr-str (into {} short)))})
                                  (ctx/result! c {:gave-up false :short (into {} short)})
                                  :done)
                  :else
                  (let [[name n] pick
                        r (await (ctx/act c :transfer (clj->js {:pos chest :direction "withdraw" :item name :count n})))]
                    (cond
                      (not= "ok" (.-status r)) (give-up! c (.-status r) short)
                      (zero? (.-moved r)) (give-up! c "nothing-moved" short)
                      :else :continue)))))))))))
