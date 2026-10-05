(ns jobs.storage.withdraw
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.fetch :as fetch]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [engine.places :as places]
            [jobs.storage.deposit :as deposit]))

(def doc
  "Walk to the chest and take items out, one name per round, until at least the wanted count of each name in
  :items is carried. What is short is re-derived from the inventory every round, so a cut or restart loses
  nothing.
  Ends with {:gave-up false :short {name n}}. :short is empty when everything is carried, else what the chest could
  not supply. After three failed attempts it ends {:gave-up true :reason r :short {...}} and warns. r is the
  inspect or transfer status, \"unreachable\" (blocked walk) or \"nothing-moved\".
  Memory: a container missing at the recorded :chest retracts that place, with one chest_missing warn.
  Zones: a chest in another owner's zone or claim that does not allow :take is refused before the walk and again
  before the transfer. The job ends {:gave-up true :reason :refused :zones [..] :claims [..]} after one
  withdraw.refused warn and takes nothing. :ignore-zones? true skips the check.
  Stock: what the chest holds is booked in body memory :fetch/stock when inspected and after each take
  (engine.jobs.fetch), for jobs.items.obtain.")

(def args
  {:chest {:doc "chest position [x y z] or {:x :y :z}; the known :chest place when nil" :default nil}
   :items {:doc "{item-name count}: carry at least this many of each name" :default {}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

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

(defn refuse!
  "End refused: one withdraw.refused warn naming the zones and claims, the refusal as the result."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])]
    (ctx/emit! c :withdraw.refused :warn (assoc fields :text (str "withdraw refused: " (access/refusal-text fields))))
    (ctx/result! c (access/refused-result verdict))
    :done))

(defn ^:async round
  "One bounded step: walk in reach, then move one name's stack. Done when
  nothing is short or the chest holds none of what is short. The shortfall is
  read once at the start and decides the early exit, the give-ups, the pick and
  the :short result."
  [c]
  (let [{:keys [items]} (:args c)
        chest (deposit/chest-of (ctx/view c) (:args c))
        short (shortfall (u/inventory (:primitives c)) items)]
    (cond
      (empty? short) (do (ctx/result! c {:gave-up false :short {}}) :done)
      (access/container-refusal c :take chest) (refuse! c (access/container-refusal c :take chest))
      :else
      (let [w (await (near/walk-near! c chest 3))]
        (case w
          :partial :continue
          :blocked (give-up! c "unreachable" short)
          (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
            (if (not= "ok" (.-status seen))
              (do (places/retract-if-missing! c :chest chest (.-status seen))
                  (give-up! c (.-status seen) short))
              (let [_ (fetch/note-stock! c chest (.-items seen))
                    pick (some (fn [[name n]] (let [held (held-in (.-items seen) name)]
                                                (when (pos? held) [name (min n held)])))
                               short)]
                (cond
                  (nil? pick) (do (ctx/emit! c :withdraw.short :info {:short (into {} short)
                                                                     :text (str "chest lacks " (pr-str (into {} short)))})
                                  (ctx/result! c {:gave-up false :short (into {} short)})
                                  :done)
                  (access/container-refusal c :take chest) (refuse! c (access/container-refusal c :take chest))
                  :else
                  (let [[name n] pick
                        r (await (ctx/act c :transfer (clj->js {:pos chest :direction "withdraw" :item name :count n})))]
                    (cond
                      (not= "ok" (.-status r)) (do (places/retract-if-missing! c :chest chest (.-status r))
                                                   (give-up! c (.-status r) short))
                      (zero? (.-moved r)) (give-up! c "nothing-moved" short)
                      :else (do (fetch/note-moved! c chest name (- (.-moved r))) :continue))))))))))))
