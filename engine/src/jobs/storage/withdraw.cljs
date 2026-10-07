(ns jobs.storage.withdraw
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.storage.deposit :as deposit]))

(def doc
  "Walk to the chest (one go-to) and take items out, every name in one call, until at least the wanted count of
  each name in :items is carried. What is short is re-derived from the inventory before each take, so a cut or
  restart loses nothing.
  Ends with {:gave-up false :short {name n}}; :short is empty when everything is carried, else what the chest could
  not supply (and the status is stopped). After three failed attempts, or at once when go-to cannot reach, it ends
  stopped {:gave-up true :reason r :short {...}} and warns. r is the inspect or transfer status, \"unreachable\" or
  \"nothing-moved\".
  Memory: a container missing at the recorded :chest retracts that place, with one chest_missing warn, and ends at once with reason \"missing\". No chest known: the job waits, reason :no-chest.
  Zones: a chest in another owner's zone or claim that does not allow :take is refused before the walk and again
  before the transfer. The job ends {:gave-up true :reason :refused :zones [..] :claims [..]} after one
  withdraw.refused warn and takes nothing. :ignore-zones? true skips the check.
  Stock: what the chest holds is booked in body memory :fetch/stock when inspected and after each take
  (jobs.lib.fetch), for jobs.items.obtain.")

(def args
  {:chest {:doc "chest position [x y z] or {:x :y :z}; the known :chest place when nil" :type :pos :default nil}
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
  "A chest is known; else waits with reason :no-chest."
  [c]
  (or (boolean (deposit/chest-of (ctx/view c) (:args c)))
      (ctx/wait c {:reason :no-chest})))

(defn stop!
  "End stopped: the reason and the shortfall as the result."
  [c reason short]
  (ctx/result! c {:gave-up true :reason reason :status :stopped :short (into {} short)})
  :done)

(defn give-up!
  "u/fail!: :again until the third failure in a row, then stop with the reason and the shortfall."
  [c reason short]
  (let [r (u/fail! c :withdraw.gave-up (str "withdraw gave up: " reason))]
    (if (= :done r) (stop! c reason short) :again)))

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

(defn ^:async take!
  "Inspect the chest in reach and take the first short name the chest holds: :again, or :done."
  [c chest short]
  (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
    (if (not= "ok" (.-status seen))
      (if (deposit/retracted? c chest (.-status seen))
        (stop! c (.-status seen) short)
        (give-up! c (.-status seen) short))
      (let [_ (fetch/note-stock! c chest (.-items seen))
            pick (some (fn [[name n]] (let [held (held-in (.-items seen) name)]
                                        (when (pos? held) [name (min n held)])))
                       short)]
        (cond
          (nil? pick) (do (ctx/emit! c :withdraw.short :info {:short (into {} short)
                                                             :text (str "chest lacks " (pr-str (into {} short)))})
                          (ctx/result! c {:gave-up false :status :stopped :short (into {} short)})
                          :done)
          (access/container-refusal c :take chest) (refuse! c (access/container-refusal c :take chest))
          :else
          (let [[name n] pick
                before (u/inventory (:primitives c))
                r (await (ctx/act c :transfer (clj->js {:pos chest :direction "withdraw" :item name :count n})))]
            (cond
              (not= "ok" (.-status r)) (if (deposit/retracted? c chest (.-status r))
                                           (stop! c (.-status r) short)
                                           (give-up! c (.-status r) short))
              (or (zero? (.-moved r))
                  (<= (deposit/carried (u/inventory (:primitives c)) name) (deposit/carried before name)))
              (give-up! c "nothing-moved" short)
              :else (do (fetch/note-moved! c chest name (- (.-moved r)))
                        (u/progress! c)
                        :again))))))))

(defn ^:async step!
  "Walk in reach, then take one name: :again, :continue (go-to waits), or :done when nothing is short, the chest
  holds none of what is short, or it stopped. The shortfall is read at the start of the step."
  [c]
  (let [{:keys [items]} (:args c)
        chest (deposit/chest-of (ctx/view c) (:args c))
        short (shortfall (u/inventory (:primitives c)) items)]
    (cond
      (empty? short) (do (ctx/result! c {:gave-up false :short {}}) :done)
      (access/container-refusal c :take chest) (refuse! c (access/container-refusal c :take chest))
      :else
      (let [w (await (deposit/walk! c chest))]
        (cond
          (= :unreachable w) (give-up! c "unreachable" short)
          w w
          :else (await (take! c chest short)))))))

(defn ^:async round
  "The whole take in one call, a name after another until nothing is short or it stops. :continue only while go-to
  waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async withdraw-step [] (await (step! c))))))
