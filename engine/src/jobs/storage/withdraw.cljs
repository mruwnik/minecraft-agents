(ns jobs.storage.withdraw
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.stock :as stock]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.storage.deposit :as deposit]
            [jobs.lib.least :as least]
            [jobs.lib.storage :as storage]))

(def doc
  "Walk to the chest (one go-to) and take items out, every name in one call, until at least the wanted count of
  each name in :items is carried. What is short is re-derived from the inventory before each take, so a cut or
  restart loses nothing.
  Needs: a chest, the :chest argument or the known :chest place; none known waits :no-chest (can-start? and can-run?: a chest place lost mid-job
  waits, a reflex ends condition-failed).
  Ends done when everything is carried (also at once when it already is), or when at least the call's :least items
  toward the wanted ones are carried at any other ending (:all wants every one). The data of every other ending has :gave-up and
  :short {name n}, what is still missing; the report counts the items carried toward the wanted ones.
  - condition-failed :chest-lacks {:gave-up false :held {name n} :short}: the chest, and the next ones of the group, hold
    none of what is short (info withdraw.short says so in words); some of the names may have been taken.
  - condition-failed :refused {:zones [..] :claims [..]}: a chest in another owner's zone or claim that does not allow
    :take, checked before the walk and again before the transfer (one withdraw.refused warn, nothing taken).
    :ignore-zones? true skips the check.
  - condition-failed :missing (no container at the chest; a recorded :chest place is retracted, one chest_missing warn): at once.
  - nothing-possible, the cause the inspect or transfer status (:unreachable, :full, :nothing-moved, ..): after three failed attempts
    in a row (warn withdraw.gave-up).
  Storage group: with no :chest, or a :chest of the group (jobs.lib.storage/group), a chest that lacks what is short
  is followed by the next one; :chest-lacks is reported only once the last has been looked into.
  Stock: what the chest holds is booked in body memory :fetch/stock when inspected and after each take
  (jobs.lib.stock), for jobs.items.obtain.")

(a/defargs args
  {:chest {:doc "chest position [x y z] or {:x :y :z}; the known :chest place when nil" :spec ::a/pos :default nil}
   :items {:doc "{item-name count}: carry at least this many of each name" :spec (a/map-of string? number?) :default {}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(defn shortfall
  "[[name n] ...] in the order of items: n is the target minus the total
  carried over all stacks, for the names with n above 0."
  [inventory items]
  (->> items
       (map (fn [[name target]] [name (- target (storage/carried inventory name))]))
       (filter (fn [[_ n]] (pos? n)))
       vec))

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a take may last: the walk and the transfers." :spec (a/num-in 0 nil)}
   ::take-s {:default 1 :doc "Seconds a quote counts for one transfer." :spec (a/num-in 0 nil)}
   ::walk-speed {:default 4 :doc "Blocks a second a quote counts for the walk to the chest." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:chest])

(def safe? ready/safe-at-chest)
(def fit? ready/ok)
(def defaults {:least :all :spend {:job-time ::job-time-s}})

(def can-start? checks/chest-known)
(def can-run? checks/chest-known)

(def carried-text "withdraw: everything wanted is carried")

(defn done?
  "Everything wanted is carried; :done counts the carried items toward the wanted ones."
  [c]
  (let [inv (u/inventory (:primitives c))
        items (:items (:args c))
        left (reduce + 0 (map second (shortfall inv items)))
        want (reduce + 0 (vals items))]
    (if (zero? left)
      (contract/ok :carried carried-text :units :items :done want :left 0)
      (contract/later :short (str "withdraw: " left " item(s) short") :units :items :done (- want left) :left left
                      :result {:short (into {} (shortfall inv items))}))))

(defn quote-of
  "Time for the walk and one transfer a name short; yields up to the shortfall, none when the chest lacks it."
  [c]
  (let [short (shortfall (u/inventory (:primitives c)) (:items (:args c)))
        chest (storage/chest-of (ctx/view c) (:args c))
        walk (if chest (/ (u/dist (u/self-pos c) chest) (settings/get settings ::walk-speed)) 0)
        t (+ walk (* (count short) (settings/get settings ::take-s)))]
    {:uses {:job-time [(* (count short) (settings/get settings ::take-s)) t (max t (time-s))]}
     :yields {:items (into {} (map (fn [[name n]] [name [0 n n]])) short)}}))

(defn end!
  "ctx/end, but done when the items carried toward the wanted meet the call's :least."
  [c kind cause data]
  (let [{:keys [done left]} (nth (done? c) 2)]
    (least/end! c kind cause data done (+ done left))))

(defn stop!
  "End with kind and cause: the data says :gave-up (else the job could go on) and the shortfall."
  ([c kind cause short] (stop! c kind cause short {}))
  ([c kind cause short more]
   (end! c kind cause (merge {:gave-up true :short (into {} short)} more))))

(defn status-stop!
  "stop! for a status that is not ok: a chest that is not there is a fact, any other status (no route included) a failure."
  [c status short]
  (stop! c (if (= "missing" status) :condition-failed :nothing-possible) (keyword status) short))

(defn give-up!
  "u/fail!: :again until the third failure in a row, then end nothing-possible with the status as the cause."
  [c status short]
  (let [r (u/fail! c :withdraw.gave-up (str "withdraw gave up: " status))]
    (if (= :done r) (status-stop! c status short) :again)))

(defn held-in
  "How many of name the inspected container items hold."
  [stacks name]
  (transduce (comp (filter #(= name (.-name %))) (map #(.-count %))) + 0 (array-seq stacks)))

(defn held-items
  "{name n} the inspected container items hold, summed per name."
  [stacks]
  (stock/stacks->items stacks))

(defn lacks-text
  "\"no shears in this chest (holds bread 5, dirt 2)\": the wanted names the chest lacks, and what it does hold. After
  earlier takes this call: \"took bread 3; no more bread in the chests (the last holds dirt 2)\"."
  ([names held] (lacks-text names held {}))
  ([names held taken]
   (let [holds (if (empty? held) "empty" (str "holds " (str/join ", " (map (fn [[n k]] (str n " " k)) (sort held)))))]
     (if (empty? taken)
       (str "no " (str/join ", " names) " in this chest (" holds ")")
       (str "took " (str/join ", " (map (fn [[n k]] (str n " " k)) (sort taken)))
            "; no more " (str/join ", " names) " in the chests (the last " holds ")")))))

(defn refuse!
  "End refused: one withdraw.refused warn naming the zones and claims; they are the cause and the data."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])
        refused (select-keys fields [:zones :claims])]
    (ctx/emit! c :withdraw.refused :warn (assoc fields :text (str "withdraw refused: " (access/refusal-text fields))))
    (end! c :condition-failed (assoc refused :reason :refused) (assoc refused :gave-up true))))

(defn ^:async take!
  "Inspect the chest in reach and take the first short name the chest holds: :again, or :done."
  [c chest short]
  (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
    (if (not= "ok" (.-status seen))
      (if (deposit/retracted? c chest (.-status seen))
        (status-stop! c (.-status seen) short)
        (give-up! c (.-status seen) short))
      (let [_ (stock/note-stock! c chest (.-items seen))
            pick (some (fn [[name n]] (let [held (held-in (.-items seen) name)]
                                        (when (pos? held) [name (min n held)])))
                       short)]
        (cond
          (nil? pick) (let [held (held-items (.-items seen))
                            lacking (into {} short)
                            others (rest (:open (deposit/chests c)))]
                        (when (seq others) (deposit/mark-full! c chest))
                        (if (seq others)
                          :again
                          (do (ctx/emit! c :withdraw.short :info {:short lacking :held held :text (lacks-text (map first short) held (:taken (ctx/mem c)))})
                              (end! c :condition-failed :chest-lacks {:gave-up false :held held :short lacking}))))
          (access/container-refusal c :take chest) (refuse! c (access/container-refusal c :take chest))
          :else
          (let [[name n] pick
                before (u/inventory (:primitives c))
                r (await (ctx/act c :transfer (clj->js {:pos chest :direction "withdraw" :item name :count n})))]
            (cond
              (not= "ok" (.-status r)) (if (deposit/retracted? c chest (.-status r))
                                           (status-stop! c (.-status r) short)
                                           (give-up! c (.-status r) short))
              (or (zero? (.-moved r))
                  (<= (storage/carried (u/inventory (:primitives c)) name) (storage/carried before name)))
              (give-up! c "nothing-moved" short)
              :else (do (stock/note-moved! c chest name (- (.-moved r)))
                        (ctx/update-mem! c update-in [:taken name] (fnil + 0) (.-moved r))
                        (u/progress! c)
                        :again))))))))

(defn ^:async step!
  "Walk in reach, then take one name: :again, :continue (go-to waits), or the ending when nothing is short, the chest
  holds none of what is short, or it stopped. The shortfall is read at the start of the step."
  [c]
  (let [{:keys [items]} (:args c)
        chest (some-> (first (:open (deposit/chests c))) deposit/cell-pos)
        short (shortfall (u/inventory (:primitives c)) items)]
    (cond
      (empty? short) (ctx/end c :done nil {:text carried-text})
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
