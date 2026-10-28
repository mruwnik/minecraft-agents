(ns jobs.storage.withdraw
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.price :as price]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.stock :as stock]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.chests :as chests]
            [jobs.lib.least :as least]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]))

(def doc
  "Walk to the chest (one go-to) and take items out, every name in one call, until at least the wanted count of
  each name in :items is carried. What is short is re-derived from the inventory before each take, so a cut or
  restart loses nothing.
  Needs (can-start? and can-run?): a chest, the :chest argument or the known :chest place, else waits :no-chest (a chest place lost mid-job
  waits, a reflex ends condition-failed); a chest of the group that zones or claims of another do not refuse :take, else waits
  :refused {:zones :claims}. :ignore-zones? true skips it.
  Ends done when everything is carried (also at once when it already is), or when at least the call's :least items
  toward the wanted ones are carried at any other ending (:all wants every one). The data of every other ending has :gave-up and
  :short {name n}, what is still missing; the report counts the items carried toward the wanted ones.
  - condition-failed :chest-lacks {:gave-up false :held {name n} :short}: the chest, and the next ones of the group, hold
    none of what is short (the ending's text says so in words); some of the names may have been taken.
  - condition-failed :missing (no container at the chest; a recorded :chest place is retracted, one chest_missing warn): at once.
  - nothing-possible, the cause the inspect or transfer status (:unreachable, :full, :nothing-moved, ..): after three failed attempts
    in a row.
  Storage group: with no :chest, or a :chest of the group (jobs.lib.storage/group), a chest that lacks what is short
  is followed by the next one; :chest-lacks is reported only once the last has been looked into.
  Stock: what the chest holds is booked in body memory :fetch/stock when inspected and after each take
  (jobs.lib.stock), for jobs.items.obtain.
  Entity form (:entity, the id of a container entity in reach, a chest minecart; no :items): the caller walks within reach, and
  every stack is moved to the bag. Needs: an entity not refused for :take (as above), and no :items, else never :bad-args.
  Ends done {:full bool :moved {item n}}: :full true when a later transfer moved nothing for lack of room, what fitted is taken.
  Else condition-failed :bag-full (the first transfer found no room) or :missing (the entity is gone), or nothing-possible with the
  inspect or transfer status as the cause (:unreachable, :nothing-moved, ..).")

(a/defargs args
  {:chest {:doc "chest position [x y z] or {:x :y :z}; the known :chest place when nil" :spec ::a/pos :default nil}
   :items {:doc "{item-name count}: carry at least this many of each name" :spec (a/map-of string? number?) :default {}}
   :entity {:doc "entity id of a container in reach (a chest minecart): move every stack to the bag; takes no :chest or :items" :spec number? :default nil}
   :ignore-zones? :shared})

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
   ::take-s {:default 1 :doc "Seconds a quote counts for one transfer." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:chest :entity])

(defn entity-form? [c] (some? (:entity (:args c))))

(def safe-here (price/stays ready/safe-here-by-night (constantly price/chest-act-s)))
(def safe-chest (price/stays ready/safe-at-chest (price/trip-s ready/chest-place price/chest-act-s)))
(defn safe? [c] (if (entity-form? c) (safe-here c) (safe-chest c)))
(def fit? ready/ok)
(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn entity-positions [c]
  (let [id (:entity (:args c))]
    (->> (look/seen-entities (:primitives c) {:radius 16 :max 32})
         (filter #(= id (:id %)))
         (map :pos))))

(defn can-start? [c]
  (if (entity-form? c)
    (contract/all [((checks/args-fit "withdraw: an :entity takes no :chest or :items"
                                     #(and (empty? (:items (:args %))) (nil? (:chest (:args %))))) c)
                   ((checks/container-refused every? :take entity-positions) c)])
    (contract/all [(checks/chest-known c) ((checks/container-refused every? :take (chests/pool-positions :take)) c)])))
(def can-run? can-start?)

(def carried-text "withdraw: everything wanted is carried")

(defn entity-done? [c]
  (if (:emptied (ctx/mem c))
    (contract/ok :emptied "withdraw: the container is empty or the bag full" :units :container :done 1 :left 0)
    (contract/later :emptying "withdraw: stacks still to move" :units :container :done 0 :left 1)))

(defn items-done?
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

(defn done? [c] (if (entity-form? c) (entity-done? c) (items-done? c)))

(defn items-quote
  "Time for the walk and one transfer a name short; yields up to the shortfall, none when the chest lacks it."
  [c]
  (let [short (shortfall (u/inventory (:primitives c)) (:items (:args c)))
        chest (storage/chest-of (ctx/view c) (:args c))
        walk (if chest (/ (u/dist (u/self-pos c) chest) cost/walk-speed) 0)
        t (+ walk (* (count short) (settings/get settings ::take-s)))]
    {:uses {:job-time [(* (count short) (settings/get settings ::take-s)) t (max t (time-s))]}
     :yields {:items (into {} (map (fn [[name n]] [name [0 n n]])) short)}}))

(defn quote-of [c]
  (if (entity-form? c)
    {:uses {:job-time [1 5 (time-s)]}}
    (items-quote c)))

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
  "Count a failure: :again until the third in a row, then end nothing-possible with the status as the cause."
  [c status short]
  (if (u/count-fail! c) (status-stop! c status short) :again))

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
   (let [holds (if (empty? held) "holds nothing" (str "holds " (str/join ", " (map (fn [[n k]] (str n " " k)) (sort held)))))]
     (if (empty? taken)
       (str "no " (str/join ", " names) " in this chest (" holds ")")
       (str "took " (str/join ", " (map (fn [[n k]] (str n " " k)) (sort taken)))
            "; no more " (str/join ", " names) " in the chests (the last " holds ")")))))

(defn ^:async take!
  "Look into the chest (walking into reach) and take the first short name the chest holds: :again, :continue, or the ending."
  [c chest short]
  (let [seen (await (chests/look-in! c chest))]
    (cond
      (= :continue seen) :continue
      (not= "ok" (:status seen))
      (if (:retracted? seen)
        (status-stop! c (:status seen) short)
        (give-up! c (:status seen) short))
      :else
      (let [pick (some (fn [[name n]] (let [held (held-in (:items seen) name)]
                                        (when (pos? held) [name (min n held)])))
                       short)]
        (cond
          (nil? pick) (let [held (held-items (:items seen))
                            lacking (into {} short)
                            others (rest (:open (chests/chests c :take)))]
                        (when (seq others) (chests/mark-full! c chest))
                        (if (seq others)
                          :again
                          (let [text (lacks-text (map first short) held (:taken (ctx/mem c)))]
                            (end! c :condition-failed :chest-lacks {:gave-up false :held held :short lacking :text text}))))
          :else
          (let [[name n] pick
                before (u/inventory (:primitives c))
                r (await (ctx/act c :transfer (clj->js {:pos chest :direction "withdraw" :item name :count n})))]
            (cond
              (not= "ok" (.-status r)) (if (chests/retracted? c chest (.-status r))
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
        chest (some-> (first (:open (chests/chests c :take))) chests/cell-pos)
        short (shortfall (u/inventory (:primitives c)) items)]
    (cond
      (empty? short) (ctx/end c :done nil {:text carried-text})
      :else
      (await (take! c chest short)))))

(defn ^:async empty-entity!
  "Inspect the container entity and move its stacks one after another; a transfer that moves nothing ends it done :full once something moved, else :bag-full or nothing-possible; any other failure nothing-possible."
  [c]
  (let [id (:entity (:args c))
        seen (await (ctx/act c :inspectContainer (clj->js {:entity id})))
        status (.-status seen)]
    (if (not= "ok" status)
      (ctx/end c (if (= "missing" status) :condition-failed :nothing-possible) (keyword status))
      (loop [stacks (vec (stock/stacks->items (.-items seen))) moved {}]
        (if-let [[name n] (first stacks)]
          (let [r (await (ctx/act c :transfer (clj->js {:entity id :direction "withdraw" :item name :count n})))
                status (.-status r)]
            (cond
              (and (= "ok" status) (pos? (.-moved r))) (recur (rest stacks) (assoc moved name (.-moved r)))
              (and (seq moved) (contains? #{"ok" "full"} status)) (do (ctx/update-mem! c assoc :emptied true) (ctx/end c :done nil {:full true :moved moved}))
              (= "full" status) (ctx/end c :condition-failed :bag-full)
              (= "ok" status) (ctx/end c :nothing-possible :nothing-moved)
              :else (ctx/end c :nothing-possible (keyword status))))
          (do (ctx/update-mem! c assoc :emptied true) (ctx/end c :done nil {:full false :moved moved})))))))

(defn ^:async round
  "The whole take in one call, a name after another until nothing is short or it stops. :continue only while go-to
  waits on the world. The entity form empties the container in reach at once."
  [c]
  (if (entity-form? c)
    (await (empty-entity! c))
    (await (pace/steps! c (fn ^:async withdraw-step [] (await (step! c)))))))
