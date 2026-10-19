(ns jobs.storage.haul
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.pace :as pace]
            [jobs.lib.storage :as storage]
            [jobs.lib.least :as least]
            [jobs.lib.util :as u]
            [jobs.storage.deposit :as deposit]
            [jobs.storage.withdraw]))

(def doc
  "Move the contents of the :from chests to the main storage in bag loads. Loop: the hauled goods (only what the takes brought, not other carried goods) are deposited first
  (jobs.storage.deposit) once the bag is down to its last free slot
  or no source holds anything more; else walk to the next source chest, inspect it, take one bag load (one slot is
  left free; jobs.storage.withdraw as carry-at-least targets), repeat. The targets are saved before the take, so a
  restart takes the same load again without doubling it; the chests and the bag are the rest of the state.
  Needs (can-start? and can-run?): some :from, else never :bad-args; a main chest (:to, else the storage group, so a known :chest place), else waits :no-chest;
  any source still to empty that zones or claims of another refuse :take, else waits :refused {:zones :claims}. :ignore-zones? skips it.
  Sources that are in the storage group are not meant: withdraw may then take from the group's other chests.
  Ends done {:trips n :moved {name n}} once every source is empty (or holds only :keep), or at any other ending when at
  least the call's :least items are moved (default :all: only a clean finish); n counts the deposits. The data of
  every other ending has :trips and :moved so far (and :gave-up true):
  - condition-failed :bag-full (nothing to deposit yet no room to take), or the child's cause (:missing, ..);
  - nothing-possible :unreachable (no route to a source), :to-full (deposit found no room and could not place a chest), else the child's cause.")

(a/defargs args
  {:from {:doc "[x y z] or {:x :y :z} of each chest to empty" :spec (a/coll-of ::a/pos) :default []}
   :to {:doc "the chest to fill; the storage group (the :chest place first) when nil" :spec ::a/pos :default nil}
   :keep {:doc "item names left in the source chests" :spec (a/coll-of string?) :default []}
   :make-chests? {:doc "when the main storage is full, put a new chest beside it (deposit's :make-chests?)" :spec boolean? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 900 :doc "Seconds a haul may last: every walk, bag load and deposit." :spec (a/num-in 0 nil)}
   ::chest-s {:default 20 :doc "Seconds a quote counts for one source chest: the walk, the load and its share of the deposits." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:from])

(def safe? ready/threatened)
(def fit? ready/ok)
(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn no-chest
  "A main chest is known."
  [c]
  (when-not (or (some? (:to (:args c))) (seq (storage/group (ctx/view c))))
    {:reason :no-chest}))

(defn sources-left [c]
  (map deposit/cell-pos (remove (set (:empty (ctx/mem c))) (map storage/cell-of (:from (:args c))))))

(defn can-start? [c]
  (contract/all [((checks/args-fit "haul: no :from chest to empty" #(seq (:from (:args %)))) c)
                 (checks/cause-status :later (no-chest c))
                 ((checks/container-refused some :take sources-left) c)]))

(def can-run? can-start?)

(defn progress-data [c] (select-keys (ctx/mem c) [:trips :moved]))

(defn totals
  "{name n} carried over all stacks."
  [inv]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inv))

(defn hauled-keep
  "{name n} for deposit's :keep: what is carried beyond the hauled count stays with the body."
  [inv hauled]
  (let [carried (totals inv)]
    (into {} (map (fn [[n k]] [n (max 0 (- (get carried n 0) k))])) hauled)))

(defn spare-hauled
  "The next hauled stack to put away (deposit/to-deposit), or nil."
  [inv hauled]
  (when (seq hauled) (deposit/to-deposit inv (keys hauled) (hauled-keep inv hauled))))

(defn done?
  "Ok when every source chest was looked at, nothing spare is carried and no child is under way."
  [c]
  (let [pending (count (remove (set (:empty (ctx/mem c))) (map storage/cell-of (:from (:args c)))))
        text (str "haul: " pending " chest(s) to empty")
        data [:units :chests :done (- (count (:from (:args c))) pending) :left pending :result (progress-data c)]]
    (cond
      (or (pos? pending) (empty? (:from (:args c)))) (apply contract/later :hauling text data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up text data)
      (spare-hauled (u/inventory (:primitives c)) (:hauled (ctx/mem c))) (apply contract/later :hauling "haul: a last deposit to do" data)
      :else (apply contract/ok :hauled text data))))

(defn quote-of
  "Per source chest ::chest-s; at most the time default."
  [c]
  (let [n (count (:from (:args c)))
        t (* n (settings/get settings ::chest-s))]
    {:uses {:job-time [(/ t 4) t (max t (time-s))]}}))

(defn load-stacks
  "The chest stacks [{:name :count}] of one bag load: the first ones (one slot each) that fit in slots, minus the kept names."
  [stacks keep slots]
  (->> stacks (remove #(some #{(:name %)} keep)) (take slots)))

(defn load-plan
  "{name target}: carry-at-least targets that take the load-stacks."
  [stacks keep slots inv]
  (reduce (fn [m {:keys [name count]}] (update m name #(+ (or % (storage/carried inv name)) count)))
          {} (load-stacks stacks keep slots)))

(defn end!
  "ctx/end, but done when the items moved to the main storage meet the call's :least (:all never: the total is unknown)."
  [c kind cause data]
  (least/end! c kind cause data (reduce + 0 (vals (:moved (ctx/mem c)))) nil))

(defn stop!
  "End with kind and cause, the trips and what moved so far as the data (and the extra fields)."
  ([c kind cause] (stop! c kind cause {}))
  ([c kind cause more]
   (end! c kind cause (merge {:gave-up true} (progress-data c) more))))

(defn finish!
  [c]
  (ctx/end c :done nil {:trips (:trips (ctx/mem c) 0) :moved (:moved (ctx/mem c) {})}))

(defn child-stop!
  "End for a child that did not finish: a full main storage is :to-full, else the child's kind and cause (a refusal keeps
  its zones and claims)."
  [c e]
  (let [cause (contract/cause-key e)]
    (if (#{:all-chests-full :no-chest-item :no-chest-spot} cause)
      (stop! c :nothing-possible :to-full)
      (stop! c (:kind e) (:cause e) (dissoc (:data e) :gave-up :moved :short :trips)))))

(defn ^:async dump!
  "Deposit the hauled goods (only what the takes brought); counts the trip and what moved. With nothing moved it is the end, or :bag-full."
  [c pending]
  (let [before (totals (u/inventory (:primitives c)))
        hauled (:hauled (ctx/mem c))
        {:keys [to make-chests? ignore-zones?]} (:args c)
        e (await (ctx/call c :deposit 'jobs.storage.deposit
                           (cond-> {:make-chests? make-chests? :ignore-zones? ignore-zones?
                                    :items (vec (keys hauled)) :keep (hauled-keep (u/inventory (:primitives c)) hauled)}
                             to (assoc :chest to))))
        after (totals (u/inventory (:primitives c)))
        gone (into {} (keep (fn [[n k]] (let [d (- k (get after n 0))] (when (pos? d) [n d])))) before)]
    (cond
      (= :continue e) :continue
      (not= :done (:kind e)) (child-stop! c e)
      (empty? gone) (if (empty? pending) (finish! c) (stop! c :condition-failed :bag-full))
      :else (do (ctx/update-mem! c #(-> % (update :trips (fnil inc 0)) (update :moved (partial merge-with +) gone)
                                       (update :hauled (fn [h] (into {} (filter (comp pos? val)) (merge-with - h gone))))))
                (u/progress! c)
                :again))))

(defn ^:async look!
  "Walk to the chest and size one bag load from what it holds: saves the intent, or marks the chest empty."
  [c cell inv free]
  (let [chest (deposit/cell-pos cell)
        w (await (deposit/walk! c chest))]
    (cond
      (= :unreachable w) (stop! c :nothing-possible :unreachable)
      w w
      :else
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))
            stacks (map (fn [s] {:name (.-name s) :count (.-count s)}) (array-seq (.-items seen)))
            targets (when (= "ok" (.-status seen)) (load-plan stacks (:keep (:args c)) (dec free) inv))]
        (if (empty? targets)
          (do (ctx/update-mem! c update :empty (fnil conj []) cell) :again)
          (do (ctx/update-mem! c assoc :intent {:chest cell :targets targets :taken (apply merge-with + (map (fn [{:keys [name count]}] {name count}) (load-stacks stacks (:keep (:args c)) (dec free))))}) :again))))))

(defn ^:async take!
  "Take the saved load; the intent goes once the take is done. A chest that lacks part of it (:chest-lacks) is not a stop:
  the load is what the chest still held."
  [c {:keys [chest taken] :as intent}]
  (let [e (await (ctx/call c :take 'jobs.storage.withdraw
                           {:chest (deposit/cell-pos chest) :items (:targets intent) :ignore-zones? (:ignore-zones? (:args c))}))]
    (cond
      (= :continue e) :continue
      (not (or (= :done (:kind e)) (false? (get-in e [:data :gave-up])))) (child-stop! c e)
      :else (do (ctx/update-mem! c (fn [m] (-> m (dissoc :intent) (update :hauled (partial merge-with +) taken)))) (u/progress! c) :again))))

(defn ^:async step!
  "One move: the saved take, a deposit, the end, or the next look: :again, :continue or :done."
  [c]
  (let [m (ctx/mem c)
        inv (u/inventory (:primitives c))
        free (u/free-slots (:primitives c))
        pending (remove (set (:empty m)) (map storage/cell-of (:from (:args c))))
        spare? (spare-hauled inv (:hauled m))]
    (cond
      (:intent m) (await (take! c (:intent m)))
      (and spare? (or (<= free 1) (empty? pending))) (await (dump! c pending))
      (empty? pending) (finish! c)
      (<= free 1) (stop! c :condition-failed :bag-full)
      :else (await (look! c (first pending) inv free)))))

(defn ^:async round
  "The whole haul in one call; :continue only while a child waits on the world. Chests seen empty are looked at again
  on every start: they may have been refilled."
  [c]
  (ctx/update-mem! c dissoc :empty)
  (await (pace/steps! c (fn ^:async haul-step [] (await (step! c))))))
