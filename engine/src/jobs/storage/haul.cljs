(ns jobs.storage.haul
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.pace :as pace]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.storage.deposit :as deposit]
            [jobs.storage.withdraw]))

(def doc
  "Move the contents of the :from chests to the main storage in bag loads. Loop: carried goods are deposited first
  (jobs.storage.deposit: everything but tools, armour and the food reserve) once the bag is down to its last free slot
  or no source holds anything more; else walk to the next source chest, inspect it, take one bag load (one slot is
  left free; jobs.storage.withdraw as carry-at-least targets), repeat. The targets are saved before the take, so a
  restart takes the same load again without doubling it; the chests and the bag are the rest of the state.
  Needs: a main chest (:to, else the storage group, so a known :chest place); waits with reason :no-chest. No :from stops :no-source. Sources that
  are in the storage group are not meant: withdraw may then take from the group's other chests.
  Returns {:gave-up false :trips n :moved {name n}} once every source is empty (or holds only :keep); n counts the
  deposits. Stops {:gave-up true :status :stopped :reason r :trips :moved}: :no-source, :to-full (deposit found no room and could
  not place a chest), :unreachable, :refused (zones; the refusal fields come with it), :bag-full (nothing to deposit
  yet no room to take), else the child's reason.")

(a/defargs args
  {:from {:doc "[x y z] or {:x :y :z} of each chest to empty" :spec (a/coll-of ::a/pos) :default []}
   :to {:doc "the chest to fill; the storage group (the :chest place first) when nil" :spec ::a/pos :default nil}
   :keep {:doc "item names left in the source chests" :spec (a/coll-of string?) :default []}
   :make-chests? {:doc "when the main storage is full, put a new chest beside it (deposit's :make-chests?)" :spec boolean? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(defn check
  "A main chest is known; else waits with reason :no-chest."
  [c]
  (or (some? (:to (:args c))) (seq (storage/group (ctx/view c))) (ctx/wait c {:reason :no-chest})))

(defn totals
  "{name n} carried over all stacks."
  [inv]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inv))

(defn load-plan
  "{name target}: carry-at-least targets that take one bag load from the chest stacks [{:name :count}]: the first
  stacks (one slot each) that fit in slots, minus the kept names."
  [stacks keep slots inv]
  (->> stacks
       (remove #(some #{(:name %)} keep))
       (take slots)
       (reduce (fn [m {:keys [name count]}] (update m name #(+ (or % (storage/carried inv name)) count))) {})))

(defn stop!
  "End stopped with reason (and the extra fields), the trips and what moved so far."
  ([c reason] (stop! c reason {}))
  ([c reason more]
   (ctx/result! c (merge {:gave-up true :status :stopped :reason reason} (select-keys (ctx/mem c) [:trips :moved]) more))
   :done))

(defn finish!
  [c]
  (ctx/result! c {:gave-up false :trips (:trips (ctx/mem c) 0) :moved (:moved (ctx/mem c) {})})
  :done)

(defn child-stop!
  "Stop for a child that gave up: refused keeps its fields, a full main storage is :to-full."
  [c res]
  (let [r (:reason res)]
    (cond
      (= :refused r) (stop! c :refused (dissoc res :gave-up :status))
      (#{"all-chests-full" "no-chest-item" "no-chest-spot"} r) (stop! c :to-full)
      (= "unreachable" r) (stop! c :unreachable)
      :else (stop! c (or r :gave-up)))))

(defn ^:async dump!
  "Deposit the carried goods; counts the trip and what moved. With nothing moved it is the end, or :bag-full."
  [c pending]
  (let [before (totals (u/inventory (:primitives c)))
        {:keys [to make-chests? ignore-zones?]} (:args c)
        r (await (ctx/call-child c :deposit 'jobs.storage.deposit
                                 (cond-> {:make-chests? make-chests? :ignore-zones? ignore-zones?} to (assoc :chest to))))
        res (when (= :done r) (ctx/child-result c :deposit))
        after (totals (u/inventory (:primitives c)))
        gone (into {} (keep (fn [[n k]] (let [d (- k (get after n 0))] (when (pos? d) [n d])))) before)]
    (cond
      (= :continue r) :continue
      (= :declined r) (stop! c :no-chest)
      (:gave-up res) (child-stop! c res)
      (empty? gone) (if (empty? pending) (finish! c) (stop! c :bag-full))
      :else (do (ctx/update-mem! c #(-> % (update :trips (fnil inc 0)) (update :moved (partial merge-with +) gone)))
                (u/progress! c)
                :again))))

(defn ^:async look!
  "Walk to the chest and size one bag load from what it holds: saves the intent, or marks the chest empty."
  [c cell inv free]
  (let [chest (deposit/cell-pos cell)
        refusal (when-not (:ignore-zones? (:args c)) (access/container-refusal c :take chest))
        w (when-not refusal (await (deposit/walk! c chest)))]
    (cond
      refusal (do (ctx/result! c (merge (access/refused-result refusal) (select-keys (ctx/mem c) [:trips :moved]))) :done)
      (= :unreachable w) (stop! c :unreachable)
      w w
      :else
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))
            stacks (map (fn [s] {:name (.-name s) :count (.-count s)}) (array-seq (.-items seen)))
            targets (when (= "ok" (.-status seen)) (load-plan stacks (:keep (:args c)) (dec free) inv))]
        (if (empty? targets)
          (do (ctx/update-mem! c update :empty (fnil conj []) cell) :again)
          (do (ctx/update-mem! c assoc :intent {:chest cell :targets targets}) :again))))))

(defn ^:async take!
  "Take the saved load; the intent goes once the take is done."
  [c {:keys [chest targets]}]
  (let [r (await (ctx/call-child c :take 'jobs.storage.withdraw
                                 {:chest (deposit/cell-pos chest) :items targets :ignore-zones? (:ignore-zones? (:args c))}))
        res (when (= :done r) (ctx/child-result c :take))]
    (cond
      (= :continue r) :continue
      (= :declined r) (stop! c :unreachable)
      (:gave-up res) (child-stop! c res)
      :else (do (ctx/update-mem! c dissoc :intent) (u/progress! c) :again))))

(defn ^:async step!
  "One move: the saved take, a deposit, the end, or the next look: :again, :continue or :done."
  [c]
  (let [m (ctx/mem c)
        inv (u/inventory (:primitives c))
        free (u/free-slots (:primitives c))
        pending (remove (set (:empty m)) (map storage/cell-of (:from (:args c))))
        spare? (deposit/to-deposit inv nil)]
    (cond
      (:intent m) (await (take! c (:intent m)))
      (and spare? (or (<= free 1) (empty? pending))) (await (dump! c pending))
      (empty? pending) (finish! c)
      (<= free 1) (stop! c :bag-full)
      :else (await (look! c (first pending) inv free)))))

(defn ^:async round
  "The whole haul in one call; :continue only while a child waits on the world. Chests seen empty are looked at again
  on every start: they may have been refilled."
  [c]
  (if (empty? (:from (:args c)))
    (stop! c :no-source)
    (do (ctx/update-mem! c dissoc :empty)
        (await (pace/steps! c (fn ^:async haul-step [] (await (step! c))))))))
