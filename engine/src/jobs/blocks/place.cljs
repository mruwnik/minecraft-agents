(ns jobs.blocks.place
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.blocks :as b]
            [engine.jobs.tidy :as tidy]
            [engine.jobs.util :as u]))

(def doc
  "Place one block at :pos ([x y z] or {:x :y :z}): :item, or the first carried of :any-of. One act per round (a
  jobs.movement.go-to round, a jobs.blocks.dig round clearing the cell, or the place).

  The check says whether it can run, and when it cannot it waits with a reason (ctx/wait; job.waiting and observe
  show it) and places nothing:
    {:reason :need :item name :pos} or {:reason :need :any-of [names] :pos}  the item is not carried
    {:reason :not-allowed :pos :by :zone|:claim|:footprint|:no-zones ...}  zones, claims or another plan's footprint
      refuse the place (:for-plan's own footprint does not; :ignore-zones? skips the rule)
    {:reason :occupied :pos :block name}  a block that is neither air, a fluid nor a plant or snow layer fills the cell
    {:reason :own-body :pos}  the body stands in the cell
    {:reason :no-support :pos}  no solid neighbour to place against
    {:reason :unreachable :pos :why kw}  the go-to child gave up; the wait lasts while the body stands where it gave up
    {:reason :not-loaded :pos}
    and while a plant is being cleared, the jobs.blocks.dig child's own wait reason.
  A plant, flower or snow layer in the cell (engine.jobs.blocks/clearable) is dug first with a jobs.blocks.dig child
  (no tool needed, the drop is not collected). Out of reach, the round walks within 3 cells (go-to child). In reach it
  places through engine.jobs.tidy/place!, so a block placed in another's zone with :ignore-zones? is recorded for
  jobs.survival.restore-broken. Ends with info blocks.place.done and the result {:placed true|false :pos :item :reason};
  :reason is :placed, :already (the cell holds the block: nothing done), :bad-args (with a blocks.place.declined warn),
  :clear-failed (the plant could not be dug) or :failed (the primitive refused the place, with its :status: the
  caller decides whether to try again).
  Fetching a missing item is not done yet: a :fetch option (items.obtain as a child) will hook in where the :need
  wait is made (needs).")

(def args
  {:pos {:doc "the cell to fill, [x y z] or {:x :y :z}" :default nil}
   :item {:doc "the block item to place" :default nil}
   :any-of {:doc "block items, the first carried one is placed (instead of :item)" :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(defn wanted [{:keys [item any-of]}]
  (vec (if item [item] any-of)))

(defn parse
  "{:pos :items [names]} of the args, or {:error text}."
  [args]
  (let [{:keys [pos error]} (b/parse args)
        items (wanted args)]
    (cond
      error {:error error}
      (not (and (seq items) (every? string? items))) {:error "needs :item, or :any-of [names]"}
      :else {:pos pos :items items})))

(defn needs
  "What the body lacks: {:item name} or {:any-of [names]} when none of items is carried, else nil. The hook for a
  :fetch option (B2): a parent or this job may obtain it instead of waiting."
  [c items]
  (let [carried (set (map :name (u/inventory (:primitives c))))]
    (when-not (some carried items)
      (if (= 1 (count items)) {:item (first items)} {:any-of items}))))

(defn chosen
  "The first of items carried, or nil."
  [c items]
  (let [carried (set (map :name (u/inventory (:primitives c))))]
    (first (filter carried items))))

(def neighbours [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])

(defn support?
  "Whether a neighbour of pos is a block to place against (not air, a fluid or a plant)."
  [p {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]]
                   (let [n (u/block-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})]
                     (and n (not (b/air n)) (not (b/fluids n)) (not (b/clearable n)))))
                 neighbours)))

(defn clear-args
  "The jobs.blocks.dig args for clearing the plant at pos."
  [c pos]
  (merge (select-keys (:args c) [:for-plan :ignore-zones?])
         {:pos pos :collect false :need-drop false :accept #{:fluid-adjacent :falling-block}}))

(defn place-verdict
  "The rules' place verdict for pos, a plant there counted as gone (it is dug first)."
  [c pos]
  (let [in (b/rules-in c)
        at (b/cell pos)
        block-at (:block-at in)]
    (access/may? (assoc in :block-at (fn [cell] (let [n (block-at cell)] (if (and (= cell at) (b/clearable n)) "air" n))))
                 :place pos)))

(defn problem
  "Why the job cannot run now: a wait reason map (see doc), or nil. Bad args and a cell already holding the block
  pass: the round ends them."
  [c]
  (let [{:keys [pos items error]} (parse (:args c))]
    (when-not error
      (let [p (:primitives c)
            block (u/block-name p pos)
            v (when block (place-verdict c pos))]
        (cond
          (nil? block) {:reason :not-loaded :pos pos}
          (some #{block} items) nil
          (= :not-loaded (:reason v)) {:reason :not-loaded :pos pos}
          (b/not-allowed pos v) (b/not-allowed pos v)
          (= :own-body (:reason v)) {:reason :own-body :pos pos}
          (= :not-replaceable (:reason v)) {:reason :occupied :pos pos :block block}
          (not (:ok v)) {:reason (:reason v) :pos pos}
          :else (or (when-let [n (needs c items)] (assoc n :reason :need :pos pos))
                    (when-not (support? p pos) {:reason :no-support :pos pos})
                    (b/unreachable-wait c (b/mem-for c pos) pos)
                    (when (b/clearable block) (b/child-wait c :clear 'jobs.blocks.dig (clear-args c pos)))))))))

(defn check [c]
  (if-let [r (problem c)]
    (ctx/wait c r)
    true))

(defn finish! [c result]
  (let [text (str (if (:placed result) "placed " "did not place ") (or (:item result) "") " at " (pr-str (b/cell (:pos result)))
                  (when-not (:placed result) (str ": " (name (:reason result)))))]
    (ctx/emit! c :blocks.place.done :info (assoc result :text text))
    (ctx/result! c result)
    :done))

(defn ^:async clear! [c pos]
  (let [r (await (ctx/call-child c :clear 'jobs.blocks.dig (clear-args c pos)))
        res (ctx/child-result c :clear)]
    (if (and (= :done r) (not (#{:dug :already-clear} (:reason res))))
      (finish! c {:placed false :pos pos :reason :clear-failed :block (:block res) :why (:reason res)})
      :continue)))

(defn ^:async place! [c pos items]
  (let [item (chosen c items)
        r (await (tidy/place! c pos item))
        status (.-status r)]
    (case status
      "placed" (finish! c {:placed true :pos pos :item item :reason :placed})
      "unreachable" (do (ctx/update-mem! c assoc :unreachable {:from (b/feet-cell c) :why :out-of-reach}) :continue)
      (finish! c {:placed false :pos pos :item item :reason :failed :status status}))))

(defn ^:async round [c]
  (let [{:keys [pos items error]} (parse (:args c))]
    (if error
      (do (ctx/emit! c :blocks.place.declined :warn {:reason :bad-args :text (str "blocks.place " error)})
          (ctx/result! c {:placed false :reason :bad-args :text error})
          :done)
      (let [_ (when-not (= pos (:for (ctx/mem c))) (ctx/update-mem! c b/fresh-mem pos))
            block (u/block-name (:primitives c) pos)]
        (cond
          (some #{block} items) (finish! c {:placed false :pos pos :item block :reason :already})
          (b/clearable block) (await (clear! c pos))
          (not (b/in-reach? c pos)) (await (b/walk! c pos))
          (nil? (chosen c items)) :continue
          :else (await (place! c pos items)))))))
