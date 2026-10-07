(ns jobs.blocks.place
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.placement :as placement]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [engine.game :as game]))

(def doc
  "Place one block at :pos ([x y z] or {:x :y :z}): :item, or the first carried of :any-of. One call is the
  whole attempt: fetch a missing item (:fetch), clear the cell, walk into reach (one jobs.movement.go-to call),
  place. It yields :continue only when a child waits on the world.

  The check waits with a reason (ctx/wait; job.waiting and observe show it) and places nothing when:
  - {:reason :need :item name :pos} or {:reason :need :any-of [names] :pos}: the item is not carried.
  - {:reason :not-allowed :pos :by :zone|:claim|:footprint|:no-zones ...}: zones, claims or another plan's
    footprint refuse the place. :for-plan's own footprint does not. :ignore-zones? skips the rule.
  - {:reason :own-body :pos}: the body stands in the cell (not for seeds, carpets and the like: no collision).
  - {:reason :no-support :pos}: no solid neighbour to place against.
  - {:reason :unreachable :pos :why kw}: the go-to child gave up. The wait lasts while the body stands where it
    gave up.
  - {:reason :not-loaded :pos}.
  - While a plant is being cleared, the jobs.blocks.dig child's own wait reason.

  A plant, flower or snow layer in the cell (jobs.lib.blocks/clearable) is dug first with a jobs.blocks.dig
  child (no tool needed, the drop is not collected). Out of reach, the round walks within 3 cells (go-to child).
  In reach it places through jobs.lib.tidy/place!, so a block placed in another's zone with :ignore-zones? is
  recorded for jobs.survival.restore-broken.

  Ends with info blocks.place.done and {:placed true|false :pos :item :reason}; a placed block carries :block
  {:name :state} as the place act reported it. :reason is :placed or :already
  (the cell holds the block: nothing done): done. Stopped ({:status :stopped}, :placed false): :occupied (another
  block fills the cell), :clear-failed (the plant could not be dug), :bad-args (with a blocks.place.declined
  warn) or :failed (the primitive refused, with its status as :primitive). Declined, the check then waits: :unreachable (the walk failed twice or the place is out of reach), a zone
  or other cell reason that appeared during the call, or an item still missing after the fetch (:need). The caller
  decides whether to try again.

  :fetch (default true; jobs.lib.fetch; false waits :need): a :need wait is not waited out. The call runs jobs.items.obtain for one
  of the item (child :fetch), then place. A failed fetch is remembered for :fail-minutes; meanwhile the check waits
  :need with {:fetch {:failed reason ...}}.")

(a/defargs args
  {:pos {:doc "the cell to fill, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :item {:doc "the block item to place" :spec a/item? :default nil}
   :any-of {:doc "block items, the first carried one is placed (instead of :item)" :spec (a/coll-of a/item?) :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :clear {:doc "dig a plant, flower or snow layer out of the cell first; false places into what a placement overwrites" :spec boolean? :default true}
   :click {:doc "how to place it ({:against [x y z] :cursor [x y z] :yaw :pitch :sneak}, jobs.lib.placement/click); nil: plainly" :spec map? :default nil}
   :fetch {:doc "get the missing block item instead of waiting :need (jobs.lib.fetch): true, a set of kinds or a map of limits" :spec fetch/option? :default true}})

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
  "What the body lacks: {:item name} or {:any-of [names]} when none of items is carried, else nil."
  [c items]
  (let [carried (set (map :name (u/inventory (:primitives c))))]
    (when-not (some carried items)
      (if (= 1 (count items)) {:item (first items)} {:any-of items}))))

(defn clear-args
  "The jobs.blocks.dig args for clearing the plant at pos."
  [c pos]
  (merge (select-keys (:args c) [:for-plan :ignore-zones?])
         {:pos pos :collect false :need-drop false :accept #{:fluid-adjacent :falling-block}}))

(defn clears?
  "Whether the call digs block out of the cell before placing (jobs.lib.blocks/clearable, unless :clear is false)."
  [c block]
  (and (:clear (:args c) true) (b/clearable block)))

(defn place-verdict
  "The rules' place verdict for pos, a plant there counted as gone (it is dug first). A block without collision
  (seeds, carpet, a sapling: rules/no-collision?) may go where the body stands."
  [c pos items]
  (let [in (cond-> (b/rules-in c) (every? #(rules/no-collision? (game/version-of (:primitives c)) %) items) (assoc :feet nil))
        at (b/cell pos)
        p (:primitives c)
        block-at (:block-at in)
        read (fn [cell] (if (= cell at) (b/target-name p pos "air") (block-at cell)))]
    (access/may? (assoc in :block-at (fn [cell] (let [n (read cell)] (if (and (= cell at) (clears? c n)) "air" n))))
                 :place pos)))

(defn occupied-by
  "The name of the block that fills pos and cannot be replaced (not the item wanted), or nil."
  [c pos items]
  (let [block (b/target-name (:primitives c) pos "air")
        v (when block (place-verdict c pos items))]
    (when (and block (not (some #{block} items)) (= :not-replaceable (:reason v))) block)))

(defn problem
  "Why the job cannot run now: a wait reason map (see doc), or nil. Bad args and a cell already holding the block
  pass: the round ends them."
  [c]
  (let [{:keys [pos items error]} (parse (:args c))]
    (when-not error
      (let [p (:primitives c)
            block (b/target-name p pos "air")
            v (when block (place-verdict c pos items))]
        (cond
          (nil? block) {:reason :not-loaded :pos pos}
          (some #{block} items) nil
          (= :not-loaded (:reason v)) {:reason :not-loaded :pos pos}
          (b/not-allowed pos v) (b/not-allowed pos v)
          (= :own-body (:reason v)) {:reason :own-body :pos pos}
          (= :not-replaceable (:reason v)) nil
          (not (:ok v)) {:reason (:reason v) :pos pos}
          :else (or (when-let [n (needs c items)] (assoc n :reason :need :pos pos))
                    (when-not (or (b/support? p pos) (b/unseen-near? p pos)) {:reason :no-support :pos pos})
                    (b/unreachable-wait c (b/mem-for c pos) pos)
                    (when (clears? c block) (b/child-wait c :clear 'jobs.blocks.dig (clear-args c pos)))))))))

(defn check [c]
  (if-let [r (problem c)]
    (fetch/check c 'jobs.blocks.place r)
    true))

(defn finish! [c result]
  (let [text (str (if (:placed result) "placed " "did not place ") (:item result) " at " (pr-str (b/cell (:pos result)))
                  (when-not (:placed result)
                    (str ": " (name (:reason result)) (when (:block result) (str " by " (:block result))))))]
    (ctx/emit! c :blocks.place.done :info (assoc result :text text))
    (ctx/result! c result)
    :done))

(defn stop!
  "finish! for a place that did not happen and could not: result :status :stopped."
  [c result]
  (finish! c (assoc result :status :stopped)))

(defn ^:async clear!
  "One jobs.blocks.dig call on the plant at pos. Resolves to :cleared, :continue (the dig waits on the world) or the
  end of the job (:done, stopped :clear-failed)."
  [c pos items]
  (let [r (await (ctx/call-child c :clear 'jobs.blocks.dig (clear-args c pos)))
        res (ctx/child-result c :clear)]
    (cond
      (= :continue r) :continue
      (and (= :done r) (#{:dug :already-clear} (:reason res))) :cleared
      :else (stop! c {:placed false :pos pos :item (first items) :reason :clear-failed
                      :block (:block res) :why (or (:reason res) (:reason (b/child-wait c :clear 'jobs.blocks.dig (clear-args c pos))))}))))

(defn ^:async place!
  "Place the chosen item. Resolves to :done (finished) or :unreachable (the primitive says out of reach)."
  [c pos items]
  (let [item (b/chosen c items)
        click (:click (:args c))
        r (await (tidy/place! c pos item false (some-> click placement/js-click)))
        status (.-status r)]
    (case status
      "placed" (finish! c (cond-> {:placed true :pos pos :item item :reason :placed}
                            (.-placed r) (assoc :block {:name (.-name (.-placed r))
                                                        :state (js->clj (.-properties (.-placed r)) :keywordize-keys true)})))
      "unreachable" :unreachable
      (stop! c {:placed false :pos pos :item item :reason :failed :primitive status}))))

(def max-walks "Failed walks of one call before it declines :unreachable." 2)
(def max-steps "Walks, clears and fetches of one call before it gives the round back with :continue." 12)

(defn unreachable!
  "Remember why the cell cannot be reached from here; the check waits on it while the body stays. :declined."
  [c why]
  (b/unreachable! c why)
  :declined)

(defn ^:async attempt!
  "The whole place: fetch the item if due, clear a plant, walk into reach, place. A walk that fails or leaves the cell
  out of reach is tried once more, then the job declines :unreachable."
  [c pos items]
  (loop [fails 0 steps 0]
    (let [_ (await (b/see-target! c pos))
          block (b/target-name (:primitives c) pos "air")
          full (occupied-by c pos items)
          already (some #{block} items)
          r (when-not (or full already) (await (fetch/fetch! c 'jobs.blocks.place problem)))
          block (if r block (b/target-name (:primitives c) pos "air"))]
      (cond
        full (stop! c {:placed false :pos pos :item (first items) :reason :occupied :block full})
        r r
        already (finish! c {:placed false :pos pos :item block :reason :already})
        (>= steps max-steps) :continue
        (clears? c block)
        (let [w (await (clear! c pos items))]
          (cond (= :cleared w) (recur fails (inc steps))
                :else w))
        (not (b/in-reach? c pos))
        (let [w (await (b/walk! c pos))]
          (cond
            (= :continue w) :continue
            (= :arrived w) (recur fails (inc steps))
            (>= (inc fails) max-walks) (unreachable! c (:unreachable w))
            :else (recur (inc fails) (inc steps))))
        (nil? (b/chosen c items))
        :declined
        (b/refused-now c (problem c)) :declined
        :else
        (let [d (await (place! c pos items))]
          (if (= :done d)
            :done
            (unreachable! c :out-of-reach)))))))

(defn ^:async round [c]
  (let [{:keys [pos items error]} (parse (:args c))]
    (if error
      (do (ctx/emit! c :blocks.place.declined :warn {:reason :bad-args :text (str "blocks.place " error)})
          (ctx/result! c {:status :stopped :placed false :reason :bad-args :text error})
          :done)
      (do (when-not (= pos (:for (ctx/mem c))) (ctx/update-mem! c b/fresh-mem pos))
          (await (attempt! c pos items))))))
