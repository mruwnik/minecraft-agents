(ns jobs.blocks.dig
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Dig the one block at :pos ([x y z] or {:x :y :z}) and pick up what it dropped, as a player would. One act per
  round: a jobs.movement.go-to round, an equip and the dig, or a jobs.forestry.collect-drops round.

  The check waits with a reason (ctx/wait; job.waiting and observe show it) and never digs when:
  - {:reason :not-allowed :pos :by :zone|:claim|:footprint|:no-zones ...}: zones, claims or another plan's
    footprint refuse the dig (jobs.lib.access). :for-plan's own footprint does not. :ignore-zones? skips the
    rule.
  - {:reason :hazard :pos :hazards [kw ..]}: a dig hazard not in :accept (:fluid-adjacent :falling-block
    :under-feet).
  - {:reason :no-tool :needs item :block name}: with :need-drop, no carried tool harvests the block
    (tools/can-harvest?). :needs is the cheapest tool that does.
  - {:reason :inventory-full :pos}: with :collect, no free slot and no carried stack of the block's drop to
    grow. The drop is judged by minecraft-data (stone gives cobblestone, leaves nothing).
  - {:reason :unreachable :pos :why kw}: the go-to child gave up, or arrived with the block still out of reach.
    The wait lasts while the body stands where it gave up, so a body moved by anyone tries again.
  - {:reason :not-loaded :pos}.

  Out of reach, the round walks within 3 cells (go-to child, which opens and shuts doors). In reach it holds the
  best carried tool (tools/equip-for!) and digs through jobs.lib.tidy/dig!, so a dig of another's block with
  :ignore-zones? is recorded for jobs.survival.restore-broken. With :collect the next rounds pick up the drops
  (jobs.forestry.collect-drops child, only the item entities that appeared with this dig, by id).

  Ends with info blocks.dig.done and {:dug true|false :pos :block :reason :collected n}. :reason is :dug,
  :already-clear (air there, nothing done), :fluid (a fluid is not dug), :cannot (bedrock and the like) or
  :bad-args (with a blocks.dig.declined warn). A dig the primitive refuses (a timeout, a failure) ends :failed
  with its :status at once. The caller decides whether to try again.

  :fetch (default false; jobs.lib.fetch): a :no-tool wait is not waited out. The check passes and the rounds run
  jobs.items.get-tool for the block (child :fetch) until a tool is carried, then dig. A fetch that fails is
  remembered for :fail-minutes; meanwhile the check waits :no-tool with {:fetch {:failed reason ...}}.")

(def args
  {:pos {:doc "the block to dig, [x y z] or {:x :y :z}" :default nil}
   :collect {:doc "pick up what the dig dropped (needs a free slot)" :default true}
   :need-drop {:doc "wait :no-tool when no carried tool harvests the block; false digs anyway and the drop is lost (clearing)" :default true}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block :under-feet)" :default #{}}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :fetch {:doc "get a missing tool instead of waiting :no-tool (jobs.lib.fetch): true, a set of kinds or a map of limits" :default false}})

(def collect-radius 8)

(defn needs
  "What the body lacks to dig block-name with these args: {:tool item} or nil."
  [c block-name]
  (when (and (:need-drop (:args c)) (not (tools/can-harvest? (:primitives c) block-name)))
    {:tool (tools/harvest-need (map :name (u/inventory (:primitives c)))
                               (some-> (.harvestTools (:primitives c) block-name) js->clj))}))

(defn room?
  "Room for what block-name drops (b/drops-of, minecraft-data): nothing dropped, a free slot, or a carried stack of
  a dropped item under 64."
  [p block-name]
  (let [drops (set (b/drops-of p block-name))]
    (or (empty? drops)
        (pos? (u/free-slots p))
        (boolean (some #(and (drops (:name %)) (< (:count %) 64)) (u/inventory p))))))

(defn problem
  "Why the job cannot run now: a wait reason map (see doc), or nil. Bad args, air and fluids pass: the round ends them."
  [c]
  (let [{:keys [pos error]} (b/parse (:args c))]
    (when-not error
      (let [m (b/mem-for c pos)
            p (:primitives c)
            block (u/block-name p pos)
            {:keys [accept collect]} (:args c)]
        (cond
          (:dug m) nil
          (nil? block) {:reason :not-loaded :pos pos}
          (or (b/air block) (b/fluids block)) nil
          :else
          (let [v (access/may-dig? (b/rules-in c) pos)]
            (case (access/judge v accept)
              :ok (or (b/unreachable-wait c m pos)
                      (when-let [{:keys [tool]} (needs c block)]
                        {:reason :no-tool :needs tool :block block})
                      (when (and collect (not (room? p block)))
                        {:reason :inventory-full :pos pos}))
              :hazard {:reason :hazard :pos pos
                       :hazards (into [] (comp (map :reason) (remove (set accept))) (:hazards v))}
              :not-loaded {:reason :not-loaded :pos pos}
              (or (b/not-allowed pos v) {:reason (:reason v) :pos pos}))))))))

(defn check [c]
  (if-let [r (problem c)]
    (fetch/check c 'jobs.blocks.dig r)
    true))

(defn finish!
  [c result]
  (let [text (str (if (:dug result) "dug " "did not dig ") (or (:block result) "") " at " (pr-str (b/cell (:pos result)))
                  (when-not (:dug result) (str ": " (name (:reason result)))))]
    (ctx/emit! c :blocks.dig.done :info (assoc result :text text))
    (ctx/result! c result)
    :done))

(defn ^:async collect!
  "One collect-drops round over the dug block's drops; finish when it is done."
  [c pos]
  (let [{:keys [block ids]} (:dug (ctx/mem c))
        r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius collect-radius :ids ids}))]
    (if (= :done r)
      (finish! c {:dug true :pos pos :block block :reason :dug
                  :collected (:collected (ctx/child-result c :collect) 0)})
      :continue)))

(defn ^:async dig!
  "Hold the best tool and dig; book the outcome."
  [c pos block]
  (await (tools/equip-for! c block))
  (let [r (await (tidy/dig! c pos))
        status (.-status r)
        ids (vec (keep #(.-id %) (array-seq (or (.-drops r) #js []))))]
    (case status
      "dug" (if (and (:collect (:args c)) (seq ids))
              (do (ctx/update-mem! c assoc :dug {:block block :ids ids}) :continue)
              (finish! c {:dug true :pos pos :block block :reason :dug :collected 0}))
      "missing" (finish! c {:dug false :pos pos :block block :reason :already-clear})
      "cannot" (finish! c {:dug false :pos pos :block block :reason :cannot})
      "unreachable" (do (ctx/update-mem! c assoc :unreachable {:from (b/feet-cell c) :why :out-of-reach}) :continue)
      (finish! c {:dug false :pos pos :block block :reason :failed :status status}))))

(defn ^:async round [c]
  (let [{:keys [pos error]} (b/parse (:args c))]
    (if error
      (do (ctx/emit! c :blocks.dig.declined :warn {:reason :bad-args :text (str "blocks.dig " error)})
          (ctx/result! c {:dug false :reason :bad-args :text error})
          :done)
      (let [_ (when-not (= pos (:for (ctx/mem c))) (ctx/update-mem! c b/fresh-mem pos))
            block (u/block-name (:primitives c) pos)
            r (await (fetch/step! c 'jobs.blocks.dig (problem c)))]
        (cond
          r r
          (:dug (ctx/mem c)) (await (collect! c pos))
          (b/air block) (finish! c {:dug false :pos pos :block block :reason :already-clear})
          (b/fluids block) (finish! c {:dug false :pos pos :block block :reason :fluid})
          (not (b/in-reach? c pos)) (await (b/walk! c pos))
          :else (await (dig! c pos block)))))))
