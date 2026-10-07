(ns jobs.blocks.dig
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as look]
            [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Dig the one block at :pos ([x y z] or {:x :y :z}) and pick up what it dropped, as a player would. One call is the
  whole attempt: fetch a missing tool (:fetch), walk into reach (one jobs.movement.go-to call), dig, pick up the
  drops (jobs.forestry.collect-drops). It yields :continue only when a child waits on the world.

  The check waits with a reason (ctx/wait; job.waiting and observe show it) and never digs when:
  - {:reason :not-allowed :pos :by :zone|:claim|:footprint|:no-zones ...}: zones, claims or another plan's
    footprint refuse the dig (jobs.lib.access). :for-plan's own footprint does not. :ignore-zones? skips the
    rule.
  - {:reason :hazard :pos :hazards [kw ..]}: a dig hazard not in :accept (:fluid-adjacent :falling-block
    :under-feet). With :on-fluid :fail a :fluid-adjacent hazard ends the job instead (see below).
  - {:reason :no-tool :needs item :block name}: with :need-drop, no carried tool harvests the block
    (tools/can-harvest?). :needs is the cheapest tool that does.
  - {:reason :inventory-full :pos}: with :collect, no free slot and no carried stack of the block's drop to
    grow. The drop is judged by minecraft-data (stone gives cobblestone, leaves nothing).
  - {:reason :unreachable :pos :why kw}: the go-to child gave up, or arrived with the block still out of reach.
    The wait lasts while the body stands where it gave up, so a body moved by anyone tries again.
  - {:reason :not-loaded :pos}.

  Out of reach, it walks within 3 cells (go-to child, which opens and shuts doors); a failed walk is tried once more.
  In reach it holds the best carried tool (tools/equip-for!) and digs through jobs.lib.tidy/dig!, so a dig of
  another's block with :ignore-zones? is recorded for jobs.survival.restore-broken. With :collect it picks up the
  drops (only the item entities that appeared with this dig, by id).

  Ends with info blocks.dig.done and {:dug true|false :pos :block :reason :collected n}. :reason is :dug or
  :already-clear (air there, nothing done): done. Stopped ({:status :stopped}, :dug false): :fluid (a fluid is not
  dug), :fluid-adjacent (:on-fluid :fail, with :hazards and a :hint), :cannot (bedrock and the like), :bad-args (with
  a blocks.dig.declined warn), or :failed (the primitive refused: a timeout, with its status as :primitive). Declined, the check
  then waits: :unreachable (the walk failed twice or the dig is out of reach), a zone or hazard that appeared during
  the call, or a tool still missing after the fetch (:no-tool). The caller decides whether to try again.

  :fetch (default true; jobs.lib.fetch; false waits :no-tool): a :no-tool wait is not waited out. The check passes and the call runs
  jobs.items.get-tool for the block (child :fetch) until a tool is carried, then digs. A fetch that fails is
  remembered for :fail-minutes; meanwhile the check waits :no-tool with {:fetch {:failed reason ...}}.")

(a/defargs args
  {:pos {:doc "the block to dig, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :collect {:doc "pick up what the dig dropped (needs a free slot)" :spec boolean? :default true}
   :need-drop {:doc "wait :no-tool when no carried tool harvests the block; false digs anyway and the drop is lost (clearing)" :spec boolean? :default true}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block :under-feet)" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block :under-feet}) :default #{}}
   :on-fluid {:doc ":wait: a block beside a fluid that :accept does not take waits :hazard; :fail: the job ends at once, reason :fluid-adjacent, with a :hint" :spec #{:wait :fail} :default :wait}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :fetch {:doc "get a missing tool instead of waiting :no-tool (jobs.lib.fetch): true, a set of kinds or a map of limits" :spec fetch/option? :default true}})

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
  "Why the job cannot run now: a wait reason map (see doc), or nil. Bad args, air and fluids pass: the round ends them.
  A block still to be looked at (b/to-see?) is only guessed: tool and room are judged once the round has looked."
  [c]
  (let [{:keys [pos error]} (b/parse (:args c))]
    (when-not error
      (let [m (b/mem-for c pos)
            p (:primitives c)
            block (b/target-name p pos b/hidden-guess)
            {:keys [accept collect]} (:args c)]
        (cond
          (:dug m) nil
          (nil? block) {:reason :not-loaded :pos pos}
          (or (b/air block) (b/fluids block)) nil
          :else
          (let [v (access/may-dig? (b/rules-in c) pos)]
            (case (access/judge v accept)
              :ok (or (b/unreachable-wait c m pos)
                      (when-not (b/to-see? c pos)
                        (or (when-let [{:keys [tool]} (needs c block)]
                              {:reason :no-tool :needs tool :block block})
                            (when (and collect (not (room? p block)))
                              {:reason :inventory-full :pos pos}))))
              :hazard {:reason :hazard :pos pos
                       :hazards (into [] (comp (map :reason) (remove (set accept))) (:hazards v))}
              :not-loaded {:reason :not-loaded :pos pos}
              (or (b/not-allowed pos v) {:reason (:reason v) :pos pos}))))))))

(defn fluid-refusal
  "With :on-fluid :fail, the hazard wait of a block beside a fluid, else nil."
  [c]
  (let [r (problem c)]
    (when (and (= :fail (:on-fluid (:args c))) (= :hazard (:reason r)) (some #{:fluid-adjacent} (:hazards r)))
      r)))

(defn check [c]
  (if-let [r (when-not (fluid-refusal c) (problem c))]
    (fetch/check c 'jobs.blocks.dig r)
    true))

(defn finish!
  [c result]
  (let [text (str (if (:dug result) "dug " "did not dig ") (or (:block result) "") " at " (pr-str (b/cell (:pos result)))
                  (when-not (:dug result) (str ": " (name (:reason result)))))]
    (ctx/emit! c :blocks.dig.done :info (assoc result :text text))
    (ctx/result! c result)
    :done))

(defn stop!
  "finish! for a dig that did not happen and could not: result :status :stopped."
  [c result]
  (finish! c (assoc result :status :stopped)))

(defn ^:async collect!
  "Pick up the dug block's drops (jobs.forestry.collect-drops child, called until it ends); finish when it has."
  [c pos]
  (let [{:keys [block ids]} (:dug (ctx/mem c))
        r (await (child/run! c :collect 'jobs.forestry.collect-drops {:radius collect-radius :ids ids}))]
    (if (= :continue r)
      :continue
      (finish! c {:dug true :pos pos :block block :reason :dug
                  :collected (if (= :done r) (:collected (ctx/child-result c :collect) 0) 0)}))))

(defn ^:async dig!
  "Hold the best tool and dig; book the outcome. Resolves to :done (finished), :collect (dug, drops to pick up) or
  :unreachable (the primitive says out of reach)."
  [c pos block]
  (await (tools/equip-for! c block))
  (let [r (await (tidy/dig! c pos))
        status (.-status r)
        _ (when (= "dug" status) (await (look/look-at! c (b/cell pos))))
        ids (vec (keep #(.-id %) (array-seq (or (.-drops r) #js []))))]
    (case status
      "dug" (if (and (:collect (:args c)) (seq ids))
              (do (ctx/update-mem! c assoc :dug {:block block :ids ids}) :collect)
              (finish! c {:dug true :pos pos :block block :reason :dug :collected 0}))
      "missing" (finish! c {:dug false :pos pos :block block :reason :already-clear})
      "cannot" (stop! c {:dug false :pos pos :block block :reason :cannot})
      "unreachable" :unreachable
      (stop! c {:dug false :pos pos :block block :reason :failed :primitive status}))))

(def max-walks "Failed walks of one call before it declines :unreachable." 2)
(def max-steps "Walks, digs and fetches of one call before it gives the round back with :continue." 12)

(defn unreachable!
  "Remember why the block cannot be reached from here; the check waits on it while the body stays. :declined."
  [c why]
  (b/unreachable! c why)
  :declined)

(defn ^:async attempt!
  "The whole dig: fetch a tool if due, walk into reach, dig, pick up the drops. A walk that fails or leaves the block
  out of reach is tried once more, then the job declines :unreachable."
  [c pos]
  (loop [fails 0 steps 0]
    (let [p (:primitives c)
          _ (await (b/see-target! c pos))
          block (b/target-name p pos b/hidden-guess)
          refused (fluid-refusal c)
          r (when-not (or refused (:dug (ctx/mem c))) (await (fetch/fetch! c 'jobs.blocks.dig problem)))
          tool (when (and block (not (:dug (ctx/mem c))) (not (b/air block)) (not (b/fluids block))) (needs c block))]
      (cond
        refused (stop! c {:dug false :pos pos :block block :reason :fluid-adjacent :hazards (:hazards refused)
                          :hint "pass :accept #{:fluid-adjacent} to dig beside water, or :on-fluid :wait to wait for it to drain"})
        r r
        (:dug (ctx/mem c)) (await (collect! c pos))
        (b/air block) (finish! c {:dug false :pos pos :block block :reason :already-clear})
        (b/fluids block) (stop! c {:dug false :pos pos :block block :reason :fluid})
        tool :declined
        (>= steps max-steps) :continue
        (not (b/in-reach? c pos))
        (let [w (await (b/walk! c pos))]
          (cond
            (= :continue w) :continue
            (= :arrived w) (recur fails (inc steps))
            (>= (inc fails) max-walks) (unreachable! c (:unreachable w))
            :else (recur (inc fails) (inc steps))))
        (b/refused-now c (problem c)) :declined
        :else
        (let [d (await (dig! c pos block))]
          (cond
            (= :done d) :done
            (= :collect d) (recur fails (inc steps))
            :else (unreachable! c :out-of-reach)))))))

(defn ^:async round [c]
  (let [{:keys [pos error]} (b/parse (:args c))]
    (if error
      (do (ctx/emit! c :blocks.dig.declined :warn {:reason :bad-args :text (str "blocks.dig " error)})
          (ctx/result! c {:status :stopped :dug false :reason :bad-args :text error})
          :done)
      (do (when-not (= pos (:for (ctx/mem c))) (ctx/update-mem! c b/fresh-mem pos))
          (await (attempt! c pos))))))
