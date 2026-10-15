(ns jobs.blocks.dig
  (:require [clojure.string :as str]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as look]
            [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.lava :as lava]
            [jobs.lib.need :as need]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Dig the one block at :pos ([x y z] or {:x :y :z}) and pick up what it dropped, as a player would. One call is the
  whole attempt: get a missing tool (:fetch, jobs.lib.need), walk into reach (one jobs.movement.go-to call), dig, pick up
  the drops (jobs.forestry.collect-drops: a drop in the dug hole only from a seen solid safe floor). It yields :continue
  only when a child waits on the world. Needs a tool that harvests the block (with :need-drop), room for the drop (with :collect) and
  a cell zones, claims and hazards allow. Quote: the walk and dig time, 1 s at least.

  Conditions (a listed call waits on them; once started the first to fail ends it :condition-failed with the cause):
  - {:reason :not-loaded :pos}. Impossible: {:reason :bad-args :text}.
  - {:reason :not-allowed :pos :by :zone|:claim|:footprint|:no-zones ...}: zones, claims or another plan's
    footprint refuse the dig (jobs.lib.access). :for-plan's own footprint does not. :ignore-zones? skips the rule.
  - {:reason :hazard :pos :hazards [kw ..]}: a dig hazard not in :accept (:fluid-adjacent water, :lava-adjacent seen
    lava, :falling-block). With :on-fluid :fail a fluid hazard ends the job :fluid-adjacent instead.
  - {:reason :inventory-full :pos}: with :collect, no free slot and no carried stack of the block's drop to grow.
  - {:reason :no-tool :needs item :block name :hint}: no carried tool harvests the block (with :need-drop) and :fetch
    does not allow a tool (otherwise the round gets one; a fetch that fails ends the same way, with :why).

  Out of reach, it walks within 3 cells (go-to child, which opens and shuts doors); a failed walk is tried once more,
  then it ends nothing-possible {:reason :unreachable :pos :why} (:no-walk, :out-of-reach, go-to's why).
  A block under the feet with no seen solid floor below it is not dug from on top: the body first steps off its
  column to a cell beside it on a seen floor (a go-to child), then digs; with none to stand on it ends {:reason :hazard
  :hazards [:under-feet] :why :no-side-stand}. Only :accept :under-feet digs it from on top.
  In reach it holds the best carried tool (tools/equip!; by hand when a tool of another kind is held) and digs through
  jobs.lib.tidy/dig!, so a dig of another's block with :ignore-zones? is recorded for jobs.survival.restore-broken.
  With :collect it picks up the drops (only the item entities that appeared with this dig, by id).
  With :on-lava :seal (default), seen lava beside the dug cell is filled with a building block from the rim
  (jobs.lib.lava, a jobs.blocks.place child, event blocks.dig.sealed); the dug cell stays dug. When it cannot be (no block
  carried or fetched (:seal-fetch), or two places fail) the body steps off the cells beside the lava and the job ends
  nothing-possible :lava-unsealed with :cell, :place (:need: no block) and :stepped-away.

  Returns info blocks.dig.done and an ending whose data is {:dug true|false :pos :block :collected n}, plus :left
  (collect-drops' drops it could not take), :stepped-aside [x y z] (the cell it stood on) when it stepped off the column
  first. :done with :reason :dug, or :already-clear (air there, nothing done). Condition-failed: :fluid (a fluid is not dug),
  :fluid-adjacent (:on-fluid :fail, with :hazards and a :hint), :cannot (bedrock and the like), and the conditions above.
  Nothing-possible: :lava-unsealed, :failed (the primitive refused, its status as :primitive). Limit-reached :time: the
  primitive timed out.")

(a/defargs args
  {:pos {:doc "the block to dig, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :collect {:doc "pick up what the dig dropped (needs a free slot)" :spec boolean? :default true}
   :need-drop {:doc "wait :no-tool when no carried tool harvests the block; false digs anyway and the drop is lost (clearing)" :spec boolean? :default true}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent water, :lava-adjacent seen lava, :falling-block; :under-feet digs a block under the feet from on top instead of from beside)" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block :under-feet}) :default #{}}
   :see {:doc "true: dig only from where the eye sees the block (jobs.lib.aim: nothing seen solid in the way, as a player aims), walking to such a stand first (go-to :see; none: :unreachable :out-of-sight); false: within reach is enough, the game lets a dig pass through blocks" :spec boolean? :default false}
   :walk {:doc "false: a block out of reach (or out of sight with :see) ends :unreachable :no-walk at once, the body takes no step to it (a pick on the way)" :spec boolean? :default true}
   :on-lava {:doc ":seal: seen lava beside the dug cell is filled with a building block (jobs.lib.lava); :leave: the caller deals with it (stair, tunnel)" :spec #{:seal :leave} :default :seal}
   :on-fluid {:doc ":wait: a block beside a fluid that :accept does not take waits :hazard; :fail: the job ends at once, reason :fluid-adjacent, with a :hint" :spec #{:wait :fail} :default :wait}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :enter-pens {:doc "true: the walk to the cell may go through the gate of a pen holding animals; false: it keeps out of those gates and, with no other way, declines (a :blocks.pen-kept-out warn names the pen)" :spec boolean? :default false}
   :seal-fetch {:doc "get a building block to seal lava with when none is carried, whatever :fetch says: true, a set of kinds or a map of limits, false to seal only with a carried block" :spec need/option? :default true}
   :fetch {:doc "get a missing tool instead of ending :no-tool (jobs.lib.need): true, a set of kinds or a map of limits" :spec need/option? :default true}})

(a/defargs settings
  {::collect-radius {:default 8 :doc "Blocks within which dropped items are collected after a dig." :spec (a/int-in 0 nil)}
   ::max-walks {:default 2 :doc "Failed walks of one call before it declines :unreachable." :spec (a/int-in 0 nil)}
   ::max-steps {:default 12 :doc "Walks, digs and fetches of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}
   ::time-s {:default 300 :doc "Seconds one call may spend (walk, tool fetch, dig, drops) when the caller sets no :spend :time." :spec (a/num-in 0 nil)}})
(defn collect-radius [] (settings/get settings ::collect-radius))

(defn use-tools
  "The tool names the call lists (:use {:tools [..]}), or nil when it names none. The default {:tools :dig} is every
  carried tool that digs, so only a call-given list narrows."
  [c]
  (when (vector? (get-in (:args c) [:use :tools])) (seq (ctx/use c :tools))))

(defn carried-names [c]
  (let [only (use-tools c)]
    (cond->> (map :name (u/inventory (:primitives c))) only (filter (set only)))))

(defn needs
  "What the body lacks to dig block-name with these args: {:tool item} (the cheapest tool of a call-given :use list
  that harvests it; :tool nil, :listed false when the list names none) or nil."
  [c block-name]
  (when (:need-drop (:args c))
    (let [harvest (some-> (.harvestTools (:primitives c) block-name) js->clj)
          only (use-tools c)
          wanted (cond->> harvest only (filter (set only)))]
      (when (some? (tools/harvest-need (carried-names c) harvest))
        (if (and only (empty? wanted))
          {:tool nil :listed false}
          {:tool (tools/harvest-need (carried-names c) wanted)})))))

(defn room?
  "Room for what block-name drops (b/drops-of, minecraft-data): nothing dropped, a free slot, or a carried stack of
  a dropped item under 64."
  [p block-name]
  (let [drops (set (b/drops-of p block-name))]
    (or (empty? drops)
        (pos? (u/free-slots p))
        (boolean (some #(and (drops (:name %)) (< (:count %) 64)) (u/inventory p))))))

(def no-tool-hint "no tool harvests it: pass :need-drop false to dig it by hand (slow, no drop)")

(defn bad-args [c]
  (when-let [error (:error (b/parse (:args c)))]
    (contract/impossible {:reason :bad-args :text (str "blocks.dig " error)})))

(defn not-loaded [c]
  (let [{:keys [pos error]} (b/parse (:args c))]
    (when (and (not error) (nil? (b/target-name (:primitives c) pos b/hidden-guess)))
      {:reason :not-loaded :pos pos})))

(defn problem
  "Why the cell or the bag refuses the dig now: a reason map (see doc), or nil. Air, fluids and an unloaded cell pass
  (the round ends them). A block still to be looked at (b/to-see?) is only guessed: room is judged once the round has looked."
  [c]
  (let [{:keys [pos error]} (b/parse (:args c))]
    (when-not error
      (let [p (:primitives c)
            block (b/target-name p pos b/hidden-guess)
            {:keys [accept collect]} (:args c)]
        (cond
          (:dug (b/mem-for c pos)) nil
          (or (nil? block) (b/air block) (b/fluids block)) nil
          :else
          (let [v (access/may-dig? (b/rules-in c) pos)]
            (case (access/judge v accept)
              :ok (when (and collect (not (b/to-see? c pos)) (not (room? p block)))
                    {:reason :inventory-full :pos pos})
              :hazard {:reason :hazard :pos pos
                       :hazards (into [] (comp (map access/hazard-key) (remove (set accept))) (:hazards v))}
              :not-loaded {:reason :not-loaded :pos pos}
              (or (b/not-allowed pos v) {:reason (:reason v) :pos pos}))))))))

(defn tool-problem
  "The {:reason :no-tool ...} the body meets at the block: no carried tool harvests it (with :need-drop), or nil."
  [c]
  (let [{:keys [pos error]} (b/parse (:args c))]
    (when-not (or error (:dug (b/mem-for c pos)) (b/to-see? c pos))
      (let [block (b/target-name (:primitives c) pos b/hidden-guess)]
        (when (and block (not (b/air block)) (not (b/fluids block)))
          (when-let [{:keys [tool listed]} (needs c block)]
            (if (false? listed)
              {:reason :no-tool :block block :hint "the :use list names no tool that harvests it"}
              {:reason :no-tool :needs tool :block block :hint no-tool-hint})))))))

(defn fetch-limits
  "The limits map of :fetch when it allows a tool, else nil."
  [c]
  (let [f (need/parse-arg (:fetch (:args c)))]
    (when (and f (contains? (or (:what f) need/kinds) :tool)) f)))

(defn fluid-refusal
  "With :on-fluid :fail, the hazard of a block beside a fluid, else nil."
  [c]
  (let [r (problem c)]
    (when (and (= :fail (:on-fluid (:args c))) (= :hazard (:reason r)) (some #{:fluid-adjacent :lava-adjacent} (:hazards r)))
      r)))

(defn refused [c] (when-not (fluid-refusal c) (problem c)))

(defn no-tool-unfetched [c] (when-not (fetch-limits c) (tool-problem c)))

(def target [:pos])

(def defaults {:spend {:time ::time-s} :least 1 :use {:tools :dig}})

(def conditions [[:fn bad-args] [:fn not-loaded] [:fn refused] [:fn no-tool-unfetched]])

(defn done? [c]
  (let [{:keys [pos]} (b/parse (:args c))
        air? (boolean (some-> (b/target-name (:primitives c) pos b/hidden-guess) b/air))]
    {:done? air? :units :blocks :done (if air? 1 0) :left (if air? 0 1)}))

(defn quote-of [c]
  (let [{:keys [pos]} (b/parse (:args c))
        walk (if (and pos (not (b/in-reach? c pos))) (/ (u/eye-dist (u/self-pos c) pos) 4) 0)]
    {:uses {:time [1 (+ 2 walk) (+ 20 (* 2 walk))]}}))

(def done-reasons #{:dug :already-clear})

(def condition-reasons #{:fluid :fluid-adjacent :cannot})

(defn text-of [result cell place stepped-aside]
  (str (if (:dug result) "dug " "did not dig ") (or (:block result) "") " at " (pr-str (b/cell (:pos result)))
       (when stepped-aside (str " from beside (stepped aside to " (pr-str stepped-aside) ": no seen floor under it)"))
       (cond
         (= :lava-unsealed (:reason result))
         (str ": lava at " (pr-str cell) " not sealed: " (if (= :need place) "no block to seal with" (name (or place :tries)))
              (when-not (:stepped-away result) "; could not step away"))
         (not (:dug result)) (str ": " (name (:reason result)))
         (seq (:left result)) (str "; " (count (:left result)) " drop(s) left: "
                                   (str/join ", " (distinct (map (comp name :reason) (:left result))))))))

(defn finish!
  "Tell blocks.dig.done and end the call with result {:dug :pos :block :reason ..}: :done for :dug and :already-clear,
  else condition-failed (:fluid :fluid-adjacent :cannot), nothing-possible (:lava-unsealed, a refused dig) or
  limit-reached :time (the primitive timed out). The data is the result."
  [c result]
  (let [result (cond-> result (:stepped-aside (ctx/mem c)) (assoc :stepped-aside (:stepped-aside (ctx/mem c))))
        {:keys [cell place stepped-aside reason]} result]
    (ctx/emit! c :blocks.dig.done :info (assoc result :text (text-of result cell place stepped-aside)))
    (cond
      (done-reasons reason) (ctx/end c :done nil result)
      (condition-reasons reason) (ctx/end c :condition-failed reason (dissoc result :reason))
      (= "timeout" (:primitive result)) (ctx/end c :limit-reached {:line :time :reason :timeout} (dissoc result :reason))
      :else (ctx/end c :nothing-possible reason (dissoc result :reason)))))

(def search-blocks
  "How far round the dug cell collect-drops walks to look for a drop that is out of sight (behind leaves)."
  3)

(defn drops-wanted
  "How many of the dug block's drops (JS array of {:name :count}) are still to come in: their count less what the inventory
  gained of those items across the dig (an adjacent drop flies to the body); 1 when the dig saw no drop, so the search stays."
  [drops before after]
  (let [drops (mapv (fn [d] {:name (.-name d) :count (.-count d)}) (array-seq (or drops #js [])))
        names (set (map :name drops))
        carried (fn [inv] (transduce (comp (filter (comp names :name)) (map :count)) + 0 inv))]
    (if (empty? drops) 1 (max 0 (- (transduce (map :count) + 0 drops) (- (carried after) (carried before)))))))

(defn mark-full
  "The left drops with their reason :inventory-full when the body has no room for the block's drop now (the pickup filled the last stack), else as they are."
  [left room?]
  (if room? left (mapv #(assoc % :reason :inventory-full) left)))

(defn ^:async collect!
  "Pick up the dug block's drops (jobs.forestry.collect-drops child); :continue while it waits, else the call's ending."
  [c pos]
  (let [{:keys [block ids want]} (:dug (ctx/mem c))
        r (await (child/run!* c :collect 'jobs.forestry.collect-drops {:radius (collect-radius) :ids ids :holes [pos] :search search-blocks :want want}))]
    (if (= :continue r)
      :continue
      (let [res (:data r)]
        (finish! c (cond-> {:dug true :pos pos :block block :reason :dug :collected (:collected res 0)}
                     (seq (:left res)) (assoc :left (mark-full (:left res) (room? (:primitives c) block)))))))))

(defn ^:async seal-step!
  "One step after a dig with :on-lava :seal: nil when no seen lava borders the dug cell, :again once one lava cell is
  sealed, :continue while a child waits on the world, :done once the job stopped :lava-unsealed (after the body
  stepped off the cells beside the lava). Re-reads the world each step; memory [:dug :unsealed] holds a failed seal."
  [c pos]
  (let [{:keys [block unsealed]} (:dug (ctx/mem c))
        cell (b/cell pos)]
    (cond
      unsealed (let [r (await (lava/step-away! c cell (:lavas unsealed)))]
                 (if (= :continue r)
                   :continue
                   (finish! c (merge {:dug true :pos pos :block block :stepped-away (= :arrived r)} (dissoc unsealed :lavas)
                                    (when (map? r) (select-keys r [:unreachable :walk]))))))
      (not= :seal (:on-lava (:args c))) nil
      :else
      (when-let [lavas (seq (do (await (lava/look-round! c cell)) (lava/exposed (:primitives c) cell)))]
        (let [r (await (lava/seal! c :blocks.dig.sealed #(u/seen-name (:primitives c) (zipmap [:x :y :z] %)) (first lavas)
                                   {:fetch (:seal-fetch (:args c)) :ignore-zones? (:ignore-zones? (:args c))}))]
          (if (map? r)
            (do (ctx/update-mem! c assoc-in [:dug :unsealed] (assoc r :lavas (vec lavas))) :again)
            r))))))

(defn ^:async dig!
  "Hold the best tool and dig; book the outcome. Resolves to :done (finished), :collect (dug: lava to seal, drops to
  pick up) or :unreachable (the primitive says out of reach)."
  [c pos block]
  (await (tools/equip! c block {:only (use-tools c)}))
  (let [before (u/inventory (:primitives c))
        r (await (tidy/dig! c pos))
        status (.-status r)
        _ (when (= "dug" status) (await (look/look-at! c (b/cell pos))))
        ids (vec (keep #(.-id %) (array-seq (or (.-drops r) #js []))))]
    (case status
      "dug" (do (ctx/update-mem! c assoc :dug {:block block :ids ids :want (drops-wanted (.-drops r) before (u/inventory (:primitives c)))}) :collect)
      "missing" (finish! c {:dug false :pos pos :block block :reason :already-clear})
      "cannot" (finish! c {:dug false :pos pos :block block :reason :cannot})
      "unreachable" :unreachable
      (finish! c {:dug false :pos pos :block block :reason :failed :primitive status}))))

(defn max-walks [] (settings/get settings ::max-walks))
(defn max-steps [] (settings/get settings ::max-steps))

(defn ^:async step-aside!
  "The block to dig is under the feet with no seen solid floor below: step off the column to a cell beside it within
  1 block (jobs.lib.step-off). :arrived, :continue, or the ending {:reason :hazard :why :no-side-stand} (none to stand
  on, or the walk failed)."
  [c pos]
  (let [[x y z] (b/feet-cell c)
        r (await (step-off/step-off-zoned! c {:x x :y y :z z} {:reach 1}))]
    (if (keyword? r)
      r
      (let [why (:unreachable r)]
        (ctx/end c :condition-failed
                 (cond-> {:reason :hazard :pos pos :hazards [:under-feet] :why :no-side-stand}
                   (not= :no-cell why) (assoc :walk why)))))))

(defn unreachable!
  "End the call nothing-possible {:reason :unreachable :pos :why}: the walk ran and found no way."
  [c why]
  (ctx/end c :nothing-possible {:reason :unreachable :pos (:pos (b/parse (:args c))) :why why}))

(defn note-under-feet!
  "Debug event blocks.dig.under-feet before the first dig of the cell under the feet: the floor below as seen and as
  guessed, the hazards the rules read, and whether the body steps aside."
  [c pos decision]
  (let [[fx fy fz] (b/feet-cell c)
        [x y z] (b/cell pos)
        below [x (dec y) z]
        p (:primitives c)]
    (when (= [x y z] [fx (dec fy) fz])
      (ctx/emit! c :blocks.dig.under-feet :debug
                 {:feet [fx fy fz] :cell [x y z] :decision decision
                  :floor-seen ((access/seen-at p) below) :floor-read ((access/sensed-at p b/hidden-guess) below)
                  :hazards (mapv :reason (:hazards (access/may-dig? (b/rules-in c) pos)))
                  :text (str "dig under the feet at " (pr-str [x y z]) ": " (name decision))}))))

(defn ^:async get-tool!
  "Get the tool the block needs (jobs.lib.need): :fetched, :continue while the fetch waits, or the ending when the
  fetch is off or fails (condition-failed :no-tool, :why the fetch's cause)."
  [c tool-problem]
  (let [limits (fetch-limits c)
        what (if (use-tools c) {:item (:needs tool-problem)} {:block (:block tool-problem)})
        r (when (and limits (:needs tool-problem)) (await (need/tool! c what (select-keys limits [:how :explore]))))]
    (cond
      (or (nil? limits) (nil? (:needs tool-problem))) (ctx/end c :condition-failed tool-problem)
      (= :continue r) :continue
      (= :done (:kind r)) :fetched
      :else (ctx/end c (:kind r) (assoc tool-problem :why (contract/cause-key r))))))

(defn ^:async attempt!
  "The whole dig: get a tool if due, walk into reach, dig, pick up the drops. A walk that fails or leaves the block
  out of reach is tried once more, then the call ends :unreachable. :continue or the ending."
  [c pos]
  (loop [fails 0 steps 0]
    (let [p (:primitives c)
          _ (await (b/see-target! c pos))
          block (b/target-name p pos b/hidden-guess)
          refusal (fluid-refusal c)
          dug? (:dug (ctx/mem c))
          tool (when-not dug? (tool-problem c))]
      (cond
        refusal (finish! c {:dug false :pos pos :block block :reason :fluid-adjacent :hazards (:hazards refusal)
                            :hint "pass :accept #{:fluid-adjacent} to dig beside water (:lava-adjacent for seen lava), or :on-fluid :wait to wait for it to drain"})
        dug?
        (let [s (await (seal-step! c pos))
              {:keys [block ids]} (:dug (ctx/mem c))]
          (cond
            (= :again s) (recur fails (inc steps))
            s s
            (and (:collect (:args c)) (seq ids)) (await (collect! c pos))
            :else (finish! c {:dug true :pos pos :block block :reason :dug :collected 0})))
        (b/air block) (finish! c {:dug false :pos pos :block block :reason :already-clear})
        (b/fluids block) (finish! c {:dug false :pos pos :block block :reason :fluid})
        tool (let [r (await (get-tool! c tool))]
               (if (= :fetched r) (recur fails (inc steps)) r))
        (>= steps (max-steps)) :continue
        (and (not (:walk (:args c))) (not (b/in-reach? c pos (:see (:args c)))))
        (unreachable! c :no-walk)
        (not (b/in-reach? c pos (:see (:args c))))
        (let [w (await (b/walk!* c pos (:see (:args c))))]
          (cond
            (= :continue w) :continue
            (= :arrived w) (recur fails (inc steps))
            (not (#{:nothing-possible :condition-failed} (:kind w))) w
            (>= (inc fails) (max-walks)) (unreachable! c (or (:why (:data w)) (when (map? (:cause w)) (:why (:cause w))) (contract/cause-key w)))
            :else (recur (inc fails) (inc steps))))
        (problem c) (ctx/end c :condition-failed (problem c))
        (and (access/under-feet? (access/may-dig? (b/rules-in c) pos)) (not (contains? (set (:accept (:args c))) :under-feet)))
        (let [_ (note-under-feet! c pos :step-aside)
              r (await (step-aside! c pos))]
          (if (= :arrived r)
            (do (ctx/update-mem! c assoc :stepped-aside (b/feet-cell c)) (recur fails (inc steps)))
            r))
        :else
        (let [_ (note-under-feet! c pos :dig-on-top)
              d (await (dig! c pos block))]
          (cond
            (= :collect d) (recur fails (inc steps))
            (= :unreachable d) (unreachable! c :out-of-reach)
            :else d))))))

(defn ^:async round [c]
  (let [{:keys [pos]} (b/parse (:args c))]
    (when-not (= pos (:for (ctx/mem c))) (ctx/update-mem! c b/fresh-mem pos))
    (await (attempt! c pos))))
