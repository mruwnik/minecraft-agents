(ns jobs.blocks.place
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.waits :as waits]
            [jobs.lib.ready :as ready]
            [engine.ctx :as ctx]
            [engine.contract :as contract]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.cost :as cost]
            [jobs.lib.torch :as torch]
            [jobs.lib.choose :as choose]
            [jobs.lib.aim :as aim]
            [jobs.lib.dig-look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.placement :as placement]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]
            [jobs.lib.world :as world]
            [engine.game :as game]))

(def doc
  "Place one block at :pos ([x y z] or {:x :y :z}): :item, or the first carried of :use {:blocks [names]} (default: the
  carried building blocks, jobs.lib.choose :build). One call is the whole attempt: fetch a missing item (the call's :ways), clear
  the cell, walk into reach (one jobs.movement.go-to call), step off the cell it stands in (jobs.lib.step-off; not for
  seeds, carpets and the like: no collision), place. It yields :continue only when a child waits on the world.

  Needs: the item carried (or fetchable by the call's :ways, jobs.lib.need). Conditions (can-start? and can-run?: a listed call waits on them, a called one ends condition-failed with the cause)
  and place nothing when:
  - {:reason :need :item name :pos} or {:reason :need :any-of [names] :pos}: not carried and the call's :ways is #{:carried}.
  - {:reason :not-allowed :pos :by :zone|:claim|:footprint|:no-zones ...}: zones, claims or another plan's
    footprint refuse the place. :for-plan's own footprint does not. :ignore-zones? skips the rule.
  - {:reason :no-support :pos}: no solid neighbour to place against (not for boats).
  - {:reason :not-loaded :pos}.

  A plant, flower or snow layer in the cell (jobs.lib.blocks/clearable) is dug first with a jobs.blocks.dig child (no tool
  needed, the drop is not collected); so is a leaves block alone in the way of the eye to a cell within reach (:see). When the walk to a stand that sees the cell fails, the first seen terrain block in the line within twice the reach (jobs.lib.blocks/through-natural; not :for-plan's own cell) is dug (a jobs.blocks.dig child), as a player does. Out of reach, the round walks within 3 cells (go-to child). In reach it places
  through jobs.lib.tidy/place!, so a block placed in another's zone with :ignore-zones? is recorded for
  jobs.survival.restore-broken.

  Returns an ending with data {:placed true|false :pos :item :reason :text}; a placed block carries :block
  {:name :state} as the place act reported it. Done: :reason :placed, or :already (the cell holds the block, nothing
  done). condition-failed: :occupied (another block fills the cell), :need (the item is missing after the fetch; the fetch's ending is its :because), or a cell reason that appeared during the
  call (the conditions' causes). Impossible: :bad-args (from can-start?). nothing-possible :unreachable (the walk
  failed twice or the place is out of reach, :why).
  nothing-possible: :clear-failed (the plant could not be dug) or :failed (the primitive refused twice in one call: :primitive its status,
  :why its reason text, else one from its status; :mobs the names of mobs it reports in the cell). The caller decides whether to try again.

  :ways (default all): a missing item is fetched with jobs.items.obtain (one of it) from the ways but :carried; #{:carried}
  waits :need.")

(a/defargs args
  {:pos {:doc "the cell to fill, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :item {:doc "the block item to place; nil: the first carried of the call's :use {:blocks}" :spec a/item? :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse, and the walk keeps off seen beds and bottom slabs near the cell; nil: every plan's footprint does" :spec a/name? :default nil}
   :ignore-zones? :shared
   :clear {:doc "dig a plant, flower or snow layer out of the cell first; false places into what a placement overwrites" :spec boolean? :default true}
   :click {:doc "how to place it ({:against [x y z] :cursor [x y z] :yaw :pitch :sneak}, jobs.lib.placement/click); nil: plainly" :spec map? :default nil}
   :facing {:doc "north, south, east or west: the way a facing block (bed head, gate, door, stairs) is oriented; the body looks that way as it places (jobs.lib.placement); not with :click; nil: as the body stands" :spec (a/pred "north, south, east or west" #{"north" "south" "east" "west"}) :default nil}
   :see {:doc "true: place only from where the eye sees the cell (jobs.lib.aim: nothing seen solid in the way, as a player aims at the face it places against), walking to such a stand first (go-to :see; none: :unreachable :out-of-sight); false: within reach is enough" :spec boolean? :default true}})

(def ways need/fetch-ways)

(def safe?
  (ready/ok-when-near (fn [c] (let [{:keys [pos error]} (b/parse (:args c))]
                                (and (not error)
                                     (or (b/in-reach? c pos (:see (:args c)))
                                         (and (torch/light-items (:item (:args c))) (cost/near-body? (:primitives c) pos))))))
                      (ready/safe-at-build (fn [c] (a/cell (:pos (:args c)))))))

(def fit? ready/ok)

(def target [:pos])
(def defaults {:use {:blocks :build} :spend {:job-time ::job-time-s}})

(defn wanted
  "The block names the call places: :item, else the call's :use {:blocks}, else (before the call starts) the carried
  building blocks the default would list."
  [c]
  (vec (if-let [item (:item (:args c))]
         [item]
         (or (ctx/use c :blocks)
             (get-in (:args c) [:use :blocks])
             (choose/default-use c :blocks (get-in defaults [:use :blocks]))))))

(defn parse
  "{:pos :items [names]} of the call, or {:error text}."
  [c]
  (let [{:keys [pos error]} (b/parse (:args c))
        items (wanted c)]
    (cond
      error {:error error}
      (and (:facing (:args c)) (:click (:args c))) {:error "takes :facing or :click, not both"}
      (not (and (seq items) (every? string? items))) {:error "needs :item, or a :use {:blocks [names]} listing one"}
      :else {:pos pos :items items})))

(defn needs
  "What the body lacks: {:item name} or {:any-of [names]} when none of items is carried, else nil."
  [c items]
  (let [carried (set (map :name (u/inventory (:primitives c))))]
    (when-not (some carried items)
      (if (= 1 (count items)) {:item (first items)} {:any-of items}))))

(defn fetching?
  "Whether a missing item is fetched (the call's ways hold more than :carried)."
  [c]
  (boolean (seq (need/fetch-how c))))

(defn clear-args
  "The jobs.blocks.dig args for clearing the plant at pos."
  [c pos]
  (merge (select-keys (:args c) [:for-plan :ignore-zones? :see])
         {:pos pos :collect false :need-drop false :accept #{:fluid-adjacent :falling-block}}))

(defn clears?
  "Whether the call digs block out of the cell before placing (jobs.lib.blocks/clearable, unless :clear is false). An
  invisible light block cannot be dug and is overwritten by the placement."
  [c block]
  (and (:clear (:args c) true) (b/clearable block) (not= "light" block)))

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

(defn boats?
  "Whether items are all boats or rafts (and there is one): it is used on the water, so it needs no block beside the cell."
  [items]
  (boolean (and (seq items) (every? #(re-find #"_(boat|raft)$" %) items))))

(defn problem
  "Why the job cannot run now: a cause map (see doc), or nil. Bad args, a cell already holding the block
  and a block that fills the cell pass: the round ends them. A missing item is no problem while it is fetched."
  [c]
  (let [{:keys [pos items error]} (parse c)]
    (when-not error
      (let [p (:primitives c)
            block (b/target-name p pos "air")
            v (when block (place-verdict c pos items))]
        (cond
          (nil? block) {:reason :not-loaded :pos pos}
          (some #{block} items) nil
          (= :not-loaded (:reason v)) {:reason :not-loaded :pos pos}
          (b/not-allowed pos v) (b/not-allowed pos v)
          (= :own-body (:reason v)) nil
          (= :not-replaceable (:reason v)) nil
          (not (:ok v)) {:reason (:reason v) :pos pos}
          :else (or (when-not (fetching? c) (when-let [n (needs c items)] (assoc n :reason :need :pos pos)))
                    (when-not (or (boats? items) (b/support? p pos) (b/unseen-near? p pos) (b/stale-support? c pos) (and (fetching? c) (nil? (b/chosen c items)))) {:reason :no-support :pos pos})))))))

(defn bad-args [c]
  (if-let [error (:error (parse c))]
    (contract/never :bad-args (str "blocks.place " error))
    (contract/ok :args "a cell and a block to place")))

(def can-start? (cond/and bad-args (checks/refusing :placeable "the cell takes the block" problem)))
(def can-run? (checks/refusing :placeable "the cell still takes the block" #(let [w (problem %)] (when (contains? b/cell-reasons (:reason w)) w))))

(defn done?
  "Ok when the cell already holds one of the wanted blocks; else later."
  [c]
  (let [{:keys [pos items error]} (parse c)
        held (when-not error (some #{(b/target-name (:primitives c) pos "air")} items))]
    (checks/goal (some? held) [:already "the cell holds the block"] [:empty "the cell is to fill"]
                 {:units :blocks :done (if held 1 0) :left (if held 0 1)
                  :result (when held {:placed false :pos pos :item held :reason :already})})))

(defn quote-of [c]
  (let [{:keys [pos items error]} (parse c)
        walk (if (or error (b/in-reach? c pos (:see (:args c)))) 0 (u/eye-dist (u/self-pos c) pos))]
    {:uses (cond-> {:job-time [0.5 (+ 1 (/ walk 4)) (+ 10 walk)]}
             (= 1 (count items)) (assoc :items {(first items) [1 1 1]}))}))

(defn finish!
  "End the call with kind and cause and result, with its text, as the data."
  [c kind cause result]
  (let [text (str (if (:placed result) "placed " "did not place ") (:item result) " at " (pr-str (b/cell (:pos result)))
                  (when-not (:placed result)
                    (str ": " (name (:reason result)) (when (:block result) (str " by " (:block result)))
                         (when (string? (:why result)) (str " (" (:why result) ")"))
                         (when (:walk-text result) (str ": " (:walk-text result))))))]
    (ctx/end c kind cause (assoc result :text text))))

(defn stop!
  "finish! for a place that did not happen and could not: the cause is the result's :reason with what it names."
  [c kind result]
  (finish! c kind (select-keys result [:reason :block :primitive]) result))

(defn ^:async clear!
  "One jobs.blocks.dig call on the plant at pos. Resolves to :cleared, :continue (the dig waits on the world) or the
  ending (nothing-possible :clear-failed)."
  [c pos items]
  (let [e (waits/waiting-out c (await (ctx/call c :clear 'jobs.blocks.dig (clear-args c pos))))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :cleared
      :else (stop! c :nothing-possible {:placed false :pos pos :item (first items) :reason :clear-failed
                                        :block (get-in e [:data :block]) :why (contract/cause-key e)}))))

(defn blocking-leaf
  "The leaves cell the eye's line to pos meets first, when pos is within reach but unseen for that leaf alone (a player
  cuts it, a canopy over a roof cell); nil otherwise, and when :for-plan's own cells include that leaf (it is to be
  placed, not cut)."
  [c pos]
  (when (and (:see (:args c)) (b/in-reach? c pos) (not (b/in-reach? c pos true)))
    (let [at (aim/first-blocker (aim/seen-kind-at (:primitives c)) (aim/body-eye c) (u/cell-vec pos))
          for-plan (:for-plan (:args c))]
      (when (and (some-> at u/cell-pos (->> (u/seen-name (:primitives c))) trees/leaves-name?)
                 (not (and for-plan (= for-plan (get (world/footprints c) (vec at))))))
        (u/cell-pos at)))))

(defn ^:async dig-leaf!
  "One jobs.blocks.dig call on the leaves cell in the way. Resolves to :continue, :again, or nil when the dig was refused
  or failed (the place then walks to a stand that sees the cell)."
  [c leaf]
  (let [e (waits/waiting-out c (await (ctx/call c :leaf 'jobs.blocks.dig (clear-args c leaf))))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :again)))

(defn ^:async dig-through!
  "A walk to a stand that sees pos failed and a seen terrain block (not :for-plan's own cell) lies in the line within reach:
  dig it, as a player does. :continue, :again, or nil when there is none or it could not be dug."
  [c pos]
  (let [hit (b/through-cell c pos)
        for-plan (:for-plan (:args c))]
    (when (and hit (not (and for-plan (= for-plan (get (world/footprints c) (vec hit))))))
      (let [e (waits/waiting-out c (await (ctx/call c :through 'jobs.blocks.dig (assoc (clear-args c (u/cell-pos hit)) :collect true :need-drop true))))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c update :through (fnil conj #{}) hit)
              (when (= :done (:kind e)) :again)))))))

(defn refusal-why
  "Why the primitive refused: its reason text, else one from its status."
  [r]
  (or (.-reason r)
      (case (.-status r)
        "occupied" (str "the cell holds " (or (.-block r) "a block") " that cannot be replaced")
        "no-support" "nothing to place against: no solid block beside the cell the body can click"
        "no-item" "the item is not carried"
        (str "the primitive answered " (.-status r)))))

(defn click-for
  "{:click} of the call's :click, else the one that turns a :facing block (jobs.lib.placement); {:refused kw} when no
  click gives that facing; {} to place plainly. A bed with no :facing takes the first direction its head has room in."
  [c pos item]
  (let [{:keys [click facing]} (:args c)
        p (:primitives c)
        block-at (fn [cell] (some-> (u/seen-block p (zipmap [:x :y :z] cell)) .-name (->> (hash-map :name))))
        eye (placement/eye (u/self-pos c))
        cell [(:x pos) (:y pos) (:z pos)]
        try-facing (fn [f] (select-keys (placement/click {:block item :facing f} cell eye block-at) [:click :refused]))
        bed? (re-find #"_bed$" item)]
    (cond
      click {:click click}
      facing (try-facing facing)
      bed? (let [tries (map try-facing ["north" "south" "east" "west"])]
             (or (first (remove :refused tries)) (first tries)))
      :else {})))

(defn ^:async place!
  "Place the chosen item. Resolves to the ending, or :unreachable (the primitive says out of reach)."
  [c pos items]
  (let [item (b/chosen c items)
        {:keys [click refused]} (click-for c pos item)]
    (if refused
      (stop! c :nothing-possible {:placed false :pos pos :item item :reason :failed
                                  :why (str "no way to place " item (some->> (:facing (:args c)) (str " facing ")) ": " (name refused))})
      (let [r (await (tidy/place! c pos item false (some-> click placement/js-click)))]
        (case (.-status r)
          "placed" (finish! c :done nil (cond-> {:placed true :pos pos :item item :reason :placed}
                                          (.-placed r) (assoc :block {:name (.-name (.-placed r))
                                                                      :state (js->clj (.-properties (.-placed r)) :keywordize-keys true)})))
          "unreachable" :unreachable
          "failed" (if (:retried (ctx/mem c))
                     (stop! c :nothing-possible (cond-> {:placed false :pos pos :item item :reason :failed :primitive (.-status r) :why (refusal-why r)}
                                                  (b/refusing-mobs r) (assoc :mobs (vec (b/refusing-mobs r)))))
                     (do (ctx/update-mem! c assoc :retried true) :again))
          (stop! c :nothing-possible {:placed false :pos pos :item item :reason :failed :primitive (.-status r) :why (refusal-why r)}))))))

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(defn unreachable
  "The nothing-possible :unreachable ending, why a keyword; walk-text is go-to's words for the give-up (the body is shut in...), if any."
  [c pos items why walk-text]
  (finish! c :nothing-possible {:reason :unreachable :why why}
           (cond-> {:placed false :pos pos :item (first items) :reason :unreachable :why why}
             walk-text (assoc :walk-text walk-text))))

(defn ^:async fetch!
  "Get one of the missing items from the ways of the call: :continue, :done (got it) or the ending."
  [c items]
  (let [o {:how (need/fetch-how c)}
        e (await (if (= 1 (count items)) (need/item! c (first items) 1 o) (need/blocks! c items 1 o)))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) :done
          :else e)))

(defn ^:async see-target!
  "b/see-target!, and a look once more at a cell whose floor or walls were last sensed long ago, so no-support is judged on
  what is there now."
  [c pos]
  (if (b/stale-support? c pos)
    (do (b/note-looked! c pos)
        (await (look/look-at! c (b/cell pos))))
    (await (b/see-target! c pos))))

(defn ^:async attempt!
  "One step of the place: fetch the item if due, clear a plant, walk into reach, or place. A walk that fails or leaves
  the cell out of reach is tried once more, then the call ends :unreachable. :again, :continue or the ending."
  [c pos items]
  (let [block (b/target-name (:primitives c) pos "air")
        full (occupied-by c pos items)
        already (some #{block} items)
        lacking? (and (not full) (not already) (nil? (b/chosen c items)))
        fetched (when (and lacking? (fetching? c)) (await (fetch! c items)))
        leaf (when-not (or full already fetched (clears? c block)) (blocking-leaf c pos))
        dug (when leaf (await (dig-leaf! c leaf)))]
    (cond
      full (stop! c :condition-failed {:placed false :pos pos :item (first items) :reason :occupied :block full})
      already (finish! c :done nil {:placed false :pos pos :item block :reason :already})
      (= :done fetched) :again
      fetched fetched
      (clears? c block)
      (let [w (await (clear! c pos items))]
        (if (= :cleared w) :again w))
      dug dug
      (not (b/in-reach? c pos (:see (:args c))))
      (let [w (await (b/walk-into-reach! c pos (:see (:args c))))]
        (if (:unreachable w)
          (or (await (dig-through! c pos)) (unreachable c pos items (:unreachable w) (:text w)))
          w))
      (= :own-body (:reason (place-verdict c pos items)))
      (let [r (await (step-off/step-off-zoned! c pos {}))]
        (cond (= :continue r) :continue
              (:unreachable r) (unreachable c pos items (:unreachable r) nil)
              :else :again))
      lacking? (finish! c :condition-failed (assoc (needs c items) :reason :need :pos pos)
                        {:placed false :pos pos :item (first items) :reason :need})
      :else
      (let [d (await (place! c pos items))]
        (if (= :unreachable d)
          (unreachable c pos items :out-of-reach nil)
          d)))))

(defn ^:async look-first!
  "A look at the cell, when due, is a step of its own, so can-run? judges what it shows (no support) before anything is placed."
  [c pos step]
  (if (or (b/to-see? c pos) (b/stale-support? c pos))
    (do (await (see-target! c pos)) :again)
    (await (step))))

(defn ^:async round [c]
  (let [{:keys [pos items]} (parse c)]
    (when-not (= pos (:for (ctx/mem c))) (ctx/update-mem! c b/fresh-mem pos))
    (await (pace/steps! c #(look-first! c pos (fn [] (attempt! c pos items)))))))
