(ns jobs.items.obtain
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.blocks :as b]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.look :as look]
            [jobs.items.recipes :as recipes]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.items.craft :as craft]
            [jobs.lib.storage :as storage]
            [engine.game :as game]
            [jobs.lib.trees :as trees]
            [clojure.string]))

(def doc
  "Get :count more of :item (or of any of :any-of, in that order of preference) than were carried at the first round.
  The target is booked once, so a cut or restart goes on toward the same count. One call is the whole attempt: it yields (:continue) only while a child waits on the world.

  Sources, in order (carried, chests, crafting, gathering):
  - Carried: the target is met, done.
  - :chest (in :how): the containers the body has seen (perception's seenBlocks, never x-ray) within 32 blocks,
    still there, whose zone or claim allows :take (an open, unzoned chest does; jobs.lib.access). A chest known
    to hold a wanted name (body memory :fetch/stock) is withdrawn from (jobs.storage.withdraw child :take); one of
    unknown stock is walked to and looked into, nearest first, at most 4 per obtain; one known not to hold any is
    skipped. Each chest is withdrawn from once. With none usable seen at the first round, one look around first.
  - :craft (in :how): a chain of crafts planned from the recipes (jobs.items.recipes) over what is carried: logs to
    planks to sticks to the tool, and a crafting table when the chain needs one and none is seen (crafted, then put
    down on a free cell beside the body with a jobs.blocks.place child). Each step is a jobs.items.craft child. The
    names are tried in order; the first with a plan is made. Three fruitless steps stop it (:tried :craft).
  - :gather (in :how): coal comes from a plain or deepslate ore seen; dirt is dug from seen dirt or grass; every raw item a chain lacks must be seen before it starts. A sapling (no recipe) comes from the leaves of its tree the body has seen, broken by
    jobs.gather.get-seeds (at most 60 leaves, one run; the drop is picked up). What a craft chain lacks that has no recipe (logs, coal, stone-tool material; recipes/gatherable?, stone-materials) is
    felled or mined as one child per round, only what the body has seen: logs by jobs.forestry.harvest-wood,
    cobblestone and coal by jobs.gather.mine (its pickaxe only from a chest or a craft, never gathered: no mine inside a mine's fetch, so no loop back into obtain). The crafts follow once the chain is whole (a chain that is craftable now
    is crafted first). A child that brings in nothing three times stops it (:tried :gather). A stone material or coal
    (seen, or with :explore) with no pickaxe carried gets a wooden pickaxe first (child, slot :pickaxe).
    With :explore, an unseen raw need is searched for (jobs.explore.search child, slot :explore, at most 3 per need;
    a notice :obtain.exploring says so); none found stops it (:tried {:gather {:searched ..}}).
  - Nothing else: stopped :no-source with :tried.

  The check waits {:reason :no-source :why text :item|:any-of} (:why: what each source lacks) before the first round (once the round has looked around, when chests are allowed) when nothing is carried enough, no
  usable chest that might hold it is seen and no craft chain makes it from what is carried, and nothing seen to gather what a chain lacks (with :explore, a chain whose raw needs are unseen passes: the search child solves it). Ends {:status :done :got n :item name} or {:status :stopped
  :reason r :got n :tried {..}}, r :no-source, :cycle (a wanted name is on :chain), :timeout (:minutes from the first
  round) or :bad-args. :depth is the nesting left for sources that need fetches of their own (not used yet: crafting
  plans its whole chain; a gather child fetches its own tools).")

(a/defargs args
  {:item {:doc "the item to get" :spec a/item? :default nil}
   :any-of {:doc "items, any one will do, the first preferred (instead of :item)" :spec (a/coll-of a/item?) :default nil}
   :count {:doc "how many more than carried at the start (counts above 64 are taken as 64)" :spec (a/int-in 1 nil) :default 1}
   :how {:doc "sources to use, a subset of #{:chest :craft :gather}; nil: all" :spec (a/coll-of #{:chest :craft :gather}) :default nil}
   :depth {:doc "nested fetches left; nil: the fetch limits (jobs.lib.fetch)" :spec (a/int-in 0 nil) :default nil}
   :minutes {:doc "time budget from the first round; nil: the fetch limits" :spec (a/num-in 0 nil) :default nil}
   :fail-minutes {:doc "passed on to nested fetches; nil: the fetch limits" :spec (a/num-in 0 nil) :default nil}
   :chain {:doc "items being fetched above this one (a cycle stops)" :spec (a/coll-of a/item?) :default []}
   :explore {:doc "walk and look for a raw need not seen: false, true (the ::explore setting) or jobs.explore.search options over it" :spec (a/or-of boolean? map?) :default false}})

(a/defargs settings
  {::max-inspections {:default 4 :doc "Looks at a place before the search for it gives up." :spec (a/int-in 0 nil)}
   ::craft-radius {:default 32 :doc "Blocks within which a crafting table is looked for." :spec (a/int-in 0 nil)}
   ::max-fruitless {:default 3 :doc "Rounds without progress before the job gives up." :spec (a/int-in 0 nil)}
   ::sheep-radius {:default 24 :doc "Blocks within which sheep are looked for when wool is gathered." :spec (a/num-in 0 nil)}
   ::explore {:default {:max-distance 64 :max-legs 12 :timeout-s 300} :doc "jobs.explore.search options of a search for a raw need (:explore true)." :spec map?}})
(defn max-inspections [] (settings/get settings ::max-inspections))
(defn sheep-radius [] (settings/get settings ::sheep-radius))

(defn names-of [{:keys [item any-of]}]
  (vec (if item [item] any-of)))

(defn wanted-fields
  "{:item name} or {:any-of [..]} of the args, for reasons and results."
  [a]
  (into {} (remove (comp nil? val)) (select-keys a [:item :any-of])))

(defn args-error [a]
  (let [names (names-of a)]
    (cond
      (not (and (seq names) (every? string? names))) "needs :item, or :any-of [names]"
      (not (and (int? (:count a)) (pos? (:count a)))) ":count is a positive whole number")))

(defn carried-of [inv names]
  (reduce + 0 (map #(storage/carried inv %) names)))

(defn limits [c]
  (fetch/merge-limits 'jobs.items.obtain nil (fetch/body-limits (ctx/view c)) (:args c)))

(defn holds [items names]
  (first (filter #(pos? (get items % 0)) names)))

(defn candidates
  "Usable seen chests not yet withdrawn from or given up: [{:pos :stock items-or-nil}]."
  [c names]
  (let [stock (fetch/stock-of (ctx/view c))
        done (:done-chests (ctx/mem c) #{})]
    (keep (fn [pos]
            (let [cell (fetch/cell-of pos)
                  items (get stock cell)]
              (when-not (or (done cell) (and items (not (holds items names))))
                {:pos pos :stock items})))
          (fetch/usable-chests c))))

(defn craft-radius [] (settings/get settings ::craft-radius))

(defn carried-counts [p]
  (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} (u/inventory p)))

(defn craft-plan
  "{:item name :steps [..]} for the first of names a craft chain makes n more of from what is carried, else nil."
  [c names n]
  (let [p (:primitives c)
        have (carried-counts p)
        mem (ctx/mem c)
        table? (and (not (get-in mem [:craft :table-unreachable]))
                    (boolean (or (:table mem) (craft/nearest-table p (craft-radius)))))
        version (game/version-of p)]
    (some (fn [name]
            (when-let [pl (recipes/plan version have name n {:table? table?})]
              (assoc pl :item name)))
          names)))

(defn leaves-of
  "The leaf block that drops this sapling (oak_sapling: oak_leaves; mangrove_propagule: mangrove_leaves), or nil."
  [name]
  (cond
    (clojure.string/ends-with? name "_sapling") (str (subs name 0 (- (count name) (count "_sapling"))) "_leaves")
    (= "mangrove_propagule" name) "mangrove_leaves"))

(defn material-blocks
  "The blocks mining which gives a stone-material (stone: cobblestone), or nil for any other item. A block
  that drops itself (blackstone) is its own."
  ([item] (material-blocks game/default-version item))
  ([version item]
   (when (contains? (recipes/stone-materials version) item)
    (let [bs (set (for [[b d] b/drop-item :when (= d item)] b))]
      (if (seq bs) bs #{item})))))

(def dirt-blocks
  "The seen blocks digging which by hand gives dirt (grass drops it)."
  #{"dirt" "grass_block"})

(defn wool? [name] (clojure.string/ends-with? name "_wool"))

(defn wool-color [name] (subs name 0 (- (count name) (count "_wool"))))

(defn sheep-in-range
  "The adult unsheared sheep within the sheep radius, as JS entities."
  [c]
  (remove #(or (true? (.-baby %)) (true? (.-sheared %))) (animals/herd (:primitives c) "sheep" (sheep-radius))))

(def wool-keep "Sheep of a colour combat.hunt spares (its :keep default for animals); a colour with no more is no source." 2)

(defn pick-plan
  "Of the gather plans (one per wanted name) the one that gathers the least wool, so the colour the body carries
  most of; a plan whose sheep are not in range comes after one whose are. {:wool-fallback {:wanted :taking}} on
  the plan when the colour the body carries most of has no sheep in range."
  [c plans]
  (let [wool-of (fn [pl] (into {} (filter (comp wool? key)) (:gather pl)))]
    (if-not (some (comp seq wool-of) plans)
      (first plans)
      (let [sorted (sort-by #(reduce + 0 (vals (wool-of %))) plans)
            colors (set (keep (fn [[k n]] (when (> n wool-keep) k)) (frequencies (keep #(.-color %) (sheep-in-range c)))))
            colors-of (fn [pl] (map wool-color (keys (wool-of pl))))
            ok? (fn [pl] (every? colors (colors-of pl)))
            chosen (or (first (filter ok? sorted)) (first sorted))
            wanted (first sorted)]
        (cond-> chosen
          (and (not= chosen wanted) (seq (wool-of chosen)) (seq (wool-of wanted)))
          (assoc :wool-fallback {:wanted (first (colors-of wanted)) :taking (first (colors-of chosen))}))))))

(defn gather-plan
  "The craft chain for the first of names that has one once the raw items it lacks are gathered, with :gather
  {name n} when it lacks any, else nil. A sapling has no recipe: its plan is just :gather {name n}, when its leaves are seen (or its child is running).
  Of several names that lack wool, the one gathering the least (pick-plan)."
  [c names n]
  (let [p (:primitives c)
        have (carried-counts p)
        version (game/version-of p)
        seen? #(seq (look/seen-blocks p {:names (vec (material-blocks version %)) :radius 16 :max 1}))
        seen (set (filter seen? (recipes/stone-materials version)))
        materials (if (seq seen) seen (recipes/stone-materials version))
        plan-of (fn [name]
                  (if-let [seen-of-name (cond (= "dirt" name) dirt-blocks
                                              (leaves-of name) #{(leaves-of name)})]
                    (when (or (= name (get-in (ctx/mem c) [:gather :item]))
                              (seq (look/seen-blocks p {:names (vec seen-of-name) :radius 16 :max 1})))
                      {:steps [] :gather {name n} :item name})
                    (when-let [pl (recipes/plan version have name n {:table? (boolean (or (:table (ctx/mem c)) (craft/nearest-table p (craft-radius))))
                                                                 :gather? true :materials materials})]
                      (assoc pl :item name))))]
    (if (= 1 (count names))
      (plan-of (first names))
      (pick-plan c (keep plan-of names)))))

(defn log? [name] (clojure.string/ends-with? name "_log"))

(def sapling-leaf-limit
  "Leaves broken for one sapling at most (a drop is about 1 in 20); one run, no retries."
  60)

(def mine-fetch
  "The :fetch of a gather mine: its pickaxe from a chest or a craft only, so a mine never starts another gather."
  {:what #{:tool} :how #{:chest :craft}})

(defn gather-needs
  "The raw needs of a plan's :gather map, logs first: [{:key k :count n :job sym :args {..} :seen fn of a block name: what must have been seen}]. A new gather source
  is one more case here. Coal is mined from a plain or a deepslate ore: :block is set by with-block."
  ([gather] (gather-needs gather game/default-version))
  ([gather version]
  (let [logs (reduce + 0 (for [[k v] gather :when (log? k)] v))]
    (vec (concat
          (for [[sapling n] gather :when (leaves-of sapling)]
            {:key sapling :count n :job 'jobs.gather.get-seeds :seen #{(leaves-of sapling)} :max-runs 1
             :args {:item sapling :count n :sources [(leaves-of sapling)] :dry-digs sapling-leaf-limit}})
          (for [[k n] gather :when (wool? k)]
            {:key k :count n :job 'jobs.combat.hunt :sheep (wool-color k)
             :args {:mob "sheep" :color (wool-color k) :count n :radius (sheep-radius)}})
          (when (pos? logs) [{:key "log" :count logs :job 'jobs.forestry.harvest-wood :seen log? :args {:fetch #{:item}}}])
          (for [[k n] gather :when (material-blocks version k)]
            (let [blocks (material-blocks version k)]
              {:key k :count n :job 'jobs.gather.mine :seen blocks
               :args {:block (first (sort blocks)) :item k :count n :fetch mine-fetch}}))
          (when-let [n (get gather "dirt")]
            [{:key "dirt" :count n :job 'jobs.gather.mine :seen dirt-blocks
              :args {:block "dirt" :item "dirt" :count n :fetch mine-fetch}}])
          (when-let [n (get gather "coal")]
            [{:key "coal" :count n :job 'jobs.gather.mine :seen #{"coal_ore" "deepslate_coal_ore"}
              :args {:block "coal_ore" :item "coal" :count n :fetch mine-fetch}}]))))))

(defn gather-need
  "The first of gather-needs."
  ([gather] (first (gather-needs gather)))
  ([gather version] (first (gather-needs gather version))))

(defn gather-carried [p {:keys [key]}]
  (reduce + 0 (for [[k v] (carried-counts p) :when (if (= "log" key) (log? k) (= key k))] v)))

(defn seen-of
  "The nearest seen block of a need (within 16), or for a wool need a sheep of its colour in range, else nil."
  [c need]
  (if-let [color (:sheep need)]
    (first (filter #(= color (.-color %)) (sheep-in-range c)))
    (first (look/seen-blocks (:primitives c) {:match #((:seen need) %) :radius 16 :max 1}))))

(defn with-block
  "The need with :block of its args the kind of block seen when it could be either (a coal ore, dirt or grass); a wool need shears when shears are carried, else hunts."
  [c need]
  (cond
    (and (:sheep need) (pos? (storage/carried (u/inventory (:primitives c)) "shears")))
    (assoc need :job 'jobs.animals.shear :args {:color (:sheep need) :radius (sheep-radius)})

    :else
    (if-let [name (when (#{"coal" "dirt"} (:key need)) (:name (seen-of c need)))]
      (assoc-in need [:args :block] name)
      need)))

(defn with-count
  "The need with the logs to carry in all (harvest-wood's :count) in its args: those carried and those it lacks."
  [p need]
  (if (= "log" (:key need))
    (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    need))

(defn gather-slot
  "The child slot of a gather need: one per source, since a slot's memory outlives its job while it runs and another job must not start over it."
  [need]
  (keyword (str "gather-" (:key need))))

(defn gather-viable?
  "Whether a chain is craftable once raw items are gathered, and a block for every raw need has been seen and
  the child of the first would run."
  [c names n]
  (when-let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))) seq)]
    (and (every? #(seen-of c %) needs)
         (let [need (with-block c (first needs))]
           (boolean (ctx/check-child c (gather-slot need) (:job need) (:args need)))))))

(defn explore-opts
  "The search options of :explore, or nil when it is off."
  [c]
  (let [e (:explore (:args c))]
    (cond (map? e) (merge (settings/get settings ::explore) e)
          e (settings/get settings ::explore))))

(defn search-names
  "The block names a search for a need looks for."
  [need]
  (if (set? (:seen need)) (vec (sort (:seen need))) trees/log-names))

(defn explorable?
  "Whether :explore is on and a raw need of the chain is not seen: the search child solves it."
  [c names n]
  (boolean (and (explore-opts c)
                (some #(and (not (:sheep %)) (not (seen-of c %)))
                      (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))))))))

(defn unseen-why
  "Text for the gather source: the raw needs of the chain whose blocks are not seen, or the general text."
  [c names n]
  (let [unseen (->> (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))))
                    (remove #(seen-of c %)))]
    (str "nothing seen to gather what a craft lacks"
         (when (seq unseen)
           (str ": " (clojure.string/join ", " (for [{:keys [key seen sheep]} unseen]
                                                  (if sheep (str key " (from " sheep " sheep within " (sheep-radius) " blocks)") (if (set? seen) (str key " (from " (clojure.string/join "/" (sort seen)) ")") key)))))))))

(defn pickaxe-why
  "For a stone material wanted with no pickaxe carried: that it needs one, and what crafting the cheapest lacks; else nil."
  [version have names]
  (when (and (some #(material-blocks version %) names)
             (not-any? (fn [[k v]] (and (pos? v) (clojure.string/ends-with? k "_pickaxe"))) have))
    (str "mining it needs a pickaxe, none carried; " (recipes/lacking version have "wooden_pickaxe" 1))))

(defn no-source-why
  "Text for the no-source wait: what each allowed source lacks."
  [c names n]
  (let [how (:how (limits c))
        have (carried-counts (:primitives c))
        version (game/version-of (:primitives c))]
    (clojure.string/join
     "; "
     (concat (when (contains? how :chest) [(str "no seen chest that may hold " (clojure.string/join "/" names))])
             (when (contains? how :craft) [(recipes/lacking version have (first names) n)])
             (when (contains? how :gather) [(unseen-why c names n)])
             (when-let [w (and (contains? how :gather) (pickaxe-why version have names))] [w])))))

(defn check [c]
  (let [a (:args c)
        names (names-of a)]
    (cond
      (args-error a) true
      (:start (ctx/mem c)) true
      (and (contains? (:how (limits c)) :chest) (seq (candidates c names))) true
      (and (contains? (:how (limits c)) :craft) (craft-plan c names (:count a))) true
      (and (contains? (:how (limits c)) :gather) (gather-viable? c names (:count a))) true
      (and (contains? (:how (limits c)) :gather) (explorable? c names (:count a))) true
      :else (let [reason (merge {:reason :no-source :why (no-source-why c names (:count a))} (wanted-fields a))]
              (if (contains? (:how (limits c)) :chest)
                (look/wait-unless-surveyed c reason)
                (ctx/wait c reason))))))

(defn survey-first?
  "Before the first round, with chests allowed, none usable seen that might hold one of names and no look around yet:
  the round looks around first, so a chest behind the body is preferred to a craft or gather."
  [c names]
  (and (not (:start (ctx/mem c)))
       (not (look/surveyed? c))
       (contains? (:how (limits c)) :chest)
       (empty? (candidates c names))))

(defn stop! [c reason extra]
  (let [a (:args c)
        res (merge {:status :stopped :reason reason :got (:got extra 0)} (wanted-fields a) extra)]
    (ctx/result! c res)
    :done))

(defn tried! [c k v] (ctx/update-mem! c assoc-in [:tried k] v) :again)

(defn ^:async withdraw! [c pos items names have]
  (let [name (holds items names)
        need (- (:target (:start (ctx/mem c))) have)
        r (await (ctx/call-child c :take 'jobs.storage.withdraw
                                 {:chest pos :items {name (+ (storage/carried (u/inventory (:primitives c)) name) need)}}))]
    (when (#{:done :declined} r)
      (ctx/update-mem! c update :done-chests (fnil conj #{}) (fetch/cell-of pos)))
    (if (= :continue r) :continue :again)))

(defn ^:async inspect!
  "Walk to within 3 of the chest (a go-to child), look into it and book its stock. :continue while go-to waits, else :again."
  [c pos]
  (let [cell (fetch/cell-of pos)
        mark! #(ctx/update-mem! c (fn [m] (-> m (update :done-chests (fnil conj #{}) cell) (update :inspected (fnil inc 0)))))
        w (when-not (u/within? (u/self-pos c) pos 3)
            (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range 3 :escalate false :warn false :retry false :zone-tolls true})))]
    (cond
      (= :continue w) :continue
      (and w (not (:arrived (ctx/child-result c :walk)))) (do (mark!) :again)
      :else
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))]
        (if (= "ok" (.-status seen))
          (do (fetch/note-stock! c pos (.-items seen))
              (ctx/update-mem! c update :inspected (fnil inc 0)))
          (mark!))
        :again))))

(defn max-fruitless [] (settings/get settings ::max-fruitless))

(defn fruitless! [c why]
  (ctx/update-mem! c update-in [:craft :fruitless] (fnil inc 0))
  (ctx/update-mem! c assoc-in [:craft :why] why)
  :again)

(defn ^:async craft-step!
  "One round of the craft source: run the current step of the plan as a child, until it ends."
  [c names have target]
  (let [mem (:craft (ctx/mem c))
        step (or (:step mem)
                 (some-> (craft-plan c names (- target have)) :steps first))]
    (cond
      (>= (:fruitless mem 0) (max-fruitless))
      (tried! c :craft (or (:why mem) :failed))

      ;; no plan without the seen table: try that table again (it may be reachable now); fruitless counts bound the retries
      (and (nil? step) (:table-unreachable mem))
      (do (ctx/update-mem! c update :craft dissoc :table-unreachable)
          (fruitless! c :table-unreachable))

      (nil? step)
      (tried! c :craft :no-plan)

      (= :place (:op step))
      (let [spot (or (:spot mem) (await (craft/spot! c)))]
        (cond
          (= :continue spot) :continue
          (not spot) (fruitless! c :no-table-spot)
          :else
          (do (ctx/update-mem! c assoc-in [:craft :step] step)
              (ctx/update-mem! c assoc-in [:craft :spot] spot)
              (let [r (await (ctx/call-child c :place 'jobs.blocks.place {:item (:item step) :pos spot :fetch false}))]
                (if (= :continue r)
                  :continue
                  (let [res (ctx/child-result c :place)]
                    (ctx/update-mem! c update :craft dissoc :step :spot)
                    (if (and (= :done r) (:placed res))
                      (do (ctx/update-mem! c assoc :table spot)
                          (ctx/update-mem! c update :craft dissoc :table-unreachable)
                          :again)
                      (fruitless! c (or (:reason res) :place-declined)))))))))

      :else
      (do (ctx/update-mem! c assoc-in [:craft :step] step)
          (let [r (await (ctx/call-child c :craft 'jobs.items.craft (cond-> {:item (:item step) :count (:count step) :fetch false}
                                                                        (and (:table? step) (:table (ctx/mem c))) (assoc :table (:table (ctx/mem c))))))]
            (if (= :continue r)
              :continue
              (let [res (ctx/child-result c :craft)]
                (ctx/update-mem! c update :craft dissoc :step)
                (cond
                  (and (= :done r) (pos? (:made res 0))) :again

                  ;; the seen table cannot be reached: plan again without it, so a carried or new table is put down
                  (and (#{"unreachable" "no-table"} (:reason res)) (not (:table-unreachable mem)))
                  (do (ctx/update-mem! c assoc-in [:craft :table-unreachable] true) :again)

                  :else
                  (fruitless! c (or (:reason res) (when (:short res) {:short (:short res)}) :declined))))))))))

(defn fruitless-limit
  "Fruitless runs that end the gather source: the need's :max-runs, but only for a run that happened; a child that
  was declined (:declined: it could not start) broke nothing and counts against the default."
  [need r]
  (if (= :declined r) (max-fruitless) (:max-runs need (max-fruitless))))

(defn gather-failed! [c need why r]
  (let [k (inc (get-in (ctx/mem c) [:gather :fruitless] 0))]
    (ctx/update-mem! c assoc-in [:gather :fruitless] k)
    (when (>= k (fruitless-limit need r)) (tried! c :gather (or why :failed)))
    :again))

(defn pickaxe-first?
  "Whether need is mined (dirt is dug by hand), no pickaxe is carried and a wooden one may be crafted (not one being fetched above)."
  [c names need]
  (let [a (:args c)]
    (and (= 'jobs.gather.mine (:job need))
         (not= "dirt" (:key need))
         (contains? (:how (limits c)) :craft)
         (not-any? #{"wooden_pickaxe"} (concat names (:chain a)))
         (not-any? (fn [[k v]] (and (pos? v) (clojure.string/ends-with? k "_pickaxe"))) (carried-counts (:primitives c))))))

(defn ^:async pickaxe-step!
  "Get a wooden pickaxe for the mine (a jobs.items.obtain child): :again once it is carried, else the gather source fails."
  [c names]
  (let [a (:args c)
        o (limits c)
        r (await (ctx/call-child c :pickaxe 'jobs.items.obtain
                                 {:any-of ["wooden_pickaxe"] :count 1 :how (:how o) :depth (max 0 (dec (:depth o)))
                                  :minutes (:minutes o) :fail-minutes (:fail-minutes o) :chain (into (vec (:chain a)) names)
                                  :explore (:explore a)}))]
    (cond
      (= :continue r) :continue
      (pickaxe-first? c names {:job 'jobs.gather.mine})
      (tried! c :gather {:pickaxe (or (:reason (ctx/child-result c :pickaxe)) r)})
      :else :again)))

(defn ^:async explore-step!
  "Walk and look for need (a jobs.explore.search child): :again once it is seen; a search that finds none stops the
  gather source, as does the third search for one need."
  [c need]
  (let [targets (search-names need)
        o (explore-opts c)
        here (.-pos (.self (:primitives c)))
        _ (ctx/emit! c :obtain.exploring :info
                     {:message (str "nothing seen to get " (clojure.string/join "/" targets) ": walking to look for it, up to " (:max-distance o)
                                    " blocks out and " (:max-legs o) " legs, from " (js/Math.floor (.-x here)) " "
                                    (js/Math.floor (.-y here)) " " (js/Math.floor (.-z here)))
                      :attention :notice
                      :for targets :max-distance (:max-distance o) :max-legs (:max-legs o)
                      :from (mapv #(js/Math.floor %) [(.-x here) (.-y here) (.-z here)])})
        r (await (ctx/call-child c :explore 'jobs.explore.search
                                 (merge (explore-opts c) {:target targets :count 1 :scan-radius 16 :use-notes false})))]
    (if (= :continue r)
      :continue
      (let [res (ctx/child-result c :explore)
            k (inc (get-in (ctx/mem c) [:explored (:key need)] 0))]
        (ctx/update-mem! c assoc-in [:explored (:key need)] k)
        (if (and (= :found (:reason res)) (seen-of c need))
          :again
          (tried! c :gather (cond-> {:searched (:key need) :for targets :result (or (:reason res) r)}
                              (:why res) (assoc :why (:why res))
                              (:coverage res) (assoc :legs (:legs (:coverage res)) :farthest (:farthest (:coverage res))))))))))

(defn ^:async gather-step!
  "One round of the gather source: the child for the first raw item the chain lacks, until it ends."
  [c names have target]
  (let [p (:primitives c)
        pl (gather-plan c names (- target have))
        need (some->> (some-> pl :gather (gather-need (game/version-of p))) (with-block c) (with-count p))]
    (cond
      (nil? need) (tried! c :gather :no-plan)
      (and (not (get-in (ctx/mem c) [:gather :before])) (pickaxe-first? c names need) (or (explore-opts c) (seen-of c need)))
      (await (pickaxe-step! c names))
      (and (not (get-in (ctx/mem c) [:gather :before])) (explore-opts c) (not (:sheep need)) (not (seen-of c need))
           (< (get-in (ctx/mem c) [:explored (:key need)] 0) (max-fruitless)))
      (await (explore-step! c need))
      (and (not (get-in (ctx/mem c) [:gather :before]))
           (not (if (explore-opts c)
                  (and (seen-of c need) (ctx/check-child c (gather-slot need) (:job need) (:args need)))
                  (gather-viable? c names (- target have)))))
      (tried! c :gather :none-seen)
      :else
      (do (when-not (get-in (ctx/mem c) [:gather :before])
            (ctx/update-mem! c assoc-in [:gather :before] (gather-carried p need))
            (ctx/update-mem! c assoc-in [:gather :item] (:key need))
            (when-let [{:keys [wanted taking]} (:wool-fallback pl)]
              (ctx/emit! c :obtain.wool-colour :info
                         {:message (str "no " wanted " sheep within " (sheep-radius) " blocks; taking " taking " wool instead")
                          :wanted wanted :taking taking})))
          (let [r (await (ctx/call-child c (gather-slot need) (:job need) (:args need)))]
            (if (= :continue r)
              :continue
              (let [res (ctx/child-result c (gather-slot need))
                    before (get-in (ctx/mem c) [:gather :before])]
                (ctx/update-mem! c update :gather dissoc :before :item)
                (if (> (gather-carried p need) before)
                  (do (ctx/update-mem! c assoc-in [:gather :fruitless] 0) :again)
                  (gather-failed! c need (or (:reason res) :nothing-gathered) r)))))))))

(defn ^:async step! [c]
  (let [a (:args c)
        names (names-of a)
        inv (u/inventory (:primitives c))
        have (carried-of inv names)
        now (ctx/now c)]
    (if-let [e (args-error a)]
      (do (ctx/emit! c :obtain.declined :warn {:reason :bad-args :text (str "items.obtain " e)})
          (stop! c :bad-args {:why e}))
      (if (survey-first? c names)
        (do (await (look/survey! c)) (if (check c) :again :continue))
        (let [_ (when-not (:start (ctx/mem c))
                  (ctx/update-mem! c update :craft dissoc :table-unreachable)
                  (ctx/update-mem! c assoc :start {:have have :target (+ have (min 64 (:count a))) :t now}))
              {:keys [target t] :as start} (:start (ctx/mem c))
              got (max 0 (- have (:have start)))
              o (limits c)
              m (ctx/mem c)]
          (cond
            (some (set (:chain a)) names) (stop! c :cycle {:chain (:chain a)})
            (>= have target) (do (ctx/result! c {:status :done :got got :item (or (holds (into {} (map (juxt :name :count)) inv) names) (first names))})
                                 :done)
            (> (- now t) (* 60000 (:minutes o))) (stop! c :timeout {:got got :tried (:tried m)})
            (and (contains? (:how o) :chest) (not (get-in m [:tried :chest])))
            (let [cs (candidates c names)
                  known (first (filter :stock cs))
                  unknown (first (remove :stock cs))]
              (cond
                known (await (withdraw! c (:pos known) (:stock known) names have))
                (and unknown (< (:inspected m 0) (max-inspections))) (await (inspect! c (:pos unknown)))
                :else (tried! c :chest (if (or (seq (:done-chests m)) (pos? (:inspected m 0))) :lacking :none-seen))))
            (and (contains? (:how o) :craft) (not (get-in m [:tried :craft]))
                 (or (not (contains? (:how o) :gather)) (get-in m [:craft :step]) (get-in m [:craft :table-unreachable]) (craft-plan c names (- target have))))
            (await (craft-step! c names have target))
            (and (contains? (:how o) :gather) (not (get-in m [:tried :gather])))
            (await (gather-step! c names have target))
            :else (stop! c (if (= :table-unreachable (get-in m [:tried :craft])) :table-unreachable :no-source)
                         {:got got :tried (:tried m)})))))))

(defn ^:async round
  "The whole attempt in one call: step! again until the count is carried or it stops; :continue only while a child waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async obtain-step [] (await (step! c))))))
