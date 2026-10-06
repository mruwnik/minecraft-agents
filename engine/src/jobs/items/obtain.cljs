(ns jobs.items.obtain
  (:require [engine.ctx :as ctx]
            [engine.foods :as foods]
            [engine.jobs.blocks :as b]
            [engine.jobs.fetch :as fetch]
            [engine.jobs.recipes :as recipes]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [jobs.items.craft :as craft]
            [jobs.storage.deposit :as deposit]))

(def doc
  "Get :count more of :item (or of any of :any-of, in that order of preference) than were carried at the first round.
  The target is booked once, so a cut or restart goes on toward the same count. One child call or act per round.

  Sources, in order (carried, chests and crafting; gathering comes later):
  - Carried: the target is met, done.
  - :chest (in :how): the containers the body has seen (perception's seenBlocks, never x-ray) within 32 blocks,
    still there, whose zone or claim allows :take (an open, unzoned chest does; engine.jobs.access). A chest known
    to hold a wanted name (body memory :fetch/stock) is withdrawn from (jobs.storage.withdraw child :take); one of
    unknown stock is walked to and looked into, nearest first, at most 4 per obtain; one known not to hold any is
    skipped. Each chest is withdrawn from once.
  - :craft (in :how): a chain of crafts planned from the recipes (engine.jobs.recipes) over what is carried: logs to
    planks to sticks to the tool, and a crafting table when the chain needs one and none is seen (crafted, then put
    down on a free cell beside the body with a jobs.blocks.place child). Each step is a jobs.items.craft child. The
    names are tried in order; the first with a plan is made. Three fruitless steps stop it (:tried :craft).
  - Nothing else: stopped :no-source with :tried.

  The check waits {:reason :no-source :item|:any-of} before the first round when nothing is carried enough, no
  usable chest that might hold it is seen (no exploring) and no craft chain makes it from what is carried. Ends {:status :done :got n :item name} or {:status :stopped
  :reason r :got n :tried {..}}, r :no-source, :cycle (a wanted name is on :chain), :timeout (:minutes from the first
  round) or :bad-args. :depth is the nesting left for sources that need fetches of their own (not used yet: crafting
  plans its whole chain).")

(def args
  {:item {:doc "the item to get" :default nil}
   :any-of {:doc "items, any one will do, the first preferred (instead of :item)" :default nil}
   :count {:doc "how many more than carried at the start, at most 64" :default 1}
   :how {:doc "sources to use, a subset of #{:chest :craft :gather}; nil: all" :default nil}
   :depth {:doc "nested fetches left; nil: the fetch limits (engine.jobs.fetch)" :default nil}
   :minutes {:doc "time budget from the first round; nil: the fetch limits" :default nil}
   :fail-minutes {:doc "passed on to nested fetches; nil: the fetch limits" :default nil}
   :chain {:doc "items being fetched above this one (a cycle stops)" :default []}})

(def max-inspections 4)

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
  (reduce + 0 (map #(deposit/carried inv %) names)))

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

(def craft-radius 32)

(defn carried-counts [p]
  (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} (u/inventory p)))

(defn craft-plan
  "{:item name :steps [..]} for the first of names a craft chain makes n more of from what is carried, else nil."
  [c names n]
  (let [p (:primitives c)
        have (carried-counts p)
        table? (boolean (or (:table (ctx/mem c)) (craft/nearest-table p craft-radius)))
        version (foods/version-of p)]
    (some (fn [name]
            (when-let [pl (recipes/plan version have name n {:table? table?})]
              (assoc pl :item name)))
          names)))

(defn check [c]
  (let [a (:args c)
        names (names-of a)]
    (cond
      (args-error a) true
      (:start (ctx/mem c)) true
      (and (contains? (:how (limits c)) :chest) (seq (candidates c names))) true
      (and (contains? (:how (limits c)) :craft) (craft-plan c names (:count a))) true
      :else (ctx/wait c (merge {:reason :no-source} (wanted-fields a))))))

(defn stop! [c reason extra]
  (let [a (:args c)
        res (merge {:status :stopped :reason reason :got (:got extra 0)} (wanted-fields a) extra)]
    (ctx/result! c res)
    :done))

(defn tried! [c k v] (ctx/update-mem! c assoc-in [:tried k] v))

(defn ^:async withdraw! [c pos items names have]
  (let [name (holds items names)
        need (- (:target (:start (ctx/mem c))) have)
        r (await (ctx/call-child c :take 'jobs.storage.withdraw
                                 {:chest pos :items {name (+ (deposit/carried (u/inventory (:primitives c)) name) need)}}))]
    (when (#{:done :declined} r)
      (ctx/update-mem! c update :done-chests (fnil conj #{}) (fetch/cell-of pos)))
    :continue))

(defn ^:async inspect! [c pos]
  (let [w (await (near/walk-near! c pos 3))
        cell (fetch/cell-of pos)
        mark! #(ctx/update-mem! c (fn [m] (-> m (update :done-chests (fnil conj #{}) cell) (update :inspected (fnil inc 0)))))]
    (case w
      :partial :continue
      :blocked (do (mark!) :continue)
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))]
        (if (= "ok" (.-status seen))
          (do (fetch/note-stock! c pos (.-items seen))
              (ctx/update-mem! c update :inspected (fnil inc 0))
              :continue)
          (do (mark!) :continue))))))

(def table-offsets [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 1] [1 -1] [-1 -1]])

(defn table-spot
  "A free cell beside the body to put a table in: air at the body's level with a solid block under it, or nil."
  [c]
  (let [p (:primitives c)
        self (u/self-pos c)
        [x y z] (mapv #(js/Math.floor (% self)) [:x :y :z])
        solid? (fn [n] (and n (not (b/air n)) (not (b/fluids n)) (not (b/clearable n))))]
    (some (fn [[dx dz]]
            (let [pos {:x (+ x dx) :y y :z (+ z dz)}]
              (when (and (b/air (u/block-name p pos))
                         (solid? (u/block-name p (update pos :y dec))))
                pos)))
          table-offsets)))

(def max-fruitless 3)

(defn fruitless! [c why]
  (ctx/update-mem! c update-in [:craft :fruitless] (fnil inc 0))
  (ctx/update-mem! c assoc-in [:craft :why] why)
  :continue)

(defn ^:async craft-step!
  "One round of the craft source: run the current step of the plan as a child, until it ends."
  [c names have target]
  (let [mem (:craft (ctx/mem c))
        step (or (:step mem)
                 (some-> (craft-plan c names (- target have)) :steps first))]
    (cond
      (>= (:fruitless mem 0) max-fruitless)
      (do (tried! c :craft (or (:why mem) :failed))
          :continue)

      (nil? step)
      (do (tried! c :craft :no-plan) :continue)

      (= :place (:op step))
      (let [spot (or (:spot mem) (table-spot c))]
        (if-not spot
          (fruitless! c :no-table-spot)
          (do (ctx/update-mem! c assoc-in [:craft :step] step)
              (ctx/update-mem! c assoc-in [:craft :spot] spot)
              (let [r (await (ctx/call-child c :place 'jobs.blocks.place {:item (:item step) :pos spot}))]
                (if (= :continue r)
                  :continue
                  (let [res (ctx/child-result c :place)]
                    (ctx/update-mem! c update :craft dissoc :step :spot)
                    (if (and (= :done r) (:placed res))
                      (do (ctx/update-mem! c assoc :table spot) :continue)
                      (fruitless! c (or (:reason res) :place-declined)))))))))

      :else
      (do (ctx/update-mem! c assoc-in [:craft :step] step)
          (let [r (await (ctx/call-child c :craft 'jobs.items.craft (cond-> {:item (:item step) :count (:count step)}
                                                                        (and (:table? step) (:table (ctx/mem c))) (assoc :table (:table (ctx/mem c))))))]
            (if (= :continue r)
              :continue
              (let [res (ctx/child-result c :craft)]
                (ctx/update-mem! c update :craft dissoc :step)
                (if (and (= :done r) (pos? (:made res 0)))
                  :continue
                  (fruitless! c (or (:reason res) (when (:short res) {:short (:short res)}) :declined))))))))))

(defn ^:async round [c]
  (let [a (:args c)
        names (names-of a)
        inv (u/inventory (:primitives c))
        have (carried-of inv names)
        now (ctx/now c)]
    (if-let [e (args-error a)]
      (do (ctx/emit! c :obtain.declined :warn {:reason :bad-args :text (str "items.obtain " e)})
          (stop! c :bad-args {:why e}))
      (let [_ (when-not (:start (ctx/mem c))
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
              (and unknown (< (:inspected m 0) max-inspections)) (await (inspect! c (:pos unknown)))
              :else (do (tried! c :chest (if (or (seq (:done-chests m)) (pos? (:inspected m 0))) :lacking :none-seen))
                        :continue)))
          (and (contains? (:how o) :craft) (not (get-in m [:tried :craft])))
          (await (craft-step! c names have target))
          :else (stop! c :no-source {:got got :tried (:tried m)}))))))
