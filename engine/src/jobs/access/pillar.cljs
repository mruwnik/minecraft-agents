(ns jobs.access.pillar
  (:require [engine.access.ledger :as ledger]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.util :as u]))

(def doc
  "Pillar up :height blocks from the cell the body stands in, by jump-placing one block at a time under itself
  (jumpPlace with a count of 1). Every block is written to the scaffold ledger (engine.access.ledger, body memory) as
  an intent before its jump and confirmed when the cell is seen holding it, so a cleanup can take the pillar back
  even after a cut or a restart; a restart between intent and placement is decided from the cell. One block per
  round. Before each block: the whole rest of the column (feet up to the top) must be permitted (no zone that does not
  allow :place, no other plan's footprint, a zone list loaded), the body must stand on a solid block, the cell above
  the head and the one above that must be clear (the new feet and head cells), a block must be carried, and the cell
  must pass engine.access.rules/may-place?. Material: :item, or without it dirt while any is carried, then
  cobblestone. Ends with the result {:status :done|:gave-up :reason :cells [[x y z] ...] :built n :height n}, :cells
  being this job's confirmed blocks, lowest first; events pillar.done (info) and pillar.gave-up (warn, with :reason
  and, by reason, :at :block :zone :short :detail). Gives up with :too-few-blocks (:short still owed), :ceiling (stops
  below it), :not-on-solid, :zone, :footprint, :no-zones, :not-loaded, :not-replaceable, :off-column (the body left
  the column it started in), :place-failed (3 failed jumps in a row, :detail the primitive's reason) or :bad-args.")

(def args
  {:height {:doc "blocks to rise, 1 to 64" :default 1}
   :item {:doc "the block to pillar with; nil: dirt while any is carried, then cobblestone" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def max-height 64)

(def default-items ["dirt" "cobblestone"])

(def purpose :pillar)

(def max-failures 3)

(defn access-inputs
  "The zones, claims, footprints, the body's name, the clock and the job's :ignore-zones? arg, as engine.access.rules
  takes them (engine.jobs.access/zone-input)."
  [c]
  (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c))}))

(def zone-keys [:zones :footprints :claims :self :now :ignore-zones?])

(defn up [[x y z] n] [x (+ y n) z])

(defn clear?
  "A cell the body's feet or head can move into: air or a non-fluid replaceable plant."
  [n]
  (boolean (and n (rules/replaceable n) (not (rules/fluids n)))))

(defn item-to-use
  "The block to place next: item when carried, without one the first of default-items carried; nil when none."
  [item carried]
  (if item
    (when (pos? (get carried item 0)) item)
    (first (filter #(pos? (get carried % 0)) default-items))))

(defn permission-refusal
  "The first refusal among cells for permission alone (zone, footprint, no zone list), as the rules' refusal with :at."
  [cells {:keys [ledger] :as in}]
  (some (fn [cell]
          (let [v (rules/may-place? (merge (select-keys in zone-keys)
                                           {:block-at (constantly "air") :cell cell :feet (up cell 1) :ledger ledger}))]
            (when-not (:ok v) (assoc v :at cell))))
        cells))

(defn give-up [reason & {:as more}] (merge {:step :give-up :reason reason} more))

(defn refusal->give-up [v] (merge (dissoc v :ok) {:step :give-up}))

(defn next-step
  "The next step of a pillar, from {:feet :base :height :block-at :carried {name count} :item :zones :footprints
  :ledger #{cells}}: {:step :done}, {:step :place :cell :item} or {:step :give-up :reason ...}."
  [{:keys [feet base height block-at carried item] :as in}]
  (let [[bx by bz] base
        [fx fy fz] feet
        top (+ by height)]
    (if-not (and (int? height) (<= 1 height max-height))
      (give-up :bad-args :text (str ":height must be 1 to " max-height ", not " (pr-str height)))
      (if (or (not= [bx bz] [fx fz]) (< fy by))
        (give-up :off-column :at feet)
        (if (>= fy top)
          {:step :done}
          (let [refusal (permission-refusal (map #(up feet %) (range (- top fy))) in)
                floor (up feet -1)
                ceiling (first (remove #(clear? (block-at %)) [(up feet 1) (up feet 2)]))
                use (item-to-use item carried)
                place (rules/may-place? (assoc in :cell feet :feet (up feet 1)))]
            (cond
              refusal (refusal->give-up refusal)
              (not (rules/solid-floor? block-at floor)) (give-up :not-on-solid :at floor :block (block-at floor))
              ceiling (give-up :ceiling :at ceiling :block (block-at ceiling))
              (nil? use) (give-up :too-few-blocks :short (- top fy))
              (not (:ok place)) (refusal->give-up (assoc place :at feet))
              :else {:step :place :cell feet :item use})))))))

;; ------------------------------------------------------------------ the round

(defn feet-cell [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn block-at-of [p] (fn [[x y z]] (u/block-name p {:x x :y y :z z})))

(defn carried [p]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory p)))

(defn built
  "This job's confirmed pillar cells in ledger l, lowest first."
  [c l]
  (->> (ledger/of-job l (:id c) purpose)
       (filter #(= :placed (:state %)))
       (map :cell)
       (sort-by second)
       vec))

(defn finish!
  "End the job: the result and its event."
  [c l step]
  (let [cells (built c l)
        height (:height (:args c))
        result (merge {:status (if (= :done (:step step)) :done :gave-up) :cells cells :built (count cells) :height height}
                      (dissoc step :step))]
    (ctx/result! c result)
    (if (= :done (:step step))
      (ctx/emit! c :pillar.done :info (assoc result :text (str "pillar of " (count cells) " up to " (pr-str (peek cells)))))
      (ctx/emit! c :pillar.gave-up :warn
                 (assoc result :text (str "pillar gave up at " (count cells) " of " height ": " (name (:reason step))
                                          (when (:short step) (str ", " (:short step) " blocks short"))
                                          (when (:at step) (str " at " (pr-str (:at step))))))))
    :done))

(defn check
  "True, or a wait for what a pillar lacks (next-step's :too-few-blocks, with how many blocks are short). Every other
  give-up stays with the round, which ends with its result; so does this one for a pillar run as a child (retreat),
  whose parent reads the result."
  [c]
  (let [p (:primitives c)
        block-at (block-at-of p)
        l (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)
        feet (feet-cell c)
        {:keys [height item]} (:args c)
        step (next-step (merge (access-inputs c)
                               {:feet feet :base (or (:base (ctx/mem c)) feet) :height height :block-at block-at
                                :carried (carried p) :item item :ledger (ledger/cells l)}))]
    (if (= :too-few-blocks (:reason step))
      (ctx/wait c {:reason :too-few-blocks :short (:short step) :item (or item default-items)})
      true)))

(defn ^:async place!
  "Write the intent, jump-place one block, confirm it when the cell shows it. Three failed jumps in a row give up."
  [c l block-at {:keys [cell item]}]
  (let [l (ledger/intend l {:cell cell :item item :before (block-at cell) :job (:id c) :purpose purpose})
        _ (ledger/remember! c l)
        r (await (ctx/act c :jumpPlace #js {:item item :count 1}))]
    (if (pos? (or (.-placed r) 0))
      (do (when (= item (block-at cell)) (ledger/remember! c (ledger/confirm l cell)))
          (ctx/update-mem! c assoc :failures 0)
          :continue)
      (let [failures (inc (:failures (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :failures failures)
        (if (< failures max-failures)
          :continue
          (let [settled (ledger/reconcile l block-at)]
            (ledger/remember! c settled)
            (finish! c settled (give-up :place-failed :detail (.-reason r)))))))))

(defn ^:async round [c]
  (let [p (:primitives c)
        block-at (block-at-of p)
        seen (ledger/open-entries (ctx/view c))
        l (ledger/reconcile seen block-at)
        feet (feet-cell c)
        base (or (:base (ctx/mem c)) feet)
        {:keys [height item]} (:args c)
        step (next-step (merge (access-inputs c)
                               {:feet feet :base base :height height :block-at block-at :carried (carried p)
                                :item item :ledger (ledger/cells l)}))]
    (when (not= l seen) (ledger/remember! c l))
    (ctx/update-mem! c assoc :base base)
    (if (= :place (:step step))
      (await (place! c l block-at step))
      (finish! c l step))))
