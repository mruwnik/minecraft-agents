(ns jobs.access.pillar
  (:require [jobs.lib.ledger :as ledger]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.util :as u]))

(def doc
  "Pillar up :height blocks from the cell the body stands in, by jump-placing one block at a time under itself.
  One block per round.

  Every block is written to the scaffold ledger (jobs.lib.ledger, body memory) as an intent before its
  jump and confirmed when the cell is seen holding it, so a cleanup can take the pillar back after a cut or
  restart. A restart between intent and placement is decided from the cell.

  Before each block it checks:
  - the whole rest of the column, feet up to the top, is permitted (no zone that bars :place, no other plan's
    footprint, a zone list loaded)
  - the body stands on a solid block
  - the two cells above the head are clear
  - a block is carried: :item, or without it dirt while any is carried, then cobblestone
  - the cell passes jobs.lib.access.rules/may-place?

  A knockback (a failed jump with the body airborne or off the column) is not a refusal: the round waits to land,
  walks back into the column (go-to child) and tries again; up to 8 per pillar, then :off-column.

  The check waits (:too-few-blocks, with :short) when blocks are missing. Every other give-up is left to the
  round, so a parent running this as a child reads the result.

  Ends with {:status :done|:gave-up :reason :cells [[x y z] ...] :built n :height n}. :cells are this job's
  confirmed blocks, lowest first. Events: pillar.done (info) and pillar.gave-up (warn, with :reason and by
  reason :at :block :zone :short :detail). Gives up with :too-few-blocks (:short still owed), :ceiling,
  :not-on-solid, :zone, :footprint, :no-zones, :not-loaded, :not-replaceable, :off-column (the body left the
  column it started in), :place-failed (3 failed jumps in a row with the body in the column, :detail the primitive's reason) or :bad-args.")

(def args
  {:height {:doc "blocks to rise, 1 to 64" :default 1}
   :item {:doc "the block to pillar with; nil: dirt while any is carried, then cobblestone" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def max-height 64)

(def default-items ["dirt" "cobblestone"])

(def purpose :pillar)

(def max-failures 3)

(def max-displacements
  "Knockbacks (a failed jump with the body airborne or off its column) a pillar rides out before it gives up."
  8)

(def land-step-ms 50)

(def land-max-steps 20)

(defn access-inputs
  "The zones, claims, footprints, the body's name, the clock and the job's :ignore-zones? arg, as jobs.lib.access.rules
  takes them (jobs.lib.access/zone-input)."
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

(defn displaced?
  "True when the body is airborne or outside the column cell [bx _ bz] (a knockback), not merely refused."
  [c [bx _ bz]]
  (let [[fx _ fz] (feet-cell c)]
    (boolean (or (false? (.-onGround (.self (:primitives c)))) (not= [bx bz] [fx fz])))))

(defn ^:async land!
  "Wait in short steps until the body is on the ground, up to land-max-steps."
  [c]
  (loop [i 0]
    (when (and (< i land-max-steps) (false? (.-onGround (.self (:primitives c)))))
      (await (ctx/act c :wait (clj->js {:ms land-step-ms})))
      (recur (inc i)))))

(defn ^:async recentre!
  "Walk back to the middle of the cell [x y z] the pillar last stood in, after a shove. True when the body is in its
  column again."
  [c [x y z]]
  (let [in-column? #(let [[fx _ fz] (feet-cell c)] (= [x z] [fx fz]))]
    (when-not (in-column?)
      (await (ctx/call-child c :recentre 'jobs.movement.go-to {:pos {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0 :escalate false})))
    (in-column?)))

(defn ^:async place!
  "Write the intent, jump-place one block, confirm it when the cell shows it. Three failed jumps in a row give up;
  a failure with the body shoved off or airborne is a knockback and does not count towards the three."
  [c l block-at {:keys [cell item]}]
  (let [l (ledger/intend l {:cell cell :item item :before (block-at cell) :job (:id c) :purpose purpose})
        _ (ledger/remember! c l)
        r (await (ctx/act c :jumpPlace #js {:item item :count 1}))]
    (if (pos? (or (.-placed r) 0))
      (do (when (= item (block-at cell)) (ledger/remember! c (ledger/confirm l cell)))
          (ctx/update-mem! c assoc :failures 0)
          :continue)
      (let [shoved? (displaced? c (:base (ctx/mem c)))
            _ (when shoved? (ctx/update-mem! c update :displaced (fnil inc 0)))
            failures (if shoved? (:failures (ctx/mem c) 0) (inc (:failures (ctx/mem c) 0)))]
        (ctx/update-mem! c assoc :failures failures)
        (if (and (< failures max-failures) (<= (:displaced (ctx/mem c) 0) max-displacements))
          :continue
          (let [settled (ledger/reconcile l block-at)]
            (ledger/remember! c settled)
            (finish! c settled (if (< failures max-failures)
                                 (give-up :off-column :at (feet-cell c) :detail (.-reason r))
                                 (give-up :place-failed :detail (.-reason r))))))))))

(defn ^:async round [c]
  (await (land! c))
  (let [{:keys [stand displaced]} (ctx/mem c)]
    (when (and stand (pos? (or displaced 0)))
      (await (recentre! c stand))))
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
    (ctx/update-mem! c assoc :base base :stand feet)
    (if (= :place (:step step))
      (await (place! c l block-at step))
      (finish! c l step))))
