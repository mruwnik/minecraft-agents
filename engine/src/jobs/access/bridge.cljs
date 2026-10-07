(ns jobs.access.bridge
  (:require [jobs.access.pillar :as pillar]
            [jobs.lib.pillar :as pl]
            [jobs.lib.escape :as escape]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.pace :as pace]))

(def doc
  "Bridge a 1-wide row of :length blocks along :heading from the cell the body stands in, over air or fluid: place the
  floor cell ahead against the block the body stands on, walk onto it (jobs.movement.go-to child, no escalation),
  repeat. One call builds the whole row; it yields :continue only while a go-to waits on the world. Not a way round a
  gap the walker could not plan: the caller decides to bridge.

  Every block is written to the scaffold ledger (jobs.lib.ledger, body memory, purpose :bridge) as an intent before
  its place and confirmed when the cell is seen holding it, so a cleanup can take the row back after a cut or restart.
  Each (re)start reads the row from the cells: a cell ahead that is already solid floor is stepped onto, not placed.

  Before each step it checks:
  - the whole rest of the row is permitted (no zone that bars :place, no other plan's footprint, a zone list loaded)
  - the body stands on a solid block, on the row's line at the start height
  - the two cells ahead at body height are clear (headroom)
  - a block is carried: :item, or without it dirt while any is carried, then cobblestone
  - the cell ahead is loaded and passes jobs.lib.access.rules/may-place?

  A shove off the line walks back to the last cell stood on (3 tries), then gives up :off-line. The sneak-edge
  variant is not used: the body stands at the block's centre, so it does not step off the edge.

  The check waits (:too-few-blocks, with :short) when blocks are missing at the start; run short part way, it gives up
  :too-few-blocks. Every give-up ends the job, so a parent running it as a child reads the result.

  Ends with {:status :done|:gave-up :reason :placed [[x y z] ...] :built n :length n}. :placed are this job's confirmed
  blocks, nearest first. Events: bridge.done (info) and bridge.gave-up (warn, with :reason and by reason :at :block
  :zone :short :detail). Gives up with :too-few-blocks, :not-on-solid, :blocked (a cell ahead at body height is not
  clear), :zone, :footprint, :no-zones, :not-loaded, :not-replaceable, :off-line, :place-failed (3 failed places in a
  row, :detail the primitive's reason), :move-failed (3 failed walks onto a placed block) or :bad-args.")

(def args
  {:heading {:doc ":north :east :south or :west" :default nil}
   :length {:doc "blocks in the row, 1 to 64" :default 1}
   :item {:doc "the block to bridge with; nil: dirt while any is carried, then cobblestone" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :fetch {:doc "get missing blocks instead of waiting :too-few-blocks (jobs.lib.fetch): true, a set of kinds or a map of limits" :default true}})

(def max-length 64)

(def purpose :bridge)

(def max-failures 3)

(def headings {:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]})

(def zone-keys pillar/zone-keys)

(defn ahead
  "The cell n steps from [x y z] along heading, at height dy."
  [[x y z] heading n dy]
  (let [[hx hz] (headings heading)] [(+ x (* n hx)) (+ y dy) (+ z (* n hz))]))

(defn progress
  "Steps of the row behind feet, or nil when feet are off its line or height."
  [[fx fy fz] [sx sy sz] heading]
  (let [[hx hz] (headings heading)
        i (+ (* hx (- fx sx)) (* hz (- fz sz)))]
    (when (and (= fy sy) (<= 0 i) (= [fx fz] [(+ sx (* i hx)) (+ sz (* i hz))])) i)))

(defn permission-refusal
  "The first refusal among the floor cells i+1 to length for permission alone, as the rules' refusal with :at."
  [{:keys [start heading length ledger] :as in} i]
  (some (fn [n]
          (let [cell (ahead start heading n -1)
                v (rules/may-place? (merge (select-keys in zone-keys)
                                           {:block-at (constantly "air") :cell cell :feet (ahead start heading n 0) :ledger ledger}))]
            (when-not (:ok v) (assoc v :at cell))))
        (range (inc i) (inc length))))

(defn next-step
  "The next step of a bridge, from {:feet :start :heading :length :block-at :carried {name count} :item :zones
  :footprints :ledger #{cells}}: {:step :done}, {:step :move :to stand}, {:step :place :cell :item} or {:step :give-up
  :reason ...}."
  [{:keys [feet start heading length block-at carried item] :as in}]
  (if-not (and (int? length) (<= 1 length max-length) (contains? headings heading))
    (pillar/give-up :bad-args :text (str ":length must be 1 to " max-length " and :heading one of :north :east :south :west, not "
                                         (pr-str length) " " (pr-str heading)))
    (let [i (progress feet start heading)]
      (cond
        (nil? i) (pillar/give-up :off-line :at feet)
        (>= i length) {:step :done}
        :else
        (let [stand (ahead feet heading 1 0)
              target (ahead feet heading 1 -1)
              refusal (permission-refusal in i)
              blocked (first (remove #(pl/clear? (block-at %)) [stand (ahead feet heading 1 1)]))
              use (pillar/item-to-use item carried)
              place (rules/may-place? (assoc in :cell target :feet stand))]
          (cond
            refusal (pillar/refusal->give-up refusal)
            (not (rules/solid-floor? block-at (ahead feet heading 0 -1))) (pillar/give-up :not-on-solid :at (ahead feet heading 0 -1))
            blocked (pillar/give-up :blocked :at blocked :block (block-at blocked))
            (rules/solid-floor? block-at target) {:step :move :to stand}
            (nil? use) (pillar/give-up :too-few-blocks :short (- length i))
            (not (:ok place)) (pillar/refusal->give-up (assoc place :at target))
            :else {:step :place :cell target :item use}))))))

;; ------------------------------------------------------------------ the round

(defn placed
  "This job's confirmed bridge cells in ledger l, nearest the start first."
  [c l start heading]
  (let [at #(progress (update % 1 inc) start heading)]
    (->> (ledger/of-job l (:id c) purpose)
         (filter #(= :placed (:state %)))
         (map :cell)
         (sort-by #(or (at %) 0))
         vec)))

(defn finish!
  "End the job: the result and its event."
  [c l step]
  (let [{:keys [heading length]} (:args c)
        cells (placed c l (:start (ctx/mem c)) heading)
        done? (= :done (:step step))
        result (merge {:status (if done? :done :gave-up) :placed cells :built (count cells) :length length}
                      (dissoc step :step))]
    (ctx/result! c result)
    (if done?
      (ctx/emit! c :bridge.done :info (assoc result :text (str "bridge of " (count cells) " blocks " (name heading))))
      (ctx/emit! c :bridge.gave-up :warn
                 (assoc result :text (str "bridge gave up at " (count cells) " of " length ": " (name (:reason step))
                                          (when (:short step) (str ", " (:short step) " blocks short"))
                                          (when (:at step) (str " at " (pr-str (:at step))))))))
    :done))

(defn inputs
  "next-step's input now, with the ledger l."
  [c l feet]
  (let [p (:primitives c)
        {:keys [heading length item]} (:args c)]
    (merge (pillar/access-inputs c)
           {:feet feet :start (or (:start (ctx/mem c)) feet) :heading heading :length length :block-at (escape/block-at-of p)
            :carried (pl/carried p) :item item :ledger (ledger/cells l)})))

(defn need
  "The wait reason for the blocks step lacks, or nil: a :need for jobs.lib.fetch."
  [c step]
  (when (= :too-few-blocks (:reason step))
    (let [item (:item (:args c))]
      (cond-> {:reason :need :count (:short step)}
        item (assoc :item item)
        (not item) (assoc :any-of pl/default-items)))))

(defn check
  "True, or a wait for the blocks a bridge lacks at its start (fetched for unless :fetch is off); every other give-up
  stays with the round."
  [c]
  (let [p (:primitives c)
        l (ledger/reconcile (ledger/open-entries (ctx/view c)) (escape/block-at-of p))
        step (next-step (inputs c l (pl/feet-cell c)))]
    (cond
      (not= :too-few-blocks (:reason step)) true
      (nil? (fetch/opts c 'jobs.access.bridge)) (ctx/wait c {:reason :too-few-blocks :short (:short step) :item (or (:item (:args c)) pl/default-items)})
      :else (fetch/check c 'jobs.access.bridge (need c step)))))

(defn ^:async place!
  "Write the intent, place one block, confirm it when the cell shows it. Three failed places in a row give up."
  [c l block-at {:keys [cell item]}]
  (let [l (ledger/intend l {:cell cell :item item :before (block-at cell) :job (:id c) :purpose purpose})
        _ (ledger/remember! c l)
        [x y z] cell
        r (await (ctx/act c :place #js {:pos #js {:x x :y y :z z} :item item}))]
    (if (= "placed" (.-status r))
      (do (when (= item (block-at cell)) (ledger/remember! c (ledger/confirm l cell)))
          (ctx/update-mem! c assoc :failures 0)
          :again)
      (let [failures (inc (:failures (ctx/mem c) 0))
            settled (ledger/reconcile l block-at)]
        (ctx/update-mem! c assoc :failures failures)
        (ledger/remember! c settled)
        (if (< failures max-failures)
          :again
          (finish! c settled (pillar/give-up :place-failed :detail (or (.-reason r) (.-status r)))))))))

(defn ^:async walk-to!
  "A go-to child to the cell [x y z] (range 0, no escalation). :continue while it waits; :ok on arrival, else :failed."
  [c [x y z]]
  (let [r (await (ctx/call-child c :step 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 0 :escalate false
                                                               :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue r) :continue
      (and (= :done r) (not= :stopped (:status (ctx/child-result c :step)))) :ok
      :else :failed)))

(defn ^:async move!
  "Walk onto the placed cell; three failed walks in a row give up."
  [c l {:keys [to]}]
  (let [r (await (walk-to! c to))]
    (case r
      :continue :continue
      :ok (do (ctx/update-mem! c assoc :moves 0 :stand to) :again)
      (let [n (inc (:moves (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :moves n)
        (if (< n max-failures) :again (finish! c l (pillar/give-up :move-failed :at to)))))))

(defn ^:async recover!
  "Off the line: walk back to the last cell stood on while its floor holds, three times; else give up :off-line."
  [c l block-at step]
  (let [{:keys [stand recovers]} (ctx/mem c)]
    (if (and stand (< (or recovers 0) max-failures) (rules/solid-floor? block-at (update stand 1 dec)))
      (let [r (await (walk-to! c stand))]
        (when-not (= :continue r) (ctx/update-mem! c update :recovers (fnil inc 0)))
        (if (= :continue r) :continue :again))
      (finish! c l step))))

(defn ^:async step!
  "One block of the bridge (or its end): :again, :continue while a go-to waits, or :done."
  [c]
  (await (pillar/land! c))
  (let [block-at (escape/block-at-of (:primitives c))
        seen (ledger/open-entries (ctx/view c))
        l (ledger/reconcile seen block-at)
        feet (pl/feet-cell c)
        step (next-step (inputs c l feet))
        fetched (await (fetch/step! c 'jobs.access.bridge (need c step) {:return? true}))]
    (when (not= l seen) (ledger/remember! c l))
    (ctx/update-mem! c #(-> % (update :start (fn [s] (or s feet))) (update :stand (fn [s] (or s feet)))))
    (if fetched
      fetched
      (case (:step step)
        :place (await (place! c l block-at step))
        :move (await (move! c l step))
        (if (= :off-line (:reason step))
          (await (recover! c l block-at step))
          (finish! c l step))))))

(defn ^:async round
  "The whole bridge: step! until it ends, a pace between blocks."
  [c]
  (await (pace/steps! c #(step! c))))
