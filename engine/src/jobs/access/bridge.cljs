(ns jobs.access.bridge
  (:require [engine.contract :as contract]
            [jobs.lib.choose :as choose]
            [jobs.lib.need :as need]
            [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [jobs.access.pillar :as pillar]
            [jobs.lib.pillar :as pl]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as look]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.spent :as spent]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]))

(def doc
  "Bridge a 1-wide row of :length blocks along :heading from the cell the body stands in, over air or fluid: place the
  floor cell ahead against the block the body stands on, walk onto it (jobs.movement.go-to child, no escalation),
  repeat. One call builds the whole row; it yields :continue only while a child waits on the world. Not a way round a
  gap the walker could not plan: the caller decides to bridge.

  Every block is written to the scaffold ledger (jobs.lib.ledger, body memory, purpose :bridge) as an intent before
  its place and confirmed when the cell is seen holding it, so a cleanup can take the row back after a cut or restart.
  Each (re)start reads the row from the cells: a cell ahead that is already solid floor is stepped onto, not placed.

  Conditions, checked at the start: the first step's :bad-args, permission refusal (:zone :footprint :no-zones) and :not-on-solid.

  Before each step it checks:
  - the whole rest of the row is permitted (no zone that bars :place, no other plan's footprint, a zone list loaded)
  - the body stands on a solid block, on the row's line at the start height
  - the two cells ahead at body height are clear (headroom) and sensed (looked at first)
  - a block is carried: :item, or without it dirt while any is carried, then cobblestone
  - the cell ahead is loaded and passes jobs.lib.access.rules/may-place?

  A shove off the line walks back to the last cell stood on (3 tries), then gives up :off-line. The sneak-edge
  variant is not used: the body stands at the block's centre, so it does not step off the edge.

  Blocks missing (:too-few-blocks, with :short) are got with jobs.items.obtain (jobs.lib.need, then the body walks back
  to the cell it stood on) unless :fetch is false; a cell ahead still unsensed after the look ends :unseen.

  Ends :done with data {:placed [[x y z] ...] :built n :length n :spent {item n} :spent-at [cell ...]}; :placed are this job's confirmed blocks, nearest
  first. A give-up is condition-failed with the cause :too-few-blocks (the fetch ending as its :because), :not-on-solid,
  :blocked (a cell ahead at body height is not clear), :zone, :footprint, :no-zones, :not-loaded, :not-replaceable,
  :unseen or :bad-args, or nothing-possible with :off-line, :place-failed (3 failed places in a row, :detail the
  primitive's reason) or :move-failed (3 failed walks onto a placed block); the data holds :placed and :built as well,
  and by reason :at :block :zone :short :detail. Event: bridge.gave-up (warn).")

(a/defargs args
  {:heading {:doc ":north :east :south or :west" :spec #{:north :east :south :west} :default nil}
   :length {:doc "blocks in the row, 1 to 64" :spec (a/int-in 1 64) :default 1}
   :item {:doc "the block to bridge with; nil: dirt while any is carried, then cobblestone" :spec a/item? :default nil}
   :ignore-zones? :shared
   :fetch {:doc "get missing blocks with jobs.items.obtain instead of ending :too-few-blocks (false: end); a set of kinds or a map of limits counts as true" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds a bridge may take." :spec (a/num-in 0 nil)}
   ::max-length {:default 64 :doc "Longest bridge a call may ask for, in blocks." :spec (a/int-in 0 nil)}
   ::max-failures {:default 3 :doc "Failed moves or recoveries before the bridge gives up." :spec (a/int-in 0 nil)}})
(defn max-length [] (settings/get settings ::max-length))
(defn time-s [] (settings/get settings ::job-time-s))

(def safe? ready/safe-at-from)

(def fit? ready/fit?)

(def target [:heading])

(def defaults {:spend {:job-time ::job-time-s}})

(def purpose :bridge)

(defn max-failures [] (settings/get settings ::max-failures))

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
  :footprints :ledger #{cells} :unknown? fn of a cell}: {:step :done}, {:step :move :to stand}, {:step :place :cell :item} or {:step :give-up
  :reason ...}."
  [{:keys [feet start heading length block-at carried item] :as in}]
  (if-not (and (int? length) (<= 1 length (max-length)) (contains? headings heading))
    (pillar/give-up :bad-args :text (str ":length must be 1 to " (max-length) " and :heading one of :north :east :south :west, not "
                                         (pr-str length) " " (pr-str heading)))
    (let [i (progress feet start heading)]
      (cond
        (nil? i) (pillar/give-up :off-line :at feet)
        (>= i length) {:step :done}
        :else
        (let [stand (ahead feet heading 1 0)
              target (ahead feet heading 1 -1)
              refusal (permission-refusal in i)
              unseen (first (filter (or (:unknown? in) (constantly false)) [stand (ahead feet heading 1 1)]))
              blocked (first (remove #(pl/clear? (block-at %)) [stand (ahead feet heading 1 1)]))
              use (pillar/item-to-use item carried (:items in))
              place (rules/may-place? (-> in (dissoc :body-cols) (assoc :cell target :feet stand)))]
          (cond
            refusal (pillar/refusal->give-up refusal)
            (not (rules/solid-floor? block-at (ahead feet heading 0 -1))) (pillar/give-up :not-on-solid :at (ahead feet heading 0 -1))
            blocked (pillar/give-up :blocked :at blocked :block (block-at blocked))
            unseen (pillar/give-up :unseen :at unseen)
            (rules/solid-floor? block-at target) {:step :move :to stand}
            (nil? use) (pillar/give-up :too-few-blocks :short (- length i))
            (not (:ok place)) (pillar/refusal->give-up (assoc place :at target))
            :else {:step :place :cell target :item use}))))))

;; ------------------------------------------------------------------ the round

(defn placed-entries
  "This job's confirmed bridge ledger entries in l, nearest the start first."
  [c l start heading]
  (let [at #(progress (update % 1 inc) start heading)]
    (->> (ledger/of-job l (:id c) purpose)
         (filter #(= :placed (:state %)))
         (sort-by #(or (at (:cell %)) 0)))))

(defn placed
  "This job's confirmed bridge cells in ledger l, nearest the start first."
  [c l start heading]
  (mapv :cell (placed-entries c l start heading)))

(defn result-of
  "The bridge's result data from ledger l: the cells placed, their count and the length, plus what they cost."
  [c l start heading]
  (let [entries (if start (placed-entries c l start heading) [])]
    (merge {:placed (mapv :cell entries) :built (count entries) :length (:length (:args c))}
           (spent/summary (reduce (fn [log {:keys [item cell]}] (spent/add log item cell)) nil entries)))))

(def condition-reasons
  "Give-up reasons that read as a failed condition; any other is nothing possible."
  #{:not-on-solid :blocked :zone :footprint :no-zones :not-loaded :not-replaceable :unseen :too-few-blocks})

(defn finish!
  "End the job on a give-up step: its event and ending."
  [c l step]
  (let [{:keys [heading length]} (:args c)
        result (merge (result-of c l (:start (ctx/mem c)) heading) (dissoc step :step :reason))
        cells (:placed result)]
    (ctx/emit! c :bridge.gave-up :warn
               (assoc result :status :stopped :reason (:reason step)
                      :text (str "bridge gave up at " (count cells) " of " length ": " (name (:reason step))
                                 (when (:short step) (str ", " (:short step) " blocks short"))
                                 (when (:at step) (str " at " (pr-str (:at step)))))))
    (ctx/end c (cond (= :bad-args (:reason step)) :impossible (condition-reasons (:reason step)) :condition-failed :else :nothing-possible)
             (:reason step) result)))

(defn inputs
  "next-step's input now, with the ledger l."
  [c l feet]
  (let [p (:primitives c)
        {:keys [heading length item]} (:args c)]
    (merge (pillar/access-inputs c)
           {:feet feet :start (or (:start (ctx/mem c)) feet) :heading heading :length length :block-at (access/sensed-at p "air") :unknown? #(look/unknown? p %)
            :carried (pl/carried p) :item item :items (mapv :item (:list (choose/fillers-for c :pillar 1))) :ledger (ledger/cells l)})))

(def start-facts
  "Give-up reasons known from the args and the map under the feet, before any look: the call's own mistakes and what permission refuses."
  #{:bad-args :zone :footprint :no-zones :claim :not-on-solid})

(defn start-fact
  "At the call's start: the first step's give-up as a status when it is one of start-facts (the call's own mistake is
  never, the rest later), else ok."
  [c]
  (let [step (next-step (inputs c (ledger/open-entries (ctx/view c)) (pl/feet-cell c)))
        fact (dissoc step :step)]
    (cond
      (not (and (= :give-up (:step step)) (start-facts (:reason step)))) (contract/ok :start "the first step is not refused")
      (= :bad-args (:reason fact)) (checks/cause-status :never fact "bad args")
      :else (checks/cause-status :later fact (str "the first step gives up: " (name (:reason fact)))))))

(def can-start? start-fact)

(defn done?
  "Done when the body stands on the row's last block: the blocks placed and the body walked onto them. Before the first
  round there is no start cell (it is the body's cell then), so it is :later :not-started."
  [c]
  (let [{:keys [heading length]} (:args c)
        start (:start (ctx/mem c))
        l (ledger/open-entries (ctx/view c))
        result (result-of c l start heading)
        cells (:placed result)
        i (when start (progress (pl/feet-cell c) start heading))]
    (if (and i (pos? (or length 0)) (>= i length))
      (contract/ok :bridged (str "bridge of " (count cells) " blocks " (name heading)) :units :blocks :done (count cells) :left 0 :result result)
      (contract/later (if start :building :not-started) (if start "the bridge is being built" "the bridge is not started: the start cell is taken in the first round") :units :blocks :done (count cells) :left (max 0 (- (or length 0) (count cells))) :result result))))

(defn quote-of [c]
  (let [{:keys [length item]} (:args c)
        n (or length 1)]
    {:uses (cond-> {:job-time [(* 0.3 n) n (max n (time-s))]} item (assoc :items {item [0 n n]}))}))

(defn ^:async place!
  "Write the intent, place one block, confirm it when the place reports it (the sensed cell may lag). Three failed places in a row give up."
  [c l block-at {:keys [cell item]}]
  (let [l (ledger/intend l {:cell cell :item item :before (block-at cell) :job (:id c) :purpose purpose})
        _ (ledger/remember! c l)
        [x y z] cell
        r (await (ctx/act c :place #js {:pos #js {:x x :y y :z z} :item item}))]
    (if (= "placed" (.-status r))
      (do (ledger/remember! c (ledger/confirm l cell))
          (ctx/update-mem! c assoc :failures 0)
          :again)
      (let [failures (inc (:failures (ctx/mem c) 0))
            settled (ledger/reconcile l block-at)]
        (ctx/update-mem! c assoc :failures failures)
        (ledger/remember! c settled)
        (if (< failures (max-failures))
          :again
          (finish! c settled (pillar/give-up :place-failed :detail (or (.-reason r) (.-status r)))))))))

(defn ^:async walk-to!
  "A go-to child to the cell [x y z] (range 0, no escalation). :continue while it waits; :ok on arrival, else :failed."
  [c [x y z]]
  (let [r (await (child/call-walk* c :step {:pos {:x x :y y :z z} :range 0 :ways #{:walk :clear-seal}
                                            :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) :ok
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
        (if (< n (max-failures)) :again (finish! c l (pillar/give-up :move-failed :at to)))))))

(defn ^:async recover!
  "Off the line: walk back to the last cell stood on while its floor holds, three times; else give up :off-line."
  [c l block-at step]
  (let [{:keys [stand recovers]} (ctx/mem c)]
    (if (and stand (< (or recovers 0) (max-failures)) (rules/solid-floor? block-at (update stand 1 dec)))
      (let [r (await (walk-to! c stand))]
        (when-not (= :continue r) (ctx/update-mem! c update :recovers (fnil inc 0)))
        (if (= :continue r) :continue :again))
      (finish! c l step))))

(defn ^:async look-ahead!
  "Look at each unsensed stand and head cell ahead of the feet, so a bridge never steps onto a cell it has not seen."
  [c feet]
  (doseq [cell [(ahead feet (:heading (:args c)) 1 0) (ahead feet (:heading (:args c)) 1 1)]]
    (when (look/unknown? (:primitives c) cell)
      (await (look/look-at! c cell)))))

(defn ^:async fetch-blocks!
  "Get the blocks step lacks with jobs.items.obtain: :continue while it works, :again once it is done, else the bridge's
  give-up (:too-few-blocks, the fetch's ending its :because). Without :fetch the give-up at once."
  [c l step]
  (if-not (:fetch (:args c))
    (finish! c l step)
    (let [item (:item (:args c))
          n (:short step)
          e (await (if item
                     (need/item! c item n {:return? true})
                     (need/blocks! c (choose/filler-names c :pillar) n {:return? true})))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) :again
        :else (finish! c l step)))))

(defn ^:async work!
  "One block of the bridge (or its end): look ahead, decide, act."
  [c]
  (await (pillar/land! c))
  (await (look-ahead! c (pl/feet-cell c)))
  (let [block-at (access/sensed-at (:primitives c) nil)
        seen (ledger/open-entries (ctx/view c))
        l (ledger/reconcile seen block-at)
        feet (pl/feet-cell c)
        step (next-step (inputs c l feet))]
    (when (not= l seen) (ledger/remember! c l))
    (ctx/update-mem! c #(-> % (update :start (fn [s] (or s feet))) (update :stand (fn [s] (or s feet)))))
    (cond
      (= :place (:step step)) (await (place! c l block-at step))
      (= :move (:step step)) (await (move! c l step))
      (= :off-line (:reason step)) (await (recover! c l block-at step))
      (= :too-few-blocks (:reason step)) (await (fetch-blocks! c l step))
      :else (finish! c l step))))

(defn ^:async step!
  "A step of the bridge: the walk back after a fetch first, else work!. :again, :continue while a child waits, or the ending."
  [c]
  (let [back (await (need/return! c))]
    (if back back (await (work! c)))))

(defn ^:async round
  "The whole bridge: step! until it ends, a pace between blocks."
  [c]
  (await (pace/steps! c #(step! c))))

(def meta {:position-bound? true})
