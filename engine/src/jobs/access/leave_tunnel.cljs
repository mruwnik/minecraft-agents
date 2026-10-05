(ns jobs.access.leave-tunnel
  (:require [engine.access.ledger :as ledger]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.util :as u]
            [jobs.access.stair :as stair]
            [jobs.access.tunnel :as tunnel]
            [jobs.build.from-plan :as from-plan]
            [jobs.gather.mine :as mine]
            [jobs.survival.dig-in :as dig-in]))

(def doc
  "Leave a dead-end tunnel (the result of jobs.access.tunnel: :line :dug :torches): take its torches back and block
  its mouth off. Every round is read from the world, so a cut or a restart goes on. First the torches that still
  stand (a torch or wall torch in the cell of a site of :torches, deepest first, none already left): the body walks
  to the cell after the site (jobs.debug.walk-plan as a child; not arriving: :stopped :walk-failed), asks
  engine.access.rules may-dig? (a refusal books the torch as left with its reason, never forced), marks the scaffold
  ledger entry :removing before the dig and drops it when the cell is air; a cell still holding the torch after the dig
  is left :dig-failed. After a dig the drops are collected (jobs.forestry.collect-drops, radius 3, torches). Then the
  body walks to the entry and seals the mouth from there: the dug cells at or above the entry's floor (entry y less 1)
  that are open now and touch an open cell the tunnel did not dig (flat ground, a stair down: the cells of the first
  steps at ground level, flush when filled; a stair into a hill: its face). Deeper cells are under rock and left.
  One cell per round, the lowest first, then the farthest from the body; each asks may-place? right before (a
  refusal books the cell open with its reason: :zone :footprint :own-body, never forced), must be within :reach of
  the eye (:out-of-reach) and is filled with the dug block's drop if carried (unless it is a :spare item), else
  the first carried building block (jobs.survival.dig-in) that is no :spare, else a :spare item that can be placed; nothing: :no-blocks;
  a place that leaves the cell open: :place-failed. Nil zone list declines the check (one leave-tunnel.declined warn).
  Hands over {:status :done|:stopped :reason :sealed|:open|:walk-failed|:bad-args :at [x y z] :taken [cells]
  :left [{:cell :site :reason}] (torches not taken, those still standing on a stop included) :filled [cells] :open
  [{:cell :reason}]} (:sealed: every mouth cell is filled; :open: some are left open) and emits leave-tunnel.done
  (info, :sealed), leave-tunnel.open (warn, :open) or leave-tunnel.stopped (warn, :walk-failed, :bad-args).
  A walk that does not arrive (the stair broken by an explosion or a cave-in, a hole in its floor) is not the end: the
  body digs its own way out with jobs.access.stair :up to the entry's height, first back along the tunnel's heading,
  then the other three (leave-tunnel.escape, info, per stair stopped warn); only when every heading stopped for an
  access reason (:zone :claim :footprint) does it try them again with :ignore-zones? as the last resort. The new stair
  is left as dug (nothing is placed, no item of another is taken). Out, the torches that cannot be reached are left
  (:walk-failed in :left), the mouth is sealed, and the entry unreachable ends :done :open with :escaped true.
  Every attempt failing ends :stopped :walk-failed with :escape (the stair results).")

(def args
  {:tunnel {:doc "the result of jobs.access.tunnel (:line :dug :torches)" :default nil}
   :spare {:doc "items filled with only when nothing else is carried (a caller's own haul)" :default []}
   :reach {:doc "mouth cells whose centre is this close to the eye are filled from the entry, in blocks" :default 4.5}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(defn check [c]
  (if (and (nil? (ctx/zones c)) (not (:ignore-zones? (:args c))))
    (access/decline! c :leave-tunnel.declined "leave-tunnel" {:reason :no-zones})
    true))

(def faces [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn open?
  "Whether the cell is loaded and holds air, a fluid or a plant: a block placed there takes its place."
  [block-at cell]
  (let [n (block-at cell)]
    (boolean (and n (rules/replaceable n)))))

(defn mouth
  "The cells of the dug blocks [{:cell :block}] that seal the tunnel off, from the entry cell: those at or above the
  entry's floor (entry y less 1) that are open now and have an open face neighbour the tunnel did not dig."
  [dug [_ entry-y _] block-at]
  (let [dug-cells (set (map :cell dug))
        outside? (fn [cell] (some #(let [n (stair/add cell %)] (and (not (dug-cells n)) (open? block-at n))) faces))]
    (vec (filter #(and (>= (% 1) (dec entry-y)) (open? block-at %) (outside? %)) (distinct (map :cell dug))))))

(defn seal-item
  "The item to fill a cell that held block with, from the carried item names: the block's drop when it can be placed
  and is no spare, else the first carried building block that is no spare, else the first carried spare that can be
  placed (a building block, or the dug block itself); nil when there is none."
  [carried block spare]
  (let [have? (set carried)
        spare? (set spare)
        drop (mine/item-name {:block block})
        placeable? #(or (= % block) (some #{%} dig-in/building-blocks))]
    (or (when (and (have? drop) (not (spare? drop)) (placeable? drop)) drop)
        (first (filter #(and (have? %) (not (spare? %))) dig-in/building-blocks))
        (first (filter #(and (have? %) (placeable? %)) spare)))))

(defn feet-of [c] (stair/feet-of c))

(defn cell-pos [[x y z]] {:x x :y y :z z})

(defn left-cells [m] (set (map :cell (:left m))))

(defn standing
  "The torches of tunnel that still stand and are not left, deepest site first."
  [block-at tunnel m]
  (let [left (left-cells m)]
    (->> (:torches tunnel)
         (filter #(and (tunnel/torch-blocks (block-at (:cell %))) (not (left (:cell %)))))
         (sort-by (comp - :site)))))

(defn finish!
  "Hand the result over and end, with the event of its reason; on a stop the torches still standing are left too."
  [c status reason detail]
  (let [{:keys [tunnel]} (:args c)
        m (ctx/mem c)
        block-at (:block-at (stair/rules-in c (feet-of c)))
        stopped? (= :stopped status)
        still (when stopped?
                (map (fn [{:keys [cell site]}] {:cell cell :site site :reason reason}) (standing block-at tunnel m)))
        result (merge {:status status :reason reason :at (feet-of c) :taken (:taken m []) :left (into (:left m []) still)
                       :filled (:filled m []) :open (:open m [])}
                      detail)
        text (str "leave-tunnel " (name reason) ": took " (count (:taken result)) " torches, filled "
                  (count (:filled result)) ", left " (count (:left result)) " torches and " (count (:open result)) " cells")]
    (ctx/result! c result)
    (case reason
      :sealed (ctx/emit! c :leave-tunnel.done :info (assoc result :text text))
      :open (ctx/emit! c :leave-tunnel.open :warn (assoc result :text text))
      (ctx/emit! c :leave-tunnel.stopped :warn (assoc result :text text)))
    :done))

(defn ^:async walk-to!
  "Walk to cell: :continue while walking or once there. A walk that did not arrive starts the escape (the way out
  does not need the tunnel's stair); once out, giveup is called with the walk instead."
  [c cell giveup]
  (let [r (await (tunnel/walk-to! c :walk cell))]
    (cond
      (= :continue r) :continue
      (and (= :arrived (:status r)) (= cell (feet-of c))) :continue
      (:escaped (ctx/mem c)) (giveup r)
      :else (do (ctx/update-mem! c assoc :escape {:i 0 :cell cell :walk r :results []})
                :continue))))

(def opposite {:north :south :south :north :east :west :west :east})

(def access-reasons #{:zone :claim :footprint :no-zones})

(defn escape-attempts
  "The stairs to try, as [{:heading :ignore-zones?}]: back along the tunnel's heading first, then the others, all
  respecting zones; then, when zones are not ignored already, the same again with :ignore-zones?."
  [heading ignore?]
  (let [order (distinct (remove nil? (concat [(opposite heading)] [:north :east :south :west])))
        pass (fn [ig] (mapv (fn [h] {:heading h :ignore-zones? ig}) order))]
    (if ignore? (pass true) (into (pass false) (pass true)))))

(defn zones-blocked?
  "Whether every attempt so far stopped, and one of them for an access reason."
  [results]
  (boolean (some #(access-reasons (:reason %)) results)))

(defn ^:async escape!
  "One attempt of the way out: a stair up to the entry's height along the next heading; done, the body is out and the
  rounds go on; stopped, the next heading; none left, the walk failure stands; the stair declined (:declined: its
  wait, e.g. :no-tool, reaches this job's job.waiting)."
  [c]
  (let [{:keys [tunnel]} (:args c)
        ignore? (boolean (:ignore-zones? (:args c)))
        {:keys [i cell walk results]} (:escape (ctx/mem c))
        attempts (escape-attempts (:heading (:line tunnel)) ignore?)
        attempt (get attempts i)
        entry-y (second (first (tunnel/line-cells (:line tunnel))))]
    (cond
      (or (nil? attempt) (and (:ignore-zones? attempt) (not ignore?) (not (zones-blocked? results))))
      (finish! c :stopped :walk-failed {:cell cell :walk walk :escape results})
      :else
      (let [r (await (ctx/call-child c (keyword (str "escape-" i)) 'jobs.access.stair
                                     {:dir :up :heading (:heading attempt) :y entry-y
                                      :ignore-zones? (:ignore-zones? attempt)}))
            res (when (= :done r) (ctx/child-result c (keyword (str "escape-" i))))]
        (cond
          (= :declined r) :declined
          (nil? res) :continue
          (= :done (:status res))
          (do (ctx/emit! c :leave-tunnel.escape :info {:at (feet-of c) :heading (:heading attempt) :ignore-zones? (:ignore-zones? attempt)
                                                      :text (str "leave-tunnel dug its own way out " (name (:heading attempt)))})
              (ctx/update-mem! c #(-> % (dissoc :escape) (assoc :escaped true)))
              :continue)
          :else (do (ctx/update-mem! c update :escape #(-> % (assoc :i (inc i)) (update :results conj (select-keys res [:reason :cell :heading]))))
                    :continue))))))

(defn book-left! [c cell site reason]
  (ctx/update-mem! c update :left (fnil conj []) {:cell cell :site site :reason reason})
  :continue)

(defn book-open! [c cell reason]
  (ctx/update-mem! c update :open (fnil conj []) {:cell cell :reason reason})
  :continue)

(defn ^:async dig-torch!
  "From the cell after its site: the torch's dig asked of the rules, its ledger entry marked :removing before the
  dig and dropped once the cell is air."
  [c {:keys [cell site]}]
  (let [in (stair/rules-in c (feet-of c))
        block-at (:block-at in)
        verdict (rules/may-dig? (assoc in :cell cell))]
    (if-not (:ok verdict)
      (book-left! c cell site (:reason verdict))
      (let [l (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)
            _ (ledger/remember! c (ledger/begin-removal l cell))
            _ (await (ctx/act c :dig (clj->js {:pos (cell-pos cell)})))]
        (if-not (rules/air (block-at cell))
          (book-left! c cell site :dig-failed)
          (do (ledger/remember! c (ledger/drop-cell l cell))
              (ctx/update-mem! c #(-> % (update :taken (fnil conj []) cell) (assoc :collect true)))
              :continue))))))

(defn ^:async collect!
  "Pick the dropped torches up near the body; the flag clears when none is left."
  [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius 3 :filter ["torch"]}))]
    (when (= :done r) (ctx/update-mem! c dissoc :collect))
    :continue))

(defn ^:async take-torch!
  "Walk to the cell after the torch's site and take it back."
  [c {:keys [site] :as torch}]
  (let [stand ((tunnel/line-cells (:line (:tunnel (:args c)))) (inc site))]
    (if (= stand (feet-of c))
      (await (dig-torch! c torch))
      (await (walk-to! c stand (fn [_] (book-left! c (:cell torch) site :walk-failed)))))))

(defn ^:async fill!
  "Fill one mouth cell from the entry, or book it open with why not."
  [c cell]
  (let [{:keys [tunnel spare reach]} (:args c)
        p (:primitives c)
        in (stair/rules-in c (feet-of c))
        block-at (:block-at in)
        verdict (rules/may-place? (assoc in :cell cell))
        block (:block (first (filter #(= cell (:cell %)) (:dug tunnel))))
        item (seal-item (map :name (filter #(pos? (:count %)) (u/inventory p))) block spare)]
    (cond
      (> (from-plan/eye-dist (u/self-pos c) cell) reach) (book-open! c cell :out-of-reach)
      (not (:ok verdict)) (book-open! c cell (:reason verdict))
      (nil? item) (book-open! c cell :no-blocks)
      :else (do (await (ctx/act c :place (clj->js {:pos (cell-pos cell) :item item})))
                (let [n (block-at cell)]
                  (if (and n (not (rules/replaceable n)))
                    (do (ctx/update-mem! c update :filled (fnil conj []) cell) :continue)
                    (book-open! c cell :place-failed)))))))

(defn ^:async seal!
  "At the entry: fill the next mouth cell, lowest first, then the farthest; none left ends the job."
  [c]
  (let [{:keys [tunnel]} (:args c)
        m (ctx/mem c)
        entry (first (tunnel/line-cells (:line tunnel)))
        in (stair/rules-in c (feet-of c))
        booked (set (map :cell (:open m)))
        body (u/self-pos c)
        todo (remove booked (mouth (:dug tunnel) entry (:block-at in)))]
    (cond
      (not= entry (feet-of c)) (await (walk-to! c entry #(finish! c :done :open {:escaped true :walk %})))
      (empty? todo) (finish! c :done (if (empty? (:open m)) :sealed :open) {})
      :else (await (fill! c (first (sort-by (juxt #(% 1) #(- (from-plan/eye-dist body %))) todo)))))))

(defn ^:async round [c]
  (let [{:keys [tunnel]} (:args c)
        m (ctx/mem c)
        torches (when (:line tunnel) (standing (:block-at (stair/rules-in c (feet-of c))) tunnel m))]
    (cond
      (not (:line tunnel)) (finish! c :stopped :bad-args {:why "tunnel must be the result of jobs.access.tunnel"})
      (:escape m) (await (escape! c))
      (:collect m) (await (collect! c))
      (seq torches) (await (take-torch! c (first torches)))
      :else (await (seal! c)))))
