(ns jobs.access.leave-tunnel
  (:require [jobs.lib.ledger :as ledger]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.declined :as declined]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.access.stair :as stair]
            [jobs.access.tunnel :as tunnel]
            [jobs.gather.mine :as mine]
            [jobs.survival.dig-in :as dig-in]
            [jobs.lib.world :as known]))

(def doc
  "Leave a dead-end tunnel (the result of jobs.access.tunnel: :line :dug :torches): take its torches back and
  block its mouth off. One call does all of it (:continue only while a child waits on the world); a cut or
  restart reads the world again and goes on. A nil zone list declines
  the check (one warn leave-tunnel.declined).

  Torches first. For each torch that still stands, deepest first, the body walks to the cell after its site
  (a go-to child). It asks jobs.lib.access.rules/may-dig?. A refusal books the torch as left
  with that reason and is never forced. Otherwise the scaffold ledger entry is marked :removing, the torch is
  dug and the entry dropped once the cell is air. A cell still holding the torch is left :dig-failed. The drops
  are then collected (jobs.forestry.collect-drops, radius 3).

  Then it walks to the entry and seals the mouth. The mouth is the dug cells at or above the entry's floor that
  are open now and touch an open cell the tunnel did not dig (flat ground, a stair down, a hill face). Deeper
  cells are under rock and left. One cell per round, the lowest first, then the farthest from the body. Each
  asks may-place? right before. A refusal (:zone :footprint :own-body) books the cell open with its reason and
  is never forced. The cell must be within :reach of the eye (:out-of-reach). It is filled with the dug block's
  drop if carried (unless a :spare item), else the first carried building block (jobs.survival.dig-in) that is
  not :spare, else a :spare item that can be placed. Nothing to place: :no-blocks. A place that leaves the cell
  open: :place-failed.

  A walk that does not arrive (stair broken by an explosion or cave-in, a hole in its floor) is not the end. The
  body digs its own way out with jobs.access.stair to the entry's height (opposite the tunnel's :dir), first back along the tunnel's
  heading, then the other three (leave-tunnel.escape: info; warn for each stair that stopped), respecting zones unless the caller passed
  :ignore-zones?. The new stair is left as dug, nothing is placed (it is the body's own way out; sealing it risks walling in a body that returns). Once out, torches that
  cannot be reached are left (:walk-failed in :left) and the mouth is sealed. If the entry itself is
  unreachable it ends :done :open with :escaped true. If every attempt fails it ends :stopped :walk-failed
  with :escape (the stair results). The ledger entries of torches the stair or the fill destroyed are dropped
  when the job ends.

  Result {:status :done|:stopped :reason :sealed|:open|:walk-failed|:bad-args :at [x y z] :taken [cells] :left
  [{:cell :site :reason}] :filled [cells] :open [{:cell :reason}]}. :left holds torches not taken, including
  those still standing on a stop. Events: leave-tunnel.done (info, :sealed), leave-tunnel.open (warn, :open),
  leave-tunnel.stopped (warn, :walk-failed or :bad-args).

  :fetch (default true; false waits :no-tool; jobs.lib.fetch): the escape stair's missing pickaxe is got with jobs.items.get-tool, then
  the body walks back to the cell it stood on and goes on.")

(def args
  {:tunnel {:doc "the result of jobs.access.tunnel (:line :dug :torches)" :default nil}
   :spare {:doc "items filled with only when nothing else is carried (a caller's own haul)" :default []}
   :reach {:doc "mouth cells whose centre is this close to the eye are filled from the entry, in blocks" :default u/bucket-reach}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :fetch {:doc "get a missing pickaxe for the escape instead of waiting :no-tool (jobs.lib.fetch): true, a set of kinds or a map of limits; false waits :no-tool" :default true}})

(defn check [c]
  (if (and (nil? (known/zones c)) (not (:ignore-zones? (:args c))))
    (access/decline! c :leave-tunnel.declined "leave-tunnel" {:reason :no-zones})
    (fetch/declined-check c 'jobs.access.leave-tunnel)))

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

(defn gone-cells
  "The cells of the tunnel's torches that have a ledger entry l and are read as something other than a torch. An
  unread cell (nil: not loaded) is not gone."
  [l tunnel block-at]
  (->> (:torches tunnel)
       (map :cell)
       (filter #(and (ledger/entry-at l %) (let [n (block-at %)] (and n (not (tunnel/torch-blocks n))))))))

(defn forget-gone!
  "Drop the ledger entries of the tunnel's torches whose cell holds no torch any more (the escape stair or the
  mouth's fill dug or covered it)."
  [c tunnel block-at]
  (let [l (ledger/open-entries (ctx/view c))
        gone (gone-cells l tunnel block-at)]
    (when (seq gone)
      (ledger/remember! c (reduce ledger/drop-cell l gone)))))

(defn finish!
  "Hand the result over and end, with the event of its reason; on a stop the torches still standing are left too."
  [c status reason detail]
  (let [{:keys [tunnel]} (:args c)
        m (ctx/mem c)
        block-at (:block-at (stair/rules-in c (feet-of c)))
        stopped? (= :stopped status)
        _ (forget-gone! c tunnel block-at)
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
  "Walk to cell: :continue while the walk waits on the world, :again once there. A walk that did not arrive starts the escape (the way out
  does not need the tunnel's stair); once out, giveup is called with the walk instead."
  [c cell giveup]
  (let [r (await (stair/walk-into! c :walk cell))]
    (cond
      (= :continue r) :continue
      (and (:arrived r) (= cell (feet-of c))) :again
      (:escaped (ctx/mem c)) (giveup r)
      :else (do (ctx/update-mem! c assoc :escape {:i 0 :cell cell :walk r :results []})
                :again))))

(def opposite {:north :south :south :north :east :west :west :east})

(defn escape-attempts
  "The stairs to try, as [{:heading :ignore-zones?}]: back along the tunnel's heading first, then the others, all
  respecting zones unless the caller passed :ignore-zones?."
  [heading ignore?]
  (let [order (distinct (remove nil? (concat [(opposite heading)] [:north :east :south :west])))]
    (mapv (fn [h] {:heading h :ignore-zones? ignore?}) order)))

(defn escape-stair
  "The stair args out of a tunnel back to the entry's height: the other way than the tunnel's own stair."
  [{:keys [dir]} entry-y]
  {:dir (if (= :up dir) :down :up) :y entry-y})

(defn ^:async escape!
  "One attempt of the way out: a stair to the entry's height (opposite the tunnel's :dir) along the next heading; done, the body is out and the
  rounds go on; stopped or refused by a zone, the next heading; none left, the walk failure stands; the stair declined (:declined: its
  wait, e.g. :no-tool, reaches this job's job.waiting)."
  [c]
  (let [{:keys [tunnel]} (:args c)
        ignore? (boolean (:ignore-zones? (:args c)))
        {:keys [i cell walk results]} (:escape (ctx/mem c))
        attempts (escape-attempts (:heading (:line tunnel)) ignore?)
        attempt (get attempts i)
        entry-y (second (first (tunnel/line-cells (:line tunnel))))]
    (cond
      (nil? attempt)
      (finish! c :stopped :walk-failed {:cell cell :walk walk :escape results})
      :else
      (let [slot (keyword (str "escape-" i))
            sargs (assoc (escape-stair (:line tunnel) entry-y)
                         :heading (:heading attempt) :fetch false :ignore-zones? (:ignore-zones? attempt))
            r (await (declined/call-child! c slot 'jobs.access.stair sargs))
            res (when (= :done r) (ctx/child-result c slot))
            refused (when (= :declined r) (blocks/child-wait c slot 'jobs.access.stair sargs))]
        (cond
          (= :refused (:reason refused))
          (do (declined/begin! c)
              (ctx/update-mem! c update :escape #(-> % (assoc :i (inc i)) (update :results conj {:reason :refused :heading (:heading attempt)})))
              :again)
          (= :declined r) :declined
          (nil? res) :continue
          (= :done (:status res))
          (do (ctx/emit! c :leave-tunnel.escape :info {:at (feet-of c) :heading (:heading attempt) :ignore-zones? (:ignore-zones? attempt)
                                                      :text (str "leave-tunnel dug its own way out " (name (:heading attempt)))})
              (ctx/update-mem! c #(-> % (dissoc :escape) (assoc :escaped true)))
              :again)
          :else (do (ctx/update-mem! c update :escape #(-> % (assoc :i (inc i)) (update :results conj (select-keys res [:reason :cell :heading]))))
                    :again))))))

(defn book-left! [c cell site reason]
  (ctx/update-mem! c update :left (fnil conj []) {:cell cell :site site :reason reason})
  :again)

(defn book-open! [c cell reason]
  (ctx/update-mem! c update :open (fnil conj []) {:cell cell :reason reason})
  :again)

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
            outcome (await (blocks/dig-cell! c (cell-pos cell) {:accept #{:fluid-adjacent :falling-block :under-feet}
                                                                :ignore-zones? true}))]
        (cond
          (= :continue outcome) :continue
          (not (rules/air (block-at cell))) (book-left! c cell site :dig-failed)
          :else (do (ledger/remember! c (ledger/drop-cell l cell))
                    (ctx/update-mem! c #(-> % (update :taken (fnil conj []) cell) (assoc :collect true)))
                    :again))))))

(defn ^:async collect!
  "Pick the dropped torches up near the body; the flag clears when the pick-up ends (done, or it cannot run)."
  [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius 3 :filter ["torch"]}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c dissoc :collect) :again))))

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
      (> (u/eye-dist (u/self-pos c) cell) reach) (book-open! c cell :out-of-reach)
      (not (:ok verdict)) (book-open! c cell (:reason verdict))
      (nil? item) (book-open! c cell :no-blocks)
      :else (let [outcome (await (blocks/place-cell! c (cell-pos cell) item {:ignore-zones? true}))
                  n (block-at cell)]
              (cond
                (= :continue outcome) :continue
                (and n (not (rules/replaceable n))) (do (ctx/update-mem! c update :filled (fnil conj []) cell) :again)
                :else (book-open! c cell :place-failed))))))

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
      :else (await (fill! c (first (sort-by (juxt #(% 1) #(- (u/eye-dist body %))) todo)))))))

(defn ^:async work-round [c]
  (declined/begin! c)
  (let [{:keys [tunnel]} (:args c)
        m (ctx/mem c)
        torches (when (:line tunnel) (standing (:block-at (stair/rules-in c (feet-of c))) tunnel m))]
    (cond
      (not (:line tunnel)) (finish! c :stopped :bad-args {:why "tunnel must be the result of jobs.access.tunnel"})
      (:escape m) (await (escape! c))
      (:collect m) (await (collect! c))
      (seq torches) (await (take-torch! c (first torches)))
      :else (await (seal! c)))))

(defn ^:async step!
  "One piece of the way out: the fetch part first (the escape stair's booked wait), then the work. :again, :continue
  (a child waits on the world), :declined or :done."
  [c]
  (let [r (await (fetch/step! c 'jobs.access.leave-tunnel (fetch/booked-wait c) {:return? true}))]
    (cond
      r r
      :else (await (work-round c)))))

(defn ^:async round
  "The whole way out: step! until it ends, a pace between; :continue after stair/max-steps of them or while a child
  waits on the world."
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) stair/max-steps) (step! c) :continue)))))
