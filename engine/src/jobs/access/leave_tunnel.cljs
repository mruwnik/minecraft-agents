(ns jobs.access.leave-tunnel
  (:require [jobs.lib.block-sets :as bs]
            [engine.contract :as contract]
            [jobs.lib.choose :as choose]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [jobs.lib.need :as need]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.pace :as pace]
            [jobs.lib.torch :as torch]
            [plan.shape :as shape]
            [jobs.lib.util :as u]
            [jobs.access.stair :as stair]
            [jobs.access.stair.judge :as judge]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.child :as child]
            [jobs.access.tunnel.line :as ln]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]))

(def doc
  "Leave a dead-end tunnel (the result of jobs.access.tunnel: :line :dug :torches): take its torches back and
  block its mouth off. One call does all of it (:continue only while a child waits on the world); a cut or
  restart reads the world again and goes on. A nil zone list waits (condition :no-zones).

  Torches first. For each torch that still stands, deepest first, the body walks to the cell after its site
  (a go-to child). It asks jobs.lib.access.rules/may-dig?. A refusal books the torch as left
  with that reason and is never forced. Otherwise the scaffold ledger entry is marked :removing, the torch is
  dug and the entry dropped once the cell is air. A cell still holding the torch is left :dig-failed. The drops
  are then collected (jobs.items.collect-drops, radius 3).

  Then it walks to the entry (after a look round when the way there is unseen) and seals the mouth. The mouth is the dug cells at or above the entry's floor that
  are open now and touch an open cell the tunnel did not dig (flat ground, a stair down, a hill face). Deeper
  cells are under rock and left. One cell per round, the lowest first, then the farthest from the body. Each
  asks may-place? right before. A refusal (:zone :footprint :own-body) books the cell open with its reason and
  is never forced. The cell must be within :reach of the eye (:out-of-reach). It is filled with the dug block's
  drop if carried (unless a :spare item), else the first carried building block (jobs.survival.shut-in) that is
  not :spare, else a :spare item that can be placed, by a jobs.blocks.fill-cells child. Nothing to place: :no-blocks. A place that leaves the cell
  open: :place-failed.

  A walk that does not arrive (stair broken by an explosion or cave-in, a hole in its floor) is not the end. The
  body digs its own way out with jobs.access.stair to the entry's height (opposite the tunnel's :dir), first back along the tunnel's
  heading, then the other three (leave-tunnel.escape: info; warn for each stair that stopped), respecting zones unless the caller passed
  :ignore-zones?. The new stair is left as dug, nothing is placed (it is the body's own way out; sealing it risks walling in a body that returns). Once out, torches that
  cannot be reached are left (:walk-failed in :left) and the mouth is sealed. If the entry itself is
  unreachable it ends nothing-possible :open with :escaped true. If every attempt fails it ends :stopped :walk-failed
  with :escape (the stair results). The ledger entries of torches the stair or the fill destroyed are dropped
  when the job ends.

  Ends :done :sealed with data {:reason :at [x y z] :taken [cells] :left [{:cell :site :reason}] :filled [cells]
  :open []}; :left holds torches not taken. A mouth cell left open (:open [{:cell :reason}]) ends nothing-possible
  :open with the same data; :escaped true: the entry itself was unreachable.
  Impossible :bad-args (no :line in the tunnel). Or condition-failed with the cause :no-tool (the escape stair has no pickaxe and
  :fetch is off; a failed fetch digs the stair bare-handed), or nothing-possible with :walk-failed (every escape failed; data
  :escape holds the stair endings). Event: leave-tunnel.escape.

  :fetch (default true; false ends :no-tool): the escape stair's missing pickaxe is got by the stair's dig child (jobs.lib.need), then
  the body walks back onto the stair. With nothing to fill the mouth with, building blocks are
  got the same way, once (jobs.items.obtain); a failed fetch or :fetch off books the cell open :no-blocks.")

(a/defargs args
  {:tunnel {:doc "the result of jobs.access.tunnel (:line :dug :torches)" :spec map? :default nil}
   :spare {:doc "items filled with only when nothing else is carried (a caller's own haul)" :spec (a/coll-of a/item?) :default []}
   :reach {:doc "mouth cells whose centre is this close to the eye are filled from the entry, in blocks" :spec (a/num-in 0 nil) :default u/bucket-reach}
   :ignore-zones? :shared
   :fetch {:doc "get a missing pickaxe for the escape with jobs.items.get-tool instead of ending :no-tool: a set of kinds or a map of limits counts as true; false ends :no-tool" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 900 :doc "Seconds a way out may take: the torches, the escape, the seal." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def meta {:category :phase :part-of '[jobs.access.tunnel]})

(def safe? ready/safe-if-lit)

(def fit? ready/ok)

(def target [:tunnel])

(def defaults {:checks {:fit? '(always)} :spend {:job-time ::job-time-s}})

(def bad-args
  "Never :bad-args unless :tunnel is the result of jobs.access.tunnel."
  (checks/args-fit "tunnel must be the result of jobs.access.tunnel" #(:line (:tunnel (:args %)))))

(def can-start? (cond/and bad-args checks/zones-read))

(defn quote-of [c]
  (let [n (count (:torches (:tunnel (:args c))))]
    {:uses {:job-time [1 (+ 3 (* 4 n)) (max (+ 3 (* 4 n)) (time-s))]}}))

(def faces rules/neighbour-deltas)

(defn open?
  "Whether the cell is loaded and holds air, a fluid or a plant: a block placed there takes its place."
  [block-at cell]
  (let [n (block-at cell)]
    (boolean (and n (bs/replaceable n)))))

(defn mouth
  "The cells of the dug blocks [{:cell :block}] that seal the tunnel off, from the entry cell: those at or above the
  entry's floor (entry y less 1) that are open now and have an open face neighbour the tunnel did not dig."
  [dug [_ entry-y _] block-at]
  (let [dug-cells (set (map :cell dug))
        outside? (fn [cell] (some #(let [n (shape/add cell %)] (and (not (dug-cells n)) (open? block-at n))) faces))]
    (vec (filter #(and (>= (% 1) (dec entry-y)) (open? block-at %) (outside? %)) (distinct (map :cell dug))))))

(defn seal-item
  "The item to fill a cell that held block with, from the carried item names: the block's drop when it can be placed
  and is no spare, else the first carried building block that is no spare, else the first carried spare that can be
  placed (a building block of builds, or the dug block itself); nil when there is none."
  [carried block spare builds]
  (let [have? (set carried)
        spare? (set spare)
        drop (blocks/item-name {:block block})
        placeable? #(or (= % block) (some #{%} builds))]
    (or (when (and (have? drop) (not (spare? drop)) (placeable? drop)) drop)
        (first (filter #(and (have? %) (not (spare? %))) builds))
        (first (filter #(and (have? %) (placeable? %)) spare)))))

(defn cell-pos [[x y z]] {:x x :y y :z z})

(def look-turns "Most looks along the way back before the walk to the entry." 16)

(defn left-cells [m] (set (map :cell (:left m))))

(defn standing
  "The torches of tunnel that still stand and are not left, deepest site first."
  [block-at tunnel m]
  (let [left (left-cells m)]
    (->> (:torches tunnel)
         (filter #(and (torch/torch-blocks (block-at (:cell %))) (not (left (:cell %)))))
         (sort-by (comp - :site)))))

(defn gone-cells
  "The cells of the tunnel's torches that have a ledger entry l and are read as something other than a torch. An
  unread cell (nil: not loaded or not sensed) is not gone."
  [l tunnel block-at]
  (->> (:torches tunnel)
       (map :cell)
       (filter #(and (ledger/entry-at l %) (let [n (block-at %)] (and n (not (torch/torch-blocks n))))))))

(defn forget-gone!
  "Drop the ledger entries of the tunnel's torches whose cell holds no torch any more (the escape stair or the
  mouth's fill dug or covered it)."
  [c tunnel block-at]
  (let [l (ledger/open-entries (ctx/view c))
        gone (gone-cells l tunnel block-at)]
    (when (seq gone)
      (ledger/remember! c (reduce ledger/drop-cell l gone)))))

(def condition-reasons "Stop reasons that read as a failed condition; any other stop is nothing possible." #{:no-tool})

(defn result-of
  "The result of the way out now: {:reason :at :taken :left :filled :open} plus detail; on a stop the torches still
  standing are left too."
  [c reason detail still]
  (let [m (ctx/mem c)
        result (merge {:reason reason :at (reach/standing-vec (:primitives c)) :taken (:taken m []) :left (into (:left m []) still)
                       :filled (:filled m []) :open (:open m [])}
                      detail)]
    (assoc result :text (str "leave-tunnel " (name reason) ": took " (count (:taken result)) " torches, filled "
                             (count (:filled result)) ", left " (count (:left result)) " torches and " (count (:open result)) " cells"))))

(defn reached
  "Whether the way out is complete (:sealed), or nil: every torch of the tunnel is taken, left or seen gone, the
  entry is seen and no mouth cell is open (none booked open either). A cell not seen is not known."
  [block-at tunnel m]
  (let [dealt (set (concat (:taken m) (map :cell (:left m))))
        entry (first (ln/line-cells (:line tunnel)))
        gone? #(or (dealt %) (some-> (block-at %) (as-> n (not (torch/torch-blocks n)))))]
    (when (and (:line tunnel) (block-at entry) (every? gone? (map :cell (:torches tunnel)))
               (empty? (mouth (:dug tunnel) entry block-at)))
      :sealed)))

(defn done?
  "Ok once no torch of the tunnel stands and its mouth is shut (already, before the start); else the torches taken."
  [c]
  (let [{:keys [tunnel]} (:args c)
        m (ctx/mem c)
        reason (when-not (:collect m) (reached (access/seen-at (:primitives c)) tunnel m))]
    (if reason
      (contract/ok reason "the tunnel is left" :units :torches :done (count (:taken m)) :left 0 :result (result-of c reason {} nil))
      (contract/later :sealing "the mouth is to be sealed" :units :torches :done (count (:taken m))))))

(defn ^:async cleanup
  "After a way out that ended :done: the ledger entries of torches the stair or the fill destroyed are dropped."
  [c ending]
  (when (= :done (:kind ending))
    (forget-gone! c (:tunnel (:args c)) (access/sensed-at (:primitives c) nil)))
  (ctx/end c :done nil {}))

(defn finish!
  "Hand the ending over; on a stop its event, with the torches still standing left too. reason is a keyword, or a cause
  map with :reason. A :done ending's ledger tidy and event are cleanup's."
  [c status cause detail]
  (let [{:keys [tunnel]} (:args c)
        reason (if (map? cause) (:reason cause) cause)
        stopped? (= :stopped status)
        block-at (access/sensed-at (:primitives c) nil)
        still (when stopped?
                (map (fn [{:keys [cell site]}] {:cell cell :site site :reason reason}) (standing block-at tunnel (ctx/mem c))))
        result (result-of c reason detail still)]
    (if stopped?
      (do (forget-gone! c tunnel block-at)
          (ctx/end c (cond (= :bad-args reason) :impossible (condition-reasons reason) :condition-failed :else :nothing-possible) cause result))
      (ctx/end c :done nil result))))

(defn open!
  "End with the mouth not sealed: nothing possible :open, the cells booked open (and :escaped when the entry was
  unreachable) in the data; the caller may accept it."
  [c detail]
  (let [result (result-of c :open detail nil)]
    (forget-gone! c (:tunnel (:args c)) (access/sensed-at (:primitives c) nil))
    (ctx/end c :nothing-possible :open result)))

(defn ^:async walk-to!
  "Walk to cell: :continue while the walk waits on the world, :again once there. A walk that did not arrive starts the escape (the way out
  does not need the tunnel's stair); once out, giveup is called with the walk instead."
  [c cell giveup]
  (let [e (await (child/walk-into!* c :walk cell))
        r (:data e)]
    (cond
      (= :continue e) :continue
      (and (:arrived r) (= cell (reach/standing-vec (:primitives c)))) :again
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
  rounds go on; stopped or refused by a zone, the next heading; none left, the walk failure stands; a stair without a pickaxe gets one
  (:fetch) or stops the job :no-tool."
  [c]
  (let [{:keys [tunnel]} (:args c)
        ignore? (boolean (:ignore-zones? (:args c)))
        {:keys [i cell walk results]} (:escape (ctx/mem c))
        attempts (escape-attempts (:heading (:line tunnel)) ignore?)
        attempt (get attempts i)
        entry-y (second (first (ln/line-cells (:line tunnel))))]
    (if (nil? attempt)
      (finish! c :stopped :walk-failed {:cell cell :walk walk :escape results})
      (let [slot (keyword (str "escape-" i))
            sargs (assoc (escape-stair (:line tunnel) entry-y)
                         :heading (:heading attempt) :fetch (:fetch (:args c)) :need-drop (not (:by-hand (ctx/mem c))) :settings {:jobs.lib.trip/spent-uses 0} :ignore-zones? (:ignore-zones? attempt))
            e (await (ctx/call c slot 'jobs.access.stair sargs))
            why (when (map? e) (contract/cause-key e))
            next-heading! #(do (ctx/update-mem! c update :escape (fn [m] (-> m (assoc :i (inc i)) (update :results conj %)))) :again)]
        (cond
          (= :continue e) :continue
          (= :done (:kind e))
          (do (ctx/emit! c :leave-tunnel.escape :info {:at (reach/standing-vec (:primitives c)) :heading (:heading attempt) :ignore-zones? (:ignore-zones? attempt)
                                                      :text (str "leave-tunnel dug its own way out " (name (:heading attempt)))})
              (ctx/update-mem! c #(-> % (dissoc :escape) (assoc :escaped true)))
              :again)
          (= :refused why) (next-heading! {:reason :refused :heading (:heading attempt)})
          (and (= :no-tool why) (:fetch (:args c)) (not (:by-hand (ctx/mem c))))
          (do (ctx/update-mem! c assoc :by-hand true)
              (ctx/emit! c :leave-tunnel.fetch-failed :warn {:because (:because (:cause e))
                                                            :text "leave-tunnel could not get a pickaxe: digging its way out bare-handed"})
              :again)
          (= :no-tool why) (finish! c :stopped (:cause e) {})
          :else (next-heading! (assoc (select-keys (:data e) [:cell]) :reason why :heading (:heading attempt))))))))

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
  (let [in (judge/rules-in c (reach/standing-vec (:primitives c)))
        block-at (:block-at in)
        verdict (rules/may-dig? (assoc in :cell cell))]
    (cond
      (not (:ok verdict)) (book-left! c cell site (:reason verdict))
      (some #(= :under-feet (:reason %)) (:hazards verdict)) (book-left! c cell site :under-feet)
      :else
      (let [l (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)
            _ (ledger/remember! c (ledger/begin-removal l cell))
            outcome (await (blocks/dig-cell! c (cell-pos cell) {:accept #{:fluid-adjacent :falling-block}
                                                                :ignore-zones? true}))]
        (cond
          (= :continue outcome) :continue
          (not (bs/air (block-at cell))) (book-left! c cell site :dig-failed)
          :else (do (ledger/remember! c (ledger/drop-cell l cell))
                    (ctx/update-mem! c #(-> % (update :taken (fnil conj []) cell) (assoc :collect true)))
                    :again))))))

(defn ^:async collect!
  "Pick the dropped torches up near the body; the flag clears when the pick-up ends (done, or it cannot run)."
  [c]
  (let [r (await (ctx/call c :collect 'jobs.items.collect-drops {:radius 3 :items ["torch"]}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c dissoc :collect) :again))))

(defn ^:async take-torch!
  "Walk to the cell after the torch's site and take it back."
  [c {:keys [site] :as torch}]
  (let [stand ((ln/line-cells (:line (:tunnel (:args c)))) (inc site))]
    (if (= stand (reach/standing-vec (:primitives c)))
      (await (dig-torch! c torch))
      (await (walk-to! c stand (fn [_] (book-left! c (:cell torch) site :walk-failed)))))))

(defn fetch-opts
  "The limits {:how :explore :minutes} for fetching blocks, nil when :fetch does not allow items."
  [c]
  (when-let [o (need/opts c 'jobs.access.leave-tunnel)]
    (when (contains? (:what o) :item) (select-keys o [:how :explore :minutes]))))

(defn ^:async fetch-blocks!
  "Get building blocks for the open mouth cells: :continue while the fetch waits, :again once got, nil when the fetch
  failed (the cell is then booked open :no-blocks)."
  [c short]
  (let [e (await (need/blocks! c (choose/filler-names c :build) short (assoc (fetch-opts c) :return? true)))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) :again)))

(defn ^:async fill!
  "Fill one mouth cell from the entry, or book it open with why not; short is how many cells are still to fill."
  [c cell short]
  (let [{:keys [tunnel spare reach]} (:args c)
        p (:primitives c)
        in (judge/rules-in c (reach/standing-vec (:primitives c)))
        block-at (:block-at in)
        verdict (rules/may-place? (assoc in :cell cell))
        block (:block (first (filter #(= cell (:cell %)) (:dug tunnel))))
        item (seal-item (map :name (filter #(pos? (:count %)) (u/inventory p))) block spare (choose/filler-names c :build))]
    (cond
      (> (u/eye-dist (u/self-pos c) cell) reach) (book-open! c cell :out-of-reach)
      (not (:ok verdict)) (book-open! c cell (:reason verdict))
      (and (nil? item) (fetch-opts c) (not (:blocks-fetched (ctx/mem c))))
      (let [r (await (fetch-blocks! c short))]
        (when-not (= :continue r) (ctx/update-mem! c assoc :blocks-fetched true))
        (or r (book-open! c cell :no-blocks)))
      (nil? item) (book-open! c cell :no-blocks)
      :else (let [outcome (await (ctx/call c :fill 'jobs.blocks.fill-cells
                                           {:cells [(cell-pos cell)] :use {:blocks [item]} :least 1 :ignore-zones? true}))
                  n (block-at cell)]
              (cond
                (= :continue outcome) :continue
                (and n (not (bs/replaceable n))) (do (ctx/update-mem! c update :filled (fnil conj []) cell) :again)
                :else (book-open! c cell :place-failed))))))

(defn way-cells
  "The cells a walk back to the entry needs seen, nearest the body first: each cell of the way and the floor under it."
  [entry tunnel]
  (mapcat (fn [[x y z :as cell]] [cell [x (dec y) z]]) (reverse (cons entry (map :cell (:dug tunnel))))))

(defn ^:async look-along!
  "Turn to each of cells ([x y z], nearest first) the body has not seen, with a sight pass after each look, at most look-turns looks."
  [c cells]
  (loop [todo (seq cells) n 0]
    (when (and todo (< n look-turns))
      (if (look/unseen? (:primitives c) [(cell-pos (first todo))])
        (let [[x y z] (first todo)]
          (await (ctx/act c :look (clj->js {:pos {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}})))
          (look/see! c)
          (recur (next todo) (inc n)))
        (recur (next todo) n)))))

(defn ^:async seal!
  "At the entry: fill the next mouth cell, lowest first, then the farthest; none left ends the job."
  [c]
  (let [{:keys [tunnel]} (:args c)
        m (ctx/mem c)
        entry (first (ln/line-cells (:line tunnel)))
        in (judge/rules-in c (reach/standing-vec (:primitives c)))
        booked (set (map :cell (:open m)))
        body (u/self-pos c)
        todo (remove booked (mouth (:dug tunnel) entry (:block-at in)))]
    (cond
      (and (not (:way-looked m)) (not= entry (reach/standing-vec (:primitives c)))
           (look/unseen? (:primitives c) (map cell-pos (way-cells entry tunnel))))
      (do (ctx/update-mem! c assoc :way-looked true)
          (await (look-along! c (way-cells entry tunnel)))
          :again)
      (not= entry (reach/standing-vec (:primitives c))) (await (walk-to! c entry #(open! c {:escaped true :walk %})))
      (and (empty? todo) (seq (:open m))) (open! c {})
      (empty? todo) (finish! c :done :sealed {})
      :else (await (fill! c (first (sort-by (juxt #(% 1) #(- (u/eye-dist body %))) todo)) (count todo))))))

(defn ^:async work-round [c]
  (let [{:keys [tunnel]} (:args c)
        m (ctx/mem c)
        torches (when (:line tunnel) (standing (:block-at (judge/rules-in c (reach/standing-vec (:primitives c)))) tunnel m))]
    (cond
      (not (:line tunnel)) (finish! c :stopped :bad-args {:why "tunnel must be the result of jobs.access.tunnel"})
      (:escape m) (await (escape! c))
      (:collect m) (await (collect! c))
      (seq torches) (await (take-torch! c (first torches)))
      :else (await (seal! c)))))

(defn ^:async step!
  "One piece of the way out: the walk back after a fetch first, then the work. :again, :continue (a child waits on the
  world) or the ending."
  [c]
  (let [back (await (need/return! c))]
    (if back back (await (work-round c)))))

(defn ^:async round
  "The whole way out: step! until it ends, a pace between; :continue while a child
  waits on the world."
  [c]
  (await (pace/steps! c #(step! c))))
