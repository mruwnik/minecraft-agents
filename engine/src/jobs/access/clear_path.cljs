(ns jobs.access.clear-path
  (:require [jobs.lib.block-sets :as bs]
            [engine.settings :as settings]
            [jobs.access.stair.limits :as limits]
            [engine.args :as a]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.reach :as reach]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Dig a door 1 wide and 2 high through the wall straight ahead along :heading, then step through it.
  - The wall starts right in front of the body and has a floor under every row and room for the body beyond it
    (jobs.lib.escape/door). A cell it has not seen is rock (one not loaded ends :no-door); a far side it cannot
    see is guessed: once the cells before it are dug it steps in, looks, and plans on from there with what it sees,
    digging on while each dug row shows more wall; a dead end walks back to where it began.
  - With :max-dig-s, each plan is priced before it is dug (too-dear): one whose price with the dug cells is over
    :max-dig-s walks back to where it began and ends :too-dear. A door whose far side it could not see emits
    clear-path.far-side {:seen :rows :open} as it ends: the rows seen then, the wall rows found, and whether it got through.
  - Never digs a door, gate, trapdoor, bed, container, sign or an unbreakable block.
  - Each cell is a jobs.blocks.dig child (zones, claims, hazards and tools are its rules). It picks up the drop
    (:collect) when there is room for it, so the block can be put back; with no room it digs on and leaves it.
  - With :close-behind, a door 2 or more deep has its first column put back (jobs.blocks.place children, the dug blocks)
    from the second as soon as both are dug, before the far side opens, so nothing follows the body through (go-to's
    break-out of its own pen); a cell it cannot fill (an animal in it) is left to go-to's put-back, and a dead end digs it
    open again to walk back.
  - Then it walks into the cell beyond (a go-to child).
  One call is the whole door; it yields :continue only while a dig child waits on the world.
  Needs nothing carried (a tool the dig child cannot find is its :dig-waits). Quote: the door's cells times the time of a
  cell, ending at the far side.
  :bad-args and :no-door are conditions, checked at the start. Ends :done, or with a cause {:reason r ...}: :impossible for :bad-args (needs :heading), :condition-failed for
  :no-door (no such wall here), :protected (a block it never digs), :not-diggable (:natural-only? and a cell that is not natural terrain), :footprint (a cell of a plan this body made,
  unless the spare-own-builds setting is false) and :dig-waits (the dig child's wait or decline, its ending the :because); :too-dear (the price over :max-dig-s); :nothing-possible for
  :dig-failed (its ending the :because), :refills (a cell dug stair.limits/max-digs times, e.g. sand falling in) and :step-failed
  (the walk through did not arrive). Data {:dug [{:cell :block}] :at [x y z] :through [x y z]}, :shut [cells put back] with :close-behind.
  It does not put the blocks back: jobs.movement.go-to does that once through.
  A tool the dig lacks is fetched (jobs.lib.need, from the call's :ways) unless they are only #{:carried}; a block no carried tool harvests
  is dug bare-handed (slow, the drop is lost) when no fetch is allowed or one failed, or at once with :need-drop false; :need-drop true stops :dig-waits :no-tool instead.")

(a/defargs args
  {:heading {:doc ":north :east :south or :west" :spec #{:north :east :south :west} :default nil}
   :ignore-zones? :shared
   :max-dig-s {:doc "seconds of digging the whole door may take (see doc); nil: no bound" :spec (a/num-in 0 nil) :default nil}
   :natural-only? {:doc "true (go-to's escalation): stops :not-diggable at a cell that is not natural terrain or the body's own block (jobs.lib.escape/diggable?)" :spec boolean? :default false}
   :close-behind {:doc "true: put the door's first column back from inside before walking on out (see the doc)" :spec boolean? :default false}
   :need-drop {:doc "false: dig a block no carried tool harvests bare-handed (slow, the drop is lost) without fetching; true: never; unset: bare-handed when no pickaxe can be fetched" :spec (a/or-of boolean? #{:auto}) :default :auto}})

(def headings {:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]})

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::cell-s {:default 1.5 :doc "Mean seconds one cell of the door takes (dig, pick up, pace), for the quote." :spec (a/num-in 0 nil)}})

(def meta {:category :phase :part-of '[jobs.movement.go-to]})

(def safe? (price/staying :work ready/safe-here-by-night))

(def fit? ready/fit?)

(def ways need/fetch-ways)

(def target [:heading])

(def bad-args
  "Never :bad-args unless :heading is given."
  (checks/args-fit "needs :heading (:north :east :south :west)"
                   (fn [c] (boolean (headings (:heading (:args c)))))))

(defn no-door-why [heading p]
  (str "no wall with a floor beyond, " (name heading) " of " (pr-str (reach/standing-vec p))))

(defn no-door
  "Later :no-door when no wall with a floor beyond lies ahead on the map as seen."
  [c]
  (let [heading (:heading (:args c))
        p (:primitives c)]
    (if (and (headings heading)
             (nil? (escape/door (escape/block-at-of p) (escape/unseen-of p) (reach/standing-vec p) (headings heading))))
      (contract/later :no-door (no-door-why heading p) :why (no-door-why heading p))
      (contract/ok :door "a wall to clear"))))

(def defaults {:spend {:job-time ::job-time-s}})

(def can-start? (cond/and bad-args no-door))

(defn quote-of [c]
  (let [heading (:heading (:args c))
        p (:primitives c)
        {:keys [cells through]} (when (headings heading)
                                  (escape/door (escape/block-at-of p) (escape/unseen-of p) (reach/standing-vec p) (headings heading)))
        s (* (count cells) (settings/get settings ::cell-s))]
    (cond-> {:uses {:job-time [s (* 1.5 s) (* 4 s)]}}
      through (assoc :moves-to through))))

(def facts
  "The stops that name a wall or a rule that bars the door (condition-failed); the others are attempts that failed."
  #{:no-door :protected :footprint :dig-waits :too-dear :not-diggable})

(defn result-of
  "The data of the door now: {:dug :at} and the :through cell once planned, plus detail."
  [c detail]
  (let [m (ctx/mem c)]
    (merge {:dug (:dug m []) :at (reach/standing-vec (:primitives c))}
           (when-let [t (:through m)] {:through t})
           (when (seq (:shut m)) {:shut (:shut m)})
           detail)))

(defn done?
  "Ok once the body stands on the far cell and every cell the call planned is seen open now (a fresh call has no plan:
  later); else the cells dug against the cells planned."
  [c]
  (let [m (ctx/mem c)
        p (:primitives c)
        cells (:cells m)
        open? (fn [cell] (or (some #{cell} (:shut m)) (bs/air (u/seen-name p (zipmap [:x :y :z] cell)))))]
    (if (and (seq cells) (:through m) (= (:through m) (reach/standing-vec p)) (every? open? cells))
      (contract/ok :done (str "cleared a way through to " (pr-str (:through m))) :units :cells :done (count cells) :left 0
                   :result (result-of c {}))
      (contract/later :digging "the door's cells are being dug"
                      :units :cells :done (count (:dug m)) :left (max 0 (- (count cells) (count (:dug m))))))))

(defn wall-rows
  "How many rows of the wall the door has dug or planned, cells (a plan's) on top of the dug ones: their columns."
  [c cells]
  (count (distinct (map (fn [[x _ z]] [x z]) (concat (map :cell (:dug (ctx/mem c))) cells)))))

(defn finish!
  "End the door: a stop ends by reason a kind (facts condition-failed, the rest nothing possible), the result as the ending's data."
  [c reason detail]
  (let [data (result-of c detail)
        {:keys [guessed-rows seen-rows cells]} (ctx/mem c)]
    (when guessed-rows
      (ctx/emit! c :clear-path.far-side :info {:seen guessed-rows :rows (max (or seen-rows 0) (wall-rows c cells)) :open (= :done reason)
                                               :text (str "a far side it could not see: " (name reason))}))
    (if (= :done reason)
      (ctx/end c :done nil data)
      (ctx/end c (if (facts reason) :condition-failed :nothing-possible)
               {:reason reason :hint (str "clear-path stopped: " (name reason) (some->> (:cell detail) pr-str (str " at ")))} data))))

(defn too-dear
  "Why digging plan {:cells :unseen} on top of the cells dug so far would take over :max-dig-s seconds, else nil:
  jobs.lib.cost/dig-cells-s with the best carried tool or the hand, every seen row of the plan priced as it is. A far
  side not seen (:unseen) is a run of rock rows of unknown length (cost/door-s at cost/wall-on-chance of the rows so
  far): the rows the budget leaves, and past them a dead end that wastes the digging and walks back. Each dug row is
  looked at and the plan priced again before the next row is paid."
  [c {:keys [cells unseen]}]
  (when-let [most (:max-dig-s (:args c))]
    (let [p (:primitives c)
          block-at (escape/block-at-of p)
          dig-s #(cost/dig-cells-s (map (fn [block] (tools/clear-ms p block)) %))
          sure-s (dig-s (concat (map :block (:dug (ctx/mem c))) (map block-at cells)))
          rows (wall-rows c cells)
          s (if unseen
              (cost/door-s {:sure-s sure-s :row-s (dig-s [b/hidden-guess b/hidden-guess]) :most most :q (cost/wall-on-chance rows)
                            :back-s #(* (cost/walk-step-s) (+ rows %))})
              sure-s)]
      (when (> s most)
        (str "digging on would take " (js/Math.round s) " s, over the " (js/Math.round most) " s the door may take")))))

(defn ^:async plan! [c]
  (let [dir (headings (:heading (:args c)))
        p (:primitives c)
        gap (escape/door (escape/block-at-of p) (escape/unseen-of p) (reach/standing-vec (:primitives c)) dir)]
    (cond
      (nil? gap) (finish! c :no-door {:why (no-door-why (:heading (:args c)) p)})
      (too-dear c gap) (do (when (:unseen gap)
                             (ctx/update-mem! c assoc :cells (:cells gap) :guessed-rows (wall-rows c (:cells gap))))
                           (finish! c :too-dear {:why (too-dear c gap)}))
      :else (let [{:keys [cells through unseen]} gap
                  rows (wall-rows c cells)]
              (ctx/update-mem! c #(cond-> (assoc % :cells cells :through through :unseen unseen :from (reach/standing-vec (:primitives c)) :start (reach/standing-vec (:primitives c)) :dug [])
                                    unseen (assoc :guessed-rows rows)))
              :again))))

(defn room-for-drop?
  "A free slot, or a carried stack of what block drops (b/drops-of) under 64."
  [p block]
  (let [drops (set (b/drops-of p block))]
    (boolean (or (pos? (u/free-slots p))
                 (some #(and (drops (:name %)) (< (:count %) 64)) (u/inventory p))))))

(defn own-build
  "The refusal naming the plan this body made that covers cell, unless the spare-own-builds setting is false; else nil."
  [c cell]
  (when (access/spare-own-builds)
    (access/own-keep (access/rules-input c {:spare-own? true}) (zipmap [:x :y :z] cell))))

(defn declined?
  "Whether the dig child's ending e is a refusal at its start (no data), not a dig that failed."
  [e]
  (and (= :condition-failed (:kind e)) (nil? (:data e))))

(defn ^:async dig-cell!
  "One dig child call on the cell being dug (:digging {:cell :block :collect}, kept until the child ends, so a resumed
  call goes on with it). :again, :continue while the child waits on the world, or the ending."
  [c {:keys [cell block collect]}]
  (let [nd (:need-drop (:args c))
        hand? (and (not (tools/can-harvest? (:primitives c) block))
                   (or (false? nd) (and (= :auto nd) (or (:by-hand (ctx/mem c)) (empty? (need/fetch-how c))))))
        args {:pos cell :collect (and collect (not hand?)) :need-drop (not hand?) :ways (need/call-ways c)
              :ignore-zones? (:ignore-zones? (:args c)) :natural-only? (:natural-only? (:args c))}
        done! (fn [] (ctx/update-mem! c dissoc :digging))]
    (cond
      (escape/protected? block) (finish! c :protected {:cell cell :block block})
      (own-build c cell) (finish! c :footprint {:cell cell :block block :plan (:plan (own-build c cell))})
      (and (not (:started (:digging (ctx/mem c))))
           (>= (get-in (ctx/mem c) [:cell-tries cell] 0) (limits/max-digs block)))
      (finish! c :refills {:cell cell :block block})
      :else
      (let [resumed? (:started (:digging (ctx/mem c)))
            _ (ctx/update-mem! c #(cond-> (assoc-in % [:digging :started] true)
                                    (not resumed?) (update-in [:cell-tries cell] (fnil inc 0))))
            e (await (ctx/call c :dig 'jobs.blocks.dig args))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) (do (done!)
                                  (when (:dug (:data e))
                                    (ctx/update-mem! c update :dug conj {:cell cell :block block}))
                                  :again)
          (and (declined? e) (= :auto nd) (not hand?) (= :no-tool (contract/cause-key e)))
          (do (done!) (ctx/update-mem! c assoc :by-hand true) :again)
          (declined? e) (do (done!) (finish! c :dig-waits {:cell cell :block block}))
          (= :not-diggable (contract/cause-key e)) (do (done!) (finish! c :not-diggable {:cell cell :block block}))
          :else (do (done!)
                    (finish! c :dig-failed {:cell cell :block block})))))))

(defn ^:async step-through! [c through]
  (let [r (await (child/walk-into!* c :walk through))]
    (cond
      (= :continue r) :continue
      (and (:arrived (:data r)) (= through (reach/standing-vec (:primitives c)))) (finish! c :done {})
      :else (finish! c :step-failed {:cell through}))))

(defn side-cells
  "Cells beside the dug cells (not dug themselves), the deepest dug cell first."
  [dir dug]
  (let [dug-set (set dug)
        [dx dz] dir]
    (for [cell (reverse dug)
          d [[dz dx] [(- dz) (- dx)]]
          :let [side (escape/ahead cell d 1)]
          :when (not (dug-set side))]
      side)))

(defn deepest-feet
  "The cell to stand in at the deepest dug cell (the one under it when that is open over a floor), or nil."
  [c]
  (let [block-at (escape/block-at-of (:primitives c))
        deepest (peek (mapv :cell (:dug (ctx/mem c))))
        below (some-> deepest (escape/up -1))]
    (when deepest (if (and (bs/passable? (block-at below)) (rules/solid-floor? block-at (escape/up below -1))) below deepest))))

(defn ^:async step-in!
  "Walk into the deepest dug cell unless the body stands there: :continue while it walks, else nil."
  [c]
  (let [feet (deepest-feet c)]
    (when (and feet (not= feet (reach/standing-vec (:primitives c))))
      (when (= :continue (await (child/walk-into!* c :walk feet))) :continue))))

(defn ^:async step-out!
  "A dead end: walk back to the cell the door was begun from (memory :start), so the cut can be put back behind the
  body; a mouth shut behind it (:shut) is dug open again first (memory :dead-end: the door is not planned on meanwhile).
  :again, :continue while it walks, else nil."
  [c]
  (let [{:keys [start shut]} (ctx/mem c)]
    (cond
      (seq shut) (do (ctx/update-mem! c #(-> % (update :cells (fn [cs] (vec (distinct (concat shut cs))))) (dissoc :shut) (assoc :dead-end true))) :again)
      (and start (not= start (reach/standing-vec (:primitives c))))
      (when (= :continue (await (child/walk-into!* c :back start))) :continue))))

(defn ^:async side-door!
  "The wall ahead goes on but a dug cell may open into air beside it that the body could not see from outside: stand in
  the deepest dug cell (once), look at the cells beside the dug ones, and take a seen-open one with a floor for the far
  side. :again, :continue (the walk waits) or nil."
  [c dir]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        dug (mapv :cell (:dug (ctx/mem c)))
        step-in (when-not (:stepped-in (ctx/mem c)) (await (step-in! c)))]
    (if (= :continue step-in)
      :continue
      (do (ctx/update-mem! c assoc :stepped-in true)
          (loop [cells (side-cells dir dug)]
            (when-let [cell (first cells)]
              (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
              (recur (rest cells))))
          (when-let [side (->> (side-cells dir dug)
                               (filter #(and (not (unseen? %)) (not (unseen? (escape/up % 1)))
                                             (bs/passable? (block-at %)) (bs/passable? (block-at (escape/up % 1)))
                                             (rules/solid-floor? block-at (escape/up % -1))))
                               first)]
            (ctx/update-mem! c #(-> % (assoc :through side) (dissoc :unseen)))
            :again)))))

(defn ^:async plan-on!
  "Plan the door on with gap from from: :again; when it is too dear, walk back to where the door was begun (step-out!) and end :too-dear."
  [c gap from]
  (ctx/update-mem! c update :seen-rows (fnil max 0) (wall-rows c (:cells gap)))
  (if-let [why (too-dear c gap)]
    (do (ctx/update-mem! c assoc :too-dear why)
        (or (await (step-out! c)) (finish! c :too-dear {:why why})))
    (do (ctx/update-mem! c assoc :cells (:cells gap) :through (:through gap) :unseen (:unseen gap) :from from)
        :again)))

(defn ^:async replan!
  "The far side was a guess (:unseen) and the cells before it are dug: step into the deepest dug cell, so the row is
  in sight, look at that row's unseen cells (memory :looked-at), then plan on from it with what the body sees
  (escape/door-from); a row still unseen is taken as guessed. A guessed row seen wall: an opening beside the dug
  cells (side-door!) is the far side, else the wall is dug on, each plan on priced (plan-on!); with no floor beyond
  it walks back. Once a dead end is reached (memory :dead-end, :too-dear for one too dear) nothing is planned on.
  :again, :continue (the walk waits), or the :no-door or :too-dear ending."
  [c]
  (let [{:keys [through from]} (ctx/mem c)
        dir (headings (:heading (:args c)))
        p (:primitives c)
        k (+ (js/Math.abs (- (first through) (first from))) (js/Math.abs (- (nth through 2) (nth from 2))))]
    (if (not= through (:looked-at (ctx/mem c)))
      (if (= :continue (await (step-in! c)))
        :continue
        (do (ctx/update-mem! c assoc :looked-at through)
            (loop [cells [(escape/up through 1) through (escape/up through -1)]]
              (when-let [cell (first cells)]
                (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
                (recur (rest cells))))
            :again))
      (let [block-at (escape/block-at-of p)
            unseen? (escape/unseen-of p)
            gap (escape/door-from block-at unseen? from dir k)]
        (if (= through (:through gap))
          (do (ctx/update-mem! c dissoc :unseen) :again)
          (or (when-not (:dead-end (ctx/mem c))
                (or (await (side-door! c dir))
                    (when gap (await (plan-on! c gap from)))))
              (await (step-out! c))
              (if-let [why (:too-dear (ctx/mem c))]
                (finish! c :too-dear {:why why})
                (finish! c :no-door {:why (str "the wall goes on past " (pr-str through) " with no floor beyond")}))))))))

(defn ^:async start-cell!
  "Look at the first cell not seen open, then say what to do with it: {:digging d} (kept in memory), :open (the look
  showed it open: plan again) or :unloaded (cannot be dug); nil when every cell is open."
  [c cells]
  (let [p (:primitives c)
        pos #(zipmap [:x :y :z] %)]
    (when-let [cell (first (remove #(bs/air (u/seen-name p (pos %))) cells))]
      (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
      (let [seen (u/seen-name p (pos cell))]
        (cond
          (bs/air seen) :open
          (and (nil? seen) (some? (.-sensedAt p)) (nil? (u/sensed p (pos cell)))) {:unloaded cell}
          :else (let [block (or seen b/hidden-guess)
                      d {:cell cell :block block :collect (room-for-drop? p block)}]
                  (ctx/update-mem! c assoc :digging d)
                  {:digging d}))))))

(defn mouth
  "[cells inner] once a door's first two columns are dug: the dug cells of its first column, feet first, and the feet
  cell of its second; else nil."
  [c]
  (let [{:keys [start dug]} (ctx/mem c)
        dir (headings (:heading (:args c)))
        column (fn [k] (let [[x _ z] (escape/ahead start dir k)]
                         (sort-by second (filter (fn [[cx _ cz]] (and (= x cx) (= z cz))) (map :cell dug)))))
        [cells inner] [(column 1) (column 2)]]
    (when (and (seq cells) (>= (count inner) (count cells))) [(vec cells) (first inner)])))

(defn all-dug?
  "Whether every planned cell of the door is dug or open (or shut behind the body) and its far side is no guess."
  [c]
  (let [{:keys [cells unseen shut digging]} (ctx/mem c)
        p (:primitives c)]
    (and (not unseen) (not digging)
         (every? #(or (some #{%} shut) (bs/air (u/seen-name p (zipmap [:x :y :z] %)))) cells))))

(defn ^:async close-behind!
  "With :close-behind, once the first two columns are dug (mouth) and before the rest opens the far side: walk into the
  second, then put back the first one cell a call (memory :to-close; :shut the cells placed), so nothing follows the body
  into the door. Once done, or at the walk out with no second column, or when the walk in did not get there, memory
  :closed. :again, :continue while a child waits, else nil (also while the two columns are not dug yet)."
  [c]
  (let [m (ctx/mem c)
        p (:primitives c)]
    (when (and (:close-behind (:args c)) (not (:closed m)))
      (let [[cells inner] (mouth c)
            left (:to-close m cells)
            closed! (fn [] (ctx/update-mem! c #(-> % (assoc :closed true) (dissoc :to-close))) :again)]
        (cond
          (nil? inner) (when (all-dug? c) (closed!))
          (empty? left) (closed!)
          (not= inner (reach/standing-vec p))
          (if (:inside-tried m)
            (closed!)
            (let [r (await (child/walk-into!* c :inside inner))]
              (if (= :continue r) :continue (do (ctx/update-mem! c assoc :inside-tried true) :again))))
          :else
          (let [cell (first left)
                block (some #(when (= cell (:cell %)) (:block %)) (:dug m))
                r (await (ctx/call c :close 'jobs.blocks.place {:pos (zipmap [:x :y :z] cell) :use {:blocks (vec (distinct (cons block (b/drops-of p block))))}
                                                                :ways #{:carried} :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
            (if (= :continue r)
              :continue
              (do (ctx/update-mem! c #(cond-> (assoc % :to-close (vec (rest left))) (:placed (:data r)) (update :shut (fnil conj []) cell)))
                  :again))))))))

(defn ^:async next!
  "One piece of the door: plan it, dig a cell, plan on past a far side it could not see, or step through. :again,
  :continue (a child waits) or the ending."
  [c]
  (let [{:keys [cells through unseen digging shut]} (ctx/mem c)]
    (if-not through
      (await (plan! c))
      (let [closing (when-not digging (await (close-behind! c)))
            start (when-not closing (if digging {:digging digging} (await (start-cell! c (remove (set shut) cells)))))]
        (cond
          closing closing
          (= :open start) :again
          (:unloaded start) (finish! c :no-door {:why (str "the cell " (pr-str (:unloaded start)) " is not loaded")
                                                 :cell (:unloaded start)})
          (:digging start) (await (dig-cell! c (:digging start)))
          unseen (await (replan! c))
          :else (await (step-through! c through)))))))

(defn ^:async round
  "The whole door: next! until it ends, a pace between pieces."
  [c]
  (await (pace/steps! c #(next! c))))
