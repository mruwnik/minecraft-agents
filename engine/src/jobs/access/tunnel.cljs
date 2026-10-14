(ns jobs.access.tunnel
  (:require [jobs.lib.choose :as choose]
            [engine.args :as a]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.declined :as declined]
            [jobs.lib.dig-look :as look]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.pace :as pace]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.torch :as torch]
            [jobs.lib.trip :as trip]
            [jobs.access.stair :as stair]
            [jobs.lib.reach :as reach]
            [jobs.lib.child :as child]
            [jobs.lib.walk :as walk]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.placement :as placement]
            [jobs.access.tunnel.line :as ln]
            [plan.shape :as shape]
            [jobs.lib.world :as known]))

(def doc
  "Cut a way to stand beside a buried block :target [x y z] and stop there, with the target the next cell ahead
  at feet height. The target itself is left. A nil zone list declines the check (one warn tunnel.declined).
  Before the first step it stocks torches when under 8 are carried (jobs.lib.trip/torches-ready!: info torch.stocked, or warn torch.no-stock and it goes on dark).

  The way is one straight line along a heading through the target's column:
  1. An entry stand: on the surface (the column's top cell over a solid floor, two open cells; no entry from
     water), or the body's own cell when it stands underground in the entry column.
  2. A 1-wide stair down (or up) to the target's height, cut by jobs.access.stair as a child with :y set, so it
     resumes from any step.
  3. A flat run, 1 wide and 2 high (head cell first), up to the stand. Never a shaft, and the run never passes
     under its own stair.
  4. At the stand the cell over the target is dug, so after the caller digs the target the cell is 2 high and can
     be walked into to pick up a drop.

  Choosing the line: the four headings are tried and, per heading, entry distances 1 to :max-length. A distance
  fits when the stair's steps are at most the distance less one. Every cell of a fitting line is judged from what
  the body senses before anything is walked or dug (a cell not seen is taken for stone, and air in an entry column's
  scan for a stand; it is dug to see, see jobs.access.stair), as the stair judges its own steps (jobs.lib.access.rules: zones,
  other plans' footprints, unloaded; a cut cell that is the floor of a stair this body cut earlier, :undercuts-way,
  from the :stair-way memory; a fluid in a cut; a fluid beside a cut or a falling block over one, taken
  only when named in :accept, default #{}; the next floor solid; the cell under it neither air nor fluid). The
  target's own column counts as a last run step. The shortest valid line wins, among those whose entry is within 24 blocks of the body if any (the body does not walk to a far surface entry when it stands in a line underground). Among equals one that leaves the
  floor under the body's start uncut wins, then the entry nearest the body.

  Every line refused by a zone, claim or plan declines the check (wait :refused, one warn tunnel.declined). Else no
  valid line stops before any walk or dig with the reason all judged lines share (:hazard
  :cave-below :no-floor :fluid-in-cut :undercuts-way :not-loaded :no-zones). :too-far: no line fits within :max-length.
  :no-approach: the headings differ, with :headings giving each one's reason.

  Then the body walks to the entry (a go-to child planning no deeper than 16 below the lower of body and entry; failing: :walk-in-failed) and the stair child cuts
  and walks its steps. Each run step is judged again right before each dig (:hazard :floor-unseen :zone :no-tool
  :inventory-full :refills :dig-failed). Exposed lava is sealed or stops as the stair's :on-lava says (tunnel.sealed
  info; :lava-exposed, :lava-unsealed). Before each further run step, and at the stand, the way back to the
  entry is planned on a fresh pathWorld and must be whole and walkable, else :no-way-back and the body stays
  where it is (a kept tunnel; a dead end goes through leave-tunnel below, which digs its own way out).

  After any other stop once the body has entered, it walks back to the entry first (:out true when it arrived).
  A dead end (:keep false) goes through jobs.access.leave-tunnel as a child (result in :leave). A kept tunnel
  is left by a plain walk.

  One call does it all, in and (a dead end) out; :continue only while a child waits on the world.

  A target off the body's row and column, with the body underground and no line entered within 24 blocks, is
  reached in two legs: a kept tunnel child along the shorter axis to a waypoint one past the target's column, then
  the line above from the body's own cell. That first leg stays open even in a dead end (:keep false). A leg that stops is tried once on the other axis; a
  floor of air is bridged with a carried building block (stair/bridge!), as the stair does. A target on the body's
  row or column farther than :max-length is cut the same way in legs of :max-length from the body's cell.

  The body's cell is the progress. On the stair line the stair child goes on, on the run line the run, at the
  stand it is done, anywhere else it walks to the entry. A cell dug before a cut is air and is not dug again.
  Dug cells are left and recorded.

  Torches: the line's cells are numbered 0 (entry) to n (stand), the target is n+1. A torch site is a cell
  index s whose torch hangs in the head cell of s on a side wall (else a floor torch in the feet cell), placed
  from cell s+1. Sites are 0, then each time the farthest s whose cell s+1 the previous torch still lights
  (light 14 less the taxicab distance), until every cell is lit. That is a flat run every 11 cells, a stair
  every 5 steps, none when the entry is the stand. The stair is cut in segments up to cell s+1 of the next
  site so the torch goes in between.

  A site is placed when due (the body on s+1, nothing hanging in its cells; the world is the record, so a
  restart does not hang it twice). A site the body already passed is :unlit :passed. With no torch carried
  (:no-torches), the place refused (may-place?, or no wall or floor to hang on: :no-support) or failed
  (:place-failed), the site is :unlit with its reason and the tunnel goes on dark. The first gives one warn
  tunnel.unlit.

  :keep (default false, a dead end) writes each torch to the scaffold ledger (purpose :tunnel-torch, before the
  place) for jobs.access.leave-tunnel to take back. :keep true leaves the torches and the tunnel as they are.

  On the run it digs the exposed ores within reach (:pick-up-ores, jobs.lib.ore-pick), never the target; :ores lists them
  with those of the stair child.

  Hands over {:status :done|:stopped :reason :reached|kw :target :entry :heading :stand :at [x y z] :dug
  [{:cell :block}] :inside bool :keep bool :line {:entry :heading :dir :steps :run :stand :target} :torches
  [{:cell :site :block}] :unlit [{:cell :site :reason}]} plus detail. :torches are those standing now, read
  from the world. :inside is true when the body is off the entry, on the way in. Also a tunnel.done info or
  tunnel.stopped warn event.

  :spent-ok (default false): every carried pickaxe down to 3 uses stops the tunnel {:reason :tool-spent} before the last breaks (jobs.access.stair).
  :fetch (default true; false waits :no-tool; jobs.lib.fetch): a missing pickaxe (the stair's wait, or a :no-tool stop of the run's own
  digs) is got with jobs.items.get-tool, then the body walks back to the cell it stood on and goes on. The stair
  child fetches only items (a block to seal lava with), not tools.")

(a/defargs args
  {:target {:doc "the buried block [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :max-length {:doc "longest line, in blocks along the heading from the entry to the target, 1 to 96" :spec (a/int-in 1 96) :default 24}
   :accept {:doc "hazards taken: #{:water :lava :falling-block}" :spec (a/coll-of #{:water :lava :falling-block}) :default #{}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :on-lava {:doc "exposed lava: :seal (fill it with a building block, then go on) or :stop (jobs.access.stair)" :spec #{:seal :stop} :default :seal}
   :keep {:doc "a tunnel that stays: torches left and the tunnel left open; false (a dead end): torches go into the scaffold ledger for jobs.access.leave-tunnel to take back" :spec boolean? :default false}
   :max-steps {:doc "give the round back with :continue after this many steps (a stair segment, a run cell), for a caller that looks around between them; nil: the stair's ::max-steps" :spec (a/int-in 1 nil) :default nil}
   :fetch {:doc "get a missing pickaxe instead of waiting :no-tool (jobs.lib.fetch): true, a set of kinds or a map of limits; false waits :no-tool" :spec fetch/option? :default true}
   :spent-ok {:doc "dig on with tools down to 3 uses instead of stopping :tool-spent (see jobs.access.stair)" :spec boolean? :default false}
   :spare-own-builds {:doc "a cell of a plan this body made is never dug; false: it may be (see jobs.access.stair)" :spec boolean? :default true}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach on the way; false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}})

(defn head-of [cell] (shape/add cell [0 1 0]))

(defn torch-cell
  "The cell of site s (head, then feet) that holds a torch in the world, or nil."
  [block-at cells s]
  (first (filter #(torch/torch-blocks (block-at %)) [(head-of (cells s)) (cells s)])))

(defn standing-torches
  "[{:cell :site :block}] for the sites of plan whose torch stands now."
  [block-at plan]
  (let [cells (ln/line-cells plan)]
    (vec (keep (fn [s] (when-let [cell (torch-cell block-at cells s)] {:cell cell :site s :block (block-at cell)}))
               (:sites plan)))))

(defn target-of
  "The :target arg as [x y z], or nil when unset."
  [c]
  (when-let [{:keys [x y z]} (:target (:args c))] [x y z]))

(defn finish!
  "Hand the result over and end: :done when reached, else :stopped (warn)."
  [c reason detail]
  (let [{:keys [plan dug unlit ores]} (ctx/mem c)
        feet (reach/standing-vec (:primitives c))
        result (merge (cond-> {:status (if (= :reached reason) :done :stopped) :reason reason :target (target-of c)
                               :at feet :dug (or dug []) :ores (into (vec ores) (ore-pick/picked c)) :inside (boolean (and plan (not= feet (:entry plan))))
                               :keep (boolean (:keep (:args c))) :torches [] :unlit (or unlit [])}
                        plan (assoc :line (select-keys plan [:entry :heading :dir :steps :run :stand :target])
                                    :torches (standing-torches (:block-at (stair/rules-in c feet)) plan)))
                      (select-keys plan [:entry :heading :stand])
                      detail)]
    (ctx/result! c result)
    (if (= :reached reason)
      (ctx/emit! c :tunnel.done :info (assoc result :text (str "tunnel reached " (pr-str (:stand plan)))))
      (ctx/emit! c :tunnel.stopped :warn
                 (assoc result :text (str "tunnel stopped: " (name reason) (some->> (:cell detail) pr-str (str " at "))))))
    :done))

(defn run-index
  "k when feet is the run's cell k steps from end, 0 <= k <= run; else nil."
  [end feet heading run]
  (first (filter #(= feet (ln/ahead end heading %)) (range 0 (inc run)))))

(defn ^:async dig-cell!
  "stair/dig! with :again for a dug cell (more to do), else a stop map."
  [c in cell cut accept]
  (let [r (await (stair/dig! c in cell cut accept))]
    (if (= :continue r) :again r)))

(defn ^:async run-step!
  "Dig one cell of the next run step or walk into it: :again, :continue while the walk waits, or a stop map."
  [c feet]
  (let [{:keys [plan]} (ctx/mem c)
        accept (set (:accept (:args c)))
        in (stair/rules-in c feet)
        base (stair/step-cells feet :level (:heading plan))
        {:keys [next cut] :as cells} (assoc base :bridged? (contains? (:bridged (ctx/mem c)) (:floor base))
                                                 :filler? (some? (choose/filler c :build)))]
    (await (stair/look-ahead! c feet next cut))
    (or (await (stair/lava-step! c :tunnel.sealed (:block-at in) feet cut))
        (let [stop (await (stair/judge! c in cells accept))]
          (if (and (= :no-floor (:reason stop)) (rules/air (:block stop)))
            (await (stair/bridge! c in cells))
            stop))
        (if-let [cell (first (remove #(rules/air ((:block-at in) %)) cut))]
          (or (await (stair/peek! c cell)) (await (dig-cell! c in cell cut accept)))
          (or (await (look/settle! c cut))
              (let [r (await (child/walk-into! c :walk next))]
                (cond
                  (= :continue r) :continue
                  (and (:arrived r) (= next (reach/standing-vec (:primitives c))))
                  (do (ctx/emit! c :tunnel.step :info {:at next :text (str "tunnel step to " (pr-str next))}) :again)
                  :else {:reason :step-failed :cell next :walk r})))))))

(defn ^:async open-over-target!
  "At the stand: the target's step judged again and the cell over the target dug; :reached once it is open."
  [c feet]
  (let [accept (set (:accept (:args c)))
        in (stair/rules-in c feet)
        {:keys [next cut] :as cells} (stair/step-cells feet :level (:heading (:plan (ctx/mem c))))
        over (first cut)]
    (await (stair/look-ahead! c feet next cut))
    (or (await (stair/lava-step! c :tunnel.sealed (:block-at in) feet [over]))
        (await (stair/judge! c in cells accept))
        (if (rules/air ((:block-at in) over))
          (or (await (look/settle! c [over])) :reached)
          (or (await (stair/peek! c over)) (await (dig-cell! c in over cut accept)))))))

(defn line-index
  "The index of feet on the line of cells (0..n), or nil."
  [cells feet]
  (first (keep-indexed (fn [i cell] (when (and (= cell feet) (< i (dec (count cells)))) i)) cells)))

(defn segment-end
  "The line index the stair child cuts to from k: the standing cell (s+1) of the next site s >= k, else the stair's end."
  [{:keys [sites steps]} k]
  (min steps (or (some #(when (>= % k) (inc %)) sites) steps)))

(defn stair-fetch
  "The :fetch the stair child gets from the tunnel's: its items (a seal block), not tools (the tunnel fetches those),
  nil when that is off."
  [fetch]
  (let [limits (fetch/parse-arg fetch)]
    (when (and limits (contains? (or (:what limits) fetch/kinds) :item))
      (assoc limits :what #{:item}))))

(defn ^:async stair-part!
  "One call of the stair child over the segment from line index k (it hands over what it dug): :again once the
  segment is cut, :continue while the child waits on the world, :declined when the stair declined (the wait, e.g. :no-tool, reaches this job's
  job.waiting), or a stop map when the stair stopped. :stair-done once the body stands at the stair's
  end."
  [c {:keys [heading dir] :as plan} k]
  (let [cells (ln/line-cells plan)
        r (await (declined/call-child! c :stair 'jobs.access.stair
                                 {:dir dir :heading heading :fetch (stair-fetch (:fetch (:args c))) :stock-torches false :y ((cells (segment-end plan k)) 1) :on-lava (:on-lava (:args c) :seal)
                                  :accept (set (:accept (:args c))) :ignore-zones? (boolean (:ignore-zones? (:args c))) :spent-ok (boolean (:spent-ok (:args c))) :spare-own-builds (:spare-own-builds (:args c))
                                 :pick-up-ores (:pick-up-ores (:args c))}))]
    (if (not= :done r)
      (if (= :declined r) :declined :continue)
      (let [res (ctx/child-result c :stair)
            done? (= :done (:status res))]
        (ctx/update-mem! c #(-> % (update :dug (fnil into []) (:dug res)) (update :ores (fnil into []) (:ores res))
                                (assoc :stair-done (and done? (= (cells (:steps plan)) (reach/standing-vec (:primitives c)))))))
        (if done?
          :again
          (assoc (select-keys res [:cell :hazards :zone :claim :plan :fluid :block :tool :walk :why]) :reason (:reason res)
                 :in :stair))))))

(defn segment-start?
  "Whether no stair child is in flight: the next call cuts a new segment."
  [c]
  (empty? (get-in (ctx/mem c) [:children :stair])))

(defn book-unlit!
  "Book site s as :unlit with reason, at the cell its torch would take (cell, else the site's head cell); the first of
  the tunnel warns."
  [c plan s reason cell]
  (let [cell (or cell (head-of ((ln/line-cells plan) s)))
        first? (empty? (:unlit (ctx/mem c)))]
    (ctx/update-mem! c update :unlit (fnil conj []) {:cell cell :site s :reason reason})
    (when first?
      (ctx/emit! c :tunnel.unlit :warn {:reason reason :cell cell
                                        :text (str "tunnel torch left out at " (pr-str cell) ": " (name reason))}))
    nil))

(defn torch-choice
  "How to hang the torch of site s from the eye: jobs.lib.torch/torch-at for the head and feet cells of s."
  [plan s eye block-at]
  (torch/torch-at (shape/headings (:heading plan)) ((ln/line-cells plan) s) eye block-at))

(defn ^:async hang!
  "Hang the torch of site s from the body's place, or book the site :unlit with why not. The ledger entry (a dead
  end) is written before the place and confirmed when the cell holds the torch. :again after a place."
  [c in plan s]
  (let [p (:primitives c)
        block-at (:block-at in)
        choice (torch-choice plan s (placement/eye (u/self-pos c)) block-at)
        cell (:cell choice)
        verdict (when cell (rules/may-place? (assoc in :cell cell)))
        reason (cond (zero? (torch/torches-held p)) :no-torches
                     (:refused choice) (:refused choice)
                     (not (:ok verdict)) (:reason verdict))]
    (if reason
      (book-unlit! c plan s reason cell)
      (let [keep? (:keep (:args c))
            intended (ledger/intend (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)
                                    {:cell cell :item (:block choice) :before "air" :job (:id c) :purpose :tunnel-torch})
            _ (when-not keep? (ledger/remember! c intended))
            _ (await (torch/off-hand-to-inventory! c))
            r (await (ctx/act c :place (clj->js {:pos (zipmap [:x :y :z] cell) :item "torch"
                                                 :click (placement/js-click (:click choice))})))
            held? (or (= "placed" (.-status r)) (torch/torch-blocks (block-at cell)))]
        (when-not keep? (ledger/remember! c (if held? (ledger/confirm intended cell) (ledger/reconcile intended block-at))))
        (when-not held? (book-unlit! c plan s :place-failed cell))
        :again))))

(defn ^:async torch-step!
  "With the body on line index k: book the sites it passed without a torch, and hang the one due (site k-1, not
  :unlit, nothing hanging in its cells). :again after a place, else nil."
  [c k]
  (let [{:keys [plan unlit]} (ctx/mem c)
        in (stair/rules-in c (reach/standing-vec (:primitives c)))
        cells (ln/line-cells plan)
        booked (set (map :site unlit))
        pending (remove #(or (booked %) (torch-cell (:block-at in) cells %)) (filter #(<= (inc %) k) (:sites plan)))]
    (doseq [s (filter #(< (inc %) k) pending)] (book-unlit! c plan s :passed nil))
    (when-let [due (first (filter #(= (inc %) k) pending))]
      (await (hang! c in plan due)))))

(defn ^:async work!
  "One bounded piece of the way from the body's place: :reached, :again, :continue (a child waits) or a stop map."
  [c]
  (let [{:keys [plan checked stair-done]} (ctx/mem c)
        {:keys [entry heading dir steps run]} plan
        cells (ln/line-cells plan)
        {:keys [end stand]} (ln/line plan)
        feet (reach/standing-vec (:primitives c))
        k (line-index cells feet)
        j (run-index end feet heading run)
        i (when (pos? steps) (stair/stair-index entry feet dir heading steps))]
    (ctx/update-mem! c stair/record-dug (:block-at (stair/rules-in c feet)))
    (or (when k (await (torch-step! c k)))
        (when (and j (= :continue (await (ore-pick/pick! c {:except #{(target-of c)}})))) :continue)
        (cond
          (and i (not stair-done) (or (< i steps) (not (segment-start? c))))
          (if-let [stop (when (and (pos? i) (not= i checked) (segment-start? c)) (await (walk/way-back c entry)))]
            stop
            (do (when (segment-start? c) (ctx/update-mem! c assoc :checked i))
                (await (stair-part! c plan i))))
          j (if-let [stop (when (and (not= feet entry) (not= (+ steps j) checked)) (await (walk/way-back c entry)))]
              stop
              (do (ctx/update-mem! c assoc :checked (+ steps j))
                  (if (= feet stand) (await (open-over-target! c feet)) (await (run-step! c feet)))))
          :else (let [r (await (child/walk-into! c :in entry))]
                  (cond
                    (= :continue r) :continue
                    (and (:arrived r) (= entry (reach/standing-vec (:primitives c)))) :again
                    :else {:reason :walk-in-failed :cell entry :walk r :outside true}))))))

(defn way-out
  "What jobs.access.leave-tunnel needs of the tunnel so far: {:line :dug :torches}."
  [c]
  (let [{:keys [plan dug]} (ctx/mem c)]
    {:line (select-keys plan [:entry :heading :dir :steps :run :stand :target]) :dug (or dug [])
     :torches (standing-torches (:block-at (stair/rules-in c (reach/standing-vec (:primitives c)))) plan)}))

(defn ^:async retreat!
  "Leave after a stop: a dead end through jobs.access.leave-tunnel (torches back, mouth sealed), a kept tunnel by the
  plain walk back to the entry; then finish with the stop."
  [c]
  (let [{:keys [stop plan way-out]} (ctx/mem c)
        keep? (:keep (:args c))
        r (if keep?
            (await (child/walk-into! c :out (:entry plan)))
            (let [k (await (declined/call-child! c :out 'jobs.access.leave-tunnel
                                           (cond-> {:tunnel way-out :ignore-zones? (boolean (:ignore-zones? (:args c)))}
                                             (some? (:fetch (:args c))) (assoc :fetch (:fetch (:args c))))))]
              (cond (= :done k) (ctx/child-result c :out)
                    (= :declined k) :declined
                    :else :continue)))]
    (cond
      (#{:continue :declined} r) r
      keep? (finish! c (:reason stop) (assoc (dissoc stop :reason) :out (= (:entry plan) (reach/standing-vec (:primitives c))) :walk-out r))
      :else (finish! c (:reason stop) (assoc (dissoc stop :reason) :out (= (:entry plan) (reach/standing-vec (:primitives c))) :leave r)))))

(defn stop!
  "After a stop: finish where the body is (outside, at the entry, or no way back in a kept tunnel), else walk out first;
  a dead end with no way back goes through leave-tunnel, which digs its own way out."
  [c {:keys [reason outside] :as stop}]
  (if (or outside (and (= :no-way-back reason) (:keep (:args c))) (= (reach/standing-vec (:primitives c)) (:entry (:plan (ctx/mem c)))))
    (finish! c reason (cond-> (dissoc stop :reason :outside) outside (assoc :inside false)))
    (do (ctx/update-mem! c assoc :stop stop :way-out (way-out c)) :again)))

(defn bad-target? [t] (not (and (vector? t) (every? int? t))))

(defn refusal
  "While no line is chosen: the approach's stop when every heading is refused by a zone, claim or plan, else nil."
  [c]
  (let [{:keys [max-length accept]} (:args c)
        target (target-of c)
        a (when-not (or (:plan (ctx/mem c)) (bad-target? target))
            (ln/approach (dissoc (stair/rules-in c (reach/standing-vec (:primitives c))) :feet) target (reach/standing-vec (:primitives c)) max-length (set accept)))]
    (when (contains? #{:zone :footprint :claim} (:reason a)) a)))

(defn check
  "Declines without a zone list and while every line is refused (wait :refused); then the fetch check."
  [c]
  (if (and (nil? (known/zones c)) (not (:ignore-zones? (:args c))))
    (access/decline! c :tunnel.declined "tunnel" {:reason :no-zones})
    (if-let [a (refusal c)]
      (access/decline! c :tunnel.declined "tunnel" (assoc (access/refusal-fields [a]) :reason :refused))
      (and (trip/torches-check c) (fetch/declined-check c 'jobs.access.tunnel)))))

(defn ^:async choose!
  "Judge the lines and keep the best, or finish with why there is none. A refusal that read cells the body has not
  seen is decided again after a look at them (jobs.lib.sense/decide!)."
  [c]
  (let [{:keys [max-length accept]} (:args c)
        target (target-of c)
        feet (reach/standing-vec (:primitives c))]
    (if (bad-target? target)
      (finish! c :bad-args {:why "target must be [x y z] of integers"})
      (let [a (await (sense/decide! c ::approach #(ln/approach (assoc (dissoc (stair/rules-in c feet) :feet) :filler? (some? (choose/filler c :build))) target feet max-length (set accept))
                                    {:retry? #(not (:entry %))}))]
        (if-let [w (when (or (not (:entry a)) (ln/far-entry? a feet))
                     (when (or (:entry a) (#{:too-far :no-approach} (:reason a)))
                       (ln/leg-target (:block-at (stair/rules-in c feet)) feet target max-length (:leg-swap (ctx/mem c)))))]
          (do (ctx/update-mem! c assoc :leg w) :again)
          (if (:entry a)
          (do (ctx/update-mem! c assoc :plan (assoc a :sites (ln/torch-sites a)) :dug [])
              (ctx/emit! c :tunnel.plan :info (assoc a :text (str "tunnel from " (pr-str (:entry a)) " " (name (:heading a))
                                                                  ", " (:steps a) " steps, run " (:run a))))
              :again)
          (finish! c (:reason a) (dissoc a :reason))))))))

(defn fetch-for-stop!
  "A :no-tool stop the fetch can cure: remember the wait so the next step fetches for it; :again. Nil when not."
  [c stop]
  (let [w {:reason :no-tool :block (:block stop)}]
    (when (and (= :no-tool (:reason stop)) (fetch/due c 'jobs.access.tunnel w))
      (ctx/update-mem! c assoc :fetch-wait w)
      :again)))

(defn ^:async leg!
  "The first leg of a diagonal target: a kept tunnel child to the waypoint (its stand is in the target's column), then
  the plan is chosen again from there. :again, :continue or :declined; else the job ends with the leg's stop."
  [c w]
  (let [{:keys [fetch accept ignore-zones? on-lava max-length]} (:args c)
        r (await (declined/call-child! c :leg 'jobs.access.tunnel
                                       (cond-> {:target w :keep true :max-length max-length :accept accept
                                                :ignore-zones? ignore-zones? :on-lava on-lava}
                                         (some? fetch) (assoc :fetch fetch))))]
    (if (not= :done r)
      (if (= :declined r) :declined :continue)
      (let [res (ctx/child-result c :leg)]
        (if (= :done (:status res))
          (do (ctx/update-mem! c dissoc :leg) :again)
          (if-let [alt (when-not (:leg-swap (ctx/mem c))
                         (let [feet (reach/standing-vec (:primitives c))]
                           (ln/leg-target (:block-at (stair/rules-in c feet)) feet (target-of c) max-length true)))]
            (do (ctx/update-mem! c assoc :leg-swap true :leg alt) :again)
            (finish! c (:reason res) (assoc (select-keys res [:cell :hazards :zone :claim :plan :fluid :block :tool :walk :why :headings])
                                            :in :leg :dug (:dug res)))))))))

(defn ^:async work-round [c]
  (declined/begin! c)
  (let [m (ctx/mem c)]
    (cond
      (:leg m) (await (leg! c (:leg m)))
      (nil? (:plan m)) (await (choose! c))
      (:stop m) (await (retreat! c))
      :else (let [r (await (work! c))]
              (cond
                (#{:again :continue :declined} r) r
                (= :reached r) (finish! c :reached {})
                :else (or (fetch-for-stop! c r) (stop! c r)))))))

(defn ^:async step!
  "One piece of the tunnel: the fetch part first (the stair's booked wait, or the stop's remembered one), then the
  work. :again, :continue (a child waits on the world), :declined or :done."
  [c]
  (let [r (await (fetch/step! c 'jobs.access.tunnel (or (fetch/booked-wait c) (:fetch-wait (ctx/mem c))) {:return? true}))]
    (when (and (:fetch-wait (ctx/mem c)) (nil? (:fetching (ctx/mem c))))
      (ctx/update-mem! c dissoc :fetch-wait))
    (cond
      r r
      :else (await (work-round c)))))

(defn ^:async round
  "The whole tunnel, in and (a dead end) out: step! until it ends, a pace between; :continue after (stair/max-steps) of
  them or while a child waits on the world."
  [c]
  (or (await (trip/torches-ready! c))
      (let [n (atom 0)]
        (await (pace/steps! c #(if (< (swap! n inc) (if-let [m (:max-steps (:args c))] (inc m) (stair/max-steps))) (step! c) :continue))))))
