(ns jobs.access.tunnel
  (:require [engine.args :as a]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [engine.settings :as settings]
            [jobs.access.stair :as stair]
            [jobs.access.stair.judge :as judge]
            [jobs.access.stair.cells :as geo]
            [jobs.access.stair.memory :as smem]
            [jobs.access.stair.step :as sstep]
            [jobs.access.tunnel.estimate :refer [target-of steps-estimate cells-estimate bad-target? cause-keys ending-result stop-of run-index]]
            [jobs.access.tunnel.line :as ln]
            [jobs.access.tunnel.torches :as torches]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.child :as child]
            [jobs.lib.choose :as choose]
            [jobs.lib.dig-look :as look]
            [jobs.lib.ledger :as ledger]
            [plan.shape :as shape]
            [jobs.lib.need :as need]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.sense :as sense]
            [jobs.access.stock :as stock]
            [jobs.lib.walk :as walk]))

(def doc
  "Cut a way to stand beside a buried block :target [x y z] and stop there (or, with :heading and :length and no :target, a level run of :length blocks that way from the body's cell: the target is the next cell), with the target the next cell ahead
  at feet height. The target itself is left. With no zone list read the job waits (:no-zones) until one is.
  Before the first step it stocks torches when under 8 are carried (jobs.access.stock/torches!: info torch.stocked, or warn torch.no-stock and it goes on dark).

  The way is one straight line along a heading (:heading limits it to one) through the target's column:
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

  Every line refused by a zone, claim or plan waits (:refused) while all are refused. Else no
  valid line ends before any walk or dig with the reason all judged lines share (:hazard
  :cave-below :no-floor :fluid-in-cut :undercuts-way :not-loaded). :too-far: no line fits within :max-length.
  :no-approach: the headings differ, with :headings giving each one's reason.

  Then the body walks to the entry (a go-to child planning no deeper than 16 below the lower of body and entry; failing: :walk-in-failed, nothing possible) and the stair child cuts
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
  floor of air is bridged with a carried building block (jobs.access.stair.step/bridge!), as the stair does. A target on the body's
  row or column farther than :max-length is cut the same way in legs of :max-length from the body's cell.

  The level run walked is remembered as a :tunnel-way, so go-to's own way up links it.

  A run cell that fills again before the walk (gravel), or gravel landing in the body's own head or feet cell, is dug again.

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

  Done (:already before the start) when the body stands beside :target at its height with the cell over it open. Ends :done {:reason :reached}; a stop is condition-failed for a fact (the reasons above, :no-tool, :no-way-back, the
  stair's) and nothing-possible for a failed attempt (:walk-in-failed, :step-failed, :dig-failed, :refills);
  :tool-spent is a fact (no tool left above its reserve). Data of every ending: {:reason :target :entry :heading :stand :at
  [x y z] :dug [{:cell :block}] :ores :inside bool :keep bool :line {:entry :heading :dir :steps :run :stand :target}
  :torches [{:cell :site :block}] :unlit [{:cell :site :reason}]} plus detail (:out true once it walked out, :leave
  the way-out's data). :torches are those standing now, read from the world. :inside is true when the body is off
  the entry, on the way in. Also a tunnel.done info or tunnel.stopped warn event.

  :spent-ok (default false): every carried pickaxe down to 3 uses stops the tunnel {:reason :tool-spent} before the last breaks (jobs.access.stair).
  :fetch (default true; false ends :no-tool; jobs.lib.need): a missing pickaxe (the stair's :no-tool ending, or a :no-tool stop of
  the run's own digs) is got with jobs.items.get-tool, then the body walks back to the cell it stood on and goes on;
  a fetch for a block that fails or cures nothing digs that block bare-handed (slow, no drop; :by-hand false ends :no-tool); one for no block ends :no-tool. The stair child fetches only items (a block to seal lava with),
  not tools.")

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a tunnel may take, in and (a dead end) out." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(a/defargs args
  {:target {:doc "the buried block [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :heading {:doc "with no :target, dig this way from the body's cell at its height, :length blocks; with a :target, take only a line along it" :spec #{:north :east :south :west} :default nil}
   :length {:doc "with :heading and no :target, blocks to dig (default :max-length less one)" :spec (a/int-in 1 96) :default nil}
   :max-length {:doc "longest line, in blocks along the heading from the entry to the target, 1 to 96" :spec (a/int-in 1 96) :default 24}
   :accept {:doc "hazards taken: #{:water :lava :falling-block}" :spec (a/coll-of #{:water :lava :falling-block}) :default #{}}
   :ignore-zones? :shared
   :on-lava {:doc "exposed lava: :seal (fill it with a building block, then go on) or :stop (jobs.access.stair)" :spec #{:seal :stop} :default :seal}
   :keep {:doc "a tunnel that stays: torches left and the tunnel left open; false (a dead end): torches go into the scaffold ledger for jobs.access.leave-tunnel to take back" :spec boolean? :default false}
   :max-steps {:doc "give the round back with :continue after this many steps (a stair segment, a run cell), for a caller that looks around between them; nil: the stair's ::max-steps" :spec (a/int-in 1 nil) :default nil}
   :fetch {:doc "get a missing pickaxe instead of ending :no-tool (jobs.lib.need): true, a set of kinds or a map of limits; false ends :no-tool" :spec need/option? :default true}
   :by-hand {:doc "a block whose tool could not be got is dug bare-handed (slow, no drop); false ends :no-tool instead" :spec boolean? :default true}
   :spent-ok {:doc "dig on with tools down to 3 uses instead of stopping :tool-spent (see jobs.access.stair)" :spec boolean? :default false}
   :spare-own-builds {:doc "a cell of a plan this body made is never dug; false: it may be (see jobs.access.stair)" :spec boolean? :default true}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach on the way; false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}})

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:target (:args c))))))

(def fit? ready/ok)

(def target [:target])

(def defaults {:spend {:job-time ::job-time-s}})

(defn refusal
  "While no line is chosen: the approach's stop when every heading is refused by a zone, claim or plan, else nil."
  [c]
  (let [{:keys [max-length accept]} (:args c)
        target (target-of c)
        a (when-not (or (:plan (ctx/mem c)) (bad-target? target))
            (ln/approach (dissoc (judge/rules-in c (reach/standing-vec (:primitives c))) :feet) target (reach/standing-vec (:primitives c)) max-length (set accept) (:heading (:args c))))]
    (when (contains? #{:zone :footprint :claim} (:reason a)) a)))

(defn not-refused
  "Later :refused while every heading is refused by a zone, claim or plan, before a line is chosen."
  [c]
  (if-let [a (refusal c)]
    (let [fields (access/refusal-fields [a])]
      (checks/cause-status :later (assoc fields :reason :refused) (str "refused by " (access/refusal-text fields))))
    (contract/ok :permitted "no zone, claim or plan refuses")))

(def can-start? (cond/and checks/zones-read not-refused))

(defn quote-of [c]
  (let [feet (reach/standing-vec (:primitives c))
        t (target-of c)
        s (* (if t (cells-estimate feet t) 0) (settings/get stair/settings ::stair/cell-s))]
    (cond-> {:uses {:job-time [(* 0.5 s) s (max (* 4 s) (time-s))]}}
      t (assoc :moves-to t))))

(def facts
  "The stops that name a fact barring the way (condition-failed); the others are attempts that failed."
  (into stair/facts #{:too-far :no-approach}))

(defn record-run!
  "Remember the level run walked (memory :tunnel-way, as the strip mine does), so go-to's own way up links the tunnel."
  [c]
  (let [{:keys [plan run-steps]} (ctx/mem c)]
    (when (pos? (or run-steps 0))
      (ledger/record-tunnel! c (:end (ln/line plan)) (shape/headings (:heading plan)) run-steps))))

(defn result-of
  "The result data of the tunnel now: {:reason r :target :at :dug :ores :inside :keep :torches :unlit} plus the line's
  entry, heading and stand once one is chosen, then detail."
  [c reason detail]
  (let [{:keys [plan dug unlit ores]} (ctx/mem c)
        feet (reach/standing-vec (:primitives c))]
    (merge (cond-> {:reason reason :target (target-of c)
                    :at feet :dug (or dug []) :ores (into (vec ores) (ore-pick/picked c)) :inside (boolean (and plan (not= feet (:entry plan))))
                    :keep (boolean (:keep (:args c))) :torches [] :unlit (or unlit [])}
             plan (assoc :line (select-keys plan [:entry :heading :dir :steps :run :stand :target])
                         :torches (torches/standing-torches (:block-at (judge/rules-in c feet)) plan)))
           (select-keys plan [:entry :heading :stand])
           detail)))

(defn finish!
  "End the tunnel: :done when reached (its event and the tunnel-way are cleanup's), else by reason a kind (facts, the
  rest nothing possible) with the detail in the cause and the whole result as data."
  [c reason detail]
  (let [result (result-of c reason detail)
        cause (assoc (select-keys detail cause-keys) :reason reason)]
    (if (= :reached reason)
      (ctx/end c :done nil result)
      (do (record-run! c)
          (ctx/emit! c :tunnel.stopped :warn
                     (assoc result :status :stopped :text (str "tunnel stopped: " (name reason) (some->> (:cell detail) pr-str (str " at ")))))
          (ctx/end c (cond (= :bad-args reason) :impossible (facts reason) :condition-failed :else :nothing-possible) cause result)))))

(defn ^:async dig-cell!
  "jobs.access.stair.step/dig! with :again for a dug cell (more to do), else a stop map."
  [c in cell cut accept]
  (let [r (await (sstep/dig! c in cell cut accept))]
    (if (= :continue r) :again r)))

(defn ^:async run-step!
  "Dig one cell of the next run step or walk into it: :again, :continue while the walk waits, or a stop map."
  [c feet]
  (let [{:keys [plan]} (ctx/mem c)
        accept (set (:accept (:args c)))
        in (judge/rules-in c feet)
        base (geo/step-cells feet :level (:heading plan))
        {:keys [next cut] :as cells} (assoc base :bridged? (contains? (:bridged (ctx/mem c)) (:floor base))
                                                 :filler? (some? (choose/filler c :build)))]
    (await (sstep/look-ahead! c feet next cut))
    (or (when-let [r (await (sstep/clear-own-cell! c in feet accept))] (if (= :continue r) :again r))
        (await (sstep/lava-step! c :tunnel.sealed (:block-at in) feet cut))
        (let [stop (await (sstep/judge! c in cells accept))]
          (if (and (= :no-floor (:reason stop)) (rules/air (:block stop)))
            (await (sstep/bridge! c in cells))
            stop))
        (if-let [cell (first (remove #(rules/air ((:block-at in) %)) cut))]
          (or (await (sstep/peek! c cell)) (await (dig-cell! c in cell cut accept)))
          (or (await (look/settle! c cut))
              (let [r (await (child/walk-into!* c :walk next))]
                (cond
                  (= :continue r) :continue
                  (and (:arrived (:data r)) (= next (reach/standing-vec (:primitives c))))
                  (do (ctx/update-mem! c update :run-steps (fnil max 0) (run-index (:end (ln/line (:plan (ctx/mem c)))) next (:heading plan) (:run plan)))
                      (ctx/emit! c :tunnel.step :info {:at next :text (str "tunnel step to " (pr-str next))})
                      :again)
                  (some #(not (rules/air ((:block-at (judge/rules-in c feet)) %))) cut) :again
                  :else {:reason :step-failed :cell next :walk (ending-result r)})))))))

(defn ^:async open-over-target!
  "At the stand: the target's step judged again and the cell over the target dug; :reached once it is open."
  [c feet]
  (let [accept (set (:accept (:args c)))
        in (judge/rules-in c feet)
        {:keys [next cut] :as cells} (geo/step-cells feet :level (:heading (:plan (ctx/mem c))))
        over (first cut)]
    (await (sstep/look-ahead! c feet next cut))
    (or (await (sstep/lava-step! c :tunnel.sealed (:block-at in) feet [over]))
        (await (sstep/judge! c in cells accept))
        (if (rules/air ((:block-at in) over))
          (or (await (look/settle! c [over])) :reached)
          (or (await (sstep/peek! c over)) (await (dig-cell! c in over cut accept)))))))

(defn line-index
  "The index of feet on the line of cells (0..n), or nil."
  [cells feet]
  (first (keep-indexed (fn [i cell] (when (and (= cell feet) (< i (dec (count cells)))) i)) cells)))

(defn at-stand?
  "Whether the body stands beside the target at its height with the cell over the target open: the goal, whatever
  line got it there."
  [c]
  (let [feet (reach/standing-vec (:primitives c))
        [tx ty tz :as t] (target-of c)
        block-at (:block-at (judge/rules-in c feet))]
    (boolean (and t (not (bad-target? t)) (= 1 (apply + (map #(js/Math.abs %) (map - t feet)))) (= ty (feet 1))
                  (rules/air (block-at [tx (inc ty) tz]))))))

(defn done?
  "Ok once the body stands beside :target at its height with the cell over the target open and settled (before the
  start: already); else the steps walked against the line's, or against the estimate before a line is chosen."
  [c]
  (let [{:keys [plan]} (ctx/mem c)
        feet (reach/standing-vec (:primitives c))
        n (if plan (+ (:steps plan) (:run plan)) (if-let [t (target-of c)] (steps-estimate feet t) 0))
        k (if plan (or (line-index (ln/line-cells plan) feet) 0) 0)]
    (if (and (at-stand? c) (or (nil? plan) (:over-open (ctx/mem c))))
      (contract/ok :reached (str "stands beside " (pr-str (target-of c))) :units :steps :done n :left 0
                   :result (result-of c :reached {:stand feet}))
      (contract/later :digging "the tunnel is being dug" :units :steps :done k :left (max 0 (- n k))))))

(defn ^:async cleanup
  "After a tunnel that reached its stand: the level run walked is remembered (:tunnel-way) and tunnel.done told."
  [c ending]
  (when (= :done (:kind ending))
    (record-run! c)
    (ctx/emit! c :tunnel.done :info (assoc (:data ending) :status :done :text (str "tunnel reached " (pr-str (:stand (:data ending)))))))
  (ctx/end c :done nil {}))

(defn segment-end
  "The line index the stair child cuts to from k: the standing cell (s+1) of the next site s >= k, else the stair's end."
  [{:keys [sites steps]} k]
  (min steps (or (some #(when (>= % k) (inc %)) sites) steps)))

(defn stair-fetch
  "The :fetch the stair child gets from the tunnel's: its items (a seal block), not tools (the tunnel fetches those),
  nil when that is off."
  [fetch]
  (let [limits (need/parse-arg fetch)]
    (when (and limits (contains? (or (:what limits) need/kinds) :item))
      (assoc limits :what #{:item}))))

(defn ^:async stair-part!
  "One call of the stair child over the segment from line index k (it hands over what it dug): :again once the
  segment is cut, :continue while the child waits on the world, or a stop map when the stair ended otherwise (its
  :no-tool reaches the fetch). :stair-done once the body stands at the stair's end."
  [c {:keys [heading dir] :as plan} k]
  (let [cells (ln/line-cells plan)
        e (await (ctx/call c :stair 'jobs.access.stair
                           {:dir dir :heading heading :fetch (stair-fetch (:fetch (:args c))) :need-drop (empty? (:tool-failed (ctx/mem c))) :y ((cells (segment-end plan k)) 1) :on-lava (:on-lava (:args c) :seal)
                            :accept (set (:accept (:args c))) :ignore-zones? (boolean (:ignore-zones? (:args c))) :spent-ok (boolean (:spent-ok (:args c))) :spare-own-builds (:spare-own-builds (:args c))
                            :pick-up-ores (:pick-up-ores (:args c))}))]
    (if (= :continue e)
      :continue
      (let [res (:data e)
            done? (= :done (:kind e))]
        (ctx/update-mem! c #(-> % (update :dug (fnil into []) (:dug res)) (update :ores (fnil into []) (:ores res))
                                (assoc :stair-done (and done? (= (cells (:steps plan)) (reach/standing-vec (:primitives c)))))))
        (if done? :again (stop-of e :stair))))))

(defn segment-start?
  "Whether no stair child is in flight: the next call cuts a new segment."
  [c]
  (empty? (get-in (ctx/mem c) [:children :stair])))

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
        i (when (pos? steps) (geo/stair-index entry feet dir heading steps))]
    (ctx/update-mem! c geo/record-dug (:block-at (judge/rules-in c feet)))
    (or (when k (await (torches/torch-step! c k)))
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
          :else (let [r (await (child/walk-into!* c :in entry))]
                  (cond
                    (= :continue r) :continue
                    (and (:arrived (:data r)) (= entry (reach/standing-vec (:primitives c)))) :again
                    :else {:reason :walk-in-failed :cell entry :walk (ending-result r) :outside true}))))))

(defn way-out
  "What jobs.access.leave-tunnel needs of the tunnel so far: {:line :dug :torches}."
  [c]
  (let [{:keys [plan dug]} (ctx/mem c)]
    {:line (select-keys plan [:entry :heading :dir :steps :run :stand :target]) :dug (or dug [])
     :torches (torches/standing-torches (:block-at (judge/rules-in c (reach/standing-vec (:primitives c)))) plan)}))

(defn ^:async retreat!
  "Leave after a stop: a dead end through jobs.access.leave-tunnel (torches back, mouth sealed), a kept tunnel by the
  plain walk back to the entry; then finish with the stop."
  [c]
  (let [{:keys [stop plan way-out]} (ctx/mem c)
        keep? (:keep (:args c))
        e (if keep?
            (await (child/walk-into!* c :out (:entry plan)))
            (await (ctx/call c :out 'jobs.access.leave-tunnel
                             (cond-> {:tunnel way-out :ignore-zones? (boolean (:ignore-zones? (:args c)))}
                               (some? (:fetch (:args c))) (assoc :fetch (:fetch (:args c)))))))]
    (if (= :continue e)
      :continue
      (finish! c (:reason stop)
               (assoc (dissoc stop :reason) :out (= (:entry plan) (reach/standing-vec (:primitives c)))
                      (if keep? :walk-out :leave) (ending-result e))))))

(defn stop!
  "After a stop: finish where the body is (outside, at the entry, or no way back in a kept tunnel), else walk out first;
  a dead end with no way back goes through leave-tunnel, which digs its own way out."
  [c {:keys [reason outside] :as stop}]
  (if (or outside (and (= :no-way-back reason) (:keep (:args c))) (= (reach/standing-vec (:primitives c)) (:entry (:plan (ctx/mem c)))))
    (finish! c reason (cond-> (dissoc stop :reason :outside) outside (assoc :inside false)))
    (do (ctx/update-mem! c assoc :stop stop :way-out (way-out c)) :again)))

(defn ^:async choose!
  "Judge the lines and keep the best, or finish with why there is none. A refusal that read cells the body has not
  seen is decided again after a look at them (jobs.lib.sense/decide!)."
  [c]
  (let [{:keys [max-length accept]} (:args c)
        target (target-of c)
        feet (reach/standing-vec (:primitives c))]
    (if (bad-target? target)
      (finish! c :bad-args {:why "target must be [x y z] of integers, or a heading with a length"})
      (let [_ (when-not (:start (ctx/mem c)) (ctx/update-mem! c assoc :start feet))
            a (await (sense/decide! c ::approach #(ln/approach (assoc (dissoc (judge/rules-in c feet) :feet) :filler? (some? (choose/filler c :build))) target feet max-length (set accept) (:heading (:args c)))
                                    {:retry? #(not (:entry %))}))]
        (if-let [w (when (or (not (:entry a)) (ln/far-entry? a feet))
                     (when (or (:entry a) (#{:too-far :no-approach} (:reason a)))
                       (ln/leg-target (:block-at (judge/rules-in c feet)) feet target max-length (:leg-swap (ctx/mem c)))))]
          (do (ctx/update-mem! c assoc :leg w) :again)
          (if (:entry a)
            (do (ctx/update-mem! c assoc :plan (assoc a :sites (ln/torch-sites a)) :dug [])
                (ctx/emit! c :tunnel.plan :info (assoc a :text (str "tunnel from " (pr-str (:entry a)) " " (name (:heading a))
                                                                    ", " (:steps a) " steps, run " (:run a))))
                :again)
            (finish! c (:reason a) (dissoc a :reason))))))))

(defn fetch-limits
  "The limits map of :fetch when it allows a tool, else nil."
  [c]
  (let [o (need/opts c 'jobs.access.tunnel)]
    (when (and o (contains? (:what o) :tool)) o)))

(defn fetch-for-stop!
  "A :no-tool stop the fetch can cure: remember the wait so the next step fetches for it; :again. Nil when not."
  [c stop]
  (when (and (= :no-tool (:reason stop)) (fetch-limits c))
    (ctx/update-mem! c assoc :fetch-wait (select-keys stop [:block :tool]))
    :again))

(defn ^:async fetch-tool!
  "Get the pickaxe the stop lacked (jobs.lib.need, then the walk back to where the body stood): :continue while it
  works, :again once it ended :done, else the stop :no-tool with the fetch's reason."
  [c {:keys [block tool] :as wait}]
  (let [e (await (need/tool! c (cond block {:block block} :else {:kind (or tool "pickaxe")})
                             (assoc (select-keys (fetch-limits c) [:how :explore]) :return? true)))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/update-mem! c dissoc :fetch-wait) :again)
      (and block (:by-hand (:args c))) (do (smem/tool-failed! c block)
                (ctx/update-mem! c dissoc :fetch-wait :need/return)
                (ctx/emit! c :tunnel.fetch-failed :warn {:block block :text (str "tunnel could not get a tool for " block ": digging it bare-handed")})
                :again)
      :else (do (ctx/update-mem! c dissoc :fetch-wait :need/return)
                (stop! c (assoc wait :reason :no-tool :fetch (contract/cause-key e)))))))

(defn ^:async leg!
  "The first leg of a diagonal target: a kept tunnel child to the waypoint (its stand is in the target's column), then
  the plan is chosen again from there. :again or :continue; else the job ends with the leg's stop."
  [c w]
  (let [{:keys [fetch accept ignore-zones? on-lava max-length by-hand]} (:args c)
        e (await (ctx/call c :leg 'jobs.access.tunnel
                           (cond-> {:target w :keep true :max-length max-length :accept accept :by-hand by-hand
                                    :ignore-zones? ignore-zones? :on-lava on-lava}
                             (some? fetch) (assoc :fetch fetch))))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/update-mem! c dissoc :leg) :again)
      :else
      (if-let [alt (when-not (:leg-swap (ctx/mem c))
                     (let [feet (reach/standing-vec (:primitives c))]
                       (ln/leg-target (:block-at (judge/rules-in c feet)) feet (target-of c) max-length true)))]
        (do (ctx/update-mem! c assoc :leg-swap true :leg alt) :again)
        (let [stop (stop-of e :leg)]
          (finish! c (:reason stop) (assoc (dissoc stop :reason) :dug (:dug (:data e)))))))))

(defn ^:async work-round [c]
  (let [m (ctx/mem c)]
    (cond
      (:leg m) (await (leg! c (:leg m)))
      (nil? (:plan m)) (await (choose! c))
      (:stop m) (await (retreat! c))
      :else (let [r (await (work! c))]
              (cond
                (#{:again :continue} r) r
                (= :reached r) (if (:over-open m)
                                 (finish! c :reached {})
                                 (do (ctx/update-mem! c assoc :over-open true) :again))
                :else (or (fetch-for-stop! c r) (stop! c r)))))))

(defn ^:async step!
  "One piece of the tunnel: the walk back after a fetch, the fetch, else the work. :again, :continue (a child waits on
  the world) or the ending."
  [c]
  (let [back (await (need/return! c))
        wait (:fetch-wait (ctx/mem c))]
    (cond
      back back
      wait (await (fetch-tool! c wait))
      :else (await (work-round c)))))

(defn ^:async round
  "The whole tunnel, in and (a dead end) out: step! until it ends, a pace between; :continue after (stair/max-steps) of
  them or while a child waits on the world."
  [c]
  (or (await (stock/torches! c))
      (let [n (atom 0)]
        (await (pace/steps! c #(if (< (swap! n inc) (if-let [m (:max-steps (:args c))] (inc m) (stair/max-steps))) (step! c) :continue))))))

(def meta {:tags [:mine :dig :light]})
