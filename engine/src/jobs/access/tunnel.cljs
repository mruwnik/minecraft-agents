(ns jobs.access.tunnel
  (:require [jobs.lib.block-sets :as bs]
            [engine.args :as a]
            [jobs.lib.waits :as waits]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [engine.settings :as settings]
            [jobs.access.stair :as stair]
            [jobs.access.stair.judge :as judge]
            [jobs.lib.stair-cells :as geo]
            [jobs.access.stair.step :as sstep]
            [jobs.access.tunnel.estimate :refer [target-of steps-estimate cells-estimate bad-target? cause-keys ending-result stop-of run-index]]
            [jobs.access.tunnel.line :as ln]
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
            [jobs.lib.trip :as trip]
            [jobs.lib.walk.drive :as drive]))

(def doc
  "Cut a way to stand beside a buried block :target [x y z] and stop there, with the target the next cell ahead
  at feet height. A bad :target is :impossible :bad-args before any step. A level run along a heading is jobs.gather.strip-tunnel. The target itself is left. With no zone list read the job waits (:no-zones) until one is.

  The way is one straight line along a heading (:heading limits it to one) through the target's column:
  1. An entry stand: on the surface (the column's top cell over a solid floor, two open cells; no entry from
     water), or the body's own height when it stands underground in the entry column or can walk to it along an open row (a corridor's run-up behind it).
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
  entry is planned on a fresh pathWorld and must be whole and walkable, else :no-way-back.

  A stop ends the job where the body stands; walking home is the caller's (go-to to :entry in the data). Torches are
  not hung: the caller lights the way afterwards with jobs.build.light-area over the line's cells.

  One call does it all; :continue only while a child waits on the world.

  A target off the body's row and column, with the body underground and no line entered within 24 blocks, is
  reached in two legs: a kept tunnel child along the shorter axis to a waypoint one past the target's column, then
  the line above from the body's own cell. A leg that stops is tried once on the other axis; a
  floor of air is bridged with a carried building block (jobs.access.stair.step/bridge!), as the stair does. A target on the body's
  row or column is cut the same way: a leg to its column or row (at most :max-length long).

  The level run walked is remembered as a :tunnel-way, so go-to's own way up links it.

  A run cell that fills again before the walk (gravel), or gravel landing in the body's own head or feet cell, is dug again.

  The body's cell is the progress. On the stair line the stair child goes on, on the run line the run, at the
  stand it is done, anywhere else it walks to the entry. A cell dug before a cut is air and is not dug again.
  Dug cells are left and recorded.

  A root tunnel marks itself for the :ore-seen trigger (jobs.lib.ore-pick): ore seen beside the line is dug, never the target.

  Done (:already before the start) when the body stands beside :target at its height with the cell over it open. Ends :done {:reason :reached}; a stop is condition-failed for a fact (the reasons above, :no-tool, :no-way-back, the
  stair's) and nothing-possible for a failed attempt (:walk-in-failed, :step-failed, :dig-failed, :refills);
  :tool-spent is a fact (no tool left above its reserve). Data of every ending: {:reason :target :entry :heading :stand :at
  [x y z] :dug [{:cell :block}] :inside bool :line {:entry :heading :dir :steps :run :stand :target}}
  plus detail. :inside is true when the body is off the entry, on the way in. 

  Before the first dig a worn last pickaxe gets a spare (jobs.lib.trip/tool-ready!; tool.no-spare says when none could be had).
  Every carried pickaxe down to jobs.lib.trip's ::spent-uses (3) uses stops the tunnel {:reason :tool-spent} before the last breaks (jobs.access.stair; the call's :settings {:jobs.lib.trip/spent-uses 0} digs on).
  The call's :ways (jobs.lib.need; only #{:carried} ends :no-tool): a missing pickaxe is got inside the jobs.blocks.dig child of the dig
  (the stair's and the run's own), then the body walks back and goes on; a fetch that fails or cures nothing, or no fetch allowed, digs the block bare-handed (slow, no drop) unless :need-drop is true (then it ends :no-tool with its reason as :because); :need-drop false digs bare-handed without fetching.")

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a tunnel may take." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(a/defargs args
  {:target {:doc "the buried block [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :heading {:doc "take only a line along this heading" :spec #{:north :east :south :west} :default nil}
   :max-length {:doc "longest line, in blocks along the heading from the entry to the target, 1 to 96" :spec (a/int-in 1 96) :default 24}
   :accept {:doc "dig hazards taken (jobs.lib.access.rules): #{:fluid-adjacent :lava-adjacent :falling-block}" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{}}
   :ignore-zones? :shared
   :through {:doc "the id of a plan the tunnel is cut for (a build reaching cells in rock): its footprint is no obstacle" :spec string? :default nil}
   :on-lava {:doc "exposed lava: :seal (fill it with a building block, then go on) or :stop (jobs.access.stair)" :spec #{:seal :stop} :default :seal}
   :need-drop {:doc "false: dig a block no carried tool clears bare-handed (slow, the drop is lost) instead of fetching a tool; unset: bare-handed when no tool can be fetched (jobs.access.stair)" :spec (a/or-of boolean? #{:auto}) :default :auto}})

(def safe? (price/staying :long (ready/safe-at-by-night (fn [c] (a/cell (:target (:args c)))) false true)))

(def fit? ready/fit?)

(def ways need/fetch-ways)

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

(def can-start?
  (cond/and (checks/args-fit "target must be [x y z] of integers" #(not (bad-target? (target-of %))))
            checks/zones-read not-refused))

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
  "The result data of the tunnel now: {:reason r :target :at :dug :inside} plus the line's
  entry, heading and stand once one is chosen, then detail."
  [c reason detail]
  (let [{:keys [plan dug]} (ctx/mem c)
        feet (reach/standing-vec (:primitives c))]
    (merge (cond-> {:reason reason :target (target-of c)
                    :at feet :dug (or dug []) :inside (boolean (and plan (not= feet (:entry plan))))}
             plan (assoc :line (select-keys plan [:entry :heading :dir :steps :run :stand :target])))
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
          (ctx/end c (cond (= :bad-args reason) :impossible (facts reason) :condition-failed :else :nothing-possible) cause
                   (assoc result :text (str "tunnel stopped: " (name reason) (some->> (:cell detail) pr-str (str " at ")))))))))

(defn ^:async dig-cell!
  "jobs.access.stair.step/dig! with :again for a dug cell (more to do), :continue while its child waits, else a stop map."
  [c in cell cut accept]
  (let [r (await (sstep/dig! c in cell cut accept))]
    (case r :continue :again :yield :continue r)))

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
    (or (when-let [r (await (sstep/clear-own-cell! c in feet accept))] (case r :continue :again :yield :continue r))
        (await (sstep/lava-step! c :tunnel.sealed (:block-at in) feet cut))
        (let [stop (await (sstep/judge! c in cells accept))]
          (if (and (= :no-floor (:reason stop)) (bs/open-floor? (:block stop)))
            (await (sstep/bridge! c in cells))
            stop))
        (if-let [cell (first (remove #(bs/air ((:block-at in) %)) cut))]
          (or (await (sstep/peek! c cell)) (await (dig-cell! c in cell cut accept)))
          (or (await (look/settle! c cut))
              (let [r (await (child/walk-into!* c :walk next))]
                (cond
                  (= :continue r) :continue
                  (and (:arrived (:data r)) (= next (reach/standing-vec (:primitives c))))
                  (do (ctx/update-mem! c update :run-steps (fnil max 0) (run-index (:end (ln/line (:plan (ctx/mem c)))) next (:heading plan) (:run plan)))
                      (ctx/emit! c :tunnel.step :info {:at next :text (str "tunnel step to " (pr-str next))})
                      :again)
                  (some #(not (bs/air ((:block-at (judge/rules-in c feet)) %))) cut) :again
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
        (if (bs/air ((:block-at in) over))
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
                  (bs/air (block-at [tx (inc ty) tz]))))))

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
  "After a tunnel that reached its stand: the level run walked is remembered (:tunnel-way)."
  [c ending]
  (when (= :done (:kind ending))
    (record-run! c))
  (ctx/end c :done nil {}))

(defn ^:async stair-part!
  "One call of the stair child over the whole stair (it hands over what it dug): :again once it is cut, :continue while the child waits on the world, or a stop map when the stair ended otherwise (its
  :no-tool reaches the fetch). :stair-done once the body stands at the stair's end."
  [c {:keys [heading dir] :as plan}]
  (let [cells (ln/line-cells plan)
        e (waits/waiting-out c (await (ctx/call c :stair 'jobs.access.stair
                           {:dir dir :heading heading :ways (need/call-ways c) :need-drop (:need-drop (:args c)) :y ((cells (:steps plan)) 1) :on-lava (:on-lava (:args c) :seal)
                            :accept (set (:accept (:args c))) :ignore-zones? (boolean (:ignore-zones? (:args c))) :through (:through (:args c))})))]
    (if (= :continue e)
      :continue
      (let [res (:data e)
            done? (= :done (:kind e))]
        (ctx/update-mem! c #(-> % (update :dug (fnil into []) (:dug res))
                                (assoc :stair-done (and done? (= (cells (:steps plan)) (reach/standing-vec (:primitives c)))))))
        (if done? :again (stop-of e :stair))))))

(defn segment-start?
  "Whether no stair child is in flight."
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
        j (run-index end feet heading run)
        i (when (pos? steps) (geo/stair-index entry feet dir heading steps))]
    (ctx/update-mem! c geo/record-dug (:block-at (judge/rules-in c feet)))
    (when (empty? (:slots c)) (ore-pick/mark! c {:except [(target-of c)]}))
    (or
        (cond
          (and i (not stair-done) (or (< i steps) (not (segment-start? c))))
          (if-let [stop (when (and (pos? i) (not= i checked) (segment-start? c)) (await (drive/way-back c entry)))]
            stop
            (do (when (segment-start? c) (ctx/update-mem! c assoc :checked i))
                (await (stair-part! c plan))))
          j (if-let [stop (when (and (not= feet entry) (not= (+ steps j) checked)) (await (drive/way-back c entry)))]
              stop
              (do (ctx/update-mem! c assoc :checked (+ steps j))
                  (if (= feet stand) (await (open-over-target! c feet)) (await (run-step! c feet)))))
          :else (let [r (await (child/walk-into!* c :in entry))]
                  (cond
                    (= :continue r) :continue
                    (and (:arrived (:data r)) (= entry (reach/standing-vec (:primitives c)))) :again
                    :else {:reason :walk-in-failed :cell entry :walk (ending-result r) :outside true}))))))

(defn stop!
  "After a stop: finish where the body is; walking home is the caller's."
  [c {:keys [reason outside] :as stop}]
  (finish! c reason (cond-> (dissoc stop :reason :outside) outside (assoc :inside false))))

(defn ^:async choose!
  "Judge the lines and keep the best, or finish with why there is none. A refusal that read cells the body has not
  seen is decided again after a look at them (jobs.lib.sense/decide!)."
  [c]
  (let [{:keys [max-length accept]} (:args c)
        target (target-of c)
        feet (reach/standing-vec (:primitives c))]
    (if (bad-target? target)
      (finish! c :bad-args {:why "target must be [x y z] of integers"})
      (let [a (await (sense/decide! c ::approach #(ln/approach (assoc (dissoc (judge/rules-in c feet) :feet) :filler? (some? (choose/filler c :build))) target feet max-length (set accept) (:heading (:args c)))
                                    {:retry? #(not (:entry %))}))]
        (if-let [w (when (or (not (:entry a)) (ln/far-entry? a feet))
                     (when (or (:entry a) (#{:too-far :no-approach} (:reason a)))
                       (ln/leg-target (:block-at (judge/rules-in c feet)) feet target max-length (:leg-swap (ctx/mem c)))))]
          (do (ctx/update-mem! c assoc :leg w) :again)
          (if (:entry a)
            (do (ctx/update-mem! c assoc :plan a :dug [])
                (ctx/emit! c :tunnel.plan :info (assoc a :text (str "tunnel from " (pr-str (:entry a)) " " (name (:heading a))
                                                                    ", " (:steps a) " steps, run " (:run a))))
                :again)
            (finish! c (:reason a) (dissoc a :reason))))))))

(defn ^:async leg!
  "The first leg of a diagonal target: a kept tunnel child to the waypoint (its stand is in the target's column), then
  the plan is chosen again from there. :again or :continue; else the job ends with the leg's stop."
  [c w]
  (let [{:keys [ways accept ignore-zones? on-lava max-length need-drop through]} (:args c)
        e (waits/waiting-out c (await (ctx/call c :leg 'jobs.access.tunnel
                           (cond-> {:target w :max-length max-length :accept accept :need-drop need-drop
                                    :ignore-zones? ignore-zones? :on-lava on-lava :through through}
                             (some? ways) (assoc :ways ways)))))]
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (= :already (:cause e)))
      (finish! c :no-approach {:why "the body already stands at the leg's waypoint and no line leads on from it" :dug []})
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
      (not (:spare-checked m)) (let [{:keys [steps run]} (:plan m)]
                                 (if (= :continue (await (trip/tool-ready! c "stone" (* 3 (+ steps run)))))
                                   :continue
                                   (do (ctx/update-mem! c assoc :spare-checked true) :again)))
      :else (let [r (await (work! c))]
              (cond
                (#{:again :continue} r) r
                (= :reached r) (if (:over-open m)
                                 (finish! c :reached {})
                                 (do (ctx/update-mem! c assoc :over-open true) :again))
                :else (stop! c r))))))

(defn ^:async step!
  "One piece of the tunnel: the walk back after a fetch, the fetch, else the work. :again, :continue (a child waits on
  the world) or the ending."
  [c]
  (await (work-round c)))

(defn ^:async round
  "The whole tunnel: step! until it ends, a pace between; :continue while a child waits on the world."
  [c]
  (await (pace/steps! c #(step! c))))

(def meta {:tags [:mine :dig :light]})
