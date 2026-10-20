(ns jobs.access.stair
  (:require [jobs.lib.block-sets :as bs]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [jobs.lib.spent :as spent]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [engine.settings :as settings]
            [jobs.lib.stair-cells :as geo]
            [jobs.access.stair.judge :as judge]
            [jobs.lib.stair-memory :as smem]
            [jobs.access.stair.step :as sstep]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.access-tool :as atool]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.child :as child]
            [jobs.lib.choose :as choose]
            [jobs.lib.dig-look :refer [unknown? settle!]]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.tools :as tools]
            [jobs.lib.trip :as trip]
            [jobs.lib.walk.drive :as drive]
            [plan.shape :as shape]))

(def doc
  "Dig a 1-wide stair :down or :up (or a flat run, :level) along :heading (nil: the way the body faces) from where the body stands, :steps steps or to feet height :y.

  A step cuts three cells, top first (a :level step two), then the body walks into it (a go-to child). Before the next
  step it plans the way back to the stair's first cell on a fresh pathWorld; a way that is not whole stops :no-way-back.
  A cell the body has not seen is taken for stone and looked at before it is judged and after it is dug. Each step is
  judged when chosen and each cell again right before its dig. A stair at its last step is done without the way-back check.

  Floors: a step's floor must be solid. A floor of air gets a carried building block (jobs.lib.choose/fillers :build) after the rules'
  may-place?, against a neighbour or from the cell under it (jobs.lib.bridge/bridge-plans); a stair down also bridges back along
  its line up to setting bridge-depth cells. No block: :no-floor; no face to place against: :no-support-floor (the cut cell over the
  gap is shut with a carried block, :sealed, else :open-to). A lava or water floor is never bridged. A go-to stair (:note) writes its
  floors to the scaffold ledger for jobs.access.cleanup (setting take-back-floors?).

  Hazards: exposed lava is filled with a building block (a jobs.blocks.place child, fetched when :fetch allows; :on-lava :stop stops
  :lava-exposed instead); after a seal it waits half a second, looking, before it digs or steps on; a failed seal, or lava flowing back after a reseal, backs the body off and stops :lava-unsealed. A cut that
  now touches seen water is plugged the same way (stops :hazard with :plugged; :water-unplugged if it cannot). :accept
  (#{:fluid-adjacent :lava-adjacent :falling-block}, default #{}) takes those hazards. A falling block over the body's own column is
  never taken. The stair never digs the block it stands on, a cell it plugged (memory :stair-plug; setting dig-plugs? digs it),
  a crop, or a base block.

  Needs a pickaxe and room for the drops. A call starts only when the first step is not refused, the args give a length and the next
  dig lacks nothing this call cannot get (:refused, :no-tool, :no-free-slot). Quote: steps x cells of the step x the time of a cell.
  - A pack without room waits :no-free-slot until it changes (the make-room reflex or the caller clears it).
  - A missing pickaxe is got by the jobs.blocks.dig child (:fetch), then the body walks back onto the line and goes on; a fetch that fails ends :no-tool with its reason as :because. :fetch off ends :no-tool.
  - Before each step the exposed ores within reach are dug, those below the feet only over a solid floor (:pick-up-ores, jobs.lib.ore-pick).

  One call cuts the whole stair; it yields :continue while a fetch or walk child waits on the world. The
  body's cell is the progress: a resumed call finds its step from where the body stands on the stair line. Dug cells are left and recorded.

  Ends :done, or {:reason r ...detail}. :condition-failed for facts that bar the next cell (:refused :zone :claim :footprint :not-loaded :no-zones
  :fluid-in-cut :crop :base-block :undercuts-way :unbreakable :no-tool :no-free-slot :inventory-full :hazard :lava-exposed :plugged
  :floor-unseen :no-floor :cave-below :off-stair :no-way-back :tool-spent :y-not-beyond-feet), :impossible for :bad-args, :nothing-possible for
  failed attempts (:refills :dig-failed :step-failed :no-support-floor :lava-unsealed :water-unplugged). :ignore-zones? skips all access
  refusals but :not-loaded; a cell of a plan this body made refuses :footprint unless the spare-own-builds setting is false.
  :undercuts-way: a cut cell is the floor of an earlier stair of this body (memory :stair-way, written at every end), or a step floor of air at a step of one.
  :off-stair: off the line with max-returns walks back used up. :floor-unseen: the floor still unseen once the cut is open, looked at again after a lean.
  Data {:steps n :at [x y z] :dug [{:cell :block}] :ores [{:cell :block :got}]} plus detail (:cell :hazards :zone :why ...; the walk child's ending is the :because of :step-failed).

  A dig whose every carried tool is down to jobs.lib.trip's ::spent-uses (3) uses (trip/spent-tool?) stops :tool-spent before the last tool
  breaks, after a spare was tried once; a way out digs on with the call's :settings {:jobs.lib.trip/spent-uses 0}. :need-drop false digs a block no carried tool clears bare-handed (go-to's last resort).")

(a/defargs args
  {:dir {:doc ":down, :up or :level (a flat run, :steps only)" :spec #{:down :up :level} :default :down}
   :heading {:doc ":north :east :south or :west; nil: the way the body faces" :spec #{:north :east :south :west} :default nil}
   :steps {:doc "steps to cut; or give :y" :spec (a/int-in 1 nil) :default nil}
   :cut-only {:doc "true: end :done (:steps 0) once the first step is cut and settled, without stepping in" :spec boolean? :default false}
   :y {:doc "feet height to end at, instead of :steps" :spec (a/num-in nil nil) :default nil}
   :accept {:doc "dig hazards taken (jobs.lib.access.rules): #{:fluid-adjacent :lava-adjacent :falling-block}" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{}}
   :ignore-zones? :shared
   :note {:doc "a map (go-to's escalation): each cell dug in another's zone, claim or plan is written to the tidy ledger at once with it (jobs.lib.escape/note-hole!), each floor placed to the scaffold ledger (take-back-floor?)" :spec map? :default nil}
   :fetch {:doc "get a missing pickaxe instead of waiting :no-tool (jobs.lib.need), and a block to seal lava with: true, a set of kinds or a map of limits" :spec need/option? :default true}
   :on-lava {:doc "exposed lava: :seal (fill it with a building block, then go on) or :stop" :spec #{:seal :stop} :default :seal}
   :need-drop {:doc "false: dig a block no carried tool clears bare-handed (slow, the drop is lost) instead of waiting :no-tool" :spec boolean? :default true}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach on the way (below the feet only over a solid floor); false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}
   :through {:doc "the id of a mineshaft plan the stair runs down: its footprint is no obstacle (jobs.lib.shafts)" :spec string? :default nil}})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds one call may last from its start, the waits on checks and the jobs that run between its rounds included (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-returns {:default 3 :doc "Walks back onto the stair's line (after a reflex moved the body off it) one call makes before it stops :off-stair." :spec (a/int-in 0 nil)}
   ::cell-s {:default 1.2 :doc "Mean seconds one cut cell of a step takes (look, dig, walk), for the quote." :spec (a/num-in 0 nil)}})
(defn max-returns [] (settings/get settings ::max-returns))

(def safe? ready/safe-at-from)

(def fit? ready/fit?)

(def target [:dir :heading :y])

(defn heading-of
  "The call's heading, else the way the body faces."
  [c]
  (or (:heading (:args c)) (keyword (look/facing (:primitives c)))))

(defn with-heading
  "c with the :heading arg filled when the call gave none: the heading the first round chose (job memory), else the way
  the body faces."
  [c]
  (if (:heading (:args c))
    c
    (assoc-in c [:args :heading] (keyword (or (:heading (ctx/mem c)) (look/facing (:primitives c)))))))

;; ------------------------------------------------------------------ the contract

(defn with-heading-check
  "The check f over the call with its heading."
  [f]
  (fn [c] (f (with-heading c))))

(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn target-fit
  "Never :bad-args for a target the args do not give; later :y-not-beyond-feet for a :y that lies at or behind the feet."
  [c]
  (let [t (geo/target-steps (:args c) (reach/standing-vec (:primitives c)))]
    (cond
      (not (map? t)) (contract/ok :args "args fit")
      (:here t) (contract/later :y-not-beyond-feet (:error t) :why (:error t))
      :else (contract/never :bad-args (:error t) :why (:error t)))))

(defn not-refused
  "Later :refused while a zone, claim or plan refuses the first cut."
  [c]
  (if-let [stop (judge/refusal c)]
    (let [fields (access/refusal-fields [stop])]
      (checks/cause-status :later (assoc fields :reason :refused) (str "refused by " (access/refusal-text fields))))
    (contract/ok :permitted "no zone, claim or plan refuses")))

(defn not-lacking
  "Later with what the next dig lacks that this call cannot make good (:no-tool, :no-free-slot); cells not yet seen are
  looked at first (only those with a seen air face, a look never shows a buried one) when no fetch could make good a missing pickaxe and none is carried (else the round would start and
  end the call)."
  [c]
  (if-let [lack (judge/unmet c)]
    (checks/cause-status :later lack (str "lacks " (name (:reason lack))))
    (let [p (:primitives c)
          unseen (filterv #(and (unknown? p %) (some bs/air (ore-pick/face-names p %))) (judge/next-cut c))]
      (if (and (seq unseen) (:need-drop (:args c)) (not (judge/fetch-limits c)) (sstep/lacks-tool? c blocks/hidden-guess))
        (contract/look :unseen "the cells to cut were not seen" :look-at unseen)
        (contract/ok :supplied "nothing lacking")))))

(defn pack-has-room
  "Later :no-free-slot while the next dig has no room for its drop: the pack can clear (a deposit, a toss), so the stair waits."
  [c]
  (let [lack (judge/unmet c)]
    (if (= :no-free-slot (:reason lack))
      (checks/cause-status :later lack "the pack has no room for the next drop")
      (contract/ok :room "the pack has room"))))

(def can-run? (with-heading-check pack-has-room))

(def can-start?
  (cond/and (with-heading-check target-fit)
            (with-heading-check not-refused)
            (with-heading-check not-lacking)))

(defn result-of
  "The data of a stair now: {:steps :at :dug :ores} and what it placed (jobs.lib.spent) plus detail."
  [c detail]
  (let [m (ctx/mem c)]
    (merge {:steps (or (:at-step m) 0) :at (reach/standing-vec (:primitives c)) :dug (:dug m []) :ores (vec (ore-pick/picked c))}
           (spent/summary (spent/log c)) detail)))

(defn done?
  "Ok once the stair is cut and its end settled; before the start, already when :y is given and the feet stand at it.
  Else the steps cut against the steps asked."
  [c]
  (let [m (ctx/mem c)
        n (or (:at-step m) 0)
        y (:y (:args c))
        at-y? (and (some? y) (nil? (:origin m)) (== y ((reach/standing-vec (:primitives c)) 1)))]
    (if (or (:finished m) at-y?)
      (contract/ok :done (str "the stair stands at " (pr-str (reach/standing-vec (:primitives c)))) :units :steps :done n :left 0
                   :result (result-of c {}))
      (contract/later :cutting "the stair is being cut" :units :steps :done n :left (max 0 (- (or (:target m) n) n))))))

(defn ^:async cleanup
  "After a stair that ended :done: the way cut is remembered (:stair-way)."
  [c ending]
  (when (= :done (:kind ending))
    (let [steps (:steps (:data ending))
          {:keys [dir heading]} (:args (with-heading c))]
      (when (and (pos? steps) (not= :level dir))
        (smem/record-way! c (:origin (ctx/mem c)) dir heading steps))))
  (ctx/end c :done nil {}))

(defn quote-of [c]
  (let [{:keys [dir]} (:args c)
        heading (heading-of c)
        feet (reach/standing-vec (:primitives c))
        t (geo/target-steps (assoc (:args c) :heading heading) feet)
        n (if (map? t) 0 t)
        s (* n (if (= :level dir) 2 3) (settings/get settings ::cell-s))]
    {:uses {:job-time [(* 0.5 s) s (* 4 s)]} :done [0 n n]
     :moves-to (mapv + feet (map #(* n %) (geo/delta dir heading)))}))

(def facts atool/facts)

(defn finish!
  "End the stair: a stop remembers its way, and ends by reason a kind (facts, the rest nothing possible) with the
  detail and a :text in the cause and, with what the stair did, in the data. :done ends with the data; its way is cleanup's."
  [c reason detail]
  (let [data (result-of c detail)
        steps (:steps data)
        {:keys [dir heading]} (:args c)]
    (if (= :done reason)
      (ctx/end c :done nil data)
      (do (when (and (pos? steps) (not= :level dir))
            (smem/record-way! c (:origin (ctx/mem c)) dir heading steps))
          (ctx/end c (cond (= :bad-args reason) :impossible (facts reason) :condition-failed :else :nothing-possible)
                   (assoc detail :reason reason
                          :text (or (:text detail)
                                    (str "stair stopped after " steps " steps: " (name reason)
                                         (some->> (:cell detail) pr-str (str " at "))
                                         (some->> (:why detail) (str ": ")))))
                   data)))))

(defn ^:async step!
  "Walk into the cut step. :again, :continue while the walk waits, or a stop map. A step on the goal cell of a go-to up the
  chain is the same call as that go-to (a duplicate), so it is walked to the cell's centre: the same place. A cut cell
  that filled again while the body walked in (gravel landing in the head or feet cell) is dug again."
  [c next-feet cut]
  (when (= :up (:dir (:args c))) (await (sstep/clear-jump-edge! c next-feet)))
  (let [r (await (child/walk-into!* c :walk next-feet))
        r (if (= :duplicate-call (when (= :condition-failed (:kind r)) (contract/cause-key r)))
            (await (child/call-walk* c :walk-centre (update (child/walk-into-args :walk next-feet) :pos (fn [[x y z]] [(+ x 0.5) y (+ z 0.5)]))))
            r)]
    (cond
      (= :continue r) :continue
      (and (:arrived (:data r)) (= next-feet (reach/standing-vec (:primitives c))))
      (do (ctx/emit! c :stair.step :info {:at next-feet :text (str "stepped to " (pr-str next-feet))})
          :again)
      (let [block-at (:block-at (judge/rules-in c (reach/standing-vec (:primitives c))))]
        (some #(when-let [b (block-at %)] (not (bs/air b))) cut)) :again
      :else {:reason :step-failed :cell next-feet})))

(defn ^:async return!
  "The body is off the stair's line (a reflex walked it away): a go-to child (slot :return) back to the stair's cell of
  step at-step, at most max-returns times per call. :continue while it walks, :again once it ended, nil when the
  returns are used up."
  [c]
  (let [{:keys [dir heading]} (:args c)
        {:keys [origin at-step returns]} (ctx/mem c)
        cell (shape/add origin (mapv #(* (or at-step 0) %) (geo/delta dir heading)))]
    (when (< (or returns 0) (max-returns))
      (let [r (await (child/walk-into!* c :return cell))]
        (when-not (= :continue r)
          (ctx/update-mem! c update :returns (fnil inc 0))
          (let [arrived (= cell (reach/standing-vec (:primitives c)))]
            (ctx/emit! c :stair.return :info {:to cell :arrived arrived
                                              :text (str "stair " (if arrived "walked back onto" "could not walk back onto") " its line at " (pr-str cell))})))
        (if (= :continue r) :continue :again)))))

(defn ^:async work!
  "One bounded piece of the stair from the body's place on it: check the way back, dig one cell or take the step."
  [c]
  (let [{:keys [dir heading]} (:args c)
        accept (set (:accept (:args c)))
        {:keys [origin target checked]} (ctx/mem c)
        feet (reach/standing-vec (:primitives c))
        in (judge/rules-in c feet)
        i (geo/stair-index origin feet dir heading target)]
    (ctx/update-mem! c geo/record-dug (:block-at in))
    (cond
      (nil? i) (or (await (return! c))
                   {:reason :off-stair :cell feet :origin origin
                    :offset (mapv - feet origin)
                    :why (str "the body is " (pr-str (mapv - feet origin)) " from the stair's start " (pr-str origin)
                              ", off its line")})
      (= :continue (await (ore-pick/pick! c))) :continue
      :else
      (do
        (ctx/update-mem! c assoc :at-step i)
        (if-let [stop (when (and (pos? i) (not= i checked) (not= i target)) (await (drive/way-back c origin)))]
          stop
          (do
            (ctx/update-mem! c assoc :checked i)
            (if (= i target)
              (or (await (sstep/lava-step! c :stair.sealed (:block-at in) feet [])) :finished)
              (let [{:keys [next cut] :as step-cells} (-> (geo/step-cells feet dir heading)
                                                           (as-> cs (assoc cs :up? (not= :down dir)
                                                                 :bridged? (contains? (:bridged (ctx/mem c)) (:floor cs))
                                                                 :filler? (some? (choose/filler c :build)))))
                    _ (await (sstep/look-ahead! c feet next cut))
                    _ (when-not (= :stop (:on-lava (:args c))) (await (sstep/look-at-lava! c (:block-at in) feet cut)))
                    blind (when-not (= :stop (:on-lava (:args c))) (sstep/blind-cell c (:block-at in) feet cut))
                    stop (or (when blind
                               (or (await (sstep/peek! c blind))
                                   (let [r (await (sstep/dig! c in blind cut accept))] (case r :continue :again :yield :continue r))))
                             (await (sstep/lava-step! c :stair.sealed (:block-at in) feet cut))
                             (when-let [r (await (sstep/clear-own-cell! c in feet accept))] (case r :continue :again :yield :continue r))
                             (await (sstep/water-step! c (:block-at in) accept cut))
                             (await (sstep/judge-after-lava! c in step-cells accept cut feet)))]
                (if (and (= :no-floor (:reason stop)) (bs/air (:block stop)))
                  (await (sstep/bridge! c in step-cells))
                  (or stop
                    (if-let [cell (first (remove #(bs/air ((:block-at in) %)) cut))]
                      (or (await (sstep/peek! c cell))
                          (let [r (await (sstep/dig! c in cell cut accept))] (case r :continue :again :yield :continue r)))
                      (or (await (settle! c cut)) (if (:cut-only (:args c)) :finished (await (step! c next cut)))))))))))))))

(defn back-cell
  "The stair's cell one step back toward its origin from where the body stands, or nil at the origin or off the line."
  [c]
  (let [{:keys [dir heading]} (:args c)
        {:keys [origin target]} (ctx/mem c)
        i (when origin (geo/stair-index origin (reach/standing-vec (:primitives c)) dir heading target))]
    (when (and i (pos? i))
      (shape/add origin (mapv #(* (dec i) %) (geo/delta dir heading))))))

(defn ^:async back-off!
  "After a failed seal: walk one step back up the stair (a go-to child), then finish with the stop, :backed-off true
  when the body got there."
  [c]
  (let [{:keys [stop cell]} (:backoff (ctx/mem c))
        r (when cell (await (child/walk-into!* c :back cell)))]
    (if (= :continue r)
      :continue
      (finish! c (:reason stop) (assoc (dissoc stop :reason) :backed-off (boolean (and cell (= cell (reach/standing-vec (:primitives c))))))))))

(defn ^:async start!
  "The first piece: the steps to cut and a spare pickaxe in hand, the body on its start cell. :again, :continue
  while a child waits, or the :bad-args ending."
  [c]
  (let [m (ctx/mem c)
        feet (or (some-> (:start m) vec) (reach/standing-vec (:primitives c)))
        target (geo/target-steps (:args c) feet)]
    (if (map? target)
      (finish! c (if (:here target) :y-not-beyond-feet :bad-args) {:why (:error target)})
      ;; stone stands for the cells to dig: unseen cells are taken for stone (see the ns doc)
      (do
        ;; the spare's fetch may walk the body off: the stair starts where it was asked, not where the fetch ended
        (ctx/update-mem! c assoc :start feet)
        (if (= :continue (await (trip/tool-ready! c "stone" (* 3 target))))
          :continue
          (if (and (not= feet (reach/standing-vec (:primitives c))) (= :continue (await (child/walk-into!* c :start feet))))
            :continue
            (do (ctx/update-mem! c assoc :origin feet :target target :dug [])
                :again)))))))

(defn ^:async next!
  "One piece of the stair: walk back after a fetch, a fetch round, the start, a dig, a bridge, a seal or a step. :again,
  :continue (a child waits) or the ending."
  [c]
  (let [m (ctx/mem c)
        back (when-not (:backoff m) (await (need/return! c)))
        lack (when-not (or (:backoff m) back) (judge/unmet c))]
    (cond
      (:backoff m) (await (back-off! c))
      back back
      lack (finish! c (:reason lack) (dissoc lack :reason))
      (nil? (:origin m)) (await (start! c))
      :else
      (let [r (await (work! c))]
        (cond
          (#{:again :continue} r) r
          (= :finished r) (if (:finished m) (finish! c :done {}) (do (ctx/update-mem! c assoc :finished true) :again))
          (= :lava-unsealed (:reason r)) (do (ctx/update-mem! c assoc :backoff {:stop r :cell (back-cell c)}) :again)
          :else (finish! c (:reason r) (dissoc r :reason)))))))

(defn ^:async round
  "The whole stair: next! until it ends, a pace between pieces."
  [c0]
  (let [c (with-heading c0)]
    (when-not (:heading (:args c0)) (ctx/update-mem! c assoc :heading (name (:heading (:args c)))))
    (await (pace/steps! c #(next! c)))))

(def meta {:tags [:mine :dig] :position-bound? true})
