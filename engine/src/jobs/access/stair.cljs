(ns jobs.access.stair
  (:require [engine.args :as a]
            [jobs.lib.ready :as ready]
            [jobs.lib.spent :as spent]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [engine.settings :as settings]
            [jobs.access.stair.cells :as geo]
            [jobs.access.stair.judge :as judge]
            [jobs.access.stair.memory :as smem]
            [jobs.access.stair.step :as sstep]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
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
            [jobs.access.stock :as stock]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [plan.shape :as shape]))

(def doc
  "Dig a 1-wide stair :down or :up (or a flat run, :level) along :heading (nil: the way the body faces) from where the body stands, :steps steps or to feet height :y.
  A stair down first stocks torches when under 8 are carried (jobs.access.stock/torches!; :stock-torches false skips it, as does :fetch off).

  A step cuts three cells, top first, so the body can walk one block forward and one down (or up) with full
  headroom; a :level step cuts two (head, then feet) and its floors are no stair way (:stair-way below). Then the body walks into the step (a go-to child). Before cutting the next step it plans the
  way back to the stair's first cell on a fresh pathWorld. The way must be whole and walkable by the executor
  (no gap, door or swim), else the stair stops :no-way-back.

  Cells are read as the body senses them (felt, seen, remembered). A cell it has not seen is taken for stone (dig to
  see): the stair looks at each step before judging it and at each cell it dug, and the stops below fire on what is
  then seen.

  A step's floor must be solid, and for a stair down the cell under it not a fluid, nor air when no building block is carried (:cave-below), and everything loaded.
  A floor of air gets a carried building block (jobs.lib.choose/fillers :build, never an ore or valuable) after the
  rules' may-place?, against a neighbour, or from the cell under it first when only that has a face (jobs.lib.bridge/bridge-plans; a plan whose face was unseen air gives way to the next); a stair down also bridges back along its line, up to setting bridge-depth cells, from a seen solid cell, looking at unseen ones first (a cave under its last floor); a cave floor with nothing within that stops :no-support-floor (the underside of the block over it cannot be aimed at).
  The cell under a placed floor is not judged. With no such block: :no-floor with :filler :none; with no face to place
  against at any of them (a void pocket): :no-support-floor with :cell, :tried and :why. A stair down that stops so shuts the cut cell over the missing floor with a carried block (:sealed [cell]), else the stop names it (:open-to [cell]). A lava or water floor is never bridged. A go-to stair (:note) writes its placed floors to the scaffold ledger
  (purpose :stair-floor) for jobs.access.cleanup to take back once go-to's job ended (setting take-back-floors?).

  Needs a pickaxe for the blocks it cuts and room for the drops. A call starts only when the first step is not refused by a zone, claim or plan, the args give a length (:steps or :y), the next dig lacks nothing this call cannot get (conditions :refused, :no-tool, :no-free-slot; start only). Quote: steps x cells of the step x the time of a cell, ending at the last step.
  - A pack without room is made room in (jobs.storage.make-room child, once per pack content; :make-room), else it waits :no-free-slot until the pack changes (a call that starts so ends condition-failed).
  - A missing pickaxe is got with jobs.items.get-tool (:fetch, jobs.lib.need), then the body walks back to the cell it stood on and goes on; a fetch that fails, or that changes nothing, is remembered for the call and the block is dug bare-handed. With :fetch off it ends :no-tool.

  Each step is judged when chosen and each cell again right before its dig. Stops:
  - Access refusals with their reason: :zone :claim :footprint :not-loaded :no-zones. :ignore-zones? skips all
    but :not-loaded. A cell of a plan this body made refuses :footprint too unless :spare-own-builds is false.
  - :fluid-in-cut: a cut cell holds a fluid.
  - :crop: a crop, stem or farmland in a cut cell (never dug, zone or not).
  - :base-block: a chest, bed, furnace, table, door or other base block in a cut cell, or the block a door stands on (never dug, anyone's).
  - :undercuts-way: a cut cell is the floor of a stair this body cut earlier, or a step's floor is air at a step of such a stair (a level run back over it; flooring it would block the stair) (memory :stair-way, written at every
    end with the floors of the steps walked, joined to the earlier stair it goes on from).
  - :unbreakable: bedrock, barrier, portal frames, command blocks.
  - :no-tool: a pickaxe block with no pickaxe carried.
  - :inventory-full: the drop has no room.
  - :refills: a cell refilled 3 times (with a :why when the block does not fall: something put it back).
  - :hazard: a hazard not in :accept (below).
  - :off-stair: the body is off the stair line and max-returns walks back onto it (go-to children) are used up.
  - :lava-exposed: lava beside an open cell of the step (or in one) with :on-lava :stop.
  - :floor-unseen: the floor of the step is still unseen once its cut is open, looked at once more and again after a lean toward it.
  - :lava-unsealed: that lava could not be filled (:on-lava :seal, the default): the place child's outcome in :place,
    or still lava after max-seals places (a seal, then one reseal when lava flows back). The body first steps back to the stair's cell before (:backed-off) and
    digs no more.
  - :plugged: a cut cell was plugged against water by this body (this stair or an earlier job; memory :stair-plug, the fluid may be out of view); :dig-plugs true digs it.
  - :water-unplugged: a cell the stair dug turned out to touch seen water (:water not in :accept) and could not be filled.

  Water a dig lays open (a cell the stair dug now touches seen water, :water not in :accept) is plugged at once: the cell
  is filled with a building block (same child and fetch as the lava seal, event stair.plugged), and the stair stops :hazard
  with :plugged <cell>.

  Exposed lava (beside the feet, the head or an open cut cell, or in one) is filled with a building block
  (jobs.lib.choose/fillers :build, a jobs.blocks.place child; it fetches one when :fetch allows), sources behind first, one
  stair.sealed info each; then the stair goes on. After the top cut cell is dug it leans toward it and looks at the cell over it when that is unseen, so lava over the cut is capped before it pours in. Before it steps into an open cut with an unseen cell beside it, it
  waits a lava flow delay since the last dig and looks at the cut once more. Lava seen beside a cell not yet dug is a hazard as below.

  Hazards are the rules' (one per fluid beside) plus a falling block over the top cut of the next column.
  :accept is a set of :water :lava :falling-block, default #{}; a falling block over the body's own column is never taken. Water beside the cut is not taken
  by default (it can flow into the cut and onto the body's cell, which the walker cannot leave). Lava never is
  in practice, and a falling block would land on the body or refill the cut. The stair never digs the block it
  stands on.

  One call cuts the whole stair; it yields :continue only while a fetch or walk child waits on the world, or after
  max-steps digs, steps and fetch rounds. The body's cell is the progress: a resumed call finds its step from where
  the body stands on the stair line. Dug cells are left and recorded.

  Before each step it digs the exposed ores within reach (:pick-up-ores, jobs.lib.ore-pick) and returns to its place.

  Ends :done, or with a cause {:reason r ...detail}: :condition-failed for the facts that bar the next cell (the access
  refusals, :fluid-in-cut :crop :base-block :undercuts-way :unbreakable :no-tool :inventory-full :hazard :lava-exposed :plugged :floor-unseen
  :no-floor :cave-below :off-stair :no-way-back :tool-spent :y-not-beyond-feet), :impossible for :bad-args, :nothing-possible for the attempts that
  failed (:refills :dig-failed :step-failed :no-support-floor :lava-unsealed :water-unplugged). Data {:steps n :at [x y z] :dug [{:cell :block}] :ores [{:cell :block :got}]} plus detail (:cell
  :hazards :zone :walk ...), also as a stair.done info or stair.stopped warn event.

  :spent-ok (default false): a dig whose every carried tool is down to 3 uses (jobs.lib.trip/spent-tool?) stops {:reason :tool-spent :tool <kind>}
  before the last tool breaks deep down, after a spare was tried once (jobs.access.stock/spare!); true digs on.
  :need-drop (default true): false digs a block no carried tool clears bare-handed anyway (slow, its drop lost): go-to's
  last resort when it has no pickaxe. Also done once the fetch of a tool for that block failed.")

(a/defargs args
  {:dir {:doc ":down, :up or :level (a flat run, :steps only)" :spec #{:down :up :level} :default :down}
   :heading {:doc ":north :east :south or :west; nil: the way the body faces" :spec #{:north :east :south :west} :default nil}
   :steps {:doc "steps to cut; or give :y" :spec (a/int-in 1 nil) :default nil}
   :cut-only {:doc "true: end :done (:steps 0) once the first step is cut and settled, without stepping in" :spec boolean? :default false}
   :y {:doc "feet height to end at, instead of :steps" :spec (a/num-in nil nil) :default nil}
   :accept {:doc "hazards taken: #{:water :lava :falling-block}" :spec (a/coll-of #{:water :lava :falling-block}) :default #{}}
   :ignore-zones? :shared
   :note {:doc "a map (go-to's escalation): each cell dug in another's zone, claim or plan is written to the tidy ledger at once with it (jobs.lib.escape/note-hole!), each floor placed to the scaffold ledger (take-back-floor?)" :spec map? :default nil}
   :fetch {:doc "get a missing pickaxe instead of waiting :no-tool (jobs.lib.need), and a block to seal lava with: true, a set of kinds or a map of limits" :spec need/option? :default true}
   :on-lava {:doc "exposed lava: :seal (fill it with a building block, then go on) or :stop" :spec #{:seal :stop} :default :seal}
   :need-drop {:doc "false: dig a block no carried tool clears bare-handed (slow, the drop is lost) instead of waiting :no-tool" :spec boolean? :default true}
   :spent-ok {:doc "dig on with tools down to jobs.lib.trip's ::spent-uses (3) uses instead of stopping :tool-spent before the last breaks; true for a way out" :spec boolean? :default false}
   :stock-torches {:doc "a stair down first gets torches when few are carried (jobs.access.stock/torches!, needs :fetch); false for a stair a job runs as its own child" :spec boolean? :default true}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach on the way; false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}
   :make-room {:doc "a pack with no room for the next drop is made room in (a jobs.storage.make-room child that tosses junk blocks; it keeps building blocks) instead of waiting :no-free-slot" :spec boolean? :default true}
   :spare-own-builds {:doc "a cell of a plan this body made is never dug (stops :footprint naming the plan); false: it may be" :spec boolean? :default true}
   :dig-plugs {:doc "true: a cell this body plugged against water may be dug (stops :plugged otherwise)" :spec boolean? :default false}
   :through {:doc "the id of a mineshaft plan the stair runs down: its footprint is no obstacle (jobs.lib.shafts)" :spec string? :default nil}})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds one call may last from its start, the waits on checks and the jobs that run between its rounds included (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-steps {:default 400 :doc "Digs, steps, bridges and fetch rounds of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}
   ::max-returns {:default 3 :doc "Walks back onto the stair's line (after a reflex moved the body off it) one call makes before it stops :off-stair." :spec (a/int-in 0 nil)}
   ::cell-s {:default 1.2 :doc "Mean seconds one cut cell of a step takes (look, dig, walk), for the quote." :spec (a/num-in 0 nil)}})
(defn max-steps [] (settings/get settings ::max-steps))
(defn max-returns [] (settings/get settings ::max-returns))

(def safe? ready/safe-at-from)

(def fit? ready/ok)

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
          unseen (filterv #(and (unknown? p %) (some blocks/air (ore-pick/face-names p %))) (judge/next-cut c))]
      (if (and (seq unseen) (:need-drop (:args c)) (not (judge/fetch-limits c)) (sstep/lacks-tool? c blocks/hidden-guess))
        (contract/look :unseen "the cells to cut were not seen" :look-at unseen)
        (contract/ok :supplied "nothing lacking")))))

(defn pack-has-room
  "Later :no-free-slot while the next dig has no room for its drop and no make-room can cure it: the pack can clear (a deposit, a toss), so the stair waits."
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
  "After a stair that ended :done: the way cut is remembered (:stair-way) and stair.done told."
  [c ending]
  (when (= :done (:kind ending))
    (let [{:keys [steps]} (:data ending)
          {:keys [dir heading]} (:args (with-heading c))]
      (when (and (pos? steps) (not= :level dir))
        (smem/record-way! c (:origin (ctx/mem c)) dir heading steps))
      (ctx/emit! c :stair.done :info (assoc (:data ending) :status :done :reason :done :text (str "stair " (name dir) " done, " steps " steps")))))
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

(def facts
  "The stops that name a fact barring the next cell (condition-failed); the others are attempts that failed."
  #{:refused :zone :claim :footprint :no-zones :not-loaded :fluid-in-cut :crop :base-block :undercuts-way :unbreakable
    :no-tool :tool-spent :no-free-slot :inventory-full :hazard :lava-exposed :plugged :floor-unseen :no-floor :cave-below :off-stair :no-way-back :y-not-beyond-feet})

(defn finish!
  "End the stair: a stop tells its event and remembers its way, and ends by reason a kind (facts, the rest nothing
  possible) with the detail in the cause and, with what the stair did, in the data. :done ends with the data; its event
  and way are cleanup's."
  [c reason detail]
  (let [data (result-of c detail)
        steps (:steps data)
        {:keys [dir heading]} (:args c)]
    (if (= :done reason)
      (ctx/end c :done nil data)
      (do (when (and (pos? steps) (not= :level dir))
            (smem/record-way! c (:origin (ctx/mem c)) dir heading steps))
          (ctx/emit! c :stair.stopped :warn
                     (assoc data :status :stopped :reason reason
                            :text (str "stair stopped after " steps " steps: " (name reason)
                                       (some->> (:cell detail) pr-str (str " at "))
                                       (some->> (:why detail) (str ": ")))))
          (ctx/end c (cond (= :bad-args reason) :impossible (facts reason) :condition-failed :else :nothing-possible) (assoc detail :reason reason) data)))))

(defn ^:async make-room!
  "Run make-room as child :room (no walk away: the stair is one tunnel wide). :continue while it waits, else :again; a
  stop short of the room is remembered with the pack it saw, so the wait is not retried until the pack changes."
  [c]
  (let [e (await (ctx/call c :room 'jobs.storage.make-room {:away 0 :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (if (= :continue e)
      :continue
      (do (when (= :no-free-slot (:reason (judge/need c)))
            (ctx/update-mem! c assoc :room-failed (u/inventory (:primitives c))))
          :again))))

(defn give-up-fetch!
  "The fetch of a tool for block failed or changed nothing: remember it (smem/tool-failed!), so the block is dug by hand and the
  call does not ask again. :again."
  [c block e]
  (smem/tool-failed! c block)
  (ctx/update-mem! c dissoc :need/return :fetched-for)
  (ctx/emit! c :stair.fetch-failed :warn {:block block :why (some-> e contract/cause-key)
                                         :text (str "stair could not get a tool for " block ": digging it bare-handed")})
  :again)

(defn ^:async fetch-tool!
  "Get the pickaxe for lack's block (jobs.lib.need, then walk back to where the body stood): :continue while it works,
  :again once it ended. A tool asked for already for this block and still missing is a failed fetch."
  [c {:keys [block]}]
  (let [limits (judge/fetch-limits c)]
    (if (= block (:fetched-for (ctx/mem c)))
      (give-up-fetch! c block nil)
      (let [e (await (need/tool! c {:block block} (assoc (select-keys limits [:how :explore]) :return? true)))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) (do (ctx/update-mem! c assoc :fetched-for block) :again)
          :else (give-up-fetch! c block e))))))

(defn ^:async step!
  "Walk into the cut step. :again, :continue while the walk waits, or a stop map. A step on the goal cell of a go-to up the
  chain is the same call as that go-to (a duplicate), so it is walked to the cell's centre: the same place. A cut cell
  that filled again while the body walked in (gravel landing in the head or feet cell) is dug again."
  [c next-feet cut]
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
        (some #(when-let [b (block-at %)] (not (rules/air b))) cut)) :again
      :else {:reason :step-failed :cell next-feet
             :walk (merge (select-keys (:data r) [:why :arrived])
                          {:status (if (= :done (:kind r)) :done :stopped) :reason (contract/cause-key r)})})))

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
        (if-let [stop (when (and (pos? i) (not= i checked) (not= i target)) (await (walk/way-back c origin)))]
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
                    stop (or (await (sstep/lava-step! c :stair.sealed (:block-at in) feet cut))
                             (when-let [r (await (sstep/clear-own-cell! c in feet accept))] (case r :continue :again :yield :continue r))
                             (await (sstep/water-step! c (:block-at in) accept cut))
                             (await (sstep/judge! c in step-cells accept)))]
                (if (and (= :no-floor (:reason stop)) (rules/air (:block stop)))
                  (await (sstep/bridge! c in step-cells))
                  (or stop
                    (if-let [cell (first (remove #(rules/air ((:block-at in) %)) cut))]
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
  "The first piece: the steps to cut, torches and a spare pickaxe in hand, the body on its start cell. :again, :continue
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
        (if (= :continue (or (await (when (and (= :down (:dir (:args c))) (:stock-torches (:args c))) (stock/torches! c)))
                             (await (stock/spare! c "stone" (* 3 target)))))
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
        lack (when-not (or (:backoff m) back) (judge/need c))]
    (when (and (:fetched-for m) (not lack))
      (ctx/update-mem! c dissoc :fetched-for))
    (cond
      (:backoff m) (await (back-off! c))
      back back
      (and lack (judge/room-due? c lack)) (await (make-room! c))
      (and lack (judge/fetchable? c lack)) (await (fetch-tool! c lack))
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
  "The whole stair: next! until it ends, a pace between pieces; :continue after max-steps of them."
  [c0]
  (let [c (with-heading c0)
        n (atom 0)]
    (when-not (:heading (:args c0)) (ctx/update-mem! c assoc :heading (name (:heading (:args c)))))
    (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (next! c) :continue)))))

(def meta {:tags [:mine :dig] :position-bound? true})
