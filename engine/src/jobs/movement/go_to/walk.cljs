(ns jobs.movement.go-to.walk
  "One step of a go-to call's walk: the round toward the goal and what follows it (arrival, a look, a waypoint, torches, air, a
  retry, the give-up or escalation).

  Rules of go-to:
  - The walk settings (per world, body or call :settings): jobs.lib.walk.world/dark false plans dark cells like lit ones (default: a dark cell costs twice a lit one, so a lit route up to about 2x
    longer is taken: a cell seen dark, or one never seen guessed dark; look/dark-fn).
  - A call that ends other than arrived, or yields (:continue) as a hold or short of air, with the head under water first swims to the air it knows (surface-before-end!).
  - Setting go-to.tuning/goal-danger :end gives up at once (:why :goal-dangerous, :mob, :mob-pos, :mob-distance) when the goal lies within the full-cost radius of a known mob the body would flee and its toll over the stay (:stay-s, else a click's) costs more than the goal's :worth (else a tour's); :wait (default) walks on, an info goal-in-danger says so.
  - Setting jobs.lib.walk.world/dangers false plans straight past known dangers (default: the plans keep away from them, jobs.lib.threats). A
    Each steer is bounded by the call's :spend :job-time left (a caller's short leg), and the engine cuts the call at that limit; a search that goes on past it gives up :search-spent (the engine only notices at an act, and a search makes none).
  - A call starts afresh from the world: its walk counters are per call (the cut count carries over only a cut of this run's call to the same goal), and a saved escalation is dropped when the body
    is already there (a cut call's memory is a hint).
  - A walk that gets more than 1 block nearer is progress. Three walks in a row without progress give up (or escalate, below), and so
    does a goal the planner proves walled in (:goal-enclosed, :goal-cut-off) or on magma (:goal-on-magma), at once; a
    walled-in goal it does not escalate into is first walked up to (the cell nearest it the body can reach, once a call, when
    that walk fits the time left; info go-to.wall-walk).
  - A search that runs out of loaded land walks to its frontier (the loaded edge), and a walk that gets nearer that edge
    is progress too: a tour of frontiers ends when every frontier within 1000 blocks of the goal is spent, or at the end of
    a frontier walk, or at a re-plan or cut of a walk short of the goal (drive/follow!'s :walk-spent?), once the tour's price (jobs.lib.cost/tour-cost of the blocks and walks since the body last came nearer its goal than
    ever in the call) passes what the goal is worth (the setting jobs.lib.cost.value/tour-worth, else jobs.lib.cost/tour-worth of the goal's straight
    blocks at the call's start and the call's :worth): it gives up :why :explore-spent (:explored the blocks, :walks the walks).
    A search cut by its budget whose nearest node is no nearer than the body has been tours the edge of what it has seen instead (known-end-check).
  - A walk stuck or off its plan at a cell is a :walker-fault warn; the next plans of the call cost that cell
    (up to 16; after a fall also the cells above it) far more (wworld/with-avoid), so a way round is taken when there is one.
    A body such a walk left stuck in water first swims at a bank beside it (go-to.unstick, info go-to.unstuck)."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.breath :as breath]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.sense :as sense]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.escape :as escape]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.watch :as watch]
            [jobs.movement.go-to.air :as air]
            [jobs.movement.go-to.aim :as aim :refer [aimed here? in-seen-ground? look-along! guess-waypoint look-at-column! look-at-goal-air! hidden-in-ground? lookout pre-aim! sight-end see-cell]]
            [jobs.movement.go-to.cut :refer [approach-recorder jumped? record send-back-tolls tour tour-spent?]]
            [jobs.movement.go-to.detour :as detour]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.reprice :as reprice]
            [jobs.movement.go-to.restore :as restore]
            [jobs.movement.go-to.faults :refer [look-at-stuck! note-fault! refused-hang shut-foreign]]
            [jobs.movement.go-to.health :as health]
            [jobs.movement.go-to.own-way :refer [dive-end dive-floor! own-way own-way-left! plan-check straight-from! walk-own-way!]]
            [jobs.movement.go-to.result :as end :refer [known-frontier-result unloaded-end]]
            [jobs.movement.go-to.tuning :as tuning :refer [surface-descent-s dry-walk-rounds climb-dip climb-stair surface-air wet-restarts waypoint-gain dry-radius leave-water max-blocked max-searching]]
            [jobs.movement.go-to.torch :as torch :refer [dark-end fetch-torches! none-held? round-unknown]]
            [jobs.movement.go-to.unstick :as unstick]
            [jobs.movement.go-to.way-steps :as ways]
            [jobs.movement.go-to.climb :as climb]))

(defn ^:async arrived!
  "Look round once if the place is risky (jobs.lib.watch), then finish: the next job starts facing along the walk.
  A goal whose y go-to chose (goal-pos) adds :resolved, the cell it walked to, :why it (:surface, :nearest-level, :explicit), with :provisional true when that was a
  column's estimated y, :stand-in true when no ground of it was known. A y goal moved up or down its column
  (:why :explicit) adds :re-aimed true, :aimed-at the cell reached and a :text saying so, with whether the goal cell was never seen."
  [c]
  (when (tuning/look-round)
    (await (watch/watch! c {})))
  (let [{:keys [aimed aimed-why column-open]} (ctx/mem c)
        asked (when (= :explicit aimed-why) (:pos (aim/target c)))
        moved? (and asked aimed (not= (u/cell-vec asked) (u/cell-vec aimed)))
        cell-str (fn [p] (pr-str (u/cell-vec p)))]
    (end/finish! c (cond-> {:arrived true}
                     moved? (assoc :re-aimed true :aimed-at (u/cell-vec aimed)
                                   :text (str "arrived at " (cell-str aimed) ", the ground of the column of " (cell-str asked) "; the goal cell "
                                              (if (sense/unknown? (:primitives c) (u/cell-vec asked)) "was never seen" "had nothing to stand on in range")))
                     aimed (assoc :resolved aimed)
                     (and aimed aimed-why) (assoc :why aimed-why)
                     (= :guess column-open) (assoc :provisional true)
                     (= :stand-in column-open) (assoc :stand-in true)
                     (:in-water (ctx/mem c)) (assoc :in-water true)))))

(defn forget-known-land!
  "Forget the land earlier searches knew to their end (wsearch/known-land) at the go-to's first walking round and after each
  escalation (:escalations, the world changed): a new go-to, or a dug way, may open a way through it."
  [c]
  (let [epoch (:escalations (ctx/mem c) 0)]
    (when-not (= epoch (:known-epoch (ctx/mem c)))
      (wsearch/forget-known! c)
      (ctx/update-mem! c assoc :known-epoch epoch))))

(defn ^:async walk-waypoint!
  "Walk to way (guess-waypoint) on the way to pos: from there the body sees more of the land toward pos and goal-pos
  aims again. The walk keeps the call's approach (approach-recorder, mem :closest and :last-dist), so a cut during it
  sees the progress. A walk that brings it less than ::waypoint-gain nearer pos (across) is a miss (mem
  :waypoint-misses); a walk refused or failed for a reason (needs-health, no torch, a dark drop, ...) is kept as mem
  :waypoint-why (end/give-up-fields), which a give-up of the call names. :again."
  [c pos way opts]
  (let [across #(js/Math.hypot (- (:x pos) (:x %)) (- (:z pos) (:z %)))
        before (across (u/self-pos c))
        {:keys [result status]} (await (near/walk-round! (assoc c :on-pose (approach-recorder c pos)) way 1 (dissoc opts :progress)))
        left (u/dist (u/self-pos c) pos)
        why (when-not (#{"arrived" "partial" "searching"} status) (:why (end/give-up-fields result)))]
    (when-not (jumped? c)
      (ctx/update-mem! c #(cond-> (-> % (dissoc :way-looks) (assoc :last-dist left :closest (min left (:closest % left))))
                            (and why (not= :no-progress why)) (assoc :waypoint-why why)
                            (nil? why) (dissoc :waypoint-why)))
      (ctx/update-mem! c update :waypoints-tried (fnil conj #{}) [(:x way) (:y way) (:z way)])
      (when (> (across (u/self-pos c)) (- before (waypoint-gain)))
        (ctx/update-mem! c update :waypoint-misses (fnil inc 0))))
    :again))

(defn ^:async look-out!
  "Walk to the lookout stand (aim/lookout) of the unseen goal pos and look at its cell from there, so the plan to it
  reads the cell, not a tour of the unseen land round it (info go-to.lookout {:goal :stand :seen}): first for a goal
  hidden in seen ground (hidden-in-ground?), else once a found plan tours round it (lookout-check, mem :lookout-due). Seen
  in the ground it is re-aimed at once (pre-aim!). A stand not reached in max-blocked walks counts as tried. :again."
  [c pos stand opts]
  (let [[x y z] stand
        to {:x x :y y :z z}
        there? (= stand (mapv #(js/Math.floor %) ((juxt :x :y :z) (u/self-pos c))))
        status (when-not there? (:status (await (near/walk-round! (assoc c :on-pose (approach-recorder c pos)) to 0 (dissoc opts :progress)))))
        jumped (and (not there?) (jumped? c))
        walks (if (= "arrived" status) 0 (inc (:lookout-walks (ctx/mem c) 0)))]
    (cond
      jumped nil

      (or there? (= "arrived" status))
      (let [cell (mapv #(js/Math.floor (% pos)) [:x :y :z])]
        (await (sense/look-at! c cell))
        (ctx/update-mem! c #(-> % (update :lookouts (fnil conj []) stand) (dissoc :lookout-walks :lookout-due)))
        (let [seen (not (sense/unknown? (:primitives c) cell))]
          (ctx/emit! c :go-to.lookout :info {:goal (zipmap [:x :y :z] cell) :stand to :seen seen
                                             :text (str "looked at the goal " cell " from " stand (if seen "" ", still unseen"))}))
        (pre-aim! c pos pos))

      :else
      (ctx/update-mem! c #(if (< walks (max-blocked))
                            (assoc % :lookout-walks walks)
                            (-> % (update :lookouts (fnil conj []) stand) (dissoc :lookout-walks :lookout-due)))))
    :again))

(def lookout-detour "A found way to an unseen y goal longer than this many times its straight distance (plus 4) is a tour round it: go-to looks from a lookout first." 2)

(defn lookout-check
  "near/walk-round!'s :stair-check: check's why, else {:lookout true} for a found plan to pos longer than
  lookout-detour times its straight distance plus 4 while a lookout stand is due (aim/lookout)."
  [c pos check]
  (fn [{:keys [status steps] :as plan} first?]
    (or (when check (check plan first?))
        (when (and (= "found" status) (seq steps)
                   (> (detour/length steps) (+ 4 (* lookout-detour (u/dist (first steps) pos))))
                   (lookout c pos))
          {:lookout true}))))

(defn known-end-check
  "near/walk-round!'s :stair-check: check's why, else {:known-end true} for a plan over a mask (c's :unknown) to where a
  search cut by its budget got nearest (not a frontier) that ends no block nearer pos than the body has been in the call
  (approaches :closest): the budget left the search short of anywhere new, and walking there swings between ends it
  knows. Not while the round tours the edge of what the body has seen (mem :edge-mode), which it turns to instead."
  [c pos check]
  (fn [{:keys [status steps r] :as plan} first?]
    (or (when check (check plan first?))
        (let [closest (:closest (record c))]
          (when (and (:unknown c) closest (not (:edge-mode (ctx/mem c))) (= "partial" status) (seq steps)
                     (= "budget" (some-> ^js r .-reason)) (not (:frontier-taken plan))
                     (>= (u/dist (peek steps) pos) (dec closest)))
            {:known-end true})))))

(defn turn-back
  "{:cost :worth :p :cells} of a plan's floors the body has not seen (steps with :unseen-floor; nil when none): each
  priced by cost/turn-back-cost, its look a peek over a drop or an aim (cost/walk-unknown-prices), its walk back the blocks
  from the plan's start to its takeoff, its chance of not being there cost/floor-absent-chance of the call's floors found
  there or not at its height (mem :floor-counts by floor y: one surface's floors tell of each other); no second way is
  known when the plan is made, so the next-best way's extra is 0. :worth is what trying the way is worth: the chance its
  first such floor is there (1 - :p) times cost/tour-worth of the goal's straight blocks at the call's start and the
  caller's :worth."
  [c steps]
  (when (some :unseen-floor steps)
    (let [steps (vec steps)
          counts (:floor-counts (ctx/mem c))
          {:keys [peek-s aim-s]} (cost/walk-unknown-prices)
          backs (vec (reductions + 0 (map #(detour/length [%1 %2]) steps (rest steps))))
          floors (keep-indexed (fn [i s]
                                 (when (:unseen-floor s)
                                   (let [cell [(js/Math.floor (:x s)) (dec (js/Math.floor (:y s))) (js/Math.floor (:z s))]]
                                     {:look (if (< (:y s) (- (:y (nth steps (dec i))) 0.5)) peek-s aim-s) :back (nth backs (dec i))
                                      :p (cost/floor-absent-chance (get counts (second cell))) :cell cell})))
                               steps)
          p (:p (first floors))]
      {:cost (:cost (cost/turn-back-cost floors 0))
       :worth (* (- 1 p) (cost/tour-worth (:detour-from (ctx/mem c) 0) (:worth (:args c))))
       :p p
       :cells (vec (take 6 (map :cell floors)))})))

(defn turn-back-check
  "near/walk-round!'s :stair-check: check's why, else {:turn-back (turn-back)} for a plan whose unseen floors cost more
  to turn back from than trying it is worth. As a surface's floors turn out not there its p rises, so a rim of missing
  floors ends."
  [c check]
  (fn [{:keys [steps] :as plan} first?]
    (or (when check (check plan first?))
        (when-let [{:keys [cost worth] :as t} (turn-back c steps)]
          (when (> cost worth) {:turn-back t})))))

(defn count-floor!
  "The walk's on-floor: one more of the call's unseen floors at the cell's height :present, else absent (mem :floor-counts)."
  [c floor [_ y _]]
  (ctx/update-mem! c update-in [:floor-counts y (if (= :present floor) :present :absent)] (fnil inc 0)))

(defn explore-spent!
  "Give up :explore-spent with the tour so far (tour)."
  [c pos]
  (end/give-up! c pos {:status :no-path :reason :explore-spent} (update (tour c) :explored js/Math.round)))

(defn walk-opts
  "The near/walk-round! options of a go-to walk round with doors (the :doors arg as a keyword)."
  [c doors]
  (let [closed? (= :closed (some-> (:one-way (:args c)) keyword))]
    {:doors doors :explore (not closed?)
     :dangers (wworld/dangers?)
     :dark (wworld/dark?)
     :timeout-s (min near/walk-timeout-s (max 0.1 (get-in (ctx/left c) [:spend :job-time] ##Inf)))
     :one-way (when-not closed? :open)
     :shut-also (shut-foreign c)
     :budget wsearch/round-budget
     :avoid (set (:fault-cells (ctx/mem c)))
     :tolls (into (vec (:tolls (:args c))) (send-back-tolls (ctx/mem c)))
     :zone-tolls (wworld/zone-tolls?)
     :unknown (round-unknown c)}))

(defn ^:async walk-dry!
  "The walk on from an arrival in water to the dry stand dry: its own walk rounds, range 0, at most dry-walk-rounds
  while its search goes on or it gets part of the way; the last round's status."
  [c dry]
  (let [opts (walk-opts c (some-> (:doors (:args c)) keyword))]
    (loop [i 1]
      (let [{:keys [status]} (await (near/walk-round! c dry 0 opts))]
        (if (and (#{"searching" "partial"} status) (< i (dry-walk-rounds)))
          (do (await (pace/pace! c)) (recur (inc i)))
          status)))))

(defn ^:async leave-hang!
  "The body hangs in a climbable at feet cell hang within range of pos: walk on to the nearest floor stand within range
  (jobs.lib.step-off, range 0), :again; with none a hanging body is no arrival, the call ends :unreachable."
  [c pos hang]
  (let [p (:primitives c)
        [x y z] hang
        range (:range (:args c) 1)
        stand (first (step-off/candidates p {:x x :y y :z z} {:reach 3 :ok? #(<= (u/dist % pos) range)}))]
    (ctx/emit! c :go-to.hanging :info {:at hang :stand (some-> stand ((juxt :x :y :z))) :text (str "within range but hanging in a climbable at " hang)})
    (if stand
      (do (await (walk-dry! c stand)) :again)
      (end/finish! c {:status :stopped :arrived false :reason :unreachable :why :hanging
                      :text (str "the only stand within range is a climbable at " hang " with no floor under it")}))))

(defn ^:async leave-water!
  "An arrival within range of pos, in water: with the leave-water setting (default true), once a call (mem :dry-tried), walk on to the
  nearest seen dry stand within range (air/dry-stand, dry-radius of the body; walk-dry!). Out of water and in range:
  arrived; out of range: :again (the call walks back); else it ends in water, an info go-to.in-water saying why."
  [c pos]
  (let [p (:primitives c)
        r (:range (:args c) 1)
        keep? (not (leave-water))
        tried? (:dry-tried (ctx/mem c))
        dry (when-not (or keep? tried?) (air/dry-stand p pos r (u/self-pos c) (dry-radius)))
        status (when dry
                 (ctx/update-mem! c assoc :dry-tried true)
                 (await (walk-dry! c dry)))]
    (cond
      (not (breath/in-water? p)) (if (here? c pos r) (arrived! c) :again)
      (not (here? c pos r)) :again
      :else
      (do (ctx/update-mem! c assoc :in-water true)
          (when-not keep?
            (ctx/emit! c :go-to.in-water :info
                       {:at (end/feet-cell c)
                        :text (str "arrived within range but in water: "
                                   (cond dry (str "the walk on to the dry stand at " [(:x dry) (:y dry) (:z dry)] " ended " status)
                                         tried? "the walk on to a dry stand failed earlier in the call"
                                         (zero? r) "the goal cell is water"
                                         :else "no dry stand seen within range"))}))
          (arrived! c)))))

(defn ^:async arrive!
  "Within range of pos: done, but for a column whose ground is only guessed (mem :column-open :guess) walked to within
  column/approach-range of it: it looks at it (look-at-column!), its looks spent it walks on within the :range arg (mem
  :column-near). In water, leave-water!."
  [c pos]
  (let [m (ctx/mem c)
        p (:primitives c)
        wet? (and (or (not= :guess (:column-open m)) (:column-near m)) (breath/in-water? p))
        hang (refused-hang c pos)]
    (cond
      (:restore-pending m) (restore/restore-next! c)
      hang (do (ctx/update-mem! c update :hang-walks (fnil inc 0))
               (await (leave-hang! c pos hang)))
      wet? (await (leave-water! c pos))
      (or (not= :guess (:column-open m)) (:column-near m)) (arrived! c)
      (await (look-at-column! c pos)) :again
      :else (do (ctx/update-mem! c assoc :column-near true) :again))))

(def loop-starts "Rounds of one call that begin from the same cell, none improving the best distance, before they count as a loop." 4)

(defn note-start
  "[starts looped?]: starts (cell -> {:n rounds begun there :best the best distance when the first began}) with a round
  from cell added. Looped when the cell began loop-starts rounds and the best distance never improved by a block since the
  first, whatever each round reported (a budget-ended search whose partial walk goes back and forth)."
  [starts cell best]
  (let [seen (get starts cell {:n 0 :best best})
        n (inc (:n seen))]
    [(assoc starts cell (assoc seen :n n))
     (and (>= n loop-starts) (>= best (dec (:best seen))))]))

(defn ^:async clear-goal-seal!
  "A goal the walk could not reach (:goal-enclosed, :goal-cut-off, or a search that ran out of land: :exhausted, :out-of-sight, :ladder-gap, :start-enclosed)
  with blocks of the body's own ledger within ledger/seal-clear-radius of the goal (a dig-in's seal beside a gate, a plug in a stair
  mouth) or of its feet (a retreat's seal that shuts the body in, once the hostile is gone): one jobs.access.cleanup
  child takes them back: :again, so the walk is planned again. The cells the cleanup left for a known hostile (held
  :hostile-near or :danger: the shelter keeps the hostile out) are kept (mem :seal-kept): no escalation of the call cuts
  them (holes/may-dig-fn), so the way out it prices goes another side."
  [c pos result]
  (when (and (ways/way? c :clear-seal) (#{:goal-enclosed :goal-cut-off :exhausted :out-of-sight :ladder-gap :start-enclosed} (some-> (:reason result) keyword)) (not (:seal-cleared (ctx/mem c))))
    (let [at (fn [[x y z]] {:pos {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)} :radius ledger/seal-clear-radius})
          entries (ledger/open-entries (ctx/view c))
          near (first (filter #(seq (ledger/near-of entries %)) [(at [(:x pos) (:y pos) (:z pos)]) (at (end/feet-cell c))]))]
      (when near
        (ctx/update-mem! c assoc :seal-cleared true)
        (let [r (await (ctx/call c :seal-cleanup 'jobs.access.cleanup {:of :all :near near :checks {:fit? '(always)}}))]
          (ctx/update-mem! c assoc :seal-kept (into #{} (keep #(when (#{:hostile-near :danger} (:reason %)) (:cell %))) (get-in r [:data :open])))
          :again)))))

(defn budget-s
  "Seconds the call chain has left (ctx/left: job time, wall time, deadline), ##Inf when nothing bounds it."
  [c]
  (let [{:keys [spend deadline]} (ctx/left c)]
    (apply min ##Inf (cond-> (keep spend [:job-time :wall-time])
                       deadline (conj (/ (- deadline (ctx/now c)) 1000))))))

(def wall-walk-slack "Blocks farther from the goal than the nearest cell that a wall walk still takes, for a shorter walk." 1.5)

(defn ^:async walk-to-wall!
  "A walled-in goal (result :goal-enclosed, :goal-cut-off) that no escalation will open: once a call (mem :wall-walked, set once the walk arrived or got nowhere),
  walk to a cell near it the body can reach (escalation/nearest-stand: the shortest walk within wall-walk-slack of the nearest) when the nearest cell is at least a block nearer than the feet (and the one chosen nearer at all) and
  the walk there (jobs.lib.cost/walk-cost) fits budget-s. True when it walked."
  [c pos tries result opts]
  (let [p (:primitives c)
        feet (end/feet-cell c)
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        near (delay (choose/nearest-stand p goal wall-walk-slack))
        nearest (delay (choose/nearest-stand p goal))
        to (fn [a b] (u/dist (zipmap [:x :y :z] a) (zipmap [:x :y :z] b)))]
    (when (and (#{:goal-enclosed :goal-cut-off} (:reason result))
               (not (:wall-walked (ctx/mem c)))
               (not (choose/escalate? c tries result))
               (<= (to @nearest goal) (dec (to feet goal)))
               (< (to @near goal) (to feet goal))
               (<= (:cost (cost/walk-cost {:distance (to feet @near)})) (budget-s c)))
      (ctx/emit! c :go-to.wall-walk :info {:at @near :why (:reason result)
                                           :text (str "the goal is walled in: walking to " @near ", the nearest cell to it")})
      (let [walk (await (near/walk-round! (assoc c :on-pose (approach-recorder c pos)) (zipmap [:x :y :z] @near) 0 (dissoc opts :progress)))]
        ;; a walk that got nearer but was cut short is tried again; one that arrived or got nowhere is not; a jumped one is no walk
        (when-not (or (= "partial" (:status walk)) (jumped? c))
          (ctx/update-mem! c assoc :wall-walked true))
        true))))

(defn search-spent?
  "Whether the call's :spend :job-time is gone."
  [c]
  (some-> (get-in (ctx/left c) [:spend :job-time]) (<= 0)))

(defn retrying!
  "A fruitless round that plans again: an info go-to.retrying says why (the result's reason, which try), so the gap before the
  next plan is not silent."
  [c result tries]
  (let [reason (or (:why result) (:reason result) :no-path)]
    (ctx/emit! c :go-to.retrying :info {:reason reason :try tries
                                        :text (str "no way found (" (name reason) "), planning again (try " tries " of " (max-blocked) ")")})))

(defn learned-round?
  "A round that ran out of re-plans while each taught the planner an unseen cell (a void edge, a floor it could not light): the next plan knows more."
  [{:keys [status learned]}]
  (and (= :replan status) (boolean learned)))

(defn surface-descent
  "The price (s per block) of a plan step down for this round, or nil: the body is not under ground and the goal (a cell,
  not a column) lies in unloaded land, where the plan is a guess and a way down has no known way back up (it costs the climb back)."
  [c pos]
  (let [pw (wworld/path-world (:primitives c))]
    (when (and pw (:y pos) (wsearch/goal-unloaded? (.-snapshot pw) (mapv #(js/Math.floor (% pos)) [:x :y :z]))
               (not (sh-sense/buried? (:primitives c))))
      (surface-descent-s))))

(defn second-surfacing-in-air?
  "Whether a swim to air has just brought the body, head out, back to the air it started the dive from (mem :last-air):
  the dive from there did not fit its air, so another try would repeat it (go-to escalates instead)."
  [c]
  (let [{:keys [surfacings last-air]} (ctx/mem c)]
    (boolean (and (>= (or surfacings 0) 1) last-air (not (breath/head-under? (:primitives c)))
                  (<= (u/dist (u/self-pos c) last-air) 2)))))

(defn ^:async land-at-air!
  "A body in water with its head out after a swim to air (the shaft under its pocket) walks the last cell or two to where it
  last stood dry (mem :last-air): a stair cannot be cut from the water."
  [c opts]
  (let [last-air (:last-air (ctx/mem c))]
    (when (and last-air (breath/in-water? (:primitives c)) (not (breath/head-under? (:primitives c)))
               (<= (u/dist (u/self-pos c) last-air) 3))
      (await (near/walk-round! (assoc c :to-air true) (zipmap [:x :y :z] (mapv #(js/Math.floor (% last-air)) [:x :y :z])) 0 (dissoc opts :progress))))))

(defn surface-first?
  "Whether a step's result r leaves the head under water to the air first: a give-up (any ending that is not :done), a
  hold (held?: a risk hold, which ends when the world changes) or a :continue slice of the walk once the breath is short
  (breath/drowning?), so a dive that fits its air goes on below."
  [p r held?]
  (boolean (and (breath/head-under? p)
                (or (and (contract/ending? r) (not= :done (:kind r)))
                    (and (= :continue r) (or held? (breath/drowning? p)))))))

(defn ^:async surface-before-end!
  "A step's result r that surface-first? names first swims to the air it knows (air/swim-to-air!, last-air included), so a
  body is never left under water (an exclusive run has no breathe reflex)."
  [c pos r]
  (when (surface-first? (:primitives c) r (:held (ctx/mem c)))
    (let [opts (dissoc (walk-opts c (some-> (:doors (:args c)) keyword)) :progress)]
      (loop [n 0]
        (when (and (< n air/max-surfacings) (ctx/alive? c) (breath/head-under? (:primitives c))
                   (await (air/swim-to-air! c opts pos "ending under water" (fn []))))
          (recur (inc n))))))
  r)

(def max-landings "Landings at the air a go-to call tries before it escalates from wherever it is." 3)

(defn ^:async land-and-escalate!
  "land-at-air!, then a stair escalation once the body stands out of the water; else the next round plans again (landings are capped)."
  [c opts]
  (ctx/update-mem! c update :landings (fnil inc 0))
  (await (land-at-air! c opts))
  (when-not (breath/in-water? (:primitives c))
    (ctx/update-mem! c assoc :escalate-now true :stair-tried true))
  :again)

(def max-column-reaims "Times a learn-spent walk to a column re-aims at its newly seen ground before it gives up." 3)

(defn ^:async look-or-re-aim-column!
  "A walk spent its learned replans: a column goal aims again if its ground has come into view (up to
  max-column-reaims times), else the body looks around once from near where it stands (a slope bumped into column by
  column, or the drop beside a lit stair, is seen at a glance). True when either happened."
  [c pos]
  (let [aimed-pos (when (and (:column-open (ctx/mem c)) (< (:column-reaims (ctx/mem c) 0) max-column-reaims))
                    (:pos (aimed c {:x (:x pos) :z (:z pos)})))]
    (if (and aimed-pos (not= (:y aimed-pos) (:y pos)))
      (do (ctx/update-mem! c update :column-reaims (fnil inc 0)) true)
      (boolean (await (look/survey! c :near))))))

(defn ^:async walk! [c0 pos range doors]
  (let [way (own-way c0 pos (dive-floor! c0 pos))
        _ (own-way-left! c0 way)
        floor (dive-floor! c0 pos)
        {:keys [own-way-top cave-ok]} (ctx/mem c0)
        c (cond-> c0
            (and own-way-top (not cave-ok)) (assoc :own-floor (- own-way-top (climb/cave-dip)))
            (:over-budget (ctx/mem c0)) (assoc :over-budget true)
            (surface-descent c0 pos) (assoc :descent-s (surface-descent c0 pos))
            floor (assoc :dive-floor floor :dive-stair (climb-stair))
            (and floor (not (:cave-ok (ctx/mem c0)))) (assoc :cave-floor (- (+ floor (climb-dip)) (climb/cave-dip))))
        from (u/self-pos c)
        _ (when-not (breath/head-under? (:primitives c)) (ctx/update-mem! c0 assoc :last-air (air/breathing-pos (:primitives c) from)))
        d (u/dist from pos)
        pw (wworld/path-world (:primitives c))
        stand (when (and pw (not way) (or (:lookout-due (ctx/mem c)) (and (not (wsearch/goal-unloaded? (.-snapshot pw) (mapv #(js/Math.floor (% pos)) [:x :y :z]))) (hidden-in-ground? c pos)))) (lookout c pos))]
    (cond
      (and (here? c pos range) (not (refused-hang c pos)))
      (await (arrive! c pos))

      (nil? pw)
      (end/refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})

      way
      (await (walk-own-way! c way (walk-opts c doors)))

      stand
      (await (look-out! c pos stand (walk-opts c doors)))

      :else
      (let [_ (forget-known-land! c)
            seen-goal? (and (see-cell c) (not (:edge-mode (ctx/mem c))) (not (wsearch/goal-unloaded? (.-snapshot pw) (mapv #(js/Math.floor (% pos)) [:x :y :z]))))
            opts (cond-> (assoc (walk-opts c doors) :progress (empty? (:frontier-best (ctx/mem c))))
                   seen-goal? (assoc :explore false))
            stair-check (turn-back-check c (reprice/check c pos (known-end-check c pos (lookout-check c pos (plan-check c pos (choose/stair-check c pos) (straight-from! c d))))))
            round (await (near/walk-round! (assoc c :on-pose (approach-recorder c pos) :on-floor #(count-floor! c %1 %2) :walk-spent? #(tour-spent? c)) pos range (assoc opts :see-cell (see-cell c) :stair-check stair-check)))]
        (if (jumped? c)
          :again
        (let [{walked :result status :status to :to} round
            status (if (and (= "stair" status) (:searching (:why walked))) "searching" status)
            _ (ctx/hold-still! c (when (= "searching" status) :searching))
            fetched (when (= :no-torch (:status walked)) (await (fetch-torches! c)))
            aided? (when (and (not fetched) (not= "arrived" status) (torch/torch-aid? c walked)) (await (torch/hold-torch! c)))
            looked? (when (and (not aided?) (= :air (:reason walked))) (await (look-at-goal-air! c pos)))
            surfaced? (when (and (not aided?) (not looked?) (or (= :air (:reason walked)) (air/short-under-water? c status walked pos))) (await (air/surface! c (dissoc opts :progress) pos)))
            way-looked? (when (and (not fetched) (= :goal-not-standable (:reason walked))) (await (look-along! c pos)))
            way (when (and (not fetched) (not way-looked?) (= :goal-not-standable (:reason walked))) (guess-waypoint c pos))
            col-looked? (when (and (not aided?) (not looked?) (not surfaced?) (not fetched) (not way) (not way-looked?) (not (#{"arrived" "searching"} status)))
                          (await (look-at-column! c pos)))
            surveyed? (when (and (:unknown c) (= :exhausted (:reason walked)) (breath/head-under? (:primitives c)) (not (look/surveyed? c :near)))
                        (await (look/survey! c :near))
                        true)
            result (known-frontier-result walked)
            left (u/dist to pos)
            best (:best (ctx/mem c) d)
            frontier (:frontier result)
            fcell (when frontier (zipmap [:x :y :z] frontier))
            fbests (:frontier-best (ctx/mem c) {})
            fbest (when frontier (get fbests frontier (u/dist from fcell)))
            explored? (boolean (and frontier (< (u/dist to fcell) (dec fbest))))
            target (:frontier-target result)
            tbests (:target-best (ctx/mem c) {})
            tdist (when target (u/dist to {:x (first target) :y (:y to) :z (second target)}))
            nearer? (boolean (and target (< tdist (dec (get tbests target js/Infinity)))))
            _ (ctx/update-mem! c assoc :tour-walks (if (< left (dec best)) 0 (cond-> (:tour-walks (ctx/mem c) 0) frontier inc)))]
        (cond
          (= "arrived" status)
          (await (arrive! c pos))

          (:out-of-time walked)
          (if (search-spent? c) (end/give-up! c pos {:status :no-path :reason :search-spent}) :again)

          (and (:learn-spent walked) (await (look-or-re-aim-column! c pos)))
          :again

          (:learn-spent walked)
          (await (esc/give-up-or-escalate! c pos (max-blocked) status (cond-> {:status :no-path :reason :unseen-floors :cells (:cells walked)} (none-held? c) (assoc :torches :none))))

          (and (= "stair" status) (:risk (:why walked)))
          (reprice/end c (:risk (:why walked)))

          (and (= "stair" status) (= :dives (:why walked)))
          (esc/dive-refused! c pos)

          (and (= "stair" status) (:lookout (:why walked)))
          (do (ctx/update-mem! c assoc :lookout-due true) :again)

          (and (= "stair" status) (:turn-back (:why walked)))
          (let [{:keys [cost worth p cells]} (:turn-back (:why walked))]
            (await (esc/give-up-or-escalate! c pos (max-blocked) status (cond-> {:status :no-path :reason :unseen-floors :cells cells :p-absent p :turn-back-s cost :worth-s worth}
                                                                           (none-held? c) (assoc :torches :none)))))

          (and (= "stair" status) (:known-end (:why walked)))
          (do (ctx/update-mem! c assoc :edge-mode true)
              (ctx/emit! c :go-to.edge-tour :info {:text "the search ran out of nodes short of anywhere nearer than the body has been: touring to the edge of what it has seen" :at (mapv #(js/Math.floor (% from)) [:x :y :z])})
              :again)

          (and (= "stair" status) (:detour (:why walked)))
          (let [why (dissoc (:why walked) :detour)]
            (let [e (choose/detour-way c pos)]
              (case (:step e)
                nil (let [{:keys [extra-s worth-s] :as priced} (detour/price why (:worth (:args c)))]
                      (if (<= extra-s worth-s)
                        (do (ctx/update-mem! c assoc :detour-ok true) :again)
                        (end/give-up! c pos {:status :no-path :reason :detour} (merge why priced))))
                :searching :again
                (esc/detour-escalate! c (:blocked (ctx/mem c) 0) why e))))

          (and (= "stair" status) (:air-spent (:why walked)) (< (:landings (ctx/mem c) 0) max-landings))
          (do (await (air/surface! c (dissoc opts :progress) pos))
              (await (land-and-escalate! c opts)))

          (= "stair" status)
          (do (ctx/update-mem! c #(cond-> (assoc % :escalate-now true) (not= :retreats (:why walked)) (assoc :stair-tried true))) :again)

          (and (or frontier (= :tour-spent (:status walked))) (tour-spent? c))
          (explore-spent! c pos)

          fetched fetched

          way-looked? :again

          way (await (walk-waypoint! c pos way opts))

          ;; the planner proved no cell in range stands: aim at the column's ground once (goal-pos, mem :re-aim); under
          ;; :unknown it proves nothing next to unseen cells, so a goal cell seen in the ground is re-aimed too
          (and (not (:re-aim (ctx/mem c)))
               (or (= :goal-not-standable (:reason walked)) (in-seen-ground? c pos))
               (:resolved (aimed c pos)))
          (do (ctx/update-mem! c assoc :re-aim true) :again)

          aided? :again

          looked? :again

          surveyed? :again

          (and (or surfaced? (= :air (:reason walked))) (ways/way? c :stair) (not (:stair-tried (ctx/mem c))) (< (:landings (ctx/mem c) 0) max-landings) (second-surfacing-in-air? c))
          (await (land-and-escalate! c opts))

          surfaced? :again

          col-looked? :again

          (and (= "searching" status)
               (await (air/surface-while-searching! c (dissoc opts :progress) pos (:fresh walked)
                                                    {:surface-air (surface-air) :restarts (wet-restarts)})))
          :again

          (and (= "searching" status) (search-spent? c))
          (end/give-up! c pos {:status :no-path :reason :search-spent})

          (= "searching" status)
          (let [n (cond-> (:searching (ctx/mem c) 0) (:fresh walked) inc)]
            (ctx/update-mem! c assoc :searching n)
            (if (< n (max-searching))
              :again
              (end/give-up! c pos {:status :no-path :reason :moved-while-searching})))

          :else
          (let [handled (when (health/refused? c result) (await (health/heal-or-drop! c pos range)))]
           (or (when (= :again handled) :again)
          (let [result (cond-> result (:needs-health handled) (merge (health/budget-end handled)))
                _ (note-fault! c walked)
                _ (await (look-at-stuck! c walked))
                unstuck? (await (unstick/swim-out! c pos walked))
                [starts looped?] (note-start (:starts (ctx/mem c)) (mapv #(js/Math.floor (% from)) [:x :y :z]) best)
                result (cond-> result
                         looped? (assoc :status :no-path :reason (if (escape/walk-enclosed? c) :start-enclosed :no-progress))
                         (= :budget (some-> (:reason result) keyword)) (assoc :goal-left left))
                progress? (and (not looped?) (or (< left (dec best)) explored? nearer? (learned-round? result)))
                edge-start? (and (not progress?) (:unknown c) (= :budget (some-> (:reason result) keyword)) (not (:edge-mode (ctx/mem c))))
                ;; land closed by the rock guess is not searched to its end: a call that cannot dig tours its rim first
                rock-start? (and (not progress?) (not edge-start?) (= :exhausted (some-> (:reason result) keyword)) (not (:rock-edge (ctx/mem c)))
                                 (not (ways/escalating? c)) (wworld/rock-guess? (:primitives c) (:unknown c)))
                edge-start? (or edge-start? rock-start?)
                tries (cond edge-start? 0 looped? (max-blocked) progress? 0 :else (inc (:blocked (ctx/mem c) 0)))]
            (when-not edge-start? (when-not progress? (ctx/book-try! c 'jobs.movement.go-to (or (:reason result) :no-progress))))
            (ctx/update-mem! c assoc :blocked tries :starts starts :searching 0 :wet-fresh 0 :best (if (< left (dec best)) left best)
                             :closest (min left (:closest (ctx/mem c) left)) :last-dist left
                             :frontier-best (cond-> fbests frontier (assoc frontier (min fbest (u/dist to fcell))))
                             :target-best (cond-> tbests target (assoc target (min tdist (get tbests target js/Infinity)))))
            (when (and progress? (:restore-pending (ctx/mem c))) (restore/restore-next! c))
            (cond
              rock-start?
              (do (ctx/update-mem! c assoc :rock-edge true)
                  (wsearch/forget-known! c)
                  (ctx/emit! c :go-to.edge-tour :info {:text "the search ran out of land where the body guessed rock it never saw: touring to the edge of what it has seen" :at (mapv #(js/Math.floor (% from)) [:x :y :z])}))

              edge-start?
              (do (ctx/update-mem! c assoc :edge-mode true)
                  (ctx/emit! c :go-to.edge-tour :info {:text "the search over unseen land ran out of nodes: touring to the edge of what the body has seen" :at (mapv #(js/Math.floor (% from)) [:x :y :z])})))
            (if (or unstuck? edge-start?
                    (and (or progress? (and (< tries (max-blocked)) (< tries (ctx/own-tries c)) (pos? (ctx/tries-left c)))) (not (#{:goal-enclosed :goal-cut-off :goal-on-magma} (:reason result)))))
              (do (when (and (pos? tries) (not unstuck?)) (retrying! c result tries))
                  :again)
              (let [unloaded? (wsearch/goal-unloaded? (.-snapshot pw) (mapv #(js/Math.floor (% pos)) [:x :y :z]))
                    result (dive-end c pos (if (choose/escalate? c tries result) result (dark-end c result)))
                    ended (cond-> (if (choose/escalate? c tries result) result (sight-end c pos (unloaded-end result unloaded?)))
                            (:waypoint-why (ctx/mem c)) (assoc :waypoint-why (:waypoint-why (ctx/mem c))))]
                (or (await (clear-goal-seal! c pos ended))
                    (when (await (walk-to-wall! c pos tries ended opts)) :again)
                    (await (esc/give-up-or-escalate! c pos tries status ended)))))))))))))))
