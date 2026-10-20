(ns jobs.movement.go-to.walk
  "One step of a go-to call's walk: the round toward the goal and what follows it (arrival, a look, a waypoint, torches, air, a
  retry, the give-up or escalation).

  Rules of go-to:
  - The walk settings (per world, body or call :settings): jobs.lib.walk.world/dark false plans dark cells like lit ones (default: a dark cell costs twice a lit one, so a lit route up to about 2x
    longer is taken: a cell seen and dark, or one never seen at night; look/dark-fn).
  - Setting go-to.tuning/goal-danger :end gives up at once (:why :goal-dangerous, :mob, :mob-pos, :mob-distance) when the goal lies within the full-cost radius of a known mob the body would flee; :wait (default) walks on, an info goal-in-danger says so.
  - Setting jobs.lib.walk.world/dangers false plans straight past known dangers (default: the plans keep away from them, jobs.lib.threats). A
    :spend :job-time of n walks one leg of at most n s and, when it got more than 1 block nearer, ends limit-reached
    {:reason :leg :line :job-time}, data {:arrived false :leg true}, for a caller chasing a moving target to call again; a
    leg that got no nearer goes on as any round, and a search that goes on past n s gives up :leg-search-spent.
  - A call starts afresh from the world: its walk counters are per call (the cut count carries over only a cut of this run's call to the same goal), and a saved escalation is dropped when the body
    is already there (a cut call's memory is a hint).
  - A walk that gets more than 1 block nearer is progress. Three walks in a row without progress give up (or escalate, below), and so
    does a goal the planner proves walled in (:goal-enclosed, :goal-cut-off) or on magma (:goal-on-magma), at once; a
    walled-in goal it does not escalate into is first walked up to (the cell nearest it the body can reach, once a call, when
    that walk fits the time left; info go-to.wall-walk).
  - A search that runs out of loaded land walks to its frontier (the loaded edge), and a walk that gets nearer that edge
    is progress too: a tour of frontiers ends when every frontier within 1000 blocks of the goal is spent, or at the end of
    a frontier walk once the tour's price (jobs.lib.cost/tour-cost of the blocks and walks since the body last came nearer its goal than
    ever in the call) passes what the goal is worth (the setting jobs.lib.cost.value/tour-worth, default jobs.lib.cost/tour-worth of the goal's straight
    blocks at the call's start): it gives up :why :explore-spent (:explored the blocks, :walks the walks).
  - A walk stuck or off its plan at a cell is a :walker-fault warn; the next plans of the call cost that cell
    (up to 16; after a fall also the cells above it) far more (wworld/with-avoid), so a way round is taken when there is one.
    A body such a walk left stuck in water first swims at a bank beside it (go-to.unstick, info go-to.unstuck)."
  (:require [engine.ctx :as ctx]
            [jobs.lib.breath :as breath]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.sense :as sense]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.util :as u]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.watch :as watch]
            [jobs.movement.go-to.air :as air]
            [jobs.movement.go-to.aim :refer [aimed here? in-seen-ground? look-along! guess-waypoint look-at-column! look-at-goal-air! hidden-in-ground? lookout pre-aim! sight-end see-cell]]
            [jobs.movement.go-to.cut :refer [approach-recorder tour tour-spent?]]
            [jobs.movement.go-to.detour :as detour]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.restore :as restore]
            [jobs.movement.go-to.faults :refer [note-fault! refused-hang shut-foreign]]
            [jobs.movement.go-to.health :as health]
            [jobs.movement.go-to.own-way :refer [dive-end dive-floor! own-way own-way-left! plan-check straight-from! walk-own-way!]]
            [jobs.movement.go-to.result :as end :refer [known-frontier-result unloaded-end]]
            [jobs.movement.go-to.tuning :as tuning :refer [dry-walk-rounds climb-dip climb-stair surface-air wet-restarts waypoint-gain dry-radius leave-water max-blocked max-searching]]
            [jobs.movement.go-to.torch :as torch :refer [dark-end fetch-torches! round-unknown]]
            [jobs.movement.go-to.unstick :as unstick]
            [jobs.movement.go-to.way-steps :as ways]
            [jobs.movement.go-to.climb :as climb]))

(defn ^:async arrived!
  "Look round once if the place is risky (jobs.lib.watch), then finish: the next job starts facing along the walk.
  A goal whose y go-to chose (goal-pos) adds :resolved, the cell it walked to, :why it (:surface, :nearest-level, :explicit), with :provisional true when that was a
  column's estimated y, :stand-in true when no ground of it was known."
  [c]
  (when (tuning/look-round)
    (await (watch/watch! c {})))
  (let [{:keys [aimed aimed-why column-open]} (ctx/mem c)]
    (end/finish! c (cond-> {:arrived true}
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
    (ctx/update-mem! c #(cond-> (-> % (dissoc :way-looks) (assoc :last-dist left :closest (min left (:closest % left))))
                          (and why (not= :no-progress why)) (assoc :waypoint-why why)
                          (nil? why) (dissoc :waypoint-why)))
    (ctx/update-mem! c update :waypoints-tried (fnil conj #{}) [(:x way) (:y way) (:z way)])
    (when (> (across (u/self-pos c)) (- before (waypoint-gain)))
      (ctx/update-mem! c update :waypoint-misses (fnil inc 0)))
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
        walks (if (= "arrived" status) 0 (inc (:lookout-walks (ctx/mem c) 0)))]
    (if (or there? (= "arrived" status))
      (let [cell (mapv #(js/Math.floor (% pos)) [:x :y :z])]
        (await (sense/look-at! c cell))
        (ctx/update-mem! c #(-> % (update :lookouts (fnil conj []) stand) (dissoc :lookout-walks :lookout-due)))
        (let [seen (not (sense/unknown? (:primitives c) cell))]
          (ctx/emit! c :go-to.lookout :info {:goal (zipmap [:x :y :z] cell) :stand to :seen seen
                                             :text (str "looked at the goal " cell " from " stand (if seen "" ", still unseen"))}))
        (pre-aim! c pos pos))
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

(defn walk-opts
  "The near/walk-round! options of a go-to walk round with doors (the :doors arg as a keyword)."
  [c doors]
  (let [closed? (= :closed (some-> (:one-way (:args c)) keyword))]
    {:doors doors :explore (not closed?)
     :dangers (wworld/dangers?)
     :dark (wworld/dark?)
     :timeout-s (or (:leg-s (:args c)) near/walk-timeout-s)
     :one-way (when-not closed? :open)
     :shut-also (shut-foreign c)
     :budget wsearch/round-budget
     :avoid (set (:fault-cells (ctx/mem c)))
     :tolls (:tolls (:args c))
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
          (do (await (pace/pace!)) (recur (inc i)))
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

(def seal-clear-radius 3)

(defn ^:async clear-goal-seal!
  "A goal the walk could not reach (:goal-enclosed, :goal-cut-off, or a search that ran out of land: :exhausted, :out-of-sight, :ladder-gap)
  with blocks of the body's own ledger within seal-clear-radius of it (a dig-in's seal beside a gate, a plug in a stair
  mouth), or a body shut in (:start-enclosed) with such blocks within seal-clear-radius of its feet (a retreat's seal
  once the hostile is gone): one jobs.access.cleanup child takes them back. True when it ran, so the walk is planned again."
  [c pos result]
  (let [[x y z] (if (= :start-enclosed (some-> (:reason result) keyword)) (end/feet-cell c) [(:x pos) (:y pos) (:z pos)])
        near {:pos {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)} :radius seal-clear-radius}]
    (when (and (ways/way? c :clear-seal) (#{:goal-enclosed :goal-cut-off :exhausted :out-of-sight :ladder-gap :start-enclosed} (some-> (:reason result) keyword)) (not (:seal-cleared (ctx/mem c)))
               (seq (ledger/near-of (ledger/open-entries (ctx/view c)) near)))
      (ctx/update-mem! c assoc :seal-cleared true)
      (await (ctx/call c :seal-cleanup 'jobs.access.cleanup {:job :all :near near :checks {:fit? '(always)}}))
      true)))

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
        ;; a walk that got nearer but was cut short is tried again; one that arrived or got nowhere is not
        (when-not (= "partial" (:status walk))
          (ctx/update-mem! c assoc :wall-walked true))
        true))))

(defn leg-alert?
  "Whether :leg-alert-within is set and a close-range attacker is within it, or the body lost health since start-attempt!."
  [c]
  (let [{:keys [leg-alert-within leg-alert-s]} (:args c)
        p (:primitives c)]
    (boolean (and leg-alert-within leg-alert-s
                  (or (< (.-health (.self p)) (:leg-hp (ctx/mem c) js/Infinity))
                      (some #(not (or (combat/ranged? %) (combat/enderman? %)))
                            (danger-q/known-hostiles p leg-alert-within {:ranged-radius 0})))))))

(defn leg-search-spent?
  "Whether a call with :leg-s has used its seconds: its :spend :job-time is gone (the engine only notices at the next
  act or round, and a search makes none), or, while leg-alert? holds, :leg-alert-s of it."
  [c]
  (let [{:keys [leg-s leg-alert-s]} (:args c)
        left (get-in (ctx/left c) [:spend :job-time])]
    (boolean (and leg-s left
                  (or (<= left 0) (and (leg-alert? c) (>= (- leg-s left) leg-alert-s)))))))

(defn retrying!
  "A fruitless round that plans again: an info go-to.retrying says why (the result's reason, which try), so the gap before the
  next plan is not silent."
  [c result tries]
  (let [reason (or (:why result) (:reason result) :no-path)]
    (ctx/emit! c :go-to.retrying :info {:reason reason :try tries
                                        :text (str "no way found (" (name reason) "), planning again (try " tries " of " (max-blocked) ")")})))

(defn ^:async walk! [c0 pos range doors]
  (let [way (own-way c0 pos (dive-floor! c0 pos))
        _ (own-way-left! c0 way)
        floor (dive-floor! c0 pos)
        {:keys [own-way-top cave-ok]} (ctx/mem c0)
        c (cond-> c0
            (and own-way-top (not cave-ok)) (assoc :own-floor (- own-way-top (climb/cave-dip)))
            (:over-budget (ctx/mem c0)) (assoc :over-budget true)
            floor (assoc :dive-floor floor :dive-stair (climb-stair))
            (and floor (not (:cave-ok (ctx/mem c0)))) (assoc :cave-floor (- (+ floor (climb-dip)) (climb/cave-dip))))
        from (u/self-pos c)
        d (u/dist from pos)
        pw (wworld/path-world (:primitives c))
        stand (when (and pw (not way) (or (:lookout-due (ctx/mem c)) (hidden-in-ground? c pos))) (lookout c pos))]
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
            seen-goal? (and (see-cell c) (not (wsearch/goal-unloaded? (.-snapshot pw) (mapv #(js/Math.floor (% pos)) [:x :y :z]))))
            opts (cond-> (assoc (walk-opts c doors) :progress (empty? (:frontier-best (ctx/mem c))))
                   seen-goal? (assoc :explore false))
            stair-check (lookout-check c pos (plan-check c pos (choose/stair-check c pos) (straight-from! c d)))
            {walked :result status :status to :to} (await (near/walk-round! (assoc c :on-pose (approach-recorder c pos)) pos range (assoc opts :see-cell (see-cell c) :stair-check stair-check)))
            _ (ctx/hold-still! c (when (= "searching" status) :searching))
            fetched (when (= :no-torch (:status walked)) (await (fetch-torches! c)))
            aided? (when (and (not fetched) (not= "arrived" status) (torch/torch-aid? c walked)) (await (torch/hold-torch! c)))
            looked? (when (and (not aided?) (= :air (:reason walked))) (await (look-at-goal-air! c pos)))
            surfaced? (when (and (not aided?) (not looked?) (or (= :air (:reason walked)) (air/short-under-water? c status walked pos))) (await (air/surface! c (dissoc opts :progress) pos)))
            way-looked? (when (and (not fetched) (= :goal-not-standable (:reason walked))) (await (look-along! c pos)))
            way (when (and (not fetched) (not way-looked?) (= :goal-not-standable (:reason walked))) (guess-waypoint c pos))
            col-looked? (when (and (not aided?) (not looked?) (not surfaced?) (not fetched) (not way) (not way-looked?) (not (#{"arrived" "searching"} status)))
                          (await (look-at-column! c pos)))
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

          (and (= "stair" status) (= :dives (:why walked)))
          (esc/dive-refused! c pos)

          (and (= "stair" status) (:lookout (:why walked)))
          (do (ctx/update-mem! c assoc :lookout-due true) :again)

          (and (= "stair" status) (:detour (:why walked)))
          (let [why (dissoc (:why walked) :detour)]
            (if-let [e (choose/detour-way c pos)]
              (esc/detour-escalate! c (:blocked (ctx/mem c) 0) why e)
              (end/give-up! c pos {:status :no-path :reason :detour} why)))

          (= "stair" status)
          (do (ctx/update-mem! c assoc :stair-tried true :escalate-now true) :again)

          (and (:leg-s (:args c)) (< left (dec d)))
          (end/finish! c {:arrived false :leg true})

          (and frontier (tour-spent? c))
          (end/give-up! c pos {:status :no-path :reason :explore-spent}
                        (update (tour c) :explored js/Math.round))

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

          surfaced? :again

          col-looked? :again

          (and (= "searching" status)
               (await (air/surface-while-searching! c (dissoc opts :progress) pos (:fresh walked)
                                                    {:surface-air (surface-air) :restarts (wet-restarts)})))
          :again

          (and (= "searching" status) (leg-search-spent? c))
          (end/give-up! c pos {:status :no-path :reason :leg-search-spent})

          (= "searching" status)
          (let [n (cond-> (:searching (ctx/mem c) 0) (:fresh walked) inc)]
            (ctx/update-mem! c assoc :searching n)
            (if (< n (max-searching))
              :again
              (end/give-up! c pos {:status :no-path :reason :moved-while-searching})))

          :else
          (let [handled (when (health/refused? c result) (await (health/heal-or-drop! c pos range)))]
           (or (when (= :again handled) :again)
          (let [result (cond-> result (:needs-health handled) (-> (assoc :reason :needs-health) (merge (select-keys handled [:heal :health :hp-cost :hp-budget]))))
                _ (note-fault! c walked)
                unstuck? (await (unstick/swim-out! c pos walked))
                progress? (or (< left (dec best)) explored? nearer?)
                tries (if progress? 0 (inc (:blocked (ctx/mem c) 0)))]
            (when-not progress? (ctx/book-try! c 'jobs.movement.go-to (or (:reason result) :no-progress)))
            (ctx/update-mem! c assoc :blocked tries :searching 0 :wet-fresh 0 :best (if (< left (dec best)) left best)
                             :closest (min left (:closest (ctx/mem c) left)) :last-dist left
                             :frontier-best (cond-> fbests frontier (assoc frontier (min fbest (u/dist to fcell))))
                             :target-best (cond-> tbests target (assoc target (min tdist (get tbests target js/Infinity)))))
            (when (and progress? (:restore-pending (ctx/mem c))) (restore/restore-next! c))
            (if (or unstuck?
                    (and (or progress? (and (< tries (max-blocked)) (< tries (ctx/own-tries c)) (pos? (ctx/tries-left c)))) (not (#{:goal-enclosed :goal-cut-off :goal-on-magma} (:reason result)))))
              (do (when (and (pos? tries) (not unstuck?)) (retrying! c result tries))
                  :again)
              (let [unloaded? (wsearch/goal-unloaded? (.-snapshot pw) (mapv #(js/Math.floor (% pos)) [:x :y :z]))
                    result (dive-end c pos (dark-end c result))
                    ended (cond-> (if (choose/escalate? c tries result) result (sight-end c pos (unloaded-end result unloaded?)))
                            (:waypoint-why (ctx/mem c)) (assoc :waypoint-why (:waypoint-why (ctx/mem c))))]
                (cond
                  (await (clear-goal-seal! c pos ended)) :again
                  (await (walk-to-wall! c pos tries ended opts)) :again
                  :else (await (esc/give-up-or-escalate! c pos tries status ended)))))))))))))
