(ns jobs.movement.go-to.cut
  "A go-to call's record across restarts: the walk so far (approaches), the send-backs priced against the goal's worth, which end a call :cut-again, the start
  of a call from the world, and the hostile a cut is guessed to come from."
  (:require [engine.core.base :as base]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.pen-entry :as pen-entry]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.aim :refer [here?]]
            [jobs.movement.go-to.tuning :refer [cut-progress send-back-margin]]))

(def approaches
  "This run's record of each open go-to's walk, by job id: {:call :last-dist :closest :explored ...}, the attempt that owns
  it (with-call), the body's distance to the goal now, its least, and the blocks walked since it last came nearer
  (approach-step). In process, not in job memory: a walk updates it every tick and a memory write is an event."
  (atom {}))

(defn with-call
  "c as one attempt of its call: the walk record it starts (note-restart!) is its own, so a walk of another attempt under the
  same job id still running (another engine in this process, a round left running) neither writes into it nor drops it."
  [c]
  (assoc c ::call #js {}))

(defn record
  "The attempt's own walk record, nil when another attempt holds the job id's."
  [c]
  (let [r (get @approaches (:id c))]
    (when (identical? (::call c) (:call r)) r)))

(defn drop-record!
  "Drop the attempt's own walk record (another attempt's stays)."
  [c]
  (swap! approaches (fn [m] (cond-> m (identical? (::call c) (:call (get m (:id c)))) (dissoc (:id c))))))

(defn fresh-record!
  "Start the attempt's walk record again, empty."
  [c]
  (swap! approaches assoc (:id c) {:call (::call c)}))

(def jump-blocks
  "Blocks between two ticks' poses that no walk, fall or knockback covers: a teleport. A speed bound, not a radius: on-pose
  runs once per physics tick, and a body falls at most about 3.9 blocks a tick (terminal velocity), sprints under 0.4."
  8)

(def falling-vy
  "Vertical velocity (blocks per tick) below which a long drop between two samples is a fall, not a teleport: a lagged sample of a fall can span more than jump-blocks."
  -0.5)

(defn cell-of [p] (mapv #(js/Math.floor (% p)) [:x :y :z]))

(defn approach-step
  "Approach record m after a tick's pose {:x :y :z ...} at d from the goal: :last-dist d, :closest the least, and :explored
  the blocks walked along x and z away from the goal (a step that ends farther than the last) since the body last came
  more than 1 block nearer than ever (:anchor, then 0 again), and :trail the cells the poses stood in, each once in a row.
  :jumped once a tick's pose lies jump-blocks or more from the last."
  [m pose d]
  (let [{:keys [prev anchor explored trail] :or {anchor d explored 0 trail []}} m
        cell (cell-of pose)
        nearer? (< d (dec anchor))
        step (if (and prev (> d (:last-dist m d))) (js/Math.hypot (- (:x pose) (:x prev)) (- (:z pose) (:z prev))) 0)
        dy (- (:y pose 0) (:y prev 0))
        falling? (and (neg? dy) (< (:vy pose 0) falling-vy)
                      (< (js/Math.hypot (- (:x pose) (:x prev)) (- (:z pose) (:z prev))) jump-blocks))
        jumped? (and prev (not falling?) (>= (js/Math.hypot (- (:x pose) (:x prev)) dy (- (:z pose) (:z prev))) jump-blocks))]
    (assoc m :trail (cond-> trail (not= cell (peek trail)) (conj cell)) :last-dist d :closest (min d (:closest m d)) :prev (select-keys pose [:x :y :z])
           :jumped (boolean (or jumped? (:jumped m)))
           :jump-from (if jumped? (assoc (select-keys prev [:x :y :z]) :blocks (u/dist prev pose)) (:jump-from m))
             :anchor (if nearer? d anchor) :explored (if nearer? 0 (+ explored step)))))

(defn approach-recorder
  "The walk's :on-pose for a call to pos: while the round is alive, keeps approaches' record (approach-step), so a cut walk
  leaves where the cut found it and the tour's price can be checked. Once the record says :jumped it returns the done map
  {:status :jumped}, which ends the walk at once (jobs.lib.walk/walk-lit!) instead of walking the old plan on."
  [c pos]
  (fn [pose]
    (when (and (ctx/alive? c) (record c))
      (when (:jumped (get (swap! approaches update (:id c) approach-step pose (u/dist pose pos)) (:id c)))
        {:status :jumped}))))

(defn jumped?
  "Whether the body was moved far in one tick (a teleport, a pearl) since the call's record began."
  [c]
  (boolean (:jumped (record c))))

(defn send-back
  "A send-back of a walk left at cell `at` that undid `blocks` of walk: {:at :lost-s the seconds of it :cells} the last
  cells of trail, one for each block undone, the way that was cut."
  [at blocks trail]
  {:at at :lost-s (* blocks (cost/walk-step-s)) :cells (vec (take-last (max 1 (long (js/Math.ceil blocks))) trail))})

(defn restart-after-jump!
  "A call whose body was teleported mid-walk starts again from where it stands: the walk record and the walk's counters go.
  A jump from within send-back-margin of the last one's start (the same place: a way that keeps getting the body moved)
  adds a send-back to the call's, losing the blocks it moved the body (:jump-from's :blocks); a jump elsewhere clears them (the evidence was
  about another place). The closest approach stays, and the distance to pos is where the next cut is measured from."
  [c pos]
  (let [at (select-keys (u/self-pos c) [:x :y :z])
        from (:jump-from (record c))
        prev (:jump-at (ctx/mem c))
        repeat? (and prev (<= (u/dist at prev) (send-back-margin)))
        spot (when (and repeat? from) (send-back (cell-of from) (:blocks from) (:trail (record c))))]
    (fresh-record! c)
    (ctx/update-mem! c #(-> %
                            (dissoc :best :starts :tour-walks :edge-mode :waypoint-misses :waypoint-why :waypoints-tried :lookout-walks :lookouts :wall-walked)
                            (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {} :jump-at at :last-dist (u/dist at pos)
                                   :cut-walks (if repeat? (inc (:cut-walks % 0)) 1)
                                   :sent-backs (cond-> (if repeat? (:sent-backs % []) []) spot (conj spot)))))
    (ctx/emit! c :go-to.jumped :info {:text "the body was moved far in one tick: the walk starts again from here"})))

(defn send-back-chance
  "The chance a walk of the call is sent back, counted: m the call's memory, its sends k of its cut-walks n (cost/counted-chance)."
  [{:keys [sent-backs cut-walks]}]
  (cost/counted-chance {:k (count sent-backs) :n (or cut-walks 0)}))

(defn send-back-price
  "Seconds the way's repeat is expected to cost more: its chance of a send-back times the walk each one lost."
  [{:keys [sent-backs] :as m}]
  (* (send-back-chance m) (reduce + 0 (map :lost-s sent-backs))))

(defn send-backs-spent?
  "Whether the send-backs of m, the call's memory or cut state, cost more (send-back-price) than the goal is worth: the
  call's :worth, else a tour's (cost/tour-worth)."
  [c m]
  (> (send-back-price m) (cost/tour-worth (or (:detour-from (ctx/mem c)) (:last-dist m 0)) (:worth (:args c)))))

(defn beside
  "The cells of the 3 x 3 round cell [x y z] at its height: the way's width, so a step to its side stays on it."
  [[x y z]]
  (for [dx [-1 0 1] dz [-1 0 1]] [(+ x dx) y (+ z dz)]))

(defn send-back-tolls
  "The :tolls of the call's walks, m its memory: the way each send-back cut (its cells and those beside them) costs the
  send-back price more, shared out over the cells of the way, so a fresh way is tried before the same one again."
  [m]
  (let [chance (send-back-chance m)
        step (cost/walk-step-s)]
    (->> (:sent-backs m)
         (filter :at)
         (mapcat (fn [{:keys [at cells lost-s]}]
                   (let [way (distinct (cons at cells))
                         each (/ (* chance lost-s) (count way) step)]
                     (map (fn [cell] [cell each]) (distinct (mapcat beside way))))))
         (reduce (fn [m [cell f]] (update m cell (fnil + 0) f)) {})
         (map (fn [[[x y z] factor]] {:x x :y y :z z :factor factor})))))

(defn tour
  "The call's tour of frontiers so far, {:explored :walks}: the blocks walked away from the goal and the frontier walks
  taken since the body last came nearer it than ever."
  [c]
  {:explored (:explored (record c) 0) :walks (:tour-walks (ctx/mem c) 0)})

(defn tour-spent?
  "Whether the tour so far (tour) costs more than its goal is worth: cost/tour-worth of the goal's
  straight blocks at the call's start and the caller's :worth (the setting ::tour-worth of jobs.lib.cost.value, when set); the settings there price the tour."
  [c]
  (> (:cost (cost/tour-cost (tour c))) (cost/tour-worth (:detour-from (ctx/mem c) 0) (:worth (:args c)))))

(defn ^:async start-attempt!
  "Begin a call from the world, with memory as a hint: the counters of an earlier call (cut, or ended :continue) start
  again (but for the cut count, kept by note-restart!, and :detour-from, what the goal is worth a tour, set at the call's
  first walk: an attempt that starts near the goal after a wait still owes the way round it found), and a saved escalation, or a pending one, is dropped when the body is already there (its holes, if any, are
  put back as after a made way). The open pen gate warning is checked at the call's first attempt, and again only
  when a walk has opened a gate since (an :opened entry the last check did not know: a cut between open and shut)."
  [c pos]
  (let [m (ctx/mem c)
        there? (here? c pos (:range (:args c)))
        opened (set (keep (comp :cell :data) (ctx/entries c :opened)))]
    (when (or (nil? (:pen-checked m)) (some #(not (contains? (:pen-checked m) %)) opened))
      (await (pen-entry/warn-left-open! c pos (:doors (:args c)))))
    (ctx/update-mem! c #(cond-> (-> % (dissoc :best :fault-cells :over-budget :off-hand-tried :air-looked :air-relooks :surfacings :column-looks :column-near :hang-walks :waypoint-misses :waypoint-why :waypoints-tried :way-looks :dry-tried :in-water :wet-fresh :wet-surfacings :dive-floor :cave-ok :cave-climb :own-way-on :own-way-done :own-way-top :tour-walks :edge-mode) (assoc :pen-checked opened :blocked 0 :searching 0 :leg-hp (.-health (.self (:primitives c))) :frontier-best {} :target-best {}))
                          (not (:torch-fetching m)) (dissoc :torch-fetched)
                          (and there? (:escalate-now m)) (dissoc :escalate-now)
                          (and there? (:escalation m))
                          (-> (dissoc :escalation :escalation-from :planned :holes-before)
                              (assoc :restore-pending true))))))

(defn fresh-session-id [] (str (js/Date.now) "-" (rand-int 1000000000)))

(defonce session (atom (fresh-session-id)))

(defn session-id
  "Names this run of the engine: a call left open by an earlier run was not cut, so it counts for nothing."
  []
  @session)

(defn new-session!
  "A warm process starting its engine again: the job ids restart, so the earlier run's walk records and open calls must not match."
  []
  (reset! approaches {})
  (reset! session (fresh-session-id)))

(base/on-warm-start! ::session new-session!)

(defn cut-state
  "The cut bookkeeping after a call restarts at dist from goal, from m, the call's memory: {:goal :closest :ref :last-dist
  :cut-walks :sent-backs}. A restart is a cut only when the earlier call was left open in this session, for the same goal.
  The send-backs and the walks cut (:cut-walks) start again when :closest (the least distance a walk reached) has improved
  cut-progress over :ref (its value then); a cut that begins more than send-back-margin farther than :last-dist, where the
  cut found the body, is a send-back (a reflex sent it back; a meal, a breath, a fight in place or knockback do not, on a
  detour neither): a send-back of the cell the walk was left at and the walk undone, from m's :trail."
  [m goal dist]
  (let [cut? (and (= (session-id) (:open m)) (= goal (:goal m)))
        closest (min dist (if cut? (:closest m dist) dist))
        ref (if cut? (:ref m closest) closest)
        improved? (>= (- ref closest) (cut-progress))
        sent-back? (and cut? (> dist (+ (:last-dist m dist) (send-back-margin))))
        keep? (and cut? (not improved?))]
    {:goal goal :closest closest :ref (if improved? closest ref) :last-dist dist
     :cut-walks (cond keep? (inc (:cut-walks m 0)) cut? 1 :else 0)
     :sent-backs (cond-> (if keep? (:sent-backs m []) [])
                   sent-back? (conj (send-back (some-> (:prev m) cell-of) (- dist (:last-dist m dist)) (:trail m))))}))

(defn note-restart!
  "Record this call's start in memory: the call is open (this session) until it returns, an open one found at the start
  was cut. The cut state."
  [c pos]
  (let [walked (dissoc (get @approaches (:id c)) :call)
        state (cut-state (merge (ctx/mem c) walked) pos (u/dist (u/self-pos c) pos))]
    (fresh-record! c)
    (ctx/update-mem! c #(-> % (assoc :open (session-id)) (merge state)))
    state))

(defn way-hostiles
  "The hostiles the body knows of (seen, heard or remembered) within the hostile-near trigger's radius (ranged ones its
  ranged radius) of the way still ahead to pos, nearest the way first: what may send the body back again."
  [c pos]
  (danger-q/hostiles-near-way (:primitives c) pos (danger-q/hostile-radius) {:ranged-radius (danger-q/ranged-radius)}))

(defn nearest-hostile
  "{:nearest-hostile name :nearest-hostile-at place} of the hostile nearest the way ahead to pos, else of the nearest
  the body knows within 16 blocks, a guess at the sender back (the engine does not say which reflex cut), else nil.
  The place is its cell when seen, {:direction :band} when only heard."
  [c pos]
  (when-let [e (or (first (way-hostiles c pos)) (first (combat/hostiles (:primitives c) 16)))]
    (let [{at :pos :as place} (threats/place-of (:primitives c) e)]
      {:nearest-hostile (.-name e)
       :nearest-hostile-at (if at (mapv #(js/Math.floor (% at)) [:x :y :z]) (select-keys place [:direction :band]))})))
