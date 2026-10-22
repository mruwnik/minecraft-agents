(ns jobs.movement.go-to.cut
  "A go-to call's record across restarts: the walk so far (approaches), the cut count that gives up :cut-again, the start
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
            [jobs.movement.go-to.tuning :refer [max-cut-restarts cut-progress send-back-margin]]))

(def approaches
  "This run's record of each open go-to's walk, by job id: {:last-dist :closest :explored ...}, the body's distance to the
  goal now, its least, and the blocks walked since it last came nearer (approach-step). In process, not in job memory: a
  walk updates it every tick and a memory write is an event."
  (atom {}))

(defn approach-step
  "Approach record m after a tick's pose {:x :z ...} at d from the goal: :last-dist d, :closest the least, and :explored
  the blocks walked along x and z away from the goal (a step that ends farther than the last) since the body last came
  more than 1 block nearer than ever (:anchor, then 0 again)."
  [m pose d]
  (let [{:keys [prev anchor explored] :or {anchor d explored 0}} m
        nearer? (< d (dec anchor))
        step (if (and prev (> d (:last-dist m d))) (js/Math.hypot (- (:x pose) (:x prev)) (- (:z pose) (:z prev))) 0)]
    (assoc m :last-dist d :closest (min d (:closest m d)) :prev (select-keys pose [:x :z])
             :anchor (if nearer? d anchor) :explored (if nearer? 0 (+ explored step)))))

(defn approach-recorder
  "The walk's :on-pose for a call to pos: while the round is alive, keeps approaches' record (approach-step), so a cut walk
  leaves where the cut found it and the tour's price can be checked."
  [c pos]
  (fn [pose]
    (when (ctx/alive? c)
      (swap! approaches update (:id c) approach-step pose (u/dist pose pos)))))

(defn tour
  "The call's tour of frontiers so far, {:explored :walks}: the blocks walked away from the goal and the frontier walks
  taken since the body last came nearer it than ever."
  [c]
  {:explored (:explored (get @approaches (:id c)) 0) :walks (:tour-walks (ctx/mem c) 0)})

(defn tour-spent?
  "Whether the tour so far (tour) costs more than its goal is worth: cost/tour-worth of the goal's
  straight blocks at the call's start and the caller's :worth (the setting ::tour-worth of jobs.lib.cost.value, when set); the settings there price the tour."
  [c]
  (> (:cost (cost/tour-cost (tour c))) (cost/tour-worth (:detour-from (ctx/mem c) 0) (:worth (:args c)))))

(defn ^:async start-attempt!
  "Begin a call from the world, with memory as a hint: the counters of an earlier call (cut, or ended :continue) start
  again (but for the cut count, kept by note-restart!), and a saved escalation, or a pending one, is dropped when the body is already there (its holes, if any, are
  put back as after a made way). The open pen gate warning is checked at the call's first attempt, and again only
  when a walk has opened a gate since (an :opened entry the last check did not know: a cut between open and shut)."
  [c pos]
  (let [m (ctx/mem c)
        there? (here? c pos (:range (:args c)))
        opened (set (keep (comp :cell :data) (ctx/entries c :opened)))]
    (when (or (nil? (:pen-checked m)) (some #(not (contains? (:pen-checked m) %)) opened))
      (await (pen-entry/warn-left-open! c pos (:doors (:args c)))))
    (ctx/update-mem! c #(cond-> (-> % (dissoc :best :fault-cells :over-budget :off-hand-tried :air-looked :air-relooks :surfacings :column-looks :column-near :hang-walks :waypoint-misses :waypoint-why :waypoints-tried :way-looks :dry-tried :in-water :wet-fresh :wet-surfacings :dive-floor :cave-ok :cave-climb :own-way-on :own-way-done :own-way-top :detour-from :tour-walks :edge-mode) (assoc :pen-checked opened :blocked 0 :searching 0 :leg-hp (.-health (.self (:primitives c))) :frontier-best {} :target-best {}))
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
  "The cut bookkeeping after a call restarts at dist from goal, from m, the call's memory: {:goal :closest :ref :cuts
  :last-dist}. A restart is a cut only when the earlier call was left open in this session, for the same goal. :cuts
  starts again at 0 when :closest (the least distance a walk reached) has improved cut-progress over :ref (its value then);
  a cut adds one when it begins more than send-back-margin farther than :last-dist, where the cut found the body (a reflex
  sent it back; a meal, a breath, a fight in place or knockback do not, on a detour neither)."
  [m goal dist]
  (let [cut? (and (= (session-id) (:open m)) (= goal (:goal m)))
        closest (min dist (if cut? (:closest m dist) dist))
        ref (if cut? (:ref m closest) closest)
        improved? (>= (- ref closest) (cut-progress))
        sent-back? (and cut? (> dist (+ (:last-dist m dist) (send-back-margin))))]
    {:goal goal :closest closest :ref (if improved? closest ref) :last-dist dist
     :cuts (+ (if (and cut? (not improved?)) (:cuts m 0) 0) (if sent-back? 1 0))
     :resets (if cut? (:resets m 0) 0)}))

(defn cut-loop? [{:keys [cuts]}] (>= cuts (max-cut-restarts)))

(defn note-restart!
  "Record this call's start in memory: the call is open (this session) until it returns, an open one found at the start
  was cut. The cut state."
  [c pos]
  (let [walked (get @approaches (:id c))
        state (cut-state (merge (ctx/mem c) walked) pos (u/dist (u/self-pos c) pos))]
    (swap! approaches dissoc (:id c))
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
