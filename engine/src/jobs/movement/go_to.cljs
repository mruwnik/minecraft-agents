(ns jobs.movement.go-to
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.places :as places]
            [jobs.lib.world :as known]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.result :as end]))

(def doc
  "Walk to :pos ([x y z] or {:x :y :z}, fractions floored to the cell) until the body's cell is within :range cells of
  it (range 0: in that cell, 1: next to it). :place (a name such as :home, set by jobs.memory.set-place) walks to
  that place's recorded position instead of :pos.
  - Refused at once, before any walk: a :pos that is not one (:bad-pos), a :place that is not a valid name (:bad-name), a :tolls entry that is not {:x :y :z :factor} of finite numbers (:bad-tolls)
    or has no recorded position (:unknown-place), a body with no pathWorld sensing (:unsupported). Both give a :refused warn and {:status :stopped :arrived false :reason <it> :text}.
  - One call is one whole attempt: it plans and walks (jobs.lib.walk: slices of about 100 ms of search, walks of at
    most 60 s each) until it arrives or gives up. A search that needs more slices walks on toward where it has got
    to, or not at all while it goes on; every iteration awaits a pace-ms timer. A goal in unloaded land is walked
    toward walk by walk, and to the edge of loaded land when that is the only way on. It returns :continue only while
    an escalation or put-back child is waiting on the world.
  - :dark false plans dark cells like lit ones (default: a dark cell costs twice a lit one, so a lit route up to about 2x
    longer is taken: a cell seen and dark, or one never seen at night; look/dark-fn).
  - :dangers false plans straight past known dangers (default: the plans keep away from them, jobs.lib.threats). :leg-s n
    walks one leg of at most n s and, when it got more than 1 block nearer, ends {:arrived false :leg true} (job :done)
    for a caller chasing a moving target to call again; a leg that got no nearer goes on as any round.
  - A call starts afresh from the world: its counters are per call, and a saved escalation is dropped when the body
    is already there (a cut call's memory is a hint).
  - A walk that gets more than 1 block nearer is progress. Three walks in a row without progress give up, and so
    does a goal the planner proves walled in (:goal-enclosed, :goal-cut-off), at once.
  - A walk stuck or off its plan at a cell is a go-to.walker-fault warn; the next plans of the call cost that cell
    (up to 8) far more (wworld/with-avoid), so a way round is taken when there is one.
  - :escalate (default true): a body that is shut in (jobs.lib.reach/enclosed?), not in its own shelter, and whose
    search ran out of land (:exhausted, :goal-enclosed or :goal-cut-off) makes a way instead of giving up, at most 3 times per go-to, one child job
    each (jobs.lib.escape/choose): jobs.access.pillar up out of a pit when it carries enough blocks; else
    jobs.access.clear-path through a wall up to 3 thick toward the goal; else jobs.access.stair up out of a pit or
    toward a higher goal; else a walk to the nearest wall first. Each emits go-to.escalated {:step :why :n}. Once
    through, it puts back what clear-path or stair dug (jobs.blocks.place, the dug block or its drop, when
    carried): info go-to.restored, warn go-to.restore-skipped for cells it could not. Pillar blocks are left to
    jobs.access.cleanup. Its children's walks never escalate. Jobs that must not change the world on the way pass
    :escalate false.
  - Gives up with {:status :stopped :arrived false :reason :unreachable :why ... :text words} (the job ends :stopped) and an :unreachable warn: :why is the planner's
    reason (:exhausted, :goal-unloaded (the goal lies in unloaded land and the loaded land leads no nearer), :goal-enclosed, :goal-cut-off, :door-stuck with :cells, :one-way with :near and :one-way ...), or :stuck
    (:kind the step, :detail the executor's text), :off-plan, :steer-failed, :no-progress or :moved-while-searching; :at is the
    body's feet cell and :near its blocks from the goal. A failed
    escalation adds :escalation {:step :reason ...}, the child's reason or wait.
  - Success is {:arrived true}. The result is also a :result info event.
  - Every walk writes a :moved memory entry {:from :to :status :target} (arrived, partial or blocked) for
    the stuck trigger.
  - :doors (jobs.lib.pass): :shut (default) opens a shut door, gate or trapdoor with an empty hand, passes and
    shuts what it opened. :leave-open leaves it open. :never treats them as walls. A door in or beside another
    owner's zone is always shut again. A door that will not open is a wall for that walk (:door-stuck). Iron
    doors are walls.")

(def args
  {:pos {:doc "target position [x y z] or {:x :y :z}" :type :pos :default nil}
   :place {:doc "name of a place in body memory (:home, :bed, ...) to walk to instead of :pos" :default nil}
   :range {:doc "how close counts as there, in cells" :default 1}
   :doors {:doc "what to do at shut doors, gates and trapdoors: :shut (open, pass, shut again what the walk opened), :leave-open (open and pass), :never (walls)"
           :default :shut}
   :escalate {:doc "when shut in with no way out, pillar, stair or dig a door to get out (and put back what was dug); false: give up"
              :default true}
   :dangers {:doc "false: plan straight past known dangers (a walk up to the hostile being fought); true: keep away from them" :default true}
   :dark {:doc "false: plan dark cells like lit ones; true: a dark cell (seen dark, or unseen at night) costs twice a lit one" :default true}
   :tolls {:doc "cells to cross only as a last resort, [{:x :y :z :factor}]: each costs factor times its own seconds more (jobs.lib.cost farm-tolls, zone-tolls)" :default nil}
   :drop-cost {:doc "number: scales the cost of a drop (fall seconds and damage; 1 as is, 0 free, 5 dear); false: no drop of 2 or 3 at all. :one-way :closed instead refuses only a drop the body cannot climb back" :default 1}
   :zone-tolls {:doc "true: also toll the cells of other bodies' zones near each walk (jobs.lib.toll-cells/zone-walk-tolls), none with :ignore-zones?; for a job that respects zones" :default false}
   :leg-s {:doc "walk one leg of at most this many seconds (0.1 to 120), then end {:arrived false :leg true} so the caller can re-aim at a moving target; nil: the whole way" :type :number :min 0.1 :max 120 :default nil}
   :one-way {:doc "arg, not the :one-way key of a give-up result: :closed takes no drop of 2 or 3 or gap jump down that the body cannot climb back, and walks to no frontier of loaded land (a walk to something visible); :open (default) takes one when the land past it runs on into unloaded land" :default :open}
   :retry {:doc "false: a walk that got no nearer gives up at once instead of walking again (up to 3 times), for a caller that re-aims itself" :default true}
   :look-round {:doc "false: no look round on arrival, for a caller that keeps moving" :default true}
   :warn {:doc "false: a give-up or refusal is an info event, not a warn, for a caller that reports the failure itself"
          :default true}
   :ignore-zones? {:doc "act regardless of zones and claims in the escalation (pillar, stair, clear-path); the rules of the game allow it" :default false}})

(def max-blocked 3)

(def max-fault-cells "Cells a call keeps its plans away from (the walker's faults: fault-cells), the oldest dropped." 8)

(def max-searching
  "Search slices in a row whose search is still going on and began afresh (wsearch/round-budget expansions each) before
  go-to gives up: a search that goes on from the same cell always ends after the planner's maxNodes; only a body moved
  off its search's start every slice (pushed, drifting) starts afresh each time."
  100)

(def pace-ms "The timer each iteration of a call's loop awaits, so a search loop never starves the event loop." 50)

(defn pace!
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))

(defn check [_c] true)

(defn ^:async arrived!
  "Look round once if the place is risky (jobs.lib.watch), then finish: the next job starts facing along the walk."
  [c]
  (when-not (false? (:look-round (:args c)))
    (await (watch/watch! c {})))
  (end/finish! c {:arrived true}))

(defn forget-known-land!
  "Forget the land earlier searches knew to their end (wsearch/known-land) at the go-to's first walking round and after each
  escalation (:escalations, the world changed): a new go-to, or a dug way, may open a way through it."
  [c]
  (let [epoch (:escalations (ctx/mem c) 0)]
    (when-not (= epoch (:known-epoch (ctx/mem c)))
      (wsearch/forget-known! c)
      (ctx/update-mem! c assoc :known-epoch epoch))))

(defn known-frontier-result
  "A round's walk result as go-to judges it: a walk to a frontier in land earlier searches knew to their end
  (:frontier-known: there was no other edge) explores nothing, so it is the search's own answer, :exhausted, and never
  progress, unless it brings an edge seen earlier and now out of view nearer (:frontier-target)."
  [result]
  (if (:frontier-known result)
    (cond-> {:status :no-path :reason :exhausted :frontier-known true}
      (:frontier-target result) (assoc :frontier-target (:frontier-target result)))
    result))

(defn unloaded-end
  "A fruitless round's result as go-to gives it up when the goal cell reads unloaded: a search that ran out of loaded land
  (:exhausted) or a partial plan walked to its end (:off-plan :partial) says :goal-unloaded, the far side was never seen."
  [result unloaded?]
  (if (and unloaded? (or (= :exhausted (:reason result)) (and (= :off-plan (:status result)) (:partial result))))
    {:status :no-path :reason :goal-unloaded}
    result))

(defn foreign?
  "Whether cell [x y z] lies in, or next to, a zone of an owner other than self-name."
  [zones self-name [x y z]]
  (boolean (some (fn [{:keys [owner min max]}]
                   (and (not= owner self-name)
                        (every? true? (map (fn [v lo hi] (<= (dec lo) v (inc hi))) [x y z] min max))))
                 zones)))

(defn shut-foreign
  "The walker's :shut-also (jobs.lib.pass): a block opened in or next to another owner's zone is shut again whatever
  :doors says, so a pass through somebody's pen lets nothing out."
  [c]
  (fn [cell] (foreign? (known/zones c) (ctx/self-name c) cell)))

(defn here?
  "Whether the body is within range of pos, by its position or by the cell it stands on."
  [c pos range]
  (or (u/within? (u/self-pos c) pos range) (u/within? (reach/standing-cell (:primitives c)) pos range)))

(defn fault-cells
  "The cells a walk that failed at the walker (:stuck at :target, :off-plan at :at) says it cannot pass, [x y z] each.
  A partial plan walked to its end (:partial) is a normal end."
  [{:keys [status target at partial]}]
  (case status
    :stuck (some-> target vector)
    :off-plan (when-not partial (some-> at vector))
    nil))

(defn note-fault!
  "A walk that failed at the walker is a bug to see (a go-to.walker-fault warn) and a cell to keep the next plans off
  (mem :fault-cells, at most max-fault-cells; a new call starts with none): the walks go round it when there is a way."
  [c {:keys [status] :as result}]
  (when-let [cells (seq (fault-cells result))]
    (ctx/emit! c :walker-fault :warn (cond-> {:why status :target (first cells)}
                                       (:move result) (assoc :kind (:move result))
                                       (:why result) (assoc :detail (:why result))))
    (ctx/update-mem! c update :fault-cells #(vec (take-last max-fault-cells (distinct (into (vec %) cells)))))))

(defn ^:async walk! [c pos range doors]
  (let [from (u/self-pos c)
        d (u/dist from pos)
        pw (wworld/path-world (:primitives c))
        closed? (= :closed (some-> (:one-way (:args c)) keyword))]
    (cond
      (here? c pos range)
      (if (:restore-pending (ctx/mem c)) (esc/restore-next! c) (arrived! c))

      (nil? pw)
      (end/refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})

      :else
      (let [_ (forget-known-land! c)
            {walked :result status :status to :to} (await (near/walk-round! c pos range {:doors doors :explore (not closed?)
                                                                                          :dangers (not (false? (:dangers (:args c))))
                                                                                          :dark (not (false? (:dark (:args c))))
                                                                                          :timeout-s (or (:leg-s (:args c)) near/walk-timeout-s)
                                                                                          :one-way (when-not closed? :open)
                                                                                          :shut-also (shut-foreign c)
                                                                                          :budget wsearch/round-budget
                                                                                          :avoid (set (:fault-cells (ctx/mem c)))
                                                                                          :tolls (:tolls (:args c))
                                                                                          :zone-tolls (:zone-tolls (:args c))
                                                                                          :drop-cost (:drop-cost (:args c))
                                                                                          :progress (empty? (:frontier-best (ctx/mem c)))}))
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
            nearer? (boolean (and target (< tdist (dec (get tbests target js/Infinity)))))]
        (cond
          (= "arrived" status)
          (if (:restore-pending (ctx/mem c)) (esc/restore-next! c) (arrived! c))

          (and (:leg-s (:args c)) (< left (dec d)))
          (end/finish! c {:arrived false :leg true})

          (= "searching" status)
          (let [n (cond-> (:searching (ctx/mem c) 0) (:fresh walked) inc)]
            (ctx/update-mem! c assoc :searching n)
            (if (< n max-searching)
              :again
              (end/give-up! c pos (:blocked (ctx/mem c) 0) :searching {:status :no-path :reason :moved-while-searching})))

          :else
          (let [_ (note-fault! c walked)
                progress? (or (< left (dec best)) explored? nearer?)
                tries (if progress? 0 (inc (:blocked (ctx/mem c) 0)))]
            (ctx/update-mem! c assoc :blocked tries :searching 0 :best (if (< left (dec best)) left best)
                             :frontier-best (cond-> fbests frontier (assoc frontier (min fbest (u/dist to fcell))))
                             :target-best (cond-> tbests target (assoc target (min tdist (get tbests target js/Infinity)))))
            (when (and progress? (:restore-pending (ctx/mem c))) (esc/restore-next! c))
            (if (and (if (false? (:retry (:args c))) progress? (< tries max-blocked)) (not (#{:goal-enclosed :goal-cut-off} (:reason result))))
              :again
              (let [unloaded? (wsearch/goal-unloaded? (.-snapshot pw) (mapv #(js/Math.floor (% pos)) [:x :y :z]))
                    ended (if (esc/escalate? c result) result (unloaded-end result unloaded?))]
                (await (esc/give-up-or-escalate! c pos tries status ended))))))))))

(defn start-attempt!
  "Begin a call from the world, with memory as a hint: the counters of an earlier call (cut, or ended :continue) start
  again, and a saved escalation, or a pending one, is dropped when the body is already there (its holes, if any, are
  put back as after a made way)."
  [c pos]
  (let [m (ctx/mem c)
        there? (here? c pos (:range (:args c)))]
    (ctx/update-mem! c #(cond-> (-> % (dissoc :best :fault-cells) (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {}))
                          (and there? (:escalate-now m)) (dissoc :escalate-now)
                          (and there? (:escalation m))
                          (-> (dissoc :escalation :escalation-from :planned :holes-before)
                              (assoc :restore-pending true))))))

(defn ^:async step!
  "One iteration of a call: :again to go on in the same call, else what the call returns."
  [c pos]
  (let [m (ctx/mem c)]
    (cond
      (:restore-now m) (await (esc/restore-round! c))
      (:escalation m) (await (esc/escalation-round! c pos))
      (:escalate-now m) (do (ctx/update-mem! c dissoc :escalate-now)
                            (await (esc/escalate! c pos nil)))
      :else (await (walk! c pos (:range (:args c)) (some-> (:doors (:args c)) keyword))))))

(defn target
  "{:pos {:x :y :z}} to walk to: the recorded position of :place when given, else :pos; else a refusal."
  [c]
  (let [{:keys [place pos]} (:args c)]
    (if (nil? place)
      (places/parse-pos pos)
      (let [named (places/parse-name place)
            there (when-not (:reason named) (mem/place (ctx/view c) (:name named)))]
        (cond
          (:reason named) named
          (nil? there) (places/refusal :unknown-place (str "no place called " (name (:name named)) " is recorded in memory"))
          :else (places/parse-pos there))))))

(defn ^:async round
  "One whole attempt: step! until it arrives, gives up, or waits on a child (:continue), with pace! between steps. A
  cut ends it at once (ctx/alive? per iteration, else at the next memory write or act, which throws)."
  [c]
  (let [parsed (target c)
        pos (:pos parsed)
        tolls-problem (wworld/tolls-problem (:tolls (:args c)))]
    (cond
      (:reason parsed)
      (end/refuse! c parsed)

      tolls-problem
      (end/refuse! c {:reason :bad-tolls :message tolls-problem})

      :else
      (do (start-attempt! c pos)
          (loop []
            (let [r (await (step! c pos))]
              (cond
                (not= :again r) r
                (ctx/alive? c) (do (await (pace!)) (recur))
                :else :continue)))))))
