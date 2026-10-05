(ns engine.path.near
  "One round of walking toward a cell, as jobs.movement.go-to and the jobs' walk-near! do it: plan from where the body stands
  within the executor's abilities (engine.path.walk), follow the plan once, opening shut doors, gates and trapdoors by the
  :doors policy (engine.path.pass), and write the :moved memory entry the stuck trigger and jobs.maintenance.unstick read.
  Its own namespace because engine.path.walk and engine.path.pass require engine.jobs.util."
  (:require [engine.access.click :as click]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.path.executor :as executor]
            [engine.path.pass :as pass]
            [engine.path.walk :as walk]
            [engine.triggers.stuck :as stuck]))

(def walk-timeout-s
  "The default bound of one steer of a walk round. A caller that chases something that moves passes a shorter one, so it
  aims again at where the thing is now."
  60)

(defn iron-cells
  "The cells {:x :y :z} among the blocks the steps open that a hand cannot open: iron doors and iron trapdoors."
  [c steps]
  (vec (distinct (for [s steps o (:opens s)
                       :let [cell (select-keys o [:x :y :z])]
                       :when (= :iron (click/kind-of (some-> (pass/block-at c cell) .-name)))]
                   cell))))

(defn door-stuck [cells] {:status :no-path :reason :door-stuck :cells cells})

(defn ^:async plan!
  "Plan from where the body stands over a fresh pathWorld (the live one copies a section the first time it reads it, so an
  old one is stale), with walls read as stone; with doors other than :never, the iron doors the plan would open are walls
  too (planned again without them). A one-way step (a drop of 2 or 3, a gap jump down) is taken when the land past it runs
  on into unloaded land (walk/plan-walk :one-way :open) when one-way is :open: a far goal past a cliff is walked on to; a
  loaded pit is never entered. With one-way nil a partial plan ends at the nearest node the body can come back from. With
  explore, a search that ran out of loaded land walks to its frontier (walk/plan-walk :frontier). With budget, at most
  that many expansions of search (walk/plan-walk! :budget): the plan may be status \"searching\" (walk nowhere, the
  search goes on at the next call) or a walk to where an unfinished search has got to (never, with progress false).
  With budget, each plan's search is an info :planned event {:ms :status :why :nodes} (nodes only once the search is over).
  A promise: the searches run in slices with yields between them (walk/plan-walk!), so the body's API answers meanwhile."
  [c to range doors policy walls explore one-way budget progress]
  (loop [walls walls]
    (let [plan (await (walk/plan-walk! c (walk/path-world (:primitives c)) to range walk/default-weight
                                       {:policy policy :walls walls :one-way one-way :frontier explore :budget budget
                                        :progress progress}))
          iron (when-not (= :never doors) (iron-cells c (:steps plan)))]
      (when budget
        (let [^js r (:r plan)]
          (ctx/emit! c :planned :info (cond-> {:ms (js/Math.round (or (:ms plan) 0)) :status (:status plan)}
                                        (some-> r .-reason) (assoc :why (.-reason r))
                                        (some-> r .-expanded) (assoc :nodes (.-expanded r))))))
      (if (seq iron)
        (recur (into walls iron))
        plan))))

(defn ^:async walk-once!
  "Plan from where the body stands and follow the plan (walk/follow!: planned again in the same round when the look-ahead
  sees the way change, a partial plan is due a refresh, or a mob is in the way of a stuck body; each replan is a :replan
  info event {:why :ms :kept :replans :at}): the walk's result map (engine.path.walk), or the no-path result of a plan that
  is not walked. Every plan reads a fresh pathWorld; the caller has checked there is one. Each steer is bounded by timeout-s.
  With doors other than :never, a plan may open blocks (iron ones are walls); one that will not open is a wall for one more
  plan, then the result is :no-path :door-stuck. Before and after the walk, the blocks this job opened and left (a cut
  round, a walk that ended in a doorway) are shut when within reach (pass/shut-leftovers!); farther ones, and those a cut
  leaves, are the door-left trigger's (engine.triggers.door-left). With explore (go-to), a search that ran out of loaded land
  walks to its frontier, and the result carries :frontier, that node's cell [x y z], when the last plan walked was one,
  and :frontier-known true when that node lay in land earlier searches knew to their end (walk/known-land).
  With budget (go-to), each plan searches at most that many expansions (plan!): a plan still searching is the result
  {:status :searching} (walk/no-walk), nothing walked; progress false: an unfinished search walks nowhere (plan!)."
  [c to range doors timeout-s explore one-way budget progress]
  (await (walk/settle! c))
  (let [policy (if (= :never doors) executor/policy executor/door-policy)
        announce! (fn [_kind data] (ctx/emit! c :replan :info data))
        walk-fn (fn [steps watch]
                  (if (= :never doors)
                    (walk/walk! c steps timeout-s watch)
                    (pass/walk! c steps {:timeout-s timeout-s :doors doors :watch watch})))]
    (when-not (= :never doors) (await (pass/shut-leftovers! c doors)))
    (let [result (loop [walls [] stuck nil]
                   (let [plan (await (plan! c to range doors policy walls explore one-way budget progress))]
                     (if-let [no (walk/no-walk plan 0 policy)]
                       (if stuck (door-stuck stuck) no)
                       (let [{done :done last-plan :plan}
                             (await (walk/follow! c plan {:plan-fn #(plan! c to range doors policy (into walls %) explore one-way budget progress)
                                                          :walk-fn walk-fn :to to :policy policy :announce! announce!}))]
                         (cond
                           (not= :door-stuck (:status done))
                           (cond-> (walk/partial-end done (:status last-plan) to range (:steps last-plan) (:stop last-plan))
                             (:frontier-taken last-plan) (assoc :frontier (:at (:frontier-taken last-plan)))
                             (:known (:frontier-taken last-plan)) (assoc :frontier-known true))
                           stuck (door-stuck (:cells done))
                           :else (recur (into walls (:cells done)) (:cells done)))))))]
      ;; however the walk ended, what it opened and could not shut yet (the body was in its column) is shut when in reach
      (when-not (= :never doors) (await (pass/shut-leftovers! c doors)))
      result)))

(defn move-status
  "The :moved entry's status of a walk that began d from the target and ended left from it."
  [arrived? d left]
  (cond
    arrived? "arrived"
    (< left (dec d)) "partial"
    :else "blocked"))

(defn ^:async walk-round!
  "One walk-once! toward the cell pos ({:x :y :z} of whole numbers) from where the body stands (the caller has checked the
  primitives have a pathWorld), with its :moved entry {:from :to :status :target} written and the round booked for the
  backoff as one walk (ctx/note-walk!: the status and the blocks the body moved). opts {:doors :timeout-s :explore :one-way :budget
  :progress}: doors, explore, one-way (default :open: past a drop toward a far goal), budget and progress (default true) as walk-once!, timeout-s the bound of each steer (default walk-timeout-s). {:result :status :from :to}: result
  is the walk's result map, status the entry's (\"partial\" too for a walk to a frontier that moved the body over a block:
  it went where a way may be, not stuck). A round whose plan is still searching (result :searching) walked nowhere and
  failed at nothing: status \"searching\", no :moved entry and no walk booked."
  [c pos range {:keys [doors timeout-s explore one-way budget progress] :or {timeout-s walk-timeout-s one-way :open progress true}}]
  (let [from (u/self-pos c)
        result (await (walk-once! c [(:x pos) (:y pos) (:z pos)] range doors timeout-s explore one-way budget progress))
        to (u/self-pos c)
        status (cond
                 (= :searching (:status result)) "searching"
                 (and (:frontier result) (> (u/dist from to) 1)) "partial"
                 :else (move-status (u/within? to pos range) (u/dist from pos) (u/dist to pos)))]
    (when-not (= "searching" status)
      (ctx/remember! c :moved (cond-> {:from from :to to :status status :target pos}
                                (= :no-path (:status result)) (assoc :no-path true))
                     stuck/moved-policy)
      (ctx/note-walk! c status (u/dist from to)))
    {:result result :status status :from from :to to}))

(defn cell-of [pos] (into {} (map (fn [k] [k (js/Math.floor (k pos))])) [:x :y :z]))

(defn ^:async walk-near!
  "Walk until the body's cell is within range cells of pos's cell (u/within?). Does nothing when it already is.
  One round of plan and walk (walk-round!, at most 60 s). Resolves to:
  - :there
  - :partial: ended more than 1 closer, call again
  - :blocked: no path, a walk that got no nearer, or a body with no pathWorld sensing (booked as a failed walk)
  opts:
  - :doors, the door policy of engine.path.pass. Default :shut (open a shut door, gate or trapdoor on the way, pass,
    shut it again). A job that works gates itself passes :never (a shut one is a wall).
  - :timeout-s bounds each steer (default walk-timeout-s). A job chasing a mob or a villager passes a short one, so it
    aims again at where the target is.
  A partial plan never takes a step the body cannot undo (a drop of 2 or 3, a gap jump down; walk-round! :one-way nil).
  The target is something the body can see, so a missing way is not past a cliff, and an unreachable target (a cow on
  an island) must not lead the body off a ledge."
  ([c pos range] (walk-near! c pos range nil))
  ([c pos range {:keys [doors timeout-s] :or {doors :shut timeout-s walk-timeout-s}}]
   (let [cell (cell-of pos)]
     (cond
       (u/within? (u/self-pos c) cell range) :there
       (nil? (walk/path-world (:primitives c))) (do (ctx/note-walk! c "blocked" 0) :blocked)
       :else (case (:status (await (walk-round! c cell range {:doors doors :timeout-s timeout-s :one-way nil})))
               "arrived" :there
               "partial" :partial
               :blocked)))))
