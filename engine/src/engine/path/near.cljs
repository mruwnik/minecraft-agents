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

(def walk-timeout-s 60)

(defn iron-cells
  "The cells {:x :y :z} among the blocks the steps open that a hand cannot open: iron doors and iron trapdoors."
  [c steps]
  (vec (distinct (for [s steps o (:opens s)
                       :let [cell (select-keys o [:x :y :z])]
                       :when (= :iron (click/kind-of (some-> (pass/block-at c cell) .-name)))]
                   cell))))

(defn door-stuck [cells] {:status :no-path :reason :door-stuck :cells cells})

(defn plan!
  "Plan from where the body stands over a fresh pathWorld (the live one copies a section the first time it reads it, so an
  old one is stale), with walls read as stone; with doors other than :never, the iron doors the plan would open are walls
  too (planned again without them). A one-way step (a drop of 2 or 3, a gap jump down) is taken when the land past it runs
  on into unloaded land (walk/plan-walk :one-way :open): a far goal past a cliff is walked on to; a loaded pit is never entered."
  [c to range doors policy walls]
  (loop [walls walls]
    (let [plan (walk/plan-walk c (walk/path-world (:primitives c)) to range walk/default-weight
                               {:policy policy :walls walls :one-way :open})
          iron (when-not (= :never doors) (iron-cells c (:steps plan)))]
      (if (seq iron)
        (recur (into walls iron))
        plan))))

(defn ^:async walk-once!
  "Plan from where the body stands and follow the plan (walk/follow!: planned again in the same round when the look-ahead
  sees the way change, a partial plan is due a refresh, or a mob is in the way of a stuck body; each replan is a :replan
  info event {:why :ms :kept :replans :at}): the walk's result map (engine.path.walk), or the no-path result of a plan that
  is not walked. pw is the pathWorld the caller checked is there; every plan reads a fresh one. With doors other than
  :never, a plan may open blocks (iron ones are walls); one that will not open is a wall for one more plan, then the result
  is :no-path :door-stuck."
  [c _pw to range doors]
  (await (walk/settle! c))
  (let [policy (if (= :never doors) executor/policy executor/door-policy)
        announce! (fn [_kind data] (ctx/emit! c :replan :info data))
        walk-fn (fn [steps watch]
                  (if (= :never doors)
                    (walk/walk! c steps walk-timeout-s watch)
                    (pass/walk! c steps {:timeout-s walk-timeout-s :doors doors :watch watch})))]
    (when-not (= :never doors) (await (pass/shut-leftovers! c doors)))
    (loop [walls [] stuck nil]
      (let [plan (plan! c to range doors policy walls)]
        (if-let [no (walk/no-walk plan 0 policy)]
          (if stuck (door-stuck stuck) no)
          (let [{done :done last-plan :plan}
                (await (walk/follow! c plan {:plan-fn #(plan! c to range doors policy (into walls %)) :walk-fn walk-fn
                                             :to to :policy policy :announce! announce!}))]
            (cond
              (not= :door-stuck (:status done))
              (walk/partial-end done (:status last-plan) to range (:steps last-plan) (:stop last-plan))
              stuck (door-stuck (:cells done))
              :else (recur (into walls (:cells done)) (:cells done)))))))))

(defn move-status
  "The :moved entry's status of a walk that began d from the target and ended left from it."
  [arrived? d left]
  (cond
    arrived? "arrived"
    (< left (dec d)) "partial"
    :else "blocked"))

(defn ^:async walk-round!
  "One walk-once! toward the cell pos ({:x :y :z} of whole numbers) from where the body stands, over pw (the primitives'
  pathWorld), with its :moved entry {:from :to :status :target} written. {:result :status :from :to}: result is the walk's
  result map, status the entry's."
  [c pw pos range doors]
  (let [from (u/self-pos c)
        result (await (walk-once! c pw [(:x pos) (:y pos) (:z pos)] range doors))
        to (u/self-pos c)
        status (move-status (u/within? to pos range) (u/dist from pos) (u/dist to pos))]
    (ctx/remember! c :moved {:from from :to to :status status :target pos} stuck/moved-policy)
    {:result result :status status :from from :to to}))

(defn cell-of [pos] (into {} (map (fn [k] [k (js/Math.floor (k pos))])) [:x :y :z]))

(defn ^:async walk-near!
  "Walk until the body's cell is within range cells of pos's cell (u/within?), skipping the walk when it already is: one
  round of plan and walk (walk-round!, at most 60 s). Resolves to :there, :partial (ended more than 1 closer, call again)
  or :blocked (no path, a walk that got no nearer, or a body with no pathWorld sensing). opts {:doors}: the door policy of
  engine.path.pass, default :shut (open a shut door, gate or trapdoor on the way, pass, shut it again); a job that works
  gates itself passes :never (a shut one is a wall)."
  ([c pos range] (walk-near! c pos range nil))
  ([c pos range {:keys [doors] :or {doors :shut}}]
   (let [cell (cell-of pos)
         pw (walk/path-world (:primitives c))]
     (cond
       (u/within? (u/self-pos c) cell range) :there
       (nil? pw) :blocked
       :else (case (:status (await (walk-round! c pw cell range doors)))
               "arrived" :there
               "partial" :partial
               :blocked)))))
