(ns jobs.lib.walk
  "The walk driver: plan from the body's cell to a goal within the executor's abilities, follow the plan with steer, plan
  again when the body ends off it. Jobs that walk (jobs.debug.walk-plan, the stair, tunnel and cleanup jobs) call this
  namespace; walk-to! is the whole loop, the other functions are its pieces; the pieces live in jobs.lib.walk.plan (planning), .search (the budgeted search a round), .watch (the look-ahead) and .world (the pathWorld and its decorations)."
  (:require [engine.ctx :as ctx]
            [engine.path.executor :as executor]
            [jobs.lib.util :as u]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.watch :as wwatch]
            [jobs.lib.walk.world :as wworld]))

(def max-timeout-s 120)
(def max-settle-waits 20)
(def default-weight 1.2)
(def held-in
  "Blocks at the feet that hold a body still enough to plan from: climbables, and water (it swims from there)."
  #{"ladder" "vine" "water"})

(defn grounded?
  "Whether the body stands on the ground, or its feet cell (felt) is a climbable or water."
  [p]
  (let [s (.self p)
        pos (.-pos s)
        b (u/feel p {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))})]
    (or (.-onGround s) (contains? held-in (some-> b .-name)))))

(defn ^:async settle!
  "Wait (100 ms at a time, at most 20 waits) until the body stands on the ground, in a climbable or in water."
  [c]
  (let [p (:primitives c)]
    (loop [n 0]
      (when (and (< n max-settle-waits) (not (grounded? p)))
        (await (ctx/act c :wait #js {:ms 100}))
        (recur (inc n))))))

(defn path-length
  "Horizontal length of the steps' stand points, in blocks, to 1 decimal."
  [steps]
  (let [d (reduce + (map (fn [a b] (js/Math.hypot (- (:px b) (:px a)) (- (:pz b) (:pz a))))
                         steps (rest steps)))]
    (/ (js/Math.round (* 10 d)) 10)))

(defn pose-of [pose]
  {:x (.-x pose) :y (.-y pose) :z (.-z pose) :vx (.-vx pose) :vy (.-vy pose) :vz (.-vz pose) :on-ground (.-onGround pose)
   :on-climbable (.-onClimbable pose) :in-water (.-inWater pose) :in-lava (.-inLava pose) :collided (.-collided pose)})

(defn steer-args
  "The act args of one walk. decide is not enumerable: the engine's act wrapper writes (js->clj args) into
  an event, and a function there would break the event stream."
  [timeout-s decide]
  (doto (js-obj "timeoutS" (min max-timeout-s timeout-s))
    (js/Object.defineProperty "decide" #js {:value decide})))

(def centre-tolerance "How near (blocks, each axis) a centring nudge ends to its target." 0.2)
(def centre-max-ticks "Ticks a centring nudge walks before it gives up." 30)

(defn centre-decider
  "A steer decide function that walks straight at [tx tz], done within centre-tolerance of it or after centre-max-ticks."
  [tx tz]
  (let [ticks (volatile! 0)]
    (fn [js-pose]
      (let [dx (- tx (.-x js-pose)) dz (- tz (.-z js-pose))]
        (if (or (and (<= (js/Math.abs dx) centre-tolerance) (<= (js/Math.abs dz) centre-tolerance))
                (>= (vswap! ticks inc) centre-max-ticks))
          #js {:done true}
          #js {:controls #js {:forward true} :yaw (js/Math.atan2 (- dx) (- dz))})))))

(defn ^:async centre!
  "A short walk (no planning, no jump) to within centre-tolerance of [tx tz] on the cell the body is in: the step a
  go-to to a cell cannot make, since a body on the edge of the cell already counts as in it."
  [c tx tz]
  (await (ctx/act c :steer (steer-args 3 (centre-decider tx tz)))))

(defn ^:async plan-walk!
  "Plan a walk with plan-within! (yields to the event loop between search slices): what the walks (jobs.lib.near, walk-to!)
  plan with, so a long search never holds the body's API. With :budget (go-to: round-budget), one call searches at most
  that many expansions (plan-walk-budgeted!): a search that needs more walks to where it has got to, or nowhere
  (\"searching\"), and goes on at the next call; with :progress false only nowhere until the search ends."
  ([c pw to range weight] (plan-walk! c pw to range weight nil))
  ([c pw to range weight {:keys [policy walls one-way frontier budget] :or {policy (wworld/body-policy c)} :as opts}]
   (if budget
     (await (wsearch/plan-walk-budgeted! c pw to range weight (assoc opts :policy policy)))
     (let [walled (wworld/with-walls pw walls)]
       (wplan/walk-plan c pw walled to one-way frontier (await (wplan/plan-within! c walled to range weight policy)))))))

(defn ^:async walk!
  "Follow steps once. [result ms]: the executor's done map, or {:status :stuck ...} on a timeout,
  {:status :failed ...}; ms is the wall time of the steer act. With a watch (see watch-stop), the walk also stops at a step
  boundary with {:status :replan ...} when the way ahead changed or a partial plan is due a refresh."
  ([c steps timeout-s] (walk! c steps timeout-s nil))
  ([c steps timeout-s watch]
  (let [policy (wworld/body-policy c)
        state (volatile! (executor/start steps 0 (let [pos (.-pos (.self (:primitives c)))] {:x (.-x pos) :z (.-z pos)})))
        last-done (volatile! nil)
        decide (fn [js-pose]
                 (let [pose (pose-of js-pose)
                       i (:i @state)
                       {:keys [state' done controls yaw pitch]}
                       (let [r (executor/tick policy @state pose)]
                         {:state' (:state r) :done (:done r) :controls (:controls r) :yaw (:yaw r) :pitch (:pitch r)})
                       done (or done (when watch (wwatch/watch-stop watch steps i state' pose)))]
                   (vreset! state state')
                   (if done
                     (do (vreset! last-done done) #js {:done (clj->js done)})
                     #js {:controls (clj->js controls) :yaw yaw :pitch pitch})))
        t (js/Date.now)
        r (await (ctx/act c :steer (steer-args timeout-s decide)))
        ms (- (js/Date.now) t)]
    [(case (.-status r)
       "done" @last-done
       "timeout" {:status :stuck :why (str "walk timed out after " timeout-s " s")
                  :at (let [p (.-pose r)] [(.-x p) (.-y p) (.-z p)])}
       {:status :failed :reason (.-reason r)})
     ms])))

(defn partial-end
  "A partial plan that was walked to its end is not an arrival unless the body is in range of the goal: then the walk
  plans again, unless the plan was cut at a step that cannot be undone, which ends it (stopped-one-way)."
  [done planner-status to range steps stop]
  (let [[x y z] (:at done)]
    (cond
      (not (and (= :arrived (:status done)) (= "partial" planner-status)
                (not (u/within? {:x x :y y :z z} (zipmap [:x :y :z] to) range))))
      done

      stop (merge stop {:at (:at done)})

      :else {:status :off-plan :partial true :at (:at done) :step (dec (count steps))})))

(defn ^:async replan-round-mob!
  "A walk stuck with mobs on its way: wait for them (mob-wait-ms, at most mob-waits times) and plan with the cells they
  stand in as walls; the first plan that can be walked, or, after the waits, a plan with no walls. [plan ms] (ms the
  planning time), the plan possibly not walkable."
  [c plan-fn walkable? steps k]
  (let [{:keys [mob-waits mob-wait-ms]} wwatch/watch-policy]
    (loop [n 1]
      (await (ctx/act c :wait #js {:ms mob-wait-ms}))
      (let [t (js/performance.now)
            walls (wwatch/mob-cells c steps k)
            plan (await (plan-fn walls))
            ms (- (js/performance.now) t)]
        (if (or (walkable? plan) (>= n mob-waits))
          (if (or (walkable? plan) (empty? walls))
            [plan ms]
            (let [t (js/performance.now) plan (await (plan-fn []))] [plan (- (js/performance.now) t)]))
          (recur (inc n)))))))

(defn ^:async follow!
  "Walk plan (a plan-walk! result) and plan again from the body's cell, in the same call, whenever the walk stops for it:
  the look-ahead saw the way change (:changed: the new plan is walked), a partial plan is due a refresh (:refresh: the new
  plan is walked only when take-refresh? says it is clearly better, else the rest of the old one), or the body is stuck with
  a mob in its way (:mob: wait, plan round it). A replan that proves the goal cut off or enclosed ends the call with that
  no-walk. At most max-watch-replans; past that the plan is walked unwatched (a kept plan counts as a replan, and is watched).
  opts: :plan-fn (fn [walls]) -> a plan-walk! result (or a promise of one: plan-walk!) from where the body stands now, the
  cells {:x :y :z} read as walls;
  :walk-fn (fn [steps watch]) -> [done ms] (walk! or jobs.lib.pass/walk!); :to the goal cell; :policy for no-walk;
  :dangers true: a danger newly sensed near the way ahead is planned round once (watch-stop);
  a :health stop (the damage still planned is over what the body may spend now) plans again and walks the new plan, or
  ends the call with its no-walk when that plan is not walkable (the caller decides: go-to heals or goes over its budget);
  :announce! (fn [:replan data]) per replan, data {:why :ms :kept :replans :at :text}.
  {:done :plan :ms :walked :replans :damage}: done the last walk's done map, or the no-walk result of a replan that has no way; plan
  the plan in force at the end; ms the time in walks; walked the blocks of plan walked; damage the hp the steps walked planned."
  [c plan {:keys [plan-fn walk-fn to policy announce! dangers] :or {policy (wworld/body-policy c) announce! (fn [_ _])}}]
  (let [known (when dangers (atom #{}))
        walkable? (fn [pl] (nil? (wplan/no-walk pl 0 policy)))
        proved-none? (fn [pl] (contains? #{:goal-cut-off :goal-enclosed} (:reason (wplan/no-walk pl 0 policy))))
        cut-length (fn [steps k] (path-length (subvec steps 0 (min (count steps) (max 1 k)))))]
    (loop [plan plan steps (:steps plan) n 0 ms 0 walked 0 damage 0]
      (let [watch (when (< n wwatch/max-watch-replans) (wwatch/watch-of c plan known))
            [done wms] (await (walk-fn steps watch))
            ms (+ ms wms)
            k (or (:step done) (count steps))
            walked' (+ walked (cut-length steps k))
            damage' (+ damage (wwatch/damage-ahead (subvec steps 0 (min (count steps) k)) 0))
            ;; a kept plan walks again from step k-1: its damage is counted once, with the walk that ends there
            kept-damage (+ damage (wwatch/damage-ahead (subvec steps 0 (min (count steps) (max 0 (dec k)))) 0))
            finish (fn [d pl w] {:done d :plan pl :ms ms :walked w :replans n :damage damage'})
            tell! (fn [why plan-ms kept]
                    (announce! :replan {:why why :ms (/ (js/Math.round (* 10 plan-ms)) 10) :kept kept :replans (inc n)
                                        :at (:at done)
                                        :text (str "re-plan " (inc n) " (" (name why) (when kept ", kept the plan") ")")}))]
        (cond
          (and (= :replan (:status done)) (= :mob (:why done)))
          (let [t (js/performance.now)
                fresh (await (plan-fn (:cells done)))
                plan-ms (- (js/performance.now) t)]
            (tell! :mob plan-ms false)
            (if (walkable? fresh)
              (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              (recur plan (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))

          (and (= :replan (:status done)) (= :health (:why done)))
          (let [t (js/performance.now)
                fresh (await (plan-fn []))
                plan-ms (- (js/performance.now) t)]
            (tell! :health plan-ms false)
            (if (walkable? fresh)
              (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              (finish (wplan/no-walk fresh 0 policy) fresh walked')))

          (and (= :replan (:status done)) (= :danger (:why done)))
          (let [t (js/performance.now)
                fresh (await (plan-fn []))
                plan-ms (- (js/performance.now) t)]
            (tell! :danger plan-ms (not (walkable? fresh)))
            (cond
              (walkable? fresh) (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              (proved-none? fresh) (finish (wplan/no-walk fresh 0 policy) fresh walked')
              :else (recur (assoc plan :ms plan-ms) (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))

          (= :replan (:status done))
          (let [t (js/performance.now)
                fresh (await (plan-fn []))
                plan-ms (- (js/performance.now) t)
                fresh (assoc fresh :ms plan-ms)]
            (if (= :refresh (:why done))
              (let [take? (and (walkable? fresh) (wwatch/take-refresh? steps fresh to plan))
                    none? (proved-none? fresh)]
                (tell! :refresh plan-ms (not (or take? none?)))
                (cond
                  take? (recur fresh (:steps fresh) (inc n) ms walked' damage')
                  none? (finish (wplan/no-walk fresh 0 policy) fresh walked')
                  :else (recur (assoc plan :ms plan-ms) (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))
              (do (tell! :changed plan-ms false)
                  (if (walkable? fresh)
                    (recur fresh (:steps fresh) (inc n) ms walked' damage')
                    (finish (wplan/no-walk fresh 0 policy) fresh walked')))))

          (and (= :stuck (:status done)) (< n wwatch/max-watch-replans) (seq (wwatch/mob-cells c steps k)))
          (let [[fresh plan-ms] (await (replan-round-mob! c plan-fn walkable? steps k))]
            (tell! :mob plan-ms false)
            (if (walkable? fresh)
              (recur fresh (:steps fresh) (inc n) ms walked' damage')
              (finish done plan walked')))

          :else (finish done plan walked'))))))

(defn note-mismatch!
  "Emit the info :damage-mismatch when the falls hurt since began cost more than margin (default wwatch/fall-margin) over planned hp."
  [c planned began margin]
  (when-let [m (wwatch/damage-mismatch planned (ctx/since c :hurt began) (or margin wwatch/fall-margin))]
    (ctx/emit! c :damage-mismatch :info m)))

(defn ^:async walk-to!
  "Walk the body to within range of the goal cell to ([x y z]): settle, plan, follow the plan (follow!: at most timeout-s
  seconds a walk, planning again in the same call when the way ahead changes, a partial plan is refreshed or a mob is in the
  way), plan again when the body ends off it (at most 5 times). The caller has checked that path-world is there.
  {:result :walked :walk-ms}: result is {:status ...} as in the walk-plan job doc, with :replans; walked is the blocks of
  every plan followed, walk-ms the time inside steer acts. announce! is called (kind data) with :plan before each walk,
  :replan when the body is off its plan and for each of follow!'s replans (those have :why). :fall-margin the hp falls may cost over
  the plan before an info :damage-mismatch (default jobs.lib.walk.watch/fall-margin)."
  [c {:keys [to range weight timeout-s announce! fall-margin] :or {announce! (fn [_ _])}}]
  (let [p (:primitives c)
        plan-fn (fn [walls] (plan-walk! c (wworld/path-world p) to range weight {:walls walls}))
        end (fn [result walked walk-ms] {:result result :walked walked :walk-ms walk-ms})]
    (loop [replans 0 walked 0 walk-ms 0]
      (await (settle! c))
      (let [plan (await (plan-fn []))]
        (if-let [no (wplan/no-walk plan replans (wworld/body-policy c))]
          (end no walked walk-ms)
          (let [{:keys [r steps status]} plan]
            (announce! :plan {:steps (count steps) :summary (some-> (.-path r) .-summary js->clj)
                              :status status :ms (.-ms r) :replans replans
                              :text (str "plan " status ", " (count steps) " steps")})
            (let [began (ctx/now c)
                  {walk-result :done last-plan :plan ms :ms length :walked planned :damage}
                  (await (follow! c plan {:plan-fn plan-fn :to to :announce! announce!
                                          :walk-fn (fn [steps watch] (walk! c steps timeout-s watch))}))
                  walked (+ walked length)
                  walk-ms (+ walk-ms ms)
                  _ (note-mismatch! c planned began fall-margin)
                  done (partial-end walk-result (:status last-plan) to range (:steps last-plan) (:stop last-plan))
                  after (if (#{:arrived :off-plan :stuck} (:status done))
                          (executor/after-walk executor/policy replans done)
                          {:finish done})]
              (if-let [n (:replan after)]
                (do (announce! :replan {:at (:at done) :replans n :text (str "off the plan, re-plan " n)})
                    (recur n walked walk-ms))
                (end (assoc (:finish after) :replans replans) walked walk-ms)))))))))
