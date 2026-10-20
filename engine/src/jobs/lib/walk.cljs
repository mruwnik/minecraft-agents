(ns jobs.lib.walk
  "The walk driver: plan from the body's cell to a goal within the executor's abilities, follow the plan with steer, plan
  again when the body ends off it. Jobs that walk (the stair, tunnel and cleanup jobs) call this
  namespace; walk-to! is the whole loop, the other functions are its pieces; the pieces live in jobs.lib.walk.plan (planning), .search (the budgeted search a round), .watch (the look-ahead) and .world (the pathWorld and its decorations) and .light (a torch for an unseen floor)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.path.blocks :as blocks]
            [engine.path.executor :as executor]
            [jobs.lib.look :as look]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk.light :as wlight]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.watch :as wwatch]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::max-timeout-s {:default 120 :doc "The longest a walk call may run, in seconds." :spec (a/int-in 1 nil)}
   ::max-settle-waits {:default 20 :doc "Waits for the body to settle after a step before the walk goes on." :spec (a/int-in 0 nil)}
   ::centre-tolerance {:default 0.2 :doc "How near (blocks, each axis) a centring nudge ends to its target." :spec (a/num-in 0 nil)}
   ::centre-max-ticks {:default 30 :doc "Ticks a centring nudge walks before it gives up." :spec (a/int-in 1 nil)}
   ::default-weight {:default 1.2 :doc "Cost weight of the search heuristic a walk plans with (above 1: faster, less exact)." :spec (a/num-in 1 nil)}
   ::light-wait-ms {:default 1500 :doc "How long a walk that placed a torch looks for the floor it lights before it gives the floor up, in ms." :spec (a/int-in 0 nil)}})

(defn max-timeout-s [] (settings/get settings ::max-timeout-s))
(defn max-settle-waits [] (settings/get settings ::max-settle-waits))
(defn default-weight [] (settings/get settings ::default-weight))
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
      (when (and (< n (max-settle-waits)) (not (grounded? p)))
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
   :on-climbable (.-onClimbable pose) :on-scaffolding (.-onScaffolding pose) :in-water (.-inWater pose) :under-water (.-underWater pose) :in-lava (.-inLava pose) :collided (.-collided pose)})

(def room-margin-s "Seconds a steer leaves of the time left, for the time between asking and the act's check." 0.05)
(def min-steer-s "The shortest steer a walk holds to the time left; with less left the act's refusal ends the call." 0.5)

(defn steer-s
  "timeout-s held to what c's call chain has left (ctx/left: time and deadline) less room-margin-s, since a longer
  steer is refused; with less than min-steer-s left, timeout-s as it is."
  [c timeout-s]
  (let [{:keys [spend deadline]} (ctx/left c)
        rooms (cond-> []
                (:job-time spend) (conj (:job-time spend))
                deadline (conj (/ (- deadline (ctx/now c)) 1000)))
        room (when (seq rooms) (- (apply min rooms) room-margin-s))]
    (if (and room (>= room min-steer-s)) (min timeout-s room) timeout-s)))

(defn steer-args
  "The act args of one walk. decide is not enumerable: the engine's act wrapper writes (js->clj args) into
  an event, and a function there would break the event stream."
  [timeout-s decide]
  (doto (js-obj "timeoutS" (min (max-timeout-s) timeout-s))
    (js/Object.defineProperty "decide" #js {:value decide})))

(defn centre-tolerance [] (settings/get settings ::centre-tolerance))
(defn centre-max-ticks [] (settings/get settings ::centre-max-ticks))

(defn centre-decider
  "A steer decide function that walks straight at [tx tz], done within tol (default centre-tolerance) of it or after
  centre-max-ticks."
  ([tx tz] (centre-decider tx tz (centre-tolerance)))
  ([tx tz tol]
   (let [ticks (volatile! 0)]
     (fn [js-pose]
       (let [dx (- tx (.-x js-pose)) dz (- tz (.-z js-pose))]
         (if (or (and (<= (js/Math.abs dx) tol) (<= (js/Math.abs dz) tol))
                 (>= (vswap! ticks inc) (centre-max-ticks)))
           #js {:done true}
           #js {:controls #js {:forward true} :yaw (js/Math.atan2 (- dx) (- dz))}))))))

(defn ^:async centre!
  "A short walk (no planning, no jump) to within centre-tolerance (or tol) of [tx tz] on the cell the body is in: the step a
  go-to to a cell cannot make, since a body on the edge of the cell already counts as in it."
  ([c tx tz] (centre! c tx tz (centre-tolerance)))
  ([c tx tz tol]
   (await (ctx/act c :steer (steer-args (steer-s c 3) (centre-decider tx tz tol))))))

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

(def sneak-moves
  "The steps a sneaking walk holds sneak on: level ground and jumps up. A drop or gap lets go (sneak stops at the edge),
  and so does a climb (it holds the body on a ladder)."
  #{:start :walk :diagonal :corner :jump})

(defn gait-controls
  "The executor's controls under the policy's gait: a sneaking walk never sprints and holds sneak only on the ground, on a
  sneak-moves step out of water (it sinks the body, and stops it at a deep bank), off a climbable and scaffolding (sneak descends there); in the air it
  lets go, so a slime landing bounces. A :climb-down step from scaffolding holds sneak under every gait (scaffolding is
  solid on top unless sneaking)."
  [policy {:keys [steps i]} {:keys [on-ground in-water on-climbable on-scaffolding]} controls]
  (let [step (get steps i)
        scaffold-down? (and on-scaffolding (= :climb-down (:move step)))]
    (cond-> controls
      (= :sneak (:gait policy))
      (assoc :sprint false
             :sneak (boolean (and on-ground (not in-water) (not on-climbable) (not on-scaffolding)
                                  (contains? sneak-moves (:move step)) (not (executor/water-step? step)))))
      scaffold-down? (assoc :sneak true))))

(defn prefix
  "The executor's steps when step k is a pending gate (nil: none): steps up to the takeoff k-1, grown from the executor's
  own (which may have changed a step in place) as gates resolve."
  [exec-steps steps k]
  (let [n (if k k (count steps))]
    (if (<= n (count exec-steps))
      (subvec exec-steps 0 n)
      (into exec-steps (subvec steps (count exec-steps) n)))))

(defn gate-tick
  "decide-tick with the floor gate (jobs.lib.walk.sight): a body standing on a pending floor ends the walk :pushed; the
  pending floors are sensed (a plain one resolves, another ends :changed); the executor gets the steps up to the first
  pending gate's takeoff (a walk that starts on the takeoff walks to it), its arrival there does not end the walk, nor its
  stuck clock on the takeoff; the gate's controller holds the body."
  [{:keys [policy steps watch-stop gate]} {:keys [exec] :as state} pose]
  (let [i (:i exec)
        at [(:x pose) (:y pose) (:z pose)]
        before (wsight/pending steps (:resolved (:gate state) #{}) i)]
    (if-let [cell (wsight/breach steps before pose)]
      [state {:done {:status :replan :why :pushed :cells [cell] :at at :step before}
              :warn {:cell cell :step before :at at}}]
      (let [{:keys [k gstate done]} (wsight/sense gate steps (:gate state) i pose)
            settling? (and done (< (:settled gstate 0) wsight/settle-ticks) (wsight/moving? pose))]
        (if settling?
          [(assoc state :gate (update gstate :settled (fnil inc 0))) (wsight/settle gate steps k pose)]
          (let [pre (prefix (:steps exec) steps k)
                exec (cond-> (assoc exec :steps pre) (>= (:i exec) (count pre)) (assoc :i (dec (count pre))))
                {exec' :state edone :done :keys [controls yaw pitch]} (executor/tick policy exec pose)
                held? (and k (= (:i exec') (dec k)))
                on? (and k (wsight/on-takeoff? (nth steps (dec k)) held? gstate pose))
                edone (when-not (or (and held? (= :arrived (:status edone))) (and on? (= :stuck (:status edone)))) edone)
                done (or done edone (when watch-stop (watch-stop i exec' pose)))
                base {:controls (gait-controls policy exec' pose controls) :yaw yaw :pitch pitch}]
            (cond
              done [(assoc state :exec exec' :gate gstate) {:done done}]
              (nil? k) [(assoc state :exec exec' :gate gstate) base]
              :else (let [{:keys [out done] gstate' :gstate} (wsight/control gate steps k (:i exec') gstate pose base)]
                      [(assoc state :exec exec' :gate gstate') (if done {:done done} out)]))))))))

(defn standing-pose
  "pose with :on-climbable off for a body on the ground in scaffolding: it stands on the scaffolding under it, so the
  executor walks a step off the tower instead of holding for a fall that never comes."
  [{:keys [on-ground on-scaffolding] :as pose}]
  (cond-> pose (and on-ground on-scaffolding) (assoc :on-climbable false)))

(defn decide-tick
  "One tick of a walk, pure: [state' out] for the pose, out {:done map} or {:controls :yaw :pitch} (with :warn, a gate
  breach). state {:exec :gate} holds the executor's and the gate's state. env {:policy :steps :watch-stop :gate}:
  watch-stop (fn [i state' pose]) the look-ahead's done map or nil; gate (nil: off) {:see :plain? :floors :opts} for
  jobs.lib.walk.sight."
  [{:keys [policy steps watch-stop gate] :as env} {:keys [exec] :as state} pose]
  (let [pose (standing-pose pose)]
    (if gate
      (gate-tick env state pose)
      (let [i (:i exec)
            {exec' :state :keys [done controls yaw pitch]} (executor/tick policy exec pose)
            done (or done (when watch-stop (watch-stop i exec' pose)))
            state' (assoc state :exec exec')]
        [state' (if done
                  {:done done}
                  {:controls (gait-controls policy exec' pose controls) :yaw yaw :pitch pitch})]))))

(defn gate-env
  "The floor gate's env for walking steps (jobs.lib.walk.sight), nil when no step has an unseen floor. see: what the body
  knows of a cell now, else what a glance at it shows (nil: still unknown); plain?: a full block the planner reads as
  stone (the floor a phantom floor was planned as); floors: the known block name of a cell (the controller's friction)."
  [p steps]
  (when (some :unseen-floor steps)
    (let [table (blocks/default-state-table)
          stone (first (blocks/state-ids table "stone"))
          known (fn [[x y z]] (let [b (u/sensed p {:x x :y y :z z})] (when-not (or (nil? b) (true? (.-unknown b))) b)))]
      {:see (fn [[x y z :as cell]] (or (known cell) (when (fn? (.-glance p)) (.glance p #js {:x x :y y :z z}))))
       :plain? (fn [^js b] (boolean (and (true? (.-fullCube b))
                                         (when-let [id (first (blocks/state-ids table (.-name b)))]
                                           (wwatch/same-for-planner? table stone id)))))
       :floors (fn [cell] (some-> (known cell) .-name))
       :opts (wsight/gate-opts)})))

(defn start-state
  "The decide-tick state for walking steps from the body at pose {:x :z}."
  [steps pose]
  {:exec (executor/start steps 0 pose)})

(defn ^:async steer-walk!
  "walk!'s one steer act over steps: [result ms], result {:status :light ...} when the gate asks for a torch."
  [c steps timeout-s watch]
  (let [gate (gate-env (:primitives c) steps)
        watch (cond-> watch (and watch gate) (update :skip (fnil into #{}) (wsight/floor-cells steps)))
        env {:policy (wworld/body-policy c) :steps steps :gate gate
             :watch-stop (when watch (fn [i exec' pose] (wwatch/watch-stop watch steps i exec' pose)))}
        state (volatile! (start-state steps (let [pos (.-pos (.self (:primitives c)))] {:x (.-x pos) :z (.-z pos)})))
        last-done (volatile! nil)
        decide (fn [js-pose]
                 (let [pose (pose-of js-pose)
                       _ (when-let [on-pose (:on-pose c)] (on-pose pose))
                       [state' {:keys [done controls yaw pitch warn]}] (decide-tick env @state pose)]
                   (vreset! state state')
                   (when warn (ctx/emit! c :gate-breach :warn warn))
                   (if done
                     (do (vreset! last-done done) #js {:done (clj->js done)})
                     #js {:controls (clj->js controls) :yaw yaw :pitch pitch})))
        t (js/Date.now)
        r (await (ctx/act c :steer (steer-args (steer-s c timeout-s) decide)))
        ms (- (js/Date.now) t)]
    [(case (.-status r)
       "done" @last-done
       "timeout" {:status :stuck :why (str "walk timed out after " timeout-s " s")
                  :at (let [p (.-pose r)] [(.-x p) (.-y p) (.-z p)])}
       {:status :failed :reason (.-reason r)})
     ms]))

(defn light-wait-ms [] (settings/get settings ::light-wait-ms))

(def look-ahead-steps "Steps ahead of the body whose own cells a masked walk looks at before it commits to them." 3)

(defn first-unseen-step
  "The index of the first step from 1 on whose feet or head cell the body has never seen and not looked at (looked, a set
  of cells), nil when there is none."
  [p steps looked]
  (let [new? (fn [cell] (and (not (contains? looked cell)) (sense/unknown? p cell)))]
    (first (keep-indexed (fn [i {:keys [x y z]}]
                           (when (and (pos? i) (or (new? [x y z]) (new? [x (inc y) z]))) i))
                         steps))))

(defn occluded?
  "Whether a seen solid block lies between the body's eye and the centre of cell: what it cannot see there is rock, not
  a place it has failed to look at."
  [c [x y z :as cell]]
  (let [p (:primitives c)
        {ex :x ey :y ez :z} (let [pos (u/self-pos c)] (update pos :y + game/eye-height))
        tx (+ x 0.5) ty (+ y 0.5) tz (+ z 0.5)
        d (js/Math.hypot (- tx ex) (- ty ey) (- tz ez))
        n (js/Math.ceil (/ d 0.2))]
    (boolean (some (fn [k]
                     (let [t (/ k n)
                           at [(js/Math.floor (+ ex (* t (- tx ex)))) (js/Math.floor (+ ey (* t (- ty ey)))) (js/Math.floor (+ ez (* t (- tz ez))))]]
                       (and (not= at cell)
                            (true? (some-> (u/sensed p (zipmap [:x :y :z] at)) .-fullCube)))))
                   (range 1 n)))))

(defn ^:async look-ahead!
  "Over a mask (go-to :unknown) a step may enter cells the body never saw, which the plan reads as air: look round and at the
  next look-ahead-steps steps' cells first, as a player looks before climbing into the dark. A cell that turns out solid, or
  stays unseen behind a seen solid block (rock: added to the mask's blocked Set), ends the walk {:status :replan :why
  :changed}, so the next plan avoids it. {:end done} then; else {:looked cells} the walk goes on over."
  [c steps]
  (let [p (:primitives c)
        cells (for [{:keys [x y z]} (take look-ahead-steps (rest steps)) y' [y (inc y)]] [x y' z])
        unseen (filter #(sense/unknown? p %) cells)]
    (await (look/survey! c :near))
    (await (sense/look-at-unknown! c (sort-by #(u/eye-dist (u/self-pos c) %) unseen) look-ahead-steps))
    (let [hidden (filter #(and (sense/unknown? p %) (occluded? c %)) cells)
          solid (filter #(true? (some-> (u/sensed p (zipmap [:x :y :z] %)) .-fullCube)) cells)]
      (doseq [[x y z] hidden] (.add ^js (.-blocked ^js (:unknown c)) (str x "," y "," z)))
      (if-let [cell (first (concat solid hidden))]
        (let [{:keys [x y z]} (u/self-pos c)]
          {:end {:status :replan :why :changed :cells [cell] :at [x y z] :step 1}})
        {:looked unseen}))))

(defn ^:async walk-lit!
  "Follow steps once, a floor the plan lights (:light) lit on the way: walk!'s inner loop."
  [c steps timeout-s watch]
  (loop [steps steps base 0 ms 0]
    (let [[done wms] (await (steer-walk! c steps (max 1 (- timeout-s (/ ms 1000))) watch))
          ms (+ ms wms)
          rebase (fn [d] (cond-> d (:step d) (update :step + base)))]
      (if-not (= :light (:status done))
        [(rebase done) ms]
        (if-let [end (await (wlight/light! c steps done (:see (gate-env (:primitives c) steps)) (light-wait-ms)))]
          [(rebase end) ms]
          (let [k (:step done)]
            (recur (update (subvec steps (dec k)) 1 dissoc :light) (+ base (dec k)) ms)))))))

(defn ^:async walk!
  "Follow steps once. [result ms]: the executor's done map, or {:status :stuck ...} on a timeout,
  {:status :failed ...}; ms is the wall time of the steer acts. With a watch (see watch-stop), the walk also stops at a step
  boundary with {:status :replan ...} when the way ahead changed or a partial plan is due a refresh. (:on-pose c), when
  set, is called with each tick's pose {:x :y :z ...} (go-to records its approach with it). A floor the plan lights
  (:light) that the gate cannot see is lit by a torch (light!) and the walk goes on from its takeoff, within timeout-s
  of steering in all; light!'s other ends end the walk. Over a mask (go-to :unknown) the walk goes in legs that end
  before the first step into cells the body has not seen, looks at them there (look-ahead!) and goes on, or ends
  {:status :replan :why :changed}."
  ([c steps timeout-s] (walk! c steps timeout-s nil))
  ([c steps timeout-s watch]
   (loop [steps steps base 0 ms 0 looked #{}]
     (let [j (when (:unknown c) (first-unseen-step (:primitives c) steps looked))
           rebase (fn [d] (cond-> d (:step d) (update :step + base)))]
       (cond
         (= 1 j) (let [r (await (look-ahead! c steps))]
                   (if-let [end (:end r)]
                     [(rebase end) ms]
                     (recur steps base ms (into looked (:looked r)))))
         (nil? j) (let [[done wms] (await (walk-lit! c steps (max 1 (- timeout-s (/ ms 1000))) watch))]
                    [(rebase done) (+ ms wms)])
         :else (let [[done wms] (await (walk-lit! c (subvec steps 0 j) (max 1 (- timeout-s (/ ms 1000))) watch))
                     ms (+ ms wms)]
                 (if (= :arrived (:status done))
                   (recur (subvec steps (dec j)) (+ base (dec j)) ms looked)
                   [(rebase done) ms])))))))

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

(defn ^:async back-off!
  "Walk the body back along the way it came to a cell clear of the hostile-near radius of the seen mob that blocks the way
  (fresh's pw dangers), so what follows (a stair, another way) does not start in reach of it. No-op when none is blocking or clear."
  [c fresh walked walk-fn policy]
  (when-let [cell (wplan/retreat-cell walked (.-dangers ^js (:pw fresh)))]
    (let [{:keys [steps]} (await (wplan/plan-within! c (:pw fresh) cell 0 (default-weight) policy))]
      (when (> (count steps) 1)
        (await (walk-fn steps nil))))))

(defn ^:async follow!
  "Walk plan (a plan-walk! result) and plan again from the body's cell, in the same call, whenever the walk stops for it:
  the look-ahead saw the way change (:changed: the new plan is walked), a partial plan is due a refresh (:refresh: the new
  plan is walked only when take-refresh? says it is clearly better, else the rest of the old one), or the body is stuck with
  a mob in its way (:mob: wait, plan round it). A replan that proves the goal cut off or enclosed ends the call with that
  no-walk; so does one the caller refused (plan-fn's {:refused true}, with its :stop) for any why but :refresh, whose old
  plan nothing speaks against. At most max-watch-replans; past that the plan is walked unwatched (a kept plan counts as a replan, and is watched).
  opts: :plan-fn (fn [walls]) -> a plan-walk! result (or a promise of one: plan-walk!) from where the body stands now, the
  cells {:x :y :z} read as walls;
  :walk-fn (fn [steps watch]) -> [done ms] (walk! or jobs.lib.pass/walk!); :to the goal cell; :policy for no-walk;
  :dangers true: a danger newly sensed near the way ahead is planned round once (watch-stop); a search that has not
  finished ends the call with its :searching no-walk (the old plan is never walked on past the danger);
  a :health stop (the damage still planned is over what the body may spend now) plans again and walks the new plan, or
  ends the call with its no-walk when that plan is not walkable (the caller decides: go-to heals or goes over its budget);
  :announce! (fn [:replan data]) per replan, data {:why :ms :kept :replans :at :text}.
  {:done :plan :ms :walked :replans :damage}: done the last walk's done map, or the no-walk result of a replan that has no way; plan
  the plan in force at the end; ms the time in walks; walked the blocks of plan walked; damage the hp the steps walked planned."
  [c plan {:keys [plan-fn walk-fn to policy announce! dangers] :or {policy (wworld/body-policy c) announce! (fn [_ _])}}]
  (let [known (when dangers (atom #{}))
        walkable? (fn [pl] (nil? (wplan/no-walk pl 0 policy)))
        proved-none? (fn [pl] (let [nw (wplan/no-walk pl 0 policy)]
                                (or (contains? #{:goal-cut-off :goal-enclosed} (:reason nw)) (:stopped-short nw))))
        searching? (fn [pl] (= :searching (:status (wplan/no-walk pl 0 policy))))
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
            (cond
              (walkable? fresh) (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              (:refused fresh) (finish (wplan/no-walk fresh 0 policy) fresh walked')
              :else (recur plan (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))

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
            (tell! :danger plan-ms (not (or (walkable? fresh) (searching? fresh) (:refused fresh))))
            (when (or (:stopped-short (wplan/no-walk fresh 0 policy)) (and (searching? fresh) (:stopped-short fresh)))
              (await (back-off! c fresh (subvec steps 0 (min (count steps) (inc k))) walk-fn policy)))
            (cond
              (walkable? fresh) (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              ;; an unfinished search: the old plan runs past the danger, so the caller searches on from here
              (or (proved-none? fresh) (searching? fresh) (:refused fresh)) (finish (wplan/no-walk fresh 0 policy) fresh walked')
              :else (recur (assoc plan :ms plan-ms) (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))

          (= :replan (:status done))
          (let [_ (wsight/note-missed! (:pw plan) done)
                t (js/performance.now)
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
  "Emit the info :damage-mismatch when the falls hurt since began cost more than margin (default the fall-margin setting) over planned hp."
  [c planned began margin]
  (when-let [m (wwatch/damage-mismatch planned (ctx/since c :hurt began) (or margin (wwatch/fall-margin)))]
    (ctx/emit! c :damage-mismatch :info m)))

(defn ^:async walk-to!
  "Walk the body to within range of the goal cell to ([x y z]): settle, plan, follow the plan (follow!: at most timeout-s
  seconds a walk, planning again in the same call when the way ahead changes, a partial plan is refreshed or a mob is in the
  way), plan again when the body ends off it (at most 5 times). The caller has checked that path-world is there.
  {:result :walked :walk-ms}: result is a {:status ...} map with :replans; walked is the blocks of
  every plan followed, walk-ms the time inside steer acts. announce! is called (kind data) with :plan before each walk,
  :replan when the body is off its plan and for each of follow!'s replans (those have :why). :fall-margin the hp falls may cost over
  the plan before an info :damage-mismatch (default the fall-margin setting of jobs.lib.walk.watch)."
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

(defn ^:async way-back
  "nil when a whole plan the executor can walk leads from the body to origin on a fresh pathWorld, else the stop (a
  promise: the search yields to the event loop)."
  [c origin]
  (let [pw (wworld/path-world (:primitives c))]
    (if (nil? pw)
      {:reason :no-way-back :why :unsupported}
      (let [{:keys [r steps beyond]} (await (wplan/plan-within! c pw origin 0 (default-weight) (wworld/body-policy c)))
            status (.-status r)
            stop (fn [refused] {:reason :no-way-back :why :refused :kind (:kind refused) :step (:at refused)})]
        (cond
          beyond (stop beyond)
          (not= "found" status) {:reason :no-way-back :why (keyword status) :planner (some-> (.-reason r) keyword)}
          :else (some-> (executor/refusal (wworld/body-policy c) steps) stop))))))
