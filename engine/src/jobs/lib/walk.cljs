(ns jobs.lib.walk
  "The walk driver: plan from the body's cell to a goal within the executor's abilities, follow the plan with steer, plan
  again when the body ends off it. Jobs that walk (the stair, tunnel and cleanup jobs) call this
  namespace; jobs.lib.walk.drive holds the loop (walk-to!, follow!); this one holds its pieces (plan-walk!, walk!); more live in jobs.lib.walk.plan (planning), .search (the budgeted search a round), .watch (the look-ahead) and .world (the pathWorld and its decorations) and .light (a torch for an unseen floor)."
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
