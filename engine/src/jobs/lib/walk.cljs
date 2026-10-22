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
            [jobs.lib.breath :as breath]
            [engine.perception.mobs :as mobs]
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
   ::glance-ticks {:default 10 :doc "A walk glances round for hostiles behind it (a mob sample with the head turned, the real head and the steering untouched) every this many ticks; 0: never." :spec (a/int-in 0 nil)}
   ::light-wait-ms {:default 1500 :doc "How long a walk that placed a torch looks for the floor it lights before it gives the floor up, in ms." :spec (a/int-in 0 nil)}})

(def glance-turns "Head turns of a walk's glances, in radians from the heading: behind, left, behind, right." [js/Math.PI (/ js/Math.PI 2) js/Math.PI (/ js/Math.PI -2)])

(def key-ring "Movement keys in the order a head turn of a quarter turn left moves each one to." [:forward :right :back :left])

(defn turn-controls
  "controls as the same movement needs them with the head turned by turn radians (a multiple of a quarter turn, left
  positive): each movement key becomes the one that moves the body the same way. Sprint stays only with forward."
  [controls turn]
  (let [k (mod (js/Math.round (/ turn (/ js/Math.PI 2))) 4)
        moved (reduce (fn [m [i key]] (cond-> m (get controls key) (assoc (nth key-ring (mod (+ i k) 4)) true)))
                      {} (map-indexed vector key-ring))
        out (merge (dissoc controls :forward :back :left :right) moved)]
    (cond-> out (not (:forward out)) (assoc :sprint false))))

(defn glance-ticks [] (settings/get settings ::glance-ticks))
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
   :on-climbable (.-onClimbable pose) :on-scaffolding (.-onScaffolding pose) :in-water (.-inWater pose) :under-water (.-underWater pose) :in-lava (.-inLava pose) :collided (.-collided pose) :collided-y (.-collidedVertically pose)})

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

(defn air-stop
  "The replan done map when the body, at a new step, swims on past the air it may spend (wplan/swim-outlasts-air?) so the
  re-plan can turn it back; nil otherwise (the look-ahead does not run in water)."
  [c steps i i2 {:keys [x y z]}]
  (when (and (> i2 i) (wplan/swim-outlasts-air? c (subvec steps (min i2 (count steps)))))
    {:status :replan :why :air :at [x y z] :step i2}))

(defn gate-tick
  "decide-tick with the floor gate (jobs.lib.walk.sight): a body standing on a pending floor ends the walk :pushed; the
  pending floors are sensed (a plain one resolves, another ends :changed); the executor gets the steps up to the first
  pending gate's takeoff (a walk that starts on the takeoff walks to it), its arrival there does not end the walk, nor its
  stuck clock on the takeoff; the gate's controller holds the body."
  [{:keys [policy steps watch-stop gate air-c]} {:keys [exec] :as state} pose]
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
                done (or done edone (when air-c (air-stop air-c steps i (:i exec') pose)) (when watch-stop (watch-stop i exec' pose)))
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

(defn box-in-cell?
  "Whether the body's box at x z lies inside the cell of step: off-centre it may clip a neighbour's block, which says nothing about the cell."
  [step x z]
  (let [slack (- 0.5 (:body-half executor/policy))]
    (and (<= (js/Math.abs (- x (+ (:x step) 0.5))) slack) (<= (js/Math.abs (- z (+ (:z step) 0.5))) slack))))

(defn felt-solid
  "The replan done map when a body swimming vertically is stopped by a block the plan took for water: it felt the floor
  (swim-down) or the roof (swim-up, only with the box inside the step's column: off-centre it may have clipped a neighbour), so that cell is solid and the plan is made again at once. nil otherwise."
  [steps i {:keys [x y z on-ground collided-y]}]
  (let [{:keys [move] :as step} (nth steps i nil)
        gap (- y (executor/stand-y step))
        arrive (:arrive-y executor/policy)
        cell (case move
               :swim-down (when (and on-ground (> gap arrive)) [(:x step) (:y step) (:z step)])
               :swim-up (when (and collided-y (not on-ground) (< gap (- arrive)) (box-in-cell? step x z))
                          [(:x step) (inc (:y step)) (:z step)])
               nil)]
    (when cell
      {:status :replan :why :changed :learned true :felt true :cells [cell] :at [x y z] :step i})))

(defn decide-tick
  "One tick of a walk, pure: [state' out] for the pose, out {:done map} or {:controls :yaw :pitch} (with :warn, a gate
  breach). state {:exec :gate} holds the executor's and the gate's state. env {:policy :steps :watch-stop :gate :felt?}: felt?, over a mask, ends the walk at a floor or roof the body felt (felt-solid);
  watch-stop (fn [i state' pose]) the look-ahead's done map or nil; gate (nil: off) {:see :plain? :floors :opts} for
  jobs.lib.walk.sight."
  [{:keys [policy steps watch-stop gate] :as env} {:keys [exec] :as state} pose]
  (let [pose (standing-pose pose)]
    (if gate
      (gate-tick env state pose)
      (let [i (:i exec)
            {exec' :state :keys [done controls yaw pitch]} (executor/tick policy exec pose)
            done (or done (when (:felt? env) (felt-solid steps (:i exec') pose)) (when-let [c (:air-c env)] (air-stop c steps i (:i exec') pose)) (when watch-stop (watch-stop i exec' pose)))
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
        env {:policy (wworld/body-policy c) :steps steps :gate gate :felt? (boolean (:unknown c)) :air-c c
             :watch-stop (when watch (fn [i exec' pose] (wwatch/watch-stop watch steps i exec' pose)))}
        _ (when gate
            (ctx/emit! c :walk.gates :debug {:steps (into [] (comp (take 60) (map (juxt :x :y :z :move #(boolean (:unseen-floor %))))) steps)}))
        state (volatile! (start-state steps (let [pos (.-pos (.self (:primitives c)))] {:x (.-x pos) :z (.-z pos)})))
        last-done (volatile! nil)
        p (:primitives c)
        every (glance-ticks)
        ticks (volatile! 0)
        turned (volatile! false)
        ;; a head turn lasts one tick: the sample comes on the next, with the head still turned, and the head goes back
        sample! (fn []
                  (when @turned
                    (vreset! turned false)
                    (when (fn? (.-knownMobs p)) (.knownMobs p))))
        glance (fn [pose {:keys [controls yaw pitch] :as out}]
                 (let [n (vswap! ticks inc)
                       turn (nth glance-turns (mod (dec (quot n every)) (count glance-turns)))]
                   (if (and (pos? every) (zero? (mod n every)) (number? yaw) (:on-ground pose) (not (:in-water pose))
                            (not (:on-climbable pose)) (not (:jump controls)) (not (:sneak controls))
                            (not (mobs/enderman-at-yaw? (aget p "perception") (+ yaw turn))))
                     (do (vreset! turned true)
                         (assoc out :controls (turn-controls controls turn) :yaw (+ yaw turn)))
                     out)))
        decide (fn [js-pose]
                 (let [pose (pose-of js-pose)
                       _ (sample!)
                       _ (when-let [on-pose (:on-pose c)] (on-pose pose))
                       [state' {:keys [done controls yaw pitch warn]}] (decide-tick env @state pose)]
                   (when gate
                     (doseq [k (remove (get-in @state [:gate :resolved] #{}) (get-in state' [:gate :resolved] #{}))]
                       (let [cell (wsight/floor-cell (nth steps k))]
                         (ctx/emit! c :walk.gate-resolved :debug {:step k :cell cell :block ((:floors gate) cell)}))))
                   (vreset! state state')
                   (when-let [[x y z] (and (:unknown c) (:felt done) (first (:cells done)))]
                     (.add ^js (.-blocked ^js (:unknown c)) (str x "," y "," z)))
                   (when warn (ctx/emit! c :gate-breach :warn warn))
                   (if done
                     (do (vreset! last-done done) #js {:done (clj->js done)})
                     (let [out (glance pose {:controls controls :yaw yaw :pitch pitch})]
                       #js {:controls (clj->js (:controls out)) :yaw (:yaw out) :pitch (:pitch out)}))))
        t (js/Date.now)
        steer-for (steer-s c timeout-s)
        r (await (ctx/act c :steer (steer-args steer-for decide)))
        ms (- (js/Date.now) t)]
    [(case (.-status r)
       "done" @last-done
       "timeout" (cond-> {:status :stuck :why (str "walk timed out after " timeout-s " s")
                          :at (let [p (.-pose r)] [(.-x p) (.-y p) (.-z p)])}
                   (< steer-for timeout-s) (assoc :out-of-time true))
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

(defn unseen-overhead
  "The first cell over a swim step's head cell that the body never saw, when the cells between are seen water (the air a
  dive comes up in is two or three cells over the head); nil when the step is not a swim with its head under or the
  first cell over the water is seen."
  [p {:keys [x y z swim]}]
  (when (and swim (not (breath/air? (breath/seen-water-name p {:x x :y (inc y) :z z}))))
    (loop [dy 2]
      (when (<= dy 3)
        (let [cell {:x x :y (+ y dy) :z z}]
          (cond
            (sense/unknown? p [x (+ y dy) z]) [x (+ y dy) z]
            (let [n (breath/seen-water-name p cell)] (and n (not (breath/air? n)) (breath/passable-water-or-air? n))) (recur (inc dy))
            :else nil))))))

(defn first-unseen-overhead
  "The index of the first step from 1 on that is a swim with an unseen cell over its head (unseen-overhead) not in looked, nil when there is none."
  [p steps looked]
  (first (keep-indexed (fn [i s] (when (pos? i) (when-let [cell (unseen-overhead p s)] (when-not (contains? looked cell) i)))) steps)))

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

(def landing-look-drop "A step down of at least this many blocks is looked at before the body drops: the landing and the cells beside it." 2)

(defn landing-cells
  "The cells [x y z] of step i's landing and beside it (feet and head level, the eight horizontal neighbours) when it is a drop of
  landing-look-drop or more, else nil."
  [steps i]
  (let [{:keys [x y z move]} (nth steps i)]
    (when (and (= :drop move) (>= (- (:y (nth steps (dec i))) y) landing-look-drop))
      (for [dx [-1 0 1] dz [-1 0 1] dy [0 1]] [(+ x dx) (+ y dy) (+ z dz)]))))

(defn orthogonal-landing-cells
  "The landing-cells of step i in its own column or the four columns beside it (no diagonal)."
  [steps i]
  (let [{:keys [x z]} (nth steps i)]
    (filter (fn [[cx _ cz]] (<= (+ (js/Math.abs (- cx x)) (js/Math.abs (- cz z))) 1)) (landing-cells steps i))))

(defn first-unseen-landing
  "The index of the first step from 1 on that is a drop (landing-cells) with a cell the body never saw and has not looked at (looked), nil when there is none."
  [p steps looked]
  (first (filter #(some (fn [cell] (and (not (contains? looked cell)) (sense/unknown? p cell))) (landing-cells steps %))
                 (range 1 (count steps)))))

(defn ^:async look-landing!
  "Look at the unseen cells of step 1's landing, as a player looks into a hole before jumping down. Lava newly seen in or
  beside the landing (its cell or one orthogonally beside) ends the walk {:status :replan :why :changed}: the planner prices
  the landing from the lava now seen and may still take it as the only way. Lava seen before the look was priced by the plan
  already, so it replans nothing; lava only diagonal is no danger: {:looked cells}."
  [c steps]
  (let [p (:primitives c)
        cells (landing-cells steps 1)
        lava (fn [] (set (filter #(= "lava" (u/seen-name p (zipmap [:x :y :z] %))) (orthogonal-landing-cells steps 1))))
        before (lava)]
    (await (sense/look-at-unknown! c (u/nearest-first (u/self-pos c) identity (filter #(sense/unknown? p %) cells)) 2))
    (if (seq (remove before (lava)))
      (let [{:keys [x y z]} (nth steps 1) {sx :x sy :y sz :z} (u/self-pos c)]
        {:end {:status :replan :why :changed :learned true :cells [[x y z] [x (inc y) z]] :at [sx sy sz] :step 1}})
      {:looked cells})))

(defn solid-ahead
  "The first feet or head cell of the next look-ahead-steps steps that the body now knows is a full block, or nil. A cell the
  plan took for air because it was unseen is seen from nearer by the time a later leg starts."
  [p steps]
  (first (for [{:keys [x y z]} (take look-ahead-steps (rest steps)) y' [y (inc y)]
               :when (true? (some-> (u/sensed p {:x x :y y' :z z}) .-fullCube))]
           [x y' z])))

(defn ^:async look-ahead!
  "Over a mask (go-to :unknown) a step may enter cells the body never saw, which the plan reads as air: look round and at the
  next look-ahead-steps steps' cells first, as a player looks before climbing into the dark. A cell that turns out solid, or
  stays unseen behind a seen solid block (rock: added to the mask's blocked Set), ends the walk {:status :replan :why
  :changed}, so the next plan avoids it. {:end done} then; else {:looked cells} the walk goes on over."
  [c steps]
  (let [p (:primitives c)
        cells (for [{:keys [x y z]} (take look-ahead-steps (rest steps)) y' [y (inc y)]] [x y' z])
        unseen (filter #(sense/unknown? p %) cells)]
    ;; unseen cells ahead that a look could reach (not behind seen rock) mean unseen land round the body: a look at the cells alone leaves the planner touring it
    (await (look/survey! c :near (look/glance-effort) (when-not (seq (remove #(occluded? c %) unseen)) {:cells cells})))
    (await (sense/look-at-unknown! c (u/nearest-first (u/self-pos c) identity unseen) look-ahead-steps))
    (let [hidden (filter #(and (sense/unknown? p %) (occluded? c %)) cells)
          solid (filter #(true? (some-> (u/sensed p (zipmap [:x :y :z] %)) .-fullCube)) cells)]
      (doseq [[x y z] (concat hidden solid)] (.add ^js (.-blocked ^js (:unknown c)) (str x "," y "," z)))
      (if-let [cell (first (concat solid hidden))]
        (let [{:keys [x y z]} (u/self-pos c)]
          {:end {:status :replan :why :changed :learned true :cells [cell] :at [x y z] :step 1}})
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
  {:status :replan :why :changed}; so does a leg that starts with a full block now seen in the next steps' cells (solid-ahead)."
  ([c steps timeout-s] (walk! c steps timeout-s nil))
  ([c steps timeout-s watch]
   (loop [steps steps base 0 ms 0 looked #{}]
     (let [p (:primitives c)
           u (when (:unknown c) (first-unseen-step p steps looked))
           o (first-unseen-overhead p steps looked)
           d (first-unseen-landing p steps looked)
           j (let [js (remove nil? [u o d])] (when (seq js) (apply min js)))
           rebase (fn [d] (cond-> d (:step d) (update :step + base)))
           solid (when (:unknown c) (solid-ahead p steps))]
       (cond
         solid (let [{:keys [x y z]} (u/self-pos c)]
                 [(rebase {:status :replan :why :changed :learned true :cells [solid] :at [x y z] :step 1}) ms])

         (and (or (pos? base) (seq looked)) (wplan/swim-outlasts-air? c steps))
         (let [pos (.-pos (.self p))]
           [(rebase {:status :replan :why :air :at [(.-x pos) (.-y pos) (.-z pos)] :step 0}) ms])

         (and (= 1 j) (= 1 u)) (let [r (await (look-ahead! c steps))]
                                 (if-let [end (:end r)]
                                   [(rebase end) ms]
                                   (recur steps base ms (into looked (:looked r)))))
         (and (= 1 j) (= 1 d)) (let [r (await (look-landing! c steps))]
                                 (if-let [end (:end r)]
                                   [(rebase end) ms]
                                   (recur steps base ms (into looked (:looked r)))))
         (= 1 j) (let [cell (unseen-overhead p (nth steps 1))]
                   (await (sense/look-at! c cell))
                   (recur steps base ms (conj looked cell)))
         (nil? j) (let [[done wms] (await (walk-lit! c steps (max 1 (- timeout-s (/ ms 1000))) watch))]
                    [(rebase done) (+ ms wms)])
         :else (let [[done wms] (await (walk-lit! c (subvec steps 0 j) (max 1 (- timeout-s (/ ms 1000))) watch))
                     ms (+ ms wms)]
                 (if (= :arrived (:status done))
                   (recur (subvec steps (dec j)) (+ base (dec j)) ms looked)
                   [(rebase done) ms])))))))
