(ns engine.path.walk
  "The walk driver: plan from the body's cell to a goal within the executor's abilities, follow the plan with steer, plan
  again when the body ends off it. Jobs that walk (jobs.debug.walk-plan, the stair, tunnel and cleanup jobs) call this
  namespace; walk-to! is the whole loop, the other functions are its pieces."
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]))

(def max-timeout-s 120)
(def max-settle-waits 20)
(def default-weight 1.2)
(def held-in
  "Blocks at the feet that hold a body still enough to plan from: climbables, and water (it swims from there)."
  #{"ladder" "vine" "water"})

(defn path-world
  "The primitives' pathWorld sensing, nil when they have none or cannot sense now."
  [p]
  (when (fn? (.-pathWorld p))
    (.pathWorld p)))

(defn ^:async settle!
  "Wait (100 ms at a time, at most 20 waits) until the body stands on the ground, in a climbable or in water."
  [c]
  (let [p (:primitives c)
        grounded? (fn []
                    (let [s (.self p)
                          pos (.-pos s)
                          b (.blockAt p #js {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))})]
                      (or (.-onGround s) (contains? held-in (some-> b .-name)))))]
    (loop [n 0]
      (when (and (< n max-settle-waits) (not (grounded?)))
        (await (ctx/act c :wait #js {:ms 100}))
        (recur (inc n))))))

(defn path-length
  "Horizontal length of the steps' stand points, in blocks, to 1 decimal."
  [steps]
  (let [d (reduce + (map (fn [a b] (js/Math.hypot (- (:px b) (:px a)) (- (:pz b) (:pz a))))
                         steps (rest steps)))]
    (/ (js/Math.round (* 10 d)) 10)))

(defn pose-of [pose]
  {:x (.-x pose) :y (.-y pose) :z (.-z pose) :vy (.-vy pose) :on-ground (.-onGround pose)
   :on-climbable (.-onClimbable pose) :in-water (.-inWater pose) :collided (.-collided pose)})

(defn steer-args
  "The act args of one walk. decide is not enumerable: the engine's act wrapper writes (js->clj args) into
  an event, and a function there would break the event stream."
  [timeout-s decide]
  (doto (js-obj "timeoutS" (min max-timeout-s timeout-s))
    (js/Object.defineProperty "decide" #js {:value decide})))

(defn plan-from
  "Plan from the body's cell to the goal, within limits (the planner's options.limits, nil for none); the planner's JS
  result."
  [c pw to range weight limits]
  (let [pos (.-pos (.self (:primitives c)))
        [gx gy gz] to
        query #js {:from #js {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))
                              :px (.-x pos) :pz (.-z pos)}
                   :goal #js {:kind "near" :x gx :y gy :z gz :range range}}]
    (planner/plan (.-snapshot pw) query #js {:table (.-table pw) :space (.-space pw) :weight weight :limits limits})))

(defn solid-fn
  "solid? for executor/with-free-sides over a pathWorld."
  [pw]
  (let [snapshot (.-snapshot pw) tops (.-top (.-table pw))]
    (fn [x y z] (pos? (aget tops (.stateAt snapshot x y z))))))

(defn plan-steps
  "The executor's steps for a found plan r over pw: corner free sides, high corners and gap ceilings marked."
  [pw r]
  (let [solid? (solid-fn pw)]
    (executor/with-gap-ceilings
      executor/policy
      (executor/with-high-corners
        executor/policy
        (executor/with-free-sides (executor/steps-of (.-steps (.-path r))) solid?)
        solid?)
      solid?)))

(defn plan-within
  "Plan within the executor's abilities: {:r :steps} (steps nil when r has no path). When that finds no whole path but a
  search without the limits does, also :beyond, the executor's refusal of that path: no path within abilities, and the
  kind of step that would have made one."
  [c pw to range weight]
  (let [r (plan-from c pw to range weight (executor/planner-limits executor/policy (solid-fn pw)))
        within {:r r :steps (when (.-path r) (plan-steps pw r))}]
    (if (= "found" (.-status r))
      within
      (let [wide (plan-from c pw to range weight nil)]
        (cond-> within
          (= "found" (.-status wide)) (assoc :beyond (executor/refusal executor/policy (plan-steps pw wide))))))))

(defn dry-end
  "A partial plan up to its last step out of water: a walk that cannot reach the goal never leaves the body swimming (at a
  bank too high to climb out, say)."
  [steps]
  (let [k (last (keep-indexed (fn [i s] (when-not (:swim s) i)) steps))]
    (if k (subvec steps 0 (inc k)) [])))

(defn near-goal
  "How far, in blocks to 1 decimal, the step is from the goal cell to."
  [{:keys [x y z]} [gx gy gz]]
  (/ (js/Math.round (* 10 (js/Math.hypot (- gx x) (- gy y) (- gz z)))) 10))

(defn one-way-of
  "The planner's oneWay of a result as {:kind :at}: the first step on the way to a node nearer the goal that the body cannot undo
  (a drop of more than a block, a gap jump down), nil when there is none."
  [r]
  (when-let [^js ow (.-oneWay r)]
    {:kind (nth executor/move-names (.-move ow)) :at [(.-x ow) (.-y ow) (.-z ow)]}))

(defn stopped-one-way
  "The no-path result of a plan whose nearer end lies behind a step that cannot be undone: that step, and how near to the goal
  the walk got (from: the last step kept, or the start)."
  [r from to one-way]
  (assoc {:status :no-path :reason :one-way :planner (some-> (.-reason r) keyword)}
         :one-way one-way :near (near-goal from to)))

(defn body-cell
  "The cell the body stands in, as a step's {:x :y :z}."
  [c]
  (let [pos (.-pos (.self (:primitives c)))]
    {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))}))

(defn plan-walk
  "Plan the next walk from where the body stands: plan-within, and for a partial plan only the steps up to its last dry step
  (dry-end). The planner ends a partial plan at the nearest node the body can come back from; {:r :steps :beyond :status :stop}:
  stop is the no-path result for a plan with a nearer end behind a step that cannot be undone (the planner's oneWay,
  stopped-one-way), nil otherwise."
  [c pw to range weight]
  (let [{:keys [r steps beyond]} (plan-within c pw to range weight)
        status (.-status r)
        walked (if (= "partial" status) (dry-end steps) steps)
        one-way (one-way-of r)]
    {:r r :steps walked :beyond beyond :status status
     :stop (when one-way (stopped-one-way r (or (peek walked) (first steps) (body-cell c)) to one-way))}))

(defn no-walk
  "The result of a plan that is not walked, nil when it is: no path within abilities (:beyond), a plan cut at a one-way step
  with no step left, no path, a plan the executor refuses. replans goes in the result."
  [{:keys [r steps beyond status stop]} replans]
  (let [partial? (= "partial" status)]
    (cond
      beyond
      {:status :no-path :reason :abilities :kind (:kind beyond) :at (:at beyond) :replans replans}

      (and stop (< (count steps) 2))
      (assoc stop :replans replans)

      (or (= "none" status) (and partial? (< (count steps) 2)))
      {:status :no-path :reason (some-> (.-reason r) keyword) :replans replans}

      :else
      (some-> (executor/refusal executor/policy steps) (assoc :replans replans)))))

(defn ^:async walk!
  "Follow steps once. [result ms]: the executor's done map, or {:status :stuck ...} on a timeout,
  {:status :failed ...}; ms is the wall time of the steer act."
  [c steps timeout-s]
  (let [state (volatile! (executor/start steps 0))
        last-done (volatile! nil)
        decide (fn [pose]
                 (let [{:keys [state' done controls yaw pitch]}
                       (let [r (executor/tick executor/policy @state (pose-of pose))]
                         {:state' (:state r) :done (:done r) :controls (:controls r) :yaw (:yaw r) :pitch (:pitch r)})]
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
     ms]))

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

      :else {:status :off-plan :at (:at done) :step (dec (count steps))})))

(defn ^:async walk-to!
  "Walk the body to within range of the goal cell to ([x y z]): settle, plan, follow the plan once (at most timeout-s
  seconds), plan again when the body ends off it (at most 5 times). The caller has checked that path-world is there.
  {:result :walked :walk-ms}: result is {:status ...} as in the walk-plan job doc, with :replans; walked is the blocks of
  every plan followed, walk-ms the time inside steer acts. announce! is called (kind data) with :plan before each walk
  and :replan when the body is off its plan."
  [c {:keys [to range weight timeout-s announce!] :or {announce! (fn [_ _])}}]
  (let [p (:primitives c)
        end (fn [result walked walk-ms] {:result result :walked walked :walk-ms walk-ms})]
    (loop [replans 0 walked 0 walk-ms 0]
      (await (settle! c))
      (let [plan (plan-walk c (path-world p) to range weight)]
        (if-let [no (no-walk plan replans)]
          (end no walked walk-ms)
          (let [{:keys [r steps status stop]} plan]
            (announce! :plan {:steps (count steps) :summary (some-> (.-path r) .-summary js->clj)
                              :status status :ms (.-ms r) :replans replans
                              :text (str "plan " status ", " (count steps) " steps")})
            (let [[walk-result ms] (await (walk! c steps timeout-s))
                  walked (+ walked (path-length steps))
                  walk-ms (+ walk-ms ms)
                  done (partial-end walk-result status to range steps stop)
                  after (if (#{:arrived :off-plan :stuck} (:status done))
                          (executor/after-walk executor/policy replans done)
                          {:finish done})]
              (if-let [n (:replan after)]
                (do (announce! :replan {:at (:at done) :replans n :text (str "off the plan, re-plan " n)})
                    (recur n walked walk-ms))
                (end (assoc (:finish after) :replans replans) walked walk-ms)))))))))
