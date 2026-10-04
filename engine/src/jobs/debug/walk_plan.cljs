(ns jobs.debug.walk-plan
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]))

(def doc
  "Debug job: plan a path to :to with the path planner and follow it with the plan executor (steer), instead
  of moveTo. One round does the whole walk and ends :done. Plans from the body's cell, refuses plans with
  steps the executor cannot walk (gap jumps, doors, swimming), re-plans when the body ends off the plan (at
  most 5 times), and hands over {:status ...}: :arrived, :refused (:kind :at), :no-path (:reason),
  :stuck (:why :at), :gave-up (:reason :replan-limit), :failed (:reason) or :unsupported (no pathWorld),
  plus :replans. Emits :walk-plan.plan per plan, :walk-plan.replan, and :walk-plan.result with :ms, :walked
  (blocks of every plan followed), :walk-ms (time inside steer acts) and, only when arrived, :blocks-per-s
  (walked over walk-ms). A cut (manual takeover, a reflex) releases every control at once; the round
  rejects and a resumed round plans afresh from where the body stands.")

(def args
  {:to {:doc "goal cell [x y z]" :default nil}
   :range {:doc "planner goal range (0: that cell)" :default 0}
   :timeout-s {:doc "bound of one walk (one plan followed), at most 120" :default 60}
   :weight {:doc "planner heuristic weight (policy: 1.2)" :default 1.2}})

(def max-timeout-s 120)
(def max-settle-waits 20)
(def climbables #{"ladder" "vine"})

(defn check [_c] true)

(defn finish!
  "Hand result over (ctx/result!), emit it as a :walk-plan.result event (info when arrived, else warn)."
  [c result t0 walked walk-ms]
  (let [ms (- (js/Date.now) t0)
        full (cond-> (assoc result :ms ms :walked walked :walk-ms walk-ms)
               (= :arrived (:status result))
               (assoc :blocks-per-s (/ (js/Math.round (* 100 (/ walked (max 0.001 (/ walk-ms 1000))))) 100)))]
    (ctx/result! c result)
    (ctx/emit! c :walk-plan.result (if (= :arrived (:status result)) :info :warn)
               (assoc full :text (str "walk " (name (:status result)) " in " ms " ms")))
    :done))

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

(defn ^:async settle!
  "Wait (100 ms at a time, at most 20 waits) until the body stands on the ground or in a climbable."
  [c]
  (let [p (:primitives c)
        grounded? (fn []
                    (let [s (.self p)
                          pos (.-pos s)
                          b (.blockAt p #js {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))})]
                      (or (.-onGround s) (contains? climbables (some-> b .-name)))))]
    (loop [n 0]
      (when (and (< n max-settle-waits) (not (grounded?)))
        (await (ctx/act c :wait #js {:ms 100}))
        (recur (inc n))))))

(defn path-world
  "The primitives' pathWorld sensing, nil when they have none or cannot sense now."
  [p]
  (when (fn? (.-pathWorld p))
    (.pathWorld p)))

(defn plan-from
  "Plan from the body's cell to the goal; the planner's JS result."
  [c pw to range weight]
  (let [pos (.-pos (.self (:primitives c)))
        [gx gy gz] to
        query #js {:from #js {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))
                              :px (.-x pos) :pz (.-z pos)}
                   :goal #js {:kind "near" :x gx :y gy :z gz :range range}}]
    (planner/plan (.-snapshot pw) query #js {:table (.-table pw) :space (.-space pw) :weight weight})))

(defn solid-fn
  "solid? for executor/with-free-sides over a pathWorld."
  [pw]
  (let [snapshot (.-snapshot pw) tops (.-top (.-table pw))]
    (fn [x y z] (pos? (aget tops (.stateAt snapshot x y z))))))

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
  "A partial plan that was walked to its end is not an arrival unless the body is in range of the goal."
  [done planner-status to range steps]
  (let [[x y z] (:at done)]
    (if (and (= :arrived (:status done)) (= "partial" planner-status)
             (not (u/within? {:x x :y y :z z} (zipmap [:x :y :z] to) range)))
      {:status :off-plan :at (:at done) :step (dec (count steps))}
      done)))

(defn ^:async round [c]
  (let [{:keys [to range timeout-s weight]} (:args c)
        p (:primitives c)
        t0 (js/Date.now)]
    (cond
      (nil? (path-world p)) (finish! c {:status :unsupported} t0 0 0)
      (not= 3 (count to)) (finish! c {:status :bad-args :reason "to must be [x y z]"} t0 0 0)
      :else
      (loop [replans 0 walked 0 walk-ms 0]
        (await (settle! c))
        (let [pw (path-world p)
              r (plan-from c pw to range weight)
              planner-status (.-status r)]
          (if (= "none" planner-status)
            (finish! c {:status :no-path :reason (some-> (.-reason r) keyword) :replans replans} t0 walked walk-ms)
            (let [solid? (solid-fn pw)
                  steps (executor/with-gap-ceilings
                          executor/policy
                          (executor/with-free-sides (executor/steps-of (.-steps (.-path r))) solid?)
                          solid?)
                  plan-len (path-length steps)]
              (if-let [refused (executor/refusal executor/policy steps)]
                (finish! c (assoc refused :replans replans) t0 walked walk-ms)
                (do
                  (ctx/emit! c :walk-plan.plan :info
                             {:steps (count steps) :summary (some-> (.-path r) .-summary js->clj)
                              :status planner-status :ms (.-ms r) :replans replans
                              :text (str "plan " planner-status ", " (count steps) " steps")})
                  (let [[walk-result ms] (await (walk! c steps timeout-s))
                        walked (+ walked plan-len)
                        walk-ms (+ walk-ms ms)
                        done (partial-end walk-result planner-status to range steps)
                        after (if (#{:arrived :off-plan :stuck} (:status done))
                                (executor/after-walk executor/policy replans done)
                                {:finish done})]
                    (if-let [n (:replan after)]
                      (do (ctx/emit! c :walk-plan.replan :info {:at (:at done) :replans n
                                                                :text (str "off the plan, re-plan " n)})
                          (recur n walked walk-ms))
                      (finish! c (assoc (:finish after) :replans replans) t0 walked walk-ms))))))))))))
