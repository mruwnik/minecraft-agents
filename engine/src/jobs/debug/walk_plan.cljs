(ns jobs.debug.walk-plan
  (:require [clojure.set :as set]
            [engine.ctx :as ctx]
            [engine.path.walk :as walk]))

(def doc
  "Debug job: plan a path to :to with the path planner and follow it with the plan executor (steer), instead of
  moveTo. One round does the whole walk and ends :done.
  The planner is told what the executor can do (engine.path.executor/planner-limits), so it walks round gap jumps
  and doors it cannot do, and it swims. A plan with a step the executor cannot walk is still refused (a
  backstop). A partial plan ends at the node nearest the goal that the body can come back from, walked up to its
  last step out of water. When a nearer node lies behind a step it cannot undo (a drop of more than a block, a
  gap jump down), it ends there: :no-path with :reason :one-way. If the body ends off the plan it re-plans, at
  most 5 times.
  Result {:status ...}: :arrived, :refused (:kind :at), :no-path (:reason; :abilities with :kind :at when only a
  step the executor cannot do leads there), :stuck (:why :at), :gave-up (:reason :replan-limit), :failed
  (:reason), :unsupported (no pathWorld), :bad-args. Plus :replans.
  Events: :walk-plan.plan per plan, :walk-plan.replan and :walk-plan.result. The result has :ms, :walked (blocks
  of every plan followed), :walk-ms (time inside steer acts) and, only when arrived, :blocks-per-s.
  A cut (manual takeover, a reflex) releases every control at once. A resumed round plans afresh from where the
  body stands.")

(def args
  {:to {:doc "goal cell [x y z]" :default nil}
   :range {:doc "planner goal range (0: that cell)" :default 0}
   :timeout-s {:doc "bound of one walk (one plan followed), at most 120" :default 60}
   :weight {:doc "planner heuristic weight (policy: 1.2)" :default walk/default-weight}})

(defn check [_c] true)

(defn finish!
  "Hand result over (ctx/result!), emit it as a :walk-plan.result event (info when arrived, else warn)."
  [c result t0 walked walk-ms]
  (let [ms (- (js/Date.now) t0)
        full (cond-> (assoc result :ms ms :walked walked :walk-ms walk-ms)
               (= :arrived (:status result))
               (assoc :blocks-per-s (/ (js/Math.round (* 100 (/ walked (max 0.001 (/ walk-ms 1000))))) 100)))]
    (ctx/result! c result)
    ;; an event's :kind is its own: the refused step kind goes as :refused-kind
    (ctx/emit! c :walk-plan.result (if (= :arrived (:status result)) :info :warn)
               (-> full
                   (set/rename-keys {:kind :refused-kind})
                   (assoc :text (str "walk " (name (:status result)) " in " ms " ms"))))
    :done))

(defn ^:async round [c]
  (let [{:keys [to range timeout-s weight]} (:args c)
        t0 (js/Date.now)
        announce! (fn [kind data]
                    (ctx/emit! c (case kind :plan :walk-plan.plan :replan :walk-plan.replan) :info data))]
    (cond
      (nil? (walk/path-world (:primitives c))) (finish! c {:status :unsupported} t0 0 0)
      (not= 3 (count to)) (finish! c {:status :bad-args :reason "to must be [x y z]"} t0 0 0)
      :else
      (let [{:keys [result walked walk-ms]}
            (await (walk/walk-to! c {:to to :range range :weight weight :timeout-s timeout-s :announce! announce!}))]
        (finish! c result t0 walked walk-ms)))))
