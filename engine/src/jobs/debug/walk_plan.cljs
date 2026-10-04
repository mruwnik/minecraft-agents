(ns jobs.debug.walk-plan
  (:require [clojure.set :as set]
            [engine.ctx :as ctx]
            [engine.path.walk :as walk]))

(def doc
  "Debug job: plan a path to :to with the path planner and follow it with the plan executor (steer), instead
  of moveTo. One round does the whole walk and ends :done. Plans from the body's cell within the executor's
  abilities (the planner is told executor/planner-limits, so it walks round gap jumps and doors it cannot do; it
  swims), still refuses a plan with a step the executor cannot walk (a backstop), walks a partial plan only up to its
  first step it cannot undo (a drop of more than a block, a gap jump down: :no-path :reason :one-way, :one-way {:kind :at},
  :near) and its last step out of water, re-plans when the body ends off the plan (at most 5 times), and hands over {:status ...}:
  :arrived, :refused (:kind :at), :no-path
  (:reason; :abilities with :kind :at when only a step the executor cannot do leads there), :stuck (:why :at),
  :gave-up (:reason :replan-limit), :failed (:reason) or :unsupported (no pathWorld), plus :replans. Emits
  :walk-plan.plan per plan, :walk-plan.replan, and :walk-plan.result (:kind as :refused-kind) with :ms, :walked
  (blocks of every plan followed), :walk-ms (time inside steer acts) and, only when arrived, :blocks-per-s
  (walked over walk-ms). A cut (manual takeover, a reflex) releases every control at once; the round
  rejects and a resumed round plans afresh from where the body stands.")

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
