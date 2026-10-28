(ns jobs.lib.walk.gaps
  "The outcome of each gap jump a walk makes, for the miss rate event-counts gap-miss reads: one :gap-jump event per gap step."
  (:require [engine.path.executor :as executor]))

(defn event
  "The :gap-jump data of gap step g of steps ended out (:landed, :fell or :stuck), sprint whether the body sprinted on the way."
  [steps g sprint out]
  (let [prev (nth steps (dec g)) step (nth steps g)]
    {:width (first (executor/gap-cells prev step))
     :dy (js/Math.round (- (executor/stand-y step) (executor/stand-y prev)))
     :sprint (boolean sprint)
     :from [(:x prev) (:y prev) (:z prev)] :to [(:x step) (:y step) (:z step)]
     :out out}))

(defn outcome
  "How the jump to gap step g ended when the executor, now aimed at step i, ended the tick with done (nil: still walking), else nil."
  [g i done]
  (cond
    (> i g) :landed
    (and (= :arrived (:status done)) (= i g)) :landed
    (not= g (:step done)) nil
    (= :off-plan (:status done)) :fell
    (= :stuck (:status done)) :stuck))

(defn tick
  "[tracked' data] after one tick of a walk of steps with the executor aimed at step i, now at i2, holding controls (done: the walk's
  end, if it ended). tracked {:g :sprint} is the gap step being jumped, nil outside one; data is the :gap-jump event to emit when this
  tick ended the jump, else nil."
  [steps tracked i2 controls done]
  (let [out (when-let [g (:g tracked)] (outcome g i2 done))
        now (when (and (not= i2 (when out (:g tracked))) (= :gap (:move (nth steps i2 nil))) (pos? i2))
              {:g i2 :sprint (or (and (= i2 (:g tracked)) (:sprint tracked)) (boolean (:sprint controls)))})]
    [now (when out (event steps (:g tracked) (:sprint tracked) out))]))
