(ns jobs.lib.walk.gaps
  "The outcome of each gap jump a walk makes, for the miss rate event-counts gap-miss reads: one :gap-jump event per gap step."
  (:require [engine.path.executor :as executor]))

(defn run-up
  "The run-up cells of gap step g of steps as the planner prices them (base/GAP-MISS-JUMPS): 2 when the step before the takeoff
  is a walk straight in the jump's direction, else 1 (the takeoff cell only)."
  [steps g]
  (let [pre (nth steps (- g 2) nil) prev (nth steps (dec g)) step (nth steps g)
        sign #(compare % 0)]
    (if (and pre (= :walk (:move prev))
             (= [(sign (- (:x step) (:x prev))) (sign (- (:z step) (:z prev)))]
                [(- (:x prev) (:x pre)) (- (:z prev) (:z pre))]))
      2
      1)))

(defn event
  "The :gap-jump data of gap step g of steps ended out (:landed, :fell or :stuck), sprint whether the body sprinted on the way."
  [steps g sprint out]
  (let [prev (nth steps (dec g)) step (nth steps g)]
    {:width (first (executor/gap-cells prev step)) :run (run-up steps g)
     :dy (js/Math.round (- (executor/stand-y step) (executor/stand-y prev)))
     :sprint (boolean sprint)
     :from [(:x prev) (:y prev) (:z prev)] :to [(:x step) (:y step) (:z step)]
     :out out}))

(defn below-gap?
  "Whether feet height y is under the gap step g's stand levels (take-off and landing) by over half a block: the body went into the hole."
  [steps g y]
  (let [level (min (executor/stand-y (nth steps (dec g))) (executor/stand-y (nth steps g)))]
    (< y (- level 0.5))))

(defn outcome
  "How the jump to gap step g ended when the executor, now aimed at step i, ended the tick with done (nil: still walking) and the
  feet at height y, else nil. :fell is the body ending below the gap (whatever ended the walk: off-plan, a replan from the hole); a walk
  that ends elsewhere, knocked or drifted off in the run-up, is no jump."
  [steps g i done y]
  (cond
    (> i g) :landed
    (and (= :arrived (:status done)) (= i g)) :landed
    (nil? done) nil
    (below-gap? steps g y) :fell
    (and (= :stuck (:status done)) (= g (:step done))) :stuck))

(defn tick
  "[tracked' data] after one tick of a walk of steps with the executor aimed at step i, now at i2, holding controls (done: the walk's
  end, if it ended) with the feet at height y. tracked {:g :sprint} is the gap step being jumped, nil outside one; data is the
  :gap-jump event to emit when this tick ended the jump, else nil."
  [steps tracked i2 controls done y]
  (let [out (when-let [g (:g tracked)] (outcome steps g i2 done y))
        now (when (and (not= i2 (when out (:g tracked))) (= :gap (:move (nth steps i2 nil))) (pos? i2))
              {:g i2 :sprint (or (and (= i2 (:g tracked)) (:sprint tracked)) (boolean (:sprint controls)))})]
    [now (when out (event steps (:g tracked) (:sprint tracked) out))]))
