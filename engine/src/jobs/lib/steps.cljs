(ns jobs.lib.steps
  "The step planning the tend and maintain jobs share: a todo list of steps, each decided from facts read live
  ({:skip reason} or {:call {:slot :args}}), the one under way kept in memory as :call-args."
  (:require [engine.ctx :as ctx]))

(defn plan
  "Walk todo, skipping the steps that decide skips (booked in report unless it already holds an entry for the step);
  {:todo :report :call}, :call nil when none is left. decide is (decide step args facts)."
  [decide todo args facts report]
  (loop [todo todo report report]
    (if (empty? todo)
      {:todo [] :report report :call nil}
      (let [step (first todo)
            {:keys [skip call]} (decide step args facts)]
        (if skip
          (recur (rest todo) (if (contains? report step) report (assoc report step {:skipped skip})))
          {:todo (vec todo) :report report :call call})))))

(defn running-call
  "The call of the step a child has already started, from memory, else nil. jobs maps a step to its job."
  [jobs m]
  (when-let [call-args (:call-args m)]
    (let [step (first (:todo m))]
      {:slot step :job (jobs step) :args call-args})))

(defn next-plan
  "The step to run: the one under way, else the first that decide wants to call."
  [decide jobs c facts]
  (let [m (ctx/mem c)]
    (if-let [call (running-call jobs m)]
      {:todo (:todo m) :report (:report m) :call call}
      (plan decide (:todo m) (:args c) (facts c) (:report m)))))
