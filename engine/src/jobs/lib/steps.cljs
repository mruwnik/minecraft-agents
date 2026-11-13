(ns jobs.lib.steps
  "The step planning the tend and maintain jobs share: a todo list of steps, each decided from facts read live
  ({:skip reason} or {:call {:slot :args}}), the one under way kept in memory as :call-args."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]))

(def idle-causes
  "Causes of a step child's condition-failed ending that mean the step has nothing worth doing now, as a decided skip
  does: nothing ripe or due, a pass not worth its walk, animals in love or on their breeding cooldown."
  #{:nothing-to-do :not-worth :cooldown})

(defn idle-entry
  "The report entry {:skipped :declined :reason cause} of a step child's ending e that declined for an idle cause, else
  nil: the pass books it as a skip, not a failed step."
  [e]
  (let [cause (contract/cause-key e)]
    (when (and (= :condition-failed (:kind e)) (contains? idle-causes cause))
      {:skipped :declined :reason cause})))

(defn plan
  "Walk todo, skipping the steps that decide skips (booked in report, with the decision's :detail map, unless it already holds an entry for the step);
  {:todo :report :call}, :call nil when none is left. decide is (decide step args facts)."
  [decide todo args facts report]
  (loop [todo todo report report]
    (if (empty? todo)
      {:todo [] :report report :call nil}
      (let [step (first todo)
            {:keys [skip detail call]} (decide step args facts)]
        (if skip
          (recur (rest todo) (if (contains? report step) report (assoc report step (assoc detail :skipped skip))))
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

(defn warn-stopped!
  "One steps.stopped warn per step of report that stopped? says ended short of done: a pass that ends done went on past it,
  so the agent hears of it here."
  [c job report stopped?]
  (doseq [[step entry] report
          :when (stopped? entry)
          :let [reason (or (:reason entry) (:cause entry))]]
    (ctx/emit! c :steps.stopped :warn {:job job :step step :reason reason
                                       :text (str job ": the " (name step) " step stopped" (when reason (str ": " (name reason))))})))
