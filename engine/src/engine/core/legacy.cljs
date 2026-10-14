(ns engine.core.legacy
  "The adapter between old jobs (check; a round of :done, :continue or :declined; result!) and contract jobs while both
  run: an old job called through ctx/call ends with an ending, a contract job called through call-child answers as an
  old one, and a contract job's legacy-args export maps an old caller's options to call keys."
  (:require [engine.contract :as contract]))

(def reason-kinds
  "The kind of ending an old job's :stopped reason reads as; any other reason is :nothing-possible. Reasons that
  name a missing target or fact, or a danger present, are condition-failed; hunger is no limit line."
  (merge (zipmap [:bad-args :refused :need :unreachable :no-tool :not-loaded :zone :no-zones :hazard :not-allowed
                  :inventory-full :bag-full :fluid :fluid-adjacent :cannot :bad-pos :unsupported :no-chest :no-bed
                  :unarmed :declined :not-ready :hungry :danger :hostile-near
                  :gone :no-target :no-targets :no-source :no-tree :no-sapling :no-composter :no-boat :no-vehicle
                  :not-found :nothing-found :no-stone-found :no-food-found :no-drops :no-box :no-pen :no-home
                  :no-base :no-block :no-item :no-items :no-hostile :no-mob-or-box :not-mounted]
                 (repeat :condition-failed))
         (zipmap [:timeout :low-health :tool-spent] (repeat :limit-reached))))

(def short-waits
  "Wait reasons of an old job that mean the world is busy and the wait passes: under ctx/call such a decline after
  the child's first round is a :continue yield; any other decline is condition-failed."
  #{:cooking :night-sheltered :torches-cooking :chunks-loading})

(def reason-lines
  "The limit line ({:line ..}, as the contract gives it) of an old limit reason; :reason keeps what an old parent reads."
  {:timeout :time :low-health :health :tool-spent :wear})

(defn reason-cause [reason]
  (if-let [line (reason-lines reason)] {:line line :reason reason} reason))

(defn wait-cause
  "A declining check's wait reason ({:reason ...}) as a cause: the keyword alone when it says nothing more."
  [w]
  (if (= [:reason] (keys w)) (:reason w) w))

(defn ending-of
  "An old job's round as a ctx/call child: :continue, or the ending of its status (:done or :declined) with the
  result it handed over (result!) and, declined, the wait reason it passed up (or nil)."
  [status result wait]
  (case status
    :continue :continue
    :declined (contract/ending :condition-failed (if wait (wait-cause wait) :declined) nil)
    :done (let [data (dissoc result :status :reason)]
            (if (= :stopped (:status result))
              (contract/ending (get reason-kinds (:reason result) :nothing-possible) (reason-cause (:reason result)) data)
              (contract/ending :done nil data)))))

(defn result-of
  "The result an old parent reads (child-result) of a contract child's ending e."
  [e]
  (if (= :done (:kind e))
    (assoc (:data e) :status :done)
    (assoc (:data e) :status :stopped :reason (contract/cause-key e) :ending e)))

(defn given-args
  "args as the job of registry entry takes them: an old caller's options mapped by its legacy-args export."
  [entry args]
  (if-let [f (:legacy-args entry)] (f args) args))
