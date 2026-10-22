(ns jobs.gather.mine.cause
  "Why the dig of jobs.gather.dig-for, or a stretch of its strip tunnel, cannot go on."
  (:require [jobs.gather.mine.targets :refer [main-block]]
            [jobs.lib.breath :as breath]
            [jobs.lib.tools :as tools]
            [jobs.lib.trip :as trip]))

(defn stop-cause
  "Why the dig cannot go on before it has begun or between digs, as {:reason r :text t ..}, else nil: the head in water, no usable
  tool carried (a nearly spent one stops a dig unless it runs under a spare fetch: that dig makes the spare). Each text says what is missing and that
  jobs.gather.mine gets it."
  [c]
  (let [p (:primitives c)
        block (main-block (:args c))
        kind (tools/needed-kind p block)]
    (cond
      (breath/head-under? p) {:reason :flooded :text "dig-for stopped: the head is in water"}
      (not (tools/can-harvest? p block))
      {:reason :no-tool :tool kind :why :missing :text (str "dig-for stopped: no " kind " that harvests " block " is carried; jobs.gather.mine fetches one before it digs")}
      (and (trip/spent-tool? p block) (not (trip/in-spare? c)))
      {:reason :no-tool :tool kind :why :spent :text (str "dig-for stopped: every " kind " carried is nearly spent; jobs.gather.mine gets a spare before it digs")})))
