(ns jobs.gather.mine.cause
  "Why the dig of jobs.gather.dig-for, or a stretch of its strip tunnel, cannot go on."
  (:require [jobs.gather.mine.targets :refer [soil-blocks]]
            [jobs.lib.breath :as breath]
            [jobs.lib.tools :as tools]
            [jobs.lib.trip :as trip]))

(defn stop-cause
  "Why the dig cannot go on before it has begun or between digs, as {:reason r :text t ..}, else nil: the head in water, hunger with
  no food carried (not for soil, dug at the surface near home), no usable tool carried. Each text says what is missing and that
  jobs.gather.mine gets it."
  [c]
  (let [p (:primitives c)
        block (:block (:args c))
        kind (tools/needed-kind p block)]
    (cond
      (breath/head-under? p) {:reason :flooded :text "dig-for stopped: the head is in water"}
      (and (not (contains? soil-blocks block)) (trip/food-short? c))
      {:reason :hungry :text "dig-for stopped: hungry with no food carried; food is needed before it digs on"}
      (not (tools/can-harvest? p block))
      {:reason :no-tool :tool kind :why :missing :text (str "dig-for stopped: no " kind " that harvests " block " is carried; jobs.gather.mine fetches one before it digs")}
      (trip/spent-tool? p block)
      {:reason :no-tool :tool kind :why :spent :text (str "dig-for stopped: every " kind " carried is nearly spent; jobs.gather.mine gets a spare before it digs")})))
