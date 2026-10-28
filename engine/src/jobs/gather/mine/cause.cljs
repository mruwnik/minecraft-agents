(ns jobs.gather.mine.cause
  "Why the dig of jobs.gather.dig-for, or a stretch of its strip tunnel, cannot go on."
  (:require [engine.ctx :as ctx]
            [jobs.gather.mine.targets :refer [main-block]]
            [jobs.lib.breath :as breath]
            [jobs.lib.ready :as ready]
            [jobs.lib.tools :as tools]
            [jobs.lib.trip :as trip]))

(defn stop-cause
  "Why the dig cannot go on before it has begun or between digs, as {:reason r :text t ..}, else nil: the head in water, junk
  over the cap (:surplus, for a :shed caller; not while a dig's drops are being collected: a new call would not know them),
  no usable tool carried, a body hurt with only food able to heal it (:cannot-heal; jobs.items.get-kit gets food then), (a nearly spent one stops a dig unless it runs under a spare fetch: that dig makes the spare). Each text says what is missing and that
  jobs.gather.mine gets it."
  [c]
  (let [p (:primitives c)
        block (main-block (:args c))
        kind (tools/needed-kind p block)]
    (cond
      (breath/head-under? p) {:reason :flooded :text "dig-for stopped: the head is in water"}
      (and (:shed (:args c)) (not (:collecting (ctx/mem c))) (pos? (trip/surplus-slots c))) {:reason :surplus :text "dig-for stopped: the pack holds more junk than a dig needs; jobs.gather.mine throws it away"}
      (ready/hurt-cannot-heal? c :long) {:reason :cannot-heal :text "dig-for stopped: hurt, and only food heals the body; going deeper would risk it"}
      (not (tools/can-harvest? p block))
      {:reason :no-tool :tool kind :why :missing :text (str "dig-for stopped: no " kind " that harvests " block " is carried; jobs.gather.mine fetches one before it digs")}
      (and (trip/spent-tool? p block) (not (trip/in-spare? c block)))
      {:reason :no-tool :tool kind :why :spent :text (str "dig-for stopped: every " kind " carried is nearly spent; jobs.gather.mine gets a spare before it digs")})))
