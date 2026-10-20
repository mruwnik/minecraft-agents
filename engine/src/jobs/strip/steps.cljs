(ns jobs.strip.steps
  "The child calls the strip jobs (site, base, smelt, haul, all) share: run children in order, wait out the night and the light."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.waits :as waits]))

(def passed-kinds "Child ending kinds a :child-failed stop ends with as its own." #{:condition-failed :limit-reached :impossible})

(defn reason-of
  "The reason a child's ending reads as: its cause (a string for the smelt's), a map cause's key."
  [e]
  (if (map? (:cause e)) (contract/cause-key e) (or (:cause e) (:kind e))))

(defn wait-night
  "e, a child's ending, but :continue (the world is waited out, as a player waits for day to set the spawn or for light to
  walk to a chest) when it ended condition-failed :night or for a reason of waits/wait-reasons."
  [c e]
  (if (and (= :condition-failed (:kind e)) (= :night (:reason (:cause e))))
    (do (some-> (:child-wait c) (reset! (:cause e))) :continue)
    (waits/waiting-out c e)))

(defn ^:async call!
  "The ending of one child, or :continue while it waits."
  [c slot job args]
  (wait-night c (await (ctx/call c slot job args))))

(defn ^:async run-all!
  "The steps [[slot job args] ..] in order: nil when all ended :done, :continue while one waits (also for the night), else {:child job :ending e} of the first that did not."
  [c steps]
  (loop [[[slot job args] & more] steps]
    (if-not slot
      nil
      (let [e (await (call! c slot job args))]
        (cond (= :continue e) :continue
              (not= :done (:kind e)) {:child (str job) :ending e}
              :else (recur more))))))

(defn child-fields
  "{:reason :child :child-kind :because} for a failed run-all! result f."
  [f]
  (let [e (:ending f)]
    {:reason :child-failed :child (:child f) :because (reason-of e) :child-kind (:kind e)}))

(defn end-failed
  "End the call with the failed child's kind when it is a passed kind, else nothing-possible; the cause :child-failed, data fields."
  [c fields]
  (let [{:keys [child-kind]} fields]
    (ctx/end c (if (passed-kinds child-kind) child-kind :nothing-possible) :child-failed (dissoc fields :reason :child-kind))))
