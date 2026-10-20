(ns jobs.lib.access-tool
  "The pickaxe fetch of the digging access jobs (stair, tunnel): one place for the call and for how a failure reads."
  (:require [engine.contract :as contract]
            [jobs.lib.need :as need]))

(defn ^:async get!
  "Fetch what ({:block b} or {:kind k}) under limits (a :fetch limits map), the walk back to where the body stood left to
  need/return!. :continue while it works, :fetched once it ended :done, else the failed ending."
  [c what limits]
  (let [e (await (need/tool! c what (assoc (select-keys limits [:how :explore]) :return? true)))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :fetched
      :else e)))

(defn no-tool
  "The stop map of lack with a fetch that failed: :no-tool, the failed ending's reason as :because (nil: it cured nothing)."
  [lack e]
  (assoc lack :reason :no-tool :because (some-> e contract/cause-key)))
