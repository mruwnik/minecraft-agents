(ns jobs.lib.shut-walk
  "What both kinds of jobs.maintenance.shut share: the walk into reach of an opening and the click tries."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]))

(a/defargs settings
  {::tries {:default 3 :doc "Failed clicks on one opening before it is given up or left." :spec (a/int-in 1 nil)}
   ::reach {:default 3 :doc "Walk until within this many cells of the opening (the click reaches 4.5 from the eye)." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn tries [] (settings/get settings ::tries))

(defn ^:async walk!
  "nil when the body stands within reach of pos, else why not: :continue (the walk goes on or waits) or :unreachable."
  [c pos ways]
  (let [e (await (child/call-walk* c :walk {:pos pos :range (reach) :doors :never :ways ways}))
        res (:data e)]
    (ctx/update-mem! c dissoc :blocker)
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (:arrived res)) nil
      :else (do (ctx/update-mem! c assoc :blocker (str (or (:hint res) (some-> (:why res) name) "no walk leads there")))
                :unreachable))))
