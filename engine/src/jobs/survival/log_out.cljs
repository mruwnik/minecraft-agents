(ns jobs.survival.log-out
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]))

(def doc
  "Leave the server for a while so another sleeping player can skip the night,
  then come back. Fired by the player-sleeping-nearby reflex. Check: it is night, no :bed lies within :bed-radius, :offline-allowed
  is true, the last log-out was not unsupported, and another player within
  :player-radius is sleeping. Round: the offline primitive for :ms (the
  primitive caps it at ten minutes; default twenty seconds), then done; a :log-out entry {:ms :status}
  (cap 10, kept one in-game day) is written. If the night is not over on
  return and the player still sleeps, the next firing of the reflex logs out again.")

(def args
  {:bed-radius {:doc "a remembered bed within this many blocks counts as usable, so the body sleeps instead"
                :default sh/default-bed-radius}
   :offline-allowed {:doc "false forbids logging out" :default true}
   :offline-ms {:doc "how long to stay away; the offline primitive caps it at ten minutes" :default 20000}
   :player-radius {:doc "sleeping players within this many blocks count (what the body can see)" :default 128}})

(def log-out-policy {:cap 10 :ttl sh/ms-per-day})

(defn check [c]
  (sh/log-out-wanted? (:primitives c) (ctx/view c) (:args c)))

(defn ^:async round [c]
  (let [r (await (ctx/act c :offline #js {:ms (:offline-ms (:args c))}))]
    (ctx/remember! c :log-out {:ms (.-ms r) :status (.-status r)} log-out-policy)
    :done))
