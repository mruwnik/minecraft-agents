(ns jobs.survival.log-out
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]))

(def doc
  "Leave the server for a while, then come back.
  Used by the player-sleeping-nearby reflex (:others :asleep-nearby, :offline-ms 20000),
  so another sleeping player can skip the night, and by the night shelter (:others :online),
  to be away until morning instead of digging in.
  Declines unless all hold: it is night, no :bed lies within :bed-radius, :offline-allowed is true,
  the last log-out was not unsupported, and :others holds.
  :asleep-nearby means another player within :player-radius is sleeping. :online means another player is in the player list.
  Stays away :offline-ms, or when that is nil until morning (a night is about 9 minutes, under the primitive's ten-minute cap).
  Ends with the result {:status :ms}. Status is ok, cut, unsupported or closed.
  If the night is not over on return, the caller logs out again.
  Memory: writes :log-out {:ms :status} (cap 10, one in-game day).")

(def args
  {:bed-radius {:doc "a remembered bed within this many blocks counts as usable, so the body sleeps instead"
                :default sh/default-bed-radius}
   :offline-allowed {:doc "false forbids logging out" :default true}
   :offline-ms {:doc "how long to stay away; nil: until morning; the offline primitive caps it at ten minutes" :default nil}
   :others {:doc ":asleep-nearby (another player within :player-radius sleeps) or :online (another player is in the player list)"
            :default :asleep-nearby}
   :player-radius {:doc "sleeping players within this many blocks count (what the body can see)" :default 128}})

(def log-out-policy {:cap 10 :ttl sh/ms-per-day})

(defn check [c]
  (sh/log-out-wanted? (:primitives c) (ctx/view c) (:args c)))

(defn away-ms [c]
  (or (:offline-ms (:args c))
      (sh/ms-until-morning (.-timeOfDay (.self (:primitives c))))))

(defn why [c]
  (if (= :online (:others (:args c))) "logged-out-for-night" "logged-out-for-sleeping-player"))

(defn ^:async round [c]
  (let [r (await (ctx/act c :offline #js {:ms (away-ms c) :why (why c)}))
        out {:ms (.-ms r) :status (.-status r)}]
    (ctx/remember! c :log-out out log-out-policy)
    (ctx/result! c out)
    :done))
