(ns jobs.survival.night.light
  "The night lights a sealed room that only its dark keeps from being shelter (jobs.build.light-area, called not copied)."
  (:require [engine.ctx :as ctx]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.torch :as torch]
            [jobs.survival.night.hold :as hold]))

(defn wanted?
  "Once a night: the body stands in a sealed room whose only fault is a dark spawnable cell beyond the spawn-free radius, and
  holds torches or coal to make them. Torches cost less than walls or a pit, so they come first."
  [c]
  (let [p (:primitives c)]
    (and (not (:light-tried (ctx/mem c)))
         (or (pos? (torch/torches-held p)) (pos? (torch/fuel-held p)))
         (sh-inside/dark-room-at? p (sh-sense/feet p)))))

(defn ^:async light!
  "jobs.build.light-area over the room's cells, called again while it goes on; once it ends, :light-tried, whatever came of it
  (the next pass finds the room safe, or digs in as before)."
  [c]
  (hold/busy! c)
  (let [p (:primitives c)
        cells (:cells (reach-room/room p (sh-sense/feet p)))
        d (await (ctx/call c :light 'jobs.build.light-area {:cells cells}))]
    (when-not (= :continue d)
      (ctx/update-mem! c assoc :light-tried true))
    :again))
