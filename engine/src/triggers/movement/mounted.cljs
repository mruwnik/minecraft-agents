(ns triggers.movement.mounted
  "The mounted trigger: the body rides something with no job listed to ride it."
  (:require [jobs.lib.vehicle :as vehicle]))

(defn mounted
  "Holds when the body rides a vehicle or mob (self().vehicle) and no job is listed (live): a riding job never hands
  its ride to another and steps off when it ends, so a ride with nothing listed is no job's. A body ends up so by an
  interact slip, a log-in Paper puts back in a boat, or a riding job that ended aboard; walking does nothing aboard, so
  (jobs.movement.leave-vehicle) takes it off. Persistence :cooldown 30 s: a body that could not get off is tried again
  after the cooldown."
  [world _view _args _kn live]
  (and (vehicle/mounted? world) (empty? live)))
