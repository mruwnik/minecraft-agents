(ns triggers.movement.mounted
  "The mounted trigger: the body rides something no job means it to ride."
  (:require [jobs.movement.vehicle :as vehicle]))

(defn mounted
  "Holds when the body rides a vehicle or mob (self().vehicle) and no live job holds the vehicle
  (jobs.movement.vehicle/held?). A body ends up aboard by an interact slip, a log-in Paper puts back in a boat, or a
  riding job that was cut; walking does nothing aboard, so (jobs.movement.leave-vehicle) takes it off. Persistence
  :stop: a body that could not get off is not pulled at again until it was off once."
  [world memory _args]
  (and (vehicle/mounted? world) (not (vehicle/held? memory))))
