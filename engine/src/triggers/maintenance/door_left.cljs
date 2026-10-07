(ns triggers.maintenance.door-left
  "The door-left trigger (the reading is jobs.lib.doors).

  It holds while a block a walk left open lies within :radius (16) of the body and the body is out of its column; its
  job (jobs.maintenance.shut-doors) walks back and shuts it (jobs tidy up after themselves)."
  (:require [jobs.lib.doors :as doors]))

(def defaults doors/defaults)

(defn door-left
  "Holds when doors/holds? says so; args :radius :open-s. The job it starts shuts the blocks."
  [world view args _kn]
  (doors/holds? world view args))
