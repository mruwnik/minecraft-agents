(ns engine.register
  "Rules shared by the engine and jobs for register entries kept in body memory (the reserve register).")

(defn live?
  "Whether a register entry {:until} still counts in view: not past :until (nil: always). Whether the job that wrote it
  still runs is never asked: a writer stamps :until and renews it while it runs."
  [view {:keys [until]}]
  (or (nil? until) (< (:now view) until)))
