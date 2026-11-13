(ns engine.register
  "Rules shared by the engine and jobs for register entries kept in body memory (the reserve register).")

(defn live?
  "Whether a register entry {:until :by} still counts in view: not past :until. No :until counts only for an entry nobody
  holds (no :by) or the agent's (:by \"agent\"); a job's entry without one predates stamping and is an orphan. Whether the
  job that wrote it still runs is never asked: a writer stamps :until and renews it while it runs."
  [view {:keys [until by]}]
  (if until
    (< (:now view) until)
    (or (nil? by) (= "agent" by))))
