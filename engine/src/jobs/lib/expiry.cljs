(ns jobs.lib.expiry
  "Common-memory records any job reads, that stop counting at a time: the record's data carries :until (ms, the memory
  clock: ctx/now, a view's :now). A writer stamps it; every reader asks current?. Whether the job that wrote it still
  runs is never asked.")

(defn stamp
  "data counting for ttl-ms from now."
  [data now ttl-ms]
  (assoc data :until (+ now ttl-ms)))

(defn current?
  "Whether data still counts at now; one with no :until never does."
  [now data]
  (boolean (some-> (:until data) (> now))))
