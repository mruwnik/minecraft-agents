(ns engine.register
  "Rules shared by the engine and jobs for register entries kept in body memory (the reserve register)."
  (:require [clojure.string :as str]
            [engine.memory :as mem]))

(defn live?
  "Whether a register entry {:until :by} still counts in view: not past :until, and its :by job (an instance id, a child's
  \"root/slot\" included; nil and \"agent\" are always live) has memory: only the root has."
  [view {:keys [until by]}]
  (and (or (nil? until) (< (:now view) until))
       (or (nil? by) (= "agent" by) (boolean (seq (mem/entries view (mem/job-kind (first (str/split by #"/")))))))))
