(ns engine.register
  "Rules shared by the engine and jobs for register entries kept in body memory (the reserve register)."
  (:require [clojure.string :as str]
            [engine.memory :as mem]))

(defn frame-live?
  "Whether the job instance id (\"root\" or a child's \"root/slot/slot\") has a frame in memory: the root has memory, and
  every slot below it is still in its parent's :children (a child's frame goes with its ending)."
  [view id]
  (let [[root & slots] (str/split id #"/")
        step (fn [m slot] (some (fn [[k v]] (when (= slot (name k)) v)) (:children m)))]
    (and (boolean (seq (mem/entries view (mem/job-kind root))))
         (boolean (reduce (fn [m slot] (or (step m slot) (reduced nil))) (mem/job-mem view root []) slots)))))

(defn live?
  "Whether a register entry {:until :by} still counts in view: not past :until, and its :by job (an instance id, a
  child's \"root/slot\" included; nil and \"agent\" are always live) has a live frame."
  [view {:keys [until by]}]
  (and (or (nil? until) (< (:now view) until))
       (or (nil? by) (= "agent" by) (frame-live? view by))))
