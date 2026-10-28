(ns world-test.unseen-floors
  "tools/event-counts.mjs unseen-floors: counts the prior behind jobs.lib.cost.value/::unseen-floor-counts from the bodies'
  logs alone (no fold): each :walk.unseen-floor event is a floor a walk planned on without having seen it, looked at; it
  was :present, :absent or :missed (still unseen once looked at, counted absent: the walk turned back). Pure parts:
  scan-line, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn scan-line
  "One log line -> :present, :absent or :missed when it is an unseen floor looked at, else nil."
  [line]
  (when (str/includes? line ":kind :walk.unseen-floor")
    (#{:present :absent :missed} (:floor (:data (reader/read-string line))))))

(defn report
  "{:absent :present} of the scanned floors, missed ones absent."
  [floors]
  {:absent (count (remove #{:present} floors)) :present (count (filter #{:present} floors))})
