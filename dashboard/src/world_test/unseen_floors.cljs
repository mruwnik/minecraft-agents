(ns world-test.unseen-floors
  "tools/event-counts.mjs unseen-floors: counts the bodies' own outcomes beside the measured prior
  jobs.lib.cost.value/::unseen-floor-counts (world-test.floor-neighbours) from their logs alone (no fold): each
  :walk.unseen-floor event is a floor a walk planned on without having seen it, looked at, of a :step-kind
  (jobs.lib.walk.sight/floor-kind); it was :present, :absent or :missed (still unseen once looked at: counted neither way). Pure parts: scan-line, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn scan-line
  "One log line -> [kind floor] (floor :present, :absent or :missed) when it is an unseen floor looked at, else nil."
  [line]
  (when (str/includes? line ":kind :walk.unseen-floor")
    (let [{:keys [step-kind floor]} (:data (reader/read-string line))]
      (when (#{:present :absent :missed} floor) [step-kind floor]))))

(defn report
  "{kind {:absent :present}} of the scanned floors, missed ones left out."
  [floors]
  (into {} (for [[kind fs] (group-by first floors)
                 :let [fs (map second fs)]]
             [kind {:absent (count (filter #{:absent} fs)) :present (count (filter #{:present} fs))}])))
