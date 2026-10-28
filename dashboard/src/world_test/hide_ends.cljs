(ns world-test.hide-ends
  "tools/event-counts.mjs hide-ends: counts the measured prior behind jobs.lib.cost.answer ::hide-stays from the trial bodies'
  logs alone (no fold): the flights that ended hidden in a refuge (respond-to-hostile done :ended :hidden), k of them handing
  back with a danger still about (:hold :spent, the hide outlasted; dug out or not) of n (with :hold :quiet: it left).
  Pure parts: scan-line, report."
  (:require [clojure.string :as str]))

(defn scan-line
  "One log line -> :spent or :quiet when it ends a flight hidden, else nil."
  [line]
  (when (and (str/includes? line "respond-to-hostile: done") (str/includes? line ":ended :hidden"))
    (some->> (re-find #":hold :(spent|quiet)\b" line) second keyword)))

(defn report
  "{:k spent :n hides} of the scanned ends."
  [ends]
  {:k (count (filter #{:spent} ends)) :n (count ends)})
