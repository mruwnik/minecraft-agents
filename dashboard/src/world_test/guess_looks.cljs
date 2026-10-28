(ns world-test.guess-looks
  "tools/event-counts.mjs guess-looks: counts the measured prior behind jobs.movement.go-to.escalation/prior-good-looks from the
  bodies' logs alone (no fold): each :go-to.guess-look event is a cell a dig route guessed as rock, of class :beside or :apart,
  looked at; k of n looks per class showed it not diggable (bad). Pure parts: scan-line, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn scan-line
  "One log line -> {:class :beside|:apart :ok? bool} when it is a guess look, else nil."
  [line]
  (when (str/includes? line ":kind :go-to.guess-look")
    (let [{:keys [class ok?]} (:data (reader/read-string line))]
      (when class {:class class :ok? (boolean ok?)}))))

(defn report
  "{class {:k bad :n looks}} of the scanned looks."
  [looks]
  (into (sorted-map)
        (map (fn [[class ls]] [class {:k (count (remove :ok? ls)) :n (count ls)}]))
        (group-by :class looks)))
