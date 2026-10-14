(ns jobs.lib.causes
  "The cause keywords endings share (docs/goals-and-actions.md 2.6), by kind. Data only; a job may use a cause of its
  own where none fits.")

(def condition-failed
  "A fact that does not hold, at the start or midway."
  #{:bad-args :unreachable :out-of-reach :not-allowed :hazard :not-loaded :need :tool :no-tool :free-slots
    :inventory-full :duplicate-call :night-sheltered :target-gone})

(def limit-reached
  "The line a limit crossed."
  #{:time :deadline :health})

(def nothing-possible
  "Why no way worked."
  #{:least :unreachable :out-of-reach :failed})

(def all (into #{} cat [condition-failed limit-reached nothing-possible]))
