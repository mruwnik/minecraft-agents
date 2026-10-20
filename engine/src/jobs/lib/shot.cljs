(ns jobs.lib.shot
  "Whether the body is under fire from a shooter it has not seen: a recent shot (flight/shot-spots) whose line of fire reaches its feet."
  (:require [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.retreat-cover :as cover]
            [jobs.survival.retreat-flight :as flight]))

(defn open?
  "Whether a shot within retreat-flight/::shot-window-s came from a spot (flight/shot-spots) with a line of fire to the body's
  feet, as the hostile-near trigger and the retreat's dodge step read it. p: primitives, view: the memory view."
  [p view]
  (let [spots (flight/view-spots p view)
        covered? (cover/cover-fn p [] spots)]
    (boolean (and covered? (not (covered? (sh-sense/feet p)))))))
