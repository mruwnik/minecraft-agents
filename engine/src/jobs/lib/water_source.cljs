(ns jobs.lib.water-source
  "The seen water sources of a body, cheapest first."
  (:require [jobs.lib.cost.ways :as ways]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]))

(defn source-s
  "Seconds a bucket takes at a source: the walk to it and a click (ways/place-s). When its top is covered the first click
  is caught by the pad: the body steps to the next face (a walk of one block) and clicks again."
  [from {:keys [pos covered?]}]
  (+ (pick/walk-s from {:pos pos :range 0})
     (ways/place-s)
     (if covered?
       (+ (pick/walk-s pos {:pos (update pos :x inc) :range 0}) (ways/place-s))
       0)))

(defn seen-sources
  "The seen water sources within radius (jobs.lib.look/seen-water-sources), cheapest first by source-s from the body."
  [c radius]
  (let [from (u/self-pos c)]
    (vec (sort-by #(source-s from %) (look/seen-water-sources (:primitives c) radius)))))
