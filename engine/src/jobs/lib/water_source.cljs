(ns jobs.lib.water-source
  "The seen water sources of a body, cheapest first."
  (:require [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]))

(defn source-s
  "Seconds a bucket takes at a source: the walk to it, twice when its top is covered (the click from where the body
  stands is caught by the pad, so the body walks again to another side)."
  [from {:keys [pos covered?]}]
  (* (if covered? 2 1) (pick/walk-s from {:pos pos :range 0})))

(defn seen-sources
  "The seen water sources within radius (jobs.lib.look/seen-water-sources), cheapest first by source-s from the body."
  [c radius]
  (let [from (u/self-pos c)]
    (vec (sort-by #(source-s from %) (look/seen-water-sources (:primitives c) radius)))))
