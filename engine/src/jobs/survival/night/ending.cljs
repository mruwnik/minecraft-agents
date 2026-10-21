(ns jobs.survival.night.ending
  "How a night ends: the result it carries (how it sheltered, what it placed, the flees), its cleanup, and the exposed ending."
  (:require [engine.ctx :as ctx]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.night.sites :as sites]))

(defn fled
  "Walks to somewhere safer this night made, or nil when none."
  [c]
  (let [n (:flees (ctx/mem c))]
    (when (and n (pos? n)) n)))

(defn spent-fields
  "What the night's children placed (the niche), as the :spent, :spent-at and :text a done ending carries; {} when nothing."
  [c]
  (let [log (spent/log c)]
    (if log (assoc (spent/summary log) :text (spent/text log "night shelter")) {})))

(defn done-data
  "The data of a done night: how it sheltered, what it placed, the flees."
  [c how]
  (cond-> (spent-fields c) how (assoc :night how) (fled c) (assoc :fled (fled c))))

(defn forget-night!
  "The night's cleanup: tonight's failed sites (sites/forget-failed!) are forgotten unless the round ends exposed in the dark: those are kept so the next firing does not dig them again."
  [c]
  (when-not (and (= :exposed (:sheltered (ctx/mem c))) (sh-sense/dark-outside? (:primitives c)))
    (sites/forget-failed! c)))

(defn end-exposed!
  "Nothing sheltered the body tonight: end nothing-possible :exposed {:sites}."
  [c]
  (let [failed (pit-site/failed c)]
    (ctx/end c :nothing-possible :exposed
             (cond-> {:sites failed
                      :text (str "no shelter tonight: no bed, nobody asleep, no site to dig in, nowhere safer known (" (sites/exposed-why failed (:no-way-text (ctx/mem c))) ")")}
               (fled c) (assoc :fled (fled c))))))
