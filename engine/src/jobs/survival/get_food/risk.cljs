(ns jobs.survival.get-food.risk
  "Whether a fragile body (jobs.lib.foods/fragile?: hurt, hungry, no food carried, so waiting cannot heal it) may walk out
  for food: it does not search unknown ground where the sky is dark above ground or a hostile is known near. Walks to known food
  are priced by go-to's own start check (all but 1 hp may be spent while the body cannot heal), not here."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.foods :as foods]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn uncalm
  "Why the body is not calm now, as text naming the dark cell with its light level and/or the nearest known hostile, else nil."
  [p c]
  (let [feet (sh-sense/feet p)
        dark (when (#{:later :look} (first (ready/dark-sky c)))
               (str "dark above ground at " (pr-str [(:x feet) (:y feet) (:z feet)]) ", light "
                    (or (sh-sense/light-at p feet) "unknown")))
        ds (dawn/known-dangers p)
        hostile (when (seq ds) (dawn/hostile-text (mapv #(dawn/describe p %) ds)))]
    (when (or dark hostile)
      (str/join "; " (remove nil? [dark hostile])))))

(defn calm-now?
  "Whether the sky is not dark above ground and no hostile is known near."
  [p c]
  (nil? (uncalm p c)))

(defn search-refusal
  "Why a fragile body does not search unknown ground now, as text, else nil."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    (when (and (foods/fragile? self) (not (calm-now? p c)))
      (str "health " (.toFixed (.-health self) 1) ": will not search unknown ground (" (uncalm p c) ")"))))

(defn search-refused?
  "search-refusal, kept in job memory (:too-dangerous) for the ending when nothing else feeds the body."
  [c]
  (when-let [why (search-refusal c)]
    (ctx/update-mem! c assoc :too-dangerous why)
    true))
