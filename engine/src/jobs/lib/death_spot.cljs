(ns jobs.lib.death-spot
  "Where the body died lately, by any cause, from the :deaths memory (engine.core/record-death!): a place a player avoids or goes back to
  prepared. A death weighs 1 when fresh and fades to 0 (jobs.lib.cost/death-weight); it counts for cells within ::radius of its spot."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::radius {:default 24 :doc "Blocks around a death spot that the death still counts for." :spec (a/num-in 0 nil)}})

(defn mob-cause?
  "Whether `cause` (the killer's name, else a damage type like fall or lava) is a mob the danger code knows; any other is a death by terrain."
  [p cause]
  (boolean (and cause (or (contains? threat/mob-hit cause) (threat/hostile? (game/version-of p) {:name cause})))))

(defn recent
  "The deaths of c still remembered: [{:pos {:x :y :z} :cause :mob? :weight :age-ms}], weight falling from 1 to 0 with age, mob? by mob-cause?."
  [c]
  (let [now (ctx/now c)]
    (vec (for [{:keys [data]} (ctx/entries c :deaths)
               :let [age (- now (:death-t data))
                     w (cost/death-weight age)
                     {:keys [x y z]} (:pos data)]
               :when (and (pos? w) x)]
           {:pos {:x x :y y :z z} :cause (:cause data) :mob? (mob-cause? (:primitives c) (:cause data)) :weight w :age-ms age}))))

(defn near
  "The remembered deaths of `deaths` (from recent) within ::radius of cell `pos`."
  [deaths pos]
  (filterv #(<= (u/dist pos (:pos %)) (settings/get settings ::radius)) deaths))

(defn lit?
  "Whether the body has seen the cell at pos and senses it lit enough that no mob spawns there (shelter.sense/lit-here?)."
  [p pos]
  (boolean (and (sh-sense/cell-light p pos) (sh-sense/lit-here? p pos))))

(defn seen-clear?
  "Whether a mob death's spot is clear: looked at since the death (fresh sight beats the record) and lit, as a dark spot spawns the killer again.
  A death by terrain never clears by sight: the place itself killed."
  [p {:keys [pos age-ms mob?]}]
  (let [b (look/seen-block p pos)]
    (boolean (and mob? b (not (:unknown b)) (< (:age-ms b) age-ms) (lit? p pos)))))

(defn uncleared
  "The remembered deaths of c not seen-clear?."
  [c]
  (remove #(seen-clear? (:primitives c) %) (recent c)))
