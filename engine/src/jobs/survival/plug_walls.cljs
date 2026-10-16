(ns jobs.survival.plug-walls
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [jobs.lib.shelter :as sh]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.plug-way :as way]
            [jobs.survival.retreat-flight :as flight]))

(def doc
  "A plug way: fill the open sides at feet and head height, a roof support and the roof round the body with carried blocks, the side
  towards a shooter, else the nearest melee hostile, first. Needs carried blocks (the call's :use :blocks, else every :build filler).
  One call is a whole attempt: :done {:pos :way :walls} once enclosed, at once when it already is; nothing-possible :failed when a
  hostile stands in a cell (not with :purpose :shelter), no block is left or a place is refused. A place refused for a mob in the cell is retried
  (dig-in-cells/mob-wait-ticks), then fails. Every block goes to the scaffold ledger with :purpose (default :retreat-plug, taken
  back by jobs.access.cleanup once no hostile is near; :shelter is left alone). Quote: time, a placement per open cell
  (##Inf with no block carried). Picked by jobs.survival.plug-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:purpose {:doc "ledger purpose of the placed blocks (:retreat-plug: taken back by the cleanup near no hostile; :shelter: left alone)" :spec #{:retreat-plug :shelter} :default :retreat-plug}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :radius {:doc "melee hostiles within this many blocks decide which side is walled first and stand in the way of a cell" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles within this many blocks decide which side is walled first" :spec (a/num-in 0 nil) :default 16}
   :max-places {:doc "placements per step" :spec (a/int-in 1 nil) :default 4}})

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds a walls attempt may take." :spec (a/num-in 0 nil)}
   ::place-s {:default 3 :doc "Seconds a placement is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done? [c] (way/done? c :walls))

(defn quote-of [c]
  (let [p (:primitives c)
        open (count (dig-cells/open-cells p (sh/feet p)))]
    (way/quote-of c (if (lb/pick c (flight/blocks c)) (* open (settings/get settings ::place-s)) ##Inf)
                  (settings/get settings ::job-time-s))))

(defn ^:async round [c] (await (way/run! c :walls way/walls-step!)))
