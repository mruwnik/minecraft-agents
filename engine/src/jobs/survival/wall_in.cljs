(ns jobs.survival.wall-in
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.plug-way :as way]
            [jobs.survival.retreat-flight :as flight]))

(def doc
  "A plug way: fill the open sides at feet and head height, a roof support and the roof round the body with carried blocks, the side
  towards a shooter, else the nearest melee hostile, first; a cell with nothing to place against waits for the cell it rests on, or gets the support chain under it (dig-in-cells/bottom-up). An open iron door in a side cell (no hand shuts it, nothing is placed into it) makes the body step once to a neighbour cell not beside it first. Needs carried blocks (the call's :use :blocks, else every :build filler).
  One call is a whole attempt: :done {:pos :way :walls} once enclosed, at once when it already is; nothing-possible :failed when a
  hostile stands in a cell (not with :for :night), no block is left or a place is refused. A place refused for a mob in the cell is retried
  (dig-in-cells/mob-wait-ticks), then fails; one refused otherwise goes after the others and is given up after two refusals; a torch or sapling in a cell is dug once and placed over. Every block goes to the scaffold ledger with :for (default :danger, ledger :retreat-plug, taken
  back by jobs.access.cleanup once no hostile is near; :night: ledger :shelter, left alone). Quote: time, a placement per open cell
  (##Inf with no block carried). Picked by jobs.survival.shut-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:for {:doc "why the body shuts itself in: :danger (blocks are taken back by the cleanup once no hostile is near; a hostile in a cell fails it), :night (blocks stay for the night; a mob in a cell is waited out)" :spec #{:danger :night} :default :danger}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh-sense/default-roof-height}
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
        open (count (dig-cells/open-cells p (sh-sense/feet p)))]
    (way/quote-of c (if (lb/pick c (flight/blocks c)) (* open (settings/get settings ::place-s)) ##Inf)
                  (settings/get settings ::job-time-s))))

(def safe? ready/ok)
(def fit? ready/ok)

(defn ^:async round [c] (await (way/run! c :walls way/walls-step!)))
