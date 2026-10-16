(ns jobs.survival.plug-pocket
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.shelter :as sh]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.plug-way :as way]
            [jobs.survival.retreat-pit :as pit]))

(def doc
  "A plug way: dig a pocket beside the body (feet and head cell, solid on every other side), step in and wall the way in with the
  dug blocks. Needs a side that fits (solid rock the carried tools harvest, no fluid beside it) and a block to wall with, carried
  or dug. One call is a whole attempt: :done {:pos :way :pocket} once enclosed, at once when it already is; nothing-possible :failed
  with no pocket that fits, a dig that fails or a way in that cannot be walled. A pocket that lays open fluid or a cave is
  dropped (lava is filled first, :on-lava :seal). The walls go to the scaffold ledger with :purpose (default :retreat-plug, taken
  back by jobs.access.cleanup once no hostile is near; :shelter is left alone). Quote: time (##Inf when no side fits). Picked by
  jobs.survival.plug-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:purpose {:doc "ledger purpose of the placed blocks (:retreat-plug: taken back by the cleanup near no hostile; :shelter: left alone)" :spec #{:retreat-plug :shelter} :default :retreat-plug}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :radius {:doc "melee hostiles within this many blocks decide which side is walled first and stand in the way of a cell" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles within this many blocks decide which side is walled first" :spec (a/num-in 0 nil) :default 16}
   :max-places {:doc "placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pocket is dropped and the lava left" :spec #{:seal :stop} :default :seal}})

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds a pocket attempt may take." :spec (a/num-in 0 nil)}
   ::pocket-s {:default 10 :doc "Seconds a pocket is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done? [c] (way/done? c :pocket))

(defn quote-of [c]
  (let [feet (sh/feet (:primitives c))]
    (way/quote-of c (if (some #(pit/pocket-cells c feet %) dig-cells/sides) (settings/get settings ::pocket-s) ##Inf)
                  (settings/get settings ::job-time-s))))

(defn ^:async step!
  "A pocket dug once (retreat-pit/pocket!: it steps in), then walls on its way in: :done, :again, or nil with none that fits
  or one that cannot be walled."
  [c]
  (if (:dug (ctx/mem c))
    (await (way/walls-step! c))
    (when (await (pit/pocket! c))
      (ctx/update-mem! c assoc :dug true)
      :again)))

(defn ^:async round [c] (await (way/run! c :pocket step!)))
