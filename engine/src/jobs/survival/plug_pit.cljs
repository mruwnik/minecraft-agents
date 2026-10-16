(ns jobs.survival.plug-pit
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.shelter :as sh]
            [jobs.survival.plug-way :as way]
            [jobs.survival.retreat-pit :as pit]))

(def doc
  "A plug way: dig down 2 (3 on flat ground) under a solid side and plug the hole over the head with carried or dug blocks.
  Needs a pit that fits (every cell solid rock the carried tools harvest, no fluid beside it, solid under the bottom) and a block to
  plug with. One call is a whole attempt: :done {:pos :way :pit} once enclosed, at once when it already is; nothing-possible :failed
  with no pit that fits or one that fails (it dug into fluid or a cave, the floor is not solid, a place is refused). Lava a dig lays open is
  filled first (:on-lava :seal) and a pit that lays open fluid or a cave is dropped. Every plug goes to the scaffold ledger with :purpose
  (default :retreat-plug, taken back by jobs.access.cleanup once no hostile is near; :shelter is left alone). Quote: time (##Inf when no
  pit fits). Picked by jobs.survival.plug-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:purpose {:doc "ledger purpose of the placed blocks (:retreat-plug: taken back by the cleanup near no hostile; :shelter: left alone)" :spec #{:retreat-plug :shelter} :default :retreat-plug}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :radius {:doc "melee hostiles within this many blocks stand in the way of a cell" :spec (a/num-in 0 nil) :default 8}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pit is dropped and the lava left" :spec #{:seal :stop} :default :seal}})

(a/defargs settings
  {::job-time-s {:default 90 :doc "Seconds a pit attempt may take." :spec (a/num-in 0 nil)}
   ::pit-s {:default 12 :doc "Seconds a pit is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done? [c] (way/done? c :pit))

(defn quote-of [c]
  (way/quote-of c (if (pit/pit-plan c (sh/feet (:primitives c))) (settings/get settings ::pit-s) ##Inf)
                (settings/get settings ::job-time-s)))

(defn ^:async step!
  "One step of the pit: its plan is made once and kept in job memory. :done, :again, or nil with no pit that fits or one that failed."
  [c]
  (when-let [plan (or (:pit (ctx/mem c)) (await (pit/plan! c)))]
    (ctx/update-mem! c assoc :pit plan)
    (case (await (pit/pit-round! c plan))
      :hidden :done
      :again :again
      nil)))

(defn ^:async round [c] (await (way/run! c :pit step!)))
