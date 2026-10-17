(ns jobs.survival.plug-pocket
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
                        [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.plug-way :as way]
            [jobs.survival.retreat-pit :as pit]))

(def doc
  "A plug way: dig a pocket beside the body (feet and head cell, solid on every other side), step in and wall the way in with the
  dug blocks. Needs a side that fits (solid rock the carried tools harvest, no fluid beside it) and a block to wall with, carried
  or dug. One call is a whole attempt: :done {:pos :way :pocket} once enclosed, at once when it already is; nothing-possible :failed
  with no pocket that fits, a dig that fails or a way in that cannot be walled. A pocket that lays open fluid or a cave is
  dropped (lava is filled first, :on-lava :seal). With no side that fits at the feet it first walks (go-to) to the nearest seen
  standing cell within :walk-radius, at the same height, from where one does (a rock wall of the room), not to a cell a hostile
  stands on; the quote adds the walk. The walls go to the scaffold ledger with :purpose (default :retreat-plug, taken
  back by jobs.access.cleanup once no hostile is near; :shelter is left alone). Quote: time (##Inf when no side fits). Picked by
  jobs.survival.plug-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:purpose {:doc "ledger purpose of the placed blocks (:retreat-plug: taken back by the cleanup near no hostile; :shelter: left alone)" :spec #{:retreat-plug :shelter} :default :retreat-plug}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh-sense/default-roof-height}
   :radius {:doc "melee hostiles within this many blocks decide which side is walled first and stand in the way of a cell" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles within this many blocks decide which side is walled first" :spec (a/num-in 0 nil) :default 16}
   :walk-radius {:doc "blocks, at the body's height, within which it walks to a standing cell with a side to pocket when none fits where it stands (0: never walks)" :spec (a/num-in 0 nil) :default 6}
   :max-places {:doc "placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pocket is dropped and the lava left" :spec #{:seal :stop} :default :seal}})

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds a pocket attempt may take." :spec (a/num-in 0 nil)}
   ::walk-s-per-block {:default 0.3 :doc "Seconds a block of walk to a pocket's standing cell is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::pocket-s {:default 10 :doc "Seconds a pocket is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done? [c] (way/done? c :pocket))

(defn side-here? [c feet] (boolean (some #(pit/pocket-cells c feet %) dig-cells/sides)))

(defn stand-cell
  "The nearest cell within :walk-radius at feet height that the body sees open (feet and head over a solid floor), holds no
  hostile and has a pocket side (pocket-cells), nil when none."
  [c feet]
  (let [p (:primitives c)
        r (int (:walk-radius (:args c)))
        mobs (pit/hostiles c)
        ok? (fn [cell]
              (and (dig-cells/open-cell? p cell) (dig-cells/open-cell? p (update cell :y inc))
                   (dig-cells/known-solid? p (update cell :y dec)) (not (mobs cell))
                   (side-here? c cell)))]
    (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r))
               :let [cell (assoc feet :x (+ (:x feet) dx) :z (+ (:z feet) dz))]
               :when (and (not= cell feet) (<= (+ (* dx dx) (* dz dz)) (* r r)))]
           cell)
         (sort-by #(u/dist feet %))
         (filter ok?)
         first)))

(defn quote-of [c]
  (let [feet (sh-sense/feet (:primitives c))
        stand (when-not (side-here? c feet) (stand-cell c feet))
        walk (if stand (* (u/dist feet stand) (settings/get settings ::walk-s-per-block)) 0)]
    (way/quote-of c (if (or stand (side-here? c feet)) (+ walk (settings/get settings ::pocket-s)) ##Inf)
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

(def safe? ready/ok)
(def fit? ready/ok)

(defn ^:async walk!
  "The walk to the pocket's standing cell once, when no side fits at the feet: nil to go on, or the ending of a walk that failed."
  [c]
  (let [feet (sh-sense/feet (:primitives c))]
    (when-not (side-here? c feet)
      (if-let [stand (stand-cell c feet)]
        (let [e (await (ctx/call c :walk 'jobs.movement.go-to {:pos stand :range 0 :ways #{:walk :clear-seal}}))]
          (cond (= :continue e) :continue
                (not= :done (:kind e)) (ctx/end c :nothing-possible :failed {:text "could not walk to a wall to pocket" :reason :walk-failed})))
        (ctx/end c :nothing-possible :failed {:text "no side to pocket and no wall in sight to walk to"})))))

(defn ^:async round [c]
  (or (and (not (way/enclosed? c)) (await (walk! c)))
      (await (way/run! c :pocket step!))))
