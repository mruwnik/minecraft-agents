(ns jobs.survival.plug-in
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter :as sh]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-pit :as pit]
            [jobs.survival.retreat-seal :as seal]))

(def doc
  "Shut the body in where it stands, atomically: no night or hostile logic, no walking; whoever calls it decides when.
  Needs nothing to start. One call is a whole attempt: it ends :done {:pos :way} once the body is enclosed (a roof within
  :roof-height over no open cell round the feet), at once when it already is.
  Ways (:ways, in this order, the first that works): :walls fills the open sides at feet and head height, a roof support and the roof
  with carried blocks, the side towards a shooter, else the nearest melee hostile, first; :pit digs down 2 (3 on flat ground) under a
  solid side and plugs the hole over the head; :pocket digs a pocket beside the body (feet and head cell, solid on every other
  side), steps in and walls the way in with the dug blocks. Blocks are the call's :use :blocks, else every :build filler, carried or dug.
  A way that fails is not tried again this call; none left is nothing-possible :no-way {:text :tried}. Lava a dig lays open is filled
  first (:on-lava :seal) and a pit or pocket that lays open fluid or a cave is dropped. A place refused for a mob in the cell is retried
  (dig-in-cells/mob-wait-ticks), then fails that way.
  Every placed block goes to the scaffold ledger with :purpose (default :retreat-plug, taken back by jobs.access.cleanup once no
  hostile is near; :shelter is left alone). A cell in another's zone is used only as a last resort (retreat.trespass-last-resort).
  Quote: time (one cell a placement up to a pit). The ways are retreat's seal, pit and pocket code (retreat-seal, retreat-pit).")

(a/defargs args
  {:ways {:doc "the ways to enclose the body, tried in the order :walls, :pit, :pocket" :spec (a/set-of #{:walls :pit :pocket}) :default #{:walls :pit :pocket}}
   :purpose {:doc "ledger purpose of the placed blocks (:retreat-plug: taken back by the cleanup near no hostile; :shelter: left alone)" :spec #{:retreat-plug :shelter} :default :retreat-plug}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :radius {:doc "melee hostiles within this many blocks decide which side is walled first and stand in the way of a cell" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles within this many blocks decide which side is walled first" :spec (a/num-in 0 nil) :default 16}
   :max-places {:doc "placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pit or pocket is dropped and the lava left" :spec #{:seal :stop} :default :seal}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a plug-in may take." :spec (a/num-in 0 nil)}
   ::mean-s {:default 12 :doc "Seconds an enclosing is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s} :ways #{:walls :pit :pocket}})

(def ways [:walls :pit :pocket])

(defn enclosed? [c]
  (let [p (:primitives c)]
    (dig-cells/sealed-in? p :walls (:roof-height (:args c)))))

(defn done? [c]
  (let [done (enclosed? c)]
    (checks/goal done [:enclosed "the body is enclosed"] [:open "the body is not enclosed"]
                 {:units :enclosures :done (if done 1 0) :left (if done 0 1)
                  :result {:pos (sh/feet (:primitives c)) :way (:way (ctx/mem c))}})))

(defn quote-of [c]
  (let [mean (if (enclosed? c) 0 (settings/get settings ::mean-s))]
    {:uses {:job-time [0 mean (max (* 3 mean) (settings/get settings ::job-time-s))]} :done [0 1 1]}))

(defn ^:async walls-step!
  "One step of the walls (retreat-seal/seal!): :done once enclosed, :again, or nil when they cannot be placed."
  [c]
  (case (await (seal/seal! c))
    :sealed :done
    :again :again
    nil))

(defn ^:async pit-step!
  "One step of the pit (retreat-pit): its plan is made once and kept in job memory. :done, :again, or nil with no pit that fits
  or one that failed."
  [c]
  (when-let [plan (or (:pit (ctx/mem c)) (await (pit/plan! c)))]
    (ctx/update-mem! c assoc :pit plan)
    (case (await (pit/pit-round! c plan))
      :hidden :done
      :again :again
      (do (ctx/update-mem! c dissoc :pit) nil))))

(defn ^:async pocket-step!
  "A pocket dug once (retreat-pit/pocket!: it steps in), then walls on its way in: :done, :again, or nil with none that fits
  or one that cannot be walled."
  [c]
  (if (:pocket-dug (ctx/mem c))
    (or (await (walls-step! c)) (do (ctx/update-mem! c dissoc :pocket-dug) nil))
    (when (await (pit/pocket! c))
      (ctx/update-mem! c assoc :pocket-dug true)
      :again)))

(defn ^:async step
  "One step of the first way not failed: :again, or the call's end."
  [c]
  (let [failed (:failed-ways (ctx/mem c) #{})
        left (remove failed (filter #(ctx/way? c %) ways))]
    (loop [[way & more] left]
      (if (nil? way)
        (ctx/end c :nothing-possible :no-way {:text (str "cannot enclose the body: " (pr-str ways) " all failed or left out")
                                              :tried (vec failed)})
        (let [r (await (case way :walls (walls-step! c) :pit (pit-step! c) :pocket (pocket-step! c)))]
          (ctx/update-mem! c assoc :way way)
          (case r
            :done (ctx/end c :done nil {:pos (sh/feet (:primitives c)) :way way})
            :again :again
            (do (ctx/update-mem! c update :failed-ways (fnil conj #{}) way)
                (recur more))))))))

(defn ^:async round [c]
  (await (pace/steps! c (fn ^:async plug-step [] (await (step c))))))
