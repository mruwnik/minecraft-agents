(ns jobs.items.use
  (:require [engine.args :as a]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cond :as cond]
            [jobs.lib.use-air :as air]
            [jobs.lib.use-block :as block]
            [jobs.lib.use-entity :as entity]))

(def doc
  "Right-click :on once, with :item in hand: a block (:on is a position, [x y z] or {:x :y :z}) or an entity (:on is its id,
  from observe entities). One call is the whole attempt. :continue only when the walk waits on the world.

  A block: walk into reach (one jobs.movement.go-to call, within :range, no escalation), click on :face with :item (nil: whatever is held stays
  in hand; none: the empty hand). A bucket :item (water_bucket pours, lava_bucket pours lava, bucket scoops) is used on the cell :on itself (with a :face, on the cell beside that face of the block :on, as a player clicks), through the
  place act: the liquid goes there, or is taken from there. A bad call is never (:bad-args); a bucket changes the cell, so zones,
  claims and plans may refuse it (a pour asks :place, a scoop :harvest; {:reason :not-allowed :pos :by ...}, :ignore-zones? skips the rule); a plain
  click changes no block by itself and asks no zone. Ends with data {:used true|false :pos :item :face :reason :raw (the primitive's answer: before,
  after, consumed)}, plus :spawn-set true when a bed answered \"respawn point set\" (a bed is used by day to set the spawn; not in the Nether or End).
  :reason is the primitive's status (:used, :unchanged, :no-item, :no-room, :missing, :cannot, :occupied, :unreachable, :failed); only :used is done.
  condition-failed: :no-item, :missing, :cannot, :occupied (a bucket's cell is filled). nothing-possible: :unreachable (a walk that failed twice, :why),
  :unchanged, :no-room, :failed.

  In the air (:air true): look at the spot :on and use :item without a block (throw an egg, snowball, ender pearl); data {:used :pos :item :reason :raw};
  condition-failed :no-item, :cannot (not a throwable); nothing-possible :unchanged, :failed.

  An entity: find it in sight, walk into reach (a go-to child, range 2, no escalation; again when the entity moved since the walk arrived), click
  with :item (nil or none: the empty hand). An entity that drops out of sight (the pit rim, a wall) between the sighting and the click is walked to at
  its last seen place and clicked there (the click finds it by id); it is :gone when the click does not. Ends with data {:used true|false :id :item
  :text, and after a click :raw the primitive's answer as a map}. The primitive's status :used is done. Condition-failed: :gone (no such entity in
  sight), :no-item, :full, :cannot. Nothing-possible: :unreachable (the walk failed twice, data :why), :no-effect, :out-of-reach, :failed.")

(a/defargs args
  {:on {:doc "what to click: a block, [x y z] or {:x :y :z}, or an entity id (observe entities lists them)" :spec (a/or-of a/position? (a/int-in 1 nil)) :default nil}
   :item {:doc "the item to hold for the click; nil: a block keeps the hand as it is, an entity gets the empty hand; none: the empty hand" :spec a/item? :default nil}
   :see {:doc "block only. true: click only from where the eye sees the block (jobs.lib.aim), walking to such a stand first (go-to :see; none: :unreachable); false: within reach is enough, though a wall in between refuses the click" :spec boolean? :default false}
   :air {:doc "true: no block is clicked; the body looks at the spot :on (any point, a whole cell at its centre) and uses :item in the air, as a player throws an egg, snowball or ender pearl. No walk; needs :item (egg snowball ender_pearl experience_bottle wind_charge splash_potion lingering_potion trident); :used when one was spent. Zones are not asked" :spec boolean? :default false}
   :ignore-zones? :shared
   :for-plan {:doc "block only: id of the plan whose work this is: its own footprint does not refuse the bucket; nil: every plan's footprint does" :spec a/name? :default nil}
   :range {:doc "block only: how many cells from :on the walk may stop (go-to :range); a click the server checks by distance, a bed's 3 blocks, wants less" :spec (a/num-in 0 nil) :default 3}
   :face {:doc "block only: the face clicked: up down north south east west; nil: up for an item, and for a bucket :on itself is the cell. A bucket with a face fills (or scoops) the cell beside that face of the block :on" :spec #{"up" "down" "north" "south" "east" "west"} :default nil}})


(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(defn entity? [c] (number? (:on (:args c))))

(defn ops
  "The functions of the kind of target (jobs.lib.use-block or jobs.lib.use-entity)."
  [c]
  (cond (entity? c) entity/ops
        (:air (:args c)) air/ops
        :else block/ops))

(def target [:on :item])
(def defaults {:spend {:job-time ::job-time-s}})

(defn safe? [c] ((:safe? (ops c)) c))
(def fit? ready/ok)
(defn quote-of [c] ((:quote (ops c)) c))

(defn bad-args [c]
  (if-let [error ((:bad-args (ops c)) c)]
    (contract/never :bad-args error)
    (contract/ok :args "something to click")))

(defn zones [c] ((:zones (ops c)) c))

(def can-start? (cond/and bad-args zones))
(def can-run? zones)

(defn done?
  "Ok once the click was made."
  [c]
  (if (:used (ctx/mem c))
    (contract/ok :used "use: the click was made" :units :uses :done 1 :left 0 :result {:used true})
    (contract/later :not-used "use: the round ends the call" :units :uses :done 0 :left 1 :result {:used false})))

(defn ^:async round [c]
  (await ((:round (ops c)) c)))
