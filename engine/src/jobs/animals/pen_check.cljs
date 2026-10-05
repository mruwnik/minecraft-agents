(ns jobs.animals.pen-check
  (:require [engine.ctx :as ctx]
            [engine.jobs.apiary :as apiary]
            [engine.jobs.pen :as pen]))

(def doc
  "Read-only: say whether an animal can walk out of a pen. Flood-fills what a cow can walk (rules in
  engine.jobs.pen: fences, walls and closed gates are 1.5 high, an open gate is a way out, a drop of more than 3
  is not taken) from the feet cell :at [x y z], or from every surface inside :box, where a step out of the box
  is a leak. Never moves, digs or places. Declines without :at and :box.

  The fill stops after :max-cells cells: a bigger pen is not closed, reason :unbounded. A fill without a box
  that finds no wall either side of a gap says that too.

  Hands over and emits (info pen-check.done) {:closed? :reason :cells :leaks :gates}:
  - :reason is nil when closed, else :leak, :unbounded, :unloaded or :no-start (:at is no floor to stand on).
  - :leaks lists {:pos {:x :y :z} :why}, why one of :open-gate :gap :climb :open :unloaded. At most 12 are
    listed, :leaks-total gives the count when more.
  - :cells counts the inside cells. For a leaky pen without a box it counts the cells the pen would enclose with
    its leaks shut, and 0 when shutting up to 4 rounds of them does not close it (a low wall all round).
  - :gates lists every fence gate in or beside those cells with :open?.")

(def args
  {:at {:doc "feet cell [x y z] of a spot inside the pen (the floor's top, where the animal's feet are)" :default nil}
   :box {:doc "the pen: {:min {:x :y :z} :max {:x :y :z}}, inclusive; a step out of it is a leak" :default nil}
   :max-cells {:doc "most cells the fill visits before it gives up with :unbounded" :default pen/default-max-cells}})

(def max-listed 12)

(defn check [c]
  (boolean (or (:at (:args c)) (:box (:args c)))))

(defn summary
  "The answer as the event and result carry it: the cells counted, the leaks capped."
  [{:keys [closed? reason inside leaks gates]}]
  (cond-> {:closed? closed? :reason reason :cells (count inside) :leaks (vec (take max-listed leaks)) :gates gates}
    (> (count leaks) max-listed) (assoc :leaks-total (count leaks))))

(defn ^:async round [c]
  (let [{:keys [at box max-cells]} (:args c)
        [x y z] at
        result (summary (pen/check {:block-at (apiary/block-at-fn (:primitives c))
                                    :at (when at {:x x :y y :z z})
                                    :box box
                                    :max-cells max-cells}))]
    (ctx/emit! c :pen-check.done :info
               (assoc result :text (if (:closed? result)
                                     (str "pen closed, " (:cells result) " cells")
                                     (str "pen not closed: " (name (:reason result))))))
    (ctx/result! c result)
    :done))
