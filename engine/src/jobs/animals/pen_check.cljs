(ns jobs.animals.pen-check
  (:require [engine.ctx :as ctx]
            [engine.jobs.apiary :as apiary]
            [engine.jobs.pen :as pen]))

(def doc
  "Read-only: say whether an animal can walk out of a pen, by flood-filling what
  a cow can walk (engine.jobs.pen has the rules: fences, walls and closed gates
  1.5 high, an open gate a way out, a block, slab or carpet stepped onto, a drop
  of more than 3 not taken) from the feet cell :at [x y z], or from every
  surface inside :box {:min :max}, where a step out of the box is a leak. The
  fill stops after :max-cells cells: a pen bigger than that is not closed with
  reason :unbounded (a fill without a box that finds no wall either side of a
  gap says that too). Hands over and emits (info pen-check.done) {:closed?
  :reason :cells :leaks :gates}: :reason is nil when closed, else :leak
  (:leaks [{:pos {:x :y :z} :why}], why one of :open-gate :gap :climb :open
  :unloaded; at most 12 listed, :leaks-total when more), :unbounded, :unloaded
  or :no-start (:at is no floor to stand on); :cells counts the inside cells
  (for a leaky pen without a box: the cells it would enclose with its leaks,
  open gates and gaps, shut; 0 when shutting up to 4 rounds of them does not close it,
  as with a low wall all round); :gates lists every fence gate in or beside
  those cells with :open?. It declines without :at and :box, and never moves,
  digs or places.")

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
