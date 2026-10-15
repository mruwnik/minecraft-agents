(ns jobs.survival.night.doorway
  "The night's shut doorway: a roofed body about to put its bed down in a room with a gap in its walls (a doorway go-to
  dug, or one never shut) fills the gap first, so it does not sleep open to the night."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(defn carried-door
  "A wooden door item the body carries (a hand opens it in the morning), or nil."
  [p]
  (some #(let [n (:name %)] (when (and (.endsWith n "_door") (not= "iron_door" n)) n)) (u/inventory p)))

(defn gap
  "The cells [feet head] that shut the room around the body: the doorway (sh/doorway?) where the room's flood first
  leaves the roof (reach/unroofed-cell, closed flood or not: a walled yard is open to the night too), or the one beside that cell, those still free; nil when the room is shut, the way out is no
  doorway (open ground), the body stands in it, the place is refused, or this doorway was tried tonight (job memory
  :doorway-tried)."
  [c]
  (let [p (:primitives c)
        feet (sh/feet p)
        free? #(#{"air" "cave_air"} (u/seen-name p %))]
    (when-let [{:keys [x y z] :as e} (:cell (reach/unroofed-cell p feet))]
      (when-let [d (first (filter #(and (free? %) (not= feet %) (sh/doorway? p %))
                                  (cons e (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] {:x (+ x dx) :y y :z (+ z dz)}))))]
        (when-not (or (contains? (:doorway-tried (ctx/mem c)) d)
                      (access/trespass-refusal (access/rules-input c) :place d))
          (seq (filter free? [d (update d :y inc)])))))))

(defn ^:async shut!
  "Fill the gap (jobs.blocks.fill-cells: a carried wooden door, else the carried building blocks). Once it ends, the
  doorway is tried for tonight, filled or not. :again."
  [c cells]
  (let [door (carried-door (:primitives c))
        r (await (ctx/call c :shut-doorway 'jobs.blocks.fill-cells (cond-> {:cells (vec cells)} door (assoc :use {:blocks [door]}))))]
    (when-not (= :continue r)
      (ctx/update-mem! c update :doorway-tried (fnil conj #{}) (first cells)))
    :again))
