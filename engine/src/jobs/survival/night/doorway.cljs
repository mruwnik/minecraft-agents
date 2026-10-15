(ns jobs.survival.night.doorway
  "The night's shut doorway: a roofed body about to put its bed down in a room with a gap in its walls (a doorway go-to
  dug, or one never shut) fills the gap first, so it does not sleep open to the night. The filled cells are remembered
  (:doorway-filled) and dug again by day (night.morning), so no doorway stays walled up."
  (:require [engine.ctx :as ctx]
            [jobs.access.pillar :as pillar]
            [jobs.lib.access :as access]
            [jobs.lib.look :as sight]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(defn carried-door
  "A wooden door item the body carries, or nil."
  [p]
  (some #(let [n (:name %)] (when (and (.endsWith n "_door") (not= "iron_door" n)) n)) (u/inventory p)))

(defn gap
  "The cells [feet head] that shut the room around the body: the doorway (sh/doorway?) where the room's flood first
  leaves the roof (reach/unroofed-cell, closed flood or not: a walled yard is open to the night too), or the one beside that cell, those still free; nil when the room is shut, the way out is no
  doorway (open ground), the body or a seen animal or mob stands in it, the place is refused, or this doorway was tried
  tonight (job memory :doorway-tried)."
  [c]
  (let [p (:primitives c)
        feet (sh/feet p)
        seen (delay (sight/seen-entities p {:radius 8}))
        free? (fn [{:keys [x y z] :as cell}]
                (and (#{"air" "cave_air"} (u/seen-name p cell)) (not (pillar/mob-in @seen [x y z]))))]
    (when-let [{:keys [x y z] :as e} (:cell (reach/unroofed-cell p feet))]
      (when-let [d (first (filter #(and (free? %) (not= feet %) (sh/doorway? p %))
                                  (cons e (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] {:x (+ x dx) :y y :z (+ z dz)}))))]
        (when-not (or (contains? (:doorway-tried (ctx/mem c)) d)
                      (access/trespass-refusal (access/rules-input c) :place d))
          (seq (filter free? [d (update d :y inc)])))))))

(defn remember-cells!
  "Remember each of cells as :doorway-filled, once, before it is filled: a fill cut short is opened by day too."
  [c cells]
  (doseq [cell cells
          :when (not-any? #(= cell (:cell (:data %))) (ctx/entries c :doorway-filled))]
    (ctx/remember! c :doorway-filled {:cell cell} sh/doorway-filled-policy)))

(defn ^:async shut!
  "Fill the gap (jobs.blocks.fill-cells: a carried wooden door, else the carried building blocks). Once it ends, the
  doorway is tried for tonight, filled or not, the cells left free are forgotten, and a full fill is job memory
  :doorway-shut. :again."
  [c cells]
  (remember-cells! c cells)
  (let [door (carried-door (:primitives c))
        r (await (ctx/call c :shut-doorway 'jobs.blocks.fill-cells (cond-> {:cells (vec cells)} door (assoc :use {:blocks [door]}))))]
    (when-not (= :continue r)
      (ctx/forget-where! c :doorway-filled #(#{"air" "cave_air"} (u/seen-name (:primitives c) (:cell %))))
      (ctx/update-mem! c #(cond-> (update % :doorway-tried (fnil conj #{}) (first cells))
                            (= :done (:kind r)) (assoc :doorway-shut true))))
    :again))
