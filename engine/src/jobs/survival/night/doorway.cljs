(ns jobs.survival.night.doorway
  "The night's shut doorway: a roofed body about to put its bed down in a room with a gap in its walls (a doorway go-to
  dug, or one never shut) fills the gap first, so it does not sleep open to the night. The filled cells are remembered
  (:doorway-filled) and dug again by day (night.morning), so no doorway stays walled up."
  (:require [engine.ctx :as ctx]
            [jobs.access.pillar :as pillar]
            [jobs.lib.access :as access]
            [jobs.lib.choose :as choose]
            [jobs.lib.look :as sight]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(defn carried-door
  "A wooden door item the body carries, or nil."
  [p]
  (some #(let [n (:name %)] (when (and (.endsWith n "_door") (not= "iron_door" n)) n)) (u/inventory p)))

(defn around
  "The cell and its four horizontal neighbours."
  [{:keys [x y z] :as cell}]
  (cons cell (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] {:x (+ x dx) :y y :z (+ z dz)})))

(defn closed-home?
  "Whether the body stands in a home closed to the night but for a doorway: every cell of its room is roofed, or the
  roof ends in a doorway (sh/doorway?) or beside one. Under an overhang or a lone roof it ends on open ground: false."
  [p]
  (let [feet (sh/feet p)
        cell (:cell (reach/unroofed-cell p feet))]
    (or (nil? cell)
        (and (not= feet cell) (boolean (some #(and (not= feet %) (sh/doorway? p %)) (around cell)))))))

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
    (when-let [e (:cell (reach/unroofed-cell p feet))]
      (when-let [d (first (filter #(and (free? %) (not= feet %) (sh/doorway? p %)) (around e)))]
        (when-not (or (contains? (:doorway-tried (ctx/mem c)) d)
                      (access/trespass-refusal (access/rules-input c) :place d))
          (seq (filter free? [d (update d :y inc)])))))))

(defn remember-cells!
  "Remember each of cells as :doorway-filled {:cell :blocks}, once, before it is filled with one of blocks: a fill cut
  short is opened by day too."
  [c cells blocks]
  (doseq [cell cells
          :when (not-any? #(= cell (:cell (:data %))) (ctx/entries c :doorway-filled))]
    (ctx/remember! c :doorway-filled {:cell cell :blocks blocks} sh/doorway-filled-policy)))

(defn ^:async shut!
  "Fill the gap (jobs.blocks.fill-cells: a carried wooden door, else the carried building blocks). Once it ends, the
  doorway is tried for tonight, filled or not, and each cell's :doorway-filled entry keeps the block it now holds, or is
  forgotten when it holds none of them. :again."
  [c cells]
  (let [door (carried-door (:primitives c))
        blocks (if door [door] (choose/default-use c :blocks :build))
        _ (remember-cells! c cells blocks)
        r (await (ctx/call c :shut-doorway 'jobs.blocks.fill-cells (cond-> {:cells (vec cells)} (seq blocks) (assoc :use {:blocks blocks}))))]
    (when-not (= :continue r)
      (ctx/forget-where! c :doorway-filled #(some #{(:cell %)} cells))
      (doseq [cell cells
              :let [n (u/seen-name (:primitives c) cell)]
              :when (some #{n} blocks)]
        (ctx/remember! c :doorway-filled {:cell cell :blocks [n]} sh/doorway-filled-policy))
      (ctx/update-mem! c update :doorway-tried (fnil conj #{}) (first cells)))
    :again))
