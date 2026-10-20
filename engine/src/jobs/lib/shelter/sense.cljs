(ns jobs.lib.shelter.sense
  "Roof, light, cave and closed-room tests of the body's own cell: what the night jobs and the night trigger ask first."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.look :as look]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::room-roof-height {:default 16 :spec (a/int-in 1 nil)
                       :doc "How high above the feet a roof may be and still make the closed room around the body a shelter."}})

(defn feet [p] (solid/cell (u/self-pos {:primitives p})))

(defn solid-at? [p pos] (solid/solid? (u/seen-name p pos)))

(defn head-in-block?
  "Whether the body's eyes are inside a full block (it suffocates): buried in, not sheltered."
  [p]
  (breath/suffocates-here? p (breath/eye-cell (.self p))))

(defn roofed?
  "Whether a solid block lies within height blocks straight above the feet cell, the head not inside one."
  ([p height]
   (boolean (and (not (head-in-block? p)) (roofed? p height (feet p)))))
  ([p height {:keys [x y z]}]
   (boolean (some #(solid-at? p {:x x :y (+ y %) :z z}) (range 1 (inc height))))))

(defn sky-light-at
  "Sky light (0-15, before the night's darkening) of the cell, or nil when no light data is loaded there. Read only for the body's cells and cells it has seen."
  [p {:keys [x y z]}]
  (let [raw (look/raw-of p)]
    (when (and raw (look/light-known? raw x y z))
      (bit-shift-right (.lightAt ^js raw x y z) 4))))

(def light-in-step-ticks "Resolution of light-in's scan of the day ahead." 100)

(defn light-in-s
  "Seconds (at the live game rate) until a cell with sky light sky (0-15, before darkening) is lit enough that monsters
  cannot spawn there (sky less the sky darkening reaches look/dark-light): 0 when it already is, nil when waiting never
  does it (no sky light reaches the cell). t is the time of day in ticks; rain and thunder (0-1) are assumed to last."
  [sky t rain thunder]
  (when (pos? sky)
    (some (fn [k]
            (let [subtract (look/sky-subtract-at (+ t (* k light-in-step-ticks)) rain thunder)]
              (when (>= (- sky subtract) (look/dark-light)) (sh-clock/ticks->s (* k light-in-step-ticks)))))
          (range (inc (/ 24000 light-in-step-ticks))))))

(defn sky-state
  "{:t :rain :thunder} of the perception's sky now; nil without one (nothing is known, so nothing is dark)."
  [p]
  (when-let [^js sky (some-> (look/raw-of p) (.sky))]
    {:t (.-timeOfDay sky) :rain (or (.-rain sky) 0) :thunder (or (.-thunder sky) 0)}))

(defn sky-subtract-now
  "What the sky's light is reduced by now (look/sky-subtract); 0 without a sky."
  [p]
  (if-let [{:keys [t rain thunder]} (sky-state p)] (look/sky-subtract-at t rain thunder) 0))

(defn buried-by-column?
  "Fallback without light data: the cell two over the feet cell is a block the body has seen (only that one shows from inside)."
  ([p] (buried-by-column? p (feet p)))
  ([p {:keys [x y z]}]
   (let [above (look/seen-block p {:x x :y (+ y 2) :z z})]
     (boolean (and above (not (:unknown above)) (solid/solid? (:name above)))))))

(defn seen-cell?
  "Whether the body has seen the cell (its light data is no sight of it)."
  [p cell]
  (let [b (look/seen-block p cell)]
    (boolean (and b (not (:unknown b))))))

(defn buried-at?
  "Whether the cell is underground: no sky light at it and the cell over it (a cell at 0 is sealed from the sky, so
  the night changes nothing there; a ravine, shaft or cave mouth keeps sky light and stays unsafe). The brightness of
  a cell the body has seen is sensed like the light at its feet; an unseen cell is not buried. Without light data there,
  buried-by-column? decides."
  [p {:keys [x y z] :as cell}]
  (let [own (feet p)
        own? (= [x y z] [(:x own) (:y own) (:z own)])
        lights (keep #(sky-light-at p %) [cell {:x x :y (inc y) :z z}])]
    (cond
      (not (or own? (seen-cell? p cell))) false
      (seq lights) (every? zero? lights)
      :else (buried-by-column? p cell))))

(defn buried?
  "The body is underground (buried-at? its feet)."
  [p]
  (buried-at? p (feet p)))

(defn night? [p] (not (.-isDay (.self p))))

(defn sleeping? [p] (boolean (.-isSleeping (.self p))))

(def default-roof-height 4)

(defn room-roof-height [] (settings/get settings ::room-roof-height))

(defn block-light-at
  "Block light (0-15) of the cell, or nil when no light data is loaded there."
  [p {:keys [x y z]}]
  (let [raw (look/raw-of p)]
    (when (and raw (look/light-known? raw x y z))
      (bit-and (.lightAt ^js raw x y z) 15))))

(defn light-at
  "Effective light (0-15) of the cell: the brighter of its block light and its sky light less the sky's darkening now; nil
  without light data there. Not gated on sight (see cell-light)."
  [p cell]
  (when-let [block (block-light-at p cell)]
    (max block (- (or (sky-light-at p cell) 0) (sky-subtract-now p)))))

(defn cell-light
  "light-at for a cell the body has seen (its own cell is always known); nil for any other."
  [p {:keys [x y z] :as cell}]
  (let [own (feet p)]
    (when (or (= [x y z] [(:x own) (:y own) (:z own)]) (seen-cell? p cell))
      (light-at p cell))))

(defn lit-here?
  "Whether the cell is lit enough that monsters cannot spawn (look/lit-light?: any source, dawn or a torch) as the body knows
  it; a cell it has not seen is guessed by the open sky (look/dark-guess?)."
  [p cell]
  (if (cell-light p cell)
    (look/lit-light? (block-light-at p cell) (or (sky-light-at p cell) 0) (sky-subtract-now p))
    (not (look/dark-guess? p))))

(defn wide-room?
  "Whether the room has a 2x2 floor patch: a hut, not a one-wide tunnel the body dug."
  [{:keys [cells]}]
  (let [in? (set cells)]
    (boolean (some (fn [[x y z]] (and (in? [(inc x) y z]) (in? [x y (inc z)]) (in? [(inc x) y (inc z)]))) in?))))

(defn leaf-roofed?
  "Whether a leaf block lies within height blocks straight above the feet cell: no mob comes through it, and the cells
  under it are judged by their light."
  ([p height] (leaf-roofed? p height (feet p)))
  ([p height {:keys [x y z]}]
   (boolean (some #(some-> (u/seen-name p {:x x :y (+ y %) :z z}) (.endsWith "_leaves")) (range 1 (inc height))))))

(defn shut-in-room?
  "Whether the body stands in a closed room (reach-room/closed-room?) with a roof (solid or leaves) over it within
  room-roof-height blocks: safe from the night though the roof is above :roof-height. Sealed from the sky (buried?)
  counts as roofed: the roof over the feet cannot be looked at from inside."
  ([p] (shut-in-room? p (feet p)))
  ([p cell]
   (and (or (roofed? p (room-roof-height) cell) (leaf-roofed? p (room-roof-height) cell) (buried-at? p cell)) (reach-room/closed-room? p cell))))

(defn shut-in-hut?
  "shut-in-room? and wide: a hut, not a one-wide tunnel the body dug."
  [p]
  (and (shut-in-room? p) (wide-room? (reach-room/room p (feet p)))))

(defn sky-in
  "Seconds until the open sky is bright enough (light-in-s of full sky light now): 0 by day, the time left of the night
  at night, longer in a storm that darkens it."
  [p]
  (when-let [{:keys [t rain thunder]} (sky-state p)]
    (light-in-s 15 t rain thunder)))

(defn dark-outside?
  "Whether the open sky is dark now and waiting lifts it (the night): what a body shut in a shelter waits out."
  [p]
  (pos? (or (sky-in p) 0)))

(defn spawnable?
  "Whether a monster can spawn at the cell as the body has seen it: a solid floor under it and two cells of air."
  [p {:keys [x y z]}]
  (let [air? (fn [dy] (let [n (u/seen-name p {:x x :y (+ y dy) :z z})] (and n (not (solid/solid? n)))))]
    (and (air? 0) (air? 1) (solid-at? p {:x x :y (dec y) :z z}))))

(defn spawnable-cells [p cells] (filter #(spawnable? p %) cells))

(defn light-read
  "How dark the cells are, as the body knows them: {:dark-fraction share of the cells not lit-here? (an unseen cell by the
  guess), :darkest the lowest light of the cells with light data, :unseen cells with none}. One dark corner is the risk,
  so no median. A point is the one-cell case."
  [p cells]
  (let [lights (map #(light-at p %) cells)
        seen (remove nil? lights)]
    {:dark-fraction (if (seq cells) (/ (count (remove #(lit-here? p %) cells)) (count cells)) 0)
     :darkest (when (seq seen) (apply min seen))
     :unseen (- (count lights) (count seen))}))

(defn light-view
  "Read-only light the body perceives, for observe light. A cell {:x :y :z} gives {:cell :seen? :block :sky :sky-subtract
  :light (effective) :lit?}, nil values (also :lit?, and :seen? false) where no light data is loaded. With :radius (a cube around the cell)
  the light-read of its cells plus :cells :seen. The cell defaults to the feet."
  [p {:keys [cell radius]}]
  (let [cell (or cell (feet p))
        {:keys [x y z]} cell]
    (if-not radius
      (let [known? (some? (light-at p cell))]
        {:cell [x y z] :seen? known?
         :block (block-light-at p cell) :sky (sky-light-at p cell) :sky-subtract (sky-subtract-now p)
         :light (light-at p cell) :lit? (when known? (lit-here? p cell))})
      (let [cells (for [dx (range (- radius) (inc radius)) dy (range (- radius) (inc radius)) dz (range (- radius) (inc radius))]
                    {:x (+ x dx) :y (+ y dy) :z (+ z dz)})]
        (assoc (light-read p cells) :cell [x y z] :radius radius :cells (count cells)
               :seen (count (filter #(seen-cell? p %) cells)))))))

(defn light-in
  "Seconds until no cell of cells (a cell, or a vector of them) is dark (lit-here?): the slowest cell, nil when some dark
  cell is never lit. Cells with no light data are left out. 0 when none is dark."
  [p cells]
  (let [cells (if (map? cells) [cells] cells)
        sky (sky-state p)
        wait (fn [c]
               (cond (nil? (block-light-at p c)) 0
                     (lit-here? p c) 0
                     :else (when-let [lvl (and sky (sky-light-at p c))] (light-in-s lvl (:t sky) (:rain sky) (:thunder sky)))))
        ts (map wait cells)]
    (when (every? some? ts) (apply max 0 ts))))

(defn walkable-at?
  "Whether the cell at pos is one a body stands or walks in: not solid, or a bed."
  [p pos]
  (let [n (u/seen-name p pos)]
    (and (some? n) (or (not (solid/solid? n)) (.endsWith n "_bed")))))

(defn doorway?
  "Whether the cell is a gap in a wall: solid on both sides along one horizontal axis and free on both ends of the other."
  [p {:keys [x y z]}]
  (let [solid (fn [dx dz] (solid-at? p {:x (+ x dx) :y y :z (+ z dz)}))
        free (fn [dx dz] (not (solid dx dz)))]
    (boolean (or (and (solid 1 0) (solid -1 0) (free 0 1) (free 0 -1))
                 (and (solid 0 1) (solid 0 -1) (free 1 0) (free -1 0))))))

(defn open-air-above?
  "Whether every cell over goal up to height y is seen and not solid: a spot in the open, like a hillside below a roofed hut."
  [p goal y]
  (every? #(let [n (u/seen-name p (update goal :y + %))] (and n (not (solid/solid? n)))) (range 1 (inc (- y (:y goal))))))

(defn seen-open-over?
  "Whether some cell within default-roof-height over goal is seen and not solid: rock shows no cell beyond its face."
  [p goal]
  (boolean (some #(some-> (u/seen-name p (update goal :y + %)) solid/solid? not) (range 1 (inc default-roof-height)))))

(defn sky-reaches-from-above?
  "Whether sky light reaches the cell straight down its own column: some sky light there and no solid block seen over it within 32 cells (leaves let it through dimmed; a stair step lit round a corner has stone over it)."
  [p cell]
  (and (pos? (or (sky-light-at p cell) 0))
       (not-any? #(solid-at? p (update cell :y + %)) (range 1 33))))
