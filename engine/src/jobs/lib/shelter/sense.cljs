(ns jobs.lib.shelter.sense
  "Roof, light, cave and closed-room tests of the body's own cell: what the night jobs and the night trigger ask first."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.clock :as sh-clock]
            [engine.perception.light :as light]
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
  [p height]
  (let [{:keys [x y z]} (feet p)]
    (boolean (and (not (head-in-block? p))
                  (some #(solid-at? p {:x x :y (+ y %) :z z}) (range 1 (inc height)))))))

(defn sky-light-at
  "Sky light (0-15, before the night's darkening) of the cell, or nil when no light data is loaded there. Read only for the body's cells and cells it has seen."
  [p {:keys [x y z]}]
  (let [raw (some-> (aget p "perception") :raw)]
    (when (and raw (<= 0 (.stateAt ^js raw x y z)))
      (bit-shift-right (.lightAt ^js raw x y z) 4))))

(def light-in-step-ticks "Resolution of light-in's scan of the day ahead." 100)

(defn light-in-s
  "Seconds (at the live game rate) until a cell with sky light sky (0-15, before darkening) is lit enough that monsters
  cannot spawn there (sky less the sky darkening reaches look/dark-light): 0 when it already is, nil when waiting never
  does it (no sky light reaches the cell). t is the time of day in ticks; rain and thunder (0-1) are assumed to last."
  [sky t rain thunder]
  (when (pos? sky)
    (some (fn [k]
            (let [subtract (js/Math.round (* 11 (/ (- 1 (light/sky-darken (+ t (* k light-in-step-ticks)) rain thunder)) 0.8)))]
              (when (>= (- sky subtract) (look/dark-light)) (sh-clock/ticks->s (* k light-in-step-ticks)))))
          (range (inc (/ 24000 light-in-step-ticks))))))

(defn sky-state
  "{:t :rain :thunder} now: the raw sky when the perception has one, else the self's clock and weather flags."
  [p]
  (if-let [^js sky (some-> (aget p "perception") :raw (.sky))]
    {:t (.-timeOfDay sky) :rain (or (.-rain sky) 0) :thunder (or (.-thunder sky) 0)}
    (let [self (.self p)]
      {:t (.-timeOfDay self) :rain (if (.-raining self) 1 0) :thunder (if (.-thundering self) 1 0)})))

(defn sky-subtract-now
  "What the sky's light is reduced by now (look/sky-subtract, from sky-state)."
  [p]
  (let [{:keys [t rain thunder]} (sky-state p)]
    (js/Math.round (* 11 (/ (- 1 (light/sky-darken t rain thunder)) 0.8)))))

(defn lit-here?
  "Whether the cell is lit enough that monsters cannot spawn: block light of 1 or more, or sky light less the sky
  darkening at dark-light, from any source (dawn, a torch). Without light data there, the sky decides (look/dark-guess?)."
  [p cell]
  (let [raw (some-> (aget p "perception") :raw)
        {:keys [x y z]} cell]
    (if (and raw (<= 0 (.stateAt ^js raw x y z)))
      (let [packed (.lightAt ^js raw x y z)]
        (or (pos? (bit-and packed 15)) (>= (- (bit-shift-right packed 4) (sky-subtract-now p)) (look/dark-light))))
      (not (look/dark-guess? p)))))

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

(def dark-here? look/dark-here?)

(defn sleeping? [p] (boolean (.-isSleeping (.self p))))

(def default-roof-height 4)

(defn room-roof-height [] (settings/get settings ::room-roof-height))

(def cave-radius 3)

(def cave-width
  "Standable cells under a ceiling within cave-radius of the feet from which the body counts as in an open cave, not a tunnel it dug."
  16)

(def cave-ceiling 8)

(def lit-level "Block light at the feet from which a cell is lit enough that the body counts it a lit pocket." 8)

(defn block-light-at
  "Block light (0-15) of the cell, or nil when no light data is loaded there."
  [p {:keys [x y z]}]
  (let [raw (some-> (aget p "perception") :raw)]
    (when (and raw (<= 0 (.stateAt ^js raw x y z)))
      (bit-and (.lightAt ^js raw x y z) 15))))

(defn light-at
  "The brighter of block and sky light (0-15, before the night's darkening) of the cell, or nil when no light data is
  loaded there."
  [p cell]
  (when-let [block (block-light-at p cell)]
    (max block (sky-light-at p cell))))

(defn cell-light
  "Effective light (0-15) of the cell: the brighter of its block light and its sky light less the sky's darkening now; nil
  for a cell the body has not seen (its own cell is always known) or without light data there."
  [p {:keys [x y z] :as cell}]
  (let [own (feet p)]
    (when (or (= [x y z] [(:x own) (:y own) (:z own)]) (seen-cell? p cell))
      (when-let [block (block-light-at p cell)]
        (max block (- (or (sky-light-at p cell) 0) (sky-subtract-now p)))))))

(defn dark-at?
  "Whether the cell is dark (effective light below look/dark-light): by its light when the body knows it, else by the
  guess for unseen ground (look/dark-guess?)."
  [p cell]
  (if-let [light (cell-light p cell)]
    (< light (look/dark-light))
    (look/dark-guess? p)))

(defn cave-cells
  "How many standable cells (feet, head and the cell over it free, a solid floor, a seen solid within cave-ceiling over them) lie
  within cave-radius of the feet, a block up or down: a cave is wide and high, what the body digs is 2 high (a tunnel, a mine's room)."
  [p]
  (let [{:keys [x y z]} (feet p)
        solid? (fn [cx cy cz] (solid-at? p {:x cx :y cy :z cz}))]
    (count (for [dx (range (- cave-radius) (inc cave-radius))
                 dz (range (- cave-radius) (inc cave-radius))
                 dy [-1 0 1]
                 :let [cx (+ x dx) cy (+ y dy) cz (+ z dz)]
                 :when (and (solid? cx (dec cy) cz) (not (solid? cx cy cz)) (not (solid? cx (inc cy) cz)) (not (solid? cx (+ cy 2) cz))
                            (some #(solid? cx (+ cy %) cz) (range 2 (inc cave-ceiling))))]
             1))))

(defn wide-room?
  "Whether the room has a 2x2 floor patch: a hut, not a one-wide tunnel the body dug."
  [{:keys [cells]}]
  (let [in? (set cells)]
    (boolean (some (fn [[x y z]] (and (in? [(inc x) y z]) (in? [x y (inc z)]) (in? [(inc x) y (inc z)]))) in?))))

(defn shut-in-room?
  "Whether the body stands in a closed room (reach/closed-room?) with a roof over it within room-roof-height blocks:
  safe from the night though the roof is above :roof-height. Sealed from the sky (buried?) counts as roofed: the roof
  over the feet cannot be looked at from inside."
  [p]
  (and (or (roofed? p (room-roof-height)) (buried? p)) (reach/closed-room? p (feet p))))

(defn shut-in-hut?
  "shut-in-room? and wide: a hut, not a one-wide tunnel the body dug."
  [p]
  (and (shut-in-room? p) (wide-room? (reach/room p (feet p)))))

(defn open-cave?
  "Buried in a dark, wide natural cave: no sky light, not shut in a closed room, under lit-level block light at the
  feet (no light data counts as dark) and cave-cells wide. Mobs spawn there whatever the hour."
  [p]
  (boolean (and (buried? p) (not (shut-in-room? p))
                (< (or (block-light-at p (feet p)) 0) lit-level)
                (>= (cave-cells p) cave-width))))

(defn sky-in
  "Seconds until the open sky is bright enough (light-in-s of full sky light now): 0 by day, the time left of the night
  at night, longer in a storm that darkens it."
  [p]
  (let [{:keys [t rain thunder]} (sky-state p)]
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

(defn cell-light-in
  "Seconds until the cell is lit (0 when it is: lit-here?), nil when waiting never does it; nil too without light data."
  [p cell]
  (cond
    (nil? (block-light-at p cell)) nil
    (lit-here? p cell) 0
    :else (when-let [sky (sky-light-at p cell)]
            (let [{:keys [t rain thunder]} (sky-state p)]
              (light-in-s sky t rain thunder)))))

(defn light-read
  "How dark the cells are, as the body knows them: {:dark-fraction share of the cells dark now, :darkest the lowest
  light (block or sky less the darkening), :unseen cells with no light data (guessed dark, counted in the fraction)}.
  One dark corner is the risk, so no median. A point is the one-cell case."
  [p cells]
  (let [subtract (sky-subtract-now p)
        light (fn [c] (when-let [block (block-light-at p c)] (max block (- (sky-light-at p c) subtract))))
        lights (map light cells)
        unseen (count (filter nil? lights))
        seen (remove nil? lights)]
    {:dark-fraction (if (seq cells) (/ (count (remove #(and (light %) (lit-here? p %)) cells)) (count cells)) 0)
     :darkest (when (seq seen) (apply min seen))
     :unseen unseen}))

(defn light-in
  "Seconds until no cell of cells (a cell, or a vector of them) is dark: the slowest of cell-light-in, nil when some
  dark cell is never lit. Cells with no light data are left out. 0 when none is dark."
  [p cells]
  (let [cells (if (map? cells) [cells] cells)
        ts (keep (fn [c] (if-let [t (cell-light-in p c)] t (when (some? (block-light-at p c)) :never))) cells)]
    (cond (some #{:never} ts) nil
          (seq ts) (apply max ts)
          :else 0)))

(defn unsafe-night?
  "Awake, not under a roof and not buried, with darkness at the feet that waiting lifts (light-in is above 0). A cell
  no sky light reaches (a cave) is dark whatever the hour: waiting is no cure. Without light data at the feet, the clock."
  [p roof-height]
  (and (not (sleeping? p))
       (not (roofed? p roof-height)) (not (buried? p))
       (if (sky-light-at p (feet p))
         (pos? (or (light-in p (feet p)) 0))
         (night? p))))

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
