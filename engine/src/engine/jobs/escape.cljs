(ns engine.jobs.escape
  "Reads for a body that cannot walk to its goal from where it stands, so jobs.movement.go-to can pick how to make a
  way (its escalation):
  - pit-depth: how many levels up from the feet are walled on at least three sides.
  - door: the cells of a thin wall straight ahead with a floor beyond it (jobs.access.clear-path digs them).
  - stair-heading: a heading with a solid block in front to stair up on (jobs.access.stair).
  - wall-spot: the nearest cell beside a wall, for a body in a hollow with no wall next to it.
  - choose: the escalation for a body here and a goal.
  Cells are [x y z]. block-at maps a cell to its block name (nil when not loaded)."
  (:require [engine.access.rules :as rules]
            [engine.jobs.reach :as reach]
            [engine.jobs.util :as u]))

(def max-depth 8)

(def wall-search-radius
  "How far (blocks along x and along z) wall-spot looks."
  24)

(def cardinals [[1 0] [-1 0] [0 1] [0 -1]])

(def heading-names {[1 0] :east [-1 0] :west [0 1] :south [0 -1] :north})

(def pillar-items
  "Blocks worth pillaring with, most common first. No sand or gravel: they fall."
  ["dirt" "cobblestone" "cobbled_deepslate" "stone" "deepslate" "netherrack" "andesite" "diorite" "granite"
   "oak_planks" "spruce_planks" "birch_planks"])

(def protected
  "Blocks a way out never digs: doors, gates, trapdoors, beds, containers, signs, and what cannot be broken."
  #"_door$|_gate$|_trapdoor$|_bed$|chest$|barrel$|shulker_box$|furnace$|^smoker$|^hopper$|^dispenser$|^dropper$|^brewing_stand$|^lectern$|_sign$|^spawner$|^bedrock$|^barrier$|portal|^command_block$|^structure_block$|^jigsaw$")

(defn block-at-of
  "block-at over primitives p."
  [p]
  (fn [[x y z]] (u/block-name p {:x x :y y :z z})))

(defn add [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])

(defn up [cell n] (add cell [0 n 0]))

(defn ahead [cell [dx dz] k] (add cell [(* k dx) 0 (* k dz)]))

(defn solid?
  "A loaded block that is neither air, a fluid nor a plant a body walks through."
  [n]
  (boolean (and n (not (rules/replaceable n)) (not (rules/fluids n)))))

(defn passable? [n] (boolean (and n (rules/replaceable n) (not (rules/fluids n)))))

(defn protected? [n] (boolean (and n (re-find protected n))))

(defn heading
  "Unit step [dx dz] along the larger horizontal axis from cell a toward cell b, or nil when b is straight above or
  below."
  [[ax _ az] [bx _ bz]]
  (let [dx (- bx ax) dz (- bz az)]
    (cond
      (and (zero? dx) (zero? dz)) nil
      (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0]
      :else [0 (js/Math.sign dz)])))

(defn walled-at?
  "At least three of the four sides of cell are solid."
  [block-at cell]
  (>= (count (filter #(solid? (block-at (ahead cell % 1))) cardinals)) 3))

(defn pit-depth
  "Levels up from the feet, 0, 1, ..., walled on at least three sides, at most max-depth."
  [block-at feet]
  (count (take-while #(walled-at? block-at (up feet %)) (range max-depth))))

(defn walled-side? [block-at cell] (boolean (some #(solid? (block-at (ahead cell % 1))) cardinals)))

(defn door
  "A door through a wall straight ahead of feet along dir: {:cells [...] :through cell}, the wall's solid cells at feet
  and head height (head first, row by row) and the first cell beyond with room for the body and a floor. nil when
  the cell in front is open, the wall is thicker than max-thick, a row has no floor, or a cell is protected."
  [block-at feet dir max-thick]
  (loop [k 1 cells []]
    (let [row (ahead feet dir k)
          pair [(up row 1) row]
          names (map block-at pair)]
      (cond
        (not (rules/solid-floor? block-at (up row -1))) nil
        (some nil? names) nil
        (every? passable? names) (when (> k 1) {:cells cells :through row})
        (> k max-thick) nil
        (some protected? names) nil
        (some rules/fluids names) nil
        :else (recur (inc k) (into cells (filter #(solid? (block-at %)) pair)))))))

(defn stair-heading
  "The first heading of dirs (each [dx dz]) with a solid, unprotected block in front at feet height to stand on and
  no protected block in the step's cut, as a keyword (:east ...), or nil."
  [block-at feet dirs]
  (some (fn [d]
          (let [front (ahead feet d 1)
                cut [(up feet 2) (up front 1) (up front 2)]]
            (when (and (solid? (block-at front)) (not (protected? (block-at front)))
                       (not-any? #(protected? (block-at %)) cut))
              (heading-names d))))
        dirs))

(defn wall-spot
  "The nearest cell (breadth-first through standable cells, at most wall-search-radius away along x and z) that has
  a solid side: one whose side along dir is solid when there is one, else the first with any. nil when none."
  [p feet dir]
  (let [block-at (block-at-of p)
        [hx _ hz] feet
        near? (fn [[x _ z]] (and (<= (js/Math.abs (- x hx)) wall-search-radius)
                                 (<= (js/Math.abs (- z hz)) wall-search-radius)))
        standable? (fn [[x y z]] (reach/standable-cell? p {:x x :y y :z z}))
        dirs (distinct (cond->> cardinals dir (cons dir)))]
    (loop [queue #queue [feet] seen #{feet} fallback nil]
      (if-let [cell (peek queue)]
        (let [here? (= cell feet)]
          (if (and dir (not here?) (solid? (block-at (ahead cell dir 1))))
            cell
            (let [fallback (or fallback (when (and (not here?) (walled-side? block-at cell)) cell))
                  nbrs (->> dirs (map #(ahead cell % 1)) (filter #(and (not (seen %)) (near? %) (standable? %))))]
              (recur (into (pop queue) nbrs) (into seen nbrs) fallback))))
        fallback))))

(defn pillar-item
  "{:item :count}: the carried pillar block with the largest count, or nil."
  [p]
  (let [have (->> (u/inventory p)
                  (filter #(some #{(:name %)} pillar-items))
                  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {}))]
    (when (seq have)
      (let [[item n] (apply max-key val have)] {:item item :count n}))))

(def max-door 3)

(defn choose
  "How a body at feet (a cell) that cannot walk to goal (a cell) can make a way, cheapest first:
  - {:step :pillar :height d :item}: in a pit d deep with room above the head and at least d pillar blocks carried.
  - {:step :clear-path :heading kw}: a wall at most max-door thick straight toward the goal, with floor beyond.
  - {:step :stair :heading kw :steps n}: a block in front to stair up on, n the pit's depth (in no pit: how far the
    goal is above, at most max-depth).
  - {:step :approach :pos cell}: no side of the body's cell is solid: walk to wall-spot first.
  - {:step :none}: none of these."
  [p feet goal]
  (let [block-at (block-at-of p)
        dir (heading feet goal)
        depth (pit-depth block-at feet)
        {:keys [item count]} (pillar-item p)
        rise (if (pos? depth) depth (min max-depth (max 0 (- (second goal) (second feet)))))
        stair (when (pos? rise) (stair-heading block-at feet (distinct (cond->> cardinals dir (cons dir)))))]
    (cond
      (and (pos? depth) item (>= count depth) (passable? (block-at (up feet 2))))
      {:step :pillar :height depth :item item}

      (and dir (door block-at feet dir max-door))
      {:step :clear-path :heading (heading-names dir)}

      stair
      {:step :stair :heading stair :steps rise}

      (not (walled-side? block-at feet))
      (if-let [spot (wall-spot p feet dir)] {:step :approach :pos spot} {:step :none})

      :else {:step :none})))
