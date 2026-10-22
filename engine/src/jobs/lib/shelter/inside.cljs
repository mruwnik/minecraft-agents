(ns jobs.lib.shelter.inside
  "Whether the body is shut in its shelter, safe from the night where it stands, and what a walk would leave."
  (:require [engine.ctx :as ctx]
            [jobs.lib.breath :as breath]
            [jobs.lib.click :as click]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.util :as u]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn shut-in?
  "Whether the body is still shut in by its shelter entry. Never for a mended :room (its door is the way out) or an entry
  that ended :sealed false. With a
  pit :start, when below that height and walled in (a pit left by a stair is open). With a :door, when a door cell is solid. With neither, when under a solid
  block within default-roof-height."
  [p {:keys [start door room sealed]}]
  (cond
    (or room (false? sealed)) false
    start (and (< (:y (sh-sense/feet p)) (:y start)) (reach-room/enclosed? p))
    door (boolean (some #(sh-sense/solid-at? p %) door))
    :else (sh-sense/roofed? p sh-sense/default-roof-height)))

(defn in-own-shelter
  "The shelter entry data when the body stands in its :pos and is shut in it, else nil."
  [p entry]
  (when (and entry (= (:pos entry) (sh-sense/feet p)) (shut-in? p entry))
    entry))

(defn sheltered-in
  "The body's latest :shelter entry when the body stands in its :pos and is shut in it, else nil."
  [c]
  (in-own-shelter (:primitives c) (:data (ctx/latest c :shelter))))

(defn open-door?
  "Whether the {:cell} door entry stands open (a door or gate seen open)."
  [p {:keys [cell]}]
  (let [b (u/seen-block p cell)]
    (and b (#{:openable :iron} (click/kind-of (.-name b))) (click/reached? :open (click/props-of b)))))

(def spawn-free-radius
  "Blocks round a player in which no mob spawns (the vanilla rule, spherical): a dark cell nearer than this to the body is no danger."
  24)

(def dark-cells-of
  "room -> its dark spawnable cells, read once per room object (a region's room is shared by all its cells)."
  (js/WeakMap.))

(defn dark-spawnable-cells
  "The cells [x y z] of the room where a mob could spawn (sh-sense/spawnable?) and that are dark as the body knows them
  (sh-sense/lit-here?)."
  [p room]
  (or (.get dark-cells-of room)
      (let [cells (filterv (fn [[x y z]] (let [cell {:x x :y y :z z}] (and (sh-sense/spawnable? p cell) (not (sh-sense/lit-here? p cell)))))
                           (:cells room))]
        (.set dark-cells-of room cells)
        cells)))

(def yard-cells
  "Cells the flood may hold for a place with no roof over it (a walled yard); a roofed one may hold reach-room/wall-cells,
  since open sky ground floods without end and the cost is the flood."
  500)

(defn dark-spawn-beyond?
  "Whether some cell of the room where a mob could spawn is dark (dark-spawnable-cells) and farther than spawn-free-radius
  from `at` ({:x :y :z}, where the body is or will be)."
  [p room at]
  (let [r2 (* spawn-free-radius spawn-free-radius)]
    (boolean (some (fn [[x y z]] (> (+ (js/Math.pow (- x (:x at)) 2) (js/Math.pow (- y (:y at)) 2) (js/Math.pow (- z (:z at)) 2)) r2))
                   (dark-spawnable-cells p room)))))

(defn sealed-region
  "The closed, fully seen region round `at` ({:x :y :z}) with no open door and no way in for a walking mob or a spider
  (reach-room/walled-region: up to reach-room/wall-cells cells under a roof, yard-cells in the open, judged once for all
  its cells), else nil: safe but for the dark. A region too big to flood or with an unseen edge is none: unseen is never safe."
  [p at]
  (let [{:keys [room checks]} (reach-room/walled-region p at (if (sh-sense/room-roofed? p at) reach-room/wall-cells yard-cells))]
    (when (and room (not-any? #(open-door? p %) (:doors room)) @checks)
      room)))

(defn walled-off-at?
  "Whether a body at `at` has no dark spawnable cell a mob could walk from to it (owner Q345 for work exposure: nothing
  spawns within spawn-free-radius, so only dark cells beyond it that a mob reaches on foot count): a sealed-region with no
  dark spawnable cell in it farther than spawn-free-radius from `at`."
  [p at]
  (boolean (when-let [room (sealed-region p at)]
             (not (dark-spawn-beyond? p room at)))))

(defn safe-room-at?
  "Whether a body at `at` would be safe from the night (owner Q345): walled-off-at? under a roof (sh-sense/room-roofed?).
  A dark 5x5 room is safe, a dark hall reaching 30 blocks is not, nor is a roof with open sides or a pit a walker on its
  rim steps down into."
  [p at]
  (boolean (and (sh-sense/room-roofed? p at) (walled-off-at? p at))))

(defn dark-room-at?
  "Whether `at` is in a room under a roof that a sealed-region closes and only its dark keeps from being safe-room-at?:
  torches fix it."
  [p at]
  (boolean (when-let [room (and (sh-sense/room-roofed? p at) (sealed-region p at))]
             (dark-spawn-beyond? p room at))))

(defn safe-from-night?
  "Whether the body is safe from the night where it stands: safe-room-at? its feet, its head not inside a block, not afloat
  (a swimmer is in no shelter). The one test the night trigger, night-sheltered, walk-leaves-shelter?, the night job's end and the hold share."
  [p]
  (boolean (and (not (breath/in-water? p)) (not (sh-sense/head-in-block? p)) (safe-room-at? p (sh-sense/feet p)))))

(defn night-sheltered
  "At night, awake and safe from it (safe-from-night?), the kind of shelter the body is in (:dug-in the one shelter says
  it is shut in, else :room), else nil: outdoor work waits for day then."
  [p shelter]
  (when (and (sh-sense/dark-outside? p) (not (sh-sense/sleeping? p)) (safe-from-night? p))
    (if (in-own-shelter p shelter) :dug-in :room)))

(defn walk-leaves-shelter?
  "At night, awake and safe from it (safe-from-night?), the shelter a walk to goal ({:x :y :z}, or {:x :z} a column)
  within dist would leave, else nil. A goal within dist of the feet leaves none. :dug-in: shelter (the latest :shelter
  entry) is the one the body is shut in. :room: no cell of the closed room is within dist of the goal."
  [p shelter goal dist]
  (when (and (sh-sense/dark-outside? p) (not (sh-sense/sleeping? p)) (safe-from-night? p))
    (let [goal (update-vals goal js/Math.floor)
          near? (fn [cell] (if (:y goal) (u/within? cell goal dist) (u/within? (assoc cell :y 0) (assoc goal :y 0) dist)))]
      (cond
        (near? (sh-sense/feet p)) nil
        (in-own-shelter p shelter) :dug-in
        :else (when-not (some (fn [[x y z]] (near? {:x x :y y :z z})) (:cells (reach-room/room p (sh-sense/feet p)))) :room)))))

(def trapped-policy
  "Policy of the :shelter-trapped entries written when leave! found no way out: up to 3, five minutes."
  {:cap 3 :ttl 300000})

(def left-policy
  "Policy of the :shelter-trapped entries written when leave-shelter got out: the walk back through its cell is no new dawn for half a day."
  {:cap 3 :ttl (/ sh-clock/ms-per-day 2)})

(defn shut-in-own?
  "The body is shut in its own shelter (data) and no :shelter-trapped entry (data) says leave! found no way out of this
  cell. Day or night."
  [p shelter trapped]
  (boolean (and (in-own-shelter p shelter)
                (not= (:pos shelter) (:pos trapped)))))

(defn shut-in-by-day?
  "shut-in-own?, never at night."
  [p shelter trapped]
  (and (not (sh-sense/dark-outside? p)) (shut-in-own? p shelter trapped)))

(defn shelter-hint
  "{:inside-own-shelter pos :hint text} when the body is sealed in its own shelter (a go-to, attack or hunt that cannot
  reach its target says so), else nil."
  [c]
  (when-let [{{:keys [x y z]} :pos :as entry} (sheltered-in c)]
    {:inside-own-shelter (:pos entry)
     :hint (str "the body is sealed in its own shelter at [" x " " y " " z "]: the shelter job lets it out by day "
                "(jobs.survival.night)")}))
