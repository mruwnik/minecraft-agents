(ns engine.fake.doors
  "Doors, gates and trapdoors in the fake world, over world data {:blocks {[x y z] name} :states {[x y z] props}}: the
  block's :open state decides whether a body passes. An open one has no collision for the fake steer, a shut one is a
  wall; the planner's snapshot reads the state; a click on a door flips both of its halves, as the server does.
  Test-only; ported from the deleted js/fake-doors.mjs (a trapdoor is climbed only over a ladder of its own facing).")

(def openable-re #"_(fence_gate|door|trapdoor)$")
;; the block properties the planner's state ids carry
(def path-keys [:open :half :facing :hinge :face :powered :drag])
;; what opens a door from a distance: the planner looks for these beside an iron door
(def activator-re #"_button$|^lever$")

(defn openable? [name] (boolean (re-find openable-re (or name ""))))

(defn open?
  "Does the block at pos let a body through: an open door, gate or trapdoor."
  [w pos]
  (and (openable? (get-in w [:blocks pos])) (true? (get-in w [:states pos :open]))))

(defn climbs-through?
  "An open trapdoor over a ladder of its own facing is climbable, as on the server."
  [w [x y z :as pos]]
  (let [below [x (dec y) z]]
    (boolean (and (open? w pos)
                  (re-find #"_trapdoor$" (get-in w [:blocks pos]))
                  (= "ladder" (get-in w [:blocks below]))
                  (= (get-in w [:states pos :facing]) (get-in w [:states below :facing]))))))

(defn path-props
  "The properties of the block at pos for a planner state id, {} for a block that is neither openable, a button or
  lever, a ladder, nor a bubble column (a trapdoor over it is judged against its facing)."
  [w pos]
  (let [name (get-in w [:blocks pos] "")]
    (if (or (openable? name) (re-find activator-re name) (= name "ladder") (= name "bubble_column"))
      (select-keys (get-in w [:states pos]) path-keys)
      {})))

(defn set-open
  "World with `open` set to value on the block at pos and, for a door, on its other half."
  [w [x y z :as pos] open]
  (let [name (get-in w [:blocks pos])
        other (if (= "upper" (get-in w [:states pos :half])) [x (dec y) z] [x (inc y) z])
        cells (cond-> [pos]
                (and (re-find #"_door$" (or name "")) (= name (get-in w [:blocks other]))) (conj other))]
    (reduce #(assoc-in %1 [:states %2 :open] open) w cells)))

(defn flip-open
  "World with `open` flipped on the block at pos and, for a door, on its other half."
  [w pos]
  (set-open w pos (not (get-in w [:states pos :open]))))

;; Wiring: world :wires {activator-pos [door-pos ...]} says which doors a button or plate works. A button opens them (and
;; shuts them again after :pulse-moves steer ticks when the world has that); a plate opens them while the body stands on
;; it and for :plate-hold ticks after.
(def plate-hold 15)

(defn set-wired
  "World with the doors wired to the activator at pos open or shut, and the activator powered as open says."
  [w pos open]
  (as-> (assoc-in w [:states pos :powered] open) w
    (reduce #(set-open %1 %2 open) w (get-in w [:wires pos]))))

(defn press
  "World after a button at pos is pressed: powered, its wired doors open, and a timer to shut them when :pulse-moves is set (a vector: one pulse per press, the last one repeating)."
  [w pos]
  (let [pulses (:pulse-moves w)
        w (set-wired w pos true)]
    (cond
      (nil? pulses) w
      (number? pulses) (assoc-in w [:timers pos] pulses)
      :else (-> (assoc-in w [:timers pos] (first pulses))
                (assoc :pulse-moves (if (next pulses) (vec (next pulses)) (last pulses)))))))

(defn tick-wires
  "World after one steer tick with the body in cell: a plate under it opens its doors; timers run down and shut theirs."
  [w cell]
  (let [w (reduce (fn [w pos]
                    (if (and (re-find #"_pressure_plate$" (get-in w [:blocks pos] "")) (= pos cell))
                      (-> (set-wired w pos true) (assoc-in [:timers pos] plate-hold))
                      w))
                  w (keys (:wires w)))]
    (reduce (fn [w [pos n]]
              (if (> n 1)
                (assoc-in w [:timers pos] (dec n))
                (-> (update w :timers dissoc pos) (set-wired pos false))))
            w (:timers w))))
