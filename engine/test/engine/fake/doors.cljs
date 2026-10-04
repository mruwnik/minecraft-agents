(ns engine.fake.doors
  "Doors, gates and trapdoors in the fake world, over world data {:blocks {[x y z] name} :states {[x y z] props}}: the
  block's :open state decides whether a body passes. An open one has no collision for the fake steer, a shut one is a
  wall; the planner's snapshot reads the state; a click on a door flips both of its halves, as the server does.
  Test-only; ported from the deleted js/fake-doors.mjs (a trapdoor is climbed only over a ladder of its own facing).")

(def openable-re #"_(fence_gate|door|trapdoor)$")
;; the block properties the planner's state ids carry
(def path-keys [:open :half :facing :hinge :face :powered])
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
  lever, nor a ladder (a trapdoor over it is judged against its facing)."
  [w pos]
  (let [name (get-in w [:blocks pos] "")]
    (if (or (openable? name) (re-find activator-re name) (= name "ladder"))
      (select-keys (get-in w [:states pos]) path-keys)
      {})))

(defn flip-open
  "World with `open` flipped on the block at pos and, for a door, on its other half."
  [w [x y z :as pos]]
  (let [name (get-in w [:blocks pos])
        open (not (get-in w [:states pos :open]))
        other (if (= "upper" (get-in w [:states pos :half])) [x (dec y) z] [x (inc y) z])
        cells (cond-> [pos]
                (and (re-find #"_door$" (or name "")) (= name (get-in w [:blocks other]))) (conj other))]
    (reduce #(assoc-in %1 [:states %2 :open] open) w cells)))
