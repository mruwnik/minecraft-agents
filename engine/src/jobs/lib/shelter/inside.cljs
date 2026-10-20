(ns jobs.lib.shelter.inside
  "Whether the body is shut in its shelter, safe from the night where it stands, and what a walk would leave."
  (:require [engine.ctx :as ctx]
            [jobs.lib.click :as click]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn shut-in?
  "Whether the body is still shut in by its shelter entry. Never for a mended :room (its door is the way out) or an entry
  that ended :sealed false. With a
  pit :start, when below that height and walled in (a pit left by a stair is open). With a :door, when a door cell is solid. With neither, when under a solid
  block within default-roof-height."
  [p {:keys [start door room sealed]}]
  (cond
    (or room (false? sealed)) false
    start (and (< (:y (sh-sense/feet p)) (:y start)) (reach/enclosed? p))
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

(def tiny-room-cells
  "A closed room of at most this many cells is small enough to need no light: no mob spawns in it beside the body."
  2)

(defn dark-spawn-inside?
  "Whether some cell of the closed room round the feet where a mob could spawn (sh-sense/spawnable?) is dark, as the body
  knows it (sh-sense/lit-here?)."
  [p room]
  (boolean (some #(not (sh-sense/lit-here? p %))
                 (sh-sense/spawnable-cells p (map (fn [[x y z]] {:x x :y y :z z}) (:cells room))))))

(defn safe-from-night?
  "Whether the body is safe from the night where it stands (owner Q345): shut in a closed room under a roof
  (sh-sense/shut-in-room?) with no open door, so no way in for a walking mob, that is tiny (at most tiny-room-cells cells) or has no dark
  spawnable cell. A roof with open sides, a buried tunnel, a dark cave or a big dark room is not. The one test the night
  trigger, night-sheltered, walk-leaves-shelter?, the night job's end and the hold share."
  [p]
  (boolean
   (and (not (sh-sense/head-in-block? p)) (sh-sense/shut-in-room? p)
        (let [room (reach/room p (sh-sense/feet p))]
          (and (not-any? #(open-door? p %) (:doors room))
               (or (<= (count (:cells room)) tiny-room-cells) (not (dark-spawn-inside? p room))))))))

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
        :else (when-not (some (fn [[x y z]] (near? {:x x :y y :z z})) (:cells (reach/room p (sh-sense/feet p)))) :room)))))

(def trapped-policy
  "Policy of the :shelter-trapped entries written when leave! found no way out: up to 3, five minutes."
  {:cap 3 :ttl 300000})

(defn shut-in-by-day?
  "By day: the body is shut in its own shelter (data) and no :shelter-trapped entry (data) says leave! found no way
  out of this cell. Never at night."
  [p shelter trapped]
  (boolean (and (not (sh-sense/dark-outside? p))
                (in-own-shelter p shelter)
                (not= (:pos shelter) (:pos trapped)))))

(defn shelter-hint
  "{:inside-own-shelter pos :hint text} when the body is sealed in its own shelter (a go-to, attack or hunt that cannot
  reach its target says so), else nil."
  [c]
  (when-let [{{:keys [x y z]} :pos :as entry} (sheltered-in c)]
    {:inside-own-shelter (:pos entry)
     :hint (str "the body is sealed in its own shelter at [" x " " y " " z "]: the shelter job lets it out by day "
                "(jobs.survival.night)")}))
