(ns jobs.lib.shelter.inside
  "Whether the body is shut in its shelter, safe from the night where it stands, and what a walk would leave."
  (:require [engine.ctx :as ctx]
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

(defn safe-from-night?
  "Whether the body is safe from the night where it stands: a roof within roof-height (default default-roof-height) over it, buried,
  or shut in a closed room (shut-in-room?), never with the head inside a block. The one notion the night trigger,
  night-sheltered and walk-leaves-shelter? share."
  ([p] (safe-from-night? p sh-sense/default-roof-height))
  ([p roof-height] (and (not (sh-sense/head-in-block? p)) (or (not (sh-sense/unsafe-night? p roof-height)) (sh-sense/shut-in-room? p)))))

(defn night-sheltered
  "At night, awake and safe from it (roofed or buried, or shut-in-room?), the kind of shelter the body is in (:dug-in
  the one shelter says it is shut in, :room, :underground, :roofed), else nil: outdoor work waits for day then."
  [p shelter]
  (when (and (sh-sense/dark-outside? p) (not (sh-sense/sleeping? p)) (safe-from-night? p))
    (cond
      (in-own-shelter p shelter) :dug-in
      (sh-sense/shut-in-room? p) :room
      (sh-sense/buried? p) :underground
      :else :roofed)))

(defn walk-leaves-shelter?
  "At night, awake and safe from it (roofed or buried, or shut-in-room?), the shelter a walk to goal ({:x :y :z}, or {:x
  :z} a column) within dist would leave, else nil. A goal within dist of the feet leaves none. :dug-in: shelter (the
  latest :shelter entry) is the one the body is shut in. :room: no cell of the closed room is within dist of the goal.
  :underground: buried, and the goal is not below the feet (unless open-air-above?), has nothing solid within default-roof-height over it, and is no solid block with no open cell seen over it (a wall in the rock: a walk deeper stays underground), and gets no sky light (a room of rock taller than the roof height is still underground). :roofed: any other roof."
  [p shelter goal dist]
  (when (and (sh-sense/dark-outside? p) (not (sh-sense/sleeping? p)) (safe-from-night? p))
    (let [goal (update-vals goal js/Math.floor)
          near? (fn [cell] (if (:y goal) (u/within? cell goal dist) (u/within? (assoc cell :y 0) (assoc goal :y 0) dist)))
          room (when (sh-sense/shut-in-room? p) (reach/room p (sh-sense/feet p)))]
      (cond
        (near? (sh-sense/feet p)) nil
        (in-own-shelter p shelter) :dug-in
        room (when-not (some (fn [[x y z]] (near? {:x x :y y :z z})) (:cells room)) :room)
        (sh-sense/buried? p) (when-not (and (:y goal) (or (and (< (:y goal) (:y (sh-sense/feet p))) (not (sh-sense/open-air-above? p goal (:y (sh-sense/feet p)))))
                                                 (some #(sh-sense/solid-at? p (update goal :y + %)) (range 1 (inc sh-sense/default-roof-height)))
                                                 (and (sh-sense/solid-at? p goal) (not (sh-sense/seen-open-over? p goal)))
                                                 (= 0 (sh-sense/sky-light-at p goal))))
                      :underground)
        :else :roofed))))

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
