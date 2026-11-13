(ns jobs.lib.sent-back
  "A walk the body was sent back from at dusk or by night, learned from its own walks: a cut walk that resumes farther
  from its goal, in shelter (a closed room, the ground) or home, with the sky dark or home due where the cut found it and
  no hostile near the way (that one is go-to's own send-back), was sent back for the night. One :sent-back entry
  {:to cell :place kind :at time-of-day}, kept until the morning, holds walks out of that place (walk-risk): walked
  now, the same cut sends them back again."
  (:require [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(defn policy
  "One entry, until the morning (sh-clock/morning-tick) after time of day t."
  [t]
  {:cap 1 :ttl (max 1 (settings/ticks->ms (mod (- sh-clock/morning-tick t) sh-clock/night-end-tick)))})

(defn place-kind
  "What shelters a body at pos: :home (sh/home-at?), :room (a closed roofed room), :ground (sealed from the sky), else nil."
  [p view pos]
  (let [cell (update-vals pos js/Math.floor)
        {:keys [arrive]} (settings/get sh/settings ::sh/home-due)]
    (cond (sh/home-at? p view pos arrive) :home
          (sh-sense/shut-in-room? p cell) :room
          (sh-sense/buried-at? p cell) :ground)))

(defn note!
  "Write the :sent-back entry when the walk's restart is one (see the ns doc): from, where the cut found the body
  ({:x :y :z}); hostiles?, whether a hostile is known near the way. The entry, else nil."
  [c from hostiles?]
  (let [p (:primitives c)
        view (ctx/view c)
        pos (u/self-pos c)
        kind (place-kind p view pos)
        t (.-timeOfDay (.self p))]
    (when (and kind (not hostiles?) (or (sh-sense/dark-outside? p) (sh/home-due-at? p view from)))
      (let [entry {:to (sh-sense/feet p) :place kind :at t}]
        (ctx/remember! c :sent-back entry (policy t))
        (ctx/emit! c :walk.sent-back :info (assoc entry :text (str "the body was sent back into " (name kind) " at " t "; walks out of it wait for the morning")))
        entry))))

(defn in-place?
  "Whether pos ({:x :y :z}, or {:x :z} a column) lies in the entry's place: within a block of its closed room, or for
  one in no closed room, within ::sh/home-due :arrive blocks of its cell."
  [p {:keys [to]} pos]
  (let [goal (update-vals pos js/Math.floor)]
    (if-let [cells (:cells (reach-room/room p to))]
      (boolean (some (fn [[x y z]] (sh-inside/near-goal? goal 1 {:x x :y y :z z})) cells))
      (sh-inside/near-goal? goal (:arrive (settings/get sh/settings ::sh/home-due)) to))))

(defn holding
  "The live :sent-back entry's data when the body stands in its place and a walk to goal leaves it, else nil."
  [c goal]
  (let [p (:primitives c)]
    (when-let [e (:data (ctx/latest c :sent-back))]
      (when (and (in-place? p e (u/self-pos c)) (not (in-place? p e goal)))
        e))))

(defn wait-s
  "Seconds until the morning ends the entry: to nightfall, then the night left (its mean)."
  [c]
  (let [t (.-timeOfDay (.self (:primitives c)))
        dusk-s (sh-clock/ticks->s (max 0 (- sh-clock/dusk-tick t)))]
    (+ dusk-s (second (get-in (sh-clock/night-left-quote false (max t sh-clock/dusk-tick)) [:uses :job-time])))))
