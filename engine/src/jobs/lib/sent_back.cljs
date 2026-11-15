(ns jobs.lib.sent-back
  "A walk the body was sent back from at dusk or by night, learned from its own walks: a cut walk that resumes in shelter
  (a closed room, the ground) or home when the cut found it outside that shelter, with the sky dark or home due where the
  cut found it and no hostile near the way (that one is go-to's own send-back), was sent back for the night. One :sent-back entry
  {:to cell :place kind :at time-of-day :until world-day-time}, kept until the world clock reaches that morning or the time of day leaves that
  night (a sleep or a time set ends it early), holds walks out of that place (walk-risk): walked
  now, the same cut sends them back again."
  (:require [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(defn until-morning-ticks
  "Ticks from time of day t to the next morning (sh-clock/morning-tick)."
  [t]
  (mod (- sh-clock/morning-tick t) sh-clock/night-end-tick))

(defn policy
  "One entry; its ttl, in engine time, is only a backstop to the world-clock :until."
  [t]
  {:cap 1 :ttl (max 1 (settings/ticks->ms (until-morning-ticks t)))})

(defn day-time
  "The body's absolute world clock in ticks (self().dayTime), nil when it has none."
  [p]
  (.-dayTime (.self p)))

(defn remaining-ticks
  "Ticks until the entry's morning: by the time of day, 0 once it lies outside the night from the entry's :at (a clock set
  back or forward past it) or the absolute clock passed :until (a sleep, a whole day gone)."
  [p {:keys [until at]}]
  (let [dt (day-time p)
        left (until-morning-ticks (.-timeOfDay (.self p)))]
    (cond (and at (> left (until-morning-ticks at))) 0
          (and dt until) (max 0 (min left (- until dt)))
          :else left)))

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
  ({:x :y :z}), sheltered by another kind than where it restarts (a meal or a fight in place is no send-back); hostiles?,
  whether a hostile is known near the way. The entry, else nil."
  [c from hostiles?]
  (let [p (:primitives c)
        view (ctx/view c)
        pos (u/self-pos c)
        kind (place-kind p view pos)
        t (.-timeOfDay (.self p))]
    (when (and kind (not= kind (place-kind p view from)) (not hostiles?) (or (sh-sense/dark-outside? p) (sh/home-due-at? p view from)))
      (let [dt (day-time p)
            entry {:to (sh-sense/feet p) :place kind :at t :until (some-> dt (+ (until-morning-ticks t))) :text (str "sent back into " (name kind) " at " t "; walks out wait for dawn")}]
        (ctx/remember! c :sent-back entry (policy t))
        (ctx/emit! c :walk.sent-back :info entry)
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
      (when (and (or (nil? (day-time p)) (nil? (:until e)) (pos? (remaining-ticks p e)))
                 (in-place? p e (u/self-pos c)) (not (in-place? p e goal)))
        e))))

(defn wait-s
  "Seconds until the entry's morning, 0 once it has passed."
  [c]
  (let [p (:primitives c)]
    (sh-clock/ticks->s (remaining-ticks p (:data (ctx/latest c :sent-back))))))
