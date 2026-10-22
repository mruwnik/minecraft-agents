(ns triggers.survival.dusk-home
  "The dusk-home trigger: before night, a body under the open sky whose remembered :home (else :bed) is within :limit
  blocks leaves for it once the day left no longer covers the walk (:slack times the straight line at :speed blocks a
  second) plus :margin-s seconds. A place beyond :limit (go-home's own) never fires it: the body digs in where night finds it. Stored args missing a key (an older entry) fall back to defaults. It does not hold at night (the night job has it), within :arrive
  blocks of the place, or already sheltered (underground, or shut in a closed room)."
  (:require [jobs.lib.body :as body]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def defaults sh/home-due-defaults)

(defn dusk-home
  [p view args]
  (boolean
   (and (not (body/recovery-pending? view :dusk-home)) (not (sh-sense/sleeping? p))
        (sh/home-due-at? p view (u/self-pos {:primitives p}) args)
        (not (or (sh-sense/buried? p) (sh-sense/shut-in-room? p))))))
