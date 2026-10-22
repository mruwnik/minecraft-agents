(ns triggers.survival.dusk-home
  "The dusk-home trigger: before night, a body under the open sky whose remembered :home (else :bed) is within
  sh-clock/home-limit blocks leaves for it once the day left no longer covers the walk (timing: setting
  jobs.lib.shelter/home-due). A place beyond the limit (go-home's own) never fires it: the body digs in where night finds
  it. It does not hold at night (the night job has it), within :arrive blocks of the place, already sheltered
  (underground, or shut in a closed room). It starts go-home with :commute?, which stays when the body is in the ground
  and go-to's quote of the walk there and back is dearer than sheltering here (jobs.lib.cost/commute-s)."
  (:require [jobs.lib.body :as body]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(defn dusk-home
  [p view _args]
  (boolean
   (and (not (body/recovery-pending? view :dusk-home)) (not (sh-sense/sleeping? p))
        (sh/home-due-at? p view (u/self-pos {:primitives p}))
        (not (or (sh-sense/buried? p) (sh-sense/shut-in-room? p))))))
