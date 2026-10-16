(ns triggers.survival.dusk-home
  "The dusk-home trigger: before night, a body under the open sky whose remembered :home (else :bed) is within :limit
  blocks leaves for it once the day left no longer covers the walk (:slack times the straight line at :speed blocks a
  second) plus :margin-s seconds. A place beyond :limit (go-home's own) never fires it: the body digs in where night finds it. Stored args missing a key (an older entry) fall back to defaults. It does not hold at night (the night job has it), within :arrive
  blocks of the place, or already sheltered (a roof within sh/default-roof-height, or underground)."
  (:require [jobs.lib.body :as body]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(def defaults {:margin-s 60 :speed 4.3 :slack 1.4 :arrive 4})

(defn dusk-home
  [p view args]
  (let [{:keys [limit margin-s speed slack arrive]} (merge defaults {:limit sh/home-limit} args)
        self (.self p)
        left-s (/ (- sh/dusk-tick (.-timeOfDay self)) 20)
        pos (sh/home-target view)]
    (boolean
     (and pos (pos? left-s) (not (body/recovery-pending? view :dusk-home)) (not (sh/sleeping? p))
          (let [d (u/dist (u/self-pos {:primitives p}) pos)]
            (and (< arrive d) (<= d limit)
                 (<= left-s (+ margin-s (/ (* slack d) speed)))
                 (not (sh/sheltered-here? p))))))))
