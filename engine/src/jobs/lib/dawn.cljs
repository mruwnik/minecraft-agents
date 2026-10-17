(ns jobs.lib.dawn
  "The dawn hold of comfort errands, triggers that send a body out of its shelter by day (needs-weapon, needs-bed).
  calm? holds once ::grace-ticks passed since the night ended (undead burn, spiders calm) and no hostile is known
  within ::hostile-radius (remembered and heard ones included), unless ::with-hostile is set."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.danger :as danger]
            [jobs.lib.shelter.clock :as sh-clock]))

(a/defargs settings
  {::grace-ticks {:default 1500 :doc "Game ticks after the night ends before a comfort errand may leave shelter." :spec (a/int-in 0 nil)}
   ::hostile-radius {:default 40 :doc "Blocks within which a known hostile holds a comfort errand (a zombie follows 35)." :spec (a/int-in 1 nil)}
   ::with-hostile {:default false :doc "Whether a comfort errand may start while a known hostile is near." :spec boolean?}})

(defn ticks-since-dawn [time-of-day] (mod (- time-of-day sh-clock/morning-tick) 24000))

(defn calm?
  [p]
  (boolean
   (and (>= (ticks-since-dawn (.-timeOfDay (.self p))) (settings/get settings ::grace-ticks))
        (or (settings/get settings ::with-hostile)
            (empty? (danger/known-hostiles p (settings/get settings ::hostile-radius) {:ranged-radius (danger/ranged-radius)}))))))

(defn hostile-hold?
  "Whether a body in shelter stays in at first light: within ::grace-ticks of the night's end, with a known hostile
  within ::hostile-radius, unless ::with-hostile is set."
  [p]
  (boolean
   (and (< (ticks-since-dawn (.-timeOfDay (.self p))) (settings/get settings ::grace-ticks))
        (not (settings/get settings ::with-hostile))
        (seq (danger/known-hostiles p (settings/get settings ::hostile-radius) {:ranged-radius (danger/ranged-radius)})))))
