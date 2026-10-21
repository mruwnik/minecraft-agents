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
   ::unarmed-grace-ticks {:default 6000 :doc "Game ticks after the night ends in which a body with no weapon stays in its shelter while a hostile is known near (husks do not burn by day, and bare hands win no fight); the dawn grace for an armed body is ::grace-ticks." :spec (a/int-in 0 nil)}
   ::with-hostile {:default false :doc "Whether a comfort errand may start while a known hostile is near." :spec boolean?}})

(defn ticks-since-dawn [time-of-day] (mod (- time-of-day sh-clock/morning-tick) 24000))

(defn hostile-known?
  "Whether a hostile is known within ::hostile-radius (remembered and heard ones included), unless ::with-hostile is set."
  [p]
  (boolean (and (not (settings/get settings ::with-hostile))
                (seq (danger/known-hostiles p (settings/get settings ::hostile-radius) {:ranged-radius (danger/ranged-radius)})))))

(defn calm?
  "Whether the dawn grace has passed and no hostile is known near (hostile-known?)."
  [p]
  (boolean (and (>= (ticks-since-dawn (.-timeOfDay (.self p))) (settings/get settings ::grace-ticks))
                (not (hostile-known? p)))))

(defn hostile-hold?
  "Whether a body in shelter stays in at first light: within ::grace-ticks of the night's end (::unarmed-grace-ticks when
  it carries no weapon), with a known hostile within ::hostile-radius, unless ::with-hostile is set. since is the ticks
  since the night ended; the caller counts it on its own clock, as the world's time can be set under a waiting body."
  [p armed? since]
  (boolean
   (and (< since (max (settings/get settings ::grace-ticks) (if armed? 0 (settings/get settings ::unarmed-grace-ticks))))
        (not (settings/get settings ::with-hostile))
        (seq (danger/known-hostiles p (settings/get settings ::hostile-radius) {:ranged-radius (danger/ranged-radius)})))))
