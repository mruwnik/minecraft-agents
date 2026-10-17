(ns jobs.survival.recover-drops.config
  "recover-drops' settings (keys under :jobs.survival.recover-drops.config/), their getters and the memory policies."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::arrive-range {:default 2 :doc "How near the death point the walk back ends, in blocks." :spec (a/int-in 0 nil)}
   ::xp-per-level {:default 5 :doc "Worth of one experience level carried at the death (the game drops a few levels' worth)." :spec (a/int-in 0 nil)}
   ::top-n {:default 3 :doc "Items and mobs named in the report of a death site, at most." :spec (a/int-in 0 nil)}
   ::max-collect-passes {:default 3 :doc "Walks back to the pile after a collect pass that left visible items of it behind." :spec (a/int-in 0 nil)}
   ::stray-range {:default 5 :doc "A body this far from the death point in the collect phase (a flee cut the trip) walks back first." :spec (a/int-in 0 nil)}
   ::search-radius {:default 3 :doc "Blocks out from the pile, on the four compass sides, at which the body looks for items that are not in sight at the pile." :spec (a/int-in 1 nil)}
   ::max-hostile-cuts {:default 2 :doc "Resumes of one trip that followed a cut by a hostile reflex, or a decline for a danger, before it is given up as :danger." :spec (a/int-in 1 nil)}
   ::max-blocked-retries {:default 3 :doc "Walks of one trip that did not arrive before it is given up as :unreachable." :spec (a/int-in 1 nil)}
   ::wet-danger {:default 30 :doc "Points of route danger added for a pile in water or lost to drowning (a swim with the air running out); 10 cost each." :spec (a/num-in 0 nil)}
   ::seal-radius {:default 4 :doc "Scaffold blocks this body placed within this many blocks of the pile (a flee seal) are taken back when they keep it out of reach." :spec (a/num-in 0 nil)}
   ::seal-sight {:default 4 :doc "The body walks to within this many blocks of the nearest own seal block, to see it, before the seal is taken back." :spec (a/num-in 0 nil)}
   ::deep-drop {:default 8 :doc "A pile this many blocks or more below the body, within ::deep-reach aside, is reached by a stair down (jobs.access.tunnel) once the walk did not arrive." :spec (a/int-in 1 nil)}
   ::deep-reach {:default 24 :doc "How far aside of a deep pile the walk goes on before the stair down is cut." :spec (a/num-in 0 nil)}
   ::stair-step-s {:default 3 :doc "Seconds one block of a stair down and its run is reckoned to take, against the despawn window left." :spec (a/num-in 0 nil)}
   ::killer-guard-ms {:default 60000 :doc "How long after a death its killer is assumed to still stand at the pile; later only the hostiles the body knows of count." :spec (a/num-in 0 nil)}
   ::pause-s {:default 1 :doc "Seconds one wait for the respawn, the settling or a kit fetch pauses (jobs.time.noop) before it looks again." :spec (a/num-in 0 nil)}
   ::danger-memory-ms {:default 150000 :doc "How long a skip for danger is remembered: the died trigger rests and the job waits (:danger-remembered) for that death, then the danger is weighed anew. A skip for the assumed killer alone lasts only while the killer guard does." :spec (a/num-in 0 nil)}
   ::bed-away-blocks {:default 10 :doc "A respawn farther than this from the bed the body slept in is reported." :spec (a/int-in 0 nil)}})

(defn seal-radius [] (settings/get settings ::seal-radius))

(defn seal-sight [] (settings/get settings ::seal-sight))

(defn killer-guard-ms [] (settings/get settings ::killer-guard-ms))

(defn danger-memory-ms [] (settings/get settings ::danger-memory-ms))

(defn pause-s [] (settings/get settings ::pause-s))

(defn wet-danger [] (settings/get settings ::wet-danger))

(defn arrive-range [] (settings/get settings ::arrive-range))

(defn max-hostile-cuts [] (settings/get settings ::max-hostile-cuts))

(defn max-blocked-retries [] (settings/get settings ::max-blocked-retries))

(def settle-ms
  "After a respawn the client needs this long to re-receive nearby entities;
  an estimate made sooner sees no hostiles."
  2000)

(def day-ms (* 24 60 60 1000))

(def recovered-policy {:cap 10 :ttl day-ms})

(def trip-policy {:cap 1 :ttl day-ms}) ; wall clock; the despawn window itself is counted in game ticks

(def danger-policy {:cap 5 :ttl day-ms})

(defn xp-per-level [] (settings/get settings ::xp-per-level))

(defn top-n [] (settings/get settings ::top-n))

(defn deep-drop [] (settings/get settings ::deep-drop))

(defn deep-reach [] (settings/get settings ::deep-reach))

(defn stair-step-s [] (settings/get settings ::stair-step-s))

(defn max-collect-passes [] (settings/get settings ::max-collect-passes))

(defn stray-range [] (settings/get settings ::stray-range))
(defn search-radius [] (settings/get settings ::search-radius))

(defn bed-away-blocks [] (settings/get settings ::bed-away-blocks))
