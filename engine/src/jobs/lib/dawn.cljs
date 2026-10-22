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

(defn known-dangers
  "The hostiles within ::hostile-radius the body has sensed, seen, heard or remembered (jobs.lib.danger/dangers; a melee
  one needs no way to the body, which waits behind walls: it stands where the body's walk out will land; a ranged one a
  line of fire), nearest first; none while ::with-hostile is set. (known-dangers p {:way? true}) keeps only the ones with
  a way to the body, for a check of a job that walks out only as far as it needs."
  ([p] (known-dangers p {:way? false}))
  ([p danger-opts]
   (if (settings/get settings ::with-hostile)
     []
     (danger/dangers p (settings/get settings ::hostile-radius) {:ranged-radius (danger/ranged-radius)} danger-opts))))

(defn hostile-known?
  "Whether a real danger is known within ::hostile-radius (known-dangers, same danger-opts), unless ::with-hostile is set."
  ([p] (boolean (seq (known-dangers p))))
  ([p danger-opts] (boolean (seq (known-dangers p danger-opts)))))

(defn describe
  "{:kind :pos :distance :how} of hostile e: how is :seen, :heard or :remembered (with :age-s since it was last sensed)."
  [p e]
  (let [how (cond (.-remembered e) :remembered (.-heard e) :heard :else :seen)
        at (danger/mob-pos p e)]
    (cond-> {:kind (.-name e) :pos (some-> at (select-keys [:x :y :z])) :distance (js/Math.round (danger/mob-distance p e)) :how how}
      (.-ageMs e) (assoc :age-s (js/Math.round (/ (.-ageMs e) 1000))))))

(defn hostile-text
  "Short text of the nearest of known-dangers ds: kind, how it is known, distance and place."
  [ds]
  (let [{:keys [kind how distance pos age-s]} (first ds)
        n (count ds)]
    (str kind " " (name how) (when (= how :remembered) (str " " age-s " s ago")) ", " distance " blocks off"
         (when pos (str " near [" (:x pos) " " (:y pos) " " (:z pos) "]"))
         (when (> n 1) (str " (+" (dec n) " more)")))))

(defn calm?
  "Whether the dawn grace has passed and no hostile is known near (hostile-known?)."
  [p]
  (boolean (and (>= (ticks-since-dawn (.-timeOfDay (.self p))) (settings/get settings ::grace-ticks))
                (not (hostile-known? p)))))

(defn hostile-hold
  "The known-dangers a body in shelter stays in for at first light: within ::grace-ticks of the night's end
  (::unarmed-grace-ticks when it carries no weapon); [] when it does not hold."
  [p armed?]
  (if (< (ticks-since-dawn (.-timeOfDay (.self p)))
         (max (settings/get settings ::grace-ticks) (if armed? 0 (settings/get settings ::unarmed-grace-ticks))))
    (known-dangers p)
    []))
