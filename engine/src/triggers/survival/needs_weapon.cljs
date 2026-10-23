(ns triggers.survival.needs-weapon
  "The needs-weapon trigger: an awake body carrying no weapon (jobs.lib.combat/default-weapons) gets a sword
  (jobs.items.obtain, the best it can make: iron, else stone, else wooden), so a mob it meets is fought, not fled from
  with the fist. Not for 10 min after a deposit put a weapon away by name (memory :weapon-stowed), nor in the first light, or with a hostile known near (jobs.lib.dawn), nor while an open death's live pile holds a weapon: recover-drops fetches it first, nor while a listed job will bring one soon enough that waiting costs less than getting one now (jobs.lib.makers/waiting-pays?: an obtain of one, a get-kit). Its default job is a :priced obtain: it searches only as far as the sword is worth."
  (:require [engine.game :as game]
            [engine.memory :as mem]
            [jobs.lib.body :as body]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.makers :as makers]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn weapon? [name] (boolean (some #(.includes name %) combat/default-weapons)))

(defn weapon-in-pile?
  "Whether a death no :recovered covers, still inside the despawn window, dropped a weapon."
  [view]
  (boolean (some (fn [d] (and (< (game/ticks-since d view) game/despawn-ticks)
                              (some (comp weapon? :name)
                                    (:inventory (:data d)))))
                 (body/open-deaths view))))

(defn needs-weapon
  "Holds alive and awake, when no carried item is a weapon, no live pile holds one and no listed job that brings one is worth waiting for."
  [p view _args kn live]
  (boolean
   (and (not (body/dead? view))
        (not (sh-sense/sleeping? p))
        (not (dawn/hostile-known? p))
        (not (weapon-in-pile? view))
        (nil? (mem/latest view :weapon-stowed))
        (nil? (combat/best-weapon p combat/default-weapons))
        (not (makers/waiting-pays? p view kn live weapon? (cost/spawn-horizon-s))))))
