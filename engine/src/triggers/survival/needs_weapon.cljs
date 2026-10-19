(ns triggers.survival.needs-weapon
  "The needs-weapon trigger: an awake body carrying no weapon (jobs.lib.combat/default-weapons) gets a sword
  (jobs.items.obtain, the best it can make: iron, else stone, else wooden), so a mob it meets is fought, not fled from
  with the fist. Not in the first light, or with a hostile known near (jobs.lib.dawn), nor while an open death's live pile holds a weapon: recover-drops fetches it first."
  (:require [engine.game :as game]
            [jobs.lib.body :as body]
            [jobs.lib.combat :as combat]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn weapon-in-pile?
  "Whether a death no :recovered covers, still inside the despawn window, dropped a weapon."
  [view]
  (boolean (some (fn [d] (and (< (game/ticks-since d view) game/despawn-ticks)
                              (some (fn [{:keys [name]}] (some #(.includes name %) combat/default-weapons))
                                    (:inventory (:data d)))))
                 (body/open-deaths view))))

(defn needs-weapon
  "Holds alive and awake, when no carried item is a weapon and no live pile holds one."
  [p view _args]
  (boolean
   (and (not (body/dead? view))
        (not (sh-sense/sleeping? p))
        (dawn/calm? p)
        (not (weapon-in-pile? view))
        (nil? (combat/best-weapon p combat/default-weapons)))))
