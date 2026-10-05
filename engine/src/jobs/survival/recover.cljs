(ns jobs.survival.recover
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]
            [engine.jobs.combat :as combat]
            [engine.triggers.hungry :as hungry]))

(def doc
  "Flee and heal a hurt body. It does not fight.
  Starts when health is below :health. Then it keeps going until health reaches :healed.
  Between the two thresholds it runs only while a :hurt entry from the last five minutes exists.
  Each round: flee a hostile within :sight (jobs.survival.retreat), else walk to the latest :bed, else :home, else stay.
  At the safe point it eats if hungry and carrying food, then waits 2 s so health regenerates.
  Ends when health reaches :healed.
  Gives up with a cannot_heal warning when food is under 18 and nothing is carried, since health does not regenerate then.
  The health-low trigger rests after that until food is carried or food reaches 18.
  Memory: writes one :hurt entry per spell and a :heal-ended entry {:why :healed|:cannot-heal} when it ends.
  A recover cut by a higher reflex starts the round again from scratch.")

(def args
  {:health {:doc "start recovering when health is below this" :default 7}
   :healed {:doc "keep recovering until health reaches this" :default 16}
   :sight {:doc "hostiles within this many blocks are fled" :default 16}})

(def hurt-window-ms (* 5 60 1000))

(def hurt-policy {:cap 20 :ttl (* 60 60 1000)})

(def safe-distance 3)

(def idle-ms
  "How long a round waits while health regenerates."
  2000)

(def regen-food
  "Natural regeneration needs at least this much food."
  18)

(defn health-of [c] (.-health (.self (:primitives c))))

(defn check
  "Health is below :health, or below :healed with a :hurt entry in the last
  five minutes, or a spell is under way (so the round gets to see it healed
  and finish, instead of the job lingering declined)."
  [c]
  (let [{:keys [health healed]} (:args c)
        h (health-of c)]
    (or (< h health)
        (boolean (:spell-started (ctx/mem c)))
        (and (< h healed) (pos? (ctx/count-in c :hurt hurt-window-ms))))))

(defn safe-point
  "The latest :bed, else the latest :home, else here."
  [c]
  (or (mem/place (ctx/view c) :bed)
      (mem/place (ctx/view c) :home)
      (u/self-pos c)))

(defn has-food?
  "Carries something jobs.survival.eat {} would eat (the trigger's own test)."
  [c]
  (hungry/carries-food? (.self (:primitives c))))

(def heal-ended-policy {:cap 5 :ttl (* 60 60 1000)})

(defn ended!
  "End the spell: a :heal-ended entry saying why, then :done."
  [c why]
  (ctx/remember! c :heal-ended {:why why :health (health-of c)} heal-ended-policy)
  :done)

(defn start-spell!
  "Once per spell (a job instance), record why we are hurt."
  [c threat]
  (when-not (:spell-started (ctx/mem c))
    (ctx/update-mem! c assoc :spell-started true)
    (ctx/remember! c :hurt
                   (cond-> {:health (health-of c) :pos (u/self-pos c)}
                     threat (assoc :hostile {:name (.-name threat) :pos (u/pos-of (.-pos threat))}))
                   hurt-policy)))

(defn ^:async flee! [c threat sight]
  (if threat
    (await (ctx/call-child c :flee 'jobs.survival.retreat {:radius sight}))
    :done))

(defn ^:async go-safe! [c]
  (let [dest (safe-point c)]
    (if (> (u/dist (u/self-pos c) dest) safe-distance)
      (await (ctx/call-child c :safety 'jobs.movement.go-to {:pos dest :range 2}))
      :done)))

(defn ^:async round [c]
  (let [{:keys [sight healed]} (:args c)
        threat (first (combat/hostiles (:primitives c) sight))]
    (cond
      (>= (health-of c) healed) (ended! c :healed)
      :else
      (do (start-spell! c threat)
          (if (= :continue (await (flee! c threat sight)))
            :continue
            (if (= :continue (await (go-safe! c)))
              :continue
              (do (when (and (< (.-food (.self (:primitives c))) 20) (has-food? c))
                    (await (ctx/call-child c :eat 'jobs.survival.eat {})))
                  (cond
                    (>= (health-of c) healed) (ended! c :healed)
                    (and (< (.-food (.self (:primitives c))) regen-food) (not (has-food? c)))
                    (do (ctx/emit! c :cannot_heal :warn {:text (str "too hungry to regenerate and nothing to eat; the "
                                                                    "health-low reflex rests until food is carried or "
                                                                    "food reaches " regen-food)})
                        (ended! c :cannot-heal))
                    :else (do (await (ctx/act c :wait #js {:ms idle-ms}))
                              :continue)))))))))
