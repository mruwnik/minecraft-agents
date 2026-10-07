(ns jobs.lib.cost.threat
  "What one mob does to a body: hostile? from minecraft-data, the damage type of its attack, and mob-hurt, the damage it
  deals over some seconds of exposure after the body's armour (jobs.lib.cost.armour).
  A creeper is one blast of 43 (:explosion) however long the exposure; a ranged mob shoots (:projectile); any other
  hits at its mob-dps (3 if unknown) once a second (:melee). Endermen and zombified piglins only fight when provoked."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            ["minecraft-data" :as minecraft-data]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.armour :as armour]))

(a/defargs settings
  {::creeper-blast {:default 43 :doc "Damage a creeper's blast is costed at." :spec (a/int-in 0 nil)}
   ::provoked-share {:default 0.1 :doc "What a provoked-only mob's threat counts for when it is met, not fought." :spec (a/num-in 0 nil)}})

(def mob-dps
  "Damage a mob deals per second while it can hit the body (normal difficulty). Unknown mobs count as 3."
  {"zombie" 3 "husk" 3 "drowned" 3 "zombie_villager" 3 "skeleton" 2 "stray" 2 "bogged" 2 "spider" 2 "cave_spider" 3
   "witch" 3 "pillager" 2 "vindicator" 8 "slime" 2 "silverfish" 1 "endermite" 2 "phantom" 2 "enderman" 7})

(defn creeper-blast [] (settings/get settings ::creeper-blast))
(def provoked-only #{"enderman" "zombified_piglin"})
(defn provoked-share [] (settings/get settings ::provoked-share))

(defn hostile?
  "Whether mob {:name :kind} is hostile for minecraft-data version: its entity type \"hostile\" (or category
  \"Hostile mobs\"); a name minecraft-data does not know goes by :kind."
  [version {:keys [name kind]}]
  (let [e (some-> (minecraft-data version) .-entitiesByName (aget name))]
    (if e
      (or (= "hostile" (.-type e)) (= "Hostile mobs" (.-category e)))
      (= "hostile" kind))))

(defn damage-type [mob-name]
  (cond (= "creeper" mob-name) :explosion
        (contains? weapon/ranged-mobs mob-name) :projectile
        :else :melee))

(defn base-threat
  "{:threat :hits} of mob-name over seconds of exposure, before armour."
  [mob-name seconds]
  (if (= "creeper" mob-name)
    {:threat (creeper-blast) :hits 1}
    {:threat (* seconds (get mob-dps mob-name 3)) :hits seconds}))

(defn mob-hurt
  "The damage mob-name deals over seconds of exposure to a body with armour stats (jobs.lib.cost.armour/armour-stats),
  threat-fn applied to the threat before armour (default identity)."
  ([stats mob-name seconds] (mob-hurt stats mob-name seconds identity))
  ([stats mob-name seconds threat-fn]
   (let [{:keys [threat hits]} (base-threat mob-name seconds)]
     (armour/after-armour stats (damage-type mob-name) (threat-fn threat) hits))))
