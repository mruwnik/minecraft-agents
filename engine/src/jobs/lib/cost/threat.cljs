(ns jobs.lib.cost.threat
  "What one mob does to a body: hostile? from minecraft-data, the damage type of its attack, and mob-hurt, the damage it
  deals over some seconds of exposure after the body's armour (jobs.lib.cost.armour).
  A witch's potions (poison, harming) bypass armour. A creeper is one blast of 43 (:explosion) however long the exposure; a ranged mob shoots (:projectile); any other
  hits for its mob-hit (3 if unknown, scaled by the server difficulty) ::hit-rate times a second (:melee). Endermen and zombified piglins only fight when provoked."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            ["minecraft-data" :as minecraft-data]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.armour :as armour]))

(a/defargs settings
  {::creeper-blast {:default 43 :doc "Damage a creeper's blast is costed at." :spec (a/int-in 0 nil)}
   ::creeper-exchange {:default 1.5 :doc "Damage costed per swing of a hit-and-back fight with a lone creeper (a late step back lets the fuse run on); the fight never costs the blast." :spec (a/num-in 0 nil)}
   ::difficulty {:default 1 :doc "The server difficulty mob hits are costed at: 0 peaceful, 1 easy, 2 normal, 3 hard (not read from the server)." :spec (a/int-in 0 3)}
   ::hit-rate {:default 0.6 :doc "Hits a zombie or skeleton lands per second of exposure (measured over trial fights; the body is out of reach part of the time). Mobs not measured count one a second." :spec (a/num-in 0 nil)}
   ::spider-hit-rate {:default 0.35 :doc "Hits a spider or cave spider lands per second of exposure." :spec (a/num-in 0 nil)}
   ::provoked-share {:default 0.1 :doc "What a provoked-only mob's threat counts for when it is met, not fought." :spec (a/num-in 0 nil)}})

(def mob-hit
  "Damage one hit of a mob deals at normal difficulty. Unknown mobs count as 3."
  {"zombie" 3 "husk" 3 "drowned" 3 "zombie_villager" 3 "skeleton" 2 "stray" 2 "parched" 2 "bogged" 2 "spider" 2 "cave_spider" 3
   "witch" 3 "pillager" 2 "vindicator" 8 "slime" 2 "silverfish" 1 "endermite" 2 "phantom" 2 "enderman" 5})

(defn creeper-blast [] (settings/get settings ::creeper-blast))
(defn creeper-exchange [] (settings/get settings ::creeper-exchange))
(def provoked-only #{"enderman" "zombified_piglin"})
(defn provoked-share [] (settings/get settings ::provoked-share))

(defn hostile?
  "Whether mob {:name :kind} is hostile for minecraft-data version: its entity type \"hostile\" (or category
  \"Hostile mobs\"); a name minecraft-data does not know goes by :kind."
  [version {:keys [name kind]}]
  (let [e (some-> (minecraft-data version) (aget "entitiesByName") (aget name))]
    (if e
      (or (= "hostile" (aget e "type")) (= "Hostile mobs" (aget e "category")))
      (= "hostile" kind))))

(defn damage-type [mob-name]
  (cond (= "creeper" mob-name) :explosion
        (contains? weapon/ranged-mobs mob-name) :projectile
        :else :melee))

(defn difficulty-scale
  "vanilla's melee/arrow damage at the ::difficulty setting for a hit of normal damage d."
  [d]
  (case (settings/get settings ::difficulty)
    0 0
    1 (min (+ (/ d 2) 1) d)
    3 (* 1.5 d)
    d))

(def measured-hit-rate #{"zombie" "husk" "drowned" "zombie_villager" "skeleton" "stray" "parched" "bogged"})

(defn hit-rate
  "Hits a second of exposure: the measured rates for the zombie and skeleton families and spiders, else one a second."
  [mob-name]
  (cond (contains? measured-hit-rate mob-name) (settings/get settings ::hit-rate)
        (contains? #{"spider" "cave_spider"} mob-name) (settings/get settings ::spider-hit-rate)
        :else 1))

(defn base-threat
  "{:threat :hits} of mob-name over seconds of exposure, before armour; hits is the expected count (rate x seconds, rate
  default hit-rate)."
  ([mob-name seconds] (base-threat mob-name seconds nil))
  ([mob-name seconds rate]
   (if (= "creeper" mob-name)
     {:threat (creeper-blast) :hits 1}
     (let [hits (* seconds (or rate (hit-rate mob-name)))]
       {:threat (* hits (difficulty-scale (get mob-hit mob-name 3))) :hits hits}))))

(defn mob-hurt
  "The damage mob-name deals over seconds of exposure to a body with armour stats (jobs.lib.cost.armour/armour-stats),
  threat-fn applied to the threat before armour (default identity); rate overrides the hits a second."
  ([stats mob-name seconds] (mob-hurt stats mob-name seconds identity nil))
  ([stats mob-name seconds threat-fn] (mob-hurt stats mob-name seconds threat-fn nil))
  ([stats mob-name seconds threat-fn rate]
   (let [{:keys [threat hits]} (base-threat mob-name seconds rate)]
     (if (= "witch" mob-name)
       (threat-fn threat)
       (armour/after-armour stats (damage-type mob-name) (threat-fn threat) hits)))))
