(ns jobs.lib.cost.threat
  "What one mob does to a body: hostile? from minecraft-data, the damage type of its attack, and mob-hurt, the damage it
  deals over some seconds of exposure after the body's armour (jobs.lib.cost.armour).
  A witch's potions (poison, harming) bypass armour. A creeper is one blast of 43 (:explosion) however long the exposure; a ranged mob shoots (:projectile); any other
  hits for its mob-hit (3 if unknown, scaled by the server difficulty) one a second (:melee). Endermen and zombified piglins only fight when provoked."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            ["minecraft-data" :as minecraft-data]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.armour :as armour]))

(a/defargs settings
  {::creeper-blast {:default 43 :doc "Damage a creeper's blast is costed at." :spec (a/int-in 0 nil)}
   ::creeper-exchange-hp {:default 3.06 :doc "k of the creeper exchange: hp creepers dealt over the swings (attack that hit or killed) of hit-and-back fights with a sword or axe against a lone creeper (a late step back lets the fuse run on); 1 trial run, 1 blast over 2 hp, all before 336cdd402 (the back-off) and none counted since. Recount: node tools/event-counts.mjs creeper-exchange (:armed); per-case evidence replaces it." :spec (a/num-in 0 nil)}
   ::creeper-exchange-swings {:default 5 :doc "n of the creeper exchange: the swings behind ::creeper-exchange-hp." :spec (a/num-in 1 nil)}
   ::difficulty {:default 1 :doc "The server difficulty mob hits are costed at: 0 peaceful, 1 easy, 2 normal, 3 hard (not read from the server)." :spec (a/int-in 0 3)}
   ::hit-rate {:default 0.6 :doc "Hits a zombie or skeleton lands per second of exposure (measured over trial fights; the body is out of reach part of the time). Mobs not measured count one a second." :spec (a/num-in 0 nil)}
   ::spider-hit-rate {:default 0.35 :doc "Hits a spider or cave spider lands per second of exposure." :spec (a/num-in 0 nil)}
   ::trident-rate {:default 0.5 :doc "Throws a second of a drowned holding a trident (vanilla: one every 40 ticks)." :spec (a/num-in 0 nil)}
   ::provoked-share {:default 0.1 :doc "What a provoked-only mob's threat counts for when it is met, not fought." :spec (a/num-in 0 nil)}
   ::default-follow-range {:default 16 :doc "Blocks a mob of unknown kind notices the body from." :spec (a/int-in 1 nil)}})

(def follow-ranges
  "Blocks a mob keeps chasing its target out to (vanilla follow_range attribute; its navigation goes no farther)."
  {"zombie" 35 "husk" 35 "drowned" 35 "zombie_villager" 35 "zombified_piglin" 35
   "enderman" 64 "pillager" 32 "vindicator" 12 "evoker" 12 "blaze" 48 "ghast" 100})

(defn follow-range [mob-name] (get follow-ranges mob-name (settings/get settings ::default-follow-range)))

(def max-follow-range (apply max (vals follow-ranges)))

(def mob-hit
  "Damage one hit of a mob deals at normal difficulty. Unknown mobs count as 3."
  {"zombie" 3 "husk" 3 "drowned" 3 "zombie_villager" 3 "skeleton" 2 "stray" 2 "parched" 2 "bogged" 2 "spider" 2 "cave_spider" 3
   "witch" 3 "drowned_trident" 8 "pillager" 2 "vindicator" 8 "slime" 2 "silverfish" 1 "endermite" 2 "phantom" 2 "enderman" 5})

(def door-breakers
  "Mobs that break a wooden door down (vanilla): zombies from hard on, vindicators from normal on."
  {"zombie" 3 "zombie_villager" 3 "husk" 3 "drowned" 3 "vindicator" 2})

(def door-break-s "Seconds a mob at a door needs to break it down (vanilla: 240 ticks)." 12)

(defn breaks-door?
  "Whether the mob called mob-name breaks a wooden door at the ::difficulty setting."
  [mob-name]
  (some-> (door-breakers mob-name) (<= (settings/get settings ::difficulty))))

(def burns-in-sun
  "Mobs that burn in daylight under the open sky (vanilla; a drowned too, but it keeps to the water)."
  #{"zombie" "zombie_villager" "skeleton" "stray" "bogged" "phantom"})

(def despawn-blocks "Blocks from every player past which a hostile despawns at once (vanilla)." 128)
(def idle-despawn-blocks "Blocks from every player past which an idle hostile may despawn (vanilla)." 32)
(def idle-despawn-s "Seconds a hostile idles before it may despawn (vanilla: 600 ticks)." 30)
(def idle-despawn-per-s "Chance a second an idle hostile despawns (vanilla: 1 in 800 a tick)." (/ 20 800))

(defn left-alive
  "The chance a hostile is not despawned (vanilla): 0 once a player was more than despawn-blocks from it (farthest), else
  1 in 800 a tick after idle-despawn-s of away-s with none within idle-despawn-blocks. Above easy a zombie or skeleton
  may pick up loot and stay (unknown: 1)."
  [farthest away-s]
  (cond (> (settings/get settings ::difficulty) 1) 1
        (> farthest despawn-blocks) 0
        :else (js/Math.exp (- (* idle-despawn-per-s (max 0 (- away-s idle-despawn-s)))))))

(def spawn-weights
  "The weights the overworld's common monster spawns are drawn by (vanilla, most biomes); slimes, kept to slime chunks and
  swamps, left out."
  {"spider" 100 "zombie" 95 "zombie_villager" 5 "skeleton" 100 "creeper" 100 "enderman" 10 "witch" 5})

(defn creeper-blast [] (settings/get settings ::creeper-blast))
(defn creeper-exchange
  "hp from creepers per swing: the mean of the Gamma-Poisson posterior under the Jeffreys prior, (k + 0.5) / n of the two settings, so a small n is pulled toward an even half hp per swing."
  []
  (/ (+ (settings/get settings ::creeper-exchange-hp) 0.5) (settings/get settings ::creeper-exchange-swings)))
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

(def trident-kind "The kind of a drowned holding a trident: it throws (vanilla: 8 hp a throw, every 2 s) rather than strikes." "drowned_trident")

(defn kind
  "The name the tables here are keyed by for mob {:name :held}: a drowned holding a trident is its own kind."
  [{:keys [name held]}]
  (if (and (= "drowned" name) (= "trident" held)) trident-kind name))

(defn ranged?
  "Whether mob {:name :held} shoots or throws."
  [m]
  (let [k (kind m)]
    (or (= trident-kind k) (contains? weapon/ranged-mobs k))))

(defn damage-type [mob-name]
  (cond (= "creeper" mob-name) :explosion
        (or (= trident-kind mob-name) (contains? weapon/ranged-mobs mob-name)) :projectile
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

(defn lone-hit-rate
  "Hits a second of a lone sword fight: the measured rates of the zombie and skeleton families and spiders, else one a second."
  [mob-name]
  (cond (= trident-kind mob-name) (settings/get settings ::trident-rate)
        (contains? measured-hit-rate mob-name) (settings/get settings ::hit-rate)
        (contains? #{"spider" "cave_spider"} mob-name) (settings/get settings ::spider-hit-rate)
        :else 1))

(defn base-threat
  "{:threat :hits} of mob-name over seconds of exposure, before armour; hits is the expected count (rate x seconds, rate
  default one a second)."
  ([mob-name seconds] (base-threat mob-name seconds nil))
  ([mob-name seconds rate]
   (if (= "creeper" mob-name)
     {:threat (creeper-blast) :hits 1}
     (let [hits (* seconds (or rate 1))]
       {:threat (* hits (difficulty-scale (get mob-hit mob-name 3))) :hits hits}))))

(defn mob-hurt
  "The damage mob-name deals over seconds of exposure to a body with armour stats (jobs.lib.cost.armour/armour-stats),
  threat-fn applied to the threat before armour (default identity); rate is the hits a second (default 1: the measured lone-fight rates, lone-hit-rate, are for fight-damage)."
  ([stats mob-name seconds] (mob-hurt stats mob-name seconds identity nil))
  ([stats mob-name seconds threat-fn] (mob-hurt stats mob-name seconds threat-fn nil))
  ([stats mob-name seconds threat-fn rate]
   (let [{:keys [threat hits]} (base-threat mob-name seconds rate)]
     (if (= "witch" mob-name)
       (threat-fn threat)
       (armour/after-armour stats (damage-type mob-name) (threat-fn threat) hits)))))
