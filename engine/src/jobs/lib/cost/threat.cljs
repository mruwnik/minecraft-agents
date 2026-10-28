(ns jobs.lib.cost.threat
  "What one mob does to a body: hostile? from minecraft-data, the damage type of its attack, and mob-hurt, the damage it
  deals over some seconds of exposure after the body's armour (jobs.lib.cost.armour).
  A witch's potions (poison, harming) bypass armour. A creeper is one blast of 43 at normal difficulty (:explosion) however long the exposure; a ranged mob shoots (:projectile); any other
  hits for its mob-hit (3 if unknown, scaled by the server difficulty) one a second (:melee). Endermen and zombified piglins only fight when provoked."
  (:require [engine.args :as a]
            [engine.game :as game]
            [engine.settings :as settings]
            ["minecraft-data" :as minecraft-data]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.cost.armour :as armour]))

(a/defargs settings
  {::creeper-blast {:default 43 :doc "Damage a creeper's blast is costed at, at normal difficulty (vanilla scales it as a mob's hit)." :spec (a/int-in 0 nil)}
   ::creeper-exchange-hp {:default 3.06 :doc "k of the creeper exchange: hp creepers dealt over the swings (attack that hit or killed) of hit-and-back fights with a sword or axe against a lone creeper (a late step back lets the fuse run on); 1 trial run, 1 blast over 2 hp, all before 336cdd402 (the back-off) and none counted since. Recount: node tools/event-counts.mjs creeper-exchange (:armed); per-case evidence replaces it." :spec (a/num-in 0 nil)}
   ::creeper-exchange-swings {:default 5 :doc "n of the creeper exchange: the swings behind ::creeper-exchange-hp." :spec (a/num-in 1 nil)}
   ::difficulty {:default -1 :doc "Override of the server difficulty mob hits and door breaking are costed at: 0 peaceful, 1 easy, 2 normal, 3 hard. -1 (unset), the body's sensed difficulty counts (hard while it is unknown)." :spec (a/int-in -1 3)}
   ::spot-occupied {:default {:k 0 :n 4} :spec (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 0 nil)})
                    :doc "Of the death and near-death spots the trial bodies came back within 16 blocks of after 300 s or more 32 to 128 blocks off (vanilla: its killer had despawned), those where within 60 s a hostile was within 16 blocks of the spot or hurt the body there (k) of all (n), whatever the spot's light (the logs carry none): node tools/event-counts.mjs spot-returns, 2026-10-23."}
   ::hit-rate {:default 0.6 :doc "Hits a zombie lands per second of exposure (measured over trial fights; the body is out of reach part of the time). Mobs not measured count one a second." :spec (a/num-in 0 nil)}
   ::hit-rate-hits {:default 195 :doc "Hits counted behind ::hit-rate, (card 162067ad, 2026-10-21: zombie 195 hits over 121 fights): the shape, + 0.5, of its Gamma posterior (hit-rate-shape). No tools/ recount yet (card 76d72c81)." :spec (a/num-in 0 nil)}
   ::spider-hit-rate {:default 0.35 :doc "Hits a spider or cave spider lands per second of exposure." :spec (a/num-in 0 nil)}
   ::trident-rate {:default 0.5 :doc "Throws a second of a drowned holding a trident (vanilla: one every 40 ticks)." :spec (a/num-in 0 nil)}
   ::provoked-share {:default 0.1 :doc "What a provoked-only mob's threat counts for when it is met, not fought." :spec (a/num-in 0 nil)}
   ::default-follow-range {:default 16 :doc "Blocks a mob of unknown kind notices the body from." :spec (a/int-in 1 nil)}
   ::default-fire-range {:default 16 :doc "Blocks a ranged mob without a known attack range (breeze, an unknown kind) shoots from." :spec (a/int-in 1 nil)}
   ::wander-leaves {:default 1 :spec a/pos-num?
                    :doc "Prior leavings of an idle hostile from where it was seen (out past 16 blocks; vanilla: a stroll of up to 10 blocks about every 6 s while a player is within 32 blocks, so about 4 strolls). A stated prior: the logs carry no unseen mob's place to count."}
   ::wander-seconds {:default 24 :spec a/pos-num? :doc "Seconds of wandering those leavings were seen over."}
   ::field-close {:default 3 :doc "Steps of a mob's way within which its field weighs 1 (way-weight)." :spec (a/int-in 0 nil)}})

(defn difficulty
  "The server difficulty costs use: the ::difficulty override, else the sensed one, else hard (an unknown is the worse case)."
  []
  (let [override (settings/get settings ::difficulty)]
    (if (<= 0 override) override (or (game/difficulty) 3))))

(defn simulation-distance "Chunks around a player that tick: the server's, nil while unknown (left-alive then assumes the worse case, no far despawn)." []
  (game/simulation-distance))

(def follow-ranges
  "Blocks a mob keeps chasing its target out to (vanilla follow_range attribute; its navigation goes no farther)."
  {"zombie" 35 "husk" 35 "drowned" 35 "zombie_villager" 35 "zombified_piglin" 35
   "enderman" 64 "pillager" 32 "vindicator" 12 "evoker" 12 "blaze" 48 "ghast" 100})

(defn follow-range [mob-name] (get follow-ranges mob-name (settings/get settings ::default-follow-range)))

(def fire-ranges
  "Blocks a ranged mob shoots from (the range argument of its vanilla attack goal: RangedBowAttackGoal 15 for skeleton, stray, bogged and parched; RangedAttackGoal 10 for a witch and a trident drowned; RangedCrossbowAttackGoal 8 for a pillager; a blaze fires within its follow range 48, a ghast within 64)."
  {"skeleton" 15 "stray" 15 "bogged" 15 "parched" 15 "drowned_trident" 10 "witch" 10 "pillager" 8 "blaze" 48 "ghast" 64})

(defn fire-range [mob-name] (get fire-ranges mob-name (settings/get settings ::default-fire-range)))

(declare ranged?)

(defn give-up-range
  "Blocks from mob-name past which it no longer gets at the body: its follow range, or a shooter's fire range when longer."
  [mob-name]
  (max (follow-range mob-name) (if (ranged? {:name mob-name}) (fire-range mob-name) 0)))

(defn field-close [] (settings/get settings ::field-close))

(defn way-weight
  "The weight a walker's field (jobs.lib.threat-field) gives a cell `steps` of its way out: 1 within ::field-close
  steps, close / steps past them (the farther its way, the more the body passing has walked on before it gets there)."
  ([steps] (way-weight (field-close) steps))
  ([close steps] (if (<= steps close) 1 (/ close steps))))

(def max-follow-range (apply max (vals follow-ranges)))

(def arrow-blow
  "Damage one skeleton arrow deals at normal difficulty: 3.8, the mean of the 197 unarmoured one-arrow hurts in the trial logs
  (136 of 3.0, 49 of 2.5, 12 of 3.5 on the trial server's easy, vanilla's min(d/2 + 1, d) of 4, 3 and 5; node tools/event-counts.mjs, card b858ed2e)."
  3.8)

(def mob-hit
  "Damage one hit of a mob deals at normal difficulty. Unknown mobs count as 3."
  {"skeleton" arrow-blow "stray" arrow-blow "parched" arrow-blow "bogged" arrow-blow "zombie" 3 "husk" 3 "drowned" 3 "zombie_villager" 3 "spider" 2 "cave_spider" 3
   "witch" 3 "drowned_trident" 8 "pillager" 2 "vindicator" 8 "slime" 2 "silverfish" 1 "endermite" 2 "phantom" 2 "enderman" 5})

(def door-breakers
  "Mobs that break a wooden door down, with the difficulty from which they do (vanilla BreakDoorGoal): a vindicator
  from normal on, only in a raid (Vindicator.VindicatorBreakDoorGoal, which the body does not sense: counted as one); a
  zombie, husk or zombie villager on hard only, when its break-door goal was drawn at spawn (breaker-draw). A drowned
  never has the goal (Drowned.supportsBreakDoorGoal is false)."
  {"zombie" 3 "zombie_villager" 3 "husk" 3 "vindicator" 2})

(def spawn-drawn #{"zombie" "zombie_villager" "husk"})

(def door-break-s "Seconds a mob at a door needs to break it down (vanilla: 240 ticks)." 12)

(def moon-brightness "Moon brightness by moon phase, full moon first (vanilla DimensionType.MOON_BRIGHTNESS_PER_PHASE)." [1 0.75 0.5 0.25 0 0.25 0.5 0.75])

(defn regional-difficulty
  "The regional (local) difficulty at the difficulty d (0-3) of a chunk inhabited for inhabited ticks, at day-time ticks
  under the moon phase (vanilla DifficultyInstance.calculateDifficulty, the wiki's Difficulty page)."
  [d day-time inhabited moon-phase]
  (if (zero? d)
    0
    (let [clamp #(max 0 (min 1 %))
          time (* 0.25 (clamp (/ (- day-time 72000) 1440000)))
          chunk (+ (* (clamp (/ inhabited 3600000)) (if (= 3 d) 1 0.75))
                   (max 0 (min time (* 0.25 (moon-brightness (mod moon-phase 8))))))]
      (* d (+ 0.75 time (if (= 1 d) (* 0.5 chunk) chunk))))))

(defn special-multiplier
  "The clamped regional difficulty, 0 to 1 (vanilla DifficultyInstance.getSpecialMultiplier): 0 under 2, 1 over 4."
  [regional]
  (max 0 (min 1 (/ (- regional 2) 2))))

(defn clock-special
  "special-multiplier at the server difficulty under clock {:day-time :moon-phase :world-age} (ticks, the body's self).
  The chunk's inhabited time is not sensed: at most the world age, so costed at that (unknown: the cap); an unknown day
  time counts the longest, an unknown moon full."
  [{:keys [day-time moon-phase world-age]}]
  (special-multiplier (regional-difficulty (difficulty) (or day-time 1512000) (or world-age 3600000) (or moon-phase 0))))

(defn breaker-draw
  "The share of zombie kinds drawn with the break-door goal at spawn (vanilla Zombie.finalizeSpawn: nextFloat < 0.1 x
  special-multiplier) under clock (clock-special's)."
  [clock]
  (* 0.1 (clock-special clock)))

(defn clock-of "breaker-draw's clock of a body's self (JS), nil fields unknown." [self]
  {:day-time (.-dayTime self) :moon-phase (.-moonPhase self) :world-age (.-worldAge self)})

(defn door-break-share
  "The chance the mob called mob-name breaks a wooden door at the server difficulty under clock (breaker-draw's): 0
  when it never does or the difficulty is below its own; for a zombie kind its breaker-draw; else 1."
  ([mob-name] (door-break-share mob-name nil))
  ([mob-name clock]
   (let [from (door-breakers mob-name)]
     (cond (or (nil? from) (> from (difficulty))) 0
           (spawn-drawn mob-name) (breaker-draw clock)
           :else 1))))

(defn door-break-chance
  "The chance at least one of mob-names breaks a door (door-break-share each, under clock)."
  ([mob-names] (door-break-chance mob-names nil))
  ([mob-names clock] (- 1 (reduce * (map #(- 1 (door-break-share % clock)) mob-names)))))

(def burns-in-sun
  "Mobs that burn in daylight under the open sky (vanilla; a drowned too, but it keeps to the water)."
  #{"zombie" "zombie_villager" "skeleton" "stray" "bogged" "phantom"})

(def despawn-blocks "Blocks from every player past which a hostile despawns at once (vanilla)." 128)
(def idle-despawn-blocks "Blocks from every player past which an idle hostile may despawn (vanilla)." 32)
(def idle-despawn-s "Seconds a hostile idles before it may despawn (vanilla: 600 ticks)." 30)
(def idle-despawn-per-s "Chance a second an idle hostile despawns (vanilla: 1 in 800 a tick)." (/ 20 800))

(def loot-pickers
  "Mobs that may spawn able to pick up loot (vanilla: chance loot-share x the regional special multiplier); one that
  picked up an item it equips never despawns."
  #{"zombie" "husk" "drowned" "zombie_villager" "zombified_piglin" "skeleton" "stray" "bogged" "parched" "wither_skeleton"})

(def loot-share "The most of loot-pickers that spawn able to pick up loot (vanilla 0.55, at a special multiplier of 1)." 0.55)

(def spawn-weights
  "The weights the overworld's common monster spawns are drawn by (vanilla, most biomes); slimes, kept to slime chunks and
  swamps, left out."
  {"spider" 100 "zombie" 95 "zombie_villager" 5 "skeleton" 100 "creeper" 100 "enderman" 10 "witch" 5})

(def bow-holders
  "Loot-pickers that spawn holding a bow, their preferred weapon (vanilla SKELETON_PREFERRED_WEAPONS): no other weapon
  replaces it, a bow only as an equal item (more enchantments or less wear)."
  #{"skeleton" "stray" "bogged" "parched"})

(def spawn-swords "The sword a loot-picker spawns holding, if any, and not a bow (vanilla)." {"wither_skeleton" "stone_sword" "zombified_piglin" "golden_sword"})

(def own-slot
  "Items a mob equips in a slot of their own, not the main hand (vanilla equippable: armour, a carved pumpkin or a head,
  a shield in the off hand); a spawned mob's is empty or worn with a piece the body's may beat."
  #"_(helmet|chestplate|leggings|boots|head|skull)$|^(shield|carved_pumpkin|elytra)$")

(def weapon-like "Main-hand items with an attack bonus (vanilla attribute modifiers)." #"_(sword|axe|pickaxe|shovel|spear)$|^(trident|mace)$")

(defn takes?
  "Whether loot-picker mob, as it spawned, equips the item called item-name (vanilla Mob.equipItemIfPossible and
  canReplaceCurrentItem): an own-slot item always (the body's piece may be the better one), else into an empty main
  hand anything; a bow-holder only a bow (its wear and enchantments are not known); a sword-holder a weapon-like item
  that may hit harder than its sword (weapon/weapon-damage, unknown counted as harder)."
  [mob item-name]
  (let [n (or item-name "")
        sword (spawn-swords mob)]
    (boolean (cond
               (re-find own-slot n) true
               (contains? bow-holders mob) (= "bow" n)
               sword (and (re-find weapon-like n)
                          (or (not (contains? weapon/weapon-damage-by-name n)) (>= (weapon/weapon-damage n) (weapon/weapon-damage sword))))
               :else true))))

(def jockey-share
  "Of the mobs of a kind, the share that spawned riding another, which never despawns (vanilla Mob.requiresCustomPersistence:
  a passenger): a skeleton on 1 in 100 spiders (over spawn-weights), a baby zombie (0.05) on a chicken (0.05, and 0.05
  of the rest when one is near)."
  (let [spiders (* (get spawn-weights "spider") 0.01)
        chicks (* 0.05 (- 1 (* 0.95 0.95)))]
    (merge {"skeleton" (/ spiders (+ spiders (get spawn-weights "skeleton")))}
           (zipmap ["zombie" "husk" "drowned" "zombie_villager" "zombified_piglin"] (repeat chicks)))))

(defn custom-kept
  "The chance mob never despawns by its own state (vanilla requiresCustomPersistence): an enderman may carry a block
  (not known: counted as carrying), a rider its jockey-share."
  [mob]
  (if (= "enderman" mob) 1 (get jockey-share mob 0)))

(defn left-alive
  "The chance the hostile mob is not despawned (vanilla): 0 once the nearest player was more than despawn-blocks from it
  (farthest; only where the server's simulation distance still ticks its chunk that far), else 1 in 800 a tick after
  idle-despawn-s of away-s with none within idle-despawn-blocks; unless it is kept: custom-kept, or it picked up one of
  drops, the names of the items the body left there (:unknown: any), that it takes?: a loot-picker spawned able to,
  loot-share x clock-special under clock."
  [mob farthest away-s drops clock]
  (let [looted (if (and (contains? loot-pickers mob) (or (= :unknown drops) (some #(takes? mob %) drops))) (* loot-share (clock-special clock)) 0)
        kept (- 1 (* (- 1 looted) (- 1 (custom-kept mob))))
        despawned (if (and (> farthest despawn-blocks) (some-> (simulation-distance) (* 16) (> despawn-blocks))) 1 (- 1 (js/Math.exp (- (* idle-despawn-per-s (max 0 (- away-s idle-despawn-s)))))))]
    (- 1 (* (- 1 kept) despawned))))

(def stroll-idle-s "Seconds a hostile with no player within idle-despawn-blocks keeps strolling (vanilla: none at 100 ticks of no action)." 5)

(defn stayed-near
  "The chance an idle hostile has not wandered from where it was seen, after near-s seconds with a player within
  idle-despawn-blocks (it strolls only then, and stroll-idle-s past): (1 + s/b)^-a, the Gamma(a, b) prior on its leaving
  rate (::wander-leaves over ::wander-seconds), as jobs.lib.memory-age prices a remembered cell."
  [near-s]
  (js/Math.pow (+ 1 (/ (+ (max 0 near-s) stroll-idle-s) (settings/get settings ::wander-seconds))) (- (settings/get settings ::wander-leaves))))

(defn spawned-there
  "The chance a mob spawned since is at a place that may spawn one (any cell of it not seen lit), after seconds with the
  nearest player 32 to 128 blocks off: vanilla spawns at a steady rate, each mob idles idle-despawn-s and then despawns
  idle-despawn-per-s, so the count there is Poisson, the rate times the expected life so far. The rate is the one at which
  a spot of any light is held (k+1)/(n+2) of the time, ::spot-occupied (the logs carry no light, so the rate is not scaled
  by the spot's dark share again)."
  [seconds]
  (let [{:keys [k n]} (settings/get settings ::spot-occupied)
        held (/ (inc k) (+ n 2))
        idle (min seconds idle-despawn-s)
        life (+ idle (/ (- 1 (js/Math.exp (- (* idle-despawn-per-s (max 0 (- seconds idle-despawn-s)))))) idle-despawn-per-s))]
    (- 1 (js/Math.pow (- 1 held) (/ life (+ idle-despawn-s (/ 1 idle-despawn-per-s)))))))

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
  "vanilla's damage to a player from a mob's hit, arrow or blast at the ::difficulty setting, for normal damage d."
  [d]
  (case (difficulty)
    0 0
    1 (min (+ (/ d 2) 1) d)
    3 (* 1.5 d)
    d))

(def measured-hit-rate #{"zombie" "husk" "drowned" "zombie_villager"})

(def bow-mobs #{"skeleton" "stray" "parched" "bogged"})

(defn shot-rate
  "Arrows a second of a skeleton kind (vanilla AbstractSkeleton: a 20-tick draw plus an attack interval of 20 ticks on hard, 40 otherwise),
  every one counted a hit (how many miss is not measured)."
  []
  (if (= 3 (difficulty)) 0.5 (/ 1 3)))

(defn lone-hit-rate
  "Hits a second of a lone sword fight: the measured rates of the zombie family and spiders, a skeleton's shot-rate, else one a second."
  [mob-name]
  (cond (= trident-kind mob-name) (settings/get settings ::trident-rate)
        (contains? bow-mobs mob-name) (shot-rate)
        (contains? measured-hit-rate mob-name) (settings/get settings ::hit-rate)
        (contains? #{"spider" "cave_spider"} mob-name) (settings/get settings ::spider-hit-rate)
        :else 1))

(defn hit-rate-shape
  "The shape of the Gamma over lone-hit-rate of ranged mob-name, the mobs of a kind sharing the one rate: ::hit-rate-hits + 0.5
  for the measured families; nil for a vanilla cadence (a trident drowned's throws, a skeleton's shots); 1 for a rate with no hits counted,
  an exponential (the maximum-entropy prior of a positive rate of known mean)."
  [mob-name]
  (cond (or (= trident-kind mob-name) (contains? bow-mobs mob-name)) nil
        (contains? measured-hit-rate mob-name) (+ 0.5 (settings/get settings ::hit-rate-hits))
        :else 1))

(defn base-threat
  "{:threat :hits} of mob-name over seconds of exposure, before armour; hits is the expected count (rate x seconds, rate
  default a skeleton's shot-rate, else one a second)."
  ([mob-name seconds] (base-threat mob-name seconds nil))
  ([mob-name seconds rate]
   (if (= "creeper" mob-name)
     {:threat (difficulty-scale (creeper-blast)) :hits 1}
     (let [hits (* seconds (or rate (if (contains? bow-mobs mob-name) (shot-rate) 1)))]
       {:threat (* hits (difficulty-scale (get mob-hit mob-name 3))) :hits hits}))))

(defn mob-hurt
  "The damage mob-name deals over seconds of exposure to a body with armour stats (jobs.lib.cost.armour/armour-stats),
  threat-fn applied to the threat before armour (default identity); rate is the hits a second (default base-threat's: the measured lone-fight rates, lone-hit-rate, are for fight-damage)."
  ([stats mob-name seconds] (mob-hurt stats mob-name seconds identity nil))
  ([stats mob-name seconds threat-fn] (mob-hurt stats mob-name seconds threat-fn nil))
  ([stats mob-name seconds threat-fn rate]
   (let [{:keys [threat hits]} (base-threat mob-name seconds rate)]
     (if (= "witch" mob-name)
       (threat-fn threat)
       (armour/after-armour stats (damage-type mob-name) (threat-fn threat) hits)))))
