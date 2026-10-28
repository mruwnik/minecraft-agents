(ns jobs.lib.cost.gear
  "What a tool, weapon or armour piece nothing yields is worth, since it cannot be replaced: what its uses save, one formula.
  A use saves seconds against the stone tool (a tool: a block of the reference hardness dug; a weapon: a mob of the
  reference hp killed) or the bare body (armour: a reference hit, an elytra a reference fall), each hp at the walking
  price; a piece is worth that times its durability. Pure."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.weapon :as weapon]))

(defn tier-speed
  "Mining speed of the weakest tool of harvest tier (at least the :dig basis's default tier)."
  [{:keys [default-tier speed]} tier]
  (get speed (max default-tier tier) (get speed default-tier)))

(defn dig-time
  "Seconds digging a block of hardness with a tool of mining speed, as the game takes it: whole ticks, 30 x hardness / speed
  (a tool that harvests the block)."
  [{:keys [hardness-s max-hardness min-s]} speed hardness]
  (let [ticks (Math/ceil (- (/ (* 20 hardness-s (min max-hardness hardness)) speed) 1e-9))]
    (max min-s (/ ticks 20))))

(defn hit-saved
  "hp one hit of hit hp costs less to a body wearing the piece than bare."
  [item hit]
  (let [stats (armour/armour-stats {"torso" {:name item}})]
    (- hit (armour/after-armour stats :melee hit 1))))

(defn weapon? [item] (boolean (re-find #"(_sword|^trident|^mace)$" item)))

(defn use-saved
  "Seconds one use of item saves against the stone tool or the bare body (0 when it saves none)."
  [{:keys [dig gear]} item]
  (let [{:keys [ref-mob ref-hardness]} gear
        ref-hit (get threat/mob-hit ref-mob)
        ref-mob-hp (get weapon/mob-max-health ref-mob)
        stone-speed (weapon/tool-speed "stone_pickaxe")]
    (max 0
         (cond
           (= "elytra" item) (* (health/walk-hp-seconds) (armour/fall-damage {} armour/ref-fall))
           (pos? (armour/piece-points item)) (* (health/walk-hp-seconds) (hit-saved item ref-hit))
           (weapon? item) (- (weapon/kill-seconds "stone_sword" ref-mob-hp) (weapon/kill-seconds item ref-mob-hp))
           :else (- (dig-time dig stone-speed ref-hardness) (dig-time dig (weapon/tool-speed item) ref-hardness))))))

(defn use-worth
  "Seconds item is worth with durability uses: its uses times what each saves."
  [basis item durability]
  (* durability (use-saved basis item)))
