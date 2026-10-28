(ns jobs.lib.cost.gear
  "What a tool, weapon or armour piece nothing yields is worth, since it cannot be replaced: what its uses save, one formula.
  A use saves seconds against the stone tool (a tool: a block of the reference hardness dug; a weapon: a mob of the
  reference hp killed) or the bare body (armour: a reference hit, an elytra a reference fall), each hp at the walking
  price; a piece is worth that times its durability. Pure."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.weapon :as weapon]))

(defn dig-time
  "Seconds digging a block of hardness with a tool of tier (at least the :dig basis's default tier)."
  [{:keys [hardness-s max-hardness min-s default-tier speed]} tier hardness]
  (max min-s (/ (* hardness-s (min max-hardness hardness)) (get speed (max default-tier tier) (get speed default-tier)))))

(defn hit-saved
  "hp one hit of hit hp costs less to a body wearing the piece than bare."
  [item hit]
  (let [stats (armour/armour-stats {"torso" {:name item}})]
    (- hit (armour/after-armour stats :melee hit 1))))

(defn weapon? [item] (boolean (re-find #"(_sword|^trident|^mace)$" item)))

(defn use-saved
  "Seconds one use of item saves against the stone tool or the bare body (0 when it saves none)."
  [{:keys [dig gear]} item]
  (let [{:keys [ref-hit ref-fall ref-mob-hp ref-hardness]} gear
        stone-tier (weapon/tool-tier "stone_pickaxe")]
    (max 0
         (cond
           (= "elytra" item) (* (health/walk-hp-seconds) (armour/fall-damage {} ref-fall))
           (pos? (armour/piece-points item)) (* (health/walk-hp-seconds) (hit-saved item ref-hit))
           (weapon? item) (- (weapon/kill-seconds "stone_sword" ref-mob-hp) (weapon/kill-seconds item ref-mob-hp))
           :else (- (dig-time dig stone-tier ref-hardness) (dig-time dig (weapon/tool-tier item) ref-hardness))))))

(defn use-worth
  "Seconds item is worth with durability uses: its uses times what each saves."
  [basis item durability]
  (* durability (use-saved basis item)))
