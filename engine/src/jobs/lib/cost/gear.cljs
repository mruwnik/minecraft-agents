(ns jobs.lib.cost.gear
  "What a tool, weapon or armour piece nothing yields is worth, since it cannot be replaced: what its uses save, one formula.
  A use saves seconds against the stone tool (a tool: a block of the reference hardness dug; a weapon: a mob of the
  reference hp killed) or the bare body (armour: a reference hit, an elytra a reference fall), each hp at the walking
  price; a piece is worth that times its durability. Pure."
  (:require [clojure.string :as str]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.weapon :as weapon]))

(defn tier-speed
  "Mining speed of the weakest tool of harvest tier (at least the :dig basis's default tier)."
  [{:keys [default-tier]} tier]
  (let [t (max default-tier tier)]
    (apply min (map :speed (filter #(<= t (:tier %)) (vals weapon/tool-material))))))

(defn dig-time
  "Seconds digging a block of hardness with a tool of mining speed, as the game takes it: whole ticks, 30 x hardness / speed
  for a tool that harvests the block, 100 x hardness / speed for one that cannot (:unfit-s). harvests? defaults to true."
  ([dig speed hardness] (dig-time dig speed hardness true))
  ([{:keys [hardness-s unfit-s max-hardness min-s]} speed hardness harvests?]
   (let [ticks (Math/ceil (- (/ (* 20 (if harvests? hardness-s unfit-s) (min max-hardness hardness)) speed) 1e-9))]
     (max min-s (/ ticks 20)))))

(defn hit-saved
  "hp one hit of hit hp costs less to a body wearing the piece than bare."
  [item hit]
  (let [stats (armour/armour-stats {"torso" {:name item}})]
    (- hit (armour/after-armour stats :melee hit 1))))

(defn weapon? [item] (boolean (re-find #"(_sword|^trident|^mace)$" item)))

(defn tool-kind
  "The block tag a tool digs best (\"iron_axe\" -> \"mineable/axe\"), nil for an item that is no digging tool."
  [item]
  (some->> (re-find #"_(pickaxe|axe|shovel|hoe)$" item) second (str "mineable/")))

(defn dig-saved
  "Seconds a use of the digging tool item saves on the reference block over a stone pickaxe: it digs at its speed only when
  it is the block's tool (else speed 1), and a tool below the block's harvest tier takes the slow unfit time."
  [{:keys [dig gear]} item]
  (let [{:keys [ref-hardness ref-material ref-tier]} gear
        effective? (boolean (some-> (tool-kind item) (str/includes? ref-material)))
        speed (if effective? (weapon/tool-speed item) 1)
        harvests? (and effective? (<= ref-tier (weapon/tool-tier item)))]
    (- (dig-time dig (weapon/tool-speed "stone_pickaxe") ref-hardness)
       (dig-time dig speed ref-hardness harvests?))))

(defn use-saved
  "Seconds one use of item saves against the stone tool or the bare body (0 when it saves none)."
  [{:keys [gear] :as basis} item]
  (let [{:keys [ref-mob]} gear
        ref-hit (get threat/mob-hit ref-mob)
        ref-mob-hp (get weapon/mob-max-health ref-mob)]
    (max 0
         (cond
           (= "elytra" item) (* (health/walk-hp-seconds) (armour/fall-damage {} armour/ref-fall))
           (pos? (armour/piece-points item)) (* (health/walk-hp-seconds) (hit-saved item ref-hit))
           (weapon? item) (- (weapon/kill-seconds "stone_sword" ref-mob-hp) (weapon/kill-seconds item ref-mob-hp))
           :else (dig-saved basis item)))))

(defn use-worth
  "Seconds item is worth with durability uses: its uses times what each saves."
  [basis item durability]
  (* durability (use-saved basis item)))
