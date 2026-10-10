(ns jobs.survival.retreat
  (:require [clojure.string :as str]
            [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.click :as click]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.goal :as goal]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.escape :as escape]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-home :as retreat-home]
            [jobs.survival.retreat-refuge :as refuge]
            [jobs.survival.retreat-walk :as walk]))

(def doc
  "Flee from the hostiles in range, one whole flight per round: run until no mob chases the body any more.
  A real danger (jobs.lib.danger: a mob with a walkable way to the body, or a ranged one with a line of fire) within
  :radius (ranged ones :ranged-radius) starts a chase, or is seen afresh. A mob stops chasing (vanilla) once it is
  gone (dead, despawned, untracked), beyond its follow range (jobs.lib.threats: zombie 35, most 16), out of line of
  sight for :lost-s, or has no walkable way to the body (nor, ranged, a line of fire).
  Each step, in this order:
  1. A door, gate or trapdoor standing open within a hand's reach and nearer the nearest chaser than the body is shut
     with one click (retreat.door-shut info). Each door is clicked at most once a flight.
  2. Runs into a closed building first (not while a creeper chases: its blast would take the door and what is inside): the latest :bed or :home within :home-range, or a room behind a wooden door or
     gate the body has seen (jobs.survival.retreat-home/rooms, bounded by its settings), that is a closed room (walls,
     doors that shut: jobs.lib.reach/room) whose door it walks to (over seen blocks, at its own speed) ahead of every
     melee chaser (jobs.lib.cost/reach-first?), the fewest steps of those, with a go-to child to the room cell inside
     that door (retreat.home info, once a flight); a step that gets no nearer drops that room for the flight. Not while
     low in a ranged chaser's line of fire (the cover step below comes first). Any cell of the room counts as home:
     there an open door of the room is shut (a step back to one out of reach first), and a chaser still with a way in
     gets the cornered options (4.), no pit or pocket dug in the hut: it does not walk out.
  Underground (shelter/buried?) it first takes the stairs the body cut (ledger/ways-of: the highest stair cell within 14
  blocks, above it and no nearer a chaser), and with none digs a side pocket (as below) before it walks across the open cave,
  unless a creeper or a melee mob is within cover-gap.
  3. Walks a short step (:step blocks) away from all chasers (nearer ones weigh more) with a go-to child (:escalate false),
     leaning toward the first closed room of 2. (else the :goal, the destination of the walk the flight interrupted; else the latest :bed or :home within :home-range) when not through the
     hostiles (a closed room behind them: turned retreat-walk ::flank-turn degrees toward it, to go round them, while it still gains on them so:
     jobs.lib.cost/outruns-at?), avoiding :hazard positions.
     When a wall blocks the way away it turns up to 120 degrees towards open ground (at least 2 clear cells).
     It prefers, in this order: a step whose end is out of every ranged chaser's line of fire (cover: leaves stop arrows),
     the full step that takes least time (a swim block costs :swim-factor), the most open.
     It first looks away and to either side where it has not seen the ground (at most 3 looks); a cell still unseen
     reads as rock.
     Eats one bite a step (up to food 20) when the nearest chaser is at least :eat-gap blocks away and food is carried.
  Low on health (at most :hide-health) in a seen ranged chaser's line of fire (over the cells it has seen) with no
  covered step, it takes cover where it stands first: seal in, else dig down and plug, else a side pocket (as below);
  none of them possible, it walks on. A seal begun where it stands is finished before it walks on (refuge/sealing-here?).
  Not while a creeper chases it or a melee mob is within cover-gap: it walks (or backs off, cornered) first.
  4. Cornered (no open direction worth a walk, or the walk is blocked, or a melee hostile within 3 hit the body since the last step: it cannot be outrun, and the fight is weighed over those hostiles alone; checked afresh every step) with no hostile within
     :radius: holds a second (wait, why cornered) and looks again; after :no-gain-steps such steps or holds with no gain the flight ends :keeps-off. With one within :radius: takes the safest option it has not yet failed.
     - an angry enderman near (one the client flags angry, that hurt the body this flight or is within 3 blocks; it teleports; a walk is no flight): at once, before any walk: water within 6 blocks (it keeps away from water; the body steps in and holds), else a seen 2-high gap under a roof within 8 blocks (a 3-tall enderman cannot enter; walks there and holds), else seal, pit, pocket, last of all fight.
     - a drowned near while the body is in water (and no fight wins): swims to the nearest seen dry stand within 16 blocks first (past the mob if need be), before seal, pillar or a fist fight.
     - fight (jobs.survival.fight-back) only when jobs.lib.cost/fight-damage leaves :reserve health. Never against a creeper.
     - seal in: fill the open sides at feet and head height and the roof (dig-in's 1x1 cells) with carried :blocks,
       at most :max-places a step, the sides towards a seen ranged hostile first (feet, then head: the first blocks
       stop its arrows). It first steps to the middle of its cell, and does not place while a hostile's hitbox
       overlaps a cell to fill. An open door is shut, not filled. A cell that answers occupied (torch, chest, bed) is left alone.
       When only such cells stay open the seal has failed. Every seal block goes to the scaffold ledger (purpose :retreat-plug), taken back by jobs.access.cleanup once no hostile is near.
     - pillar up 3 (jobs.access.pillar, the carried block with the most, at least 3). Needs a solid floor, free cells above and no slope or ledge a mob could climb to beside its top.
       Not against a ranged mob or a spider (it climbs the pillar). Every block goes to the scaffold ledger (purpose :pillar) for jobs.access.cleanup.
     - back off: a step of up to 2 cells in any of 8 directions that gains at least a block on the hostile.
     - dig down and plug: a pit 2 deep under a solid side, else 3, then carried or dug blocks beside and over the head
       (ledger purpose :retreat-plug). It never drops onto a floor it has not seen solid: it zigzags over an open side
       column, digging beside the body and looking down the open column first; straight down only onto known floors.
       Every cell must be solid, harvestable with what is carried, with no fluid beside and solid under it.
     - side pocket: with no block to seal with, dig a pocket beside the body (feet and head cell, solid on every other side,
       harvestable with what is carried), step in and seal the way in with the dug blocks (the seal option again).
     - The pit and the pocket are dropped when a dig shows fluid or a cave (a seen open cell) in their shell. Lava is first
       filled with a carried block (:on-lava :seal, the default; :stop leaves it).
     - last of all, fight with the best weapon or tool (pickaxe, shovel, hoe) or the fist.
     Order: fight if it wins, then seal, pillar (not against a ranged mob or a spider), back off, pit, pocket, fight.
     Against a creeper back off comes first.
     An option that fails is not tried again until all have failed. Then, after a second's hold, all are tried again
     (one retreat_blocked warning per flight).
  Sealed in, up a pillar or down a pit it hides (one retreat_sealed warning; a declared hold, wait why hiding) while a
  hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius) would have a walkable way to
  the refuge if its own blocks were gone, and for :quiet-s more after the last such danger (a silent mob is forgotten
  after a few seconds); then the flight ends :hidden. After :hide-s in the refuge with no real danger on the body as it
  stands (jobs.lib.danger/danger-near?, the hostile-near trigger's test) it ends :hidden even while that hostile stays
  (info retreat.hide-ended): the refuge stays, its plugs too while a hostile is near (jobs.access.cleanup).
  A chase has no time limit. With no new best gap to the nearest chaser in :no-gain-steps steps it takes the
  cornered options instead of walking on; stopped :cannot_escape only once every option failed in two sweeps in a row
  with no gain between.
  Returns done {:fled [ids] :ended :gone|:far|:lost|:closed|:hidden|:keeps-off|:none}, or stopped :cannot_escape.
  Memory: one :threat entry per mob fled (jobs.lib.threats); the third from one mob within 5 min warns hostile.chased.
  A cell in another's zone is used only as a last resort (retreat.trespass-last-resort warning).")

(a/defargs args
  {:radius {:doc "hostiles within this many blocks start a flight" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks start a flight" :spec (a/num-in 0 nil) :default 16}
   :eat-gap {:doc "with at least this many blocks to the nearest hostile, eat a bite per flee step" :spec (a/num-in 0 nil) :default 12}
   :goal {:doc "a position {:x :y :z} the flight walks to with go-to legs (planner danger costs keep it round the hostiles) while the body outruns them (dropped within 3 blocks of it or after a leg that lost ground), else leans toward, like a refuge; nil: away only" :spec goal/pos-spec :default nil}
   :step {:doc "blocks per walk" :spec (a/num-in 1 nil) :default 6}
   :home-range {:doc "a flight leans towards the latest :bed or :home only when it lies within this many blocks" :spec (a/num-in 0 nil) :default 64}
   :weapons {:doc "item name substrings that count as weapons, for a cornered fight" :spec (a/coll-of (a/or-of string? keyword?)) :default combat/default-weapons}
   :reserve {:doc "health a cornered fight must be expected to leave" :spec (a/num-in 0 nil) :default cost/default-reserve}
   :blocks {:doc "names of the blocks a cornered body may seal itself in with" :spec (a/coll-of (a/or-of string? keyword?)) :default lb/building-blocks}
   :max-places {:doc "seal placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pocket or pit is dropped and the lava left" :spec #{:seal :stop} :default :seal}
   :lost-s {:doc "a mob out of line of sight this many seconds has stopped chasing" :spec (a/num-in 0 nil) :default 4}
   :quiet-s {:doc "a hidden body keeps its refuge this many seconds after the last danger" :spec (a/num-in 0 nil) :default 30}
   :hide-s {:doc "a hidden body hands back after this many seconds in its refuge, though a danger it keeps off stays, once no real danger is on it as it stands" :spec (a/num-in 0 nil) :default 60}
   :swim-factor {:doc "blocks of walking a block of swimming costs a flight; nil: jobs.lib.cost/swim-factor of the boots" :spec (a/num-in 1 nil) :default nil}
   :hide-health {:doc "at or under this health, in a ranged chaser's line of fire with no covered step, take cover where it stands before walking on" :spec (a/num-in 0 20) :default 12}
   :no-gain-steps {:doc "flight steps without a new best gap to the nearest chaser before the cornered options are tried" :spec (a/int-in 1 nil) :default 20}})

(def tool-weapons
  "Item name substrings a cornered body with no weapon and no seal swings: any of them beats the fist."
  ["_pickaxe" "_shovel" "_hoe"])

(def flight-timeout-s
  "Bound of one step of the flight: the mob moves, so the next step aims again."
  5)

(defn home-pos
  "The position of the latest :bed or :home entry within :home-range blocks of the body, or nil."
  [c]
  (let [pos (->> [(ctx/latest c :bed) (ctx/latest c :home)]
                 (remove nil?)
                 (sort-by :t >)
                 first
                 :data
                 :pos)]
    (when (and pos (<= (u/dist pos (u/self-pos c)) (:home-range (:args c)))) pos)))

(defn refuges
  "The closed rooms to flee to: the latest :home or :bed (home-pos) and those behind the doors the body has seen
  (jobs.survival.retreat-home/rooms)."
  [c]
  (retreat-home/rooms c (home-pos c)))

(defn check [_c] true)

(defn block-at-fn
  "pos -> the block name the body sees or remembers there. A cell it has not seen reads as rock (behind the body or a
  wall is no free way until it looks: glance-away!); nil (open: the walk finds out) when the cell is not loaded."
  [p]
  (let [at (access/sensed-at p access/hidden-guess)]
    (fn [{:keys [x y z]}] (at [x y z]))))

(defn ^:async fight!
  "Fight back with the best of weapons (the fist when none is carried) whatever
  the health, kept while the hostile stays close: :again, nil when the fight
  cannot reach any hostile (the option failed this flight)."
  [c weapons]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (ctx/update-mem! c assoc :cornered true)
    (let [r (await (ctx/call-child c :cornered 'jobs.survival.fight-back
                                   {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons
                                    :skip (flight/dead-ids c)}))]
      (ctx/update-mem! c update :dead #(into (vec %) (concat (get-in (ctx/mem c) [:children :cornered :killed])
                                                             (:killed (ctx/child-result c :cornered)))))
      (if (= :declined r)
        (do (flight/tried! c :fight) nil)
        :again))))

;; ------------------------------------------------------------------ cornered: the safest option

(defn escape-order
  "Pure: the cornered options to try, safest first. {:win? the odds say a fight leaves reserve health (never true
  for a creeper), :flyer? a phantom is near (it outflies a walker: shelter, else fight), :creeper? one is near (it comes first), :enderman? an angry enderman is near (angry-enderman?) (it teleports, so no walk or back-off, and a
  pillar is within its reach: the body enters water within water-reach, which it avoids, else walks under a 2-high roof within roof-reach, which it cannot enter, else shuts itself in), :ranged? the threat shoots, :climber? it is a spider (it climbs the pillar), :home? the body is in its home
  room: it digs no pit or pocket in its own floor or walls, :swim? the body is in water with a drowned near: swimming out to dry land comes first unless a fight wins}."
  [{:keys [win? creeper? enderman? flyer? ranged? climber? swim? home?]}]
  (cond->> (cond
             creeper? [:back-off :seal :pillar :pit :pocket :fight]
             enderman? [:water :roof :seal :pit :pocket :fight]
             win? [:fight]
             flyer? [:seal :pit :pocket :fight]
             :else (into (if (or ranged? climber?) [:seal] [:seal :pillar]) [:back-off :pit :pocket :fight]))
    (and swim? (not creeper?) (not enderman?) (not win?)) (into [:swim-out])
    home? (filterv (complement #{:pit :pocket}))))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (flight/near-known (:primitives c) (set (flight/dead-ids c)) radius ranged-radius)))

(def enderman-close
  "Blocks within which an enderman counts as angry though it has not hurt the body yet (a calm one does not come this near)."
  3)

(defn angry-enderman?
  "Whether one of hostiles is an enderman that is angry (the client's flag), hurt the body this flight (its health is under the flight's start) or
  stands within enderman-close: a calm one is walked away from like any mob, never sealed against."
  [c hostiles]
  (let [p (:primitives c)
        hurt? (< (.-health (.self p)) (:flight-health (ctx/mem c) 0))]
    (boolean (some #(and (combat/enderman? %) (or hurt? (true? (.-angry %)) (<= (danger-q/mob-distance p %) enderman-close))) hostiles))))

(def hit-reach
  "Blocks within which a melee hostile counts as the one that hit the body."
  3)

(defn hitters
  "Of hostiles, the melee ones (not a creeper or an enderman) within hit-reach."
  [c hostiles]
  (filterv #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                 (<= (danger-q/mob-distance (:primitives c) %) hit-reach))
           hostiles))

(defn hit-on-the-run!
  "Whether the body lost health since the previous flight step with a hitter near: a chaser that hits it cannot be
  outrun. Noted in job memory (:hit-on-the-run), with the health, for the cornered options' odds."
  [c]
  (let [health (.-health (.self (:primitives c)))
        prev (:step-health (ctx/mem c))
        hit? (boolean (and prev (< health prev) (seq (hitters c (near-hostiles c)))))]
    (ctx/update-mem! c assoc :step-health health :hit-on-the-run hit?)
    hit?))

(defn fight-wins?
  "Whether fighting hostiles with the best weapon carried is expected to leave :reserve health
  (jobs.lib.cost/fight-damage); never against a creeper."
  [c hostiles]
  (let [p (:primitives c)
        self (.self p)
        {:keys [weapons reserve]} (:args c)]
    (boolean
     (and (seq hostiles)
          (not-any? combat/creeper? hostiles)
          (<= (cost/fight-damage {:weapon (combat/best-weapon p weapons)
                                  :equipment (cost/equipment-of (.-equipment self))
                                  :mobs (map (fn [e] {:name (.-name e) :distance (danger-q/mob-distance p e) :hits 0}) hostiles)})
              (- (.-health self) reserve))))))

(defn back-off-target
  "A cell to back off to: of the ends of the open cells (refuge/up to 2, see walk-cells) in the eight compass directions,
  the farthest from threat, when it gains at least a block on it and passes no hazard; nil when none does."
  [block-at from threat hazards]
  (let [now (u/dist from threat)]
    (->> (range 0 360 45)
         (keep #(peek (walk/walk-cells block-at from (walk/rotate [1 0] %) 2)))
         (remove #(walk/near-hazard? hazards from %))
         (filter #(>= (u/dist % threat) (inc now)))
         (sort-by #(- (u/dist % threat)))
         first)))

(defn ^:async step!
  "One flee step to target as a go-to child (a flee never digs or pillars: :escalate false; known dangers stay costed):
  true when the body arrived or got on a leg nearer, false when go-to gave up, waited or was declined."
  [c slot target range]
  (let [r (await (ctx/call-child c slot 'jobs.movement.go-to {:pos target :range range :escalate false :at-night :go
                                                              :warn false :leg-s flight-timeout-s
                                                              :retry false :look-round false :allow-over-budget true}))
        res (when (= :done r) (ctx/child-result c slot))]
    (boolean (or (:arrived res) (:leg res)))))

(defn ^:async back-off!
  "Step back from threat (back-off-target): :again, nil when there is no such cell or the walk is blocked."
  [c threat]
  (when-let [target (back-off-target (block-at-fn (:primitives c)) (u/self-pos c) (danger-q/mob-pos (:primitives c) threat)
                                     (keep (comp :pos :data) (ctx/entries c :hazard)))]
    (if-not (await (step! c :back-off target 0))
      (do (flight/tried! c :back-off) nil)
      :again)))

(def water-reach
  "Blocks within which a body hunted by an enderman looks for water to stand in."
  6)

(defn water-cell
  "The nearest cell within reach (blocks, horizontally and up to 2 vertically) of from that block-at reads as water, or nil."
  [block-at from reach]
  (let [[fx fy fz] (mapv #(js/Math.floor %) [(:x from) (:y from) (:z from)])]
    (->> (for [dx (range (- reach) (inc reach)) dy (range -2 3) dz (range (- reach) (inc reach))
               :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))
               :let [cell {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)}]
               :when (= "water" (block-at cell))]
           cell)
         (sort-by #(u/dist from {:x (+ 0.5 (:x %)) :y (:y %) :z (+ 0.5 (:z %))}))
         first)))

(defn ^:async water!
  "Stand in water (an enderman keeps away from it): a step to the nearest water within water-reach, a short hold once
  in it. :again, nil when there is none or the walk is blocked."
  [c]
  (let [block-at (block-at-fn (:primitives c))
        self (u/self-pos c)]
    (if (= "water" (block-at {:x (js/Math.floor (:x self)) :y (js/Math.floor (:y self)) :z (js/Math.floor (:z self))}))
      (do (await (ctx/act c :wait #js {:ms (flight/wait-ms) :why "in water, away from the enderman"})) :again)
      (when-let [cell (water-cell block-at self water-reach)]
        (if (await (step! c :water {:x (+ 0.5 (:x cell)) :y (:y cell) :z (+ 0.5 (:z cell))} 0))
          :again
          (do (flight/tried! c :water) nil))))))

(def roof-reach
  "Blocks within which a body hunted by an enderman looks for a 2-high gap under a roof."
  8)

(defn roof-cell
  "The nearest cell within reach (blocks, horizontally and 1 vertically) of from that block-at reads as a 2-high gap
  on a solid floor under a solid roof, which a 3-tall enderman cannot enter, or nil. A cell with no cell an enderman
  could stand in (3 clear above a solid floor) within 2 blocks comes first: it cannot reach one at the roof's edge."
  [block-at from reach]
  (let [[fx fy fz] (mapv #(js/Math.floor %) [(:x from) (:y from) (:z from)])
        at (fn [cell dy] (block-at (update cell :y + dy)))
        gap? (fn [cell] (and (escape/solid? (at cell -1)) (escape/passable? (at cell 0))
                             (escape/passable? (at cell 1)) (escape/solid? (at cell 2))))
        tall? (fn [cell] (and (escape/solid? (at cell -1)) (escape/passable? (at cell 0))
                              (escape/passable? (at cell 1)) (escape/passable? (at cell 2))))
        deep? (fn [cell] (not-any? tall? (for [dx (range -2 3) dz (range -2 3)]
                                           (-> cell (update :x + dx) (update :z + dz)))))]
    (->> (for [dx (range (- reach) (inc reach)) dy (range -1 2) dz (range (- reach) (inc reach))
               :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))
               :let [cell {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)}]
               :when (gap? cell)]
           cell)
         (sort-by (juxt (complement deep?) #(u/dist from {:x (+ 0.5 (:x %)) :y (:y %) :z (+ 0.5 (:z %))})))
         first)))

(defn ^:async roof!
  "Stand under a roof an enderman cannot enter: a step to the nearest roof-cell, a short hold once there. :again, nil when
  there is none or the walk is blocked."
  [c]
  (let [block-at (block-at-fn (:primitives c))
        self (u/self-pos c)
        here {:x (js/Math.floor (:x self)) :y (js/Math.floor (:y self)) :z (js/Math.floor (:z self))}]
    (if (and (escape/passable? (block-at (update here :y inc))) (escape/solid? (block-at (update here :y + 2))))
      (do (await (ctx/act c :wait #js {:ms (flight/wait-ms) :why "under a roof, out of the enderman's reach"})) :again)
      (when-let [cell (roof-cell block-at self roof-reach)]
        (if (await (step! c :roof {:x (+ 0.5 (:x cell)) :y (:y cell) :z (+ 0.5 (:z cell))} 0))
          :again
          (do (flight/tried! c :roof) nil))))))

(def swim-reach
  "Blocks within which a body in water looks for dry land to swim out to."
  16)

(defn in-water?
  "Whether the body's feet cell reads as water."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    (= "water" ((block-at-fn (:primitives c)) {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)}))))

(defn ^:async swim-out!
  "Swim to the nearest seen dry stand within swim-reach (a go-to step, past the mob if that is the only way): :again,
  nil when there is none or the walk is blocked."
  [c]
  (let [p (:primitives c)
        block-at (block-at-fn p)
        feet (u/self-pos c)
        dry? (fn [{:keys [x y z]}] (and (not= "water" (block-at {:x x :y y :z z})) (not= "water" (block-at {:x x :y (inc y) :z z}))))
        to (first (step-off/candidates p {:x (js/Math.floor (:x feet)) :y (js/Math.floor (:y feet)) :z (js/Math.floor (:z feet))}
                                       {:reach swim-reach :ok? dry?}))]
    (when to
      (if (await (step! c :swim-out {:x (+ 0.5 (:x to)) :y (:y to) :z (+ 0.5 (:z to))} 0))
        :again
        (do (flight/tried! c :swim-out) nil)))))

(defn ^:async blocked!
  "Every option failed: forget them all, warn once a flight, hold a moment (wait, why cornered) and go on: the next step
  tries them again from the start (the danger still stands). The second sweep in a row ends the flight :cannot-escape."
  [c why]
  (ctx/update-mem! c #(-> % (dissoc :tried) flight/count-sweep))
  (when-not (:blocked-warned (ctx/mem c))
    (ctx/update-mem! c assoc :blocked-warned true)
    (ctx/emit! c :retreat_blocked :warn {:text (str why "; every escape failed, trying them again")}))
  (await (ctx/act c :wait #js {:ms (flight/wait-ms) :why "cornered"}))
  (if (flight/cannot-escape? (ctx/mem c)) (flight/end-flight! c :cannot-escape) :again))

(defn ^:async cornered!
  "Nowhere worth walking to: the first option of escape-order that it can take and has not failed since the last
  start over (blocked!). home?: the body is in its home room."
  ([c why] (await (cornered! c why false)))
  ([c why home?]
  (let [{:keys [weapons]} (:args c)
        hostiles (near-hostiles c)
        threat (first hostiles)
        order (escape-order {:win? (fight-wins? c (if (:hit-on-the-run (ctx/mem c)) (hitters c hostiles) hostiles))
                             :creeper? (boolean (some combat/creeper? hostiles))
                             :enderman? (angry-enderman? c hostiles)
                             :flyer? (boolean (some combat/flyer? hostiles))
                             :ranged? (boolean (and threat (combat/ranged? threat)))
                             :climber? (boolean (some combat/climber? hostiles))
                             :swim? (and (boolean (some #(= "drowned" (.-name %)) hostiles)) (in-water? c))
                             :home? home?})]
    (loop [[option & more] order]
      (let [r (when-not (flight/tried? c option)
                (case option
                  :fight (await (fight! c (if (= [:fight] order) weapons (into (vec weapons) tool-weapons))))
                  :seal (await (refuge/hide! c))
                  :pillar (await (refuge/pillar! c))
                  :water (await (water! c))
                  :roof (await (roof! c))
                  :swim-out (await (swim-out! c))
                  :back-off (when threat (await (back-off! c threat)))
                  :pit (await (refuge/pit! c))
                  :pocket (await (refuge/pocket! c))))]
        (cond
          (some? r) r
          (seq more) (recur more)
          :else (await (blocked! c why))))))))

(def glance-turns
  "Degrees from the way away a flight turns to look before it chooses: the view cone (about 100 degrees wide) then
  takes in every turn walk/choose-target tries."
  [0 90 -90])

(a/defargs settings
  {::lost-memory-ms {:default 30000 :doc "Milliseconds a mob the flight lost (its :threat entry) still counts, at the place it was last known, when choosing where to flee; a creeper counts triple." :spec (a/int-in 0 nil)}
   ::glance-reach {:default 3 :doc "Blocks along a direction to the feet cell a glance looks at." :spec (a/int-in 0 nil)}
   ::glance-memory {:default 24 :doc "Most glanced cells a flight remembers." :spec (a/int-in 0 nil)}
   ::cover-gap {:default 5 :doc "Blocks within which a melee chaser keeps a low body from taking cover where it stands." :spec (a/num-in 0 nil)}})
(defn glance-reach [] (settings/get settings ::glance-reach))

(defn glance-memory [] (settings/get settings ::glance-memory))

(defn cover-gap [] (settings/get settings ::cover-gap))

(defn unseen-before-wall?
  "Whether a cell the body has not sensed lies in the columns along d from the first, up to glance-reach or the first
  column whose seen feet or head cell blocks the way (the cells behind it cannot change the walk)."
  [p from d y]
  (loop [k 1]
    (when (<= k (glance-reach))
      (let [[x z] (walk/column-along from d k)
            open-floor? (walk/floorless? (u/seen-name p {:x x :y (dec y) :z z}))
            unknown (some #(dig-look/unknown? p [x % z]) (range (if open-floor? (- y 2) (dec y)) (+ y 2)))
            blocked? (some (fn [yy] (and (not (dig-look/unknown? p [x yy z]))
                                         (not (walk/passable? (u/seen-name p {:x x :y yy :z z})))))
                           [y (inc y)])]
        (cond
          unknown true
          blocked? false
          :else (recur (inc k)))))))

(defn ^:async glance-away!
  "Turn to look along each glance-turns direction from dir ([ux uz]) with a cell the body has not sensed in its first
  glance-reach columns (feet, head, floor, and under an open floor the cell below; not past a seen wall), at the feet
  cell glance-reach blocks along, as a player turns round before running: at most three looks, a sight pass after
  each, none at a cell already glanced this flight."
  [c dir]
  (let [p (:primitives c)
        from (u/self-pos c)
        y (js/Math.floor (:y from))]
    (loop [[turn & more] glance-turns]
      (when turn
        (let [d (walk/rotate dir turn)
              [gx gz] (walk/column-along from d (glance-reach))
              cell [gx y gz]]
          (when (and (not (some #{cell} (:glanced (ctx/mem c)))) (unseen-before-wall? p from d y))
            (ctx/update-mem! c update :glanced #(vec (take-last (glance-memory) (conj (vec %) cell))))
            (await (dig-look/look-at! c cell))
            (look/see! c))
          (recur more))))))

(defn cover-fn
  "(feet cell -> whether it is out of every seen ranged threat's line of fire, over the cells the body has seen: an
  unseen cell stops no arrow), nil when no seen threat is ranged (a heard one's rough spot is no line of fire)."
  [p threats]
  (when-let [ranged (seq (filter #(and (combat/ranged? %) (danger-q/seen-only? %)) threats))]
    (let [arrow-at (fn [x y z] (reach/arrow-kind-of (u/seen-block p {:x x :y y :z z})))
          at (mapv #(danger-q/mob-pos p %) ranged)]
      #(walk/out-of-fire? arrow-at at %))))

(defn swim-factor
  "The flight's price of a swim block: :swim-factor, else jobs.lib.cost/swim-factor of the boots worn."
  [c]
  (or (:swim-factor (:args c)) (cost/swim-factor (.-equipment (.self (:primitives c))))))

(defn lost-memory-ms [] (settings/get settings ::lost-memory-ms))

(defn lost-spots
  "The places of the mobs a flight lost within lost-memory-ms (:threat entries, other than the chasers now on), as walk
  positions: a creeper three times, so the way away leans from it."
  [c chasing]
  (let [ids (set (map #(.-id %) chasing))]
    (->> (ctx/since c :threat (- (ctx/now c) (lost-memory-ms)))
         (map :data)
         (remove #(contains? ids (:id %)))
         (mapcat (fn [d] (when-let [pos (or (:pos d) (danger-q/rough-pos d))]
                           (repeat (if (= "creeper" (:mob d)) 3 1) pos)))))))

(defn goal-pos
  "The :goal arg ({:x :y :z} or [x y z]) as a position map, nil when none."
  [goal]
  (when goal
    (let [[x y z] (if (vector? goal) goal [(:x goal) (:y goal) (:z goal)])]
      {:x x :y y :z z})))

(defn active-goal
  "The :goal as a position, nil when none or a walk to it lost ground on the chasers this flight (:goal-off)."
  [c]
  (when-not (:goal-off (ctx/mem c)) (goal-pos (:goal (:args c)))))

(def goal-near
  "Blocks from the goal at which the flight no longer walks to it."
  3)

(defn goal-leg
  "The goal as a go-to target while the body outruns its chasers (cost/outruns-at? at the flank turn): the planner's
  danger costs route the leg round the hostiles. Once the body is within goal-near of it the goal is done for the flight
  (:goal-off), as it is after a leg that lost ground: the flight goes on away from the chasers; without that it walked
  back and forth between the goal and the way away for ever."
  [c]
  (let [goal (active-goal c)]
    (cond
      (nil? goal) nil
      (<= (u/dist (u/self-pos c) goal) goal-near) (do (ctx/update-mem! c assoc :goal-off true) nil)
      (cost/outruns-at? (retreat-home/food (:primitives c)) (walk/flank-turn)) goal)))

(def way-reach
  "Blocks within which the stairs the body cut are a way out of a flight."
  14)

(defn way-up
  "Underground, the feet cell on a stair this body cut (ledger/ways-of) that leads up and away from threats
  (walk/way-target), else nil."
  [c threats]
  (let [p (:primitives c)]
    (when (and (seq threats) (sh/buried? p))
      (walk/way-target (u/self-pos c) (mapv #(danger-q/mob-pos p %) threats) (ledger/ways-of c) way-reach))))

(defn ^:async flight-target!
  "The walk target away from threats (walk/choose-target over what the body sees, after glance-away!; cover from the
  ranged ones first, a swim slow), leaning toward the first of rooms (refuges), nil when none. covered? as cover-fn
  (given so the caller shares one)."
  ([c threats] (await (flight-target! c threats (cover-fn (:primitives c) threats) (refuges c))))
  ([c threats covered? rooms]
   (if-let [way (way-up c threats)]
     way
   (let [p (:primitives c)
         from (u/self-pos c)
         threat-pos (into (mapv #(danger-q/mob-pos p %) threats) (lost-spots c threats))
         refuge (:start (first rooms))
         goal (active-goal c)
         home (or refuge goal (home-pos c))
         flank? (and (some? (or refuge goal)) (cost/outruns-at? (retreat-home/food p) (walk/flank-turn)))]
     (await (glance-away! c (walk/direction from threat-pos home flank?)))
     (walk/choose-target (block-at-fn p) from threat-pos home (keep (comp :pos :data) (ctx/entries c :hazard))
                         (:step (:args c)) {:swim-factor (swim-factor c) :covered? covered? :flank? flank?})))))

(defn ^:async take-cover!
  "Seal in, else dig down and plug, else a side pocket (refuge), the first not failed this sweep: its step's result, nil
  when none can be taken."
  [c]
  (loop [[option & more] [:seal :pit :pocket]]
    (when option
      (let [r (when-not (flight/tried? c option)
                (case option
                  :seal (await (refuge/hide! c))
                  :pit (await (refuge/pit! c))
                  :pocket (await (refuge/pocket! c))))]
        (if (some? r) r (recur more))))))

(defn ^:async dig-pocket!
  "Underground with no stair to take, a side pocket in the rock beats a hop across the open cave: its step's result,
  nil when it is not possible. One dig attempt a flight (a hop does not clear that); once dug, the next steps seal it."
  [c]
  (let [{:keys [pocket-tried pocket-dug]} (ctx/mem c)]
    (cond
      pocket-dug (or (await (refuge/hide! c)) (do (ctx/update-mem! c dissoc :pocket-dug) nil))
      pocket-tried nil
      :else (do (ctx/update-mem! c assoc :pocket-tried true)
                (let [r (await (refuge/pocket! c))]
                  (when r (ctx/update-mem! c assoc :pocket-dug true))
                  r)))))

(defn close-danger?
  "Whether a creeper chases the body or a melee threat is within cover-gap: a seal or pit beside it is no cover (a blast
  breaks the wall, a mob stands in the cell to fill)."
  [p threats]
  (boolean (some #(or (combat/creeper? %)
                      (and (not (combat/ranged? %)) (<= (danger-q/mob-distance p %) (cover-gap))))
                 threats)))

(defn exposed-low?
  "Whether the body is at or under :hide-health with a ranged threat's line of fire on it (covered? false at its feet),
  or, a ranged threat seen, a seal begun where it stands is not finished (refuge/sealing-here?: its first blocks may
  be all that stop the arrows)."
  [c covered?]
  (boolean (and covered?
                (or (refuge/sealing-here? c)
                    (and (<= (.-health (.self (:primitives c))) (:hide-health (:args c)))
                         (not (covered? (sh/feet (:primitives c)))))))))

(defn ^:async eat-on-the-run!
  "One bite a flee step, with the nearest hostile at least :eat-gap away; none for the rest of the flight once
  nothing is left to eat or a bite fails."
  [c threat]
  (when (and (not (:ate (ctx/mem c)))
             (>= (danger-q/mob-distance (:primitives c) threat) (:eat-gap (:args c))))
    (let [r (await (ctx/call-child c :eat 'jobs.survival.eat {:until 20 :max-bites 1}))]
      (when (or (= :declined r) (:reason (ctx/child-result c :eat)))
        (ctx/update-mem! c assoc :ate true)))))

(def door-reach
  "Farthest (blocks, feet to the cell's middle) an open door may be for the flight to shut it: within a hand's reach."
  4)

(defn door-key [{:keys [x y z]}] [x y z])

(defn lower-half
  "The cell of a door's lower half (a gate's or trapdoor's own cell) for an openable block b at cell."
  [b cell]
  (if (= "upper" (:half (click/props-of b))) (update cell :y dec) cell))

(defn door-to-shut
  "The nearest door, gate or trapdoor a hand shuts that stands open within door-reach of the body, is nearer the
  threat at threat-pos than the body is (it lies between them, or beyond the body towards the threat), is not the
  one the body stands in, and has not been clicked this flight (:doors-clicked): {:cell :name}, or nil."
  [c threat-pos]
  (let [p (:primitives c)
        self (u/self-pos c)
        {fx :x fy :y fz :z} (sh/feet p)
        r (js/Math.ceil door-reach)
        clicked (:doors-clicked (ctx/mem c) #{})
        mid retreat-home/mid]
    (->> (for [x (range (- fx r) (+ fx r 1)) y (range (dec fy) (+ fy 3)) z (range (- fz r) (+ fz r 1))]
           {:x x :y y :z z})
         (keep (fn [cell]
                 (when-let [b (dig-cells/open-openable p cell)]
                   (when (= :openable (click/kind-of (.-name b)))
                     (let [low (lower-half b cell)]
                       {:cell low :name (.-name b) :half (:half (click/props-of b))})))))
         distinct
         (remove #(clicked (door-key (:cell %))))
         (remove #(click/standing-in? self (:cell %) (:half %)))
         (filter #(<= (u/dist self (mid (:cell %))) door-reach))
         (filter #(< (u/dist threat-pos (mid (:cell %))) (u/dist threat-pos self)))
         (sort-by #(u/dist self (mid (:cell %))))
         first)))

(defn ^:async shut-door!
  "Shut door (door-to-shut) with one click; every door is clicked at most once a flight, so a door that will not stay
  shut does not hold the flight. :again: the next step sees whether the danger is still there."
  [c {:keys [cell name]}]
  (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell))
  (let [r (await (dig-cells/shut-open! c cell))]
    (when (= :shut r)
      (ctx/emit! c :retreat.door-shut :info {:cell (door-key cell) :text (str "shut the " name " at " (door-key cell)
                                                                             " on the hostile")})))
  :again)

(defn ^:async wait-far!
  "Cornered with every chaser beyond :radius: hold still a moment (a :wait, why cornered) and look again."
  [c]
  (await (ctx/act c :wait #js {:ms (flight/wait-ms) :why "cornered"}))
  :again)

(defn ^:async run-home!
  "A step to room (home or another closed building) to the room cell inside its door way (retreat-home/way-in), said
  once a flight (retreat.home): :again; a step that gets no nearer drops that room for the rest of the flight."
  [c room way]
  (retreat-home/say-once! c room)
  (when-not (await (step! c :home (:inside way) 0)) (retreat-home/failed! c room))
  :again)

(defn home-door
  "The door of the home room (jobs.lib.reach/room) that stands open and has not been clicked this flight, or nil."
  [c room]
  (let [clicked (:doors-clicked (ctx/mem c) #{})]
    (->> (:doors room)
         (filter #(dig-cells/open-openable (:primitives c) (:cell %)))
         (remove #(clicked (door-key (:cell %))))
         (sort-by #(u/dist (u/self-pos c) (retreat-home/mid (:cell %))))
         first)))

(defn ^:async shut-home-door!
  "Shut an open door of the home room the body is in: from where it stands within door-reach, else a step to the room
  cell inside it first (a step that gets no nearer gives the door up for the flight). :again."
  [c {:keys [cell inside]}]
  (let [b (dig-cells/open-openable (:primitives c) cell)]
    (if (<= (u/dist (u/self-pos c) (retreat-home/mid cell)) door-reach)
      (await (shut-door! c {:cell cell :name (.-name b)}))
      (do (when-not (await (step! c :home inside 0))
            (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell)))
          :again))))

(defn say-target!
  "Log (retreat.target) the cell a flee step walks to, the chasers it runs from and whether it is out of every ranged one's line of fire."
  [c target threats covered?]
  (let [cell (mapv #(js/Math.floor %) [(:x target) (:y target) (:z target)])
        from (mapv #(str (.-name %) " " (.-id %)) threats)]
    (ctx/emit! c :retreat.target :info {:target cell :from from :cover (boolean (and covered? (covered? target)))
                                        :text (str "flee step to " (str/join " " cell) " away from " (str/join ", " from))})))

(defn ^:async flight-step!
  "One step of the flight (flight/look!, then shut a door, or run home, or walk a step away, or the cornered options),
  :again; the flight's end (flight/end-flight!) once no chaser is left."
  [c]
  (let [{:keys [no-gain-steps]} (:args c)
        p (:primitives c)
        threats (flight/look! c)
        threat (first threats)
        hit? (hit-on-the-run! c)
        door (when threat (door-to-shut c (danger-q/mob-pos p threat)))
        covered? (cover-fn p threats)
        rooms (when threat (refuges c))
        {:keys [room way inside?]} (when threat (retreat-home/choose p rooms threats (not (exposed-low? c covered?))))
        stuck (fn ^:async stuck [why]
                (if (seq (near-hostiles c))
                  (await (cornered! c why inside?))
                  (do (ctx/update-mem! c update :since-gain (fnil inc 0))
                      (if (flight/no-gain? (ctx/mem c) no-gain-steps)
                        (flight/end-flight! c :keeps-off)
                        (await (wait-far! c))))))]
    (cond
      (nil? threat) (or (await (flight/look-back! c)) (flight/end-flight! c nil))
      (flight/no-gain? (ctx/mem c) no-gain-steps)
      (do (when (seq (near-hostiles c)) (ctx/update-mem! c assoc :since-gain 0))
          (await (stuck "the chaser keeps up")))
      (angry-enderman? c (near-hostiles c)) (await (cornered! c "an enderman cannot be outrun" inside?))
      hit? (await (cornered! c "a hostile keeps hitting the body as it flees" inside?))
      (some combat/flyer? (near-hostiles c)) (await (cornered! c "a phantom cannot be outrun" inside?))
      door (await (shut-door! c door))
      (and inside? (home-door c room)) (await (shut-home-door! c (home-door c room)))
      inside? (await (stuck "at home, the hostile still has a way in"))
      way (await (run-home! c room way))
      :else
      (let [_ (await (eat-on-the-run! c threat))
            leg (goal-leg c)
            gap0 (flight/nearest-gap p threats)
            goal-gap0 (when leg (u/dist (u/self-pos c) leg))
            target (or leg (await (flight-target! c threats covered? rooms)))
            cover (cond
                    (and (exposed-low? c covered?)
                         (or (refuge/sealing-here? c) (not (and target (covered? target))))
                         (not (close-danger? p threats)))
                    (await (take-cover! c))
                    (and target (not (way-up c threats)) (sh/buried? p) (not (close-danger? p threats)))
                    (await (dig-pocket! c)))]
        (cond
          (some? cover) cover
          (nil? target) (await (stuck "no open way away from the hostile"))
          :else
          (let [_ (when (:cornered (ctx/mem c)) (ctx/update-mem! c dissoc :cornered))
                _ (say-target! c target threats covered?)
                moved (await (step! c :step target 1))]
            (cond
              (and leg (not (and moved
                                 (>= (or (flight/nearest-gap p threats) js/Infinity) (dec (or gap0 0)))
                                 (< (u/dist (u/self-pos c) leg) (- goal-gap0 0.5)))))
              (do (ctx/update-mem! c assoc :goal-off true)
                  (ctx/emit! c :retreat.goal-off :info {:text "the walk to the goal lost ground on the hostiles or did not near the goal: away from them instead"})
                  :again)
              (not moved) (await (stuck "the way away from the hostile is blocked"))
              :else
              (let [gap (flight/nearest-gap p threats)]
                (ctx/update-mem! c #(-> % (dissoc :tried) (flight/note-gap gap)))
                :again))))))))

(defn ^:async round
  "One whole flight: steps (flight-step!, or the refuge's) until it ends."
  [c]
  (ctx/update-mem! c flight/resume-flight (ctx/now c))
  (when-not (:flight-start (ctx/mem c)) (ctx/update-mem! c assoc :flight-start (ctx/now c) :flight-from (u/self-pos c) :flight-health (.-health (.self (:primitives c)))))
  (loop []
    (ctx/update-mem! c assoc :last-step (ctx/now c))
    (let [r (await (if (:refuge (ctx/mem c)) (refuge/refuge-round! c) (flight-step! c)))]
      (if (= :again r)
        (do (await (pace/pace!)) (recur))
        r))))
