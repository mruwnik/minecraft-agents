(ns jobs.survival.retreat
  (:require [engine.args :as a]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.click :as click]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.look :as look]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]
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
  2. Walks a short step (:step blocks) away from all chasers (nearer ones weigh more) with a go-to child (:escalate false),
     leaning toward the latest :bed or :home when it is within :home-range and not through the hostiles, avoiding :hazard positions.
     When a wall blocks the way away it turns up to 120 degrees towards open ground (at least 2 clear cells).
     It first looks away and to either side where it has not seen the ground (at most 3 looks); a cell still unseen
     reads as rock.
     Eats one bite a step (up to food 20) when the nearest chaser is at least :eat-gap blocks away and food is carried.
  3. Cornered (no open direction worth a walk, or the walk is blocked; checked afresh every step) with no hostile within
     :radius: holds a second (wait, why cornered) and looks again; after :no-gain-steps such steps or holds with no gain the flight ends :keeps-off. With one within :radius: takes the safest option it has not yet failed.
     - fight (jobs.survival.fight-back) only when jobs.lib.cost/fight-damage leaves :reserve health. Never against a creeper.
     - seal in: fill the open sides at feet and head height and the roof (dig-in's 1x1 cells) with carried :blocks,
       at most :max-places a step. It first steps to the middle of its cell, and does not place while a hostile's hitbox
       overlaps a cell to fill. An open door is shut, not filled. A cell that answers occupied (torch, chest, bed) is left alone.
       When only such cells stay open the seal has failed.
     - pillar up 3 (jobs.access.pillar, the carried block with the most, at least 3). Needs a solid floor and free cells above.
       Not against a ranged mob. Every block goes to the scaffold ledger (purpose :pillar) for jobs.access.cleanup.
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
     Order: fight if it wins, then seal, pillar (not against a ranged mob), back off, pit, pocket, fight.
     Against a creeper back off comes first.
     An option that fails is not tried again until all have failed. Then, after a second's hold, all are tried again
     (one retreat_blocked warning per flight).
  Sealed in, up a pillar or down a pit it hides (one retreat_sealed warning; a declared hold, wait why hiding) while a
  hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius) would have a walkable way to
  the refuge if its own blocks were gone, and for :quiet-s more after the last such danger (a silent mob is forgotten
  after a few seconds); then the flight ends :hidden.
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
   :step {:doc "blocks per walk" :spec (a/num-in 1 nil) :default 6}
   :home-range {:doc "a flight leans towards the latest :bed or :home only when it lies within this many blocks" :spec (a/num-in 0 nil) :default 64}
   :weapons {:doc "item name substrings that count as weapons, for a cornered fight" :spec (a/coll-of (a/or-of string? keyword?)) :default combat/default-weapons}
   :reserve {:doc "health a cornered fight must be expected to leave" :spec (a/num-in 0 nil) :default cost/default-reserve}
   :blocks {:doc "names of the blocks a cornered body may seal itself in with" :spec (a/coll-of (a/or-of string? keyword?)) :default lb/building-blocks}
   :max-places {:doc "seal placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pocket or pit is dropped and the lava left" :spec #{:seal :stop} :default :seal}
   :lost-s {:doc "a mob out of line of sight this many seconds has stopped chasing" :spec (a/num-in 0 nil) :default 4}
   :quiet-s {:doc "a hidden body keeps its refuge this many seconds after the last danger" :spec (a/num-in 0 nil) :default 30}
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
  for a creeper), :creeper? one is near, :ranged? the threat shoots}."
  [{:keys [win? creeper? ranged?]}]
  (cond
    win? [:fight]
    creeper? [:back-off :seal :pillar :pit :pocket :fight]
    :else (into (if ranged? [:seal] [:seal :pillar]) [:back-off :pit :pocket :fight])))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (flight/near-known (:primitives c) (set (flight/dead-ids c)) radius ranged-radius)))

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
  (let [r (await (ctx/call-child c slot 'jobs.movement.go-to {:pos target :range range :escalate false
                                                              :warn false :leg-s flight-timeout-s
                                                              :retry false :look-round false}))
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

(defn ^:async blocked!
  "Every option failed: forget them all, warn once a flight, hold a moment (wait, why cornered) and go on: the next step
  tries them again from the start (the danger still stands). The second sweep in a row ends the flight :cannot-escape."
  [c why]
  (ctx/update-mem! c #(-> % (dissoc :tried) flight/count-sweep))
  (when-not (:blocked-warned (ctx/mem c))
    (ctx/update-mem! c assoc :blocked-warned true)
    (ctx/emit! c :retreat_blocked :warn {:text (str why "; every escape failed, trying them again")}))
  (await (ctx/act c :wait #js {:ms flight/wait-ms :why "cornered"}))
  (if (flight/cannot-escape? (ctx/mem c)) (flight/end-flight! c :cannot-escape) :again))

(defn ^:async cornered!
  "Nowhere worth walking to: the first option of escape-order that it can take and has not failed since the last
  start over (blocked!)."
  [c why]
  (let [{:keys [weapons]} (:args c)
        hostiles (near-hostiles c)
        threat (first hostiles)
        order (escape-order {:win? (fight-wins? c hostiles)
                             :creeper? (boolean (some combat/creeper? hostiles))
                             :ranged? (boolean (and threat (combat/ranged? threat)))})]
    (loop [[option & more] order]
      (let [r (when-not (flight/tried? c option)
                (case option
                  :fight (await (fight! c (if (= [:fight] order) weapons (into (vec weapons) tool-weapons))))
                  :seal (await (refuge/hide! c))
                  :pillar (await (refuge/pillar! c))
                  :back-off (when threat (await (back-off! c threat)))
                  :pit (await (refuge/pit! c))
                  :pocket (await (refuge/pocket! c))))]
        (cond
          (some? r) r
          (seq more) (recur more)
          :else (await (blocked! c why)))))))

(def glance-turns
  "Degrees from the way away a flight turns to look before it chooses: the view cone (about 100 degrees wide) then
  takes in every turn walk/choose-target tries."
  [0 90 -90])

(def glance-reach "Blocks along a direction to the feet cell a glance looks at." 3)

(def glance-memory "Most glanced cells a flight remembers." 24)

(defn unseen-before-wall?
  "Whether a cell the body has not sensed lies in the columns along d from the first, up to glance-reach or the first
  column whose seen feet or head cell blocks the way (the cells behind it cannot change the walk)."
  [p from d y]
  (loop [k 1]
    (when (<= k glance-reach)
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
              [gx gz] (walk/column-along from d glance-reach)
              cell [gx y gz]]
          (when (and (not (some #{cell} (:glanced (ctx/mem c)))) (unseen-before-wall? p from d y))
            (ctx/update-mem! c update :glanced #(vec (take-last glance-memory (conj (vec %) cell))))
            (await (dig-look/look-at! c cell))
            (look/see! c))
          (recur more))))))

(defn ^:async flight-target!
  "The walk target away from threats (walk/choose-target over what the body sees, after glance-away!), nil when none."
  [c threats]
  (let [p (:primitives c)
        from (u/self-pos c)
        threat-pos (mapv #(danger-q/mob-pos p %) threats)
        home (home-pos c)]
    (await (glance-away! c (walk/direction from threat-pos home)))
    (walk/choose-target (block-at-fn p) from threat-pos home (keep (comp :pos :data) (ctx/entries c :hazard))
                        (:step (:args c)))))

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
        mid (fn [{:keys [x y z]}] {:x (+ x 0.5) :y y :z (+ z 0.5)})]
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
  (let [r (await (click/click! c cell :closed name))]
    (when (= :changed (:outcome r))
      (ctx/emit! c :retreat.door-shut :info {:cell (door-key cell) :text (str "shut the " name " at " (door-key cell)
                                                                             " on the hostile")})))
  :again)

(defn ^:async wait-far!
  "Cornered with every chaser beyond :radius: hold still a moment (a :wait, why cornered) and look again."
  [c]
  (await (ctx/act c :wait #js {:ms flight/wait-ms :why "cornered"}))
  :again)

(defn ^:async flight-step!
  "One step of the flight (flight/look!, then shut a door, or walk a step away, or the cornered options), :again; the flight's
  end (flight/end-flight!) once no chaser is left."
  [c]
  (let [{:keys [no-gain-steps]} (:args c)
        p (:primitives c)
        threats (flight/look! c)
        threat (first threats)
        door (when threat (door-to-shut c (danger-q/mob-pos p threat)))
        stuck (fn ^:async stuck [why]
                (if (seq (near-hostiles c))
                  (await (cornered! c why))
                  (do (ctx/update-mem! c update :since-gain (fnil inc 0))
                      (if (flight/no-gain? (ctx/mem c) no-gain-steps)
                        (flight/end-flight! c :keeps-off)
                        (await (wait-far! c))))))]
    (cond
      (nil? threat) (flight/end-flight! c nil)
      (flight/no-gain? (ctx/mem c) no-gain-steps)
      (do (when (seq (near-hostiles c)) (ctx/update-mem! c assoc :since-gain 0))
          (await (stuck "the chaser keeps up")))
      door (await (shut-door! c door))
      :else
      (let [_ (await (eat-on-the-run! c threat))
            target (await (flight-target! c threats))]
        (if (nil? target)
          (await (stuck "no open way away from the hostile"))
          (let [_ (when (:cornered (ctx/mem c)) (ctx/update-mem! c dissoc :cornered))
                moved (await (step! c :step target 1))]
            (if-not moved
              (await (stuck "the way away from the hostile is blocked"))
              (let [gap (flight/nearest-gap p threats)]
                (ctx/update-mem! c #(-> % (dissoc :tried) (flight/note-gap gap)))
                :again))))))))

(defn ^:async round
  "One whole flight: steps (flight-step!, or the refuge's) until it ends."
  [c]
  (ctx/update-mem! c flight/resume-flight (ctx/now c))
  (when-not (:flight-start (ctx/mem c)) (ctx/update-mem! c assoc :flight-start (ctx/now c)))
  (loop []
    (ctx/update-mem! c assoc :last-step (ctx/now c))
    (let [r (await (if (:refuge (ctx/mem c)) (refuge/refuge-round! c) (flight-step! c)))]
      (if (= :again r)
        (do (await (pace/pace!)) (recur))
        r))))
