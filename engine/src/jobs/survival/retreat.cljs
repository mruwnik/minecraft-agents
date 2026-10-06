(ns jobs.survival.retreat
  (:require [jobs.lib.click :as click]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.combat :as combat]
            [jobs.lib.escape :as escape]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as r]
            [jobs.lib.threats :as threats]
            [jobs.access.pillar :as pillar]
            [jobs.survival.dig-in :as dig-in]))

(def doc
  "Flee from the hostiles in range, one whole flight per round: run until no mob chases the body any more.
  A real danger (jobs.lib.reach: a mob with a walkable way to the body, or a ranged one with a line of fire) within
  :radius (ranged ones :ranged-radius) starts a chase, or is seen afresh. A mob stops chasing (vanilla) once it is
  gone (dead, despawned, untracked), beyond its follow range (jobs.lib.threats: zombie 35, most 16), out of line of
  sight for :lost-s, or has no walkable way to the body (nor, ranged, a line of fire).
  Each step, in this order:
  1. A door, gate or trapdoor standing open within a hand's reach and nearer the nearest chaser than the body is shut
     with one click (retreat.door-shut info). Each door is clicked at most once a flight.
  2. Walks a short step (:step blocks) away from all chasers (nearer ones weigh more) with the engine walker (jobs.lib.near/walk-near!),
     leaning toward the latest :bed or :home when it is within :home-range and not through the hostiles, avoiding :hazard positions.
     When a wall blocks the way away it turns up to 120 degrees towards open ground (at least 2 clear cells).
     Eats one bite a step (up to food 20) when the nearest chaser is at least :eat-gap blocks away and food is carried.
  3. Cornered (no open direction worth a walk, or the walk is blocked; checked afresh every step) with no hostile within
     :radius: holds a second (wait, why cornered) and looks again. With one within :radius: takes the safest option it has not yet failed.
     - fight (jobs.survival.fight-back) only when jobs.lib.cost/fight-damage leaves :reserve health. Never against a creeper.
     - seal in: fill the open sides at feet and head height and the roof (dig-in's 1x1 cells) with carried :blocks,
       at most :max-places a step. It first steps to the middle of its cell, and does not place while a hostile's hitbox
       overlaps a cell to fill. An open door is shut, not filled. A cell that answers occupied (torch, chest, bed) is left alone.
       When only such cells stay open the seal has failed.
     - pillar up 3 (jobs.access.pillar, the carried block with the most, at least 3). Needs a solid floor and free cells above.
       Not against a ranged mob. Every block goes to the scaffold ledger (purpose :pillar) for jobs.access.cleanup.
     - back off: a step of up to 2 cells in any of 8 directions that gains at least a block on the hostile.
     - dig down and plug: dig-in's pit, then a carried or dug block over the head (ledger purpose :retreat-plug).
       The pit is 2 deep under a solid side, else 3.
       Every cell must be solid, harvestable with what is carried, with no fluid beside and solid under it.
     - side pocket: with no block to seal with, dig a pocket beside the body (feet and head cell, solid on every other side,
       harvestable with what is carried), step in and seal the way in with the dug blocks (the seal option again).
     - last of all, fight with the best weapon or tool (pickaxe, shovel, hoe) or the fist.
     Order: fight if it wins, then seal, pillar (not against a ranged mob), back off, pit, pocket, fight.
     Against a creeper back off comes first.
     An option that fails is not tried again until all have failed. Then, after a second's hold, all are tried again
     (one retreat_blocked warning per flight).
  Sealed in, up a pillar or down a pit it hides (one retreat_sealed warning; a declared hold, wait why hiding), with no
  time limit, while a hostile within :radius (ranged ones :ranged-radius) would have a walkable way to the refuge if
  its own blocks were gone; then the flight ends :hidden.
  Returns done {:fled [ids] :ended :gone|:far|:lost|:closed|:hidden|:none}, or stopped :still-chased after
  :max-flight-s (the hostile reflex fires again if the danger is still near).
  Memory: one :threat entry per mob fled (jobs.lib.threats); the third from one mob within 5 min warns hostile.chased.
  A cell in another's zone is used only as a last resort (retreat.trespass-last-resort warning).")

(def args
  {:radius {:doc "hostiles within this many blocks start a flight" :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks start a flight" :default 16}
   :eat-gap {:doc "with at least this many blocks to the nearest hostile, eat a bite per flee step" :default 12}
   :step {:doc "blocks per walk" :default 6}
   :home-range {:doc "a flight leans towards the latest :bed or :home only when it lies within this many blocks" :default 64}
   :weapons {:doc "item name substrings that count as weapons, for a cornered fight" :default combat/default-weapons}
   :reserve {:doc "health a cornered fight must be expected to leave" :default 4}
   :blocks {:doc "names of the blocks a cornered body may seal itself in with" :default dig-in/building-blocks}
   :max-places {:doc "seal placements per step" :default 4}
   :lost-s {:doc "a mob out of line of sight this many seconds has stopped chasing" :default 4}
   :max-flight-s {:doc "a flight still chased after this many seconds stops :still-chased" :default 180}})

(def tool-weapons
  "Item name substrings a cornered body with no weapon and no seal swings: any of them beats the fist."
  ["_pickaxe" "_shovel" "_hoe"])

(def hazard-clearance 2.5)

(def wait-ms "One hold of a hidden or cornered body before it looks again." 1000)

(def flight-timeout-s
  "Bound of one step of the flight: the mob moves, so the next step aims again."
  5)

(def turns
  "Angles to try, in degrees from the preferred direction, best first."
  [0 30 -30 60 -60 90 -90 120 -120])

(def min-open
  "Fewest clear cells a direction needs to be worth walking."
  2)

(def passable-names
  #{"air" "cave_air" "void_air" "short_grass" "tall_grass" "grass" "fern" "large_fern" "dead_bush" "snow"
    "water" "dandelion" "poppy" "torch" "sweet_berry_bush" "vine"})

(defn passable?
  "Whether a block name lets the body walk through; an unloaded cell (nil) counts as open."
  [block-name]
  (or (nil? block-name) (contains? passable-names block-name)
      (some #(.endsWith block-name %) ["_sapling" "_flower" "_carpet" "_tulip" "_orchid" "_button" "_pressure_plate"])))

(defn floorless?
  "Whether a block name under a walker's feet is no floor: a loaded passable
  block other than water (which a walker swims on). An unloaded cell (nil) is not."
  [block-name]
  (and (some? block-name) (passable? block-name) (not= "water" block-name)))

(defn unit
  "[ux uz] for the vector (dx dz), or nil when it is zero."
  [dx dz]
  (let [n (js/Math.hypot dx dz)]
    (when (pos? n) [(/ dx n) (/ dz n)])))

(defn direction
  "The preferred unit [ux uz] from from: directly away from the threats (positions), each weighing 1/distance so the
  nearer pushes harder (+x when they cancel out), blended with the direction of home when that lies on the away side
  (not through the threats)."
  [from threats home]
  (let [push (fn [t] (let [d (max 1 (js/Math.hypot (- (:x from) (:x t)) (- (:z from) (:z t))))
                           [ux uz] (or (unit (- (:x from) (:x t)) (- (:z from) (:z t))) [0 0])]
                       [(/ ux d) (/ uz d)]))
        sum (reduce (fn [[ax az] t] (let [[ux uz] (push t)] [(+ ax ux) (+ az uz)])) [0 0] threats)
        away (or (unit (first sum) (second sum)) [1 0])
        to-home (when home (unit (- (:x home) (:x from)) (- (:z home) (:z from))))
        along (when to-home (+ (* (first away) (first to-home)) (* (second away) (second to-home))))]
    (if (and along (pos? along))
      (or (unit (+ (first away) (first to-home)) (+ (second away) (second to-home))) away)
      away)))

(defn rotate [[ux uz] degrees]
  (let [a (* degrees (/ js/Math.PI 180))
        c (js/Math.cos a)
        s (js/Math.sin a)]
    [(- (* ux c) (* uz s)) (+ (* ux s) (* uz c))]))

(defn column-along
  "The [x z] column k blocks along [ux uz] from the centre of from's cell (a
  body at x 58.5 stands in column 58 and probes from 58.5, not 59)."
  [from [ux uz] k]
  [(js/Math.floor (+ (js/Math.floor (:x from)) 0.5 (* ux k)))
   (js/Math.floor (+ (js/Math.floor (:z from)) 0.5 (* uz k)))])

(defn point-along
  "The cell step blocks from from along [ux uz], same height."
  [from dir step]
  (let [[x z] (column-along from dir step)]
    {:x x :y (:y from) :z z}))

(defn near-hazard?
  "Whether the walk from from to target passes within clearance of a hazard
  (checked at the middle and the end)."
  [hazards from target]
  (let [mid {:x (/ (+ (:x from) (:x target)) 2) :y (:y from) :z (/ (+ (:z from) (:z target)) 2)}]
    (boolean (some #(or (< (u/dist % target) hazard-clearance) (< (u/dist % mid) hazard-clearance)) hazards))))

(defn free-at?
  "Whether feet and head cells at feet height y in column x z are passable."
  [block-at x y z]
  (and (passable? (block-at {:x x :y y :z z})) (passable? (block-at {:x x :y (inc y) :z z}))))

(defn next-y
  "The feet height a walker at feet height y in column [px pz] reaches in the
  next column [x z], or nil when it cannot enter it: the same height, one
  lower when the floor there is open and the cell under it is not (a step
  down; a column with no floor, a drop of two or more, is not entered), else one higher when that is free and there is headroom above the
  column it steps from (a step up)."
  [block-at [px pz] [x z] y]
  (cond
    (free-at? block-at x y z)
    (let [below (block-at {:x x :y (dec y) :z z})
          below2 (block-at {:x x :y (- y 2) :z z})]
      (cond
        (and (passable? below) (not (passable? below2))) (dec y)
        (floorless? below) nil
        :else y))
    (and (free-at? block-at x (inc y) z) (passable? (block-at {:x px :y (+ y 2) :z pz}))) (inc y)
    :else nil))

(defn corner-shut?
  "Whether a diagonal step from column [px pz] to [x z] at feet height y passes between two blocked columns: a body
  0.6 wide cannot squeeze through that corner."
  [block-at [px pz] [x z] y]
  (and (not= px x) (not= pz z)
       (not (free-at? block-at px y z))
       (not (free-at? block-at x y pz))))

(defn walk-cells
  "The feet cells {:x :y :z}, one per block along [ux uz] from from, up to n,
  that a walker passes before the first column it cannot enter, per block-at
  (a cell -> block name or nil). A column met twice (a diagonal) repeats its cell;
  a diagonal step between two blocked columns (corner-shut?) is not taken."
  [block-at from dir n]
  (loop [k 1
         prev [(js/Math.floor (:x from)) (js/Math.floor (:z from))]
         y (js/Math.floor (:y from))
         out []]
    (let [col (column-along from dir k)
          ny (when (<= k n)
               (cond
                 (= col prev) y
                 (corner-shut? block-at prev col y) nil
                 :else (next-y block-at prev col y)))]
      (if (nil? ny)
        out
        (recur (inc k) col ny (conj out {:x (first col) :y ny :z (second col)}))))))

(defn open-cells
  "How many blocks along [ux uz] from from, up to n, a walker gets before the
  first obstacle (see walk-cells)."
  [block-at from dir n]
  (count (walk-cells block-at from dir n)))

(defn worth?
  "Whether walking the open blocks along a direction from from, to the cell
  end, is worth it against threat: it must not end closer, and must either be
  a real walk (min-open + 1 blocks) or gain at least 2 blocks of distance. A
  short side step in a dead end is neither, so a body that only has those
  left is cornered."
  [from threats {:keys [open end]}]
  (and (>= open min-open)
       (let [now (apply min (map #(u/dist from %) threats))
             then (apply min (map #(u/dist end %) threats))]
         (and (>= then now)
              (or (> open min-open) (>= (- then now) 2))))))

(defn choose-target
  "The walk target, a feet cell: the end of the first direction, turning away
  from the preferred one as needed, that avoids every hazard and is open for a
  full step; else the most open one that is still worth walking (see
  worth?). nil when cornered."
  [block-at from threats home hazards step]
  (let [dir (direction from threats home)
        options (->> turns
                     (map #(rotate dir %))
                     (map (fn [d] (let [cells (walk-cells block-at from d step)]
                                    {:open (count cells) :dir d :end (peek cells)})))
                     (remove #(near-hazard? hazards from (point-along from (:dir %) step)))
                     (filter #(worth? from threats %)))
        pick (or (first (filter #(>= (:open %) step) options))
                 (last (sort-by :open options)))]
    (:end pick)))

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

(defn block-at-fn [p]
  (fn [pos] (u/block-name p pos)))

(defn dead-ids
  "The ids of hostiles a cornered fight has killed: their corpses may stay listed a while."
  [c]
  (vec (:dead (ctx/mem c))))

(defn tried? [c option] (contains? (:tried (ctx/mem c)) option))

(defn tried! [c option] (ctx/update-mem! c update :tried (fnil conj #{}) option))

(defn ^:async fight!
  "Fight back with the best of weapons (the fist when none is carried) whatever
  the health, kept while the hostile stays close: :again, nil when the fight
  cannot reach any hostile (the option failed this flight)."
  [c weapons]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (ctx/update-mem! c assoc :cornered true)
    (let [r (await (ctx/call-child c :cornered 'jobs.survival.fight-back
                                   {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons
                                    :skip (dead-ids c)}))]
      (ctx/update-mem! c update :dead #(into (vec %) (concat (get-in (ctx/mem c) [:children :cornered :killed])
                                                             (:killed (ctx/child-result c :cornered)))))
      (if (= :declined r)
        (do (tried! c :fight) nil)
        :again))))

(def mob-half-width
  "Half the width of a zombie-sized mob's hitbox."
  0.3)

(defn hitbox-cells
  "The feet and head cells a mob standing at pos overlaps (its hitbox is 0.6 wide, so one straddling a cell
  boundary is in both cells: the server refuses a block there)."
  [{:keys [x y z]}]
  (let [span (fn [v] (distinct [(js/Math.floor (- v mob-half-width)) (js/Math.floor (+ v mob-half-width))]))
        fy (js/Math.floor y)]
    (for [cx (span x) cz (span z) cy [fy (inc fy)]] {:x cx :y cy :z cz})))

(defn hostile-cells
  "The cells the hostiles within radius overlap: no block goes there."
  [p radius]
  (set (mapcat #(hitbox-cells (u/pos-of (.-pos %))) (reach/known-hostiles p radius {}))))

(defn occupied? [c cell] (contains? (:seal-occupied (ctx/mem c)) cell))

(defn ^:async place-seal!
  "Place carried blocks at cells in order. :ok, or :failed at the first
  placement refused or with nothing left to place. A door, gate or trapdoor standing open is shut instead
  (dig-in/shut-open!); a cell the place answers occupied (a torch, a chest, a bed: a block the seal leaves alone), or an
  open iron door, is remembered in :seal-occupied and never tried again this flight."
  [c cells]
  (loop [cells cells]
    (let [item (dig-in/pick c (:blocks (:args c)))
          cell (first cells)]
      (cond
        (empty? cells) :ok
        (dig-in/sealed? (:primitives c) cell) (recur (rest cells))
        (nil? item) :failed
        :else (let [door (await (dig-in/shut-open! c cell))
                    status (when-not door (.-status (await (tidy/place! c cell item true))))]
                (cond
                  (or (= :shut door) (= "placed" status)) (recur (rest cells))
                  (or (= :open door) (= "occupied" status))
                  (do (ctx/update-mem! c update :seal-occupied (fnil conj #{}) cell)
                      (recur (rest cells)))
                  :else :failed))))))

(defn off-centre?
  "Whether the body's hitbox (0.6 wide) reaches out of its cell into a side cell."
  [p]
  (let [{:keys [x z]} (u/self-pos {:primitives p})
        frac #(- % (js/Math.floor %))]
    (boolean (some #(or (< (frac %) 0.3) (> (frac %) 0.7)) [x z]))))

(defn ^:async centre!
  "Step to the middle of the body's own cell, so the side cells around it can take blocks."
  [c]
  (let [{:keys [x y z]} (sh/feet (:primitives c))]
    ;; raw moveTo kept: a step inside the body's own cell, range 0.15; the planner has no goal finer than a cell.
    (await (ctx/act c :moveTo #js {:pos #js {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0.15}))))

(defn ^:async seal!
  "Fill the open cells around the body (dig-in's 1x1: sides at feet and head
  height, a roof support, the roof) with carried :blocks, at most
  :max-places this step. :sealed when none is left open, :again while
  more are owed, :failed when a hostile stands in one, none is carried, a
  placement is refused, or only cells it cannot fill are left open (place-seal!'s :seal-occupied: a torch, chest or bed
  in a side cell, which the seal leaves alone; the option then counts as failed this flight, so a cornered body moves
  on to the next one instead of placing there step after step).
  A door standing open in a side cell is no wall (dig-in/sealed?): it is shut."
  [c]
  (let [{:keys [radius max-places blocks]} (:args c)
        p (:primitives c)
        open (dig-in/open-cells p (sh/feet p))
        cells (remove #(occupied? c %) open)
        unfillable (fn [] (tried! c :seal) :failed)]
    (cond
      (empty? open) :sealed
      (empty? cells) (unfillable)
      (some (hostile-cells p radius) cells) :failed
      (nil? (dig-in/pick c blocks)) :failed
      :else
      (do (when (off-centre? p) (await (centre! c)))
          (access/trespass! c "retreat" (some #(access/trespass-refusal (access/rules-input c) :place %) cells))
          (ctx/update-mem! c update :seal-cells (fnil into #{}) (map (juxt :x :y :z) (take max-places cells)))
          (if (= :failed (await (place-seal! c (take max-places cells))))
            (do (tried! c :seal) :failed)
            (let [left (dig-in/open-cells p (sh/feet p))]
              (cond
                (empty? left) :sealed
                (every? #(occupied? c %) left) (unfillable)
                :else :again)))))))

(declare hide-now!)

(defn ^:async hide!
  "Seal the body in and hide there (a :seal refuge): :again while sealing or once sealed, nil when it cannot seal."
  [c]
  (case (await (seal! c))
    :failed nil
    :again :again
    :sealed (let [feet (sh/feet (:primitives c))]
              (hide-now! c {:kind :seal :anchor feet :cells (vec (:seal-cells (ctx/mem c)))}
                         "cornered: sealed in with blocks until the hostile leaves"))))

;; ------------------------------------------------------------------ cornered: the safest option

(defn escape-order
  "Pure: the cornered options to try, safest first. {:win? the odds say a fight leaves reserve health (never true
  for a creeper), :creeper? one is near, :ranged? the threat shoots}."
  [{:keys [win? creeper? ranged?]}]
  (cond
    win? [:fight]
    creeper? [:back-off :seal :pillar :pit :pocket :fight]
    :else (into (if ranged? [:seal] [:seal :pillar]) [:back-off :pit :pocket :fight])))

(defn near-known
  "The hostiles p can be said to know within radius (ranged ones within ranged-radius), the dead skipped."
  [p dead radius ranged-radius]
  (remove #(contains? dead (.-id %)) (reach/known-hostiles p radius {:ranged-radius ranged-radius})))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (near-known (:primitives c) (set (dead-ids c)) radius ranged-radius)))

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
                                  :mobs (map (fn [e] {:name (.-name e) :distance (.-distance e) :hits 0}) hostiles)})
              (- (.-health self) reserve))))))

(defn back-off-target
  "A cell to back off to: of the ends of the open cells (up to 2, see walk-cells) in the eight compass directions,
  the farthest from threat, when it gains at least a block on it and passes no hazard; nil when none does."
  [block-at from threat hazards]
  (let [now (u/dist from threat)]
    (->> (range 0 360 45)
         (keep #(peek (walk-cells block-at from (rotate [1 0] %) 2)))
         (remove #(near-hazard? hazards from %))
         (filter #(>= (u/dist % threat) (inc now)))
         (sort-by #(- (u/dist % threat)))
         first)))

(defn ^:async back-off!
  "Step back from threat (back-off-target): :again, nil when there is no such cell or the walk is blocked."
  [c threat]
  (when-let [target (back-off-target (block-at-fn (:primitives c)) (u/self-pos c) (u/pos-of (.-pos threat))
                                     (keep (comp :pos :data) (ctx/entries c :hazard)))]
    (if (= :blocked (await (near/walk-near! c target 0 {:timeout-s flight-timeout-s})))
      (do (tried! c :back-off) nil)
      :again)))

(def pillar-height 3)

(defn pillar-item
  "The carried :blocks block with the most, when it is at least pillar-height; else nil."
  [c]
  (->> (dig-in/carried c (:blocks (:args c)))
       (filter #(>= (:count %) pillar-height))
       (sort-by :count >)
       first
       :name))

(defn up [[x y z] n] [x (+ y n) z])

(defn pillar-ok?
  "Whether a pillar-height pillar fits here: a solid floor, and the head cell and every cell the body rises into free."
  [block-at feet]
  (and (rules/solid-floor? block-at (up feet -1))
       (every? #(pillar/clear? (block-at (up feet %))) (range 1 (+ 2 pillar-height)))))

(defn cell-map [[x y z]] {:x x :y y :z z})

(declare refuge-round!)

(defn ^:async start-refuge!
  "Note refuge (a map with :kind, :anchor the feet cell it starts from, :cells [x y z] the cells it fills) in job
  memory and run its first step."
  [c refuge]
  (ctx/update-mem! c assoc :refuge refuge)
  (await (refuge-round! c)))

(defn ^:async pillar!
  "Start a pillar-height pillar (jobs.access.pillar, ledgered) when it fits: a step's result, else nil."
  [c]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        feet (pillar/feet-cell c)
        item (pillar-item c)]
    (when (and item (pillar-ok? block-at feet))
      (access/trespass! c "retreat" (some #(access/trespass-refusal (access/rules-input c) :place (cell-map (up feet %)))
                                          (range pillar-height)))
      (await (start-refuge! c {:kind :pillar :item item :anchor (cell-map feet)
                               :cells (mapv #(up feet %) (range pillar-height))})))))

(def drop-of
  "The block a dug block drops, where it is another (an unlisted one drops itself)."
  {"grass_block" "dirt" "stone" "cobblestone" "deepslate" "cobbled_deepslate" "podzol" "dirt" "mycelium" "dirt"})

(defn pit-plan
  "{:roof :target-y} for a pit dug down from feet and plugged over the head (dig-in/dig-plan: 2 deep under a cell with
  a solid side, else 3), or nil: every cell to dig solid, no fluid in or beside it, harvestable with what is carried,
  solid under the bottom, and a block to plug with carried or dug."
  [c feet]
  (let [p (:primitives c)
        blocks (:blocks (:args c))
        {:keys [roof depth] :as plan} (dig-in/dig-plan p feet)
        cells (when plan (map #(update feet :y - %) (range 1 (inc depth))))
        names (map #(u/block-name p %) cells)]
    (when (and plan
               (every? #(sh/solid-at? p %) cells)
               (not-any? dig-in/hazards names)
               (not-any? #(dig-in/lateral-fluid p %) cells)
               (every? #(tools/can-harvest? p %) names)
               (sh/solid-at? p (update feet :y - (inc depth)))
               (or (dig-in/pick c blocks) (some (set blocks) (map #(get drop-of % %) names))))
      {:roof roof :target-y (- (:y feet) depth)})))

(defn ^:async pit!
  "Start digging down and plugging (pit-plan) when it can: a step's result, else nil."
  [c]
  (let [feet (sh/feet (:primitives c))]
    (when-let [{:keys [roof target-y] :as plan} (pit-plan c feet)]
      (let [in (access/rules-input c)]
        (access/trespass! c "retreat" (or (some #(access/trespass-refusal in :dig (assoc feet :y %)) (range target-y (:y feet)))
                                          (access/trespass-refusal (assoc in :feet nil) :place roof))))
      (await (start-refuge! c (assoc plan :kind :pit :anchor feet
                                     :cells (mapv #(vector (:x feet) % (:z feet)) (range target-y (inc (:y feet))))))))))

(defn pocket-cells
  "The two cells [feet head] of a side pocket dug from feet towards [dx dz], or nil: both solid, harvestable with what is
  carried, with no hazard or fluid in or beside them, solid all round them except the way in (floor, roof and the three
  far sides), and a dug block that can seal the way in (carried or dropped)."
  [c feet [dx dz]]
  (let [p (:primitives c)
        blocks (:blocks (:args c))
        at (fn [dy] {:x (+ (:x feet) dx) :y (+ (:y feet) dy) :z (+ (:z feet) dz)})
        [lo hi] [(at 0) (at 1)]
        walls (concat [(at -1) (at 2)]
                      (for [c [lo hi] [sx sz] dig-in/sides :when (not= [sx sz] [(- dx) (- dz)])]
                        (assoc c :x (+ (:x c) sx) :z (+ (:z c) sz))))
        names (map #(u/block-name p %) [lo hi])]
    (when (and (every? #(sh/solid-at? p %) (concat [lo hi] walls))
               (not-any? dig-in/hazards names)
               (not-any? #(dig-in/lateral-fluid p %) [lo hi])
               (every? #(tools/can-harvest? p %) names)
               (or (dig-in/pick c blocks) (some (set blocks) (map #(get drop-of % %) names))))
      [lo hi])))

(defn ^:async pocket!
  "Dig a side pocket (feet and head cell beside the body, closed on every other side), collecting the blocks, and step
  into it: :again, so the next step seals the way in with them. nil when no side fits or a dig fails."
  [c]
  (let [p (:primitives c)
        feet (sh/feet p)]
    (when-let [cells (some #(pocket-cells c feet %) dig-in/sides)]
      (let [in (access/rules-input c)]
        (access/trespass! c "retreat" (some #(access/trespass-refusal in :dig %) cells)))
      (tried! c :pocket)
      (loop [[cell & more] cells]
        (if (nil? cell)
          (do (ctx/update-mem! c update :tried disj :seal)
              ;; raw moveTo kept: a step into the body's own pocket, as the pit's drop; the planner has no standable goal there.
              (await (ctx/act c :moveTo (clj->js {:pos (first cells) :range 0.5})))
              :again)
          (let [_ (await (tools/equip-for! c (u/block-name p cell) {:fast true}))
                r (await (tidy/dig! c cell true))]
            (when (= "dug" (.-status r))
              (await (dig-in/collect-drops! c (:blocks (:args c)) (.-drops r)))
              (recur more))))))))

(defn ^:async blocked!
  "Every option failed: forget them all, warn once a flight, hold a moment (wait, why cornered) and go on: the next step
  tries them again from the start (the danger still stands)."
  [c why]
  (ctx/update-mem! c dissoc :tried)
  (when-not (:blocked-warned (ctx/mem c))
    (ctx/update-mem! c assoc :blocked-warned true)
    (ctx/emit! c :retreat_blocked :warn {:text (str why "; every escape failed, trying them again")}))
  (await (ctx/act c :wait #js {:ms wait-ms :why "cornered"}))
  :again)

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
      (let [r (when-not (tried? c option)
                (case option
                  :fight (await (fight! c (if (= [:fight] order) weapons (into (vec weapons) tool-weapons))))
                  :seal (await (hide! c))
                  :pillar (await (pillar! c))
                  :back-off (when threat (await (back-off! c threat)))
                  :pit (await (pit! c))
                  :pocket (await (pocket! c))))]
        (cond
          (some? r) r
          (seq more) (recur more)
          :else (await (blocked! c why)))))))

;; ------------------------------------------------------------------ who still chases

(defn stopped-chasing
  "Pure: nil while a mob still chases, else why it stopped. m is the mob as seen now ({:mob :distance :in-line? :way?};
  :way? false: no walkable way, nor a line of fire for a ranged mob), nil when no longer listed (dead, despawned, out
  of tracking); seen-t when it was last in line. Vanilla: a mob drops its target past its follow range or a while out
  of sight."
  [m seen-t now lost-ms]
  (cond
    (nil? m) :gone
    (> (:distance m) (threats/follow-range (:mob m))) :far
    (and (not (:in-line? m)) (> (- now seen-t) lost-ms)) :lost
    (false? (:way? m)) :closed
    :else nil))

(defn seen-now
  "What stopped-chasing needs of hostile e (a JS entity; visible: a clear line from the eye, whichever way the body
  faces). A way: a walkable way to the body, or for a ranged mob a line of fire (a shut door takes both)."
  [p e]
  {:mob (.-name e) :distance (.-distance e) :in-line? (true? (.-visible e))
   :way? (or (reach/walkable-way? p (u/pos-of (.-pos e)) (u/pos-of (.-pos (.self p))))
             (and (combat/ranged? e) (reach/danger? p e {:sight? false})))})

(defn known-chasers
  "The hostiles the body knows of (seen or heard, or remembered where last sensed) within the longest follow range."
  [p]
  (reach/known-hostiles p threats/max-follow-range {}))

(def resume-gap-ms "A flight cut for longer than this starts its clocks afresh when it resumes." 5000)

(defn resume-flight
  "mem of a flight resumed at now: after a gap past resume-gap-ms since its last step, the flight starts now and every
  chaser counts as just seen (the world is judged afresh); else as it was."
  [mem now]
  (if (or (nil? (:last-step mem)) (<= (- now (:last-step mem)) resume-gap-ms))
    mem
    (cond-> (assoc mem :flight-start now :last-step now)
      (:chasers mem) (update :chasers update-vals #(assoc % :seen-t now)))))

(defn chaser-entry [e now] {:id (.-id e) :uuid (.-uuid e) :mob (.-name e) :pos (u/pos-of (.-pos e)) :seen-t now})

(defn look!
  "Update the flight's chasers (job memory :chasers, by id): the real dangers within :radius (ranged :ranged-radius)
  join or are seen afresh; one that stopped chasing (stopped-chasing) moves to :fled with :ended why. The chasers
  still on, as JS entities, nearest first."
  [c]
  (let [{:keys [radius ranged-radius lost-s]} (:args c)
        p (:primitives c)
        now (ctx/now c)
        dead (set (dead-ids c))
        live (remove #(dead (.-id %)) (known-chasers p))
        listed (into {} (map (juxt #(.-id %) identity)) live)
        joining (remove #(dead (.-id %)) (reach/dangers p radius {:ranged-radius ranged-radius} {:sight? false}))
        chasers (merge (:chasers (ctx/mem c)) (into {} (map (juxt #(.-id %) #(chaser-entry % now))) joining))
        judged (for [[id ch] chasers
                     :let [e (listed id)
                           seen-t (if (and e (true? (.-visible e))) now (:seen-t ch))
                           ch (cond-> (assoc ch :seen-t seen-t) e (assoc :pos (u/pos-of (.-pos e))))]]
                 {:id id :ch ch :e e :why (stopped-chasing (some->> e (seen-now p)) seen-t now (* 1000 lost-s))})
        on (filter #(nil? (:why %)) judged)
        off (remove #(nil? (:why %)) judged)]
    (ctx/update-mem! c #(-> %
                            (assoc :chasers (into {} (map (juxt :id :ch)) on))
                            (update :fled (fnil into []) (map (fn [{:keys [ch why]}] (assoc ch :ended why))) off)))
    (sort-by #(.-distance %) (map :e on))))

(defn end-flight!
  "Write a :threat entry per mob fled (jobs.lib.threats; one per mob, its last way out) and end the flight: done with
  {:fled [ids] :ended}, :ended why the last chaser stopped (or ended, given for the chasers still on: :hidden); ended
  :still-chased stops."
  [c ended]
  (let [{:keys [chasers fled]} (ctx/mem c)
        by-id (merge (into {} (map (juxt :id identity)) fled)
                     (into {} (map (fn [[id ch]] [id (assoc ch :ended ended)])) chasers))
        all (vals by-id)
        why (or ended (:ended (peek (vec fled))) :none)
        ids (mapv :id all)]
    (doseq [t all] (threats/remember! c t))
    (ctx/update-mem! c dissoc :chasers :fled)
    (if (= :still-chased ended)
      (r/stop! c :still-chased (str "still chased after " (:max-flight-s (:args c)) " s of flight") :fled ids)
      (r/finish! c {:fled ids :ended why}))))

;; ------------------------------------------------------------------ up a pillar or down a pit

(defn ^:async abandon-refuge!
  "The refuge failed: forget it, never try that option again this flight."
  [c]
  (let [kind (:kind (:refuge (ctx/mem c)))]
    (ctx/update-mem! c dissoc :refuge)
    (tried! c kind)
    :again))

(defn refuge-danger?
  "Whether a hostile within :radius (ranged ones within :ranged-radius), the dead skipped, would have a walkable way to
  the refuge's anchor cell were the refuge's own cells open (jobs.lib.reach): a danger the refuge keeps off."
  [c {:keys [anchor cells]}]
  (let [p (:primitives c)
        open (set cells)]
    (boolean (some #(reach/walkable-way? p (u/pos-of (.-pos %)) anchor #{} open) (near-hostiles c)))))

(defn ^:async hide-hold!
  "Sealed in, up the pillar or down the pit: hold (a :wait, why hiding) while the refuge keeps a danger off
  (refuge-danger?), then end the flight :hidden. No time limit."
  [c]
  (loop []
    (if (refuge-danger? c (:refuge (ctx/mem c)))
      (do (await (ctx/act c :wait #js {:ms wait-ms :why "hiding"}))
          (await (pace/pace!))
          (recur))
      (end-flight! c :hidden))))

(defn hide-now! [c refuge text]
  (ctx/update-mem! c assoc :refuge (assoc refuge :hidden true))
  (ctx/emit! c :retreat_sealed :warn {:text text :pos (sh/feet (:primitives c))})
  :again)

(defn ^:async pillar-round!
  "One block of the pillar (the child jobs.access.pillar); hidden once it is at least 2 high."
  [c {:keys [item] :as refuge}]
  (let [r (await (ctx/call-child c :pillar 'jobs.access.pillar {:height pillar-height :item item :ignore-zones? true}))]
    (case r
      :declined (await (abandon-refuge! c))
      :done (if (>= (:built (ctx/child-result c :pillar) 0) 2)
              (hide-now! c refuge "cornered: pillared up out of reach until the hostile leaves")
              (await (abandon-refuge! c)))
      :again)))

(defn ^:async plug!
  "Place a carried block over the head at roof, ledgered as :retreat-plug."
  [c {:keys [roof] :as refuge}]
  (let [p (:primitives c)
        item (dig-in/pick c (:blocks (:args c)))
        cell [(:x roof) (:y roof) (:z roof)]]
    (if (nil? item)
      (await (abandon-refuge! c))
      (let [l (ledger/intend (ledger/open-entries (ctx/view c))
                             {:cell cell :item item :before (u/block-name p roof) :job (:id c) :purpose :retreat-plug})
            _ (ledger/remember! c l)
            r (await (tidy/place! c roof item true))]
        (ledger/remember! c (ledger/reconcile l (escape/block-at-of p)))
        (if (= "placed" (.-status r))
          (hide-now! c refuge "cornered: dug down and plugged the hole until the hostile leaves")
          (await (abandon-refuge! c)))))))

(defn ^:async pit-round!
  "One step of the pit: dig the cell under the feet (collecting the blocks it drops), drop into it, or plug."
  [c {:keys [roof target-y] :as refuge}]
  (let [p (:primitives c)
        blocks (:blocks (:args c))
        {:keys [x y z]} (sh/feet p)
        below {:x x :y (dec y) :z z}]
    (cond
      (not (and (= x (:x roof)) (= z (:z roof)))) (await (abandon-refuge! c))
      (<= y target-y) (await (plug! c refuge))
      (sh/solid-at? p below)
      (let [_ (await (tools/equip-for! c (u/block-name p below) {:fast true}))
            r (await (tidy/dig! c below true))]
        (if (= "dug" (.-status r))
          (do (await (dig-in/collect-drops! c blocks (.-drops r))) :again)
          (await (abandon-refuge! c))))
      :else
      ;; raw moveTo kept: a drop into the body's own pit, as dig-in's descent; the planner has no standable goal there.
      (do (await (ctx/act c :moveTo (clj->js {:pos below :range 0.5})))
          (if (< (:y (sh/feet p)) y) :again (await (abandon-refuge! c)))))))

(defn ^:async refuge-round!
  "A step in a refuge: building it (pillar or pit), or hidden in it (to the end of the flight)."
  [c]
  (let [{:keys [kind hidden] :as refuge} (:refuge (ctx/mem c))]
    (cond
      hidden (await (hide-hold! c))
      (= :pillar kind) (await (pillar-round! c refuge))
      :else (await (pit-round! c refuge)))))

(defn ^:async eat-on-the-run!
  "One bite a flee step, with the nearest hostile at least :eat-gap away; none for the rest of the flight once
  nothing is left to eat or a bite fails."
  [c threat]
  (when (and (not (:ate (ctx/mem c)))
             (>= (.-distance threat) (:eat-gap (:args c))))
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
                 (when-let [b (dig-in/open-openable p cell)]
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
  (await (ctx/act c :wait #js {:ms wait-ms :why "cornered"}))
  :again)

(defn ^:async flight-step!
  "One step of the flight (look!, then shut a door, or walk a step away, or the cornered options), :again; the flight's
  end (end-flight!) once no chaser is left or after :max-flight-s."
  [c]
  (let [{:keys [step max-flight-s]} (:args c)
        p (:primitives c)
        threats (look! c)
        threat (first threats)
        door (when threat (door-to-shut c (u/pos-of (.-pos threat))))
        stuck (fn ^:async stuck [why]
                (if (empty? (near-hostiles c)) (await (wait-far! c)) (await (cornered! c why))))]
    (cond
      (nil? threat) (end-flight! c nil)
      (> (- (ctx/now c) (:flight-start (ctx/mem c))) (* 1000 max-flight-s)) (end-flight! c :still-chased)
      door (await (shut-door! c door))
      :else
      (let [_ (await (eat-on-the-run! c threat))
            from (u/self-pos c)
            target (choose-target (block-at-fn p) from (mapv #(u/pos-of (.-pos %)) threats) (home-pos c)
                                  (keep (comp :pos :data) (ctx/entries c :hazard)) step)]
        (if (nil? target)
          (await (stuck "no open way away from the hostile"))
          (let [_ (when (:cornered (ctx/mem c)) (ctx/update-mem! c dissoc :cornered))
                r (await (near/walk-near! c target 1 {:timeout-s flight-timeout-s}))]
            (if (= :blocked r)
              (await (stuck "the way away from the hostile is blocked"))
              (do (ctx/update-mem! c dissoc :tried) :again))))))))

(defn ^:async round
  "One whole flight: steps (flight-step!, or the refuge's) until it ends."
  [c]
  (ctx/update-mem! c resume-flight (ctx/now c))
  (when-not (:flight-start (ctx/mem c)) (ctx/update-mem! c assoc :flight-start (ctx/now c)))
  (loop []
    (ctx/update-mem! c assoc :last-step (ctx/now c))
    (let [r (await (if (:refuge (ctx/mem c)) (refuge-round! c) (flight-step! c)))]
      (if (= :again r)
        (do (await (pace/pace!)) (recur))
        r))))
