(ns jobs.survival.retreat
  (:require [engine.access.ledger :as ledger]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.combat :as combat]
            [engine.jobs.reach :as reach]
            [engine.jobs.shelter :as sh]
            [engine.jobs.tidy :as tidy]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [jobs.access.pillar :as pillar]
            [jobs.survival.dig-in :as dig-in]))

(def doc
  "Walk a short step away from the nearest hostile each round with the engine
  walker (engine.path.near/walk-near!, doors opened and shut behind), leaning
  towards the latest :bed or :home when that is not through the hostile,
  avoiding :hazard positions and turning towards open ground when a wall is
  behind. A direction is probed column by column from the body's own cell,
  stepping one block up or down where a walker would, so a stair dug behind
  the body is a way back. A flight starts for a hostile within :radius (ranged
  ones within :ranged-radius) and keeps going while one is within
  :clear-radius, so a chasing mob does not catch up between steps.
  Cornered (no open direction worth a walk, or the walk is blocked) it takes the
  safest option it has (escape-order), skipping any that failed this flight:
  - fight (jobs.survival.fight-back, best weapon) only when the odds say it wins:
    engine.jobs.combat/fight-damage leaves :reserve health, and never a creeper;
  - seal in: fill the open sides at feet and head height and the roof (dig-in's 1x1
    cells) with carried :blocks, first stepping to the middle of its cell when its
    hitbox reaches into a cell to fill, at most :max-places a round, one
    retreat_sealed warn, then wait there until the flight is over or for
    :max-hide-ms; not while a hostile's hitbox overlaps a cell to fill;
  - pillar up 3 (jobs.access.pillar as a child, the carried block with the most,
    3 or more): solid floor, the cells above free, not against a ranged mob;
    every block goes to the scaffold ledger (purpose :pillar), so
    jobs.access.cleanup takes it back once the job has ended;
  - back off: a step of up to 2 cells in any of the 8 directions that gains at
    least a block on the hostile;
  - dig down and plug: dig-in's pit (2 deep under a cell with a solid side, else
    3), every cell solid, harvestable with what is carried, no fluid beside, solid
    under it, then a carried or dug block placed over the head (ledger purpose
    :retreat-plug);
  - last of all fight with the best weapon or tool (pickaxe, shovel, hoe) or the
    fist, kept while the hostile stays within :radius.
  The order is fight-if-it-wins, then seal, pillar (not against a ranged mob),
  back off, pit, fight; against a creeper back off comes first. Up on a pillar or
  down a pit it waits while any hostile is within :radius (no time limit), done
  :cooldown-ms after the last. Only a fight that cannot reach the hostile counts
  a failed round, retreat_blocked after three. A cell in another's zone is used
  as a last resort (retreat.trespass-last-resort). Once per flight, with at
  least :eat-gap blocks to the nearest hostile and food carried, it eats
  (jobs.survival.eat up to 20) so health can regenerate on the run. Done when
  no real danger (engine.jobs.reach: one with no walkable way to the body, or a
  ranged one with no line of fire, is none) has been within :clear-radius for
  :cooldown-ms. Each round looks for the nearest real danger only, so a mob far
  off costs no search while one is close.")

(def args
  {:radius {:doc "hostiles within this many blocks start a flight" :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks start a flight" :default 16}
   :clear-radius {:doc "the flight goes on while a hostile is within this many blocks (zombies track to 35)" :default 40}
   :eat-gap {:doc "with at least this many blocks to the nearest hostile, eat once per flight" :default 12}
   :step {:doc "blocks per walk" :default 6}
   :cooldown-ms {:doc "done once no hostile was in the clear radius for this long" :default 5000}
   :weapons {:doc "item name substrings that count as weapons, for a cornered fight" :default combat/default-weapons}
   :reserve {:doc "health a cornered fight must be expected to leave" :default 4}
   :blocks {:doc "names of the blocks a cornered body may seal itself in with" :default dig-in/building-blocks}
   :max-places {:doc "seal placements per round" :default 4}
   :max-hide-ms {:doc "a sealed body waits at most this long before the flight ends" :default 60000}})

(def tool-weapons
  "Item name substrings a cornered body with no weapon and no seal swings: any of them beats the fist."
  ["_pickaxe" "_shovel" "_hoe"])

(def hazard-clearance 2.5)

(def flight-timeout-s
  "Bound of one step of the flight: the mob moves, so the next round aims again."
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
  "The preferred unit [ux uz] from from: directly away from threat (+x when
  they coincide), blended with the direction of home when that lies on the
  away side (not through the threat)."
  [from threat home]
  (let [away (or (unit (- (:x from) (:x threat)) (- (:z from) (:z threat))) [1 0])
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

(defn walk-cells
  "The feet cells {:x :y :z}, one per block along [ux uz] from from, up to n,
  that a walker passes before the first column it cannot enter, per block-at
  (a cell -> block name or nil). A column met twice (a diagonal) repeats its cell."
  [block-at from dir n]
  (loop [k 1
         prev [(js/Math.floor (:x from)) (js/Math.floor (:z from))]
         y (js/Math.floor (:y from))
         out []]
    (let [col (column-along from dir k)
          ny (when (<= k n) (if (= col prev) y (next-y block-at prev col y)))]
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
  [from threat {:keys [open end]}]
  (and (>= open min-open)
       (let [now (u/dist from threat)
             then (u/dist end threat)]
         (and (>= then now)
              (or (> open min-open) (>= (- then now) 2))))))

(defn choose-target
  "The walk target, a feet cell: the end of the first direction, turning away
  from the preferred one as needed, that avoids every hazard and is open for a
  full step; else the most open one that is still worth walking (see
  worth?). nil when cornered."
  [block-at from threat home hazards step]
  (let [dir (direction from threat home)
        options (->> turns
                     (map #(rotate dir %))
                     (map (fn [d] (let [cells (walk-cells block-at from d step)]
                                    {:open (count cells) :dir d :end (peek cells)})))
                     (remove #(near-hazard? hazards from (point-along from (:dir %) step)))
                     (filter #(worth? from threat %)))
        pick (or (first (filter #(>= (:open %) step) options))
                 (last (sort-by :open options)))]
    (:end pick)))

(defn home-pos
  "The position of the latest :bed or :home entry, or nil."
  [c]
  (->> [(ctx/latest c :bed) (ctx/latest c :home)]
       (remove nil?)
       (sort-by :t >)
       first
       :data
       :pos))

(defn check [_c] true)

(defn block-at-fn [p]
  (fn [pos] (u/block-name p pos)))

(defn dead-ids
  "The ids of hostiles a cornered fight has killed: their corpses may stay listed a while."
  [c]
  (vec (:dead (ctx/mem c))))

(defn ^:async fight!
  "Fight back with the best of weapons (the fist when none is carried) whatever
  the health, kept while the hostile stays close; a fight that cannot reach
  any hostile counts a failed round."
  [c weapons why]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (ctx/update-mem! c assoc :cornered true)
    (let [r (await (ctx/call-child c :cornered 'jobs.survival.fight-back
                                   {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons
                                    :skip (dead-ids c)}))]
      (ctx/update-mem! c update :dead #(into (vec %) (concat (get-in (ctx/mem c) [:children :cornered :killed])
                                                             (:killed (ctx/child-result c :cornered)))))
      (if (= :declined r)
        (u/fail! c :retreat_blocked why)
        :continue))))

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
  (set (mapcat #(hitbox-cells (u/pos-of (.-pos %))) (combat/hostiles p radius))))

(defn tried? [c option] (contains? (:tried (ctx/mem c)) option))

(defn tried! [c option] (ctx/update-mem! c update :tried (fnil conj #{}) option))

(defn ^:async place-seal!
  "Place carried blocks at cells in order. :ok, or :failed at the first
  placement refused or with nothing left to place."
  [c cells]
  (loop [cells cells]
    (let [item (dig-in/pick c (:blocks (:args c)))]
      (cond
        (empty? cells) :ok
        (nil? item) :failed
        :else (let [r (await (ctx/act c :place (clj->js {:pos (first cells) :item item})))]
                (if (#{"placed" "occupied"} (.-status r))
                  (recur (rest cells))
                  :failed))))))

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
  :max-places this round. :sealed when none is left open, :continue while
  more are owed, :failed when a hostile stands in one, none is carried or a
  placement is refused."
  [c]
  (let [{:keys [radius max-places blocks]} (:args c)
        p (:primitives c)
        cells (dig-in/open-cells p (sh/feet p))]
    (cond
      (empty? cells) :sealed
      (some (hostile-cells p radius) cells) :failed
      (nil? (dig-in/pick c blocks)) :failed
      :else
      (do (when (off-centre? p) (await (centre! c)))
          (access/trespass! c "retreat" (some #(access/trespass-refusal (access/rules-input c) :place %) cells))
          (if (= :failed (await (place-seal! c (take max-places cells))))
            (do (tried! c :seal) :failed)
            (if (empty? (dig-in/open-cells p (sh/feet p))) :sealed :continue))))))

(defn ^:async hide!
  "Seal the body in and wait there: :continue while sealing or waiting, :done
  once sealed for :max-hide-ms, nil when it cannot seal."
  [c]
  (let [r (await (seal! c))
        now (ctx/now c)]
    (case r
      :failed nil
      :continue :continue
      :sealed (let [at (:sealed-at (ctx/mem c))]
                (cond
                  (nil? at) (do (ctx/update-mem! c assoc :sealed-at now)
                                (ctx/emit! c :retreat_sealed :warn {:text "cornered: sealed in with blocks until the hostile leaves"
                                                                    :pos (sh/feet (:primitives c))})
                                :continue)
                  (>= (- now at) (:max-hide-ms (:args c))) :done
                  :else :continue)))))

;; ------------------------------------------------------------------ cornered: the safest option

(defn escape-order
  "Pure: the cornered options to try, safest first. {:win? the odds say a fight leaves reserve health (never true
  for a creeper), :creeper? one is near, :ranged? the threat shoots}."
  [{:keys [win? creeper? ranged?]}]
  (cond
    win? [:fight]
    creeper? [:back-off :seal :pillar :pit :fight]
    :else (into (if ranged? [:seal] [:seal :pillar]) [:back-off :pit :fight])))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)
        dead (set (dead-ids c))]
    (remove #(dead (.-id %)) (combat/hostiles (:primitives c) radius {:ranged-radius ranged-radius}))))

(defn fight-wins?
  "Whether fighting hostiles with the best weapon carried is expected to leave :reserve health
  (engine.jobs.combat/fight-damage); never against a creeper."
  [c hostiles]
  (let [p (:primitives c)
        self (.self p)
        {:keys [weapons reserve]} (:args c)]
    (boolean
     (and (seq hostiles)
          (not-any? combat/creeper? hostiles)
          (<= (combat/fight-damage {:weapon (combat/best-weapon p weapons)
                                    :armour (combat/armour-points (.-equipment self))
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
  "Step back from threat (back-off-target): :continue, nil when there is no such cell or the walk is blocked."
  [c threat]
  (when-let [target (back-off-target (block-at-fn (:primitives c)) (u/self-pos c) (u/pos-of (.-pos threat))
                                     (keep (comp :pos :data) (ctx/entries c :hazard)))]
    (if (= :blocked (await (near/walk-near! c target 0 {:timeout-s flight-timeout-s})))
      (do (tried! c :back-off) nil)
      :continue)))

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
  "Note refuge (a map with :kind) in job memory and run its first round."
  [c refuge]
  (ctx/update-mem! c assoc :refuge refuge)
  (await (refuge-round! c)))

(defn ^:async pillar!
  "Start a pillar-height pillar (jobs.access.pillar, ledgered) when it fits: a round's result, else nil."
  [c]
  (let [p (:primitives c)
        block-at (pillar/block-at-of p)
        feet (pillar/feet-cell c)
        item (pillar-item c)]
    (when (and item (pillar-ok? block-at feet))
      (access/trespass! c "retreat" (some #(access/trespass-refusal (access/rules-input c) :place (cell-map (up feet %)))
                                          (range pillar-height)))
      (await (start-refuge! c {:kind :pillar :item item})))))

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
  "Start digging down and plugging (pit-plan) when it can: a round's result, else nil."
  [c]
  (let [feet (sh/feet (:primitives c))]
    (when-let [{:keys [roof target-y] :as plan} (pit-plan c feet)]
      (let [in (access/rules-input c)]
        (access/trespass! c "retreat" (or (some #(access/trespass-refusal in :dig (assoc feet :y %)) (range target-y (:y feet)))
                                          (access/trespass-refusal (assoc in :feet nil) :place roof))))
      (await (start-refuge! c (assoc plan :kind :pit))))))

(defn ^:async cornered!
  "Nowhere worth walking to: the first option of escape-order that it can take and has not failed this flight."
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
                  :fight (await (fight! c (if (= [:fight] order) weapons (into (vec weapons) tool-weapons)) why))
                  :seal (await (hide! c))
                  :pillar (await (pillar! c))
                  :back-off (when threat (await (back-off! c threat)))
                  :pit (await (pit! c))))]
        (cond
          (some? r) r
          (seq more) (recur more)
          :else (await (fight! c (into (vec weapons) tool-weapons) why)))))))

;; ------------------------------------------------------------------ up a pillar or down a pit

(defn ^:async abandon-refuge!
  "The refuge failed: forget it, never try that option again this flight."
  [c]
  (let [kind (:kind (:refuge (ctx/mem c)))]
    (ctx/update-mem! c dissoc :refuge)
    (tried! c kind)
    :continue))

(defn hidden-round
  "Up the pillar or down the pit: wait while a hostile is near, done cooldown-ms after the last."
  [c]
  (let [now (ctx/now c)]
    (cond
      (seq (near-hostiles c)) (do (ctx/update-mem! c assoc :last-seen now) :continue)
      (>= (- now (or (:last-seen (ctx/mem c)) now)) (:cooldown-ms (:args c))) :done
      :else :continue)))

(defn hide-now! [c refuge text]
  (ctx/update-mem! c assoc :refuge (assoc refuge :hidden true))
  (ctx/emit! c :retreat_sealed :warn {:text text :pos (sh/feet (:primitives c))})
  :continue)

(defn ^:async pillar-round!
  "One block of the pillar (the child jobs.access.pillar); hidden once it is at least 2 high."
  [c {:keys [item] :as refuge}]
  (let [r (await (ctx/call-child c :pillar 'jobs.access.pillar {:height pillar-height :item item :ignore-zones? true}))]
    (case r
      :declined (await (abandon-refuge! c))
      :done (if (>= (:built (ctx/child-result c :pillar) 0) 2)
              (hide-now! c refuge "cornered: pillared up out of reach until the hostile leaves")
              (await (abandon-refuge! c)))
      :continue)))

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
        (ledger/remember! c (ledger/reconcile l (pillar/block-at-of p)))
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
      (let [_ (await (tools/equip-for! c (u/block-name p below)))
            r (await (tidy/dig! c below true))]
        (if (= "dug" (.-status r))
          (do (await (dig-in/collect-drops! c blocks (.-drops r))) :continue)
          (await (abandon-refuge! c))))
      :else
      ;; raw moveTo kept: a drop into the body's own pit, as dig-in's descent; the planner has no standable goal there.
      (do (await (ctx/act c :moveTo (clj->js {:pos below :range 0.5})))
          (if (< (:y (sh/feet p)) y) :continue (await (abandon-refuge! c)))))))

(defn ^:async refuge-round!
  "A round in a refuge: building it (pillar or pit), or hidden in it."
  [c]
  (let [{:keys [kind hidden] :as refuge} (:refuge (ctx/mem c))]
    (cond
      hidden (hidden-round c)
      (= :pillar kind) (await (pillar-round! c refuge))
      :else (await (pit-round! c refuge)))))

(defn ^:async eat-on-the-run!
  "Once per flight, with the nearest hostile at least :eat-gap away, eat."
  [c threat]
  (when (and (not (:ate (ctx/mem c)))
             (>= (.-distance threat) (:eat-gap (:args c))))
    (let [r (await (ctx/call-child c :eat 'jobs.survival.eat {:until 20}))]
      (when (not= :declined r) (ctx/update-mem! c assoc :ate true)))))

(defn ^:async flight-round [c]
  (let [{:keys [radius ranged-radius clear-radius step cooldown-ms]} (:args c)
        p (:primitives c)
        now (ctx/now c)
        fleeing? (some? (:last-seen (ctx/mem c)))
        dead (set (dead-ids c))
        threat (reach/nearest-danger p (if fleeing? (max clear-radius radius) radius)
                                     {:ranged-radius (if fleeing? (max clear-radius ranged-radius) ranged-radius)}
                                     {:sight? false :skip dead})]
    (cond
      (and (nil? threat) fleeing? (>= (- now (:last-seen (ctx/mem c))) cooldown-ms)) :done
      (and (nil? threat) fleeing?) :continue
      (nil? threat) (do (ctx/update-mem! c assoc :last-seen now) :continue)
      (and (:cornered (ctx/mem c)) (<= (.-distance threat) radius))
      (do (ctx/update-mem! c assoc :last-seen now)
          (await (cornered! c "cornered")))
      :else
      (let [_ (when (:cornered (ctx/mem c)) (ctx/update-mem! c dissoc :cornered))
            _ (await (eat-on-the-run! c threat))
            from (u/self-pos c)
            target (choose-target (block-at-fn p) from (u/pos-of (.-pos threat)) (home-pos c)
                                  (keep (comp :pos :data) (ctx/entries c :hazard)) step)]
        (ctx/update-mem! c assoc :last-seen now)
        (if (nil? target)
          (await (cornered! c "no open way away from the hostile"))
          (let [r (await (near/walk-near! c target 1 {:timeout-s flight-timeout-s}))]
            (if (= :blocked r)
              (await (cornered! c "the way away from the hostile is blocked"))
              (do (ctx/update-mem! c dissoc :tried) :continue))))))))

(defn ^:async round [c]
  (if (:refuge (ctx/mem c))
    (await (refuge-round! c))
    (await (flight-round c))))
