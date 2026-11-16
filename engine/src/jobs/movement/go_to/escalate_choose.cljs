(ns jobs.movement.go-to.escalate-choose
  "go-to's choice of a way out of a shut-in body, or in to a walled-in goal: whether a give-up may escalate, and which
  pillar, stair, dig route or walk to the wall to take."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.aim :as aim]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.way :as escape-way]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [jobs.lib.per-body :as per-body]
            [jobs.lib.places :as places]
            [jobs.lib.breath :as breath]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.column-kinds :as kinds]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.seen-sections :as seen-sections]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threat-field :as threat-field]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.threats :as threats]
            [jobs.lib.toll-cells :as tc]
            [jobs.lib.walk-risk :as walk-risk]
            [jobs.movement.go-to.aim :as goal-aim]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.cut :refer [walk-tolls]]
            [jobs.lib.own-builds :refer [may-dig-fn own-cut-fn own-placed-fn]]
            [jobs.lib.stair-memory :as smem]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.route-ways :as route-ways]
            [jobs.movement.go-to.tuning :refer [budget-near max-blocked]]
            [jobs.lib.way-steps :as ways]))

(defn skips
  "The escalation steps this call does not take: those that failed (memory :skipped-steps), the stair headings that failed
  from the feet cell in an earlier call (smem/ruled-headings) and those of the ways it does not allow."
  [c]
  (into (into (:skipped-steps (ctx/mem c) #{}) (ways/barred-steps c)) (smem/ruled-headings c (end/feet-cell c))))

(def escalate-reasons
  "Planner reasons that prove the way needs the world changed. :start-enclosed is the search running out of land
  with no loaded edge reached (a sealed pen, the goal far and unloaded); :one-way is every nearer node lying behind a drop
  (escalating then needs the body shut in, escalate?)."
  #{:exhausted :goal-enclosed :goal-cut-off :start-enclosed :one-way})

(defn walker-fault?
  "Whether a walk result is the walker's fault: stuck, off its plan (not a partial plan's end), a failed steer, or a
  found plan walked to its end no nearer. Once the walks round it are spent it escalates as a planner reason does, for
  a free body too: go-to arrives unless truly impossible."
  [{:keys [status partial]}]
  (boolean (or (#{:stuck :failed :arrived} status) (and (= :off-plan status) (not partial)))))

(defn sprint-only?
  "Whether a walk result is the planner's only way needing a sprint the body lacks the food for (:abilities of kind
  :corner-jump or :gap-sprint): it cannot walk that way, so it digs or pillars out like a shut-in body."
  [result]
  (and (= :abilities (some-> (:reason result) keyword))
       (contains? #{:corner-jump :gap-sprint} (some-> (:kind result) keyword))))

(defn escalate-reason?
  "Whether a walk result's reason proves the way needs the world changed (escalate-reasons), or is the planner's way
  through a door that :doors :never refuses (:abilities of kind :open): such a door is a wall. A gap jump refused for a low ceiling or
  a step up (:gap-low-ceiling, :gap-up), with no block to fill the gap, counts too: a shut-in body drops in and digs out. :goal-unloaded counts too:
  the caller also requires the body shut in (escape/walk-enclosed?), and a shut-in body's search can end at a loaded edge
  beyond its walls (an unseen cell refused) instead of :start-enclosed."
  [result doors]
  (let [reason (some-> (:reason result) keyword)]
    (boolean (or (contains? escalate-reasons reason)
                 (= :goal-unloaded reason)
                 (and (= :abilities reason) (contains? #{:gap-low-ceiling :gap-up} (some-> (:kind result) keyword)))
                 (and (= :abilities reason) (= :never doors) (= :open (some-> (:kind result) keyword)))))))

(defn air-pocket?
  "Whether a give-up with result is a dry body's whose every way is a swim past its air (:air, :air-lethal), or leads no nearer
  a goal in unloaded land (:goal-unloaded: the swim's far end not loaded either), from a pocket it can walk out of
  only through water: it digs or builds out as a player would."
  [c result]
  (let [p (:primitives c)]
    (boolean (and (#{:air :air-lethal :goal-unloaded} (some-> (:reason result) keyword))
                  (not (breath/in-water? p))
                  (reach-room/dry-shut-in? p :ground)))))

(defn dive?
  "Whether a give-up with result is a way that dives (:way-dives) that go-to may climb out of by a stair (c :dive-stair,
  the setting)."
  [c result]
  (boolean (and (:dive-stair c) (ways/way? c :stair) (= :way-dives (some-> (:reason result) keyword)))))

(defn night-leave-bought?
  "Whether the call's worth buys the walk to its goal from here (walk-risk/bought?), leaving the shelter at a dark sky included
  (walk-risk/estimate prices the way out by its light)."
  [c]
  (let [target (goal-aim/target c)
        self (.self (:primitives c))]
    (boolean (and (not (:reason target))
                  (walk-risk/bought? c (walk-risk/estimate c (:pos target)) (.-health self) (.-food self) (ctx/worth c))))))

(defn night-dug-in?
  "Whether the body is shut in its shelter at a night it is dug in for (sh-inside/night-sheltered)."
  [c]
  (let [p (:primitives c)]
    (and (sh-inside/sheltered-in c)
         (= :dug-in (sh-inside/night-sheltered p (:data (ctx/latest c :shelter)))))))

(defn night-refusal?
  "Whether a give-up with result is a way out of the shelter that the dug-in night's walk price refuses (night-leave-bought?):
  the call's worth does not buy it."
  [c result]
  (boolean (and (not (contains? (skips c) :leave-shelter))
                (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                (night-dug-in? c)
                (dig-leave/way-out? (:primitives c) (sh-inside/sheltered-in c))
                (not (night-leave-bought? c)))))

(defn shelter-exit?
  "Whether a give-up with result may leave the body's own shelter instead: the walk proved the way needs the world
  changed and the body is shut in its shelter. While it is :dug-in at night (sh-inside/night-sheltered, the verdict the
  readiness checks wait for light on) only when the call's worth buys the walk's night risk (night-leave-bought?)."
  [c result]
  (boolean (and (not (contains? (skips c) :leave-shelter))
                (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                (sh-inside/sheltered-in c)
                (or (not (night-dug-in? c)) (night-leave-bought? c)))))

(defn fetch-tool?
  "Whether the escalation may fetch a pickaxe (the stair's and a :dig-route dig's ways): the body is free (a shut-in body
  cannot walk to one; by-hand is its fallback) and carries one pickaxe or none, so the last one breaking is no stop."
  ([c] (fetch-tool? (:primitives c) nil (delay (escape/walk-enclosed? c))))
  ([p _ shut?]
   (and (<= (count (filter #(str/ends-with? (str (:name %)) "_pickaxe") (u/inventory p))) 1)
        (not @shut?))))


(defn gap-fill
  "{:step :fill-gap :cell :item} when result (a planner no-path) is a gap jump refused for a low ceiling or a gap jump
  up, a carried pillar block can fill the gap's floor (at the takeoff's height: a step up is then walkable), and no zone
  refuses the place; else nil."
  [c result skip]
  (let [p (:primitives c)
        {:keys [item] carried :count} (escape/pillar-item c)
        cell (when (and (= :abilities (some-> (:reason result) keyword)) (#{:gap-low-ceiling :gap-up} (some-> (:kind result) keyword))
                        (not (skip :fill-gap)) item (:at result) (:from result))
               (escape/gap-floor (assoc (vec (:at result)) 1 (second (:from result))) (:from result)))]
    (when (and cell (or (:ignore-zones? (:args c)) (nil? (tidy/refusal c :place (zipmap [:x :y :z] cell)))))
      {:step :fill-gap :cell cell :item item :count carried})))

(defn drop-edges
  "The floor cells [x y z] a body would drop off by 2 or more (or into a pit with no floor) when it leaves the region it
  can step back into (level, or one up or down) from the first of starts and that the body at feet can step onto once
  filled, nearest to the [x _ z] target first; nil when the region is not closed within the room cells."
  [p starts feet]
  (let [kind-at (reach/seen-lookup p reach-room/body-kind-of)
        level (fn [x y z] (.filter (reach/forward kind-at x y z) (fn [n] (>= (aget n 1) (dec y)))))
        {:keys [cells closed?]} (reach-room/flood-visit level (first starts) reach-room/room-cells)
        open? (fn [x y z] (keyword-identical? :open (kind-at x y z)))
        [tx _ tz] feet
        mine (into #{} (map vec) (:cells (reach-room/flood-visit #(reach/forward kind-at %1 %2 %3) feet reach-room/room-cells)))
        steps-on? (fn [nx y nz] (some (fn [[dx dz dy]] (contains? mine [(+ nx dx) (+ y dy) (+ nz dz)]))
                                      (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] dy [-1 0 1]] [dx dz dy])))]
    (when closed?
      (->> (for [c cells
                 :let [x (aget c 0) y (aget c 1) z (aget c 2)]
                 [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                 :let [nx (+ x dx) nz (+ z dz)]
                 :when (and (open? nx y nz) (open? nx (inc y) nz) (open? nx (dec y) nz) (open? nx (- y 2) nz)
                              (steps-on? nx y nz))]
             [nx (dec y) nz])
           distinct
           (sort-by (fn [[x _ z]] (+ (js/Math.abs (- x tx)) (js/Math.abs (- z tz)))))))))

(defn late-flood
  "The result of the planner's late goal flood (wider than its early one: no sealed test) toward the goal within range of to
  [x y z] over pw before any search, over at most the room-cells drop-edges closes the goal's region in; fill-cells (nil:
  none) its options.fillCells. pw: a pathWorld (wworld/path-world)."
  [c ^js pw to range fill-cells]
  (let [^js s (planner/create-search (.-snapshot pw) (wplan/plan-query c to range)
                                     (doto (wplan/plan-options pw 1 nil nil)
                                       (unchecked-set "preFlood" 0) (unchecked-set "floodAfter" 0) (unchecked-set "maxNodes" 1)
                                       (unchecked-set "goalFlood" reach-room/room-cells)
                                       (unchecked-set "fillCells" fill-cells)))]
    (.step s js/Infinity)
    (.result s)))

(defn shut-result?
  "Whether a planner result proves the goal walled in or cut off by a drop (or the start shut in): the proof behind a
  plan's :goal-enclosed and :goal-cut-off."
  [^js r]
  (boolean (#{"goal-enclosed" "goal-cut-off" "start-enclosed"} (.-reason r))))

(defn joining-fill
  "The fill of fills (floor cells [x y z], nearest first) whose block alone, read solid, leaves goal ({:x :y :z}) no longer
  proven shut by the planner's late goal flood (late-flood, over the call's read: c's :unknown): one flood over the world
  read with every fill solid, each fill's stand labelled (options.fillCells), names the fill on the planner's fewest-move
  way from the goal to the body. A fill beside land the planner's moves never reach the goal from bridges nothing. nil
  when no fill alone frees it; the first of fills when it is not proven shut with none filled, or without a path world."
  [c goal fills]
  (let [pw (wworld/path-world (:primitives c) (:unknown c))
        fills (vec fills)]
    (cond
      (nil? pw) (first fills)
      (empty? fills) nil
      :else (let [stands (js/Map.)
                  _ (doseq [[i [x y z]] (map-indexed vector fills)] (.set stands (planner/cell-key x (inc y) z) (inc i)))
                  r (late-flood c (wworld/with-walls pw (map #(zipmap [:x :y :z] %) fills)) [(:x goal) (:y goal) (:z goal)]
                                (:range (:args c) 1) stands)
                  joined (.-joined r)]
              (cond
                (shut-result? r) nil
                (pos? joined) (fills (dec joined))
                :else (first fills))))))

(defonce ^{:doc "Per body (its primitives): the last drop-fill asked, {:key :unknown :v}."} drop-fills (js/WeakMap.))

(defn drop-fill-of
  "drop-fill's answer, without the kept one."
  [c result skip feet]
  (when (and (contains? #{:goal-cut-off :goal-enclosed} (some-> (:reason result) keyword)) (not (skip :fill-gap)))
    (let [p (:primitives c)
          {:keys [item] carried :count} (escape/pillar-item c)
          goal (some-> (places/parse-pos (:pos (:args c))) :pos)
          kind-at (reach/seen-lookup p reach-room/body-kind-of)
          starts (when goal (filter (fn [[x y z]] (reach/standable-cell? p {:x x :y y :z z} kind-at))
                                    (for [dy [0 1] [dx dz] [[0 0] [1 0] [-1 0] [0 1] [0 -1]]]
                                      [(+ (:x goal) dx) (+ (:y goal) dy) (+ (:z goal) dz)])))
          cell (when (and item (seq starts)) (joining-fill c goal (drop-edges p starts feet)))]
      (when (and cell (or (:ignore-zones? (:args c)) (nil? (tidy/refusal c :place (zipmap [:x :y :z] cell)))))
        {:step :fill-gap :cell cell :item item :count carried}))))

(defn drop-fill
  "{:step :fill-gap :cell :item} when result is :goal-cut-off or :goal-enclosed (the goal's region ends at a drop the body cannot climb back
  from), a carried block can fill the floor of the cell beyond a drop edge that the planner's goal flood then no longer
  proves walled in (joining-fill), and no zone refuses the place: the bridge a player lays over the hole before a door;
  else nil. Kept per body while the world it read (threat-field/world-epoch), the call's read and the question hold (never
  under check-cache/fresh): the escalate? that keeps a give-up and the choice after it ask the same."
  [c result skip feet]
  (let [p (:primitives c)
        epoch (threat-field/world-epoch p)
        u (:unknown c)
        k [epoch feet (some-> (:reason result) keyword) (boolean (skip :fill-gap)) (:pos (:args c)) (:range (:args c))
           (:ignore-zones? (:args c)) (escape/pillar-item c) (some-> u .-missed .-size) (some-> u .-blocked .-size)]
        keep? (and (some? epoch) (not (::check-cache/fresh c)))
        kept (.get drop-fills p)]
    (if (and keep? kept (identical? u (:unknown kept)) (= k (:key kept)))
      (:v kept)
      (let [v (drop-fill-of c result skip feet)]
        (when keep? (.set drop-fills p {:key k :unknown u :v v}))
        v))))

(defn own-stair
  "The body's own stair cut again (climb/recut) as the way out for a body standing on it under goal: what stands in its
  walk space (a cap the body put there) is cleared before any other way is cut, its floors are not. nil once it failed
  this call (memory :own-way-done) and on a climb away from a dive (climb/own-route chose the way there), and nil
  while the stair stands open: cutting it again then gets the body nowhere nearer a goal the walk could not reach."
  [c feet goal]
  (when (and (> (second goal) (second feet)) (ways/way? c :stair) (not-any? (ctx/mem c) [:own-way-done :dive-climb]))
    (when-let [e (climb/recut feet (ledger/ways-of c))]
      (let [[fx fy fz] feet
            [dx dz] (some #(when (= (:heading e) (escape/heading-names %)) %) escape/cardinals)
            kind-at (kinds/seen-kind-at (:primitives c))
            shut? (fn [i] (let [x (+ fx (* i dx)) y (+ fy i) z (+ fz (* i dz))]
                            (some #(keyword-identical? :solid (kind-at x % z)) [y (inc y)])))]
        (when (some shut? (range 1 (inc (:steps e))))
          e)))))

(defn out-of-sight?
  "Whether a give-up with result under :see is a search that ran out of land (:exhausted): no stand it reached sees the
  block, so a stand in range that sees only rock leaves the goal walled in."
  [c result]
  (boolean (and (= :exhausted (some-> (:reason result) keyword)) (goal-aim/see-cell c))))

(defn refused-drop?
  "Whether a give-up with result is a :one-way end whose refused step is a fall or a way down (:descent): the only
  nearer way is one the walk will not take down, so a free body digs to the goal as a player who won't jump does."
  [result]
  (and (= :one-way (some-> (:reason result) keyword))
       (#{:drop :descent} (some-> (:one-way result) :kind keyword))))

(defn damage-refused?
  "Whether a give-up with result is a :damage-budget end: at full health the only whole way costs more hp than the body
  may spend, so any body, free or shut in, digs or climbs to the goal by price instead, as a player who won't jump does."
  [result]
  (= :damage-budget (some-> (:reason result) keyword)))

(defn drop-stranded?
  "Whether a give-up with result is a search that ran out of land (:exhausted), or a walk whose felt-for floors kept
  turning out not there (:unseen-floors), for a body above the goal pos by more than the fall the walk takes (the
  planner's own limit: wworld/damage-budget's hp): every edge of its land is a fall the walk refuses, so as a player on
  a platform with a pickaxe does, it digs a stair down, priced against the other ways like any dig-in."
  [c feet pos result]
  (let [self (.self (:primitives c))
        limit (:max-drop (cost/fall-profile {:equipment (cost/equipment-of (.-equipment self)) :damage-budget (wworld/damage-budget c)}))]
    (boolean (and (#{:exhausted :unseen-floors} (some-> (:reason result) keyword))
                  (> (- (second feet) (js/Math.floor (:y pos))) limit)))))

(defn goal-walled?
  "Whether a give-up with result is a free body's (not escape/walk-enclosed?) whose goal the planner proved walled in or cut off by a drop (a column top), or whose
  search spent its node budget (:budget) with the body within budget-near of the goal (:goal-left): a goal that close
  with no whole way found is walled off, whatever the flood could not prove, or one out-of-sight?. The ways
  that change the world run toward the goal (goal-way). shut?: a delay of escape/walk-enclosed?, when the caller has one."
  ([c result] (goal-walled? c result (delay (escape/walk-enclosed? c))))
  ([c result shut?]
   (let [reason (some-> (:reason result) keyword)]
     (boolean
      (and (or (contains? #{:goal-enclosed :goal-cut-off} reason)
               (and (= :budget reason) (some-> (:goal-left result) (<= (budget-near))))
               (out-of-sight? c result))
           (not @shut?))))))

(defn gap-walled?
  "Whether a give-up with result is one whose only way on is a gap jump the walker refuses (a low ceiling, a
  step up over a hole) that no carried block can fill (gap-fill): the goal is walled off by the gap: a free body
  goes round it (goal-way), a shut-in one digs toward the goal."
  [c result skip]
  (boolean
   (and (= :abilities (some-> (:reason result) keyword))
        (#{:gap-low-ceiling :gap-up} (some-> (:kind result) keyword))
        (nil? (gap-fill c result skip)))))

(defn gap-dig
  "{:step :dig-route :cells [{:cell riser :block}]} for a step up over a one-cell gap (:gap-up of result, the landing :at a step above the
  takeoff :from): the block at the takeoff's height on the landing side, with a solid floor under it, cut so the jump
  is level; nil for any other refusal, or a riser the body may not cut or that is not seen to be solid."
  [c result may-dig? {:keys [own? skip]}]
  (let [{:keys [at from]} result
        block-at (escape/block-at-of (:primitives c))]
    (when (and (= :gap-up (some-> (:kind result) keyword)) (not (skip :dig-route)) at from
               (= (second at) (inc (second from))))
      (let [riser (assoc (vec at) 1 (second from))
            floor (escape/up riser -1)]
        (when (and (= 2 (u/dist (zipmap [:x :y :z] riser) (zipmap [:x :y :z] from)))
                   (escape/solid? (block-at riser))
                   (escape/diggable? block-at may-dig? riser)
                   (escape/solid? (block-at floor)))
          {:step :dig-route :cells [{:cell riser :block (block-at riser)}]})))))

(defn goal-lava?
  "Whether the goal cell [x y z] is refused outright, whatever is dug: it is lava, it stands over lava, or lava lies
  beside it on all four sides (at its level or the floor's). Cells never seen read as not lava."
  [p [x y z]]
  (let [lava? (fn [dx dy dz] (some-> (u/seen-block p {:x (+ x dx) :y (+ y dy) :z (+ z dz)}) .-name (= "lava")))]
    (boolean
     (or (lava? 0 0 0) (lava? 0 -1 0)
         (every? (fn [[dx dz]] (or (lava? dx 0 dz) (lava? dx -1 dz))) [[1 0] [-1 0] [0 1] [0 -1]])))))

(def fetched-pickaxe "The pickaxe a dig fetches when it has none quicker (pickaxe-fetch)." "wooden_pickaxe")

(defn pickaxe-fetch
  "When fetch? (fetch-tool?): a delay of the seconds a wooden pickaxe (fetched-pickaxe) costs to get (need/item-price under the fetch ways:
  its walk there and back with its risk under the sky now, so a long walk at night is dear), asked only when route-s
  digs quicker with one; else nil. The price is kept per body (threat-field/shared) while its world epoch, carried items and
  walk-risk/body-inputs hold: every quote and round of a dig search asks it."
  [c fetch?]
  (when fetch?
    (delay (threat-field/shared c [::pickaxe-fetch (hash (walk-risk/body-inputs c)) (mapv (juxt :name :count) (u/inventory (:primitives c)))]
                                #(need/item-price c fetched-pickaxe 1 {:how need/sources} nil)))))

(defn search-clear-ms
  "(fn [name]) the ms a dig search prices a block's dig at: the quickest tool carried, or with fetch (pickaxe-fetch) the
  fetched-pickaxe too (its fetch priced once a route is found: route-tool)."
  [p fetch]
  (if fetch #(tools/clear-ms p % fetched-pickaxe) #(tools/clear-ms p %)))

(defn route-tool
  "{:s :fetch? :searched}: the seconds digging cells ([x y z]) takes with the quickest tool carried, or with a wooden pickaxe got
  first at @fetch (pickaxe-fetch) when that is quicker (:fetch? true); :searched the digs at search-clear-ms, no fetch paid;
  blocks own? calls the body's own not counted; block-at reads them (default escape/block-at-of). shares (optional) weighs each cell's dig."
  ([p cells own? fetch] (route-tool p (escape/block-at-of p) cells own? fetch (constantly 1)))
  ([p block-at cells own? fetch shares]
   (let [cells (remove #(and own? (own? % (block-at %))) cells)
         sum (fn [extra] (reduce + 0 (map (fn [cell ms] (* (shares cell) (/ ms 1000))) cells (tools/cuts-ms p (map block-at cells) extra))))
         carried (sum nil)
         fetched (when fetch (sum fetched-pickaxe))]
     (if (and fetched (< fetched carried) (< (+ @fetch fetched) carried))
       {:s (+ @fetch fetched) :fetch? true :searched fetched}
       {:s carried :fetch? false :searched (or fetched carried)}))))

(defn route-s
  "The seconds of route-tool."
  ([p cells own? fetch] (:s (route-tool p cells own? fetch)))
  ([p block-at cells own? fetch shares] (:s (route-tool p block-at cells own? fetch shares))))

(defn rest-dig-s
  "Seconds an unfinished dig search's rest is expected to cost (dig-price's :searching or :budget, so a quote does not
  price it as a walk): route-s of the solid feet and head cells on the straight line from cell from into goal, a cell not
  sensed or not loaded as rock (access/hidden-guess) at the measured chance it is solid (look/unseen-solid-at-fn). Past the
  blocks whose two reads fit in dig-route/answer-s, each block is priced as the goal's feet and head unseen, with the tool carried."
  [c from goal]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        rock-at #(or (block-at %) access/hidden-guess)
        solid-at (look/unseen-solid-at-fn p)
        own? (own-placed-fn c)
        share #(cond (or (nil? (block-at %)) (dig-look/unknown? p %)) (solid-at %)
                     (escape/solid? (block-at %)) 1
                     :else 0)
        read (js/Math.floor (/ dig-route/answer-s (* 2 (cost/search-read-ceiling-s))))
        far (- (js/Math.ceil (apply js/Math.hypot (map - goal from))) read)
        pair (fn [[x y z]] [[x y z] [x (inc y) z]])
        cells (filter #(pos? (share %)) (mapcat pair (take read (concat (escape/line-cells from goal) [goal]))))]
    (+ (route-s p rock-at cells own? (pickaxe-fetch c (fetch-tool? c)) share)
       (if (pos? far) (* far (route-s p (constantly access/hidden-guess) (pair goal) nil nil solid-at)) 0))))

(def route-searches
  "The most stands frontier-way tries a route to."
  3)

(defonce ^{:doc "Per go-to call (route-owner): {key search-fn} of the dig-route searches its last escalate! left unfinished
  (jobs.lib.dig-route/search), gone on by later ones (route-slice)."}
  kept-routes (atom {}))

(per-body/store! ::kept-routes #(reset! kept-routes {}))

(defonce ^{:doc "Per go-to call (route-owner): the cells its sliced dig-route searches have expanded in all (sliced-answer),
  the CPU search-bound prices standing still by."}
  searched-cells (atom {}))

(per-body/store! ::searched-cells #(reset! searched-cells {}))

(defn searched-cells-of [id] (get @searched-cells id 0))

(defn route-owner
  "The key of go-to call c's kept searches (kept-routes), made once a call (memory :route-owner): a job id comes again
  in a later engine."
  [c]
  (or (:route-owner (ctx/mem c))
      (let [k (str (:id c) "/" (random-uuid))] (ctx/update-mem! c assoc :route-owner k) k)))

(defn route-slice
  "A slice of route searching for one escalate! of the go-to call id: its searches together expand at most the dig-route
  ::max-nodes setting less the floods the call made (charge!), and searches kept unfinished from the last one (kept-routes) go on."
  [id]
  (let [nodes (settings/get dig-route/settings :jobs.lib.dig-route/max-nodes)]
    {:id id :left (volatile! nodes) :floor (quot nodes 2) :asked (volatile! {})}))

(defn same-question-kept
  "[key kept] of the call id's kept search that asks k's question but for the :bound-s: the chain's time left falls every
  round, and a search begun again at each would never get on (sliced-answer reads an ended one's answer under k's bound)."
  [id k]
  (let [same (fn [k] (update k 3 dissoc :bound-s))]
    (first (for [[k2 kept] (get @kept-routes id) :when (= (same k2) (same k))] [k2 kept]))))

(defn answered-run
  "The kept run of an ended search: answer a, nothing else (a closure made inside sliced-answer's swap! would share its
  context, and with it the search fn)."
  [a]
  (fn [& _] [a 0 (constantly false) 0]))

(defn route-epoch
  "What a dig-route search's answer rests on: the body's memory (sense/known-memory), all it reads; without a perception
  (sensedAt reads the world) the world's change counter."
  [p]
  (if (aget p "perception") (sense/known-memory p) (threat-field/world-epoch p)))

(defn sliced-answer
  "The answer of the dig-route search under key k in slice (start begins it: a dig-route/search), run on with what the
  slice has left under max-s when given (a kept search goes on under the call's bound as it falls); :searching while it
  has not ended. An ended search keeps its answer only (its cell maps are freed), the route-epoch it was last run at and
  the max-s and deepest fall (:max-drop) it ran under: while that epoch holds, its route answers k under k's bound,
  priced again at this call's prices ((reprice route) -> [answer expanded], dig-route/route-within; a bound: at a higher
  health a way of more falls or unseen steps may be cheaper) and searched afresh when no way within its cells is left
  that the body survives (a fall past its deepest drop, a flood it drowns in; a route found so stays), and
  its no route answers a max-s and a :max-drop no larger."
  ([slice k start epoch] (sliced-answer slice k start nil epoch nil))
  ([slice k start max-s epoch] (sliced-answer slice k start max-s epoch nil))
  ([{:keys [id left asked] :as slice} k start max-s epoch {:keys [reprice max-drop] :as body}]
   (if-let [a (get @asked k)]
     a
     (let [[k2 kept] (if-let [kept (get-in @kept-routes [id k])] [k kept] (same-question-kept id k))
           kept (when-not (and (:ended? kept)
                               (or (not= epoch (:epoch kept))
                                   (and (nil? (:dig (:a kept)))
                                        (or (> (or max-s ##Inf) (or (:max-s kept) ##Inf))
                                            (> (or max-drop 0) (or (:max-drop kept) ##Inf))))))
                  kept)
           run (or (:run kept) (start))
           [a used] (if (pos? @left) (run @left max-s) [nil 0])
           ;; a search begun in an earlier call priced its route at that call's body. Its price excludes the CPU the search
           ;; spent (sunk: a fresh search would spend it again). A route found at ##Inf (a flood the body drowns in) is
           ;; priced again too, as health rises; it stays when no finite price comes of it
           repriced? (boolean (and kept reprice (:dig a)))
           was-finite? (and repriced? (js/isFinite (:seconds a)))
           [a used2] (if repriced?
                       (let [[r n] (reprice a)] [(cond (:dig r) (merge a r) was-finite? r :else a) n])
                       [a 0])]
       (vswap! left - used used2)
       (swap! searched-cells update id (fnil + 0) used used2)
       (swap! kept-routes update id dissoc k2)
       (if (and was-finite? (not (and (:dig a) (js/isFinite (:seconds a)))))
         (sliced-answer slice k start max-s epoch body)
         (do
           (swap! kept-routes assoc-in [id k] {:run (if a (answered-run a) run) :a a :epoch epoch
                                                :max-s (if (:ended? kept) (:max-s kept) (or max-s (:max-s kept)))
                                                :max-drop (if kept (:max-drop kept) max-drop)
                                                :ended? (or (some? a) (:ended? kept) false)})
           (let [a (if (and (:dig a) max-s (> (:seconds a) max-s)) {:problem :dearer} a)]
             (vswap! asked assoc k (or a :searching))
             (or a :searching))))))))

(defn flood-cells-per-node
  "Flood cells that cost a slice as much CPU as one search node expanded: cost/search-cell-s over cost/flood-cell-s."
  []
  (/ (cost/search-cell-s) (cost/flood-cell-s)))

(defn charge!
  "Charge slice n floods' cells at per cells a node: its searches get that much less of the call, never under half of it
  (a slice a flood eats up would never let a search end)."
  [{:keys [left floor]} n per]
  (vswap! left (fn [l] (max (min l floor) (- l (/ n per))))))

(defn searching?
  "Whether a search slice asked is still going on."
  [slice]
  (boolean (some #{:searching} (vals @(:asked slice)))))

(defn keep-routes!
  "Keep for the call's next escalate! only the searches slice asked, those that ended too: the choice waits on the
  others, and asks them all again."
  [{:keys [id asked]}]
  (swap! kept-routes update id select-keys (keys @asked)))

(defn routes-searching!
  "Go on with the call id's searches kept unfinished (kept-routes) within one slice, at route-epoch epoch: whether one still
  has not ended. The ones that end stay kept, for the next escalate!'s choice."
  [id epoch]
  (let [slice (route-slice id)]
    (doseq [[k kept] (get @kept-routes id) :when (not (:ended? kept))] (sliced-answer slice k nil epoch))
    (searching? slice)))

(defn searching-ends
  "The [from goal] of the call id's kept searches that have not ended."
  [id]
  (into #{} (keep (fn [[k kept]] (when-not (:ended? kept) (vec (take 2 k))))) (get @kept-routes id)))

(defn forget-routes!
  "Drop the kept searches of go-to call id."
  [id]
  (swap! kept-routes dissoc id))

(defn end-searches!
  "Go-to call id has ended: drop its kept searches and its count of cells searched."
  [id]
  (forget-routes! id)
  (swap! searched-cells dissoc id))

(defn guess-class
  "The escape/guess-class of unseen cell [x y z]: its seen solid face neighbours' names decide. sensed: cell -> the sensed
  block or nil."
  [sensed [x y z]]
  (escape/guess-class y (keep (fn [[dx dy dz]] (let [b (sensed [(+ x dx) (+ y dy) (+ z dz)])]
                                                  (when (and b (not (true? (.-unknown b))) (escape/solid? (.-name b))) (.-name b))))
                              [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])))

(defn route-answer
  "The dig-route answer {:dig :steps} of the cheapest way from feet cell from into goal (jobs.lib.dig-route priced by
  jobs.lib.cost: a step's walk-cost, a fall's hp (fall-damage at hp-cost, the falls the body survives), a block's dig at the quickest tool carried (search-clear-ms; with fetch? a wooden pickaxe's when quicker, its fetch then paid once, route-tool; an unseen one's at the chance it is solid, escape/dig-share-fn), a stair's step at least its stair-cost, a step's unseen cells at the unseen walk's price
  (wworld/unseen-share), what each dig destroys (dig-route/body-prices: cost/destroyed-fn; a cell of the body's own builds at route-opts :own-s, own-builds/own-cut-fn), a step into a cell of route-opts :tolls at its factor (dig-route/toll-fn, in the search and the straight dig), never a floor of the body's own stairs (route-opts :ways)), every cell to dig escape/diggable?
  and may-dig? (a dig beside seen water priced as its flood: body-prices); :searching while route-opts :slice's search goes on; nil when there is none, when route-opts :spent
  holds [from goal] (a search search-bound dropped), or when its digging takes the
  body longer than the least of route-opts :worth-s and :bound-s with what it carries, or with
  a wooden pickaxe got at its price when fetch? (pickaxe-fetch: the dig fetches one, fetch-tool?), else bare-handed (a player does not punch a tunnel
  through rock). A floor never seen under a cell seen open may be a void: dig-route reads it as no floor, unless
  floors-unseen-ok? (a pocket's floor shows only from inside it) or it is the goal's own: read as rock, each walk onto one under a cell seen open
  priced at the chance it is none (cost/floor-absent-chance of the step's kind) times the fall it adds past the step's own (cost/unseen-fall-s less its :fall-s), in the search and the straight dig. Other route-opts go to the search (:place-s,
  :pillar-s (a fn is rebuilt each call: not part of the slice's key), :margin).
  The search ends by price once its cheapest open cell costs more than the least of route-opts :bound-s, :worth-s and the straight dig (each cell's dig seconds, a walked step or the fall a dug floor drops it, and an unseen toll), the back search's bound in the heuristic; each is searched round dig-route/lane first (seeded-search; a climb's pillar part the route-opts :max-places carried), the straight dig held (:held-s: past dig-route/answer-s with no cheaper route in hand it is the route); every search through search-opts (dig-way, frontier-way, replans, climb) also prices its CPU (:read-s a cell read). With :slice (route-slice) a search that spends it goes on at the next call; without, it ends
  at the node budget as no route. With :range (and :near, the cell it is measured from) the route ends on any stand
  within it (with :see, one whose eye sees that block over the rock it guesses, aim/dug-stand-sees-fn) and never digs :near. Each guess on it pays, at the chance it is not diggable (its class's risk, route-opts :guess-risk {class
  chance}: guess-class), a look and a replan round it, in the search; it is none over :worth-s seconds. Its :share-s is its
  :seconds without those misses: a way's price (way-total) reads unseen cells at the chance they are solid only. The body's own blocks (own?) are dug free in the search and the worth checks; the reported :seconds and :share-s add their dig."
  [p from goal may-dig? own? floors-unseen-ok? fetch? {:keys [slice spent] :as route-opts}]
  (let [own-s (:own-s route-opts)
        guess-risk (:guess-risk route-opts)
        worth-s (:worth-s route-opts)
        dearer? (:dearer? route-opts)
        max-damage (:max-damage route-opts)
        plugs (:plugs route-opts)
        route-opts (dissoc route-opts :slice :spent :own-s :guess-risk :worth-s :dearer? :max-damage :plugs)
        [fx fy fz] from
        [gx gy gz] goal
        ;; each cell is read for its floor, its name to dig and whether it is unseen: sensed once
        known (js/Map.)
        sensed (fn [x y z] (let [k (+ (* (+ (- x fx) 1048576) 2147483648) (* (+ (- z fz) 1048576) 1024) y 512)]
                             (if (.has known k) (.get known k) (let [b (u/sensed p {:x x :y y :z z})] (.set known k b) b))))
        block-at (fn [[x y z]] (when-let [b (sensed x y z)] (if (true? (.-unknown b)) access/hidden-guess (.-name b))))
        seen-floor (fn [[x y z]]
                     (when-let [b (sensed x y z)]
                       (if-not (true? (.-unknown b))
                         (.-name b)
                         ;; the goal's own floor shows only from inside it, as a pocket's: read as a void, no route would end there
                         (let [a (when-not (or floors-unseen-ok? (and (== x gx) (== (inc y) gy) (== z gz))) (sensed x (inc y) z))]
                           (when-not (and a (not (true? (.-unknown a))) (dig-route/open? (.-name a)))
                             access/hidden-guess)))))
        unseen? (fn [[x y z]] (true? (some-> (sensed x y z) .-unknown)))
        clear-ms (search-clear-ms p fetch?)
        base (cond-> (dig-route/body-prices p unseen? {:sensed (fn [[x y z]] (sensed x y z)) :own-s own-s :max-damage max-damage :plugs plugs :clear-ms clear-ms})
               ;; the body's own blocks are dug out however long it takes: the search's bound holds the worth, so their dig is
               ;; not searched (route-s leaves it out too)
               own? (update :dig-s (fn [dig-s] (fn [name cell] (cond-> (dig-s name cell) (own? cell name) (some-> (- (/ (clear-ms name) 1000))))))))
        risk (fn [[x y z :as cell]]
               (cond (not (true? (some-> (sensed x y z) .-unknown))) 0
                     :else (get guess-risk (guess-class (fn [[x y z]] (sensed x y z)) cell) 0)))
        solid-share (escape/dig-share-fn p unseen?)
        ;; an unseen cell is dug at the chance it is solid, as a way's price reads it (escape/dig-share-fn), so a bound
        ;; cuts no route that would win on those prices
        shared (dig-route/by-share base solid-share)
        ;; a harder guess is still dug (its dig's price); one not diggable breaks the route there, at its class's risk, once:
        ;; the look that shows it and the replan round it, guessed (Q49) as two digs of the rock beside it and a step
        miss-s (fn [name cell] (let [s ((:dig-s base) name cell) k (risk cell)]
                                 (if (and s (pos? (solid-share cell)) (pos? k)) (* k (+ (cost/look-s 1) (* 2 s) (:step-s base))) 0)))
        prices (cond-> shared
                 (not (every? zero? (vals guess-risk)))
                 (update :dig-s (fn [dig-s] (fn [name cell] (some-> (dig-s name cell) (+ (miss-s name cell)))))))
        ;; a floor not seen is read as rock where the route may walk on it; the chance it is none (cost/floor-absent-chance of
        ;; the step's kind, the prior turn-back prices it by) is the fall at the measured depths past the step's own (cost/unseen-fall-s less its :fall-s) and the pillar
        ;; back up, priced onto each walk step that lands on one, in the search and the straight dig. Only a floor under a cell
        ;; seen open: under a guessed cell it is the rock mass the dig prices as a guess
        fall (let [self (.self p)]
               {:equipment (cost/equipment-of (.-equipment self)) :max-drop (:max-fall prices)
                :carried (:count (escape/carried-pillar-item p) 0) :share (wworld/unseen-share p)})
        absent-s (memoize (fn [rise]
                            (let [kind (wsight/rise-kind rise)
                                  n (if (= :drop kind) (- rise) 1)]
                              (* (cost/floor-absent-chance (wsight/prior-kind rise) nil)
                                 (max 0 (- (cost/unseen-fall-s (if (= :drop kind) (wsight/prior-kind rise) :down) n fall) ((:fall-s prices) n)))))))
        floor-absent-s (fn [[x y z] rise]
                         (let [a (sensed x (inc y) z)]
                           (if (and a (not (true? (.-unknown a))) (dig-route/open? (.-name a))) (absent-s rise) 0)))
        max-fall (get (merge prices route-opts) :max-fall)
        toll-f (dig-route/toll-fn (:tolls route-opts) (:threat-tolls route-opts) from)
        ;; the straight dig is a way to the same goal: down: y, then x, then z at the goal's height; otherwise x, z, then y: each cell a walked step (and the unseen step's toll)
        ;; plus its dig, and the digs and steps inside water it lets in (dig-route/flood-s). A route dearer is no cheaper than it, so the
        ;; search may stop there (1e-9 over: the same route's seconds summed in the search's order), and the CPU of reading its feet and head cells (:read-s each). A leg through a cell never dug (bedrock), one the search refused or a step onto no seen floor is no way: no bound but the worth
        ;; one whose feet and head reads alone pass dig-route/answer-s is none in hand (Q141): reading it would outgrow a search
        too-long? (> (* 2 (cost/search-read-ceiling-s) (reduce + (map #(js/Math.abs (- %1 %2)) from goal))) dig-route/answer-s)
        straight (fn [refused] (when-not too-long? (let [leg (fn [a b] (let [d (if (< a b) 1 -1)] (range (+ a d) (+ b d) d)))
                         up (max (:step-s prices) (:stair-s prices))
                         down? (< gy fy)
                         ly (if down? gy fy)
                         cells (concat (when down? (map (fn [y] [fx y fz]) (leg fy gy)))
                                       (map (fn [x] [x ly fz]) (leg fx gx))
                                       (map (fn [z] [gx ly z]) (leg fz gz))
                                       (when-not down? (map (fn [y] [gx y gz]) (leg fy gy))))
                         floored? (fn [[x y z]] (let [f (sensed x (dec y) z)
                                                      here (sensed x y z)]
                                                  (if (and f (not (true? (.-unknown f))))
                                                    (not (dig-route/open? (.-name f)))
                                                    (or floors-unseen-ok? (= [x y z] goal) (nil? here) (true? (.-unknown here)) (not (dig-route/open? (.-name here)))))))
                         climb? (fn [[x y z]] (and (not= y ly) (if down? (and (= x fx) (= z fz)) (and (= x gx) (= z gz)))))
                         open-at? (fn [cell] (let [nm (seen-floor cell)] (or (nil? nm) (dig-route/open? nm))))
                         ;; down, a cell dug drops the body through every open cell under it at once: one fall, landing
                         ;; on the first floor (as the search reads floors), none past :max-fall or below the goal's height
                         down-steps (when down?
                                      (loop [ys (leg fy gy) n 1 out []]
                                        (if-let [y (first ys)]
                                          (cond
                                            (open-at? [fx (dec y) fz]) (if (= y gy) (conj out nil) (recur (rest ys) (inc n) (conj out 0)))
                                            (> n (:max-fall prices)) (conj out nil)
                                            :else (recur (rest ys) 1 (conj out (+ (:step-s prices) ((:fall-s prices) n)))))
                                          out)))
                         cell-s (fn [[x y z :as cell]]
                                  (let [unseen? (true? (some-> (sensed x y z) .-unknown))
                                        nm (block-at cell)
                                        climb? (climb? cell)
                                        fall? (and down? (= x fx) (= z fz))
                                        step (cond fall? (get down-steps (- fy y 1)) climb? up :else (:step-s prices))
                                        dig (if (or (nil? nm) (dig-route/open? nm)) 0 ((:dig-s prices) nm cell))]
                                    (when (and dig step (or climb? fall? (floored? cell)))
                                      [dig (+ step (if unseen? (:unseen-step-s prices) 0))])))
                         head-s (fn [[x y z]] (let [h [x (inc y) z]
                                                    nm (block-at h)]
                                                (if (or (nil? nm) (dig-route/open? nm)) 0 ((:dig-s prices) nm h))))
                         steps (map cell-s cells)
                         heads (map head-s (cons from cells))
                         ;; each step's own seconds (its head and feet dug, the step) times its cell's toll, as the search prices it
                         tolls (map (fn [[x y z :as cell] [dig step] head] (let [f (toll-f x y z (or (climb? cell) (and down? (= x fx) (= z fz))))] (if (pos? f) (* f (+ dig step head)) 0)))
                                    cells steps (rest heads))
                         ;; each cell's head then its feet dug, then the step into it, as the search adds them
                         moves (distinct (for [[x y z :as c] (cons from cells)
                                               m (concat (map #(vector :dig %) (if (= c from) [[x (inc y) z]] [[x (inc y) z] c]))
                                                         (when-not (= c from) [[:step c]]))
                                               :when (or (= :step (first m)) (not (let [nm (block-at (m 1))] (or (nil? nm) (dig-route/open? nm)))))]
                                           m))
                         dig (vec (keep (fn [[kind cell]] (when (= :dig kind) cell)) moves))]
                     (when (and (not-any? refused (concat cells (map (fn [[x y z]] [x (inc y) z]) cells)))
                                (every? some? steps) (every? some? heads) (not-any? #{##Inf} tolls))
                       {:seconds (+ (reduce + (reduce + 0 heads) (map first steps)) (reduce + 0 (map second steps)) (reduce + 0 tolls)
                                    (if-let [flood (:flood prices)]
                                      (dig-route/flood-s flood (map (fn [[kind cell]] [kind cell (block-at cell)]) moves))
                                      0)
                                    (reduce + 0 (for [[x y z :as cell] cells
                                                      :let [i (- fy y 1)]
                                                      :when (and (or (not (climb? cell)) (and down? (pos? (get down-steps i 0)))) (true? (some-> (sensed x (dec y) z) .-unknown)))]
                                                  ;; a landing's rise: the open cells fallen through before it and its own
                                                  (floor-absent-s [x (dec y) z] (if (and down? (= x fx) (= z fz)) (- -1 (count (take-while zero? (rseq (subvec down-steps 0 i))))) 0))))
                                    (* (:read-s route-opts 0) 2 (count cells)))
                        :dig dig}))))
        straight-s (fn [refused] (or (:seconds (straight refused)) ##Inf))
        ;; a route dearer than worth-s is none: no search goes on past it
        max-s (fn [refused] (min (or (:bound-s route-opts) ##Inf) (or worth-s ##Inf) (* (straight-s refused) (+ 1 1e-9))))
        allowed? (memoize #(escape/diggable? block-at may-dig? %))
        ;; planned only when a search begins, not on each slice that goes on with a kept one: its CPU is that search's
        lane (delay (dig-route/lane from goal (merge prices route-opts) #(when-let [n (seen-floor %)] (not (dig-route/open? n))) #(when-let [n (block-at %)] (nil? (dig-route/cell-cost n)))))
        opts (fn [refused m spent lazy?] (merge prices
                                  ;; the straight dig is a route in hand: past answer-s with none cheaper the search ends :dearer and it answers
                                  {:max-s m :back-h? true :floor-absent-s floor-absent-s :searched-s spent :held-s (let [st (straight-s refused)] (when (< st ##Inf) st))}
                                  ;; a cell seen-floor reads nameless (not seen, under a cell seen open: no floor) is still dug at its guess
                                  {:dig-s (fn [name cell] (when (and (not (refused cell)) (escape/diggable? block-at (constantly true) cell) (or (not lazy?) (allowed? cell)))
                                                            ((:dig-s prices) (or name (block-at cell)) cell)))}
                                  route-opts
                                  (when-let [cell (:see route-opts)] {:sees (aim/dug-stand-sees-fn p cell)})))
        ;; the search itself reads no may-dig?: escalate!'s choices with other zone rules share it
        search (fn [refused spent lazy?]
                 (let [m (max-s refused)
                       a (if slice
                           (sliced-answer slice [from goal floors-unseen-ok? (dissoc route-opts :pillar-s) guess-risk refused lazy? worth-s (select-keys fall [:equipment :carried]) spent (some? fetch?) (tools/carried-dig-tools p)] #(dig-route/seeded-search seen-floor from goal (merge (opts refused m spent lazy?) @lane {:full? dig-route/heap-full?})) m (route-epoch p)
                                         {:reprice #(dig-route/route-within seen-floor from goal % (assoc (opts refused m spent lazy?) :max-s nil :read-s nil :searched-s 0 :back-h? false)) :max-drop max-fall})
                           (dig-route/route seen-floor from goal (merge (opts refused m spent lazy?) @lane)))]
                   a))
        ;; may-dig? (zones, claims, plans) is asked of a found route's cells only, not of every cell the search weighs: it was
        ;; about half a search's time; a refused cell is searched round, each search counting the CPU the ones before it spent
        ;; (:searched-s). The rounds end on the route's price (dearer than max-s) or, with no bound, in the second: once a
        ;; route has been refused may-dig? is asked of each cell the search weighs (allowed?), so no route it finds is refused again
        a (when-not (contains? spent [from goal])
            ;; with a :range the goal is worked from a stand within it, never dug away
            (loop [refused (if (>= (:range route-opts 0) 1) #{(:near route-opts goal)} #{}) spent (:searched-s route-opts 0) lazy? false]
            (let [a (search refused spent lazy?)
                  ;; the straight dig needs no search: when the search prices its own expansions past it or spends its nodes, it is the route
                  a (if-let [st (when (#{:dearer :budget} (:problem a)) (some-> (straight refused) (as-> st (when (<= (:seconds st) (or (:bound-s route-opts) ##Inf)) st))))] st a)
                  bad (when (map? a) (remove allowed? (:dig a)))]
              (cond (= :searching a) a
                    (nil? (:dig a)) (when dearer? (cond (= :dearer (:problem a)) :dearer (:boxed a) a))
                    (empty? bad) a
                    (map? a) (recur (into refused bad) (:searched-s a spent) true)))))]
    (if (or (nil? a) (#{:searching :dearer} a) (:boxed a))
      a
      ;; the search dug at search-clear-ms: the route pays the fetch when that is the quicker, else digs with what is carried
      (let [tool (route-tool p block-at (:dig a) own? fetch? solid-share)
            a (update a :seconds + (- (:s tool) (:searched tool)))
            ;; the search and the worth checks dig the body's own blocks free (its escape is not held to the worth); the reported price pays them
            own-s (if own? (transduce (comp (filter #(own? % (block-at %))) (map #(/ (clear-ms (block-at %)) 1000))) + 0 (:dig a)) 0)]
        (cond
          (not (cost/affords? (:s tool) (min (or worth-s ##Inf) (or (:bound-s route-opts) ##Inf)))) nil
          (cost/affords? (:seconds a) (or worth-s ##Inf))
          (assoc a :seconds (+ (:seconds a) own-s)
                 :share-s (- (+ (:seconds a) own-s) (transduce (map #(miss-s (block-at %) %)) + 0 (:dig a))))
          dearer? :dearer)))))

(defn route-cells
  "[{:cell :block :guess :class :floor-guess}]: the cells to dig of route-answer's way, in walking order, :guess on a cell not seen (read as
  rock), :class its guess-class, :floor-guess the cell under it when the route stands on that cell unseen (read as rock) and does
  not dig it; [] when nothing is left to dig; :searching or nil as route-answer; :dearer, or dig-route's
  {:problem :no-route :boxed true :searched-s} (no route in the :margin box), under route-opts :dearer?."
  ([p from goal may-dig? own?] (route-cells p from goal may-dig? own? false false))
  ([p from goal may-dig? own? floors-unseen-ok?] (route-cells p from goal may-dig? own? floors-unseen-ok? false))
  ([p from goal may-dig? own? floors-unseen-ok? fetch?] (route-cells p from goal may-dig? own? floors-unseen-ok? fetch? nil))
  ([p from goal may-dig? own? floors-unseen-ok? fetch? route-opts]
   (let [a (route-answer p from goal may-dig? own? floors-unseen-ok? fetch? route-opts)
         block-at (escape/block-at-of p)]
     (cond
       (or (#{:searching :dearer} a) (:boxed a)) a
       a (let [sensed #(u/sensed p (zipmap [:x :y :z] %))]
           (mapv #(cond-> % (:guess %) (assoc :class (guess-class sensed (:cell %))))
                 (route-ways/dig-cells block-at #(dig-look/unknown? p %) (:dig a))))))))

(defn way-guesses
  "[{:cell :class}]: the cells stair e from feet cuts that the body has not sensed (read as rock), each with its
  guess-class."
  [p {:keys [step heading steps]} feet]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        sensed #(u/sensed p (zipmap [:x :y :z] %))
        cells (case step
                :stair (escape/stair-cuts feet (escape/heading-dirs heading) steps)
                nil)]
    (vec (for [cell cells :when (unseen? cell)] {:cell cell
                                                :class (guess-class sensed cell)}))))

(defn past-worth?
  "Whether a way priced price-s (its own cells' put-backs too) is dearer than worth-s once priced over the chance each
  of its guesses is diggable (1 - its class's risk in guess-risk {class chance}): the stair or door has no search to price them in."
  [price-s guesses guess-risk worth-s]
  (not (cost/affords? (/ price-s (reduce * 1 (map #(- 1 (get guess-risk (:class %) 0)) guesses))) worth-s)))

(defn search-opts
  "route-answer's route-opts for a choice's opts: its :slice, its :spent searches, its :own-s and :own-cut
  (a way that may cut the body's own builds: searched apart), its :tolls, :guess-risk ({class chance} that a guess of the class fails) and :worth-s (a route costing more is none), its :range, :near and :see and the floors under its :stands
  (the body's own stairs) as :ways."
  [{:keys [stands slice spent own-s own-cut range near see guess-risk worth-s bound-s closed? max-damage plugs tolls threat-tolls]}]
  (cond-> {:ways (into #{} (map (fn [[x y z]] [x (dec y) z])) stands) :bound-s bound-s :read-s (cost/search-read-s) :max-damage max-damage}
    closed? (assoc :max-fall 1)
    plugs (assoc :plugs plugs) (seq tolls) (assoc :tolls tolls) (seq threat-tolls) (assoc :threat-tolls threat-tolls) slice (assoc :slice slice) (seq spent) (assoc :spent spent) own-s (assoc :own-s own-s) own-cut (assoc :own-cut true)
    (and range near) (assoc :range range :near near) see (assoc :see see)
    guess-risk (assoc :guess-risk guess-risk :worth-s worth-s)))

(defn range-opts
  "Where the go-to call c's dig routes to pos may end: :range, :near (pos's cell) and under :see :see, the block the end stand must see;
  and the tolls the walk planner adds to the call's: the cells of other bodies' zones near the way (:tolls, with the call's) and every known threat's field (:threat-tolls)."
  [c pos]
  (let [zone (when (wworld/zone-tolls?) (tc/zone-walk-tolls c pos))
        all (when (wworld/dangers?) (threats/known-threats c))]
    (cond-> {:range (:range (:args c) 1) :near (mapv #(js/Math.floor (% pos)) [:x :y :z])}
      (goal-aim/see-cell c) (assoc :see (goal-aim/see-cell c))
      zone (assoc :tolls (into (walk-tolls c) zone))
      (seq all) (assoc :threat-tolls (threats/shared-tolls c all all)))))

(defn dig-way
  "{:step :dig-route :cells (route-cells)} when there are cells to dig, {:step :searching} while opts :slice's search
  goes on (route-cells), else nil."
  [p from goal may-dig? {:keys [own? skip fetch? slice] :as opts}]
  (when-not (skip :dig-route)
    (let [cells (route-cells p from goal may-dig? own? false fetch? (search-opts opts))]
      (cond (= :searching cells) {:step :searching}
            (seq cells) {:step :dig-route :cells (vec cells)}))))

(def leg-blocks
  "The most blocks along the straight line to the goal one leg of a long dig (dig-leg) reaches: its route stays within
  the dig-route settings' digs."
  6)

(def leg-margin
  "Blocks round its two ends a leg's route (dig-leg) first strays: a leg runs along the line, and a narrow box keeps
  its search small."
  1)

(defn leg-ends
  "The feet cells on the straight line from feet toward goal (cells [x y z]) leg-blocks and half that on, farthest
  first; the goal alone when it is that near (the last leg), none at the goal."
  [feet goal]
  (let [d (js/Math.sqrt (reduce + (map #(* (- %2 %1) (- %2 %1)) feet goal)))]
    (cond
      (> d leg-blocks) (distinct (for [i [leg-blocks (quot leg-blocks 2)]]
                                   (mapv #(js/Math.floor (+ %1 0.5 (* (/ i d) (- %2 %1)))) feet goal)))
      (pos? d) [goal])))

(defn time-left-s
  "Seconds the call chain of c has left (ctx/left: job time, wall time, deadline), ##Inf when nothing bounds it."
  [c]
  (let [{:keys [spend deadline]} (when (:left c) (ctx/left c))]
    (apply min ##Inf (cond-> (keep (or spend {}) [:job-time :wall-time])
                       deadline (conj (/ (- deadline (ctx/now c)) 1000))))))

(defn call-budget-s
  "Seconds the go-to call c may spend on a way: what the goal is worth (ctx/worth) within the time its chain has left."
  [c]
  (min (ctx/worth c) (time-left-s c)))

(defn dig-left-s
  "Seconds of digging the go-to call c has left for its next door or dig leg: call-budget-s less what its doors and
  legs so far took (memory :dug-s)."
  [c]
  (- (call-budget-s c) (:dug-s (ctx/mem c) 0)))

(defn dig-leg
  "A buried body's next leg of a dig toward a goal: {:step
  :dig-route :cells :leg-s} to the farthest of leg-ends that route-cells reaches, :leg-s its dig seconds, searched
  within leg-margin of the line, then, when no end is reached there and none was dearer than the goal is worth, round
  what blocks it (a column of lava, bedrock or another's claim) within the leg's own length of its two ends, that box
  doubled while it alone kept the search from an end (dig-route's :boxed), each sliced as the first; an end but the goal
  over seen air is the cell over the first floor under it, left out when that floor is unseen or there is none, and one
  no route ends in (lava, bedrock, a claim) the stand nearest it (searched outward ring by ring, as many cells as the
  call's budget buys at cost/search-read-s) that is nearer the goal than from. The wider boxes'
  searches are bounded by the least of opts :bound-s, :worth-s and the way without a dig route (memory :route-search
  :fallback-s), and each counts the CPU the boxed searches before it spent (:searched-s): the widening ends :past-worth
  once the search has spent what the leg could save, and with no end reached once the body remembers nothing within a
  step's reads of the box's faces (seen-sections/faces-unseen?);
  nil when none does; {:step :searching} while opts :slice's search goes on; {:step :none :why :dig-budget} when this leg would dig
  longer than the call has left (dig-left-s); {:step :none :why :past-worth} when no end is reached because every
  route to them costs more than the goal is worth (the search stopped at that bound, not at the end of the rock)."
  [c from goal may-dig? {:keys [own? skip fetch?] :as opts}]
  (when-not (skip :dig-route)
    (let [p (:primitives c)
          block-at (escape/block-at-of p)
          length (fn [end] (js/Math.ceil (js/Math.hypot (- (end 0) (from 0)) (- (end 1) (from 1)) (- (end 2) (from 2)))))
          ;; feet and head cells a route may end in (each open or diggable, no fluid, bedrock or another's claim, no lava
          ;; beside to flow in) over a floor
          stand? (fn [[x y z]] (let [cells [[x y z] [x (inc y) z]]]
                                 (and (every? #(and (some? (dig-route/cell-cost (block-at %))) (escape/diggable? block-at may-dig? %)) cells)
                                      (dig-route/floor-ok? (block-at [x (dec y) z]))
                                      (not-any? (fn [[n]] (= "lava" n)) (mapcat #(rules/fluid-neighbours block-at %) cells)))))
          ;; offsets at Chebyshev distance r: the stand nearest an end is looked for outward, ring by ring
          shell (fn [r] (let [rs (range (- r) (inc r)) in (range (- 1 r) r)]
                          (concat (for [dy rs dz rs s [(- r) r]] [s dy dz])
                                  (for [dx in dz rs s [(- r) r]] [dx s dz])
                                  (for [dx in dy in s [(- r) r]] [dx dy s]))))
          gap (fn [a b] (js/Math.hypot (- (a 0) (b 0)) (- (a 1) (b 1)) (- (a 2) (b 2))))
          ;; the cells one end's stand search may read, priced like a route search's nodes against what the leg could save at most
          ;; (the call's budget, opts :bound-s and :worth-s)
          budget-s (min (call-budget-s c) (or (:bound-s opts) ##Inf) (or (:worth-s opts) ##Inf))
          scan-cells (/ budget-s (cost/search-read-s))
          read-cells (fn [r] (js/Math.pow (inc (* 2 r)) 3))
          ;; a leg end over seen air is no stand a dig route makes (it lays no floor): the body lands under it, the fall
          ;; priced by the route, on a floor seen (one never seen under seen air is no landing to plan a leg to: none); one no
          ;; route can end in (inside lava, bedrock or a claim) is the stand nearest it, within
          ;; nearer the goal than the body, searched outward (no stand nearer the goal lies beyond the two distances' sum); the goal is the call's own
          ends (distinct
                (keep (fn [[x y z :as end]]
                       (when-let [end (if (or (= end goal) (not (dig-route/open? (block-at [x (dec y) z]))))
                                        end
                                        (when-let [f (some #(when-not (dig-route/open? (block-at [x (dec %) z])) %) (range (dec y) (places/min-y p) -1))]
                                          (when-not (dig-look/unknown? p [x (dec f) z]) [x f z])))]
                         (let [reach (js/Math.ceil (+ (gap end goal) (gap from goal)))]
                           (if (or (= end goal) (stand? end))
                             end
                             (or (some (fn [r] (->> (shell r)
                                                    (keep #(let [c (mapv + end %)] (when (and (< (gap c goal) (gap from goal)) (stand? c)) c)))
                                                    (sort-by #(gap % end))
                                                    first))
                                       (take-while #(<= (read-cells %) scan-cells) (range 1 (inc reach))))
                                 end)))))
                     (leg-ends from goal)))
          ;; [cells seconds], :searching, :dearer, or the seconds the boxed searches spent when the box cut every end off
          leg (fn [margin route-opts]
                (loop [ends ends dear? false boxed-s nil]
                  (if-let [end (first ends)]
                    (let [cells (route-cells p from end may-dig? own? false fetch? (assoc route-opts :margin (margin end) :dearer? true))]
                      (cond (= :searching cells) :searching
                            (and (sequential? cells) (seq cells)) [(vec cells) (route-s p (map :cell cells) own? fetch?)]
                            :else (recur (rest ends) (or dear? (= :dearer cells)) (if (:boxed cells) (+ (or boxed-s 0) (- (:searched-s cells) (:searched-s route-opts 0))) boxed-s))))
                    (cond boxed-s boxed-s dear? :dearer))))
          first-pass (leg (constantly leg-margin) (search-opts opts))
          ;; a box keeps the search's heap to what it holds; a wider one is searched only once this one cut the way off,
          ;; under what the leg could save at most, with what the boxes before it spent counted
          wide-opts (update (search-opts opts) :bound-s #(min (or % ##Inf) (:fallback-s (:route-search (ctx/mem c)) ##Inf)))
          ;; whether the box of k round each end holds no remembered cell within the 3 cells a step reads past it (the cell
          ;; over its head and the water over that) of its faces: a way out of the box crosses guessed rock to come back,
          ;; and one along the box's guessed side joins the same two cells, so a wider box finds no way this one did not
          covers? (fn [k] (every? (fn [end] (let [m (* k (length end))]
                                              (seen-sections/faces-unseen? p [(- (min (from 0) (end 0)) m) (- (min (from 1) (end 1)) m) (- (min (from 2) (end 2)) m)
                                                                              (+ (max (from 0) (end 0)) m) (+ (max (from 1) (end 1)) m) (+ (max (from 2) (end 2)) m)]
                                                                           3 (places/min-y p))))
                                  ends))
          found (if (or (nil? first-pass) (number? first-pass))
                  (loop [k 1 spent-s (or first-pass 0)]
                    (let [r (leg #(* k (length %)) (assoc wide-opts :searched-s spent-s))]
                      (cond (not (number? r)) r
                            (covers? k) nil
                            :else (recur (* 2 k) (+ spent-s r)))))
                  first-pass)]
      (cond
        (= :searching found) {:step :searching}
        (= :dearer found) {:step :none :why :past-worth}
        found (let [[cells s] found]
                (if (<= s (dig-left-s c))
                  {:step :dig-route :cells cells :leg-s s}
                  {:step :none :why :dig-budget :dug-s (:dug-s (ctx/mem c) 0) :budget-s (call-budget-s c)}))))))

(defn dig-toward
  "dig-way to a goal within leg-blocks of from, else its next leg (dig-leg, which may be {:step :none :why ..}): a
  whole-way search to a far goal searches a ball of guessed rock round the body."
  [c from goal may-dig? opts]
  (if (next (leg-ends from goal))
    (dig-leg c from goal may-dig? opts)
    (dig-way (:primitives c) from goal may-dig? opts)))

(defn nearest-stand
  "A cell [x y z] the body can walk to (a flood of at most reach-room/room-cells cells) nearest goal: of the cells no farther
  than slack (default 0) beyond the nearest, in the whole distance and in the distance along the ground, the first the
  flood reaches (the shortest walk). A route slice given is charged the flood's cells."
  ([p goal] (nearest-stand p goal 0 nil))
  ([p goal slack] (nearest-stand p goal slack nil))
  ([p goal slack slice]
   (let [kind-at (reach/seen-lookup p reach-room/body-kind-of)
         [gx gy gz] goal
         ;; plain arithmetic: a flood is up to room-cells cells, each weighed twice
         across #(js/Math.hypot (- (aget % 0) gx) (- (aget % 2) gz))
         to-goal #(js/Math.hypot (- (aget % 0) gx) (- (aget % 1) gy) (- (aget % 2) gz))
         ^js cells (:cells (reach-room/flood-visit #(reach/forward kind-at %1 %2 %3) (reach/standing-vec p) reach-room/room-cells))
         best (.reduce cells (fn [a b] (if (< (to-goal b) (to-goal a)) b a)))
         near (+ (to-goal best) slack)
         flat (+ (across best) slack)]
     (some-> slice (charge! (.-length cells) (flood-cells-per-node)))
     (vec (.find cells #(and (<= (to-goal %) near) (<= (across %) flat)))))))

(defn frontier-way
  "A shut-in body's way off its land (a plateau top) toward a goal too far for one dig route: {:step :dig-route :cells}
  to the first seen stand below a step beyond the land's edge, nearest goal (and more than a block
  nearer than feet), the next when no route reaches it (route-cells, opts as dig-way's); {:step :searching} while that
  search goes on; else, while the land beyond is not seen, {:step :approach :pos cell} to the edge cell nearest goal (not
  one approached, more than a block nearer than feet) to look over it; nil when none."
  [p feet goal may-dig? {:keys [own? skip fetch? slice approached] :as opts}]
  (when-not (skip :dig-route)
    (let [kind-at (reach/seen-lookup p reach-room/body-kind-of)
          ^js cells (:cells (reach-room/flood-visit #(reach/forward kind-at %1 %2 %3) (reach/standing-vec p) reach-room/room-cells))
          land (into #{} (map vec) cells)
          _ (some-> slice (charge! (.-length cells) (flood-cells-per-node)))
          to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
          nearer? #(< (to-goal %) (dec (to-goal feet)))
          stand? (fn [[x y z]] (reach/standable-cell? p {:x x :y y :z z} kind-at))
          off (fn [[x y z]] (for [[dx dz] escape/cardinals
                                  :when (not-any? #(land [(+ x dx) % (+ z dz)]) [(dec y) y (inc y)])]
                              [(+ x dx) y (+ z dz)]))
          below (fn [[x y z]] (some #(let [c [x % z]] (when (stand? c) c)) (range (inc y) (dec (places/min-y p)) -1)))
          stands (->> land (mapcat off) (keep below) (remove land) distinct (filter nearer?) (sort-by to-goal))
          edge (delay (->> land (filter #(seq (off %))) (remove #(contains? approached %)) (filter nearer?) (sort-by to-goal) first))]
      (or (->> stands
               (take route-searches)
               (some #(let [cells (route-cells p feet % may-dig? own? false fetch? (search-opts opts))]
                        (cond (= :searching cells) {:step :searching}
                              (seq cells) {:step :dig-route :cells (vec cells)}))))
          (some->> @edge (hash-map :step :approach :pos))))))

(defn leads-in?
  "Whether escape-way/choose's way e from cell from leads into a walled-in goal: a stair or pillar up to a higher
  goal, a walk along the wall to a goal not below it."
  [{:keys [step]} from goal]
  (case step
    (:stair :pillar) (> (second goal) (second from))
    :approach (>= (second goal) (second from))
    false))

(defn no-dig "A route search's :dig-s that digs no cell." [_ _] nil)

(defn rim-leads-in?
  "Whether escape-way/choose's rim pillar e from cell from leaves the body on a wall's top (a wall h high, e's height h - 1,
  then a step onto the rim: the side toward goal when it is that high, as escape/rim-pillar measures it, else the first
  that is) from which a walk with no dig reaches goal; :searching while opts :slice's search goes on."
  [p {:keys [height]} from goal opts]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        d (some #(when (= (inc height) (escape/rim-height block-at unseen? from %)) %)
                (distinct (cond->> escape/cardinals (escape/heading from goal) (cons (escape/heading from goal)))))
        ;; searched with no dig at all: a dig route found and refused cell by cell would be searched again for each
        walk (when d (route-cells p (escape/up (escape/ahead from d 1) (inc height)) goal (constantly false) (:own? opts) false false (assoc (search-opts opts) :dig-s no-dig)))]
    (if (= :searching walk) walk (= [] walk))))

(defn goal-way
  "A free body's way into a walled-in goal: escape-way/choose from where it stands, or, while the cell it can walk to
  nearest the goal (nearest-stand) is more than a block nearer, from there, as
  {:step :approach :pos cell} first. Only a way that leads-in? (a walk along the wall from where it stands only; a rim
  pillar to a goal not above only when the wall's top walks on to it), else the fewest cells dug (dig-way, :searching
  while its search goes on); else {:step :none :why :no-dig}, from the nearest cell once it has been walked to."
  [c feet goal may-dig? opts]
  (let [p (:primitives c)
        to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
        near (nearest-stand p goal 0 (:slice opts))
        here? (or ((:skip opts) :approach) (>= (to-goal near) (dec (to-goal feet))))
        from (if here? feet near)
        level? (<= (second goal) (second from))
        ;; a stair, and a pillar but a rim one that walks on, lead in only to a goal above: priced against the door they
        ;; would win and then be dropped
        e (escape-way/choose p from goal may-dig? (cond-> opts level? (assoc :drop-steps #{:stair})))
        rim (when (and level? (= :pillar (:step e))) (rim-leads-in? p e from goal opts))
        e (cond (= :searching rim) {:step :searching}
                (false? rim) (escape-way/choose p from goal may-dig? (assoc opts :drop-steps #{:pillar :stair}))
                :else e)
        in? (and (or (leads-in? e from goal) (true? rim) (= :searching (:step e)))
                 (or here? (not= :approach (:step e))))
        dw (when-not in? (dig-way p from goal may-dig? opts))
        e (cond in? e
                :else (or dw
                          (if (= :none (:step e)) e {:step :none :why :no-dig})))]
    (cond
      (= :searching (:step e)) e
      here? e
      ;; from where it stands it may misread the land between (rock it has not seen): it walks nearer and looks
      (and (= :none (:step e)) (contains? (:approached opts) near)) e
      :else {:step :approach :pos near})))

(defn guess-risk
  "{class chance}: the chance a guess of each escape/guess-classes class is not diggable (cost/guess-bad-chance): those this
  call looked at on top of the measured prior."
  [c]
  (let [looks (:guess-looks (ctx/mem c))]
    (into {} (map (fn [k] [k (cost/guess-bad-chance k (get looks k))])) escape/guess-classes)))

(defn way-opts
  "The opts of go-to call c's choice of a way from feet (escape-way/choose and the dig ways): a search that stood the body
  still past its worth (search-bound, memory :spent-routes) is not begun again; a dig route prices a cell of the body's
  own builds at its put-back (:own-s); escape-way/choose prices a way's dangers by walk-risk/way-risk-s (:risk-s). A
  choice after a failed escalation searches no dearer than the dig route's replans (:worth-s), held with its searches
  (memory :route-search) over the rounds they take."
  [c feet fetch? slice]
  {:closed? (= :closed (some-> (:one-way (:args c)) keyword)) :own? (own-placed-fn c) :skip (skips c) :spent (:spent-routes (ctx/mem c) #{}) :fetch? (pickaxe-fetch c fetch?) :own-s (own-cut-fn c) :plugs (smem/plugs-of c)
   :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c) :stands (ledger/stair-stands c) :slice slice
   :guess-risk (guess-risk c) :worth-s (let [m (ctx/mem c)] (if-let [rs (:route-search m)] (:worth-s rs) (when (:escalation m) (:fallback-s (:replan-budget m))))) :bound-s (call-budget-s c)
   :max-damage (:max-damage (wworld/walk-settings c)) :tolls (walk-tolls c)
   :risk-s (fn [cells goal work-s] (walk-risk/way-risk-s c (zipmap [:x :y :z] goal) cells work-s))})

(defn column-target
  "The cell go-to call c's escalation makes a way to for pos: for a column goal (:column-goal) of more than one stand,
  the stand ({:x :y :z}) the cheapest dig route from feet ends in :range of (route-answer's :seconds over way-opts: a
  built cell costs its put-back), :searching while one of their searches goes on in slice; else, or when no stand has
  a route, pos (the aimed stand: a stair or pillar is chosen to it as before)."
  [c feet pos slice]
  (let [{:keys [x z stands]} (:column-goal c)]
    (if (or (not (wplan/column-goal? c pos)) (< (count stands) 2) (contains? (skips c) :dig-route))
      pos
      (let [p (:primitives c)
            may-dig? (may-dig-fn c)
            base (way-opts c feet (fetch-tool? c) slice)
            priced (mapv (fn [y]
                           (let [cell {:x x :y y :z z}
                                 o (merge base (range-opts c cell))
                                 a (route-answer p feet [x y z] may-dig? (:own? o) false (:fetch? o) (search-opts o))]
                             [cell (cond (= :searching a) :searching (:dig a) (:seconds a))]))
                         stands)
            routes (filter (comp number? second) priced)]
        (cond (some #(= :searching (second %)) priced) :searching
              (seq routes) (first (apply min-key second routes))
              :else pos)))))

(defn detour-way
  "A buried body's way to a goal its only found way reaches by a long detour (walk.cljs :detour): the cheapest cells
  dug from the feet into goal pos (dig-way), {:step :searching} while that search goes on (kept for the call: ask again),
  else a stair when the goal is above; nil when the call may not escalate, tried it already (memory :detour-tried) or
  neither is there."
  [c pos]
  (when (and (ways/escalating? c) (not (:detour-tried (ctx/mem c))))
    (let [p (:primitives c)
          feet (end/feet-cell c)
          goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
          slice (route-slice (route-owner c))
          opts (merge (way-opts c feet (fetch-tool? c) slice) (range-opts c pos))
          may-dig? (may-dig-fn c)
          e (dig-way p feet goal may-dig? opts)]
      (keep-routes! slice)
      (or e
          (when (> (second goal) (second feet))
            (let [e (escape-way/choose p feet goal may-dig? opts)]
              (when (= :stair (:step e)) e)))))))

(defn dig-price
  "Seconds the fewest cells dug (dig-way) from the feet to the stand cell goal ([x y z]) cost go-to call c, the rest of the
  way walked as a quote prices it, and no more than the call's worth; nil when the call may not dig a route or none is found.
  A running call (route-owner) has its search go on, :searching, at its next quote; a quote of no running call asks one
  slice and gets :budget when that spent its nodes (no search is kept: each quote ask would otherwise grow it for ever, at
  no time spent). A quote asks this when the walk's planner found no walk. walk-s, given when that walk was cut by its
  budget (a partial to walk on): a dig route dearer than walking on is no use, so the search ends by that price (nil). The
  walk on alone past the worth is nil unsearched."
  [c goal walk-s]
  (when-let [[feet rest-s] (when (ways/way? c :dig-route)
                             (let [feet (end/feet-cell c)
                                   rest-s (cost-planner/rest-s (js/Math.round (u/dist (zipmap [:x :y :z] feet) (zipmap [:x :y :z] goal))))]
                               (when (cost/affords? rest-s (ctx/worth c)) [feet rest-s])))]
    (let [p (:primitives c)
          owner (:route-owner (ctx/mem c))
          quote-id (str (:id c) "/quote")
          slice (route-slice (or owner quote-id))
          opts (cond-> (assoc (merge (way-opts c feet (fetch-tool? c) slice) (range-opts c (zipmap [:x :y :z] goal))) :read-s (cost/search-read-s))
                 walk-s (update :bound-s min walk-s))
          e (dig-way p feet goal (may-dig-fn c) opts)]
      (keep-routes! slice)
      (when-not owner (forget-routes! quote-id))
      (case (:step e)
        :searching (if owner :searching :budget)
        :dig-route (let [s (+ (route-s p (map :cell (:cells e)) (:own? opts) (:fetch? opts)) rest-s)]
                     (when (cost/affords? s (ctx/worth c)) s))
        nil))))

(def stair-first-rise
  "How far above the feet a goal is for a found plan to be weighed against a stair up."
  8)

(defn stair-dark-factor
  "How many times dearer a stair is cut where the body stands dark: the plan's seconds carry its dark surcharge, and the
  stair is cut in that dark too."
  [c]
  (if (and (wworld/dark?) (look/dark-here? (:primitives c))) (+ 1 (cost/dark-factor (wworld/kit-of c))) 1))

(defn stair-check
  "A fn of a plan and whether it is the first of its round, saying why it is not walked and a stair up is cut instead (a
  re-plan inside a walk is held to :dives only), when escalating is on, the body is not in its shelter and escape-way/choose would stair (steps n):
  - :dives: under a cave floor (c :cave-floor, with c :dive-stair) the plan goes below it on a cell that is no stair of
    the body's own (climb/dives?), or below c :own-floor back down a stair of its own (climb/returns?);
  - :cheaper: it is a whole path costing more seconds than cutting a new stair up to the goal's height (escape-way/new-stair-step-s a step; a stair cut in the dark costs as much more as the dark walk does), the goal is stair-first-rise or more above
    the feet and the stair has not been tried this call.
  - :retreats: it is a partial plan (the way on is shut), a re-plan too, that goes down the body's own stair, for a goal no lower than
    the feet, and escape-way/choose has a way that changes the world: the stair was cut to get up, not to go back down.
  Else nil."
  [c pos]
  (let [m (ctx/mem c)
        feet (end/feet-cell c)
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        cave (when (:dive-stair c) (:cave-floor c))
        back (:own-floor c)
        cheaper? (and (not (:stair-tried m)) (>= (- (second goal) (second feet)) stair-first-rise))
        retreats? (>= (second goal) (second feet))
        chosen (delay (escape-way/choose (:primitives c) feet goal (may-dig-fn c) {:skip (skips c) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c) :stands (ledger/stair-stands c)}))
        stair (delay (when (= :stair (:step @chosen)) @chosen))
        way (delay (not (#{:none :approach} (:step @chosen))))]
    (when (and (ways/way? c :stair) (not (sh-inside/sheltered-in c)) (or cave cheaper? back retreats?))
      (fn [{:keys [status steps ^js r]} first?]
        (let [seconds (some-> r .-path .-cost .-priced)]
          (cond
            (and cave (climb/dives? steps cave (ledger/ways-of c)) @stair) :dives
            (and back (climb/returns? steps back (ledger/stair-stands c)) @stair) :dives
            (and retreats? (= "partial" status) (climb/returns? steps (second feet) (ledger/stair-stands c)) @way) :retreats
            (and first? cheaper? (= "found" status) (number? seconds) @stair (> seconds (* (stair-dark-factor c) (- (second goal) (second feet)) (escape-way/new-stair-step-s (:primitives c))))) :cheaper))))))

(defn escalate?
  "Whether a give-up after tries fruitless walks with result may escalate instead: a carried block fills the 1-cell gap it was refused (gap-fill), its only way dives (dive?), the goal is walled in (goal-walled?) or off behind a gap nothing fills (gap-walled?), or the walker failed max-blocked walks running (walker-fault?; one stuck walk of a :tries 1 caller is no proof) and no door or gate borders where it can walk
  (escape/door-beside?: that is its way out; an iron door, or any door under :doors :never, is a wall), or the planner's
  reason proves the way needs the world changed and the body is shut in (reach-room/enclosed?, which already counts doors
  as opened: a door that leads nowhere, e.g. onto a step under its lintel, is no way out), or the search ran out of
  land (:exhausted, after the walks that looked at every edge) in a mine or cave larger than a room, or the planner
  proved the start sealed (:start-enclosed: the walls it could not see may be open ground to the room flood), or its
  felt-for floors ran out (:unseen-floors) above the goal by more than a fall the walk takes (drop-stranded?). Never out of
  the body's own shelter: it is shut in on purpose."
  [c tries result]
  (let [doors (some-> (:doors (:args c)) keyword)
        shut? (delay (escape/walk-enclosed? c))]
    (and (ways/escalating? c)
         (or (shelter-exit? c result) (not (sh-inside/sheltered-in c)))
         ;; the cheap checks first: drop-fill and air-pocket? flood the body's land
         (or (shelter-exit? c result)
             (some? (gap-fill c result (skips c)))
             (gap-walled? c result (skips c))
             (sprint-only? result)
             (dive? c result)
             (goal-walled? c result shut?)
             (damage-refused? result)
             (and (refused-drop? result) (not @shut?))
             (and (= :unseen-floors (some-> (:reason result) keyword))
                  (some-> (places/parse-pos (:pos (:args c))) :pos (as-> goal (drop-stranded? c (end/feet-cell c) goal result))))
             (some? (drop-fill c result (skips c) (end/feet-cell c)))
             (air-pocket? c result)
             (and (walker-fault? result) (>= tries (max-blocked))
                  (not (escape/door-beside? (:primitives c) (end/feet-cell c) doors)))
             (and (escalate-reason? result doors)
                  (or (#{:exhausted :start-enclosed} (some-> (:reason result) keyword))
                      @shut?))))))

(defn climb-on?
  "Whether a stair that is done leaves the body still shut in (escape/walk-enclosed?) under the goal pos: a walk could only
  lead back down the stair (live: a partial walk toward the goal's column took the body back under it), so the next
  escalation follows at once."
  [c pos]
  (and (< (second (end/feet-cell c)) (js/Math.floor (:y pos)))
       (escape/walk-enclosed? c)))

(defn dive-climb-on?
  "Whether a stair cut because the only way dives (memory :dive-climb) leaves the body still under ground below the
  goal pos: a walk would lead back down the stair toward the dive, so the next stair follows at once."
  [c pos]
  (boolean (and (:dive-climb (ctx/mem c))
                (< (second (end/feet-cell c)) (js/Math.floor (:y pos)))
                (sh-sense/buried? (:primitives c)))))

(defn cheaper-fill
  "The first of fills (gap-fill, drop-fill steps; nils skipped) when its price is no more than way's (own-stair or the
  chosen escape); way when it is no priced span (a leave-shelter, an approach, none) or dearer."
  [p feet way & fills]
  (let [fill (first (remove nil? fills))]
    (if (and fill (or (not (#{:stair :pillar :dig-route :bridge} (:step way))) (<= (escape-way/price p feet fill) (escape-way/price p feet way))))
      fill
      way)))
