(ns jobs.movement.go-to.escalate-choose
  "go-to's choice of a way out of a shut-in body, or in to a walled-in goal: whether a give-up may escalate, and which
  pillar, stair, clear-path, dig route or walk to the wall to take."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.breath :as breath]
            [jobs.lib.column-kinds :as kinds]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.holes :refer [may-dig-fn own-placed-fn]]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.tuning :refer [budget-near max-blocked max-route-s]]
            [jobs.movement.go-to.way-steps :as ways]))

(defn skips
  "The escalation steps this call does not take: those that failed (memory :skipped-steps) and those of the ways it
  does not allow."
  [c]
  (into (:skipped-steps (ctx/mem c) #{}) (ways/barred-steps c)))

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
  the caller also requires the body shut in (escape/enclosed?), and a shut-in body's search can end at a loaded edge
  beyond its walls (an unseen cell refused) instead of :start-enclosed."
  [result doors]
  (let [reason (some-> (:reason result) keyword)]
    (boolean (or (contains? escalate-reasons reason)
                 (= :goal-unloaded reason)
                 (and (= :abilities reason) (contains? #{:gap-low-ceiling :gap-up} (some-> (:kind result) keyword)))
                 (and (= :abilities reason) (= :never doors) (= :open (some-> (:kind result) keyword)))))))

(defn air-pocket?
  "Whether a give-up with result is a dry body's whose every way is a swim past its air (:air, :air-lethal) from a pocket it can walk out of
  only through water: it digs or builds out as a player would."
  [c result]
  (let [p (:primitives c)]
    (boolean (and (#{:air :air-lethal} (some-> (:reason result) keyword))
                  (not (breath/in-water? p))
                  (reach-room/dry-shut-in? p)))))

(defn dive?
  "Whether a give-up with result is a way that dives (:way-dives) that go-to may climb out of by a stair (c :dive-stair,
  the setting)."
  [c result]
  (boolean (and (:dive-stair c) (ways/way? c :stair) (= :way-dives (some-> (:reason result) keyword)))))

(defn shelter-exit?
  "Whether a give-up with result may leave the body's own shelter instead: the walk proved the way needs the world
  changed, the body is shut in its shelter (at any hour: the start check priced the way out)."
  [c result]
  (boolean (and (not (contains? (skips c) :leave-shelter))
                (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                (sh-inside/sheltered-in c))))

(defn fetch-tool?
  "Whether the escalation may fetch a pickaxe (the stair's and a :dig-route dig's ways): the body is free (a shut-in body
  cannot walk to one; by-hand is its fallback) and carries one pickaxe or none, so the last one breaking is no stop."
  ([c] (fetch-tool? (:primitives c) (:doors (:args c))))
  ([p doors]
   (and (not (escape/enclosed? p (some-> doors keyword)))
        (<= (count (filter #(str/ends-with? (str (:name %)) "_pickaxe") (u/inventory p))) 1))))


(defn gap-fill
  "{:step :fill-gap :cell :item} when result (a planner no-path) is a gap jump refused for a low ceiling or a gap jump
  up, a carried pillar block can fill the gap's floor (at the takeoff's height: a step up is then walkable), and no zone
  refuses the place; else nil."
  [c result skip]
  (let [p (:primitives c)
        {:keys [item]} (escape/pillar-item c)
        cell (when (and (= :abilities (some-> (:reason result) keyword)) (#{:gap-low-ceiling :gap-up} (some-> (:kind result) keyword))
                        (not (skip :fill-gap)) item (:at result) (:from result))
               (escape/gap-floor (assoc (vec (:at result)) 1 (second (:from result))) (:from result)))]
    (when (and cell (or (:ignore-zones? (:args c)) (nil? (tidy/refusal c :place (zipmap [:x :y :z] cell)))))
      {:step :fill-gap :cell cell :item item})))

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

(defn goal-walled?
  "Whether a give-up with result is a free body's (not escape/enclosed?) whose goal the planner proved walled in, or whose
  search spent its node budget (:budget) with the body within budget-near of the goal (:goal-left): a goal that close
  with no whole way found is walled off, whatever the flood could not prove. The ways that change the world run toward
  the goal (goal-way)."
  [c result]
  (boolean
   (and (or (= :goal-enclosed (some-> (:reason result) keyword))
           (and (= :budget (some-> (:reason result) keyword)) (some-> (:goal-left result) (<= (budget-near)))))
       (not (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword))))))

(defn goal-lava?
  "Whether the goal cell [x y z] is refused outright, whatever is dug: it is lava, it stands over lava, or lava lies
  beside it on all four sides (at its level or the floor's). Cells never seen read as not lava."
  [p [x y z]]
  (let [lava? (fn [dx dy dz] (some-> (u/seen-block p {:x (+ x dx) :y (+ y dy) :z (+ z dz)}) .-name (= "lava")))]
    (boolean
     (or (lava? 0 0 0) (lava? 0 -1 0)
         (every? (fn [[dx dz]] (or (lava? dx 0 dz) (lava? dx -1 dz))) [[1 0] [-1 0] [0 1] [0 -1]])))))

(defn route-s
  "Seconds digging cells ([x y z]) takes with the quickest tool carried (with a wooden pickaxe too when fetch?), blocks
  own? calls the body's own not counted."
  [p cells own? fetch?]
  (let [block-at (escape/block-at-of p)]
    (transduce (comp (remove #(and own? (own? % (block-at %))))
                     (map #(/ (cond-> (tools/clear-ms p (block-at %))
                                fetch? (min (js/Number (.clearTime p (block-at %) "wooden_pickaxe"))))
                              1000)))
               + 0 cells)))

(defn route-cells
  "[{:cell :block :guess}]: the cheapest cells to dig for a walk from feet cell from down or across into goal
  (jobs.lib.dig-route priced by jobs.lib.cost: a block's walk and dig-cost; at most the dig-route settings' digs and fall),
  in walking order, every one escape/diggable?, :guess on a cell not seen (read as rock); [] when nothing is left to
  dig, nil when no such route or when its digging takes the body longer than max-route-s (blocks of the body's own, a scaffold it left, not counted), with what it carries, or with
  a wooden pickaxe when fetch? (the dig fetches one, fetch-tool?), else bare-handed (a player does not punch a tunnel through rock). A floor never seen under a cell seen open may be a void: dig-route reads it as no floor,
  unless floors-unseen-ok? (a pocket's floor shows only from inside it)."
  ([p from goal may-dig? own?] (route-cells p from goal may-dig? own? false false))
  ([p from goal may-dig? own? floors-unseen-ok?] (route-cells p from goal may-dig? own? floors-unseen-ok? false))
  ([p from goal may-dig? own? floors-unseen-ok? fetch?] (route-cells p from goal may-dig? own? floors-unseen-ok? fetch? nil))
  ([p from goal may-dig? own? floors-unseen-ok? fetch? route-opts]
  (let [block-at (escape/block-at-of p)
        seen-floor (fn [[x y z :as cell]]
                     (when-not (and (not floors-unseen-ok?) (dig-look/unknown? p cell) (dig-route/open? (block-at [x (inc y) z]))
                                    (not (dig-look/unknown? p [x (inc y) z])))
                       (block-at cell)))
        ;; dig-cost merges the whole cost basis a call, and a search asks it of every cell
        dig-cost (memoize cost/dig-cost)
        cells (:dig (dig-route/route seen-floor from goal (merge {:dig-s (fn [name cell] (when (escape/diggable? block-at may-dig? own? cell) (dig-cost name)))
                                                                  :step-s (:cost (cost/walk-cost {:distance 1}))}
                                                                 route-opts)))]
    (when (and cells (every? #(escape/diggable? block-at may-dig? own? %) cells)
               (<= (route-s p cells own? fetch?) (max-route-s)))
      (mapv (fn [cell] {:cell cell :block (block-at cell) :guess (dig-look/unknown? p cell)}) cells)))))

(defn dig-way
  "{:step :dig-route :cells (route-cells)} when there are cells to dig, else nil."
  [p from goal may-dig? {:keys [own? skip fetch?]}]
  (when-not (skip :dig-route)
    (when-let [cells (seq (route-cells p from goal may-dig? own? false fetch?))]
      {:step :dig-route :cells (vec cells)})))

(def leg-blocks
  "The most blocks along the straight line to the goal one leg of a long dig (dig-leg) reaches: its route stays within
  the dig-route settings' digs."
  6)

(def leg-margin
  "Blocks round its two ends a leg's route (dig-leg) may stray: a leg runs along the line, and every cell the route
  search weighs is a zone and tool check."
  1)

(defn leg-ends
  "The feet cells on the straight line from feet toward goal (cells [x y z]) leg-blocks and half that on, farthest
  first; none when the goal is that near."
  [feet goal]
  (let [d (js/Math.sqrt (reduce + (map #(* (- %2 %1) (- %2 %1)) feet goal)))]
    (when (> d leg-blocks)
      (distinct (for [i [leg-blocks (quot leg-blocks 2)]]
                  (mapv #(js/Math.floor (+ %1 0.5 (* (/ i d) (- %2 %1)))) feet goal))))))

(defn dig-leg
  "A buried body's next leg of a dig toward a goal no one dig route reaches (dig-way refused it as too long): {:step
  :dig-route :cells :leg-s} to the farthest of leg-ends that route-cells reaches, :leg-s its dig seconds; nil when none
  does; {:step :none :why :dig-budget} when the call's legs so far (memory :leg-dig-s) and this one would dig longer than
  the caller's :worth, else the call's job time left."
  [c from goal may-dig? {:keys [own? skip fetch?]}]
  (when-not (skip :dig-route)
    (let [p (:primitives c)
          budget (or (:worth (:args c)) (get-in (ctx/left c) [:spend :job-time] ##Inf))]
      (when-let [[cells s] (some (fn [end]
                                   (when-let [cells (seq (route-cells p from end may-dig? own? false fetch? {:margin leg-margin}))]
                                     [(vec cells) (route-s p (map :cell cells) own? fetch?)]))
                                 (leg-ends from goal))]
        (if (<= (+ (:leg-dig-s (ctx/mem c) 0) s) budget)
          {:step :dig-route :cells cells :leg-s s}
          {:step :none :why :dig-budget :dug-s (:leg-dig-s (ctx/mem c) 0) :budget-s budget})))))

(defn nearest-stand
  "A cell [x y z] the body can walk to (a flood of at most reach-room/room-cells cells) nearest goal: of the cells no farther
  than slack (default 0) beyond the nearest, in the whole distance and in the distance along the ground, the first the
  flood reaches (the shortest walk)."
  ([p goal] (nearest-stand p goal 0))
  ([p goal slack]
   (let [kind-at (reach/lookup p reach-room/body-kind-of)
         to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
         across #(u/flat-dist {:x (% 0) :z (% 2)} {:x (goal 0) :z (goal 2)})
         cells (->> (:cells (reach-room/flood-visit #(reach/forward kind-at %1 %2 %3) (reach/standing-vec p) reach-room/room-cells))
                    (map vec))
         best (reduce (fn [a b] (if (< (to-goal b) (to-goal a)) b a)) cells)]
     (first (filter #(and (<= (to-goal %) (+ (to-goal best) slack)) (<= (across %) (+ (across best) slack))) cells)))))

(defn goal-way
  "A free body's way into a walled-in goal: escape/choose from where it stands, or, while the cell it can walk to
  nearest the goal (nearest-stand) is more than a block nearer, from there, as
  {:step :approach :pos cell} first. Only a door, or a stair or pillar up to a higher goal, leads in (a walk along
  the wall too, from where it stands, to a goal not below it), else the fewest cells dug (dig-way); else {:step :none :why :no-dig}."
  [c feet goal may-dig? opts]
  (let [p (:primitives c)
        to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
        near (nearest-stand p goal)
        here? (or ((:skip opts) :approach) (>= (to-goal near) (dec (to-goal feet))))
        from (if here? feet near)
        e (escape/choose p from goal may-dig? opts)
        in? (case (:step e)
              :clear-path true
              (:stair :pillar) (> (second goal) (second from))
              :approach (and here? (>= (second goal) (second from)))
              false)
        e (cond in? e
                :else (or (dig-way p from goal may-dig? opts)
                          (if (= :none (:step e)) e {:step :none :why :no-dig})))]
    (cond
      (= :none (:step e)) e
      here? e
      :else {:step :approach :pos near})))

(defn detour-way
  "A buried body's way to a goal its only found way reaches by a long detour (walk.cljs :detour): the cheapest cells
  dug from the feet into goal pos (dig-way), else a stair when the goal is above; nil when the call may not escalate,
  tried it already (memory :detour-tried) or neither is there."
  [c pos]
  (when (and (ways/escalating? c) (not (:detour-tried (ctx/mem c))))
    (let [p (:primitives c)
          feet (end/feet-cell c)
          goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
          opts {:own? (own-placed-fn c) :skip (skips c) :fetch? (fetch-tool? c) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c)}
          may-dig? (may-dig-fn c)]
      (or (dig-way p feet goal may-dig? opts)
          (when (> (second goal) (second feet))
            (let [e (escape/choose p feet goal may-dig? opts)]
              (when (= :stair (:step e)) e)))))))

(def stair-step-s
  "Seconds one step of a stair up is reckoned to take (two cuts and a step), against a found plan's seconds."
  3)

(def stair-first-rise
  "How far above the feet a goal is for a found plan to be weighed against a stair up."
  8)

(defn stair-step-s-here
  "stair-step-s, dearer by the dark factor where the body stands dark: the plan's seconds carry its dark surcharge, and the
  stair is cut in that dark too."
  [c]
  (cond-> stair-step-s
    (and (wworld/dark?) (look/dark-here? (:primitives c))) (* (+ 1 (cost/dark-factor (wworld/kit-of c))))))

(defn stair-check
  "A fn of a plan and whether it is the first of its round, saying why it is not walked and a stair up is cut instead (a
  re-plan inside a walk is held to :dives only), when escalating is on, the body is not in its shelter and escape/choose would stair (steps n):
  - :dives: under a cave floor (c :cave-floor, with c :dive-stair) the plan goes below it on a cell that is no stair of
    the body's own (climb/dives?), or below c :own-floor back down a stair of its own (climb/returns?);
  - :cheaper: it is a whole path costing more seconds than 3 such stairs (a stair cut in the dark costs as much more as the dark walk does), the goal is stair-first-rise or more above
    the feet and the stair has not been tried this call.
  - :retreats: it is a partial plan (the way on is shut), a re-plan too, that goes down the body's own stair, for a goal no lower than
    the feet, and escape/choose has a way that changes the world: the stair was cut to get up, not to go back down.
  Else nil."
  [c pos]
  (let [m (ctx/mem c)
        feet (end/feet-cell c)
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        cave (when (:dive-stair c) (:cave-floor c))
        back (:own-floor c)
        cheaper? (and (not (:stair-tried m)) (>= (- (second goal) (second feet)) stair-first-rise))
        retreats? (>= (second goal) (second feet))
        chosen (delay (escape/choose (:primitives c) feet goal (may-dig-fn c) {:own? (own-placed-fn c) :skip (skips c) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c)}))
        stair (delay (when (= :stair (:step @chosen)) @chosen))
        way (delay (not (#{:none :approach} (:step @chosen))))]
    (when (and (ways/way? c :stair) (not (sh-inside/sheltered-in c)) (or cave cheaper? back retreats?))
      (fn [{:keys [status steps ^js r]} first?]
        (let [seconds (some-> r .-path .-cost .-priced)]
          (cond
            (and cave (climb/dives? steps cave (ledger/ways-of c)) @stair) :dives
            (and back (climb/returns? steps back (ledger/stair-stands c)) @stair) :dives
            (and retreats? (= "partial" status) (climb/returns? steps (second feet) (ledger/stair-stands c)) @way) :retreats
            (and first? cheaper? (= "found" status) (number? seconds) @stair (> seconds (* 3 (stair-step-s-here c) (:steps @stair)))) :cheaper))))))

(defn escalate?
  "Whether a give-up after tries fruitless walks with result may escalate instead: a carried block fills the 1-cell gap it was refused (gap-fill), its only way dives (dive?), the goal is walled in (goal-walled?), or the walker failed max-blocked walks running (walker-fault?; one stuck walk of a :tries 1 caller is no proof) and no door or gate borders where it can walk
  (escape/door-beside?: that is its way out; an iron door, or any door under :doors :never, is a wall), or the planner's
  reason proves the way needs the world changed and the body is shut in (reach-room/enclosed?, which already counts doors
  as opened: a door that leads nowhere, e.g. onto a step under its lintel, is no way out), or the search ran out of
  land (:exhausted, after the walks that looked at every edge) in a mine or cave larger than a room. Never out of
  the body's own shelter: it is shut in on purpose."
  [c tries result]
  (and (ways/escalating? c)
       (or (shelter-exit? c result) (not (sh-inside/sheltered-in c)))
       (or (shelter-exit? c result)
           (some? (gap-fill c result (skips c)))
           (sprint-only? result)
           (dive? c result)
           (air-pocket? c result)
           (goal-walled? c result)
           (let [doors (some-> (:doors (:args c)) keyword)]
             (or (and (walker-fault? result) (>= tries (max-blocked))
                      (not (escape/door-beside? (:primitives c) (end/feet-cell c) doors)))
                 (and (escalate-reason? result doors)
                      (or (= :exhausted (some-> (:reason result) keyword))
                          (escape/enclosed? (:primitives c) doors))))))))

(defn climb-on?
  "Whether a stair that is done leaves the body still shut in (escape/enclosed?) under the goal pos: a walk could only
  lead back down the stair (live: a partial walk toward the goal's column took the body back under it), so the next
  escalation follows at once."
  [c pos]
  (and (< (second (end/feet-cell c)) (js/Math.floor (:y pos)))
       (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword))))

(defn dive-climb-on?
  "Whether a stair cut because the only way dives (memory :dive-climb) leaves the body still under ground below the
  goal pos: a walk would lead back down the stair toward the dive, so the next stair follows at once."
  [c pos]
  (boolean (and (:dive-climb (ctx/mem c))
                (< (second (end/feet-cell c)) (js/Math.floor (:y pos)))
                (sh-sense/buried? (:primitives c)))))
