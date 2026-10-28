(ns jobs.movement.go-to.escalate-choose
  "go-to's choice of a way out of a shut-in body, or in to a walled-in goal: whether a give-up may escalate, and which
  pillar, stair, clear-path, dig route or walk to the wall to take."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.choose :as choose]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.access :as access]
            [jobs.lib.aim :as aim]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.per-body :as per-body]
            [jobs.lib.places :as places]
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
            [jobs.movement.go-to.aim :as goal-aim]
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
  the caller also requires the body shut in (escape/walk-enclosed?), and a shut-in body's search can end at a loaded edge
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
                  (reach-room/dry-shut-in? p :ground)))))

(defn dive?
  "Whether a give-up with result is a way that dives (:way-dives) that go-to may climb out of by a stair (c :dive-stair,
  the setting)."
  [c result]
  (boolean (and (:dive-stair c) (ways/way? c :stair) (= :way-dives (some-> (:reason result) keyword)))))

(defn shelter-exit?
  "Whether a give-up with result may leave the body's own shelter instead: the walk proved the way needs the world
  changed and the body is shut in its shelter. Not while it is :dug-in at night (sh-inside/night-sheltered, the verdict
  the readiness checks wait for light on): the night job would shut the hole again."
  [c result]
  (let [p (:primitives c)]
    (boolean (and (not (contains? (skips c) :leave-shelter))
                  (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                  (sh-inside/sheltered-in c)
                  (not= :dug-in (sh-inside/night-sheltered p (:data (ctx/latest c :shelter))))))))

(defn fetch-tool?
  "Whether the escalation may fetch a pickaxe (the stair's and a :dig-route dig's ways): the body is free (a shut-in body
  cannot walk to one; by-hand is its fallback) and carries one pickaxe or none, so the last one breaking is no stop."
  ([c] (fetch-tool? (:primitives c) nil (delay (escape/walk-enclosed? c))))
  ([p _ shut?]
   (and (<= (count (filter #(str/ends-with? (str (:name %)) "_pickaxe") (u/inventory p))) 1)
        (not @shut?))))


(defn carried-build-n
  "How many blocks of the stair's floor fillers (choose/fillers :build) the body carries."
  [p]
  (let [names (set (choose/fillers :build))]
    (transduce (comp (filter #(names (:name %))) (map :count)) + 0 (u/inventory p))))

(defn stair-route-s
  "Seconds the stair way e takes from feet, each step as jobs.lib.dig-route (route-answer) prices one: jobs.lib.cost/stair-leg-pricer
  of its cuts (dig-cost), a fill for an open floor (a fetch of the blocks, with none carried) and an unseen cut or floor."
  [p feet {:keys [heading steps]}]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        leg-s (cost/stair-leg-pricer (wworld/unseen-share p))
        fill-s (cw/fill-cost (carried-build-n p))
        cut-s (fn [cell] (let [n (block-at cell)]
                           (cond (dig-route/open? n) nil
                                 (nil? (dig-route/cell-cost n)) ##Inf
                                 :else (or (cost/dig-cost (or n "stone")) ##Inf))))]
    (transduce (map (fn [cuts] (let [floor (escape/add (nth cuts 2) [0 -1 0])]
                                 (leg-s (map cut-s cuts) (when (dig-route/open? (block-at floor)) fill-s) (some unseen? (cons floor cuts))))))
               + 0 (partition 3 (escape/stair-cuts feet (escape/heading-dirs (keyword heading)) steps)))))

(defn new-stair-step-s
  "Seconds one step of a stair not yet laid out (no heading chosen) takes: its three cuts priced as stone, and its cells
  unseen (the worse case)."
  [p]
  ((cost/stair-leg-pricer (wworld/unseen-share p)) (repeat 3 (or (cost/dig-cost "stone") 0)) nil true))

(defn price
  "Seconds way e from feet is reckoned to take (jobs.lib.cost): a stair, a pillar, a bridge or a fill by its blocks, a dug door or
  route by what its cells take to dig with the tool carried, plus a step a cell; 0 for a step that is no span."
  [p feet {:keys [step] :as e}]
  (let [block-at (delay (escape/block-at-of p))
        dig-s (fn [cells] (cost/dig-cells-s (map #(tools/clear-ms p (@block-at %)) cells)))]
    (case step
      :stair (+ (stair-route-s p feet e) (* (:aside e 0) (or (cost/dig-cost "stone") 1)))
      :pillar (cost/pillar-s (wworld/unseen-share p) (:height e) (escape/unseen-pillar-blocks p feet (:height e)))
      :bridge (cw/bridge-cost (:length e) (:short e 0))
      :fill-gap (cw/fill-cost (:count e 1))
      :dig-route (dig-s (map :cell (:cells e)))
      :clear-path (dig-s (:cells (escape/door @block-at (escape/unseen-of p) feet (escape/heading-dirs (:heading e)) (escape/max-door))))
      0)))

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

(defn drop-fill
  "{:step :fill-gap :cell :item} when result is :goal-cut-off or :goal-enclosed (the goal's region ends at a drop the body cannot climb back
  from), a carried block can fill the floor of the cell beyond the drop edge nearest the body, and no zone refuses the
  place: the bridge a player lays over the hole before a door; else nil."
  [c result skip feet]
  (when (and (contains? #{:goal-cut-off :goal-enclosed} (some-> (:reason result) keyword)) (not (skip :fill-gap)))
    (let [p (:primitives c)
          {:keys [item] carried :count} (escape/pillar-item c)
          goal (some-> (places/parse-pos (:pos (:args c))) :pos)
          kind-at (reach/seen-lookup p reach-room/body-kind-of)
          starts (when goal (filter (fn [[x y z]] (reach/standable-cell? p {:x x :y y :z z} kind-at))
                                    (for [dy [0 1] [dx dz] [[0 0] [1 0] [-1 0] [0 1] [0 -1]]]
                                      [(+ (:x goal) dx) (+ (:y goal) dy) (+ (:z goal) dz)])))
          cell (when (and item (seq starts)) (first (drop-edges p starts feet)))]
      (when (and cell (or (:ignore-zones? (:args c)) (nil? (tidy/refusal c :place (zipmap [:x :y :z] cell)))))
        {:step :fill-gap :cell cell :item item :count carried}))))

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
  "Whether a give-up with result is a free body's (not escape/walk-enclosed?) whose goal the planner proved walled in, or whose
  search spent its node budget (:budget) with the body within budget-near of the goal (:goal-left): a goal that close
  with no whole way found is walled off, whatever the flood could not prove. The ways that change the world run toward
  the goal (goal-way). shut?: a delay of escape/walk-enclosed?, when the caller has one."
  ([c result] (goal-walled? c result (delay (escape/walk-enclosed? c))))
  ([c result shut?]
   (boolean
    (and (or (= :goal-enclosed (some-> (:reason result) keyword))
             (and (= :budget (some-> (:reason result) keyword)) (some-> (:goal-left result) (<= (budget-near)))))
         (not @shut?)))))

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
                   (escape/diggable? block-at may-dig? own? riser)
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

(def route-searches
  "The most route searches route-cells makes round cells may-dig? refuses."
  3)

(defonce ^{:doc "Per go-to call (route-owner): {key search-fn} of the dig-route searches its last escalate! left unfinished
  (jobs.lib.dig-route/search), gone on by later ones (route-slice)."}
  kept-routes (atom {}))

(per-body/store! ::kept-routes #(reset! kept-routes {}))

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

(defn sliced-answer
  "The answer of the dig-route search under key k in slice (start begins it: a dig-route/search), run on with what the
  slice has left; :searching while it has not ended."
  [{:keys [id left asked]} k start]
  (if-let [a (get @asked k)]
    a
    (let [run (or (get-in @kept-routes [id k]) (start))
          [a used] (if (pos? @left) (run @left) [nil 0])]
      (vswap! left - used)
      (swap! kept-routes assoc-in [id k] run)
      (vswap! asked assoc k (or a :searching))
      (or a :searching))))

(def flood-cells-per-node
  "Flood cells that cost a slice as much as one search node expanded (measured in the fake world: a node about 0.2 ms, a flood cell 0.02 ms)."
  8)

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
  "Go on with the call id's searches kept unfinished (kept-routes) within one slice: whether one still has not ended.
  The ones that end stay kept, for the next escalate!'s choice."
  [id]
  (let [slice (route-slice id)]
    (doseq [[k _] (get @kept-routes id)] (sliced-answer slice k nil))
    (searching? slice)))

(defn searching-ends
  "The [from goal] of the call id's kept searches that have not ended."
  [id]
  (into #{} (keep (fn [[k run]] (when-not (first (run 0)) (vec (take 2 k))))) (get @kept-routes id)))

(defn forget-routes!
  "Drop the kept searches of go-to call id: it has ended."
  [id]
  (swap! kept-routes dissoc id))

(defn beside-undiggable?
  "Whether a seen face-neighbour of cell is solid rock an escalation never digs (escape/diggable? over own?): such rock
  comes in veins and rooms, so a guess beside it is at risk. sensed: cell -> the sensed block or nil."
  [sensed block-at own? [x y z]]
  (boolean (some (fn [[dx dy dz]] (let [at [(+ x dx) (+ y dy) (+ z dz)] b (sensed at)]
                                    (and b (not (true? (.-unknown b))) (escape/solid? (.-name b))
                                         (not (escape/diggable? block-at (constantly true) own? at)))))
                 [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])))

(defn route-answer
  "The dig-route answer {:dig :steps} of the cheapest way from feet cell from into goal (jobs.lib.dig-route priced by
  jobs.lib.cost: a step's walk-cost, a fall's hp (fall-damage at hp-cost, the falls the body survives), a block's dig-cost, a stair's step at least its stair-cost, a step's unseen cells at the unseen walk's price
  (wworld/unseen-share), a cell of the body's own builds at its put-back too (route-opts :own-s, holes/own-cut-fn), never a floor of the body's own stairs (route-opts :ways)), every cell to dig escape/diggable?
  and may-dig?; :searching while route-opts :slice's search goes on; nil when there is none, when route-opts :spent
  holds [from goal] (a search search-bound dropped), or when its digging takes the
  body longer than max-route-s (blocks of the body's own, a scaffold it left, not counted) with what it carries, or with
  a wooden pickaxe when fetch? (the dig fetches one, fetch-tool?), else bare-handed (a player does not punch a tunnel
  through rock). A floor never seen under a cell seen open may be a void: dig-route reads it as no floor, unless
  floors-unseen-ok? (a pocket's floor shows only from inside it). Other route-opts go to the search (:place-s,
  :pillar-s, :margin). With :slice (route-slice) a search that spends it goes on at the next call; without, it ends
  at the node budget as no route. With :range (and :near, the cell it is measured from) the route ends on any stand
  within it (with :see, one whose eye sees that block, jobs.lib.aim) and never digs :near. A route is dug again till
  it holds: priced over the chance each guess on it is diggable (1 - its class's risk, route-opts :guess-risk
  {:beside :apart}: beside-undiggable? or not), it is none over :worth-s seconds."
  [p from goal may-dig? own? floors-unseen-ok? fetch? {:keys [slice spent] :as route-opts}]
  (let [own-s (:own-s route-opts)
        {:keys [beside apart] :or {beside 0 apart 0}} (:guess-risk route-opts)
        worth-s (:worth-s route-opts)
        route-opts (dissoc route-opts :slice :spent :own-s :guess-risk :worth-s)
        [fx _ fz] from
        ;; each cell is read for its floor, its name to dig and whether it is unseen: sensed once
        known (js/Map.)
        sensed (fn [x y z] (let [k (+ (* (+ (- x fx) 1048576) 2147483648) (* (+ (- z fz) 1048576) 1024) y 512)]
                             (if (.has known k) (.get known k) (let [b (u/sensed p {:x x :y y :z z})] (.set known k b) b))))
        block-at (fn [[x y z]] (when-let [b (sensed x y z)] (if (true? (.-unknown b)) access/hidden-guess (.-name b))))
        seen-floor (fn [[x y z]]
                     (when-let [b (sensed x y z)]
                       (if-not (true? (.-unknown b))
                         (.-name b)
                         (let [a (when-not floors-unseen-ok? (sensed x (inc y) z))]
                           (when-not (and a (not (true? (.-unknown a))) (dig-route/open? (.-name a)))
                             access/hidden-guess)))))
        prices (dig-route/body-prices p (fn [[x y z]] (true? (some-> (sensed x y z) .-unknown))))
        risk (fn [[x y z :as cell]]
               (cond (not (true? (some-> (sensed x y z) .-unknown))) 0
                     (= beside apart) beside
                     (beside-undiggable? (fn [[x y z]] (sensed x y z)) block-at own? cell) beside
                     :else apart))
        opts (fn [refused] (merge prices
                                  {:dig-s (fn [name cell] (when (and (not (refused cell)) (escape/diggable? block-at (constantly true) own? cell))
                                                            (some-> ((:dig-s prices) name cell) (+ (if own-s (own-s cell) 0)))))
                                   :max-dig-s (max-route-s)}
                                  route-opts
                                  (when-let [cell (:see route-opts)] {:sees (aim/stand-sees-fn p cell)})))
        ;; a route is dug again till it holds: its price over the chance every guess on it is diggable
        expected-s (fn [a] (/ (:seconds a) (reduce * 1 (map #(- 1 (risk %)) (:dig a)))))
        ;; the search itself reads no may-dig?: escalate!'s choices with other zone rules share it
        search (fn [refused]
                 (if slice
                   (sliced-answer slice [from goal floors-unseen-ok? route-opts refused] #(dig-route/search seen-floor from goal (opts refused)))
                   (dig-route/route seen-floor from goal (opts refused))))
        ;; may-dig? (zones, claims, plans) is asked of a found route's cells only, not of every cell the search weighs: it was
        ;; about half a search's time; a refused cell is searched round, at most route-searches times
        a (when-not (contains? spent [from goal])
            ;; with a :range the goal is worked from a stand within it, never dug away
            (loop [refused (if (>= (:range route-opts 0) 1) #{(:near route-opts goal)} #{}) n 1]
            (let [a (search refused)
                  bad (when (map? a) (remove #(escape/diggable? block-at may-dig? own? %) (:dig a)))]
              (cond (= :searching a) a
                    (nil? (:dig a)) nil
                    (empty? bad) a
                    (< n route-searches) (recur (into refused bad) (inc n))))))]
    (when (or (= :searching a) (and a (<= (route-s p (:dig a) own? fetch?) (max-route-s)) (<= (expected-s a) (or worth-s ##Inf))))
      a)))

(defn route-cells
  "[{:cell :block :guess :beside}]: the cells to dig of route-answer's way, in walking order, :guess on a cell not seen (read as
  rock), :beside on a guess beside-undiggable?; [] when nothing is left to dig; :searching or nil as route-answer."
  ([p from goal may-dig? own?] (route-cells p from goal may-dig? own? false false))
  ([p from goal may-dig? own? floors-unseen-ok?] (route-cells p from goal may-dig? own? floors-unseen-ok? false))
  ([p from goal may-dig? own? floors-unseen-ok? fetch?] (route-cells p from goal may-dig? own? floors-unseen-ok? fetch? nil))
  ([p from goal may-dig? own? floors-unseen-ok? fetch? route-opts]
   (let [a (route-answer p from goal may-dig? own? floors-unseen-ok? fetch? route-opts)
         block-at (escape/block-at-of p)]
     (cond
       (= :searching a) a
       a (let [sensed #(u/sensed p (zipmap [:x :y :z] %))]
           (mapv (fn [cell] (let [guess (dig-look/unknown? p cell)]
                              (cond-> {:cell cell :block (block-at cell) :guess guess}
                                (and guess (beside-undiggable? sensed block-at own? cell)) (assoc :beside true))))
                 (:dig a)))))))

(def dig-margin
  "Blocks round its two ends a dig route (dig-way, frontier-way, a climb's) may stray. Not a price of guessed cells (unseen-step-s
  prices those): it bounds the flood, which with no route spreads over about 270k cells of rock within max-route-s.
  search-bound's stand-still bound alone (the whole way without a route, ~100 s on a far goal) is too loose for that."
  3)

(defn search-opts
  "route-answer's route-opts for a choice's opts: its :slice, dig-margin, its :spent searches, its :own-s and :own-cut
  (a way that may cut the body's own builds: searched apart), :guess-risk ({:beside :apart} the chance a guess beside
  seen undiggable rock or apart from it fails) and :worth-s (a route costing more is none), its :range, :near and :see and the floors under its :stands
  (the body's own stairs) as :ways."
  [{:keys [stands slice spent own-s own-cut range near see guess-risk worth-s]}]
  (cond-> {:ways (into #{} (map (fn [[x y z]] [x (dec y) z])) stands) :margin dig-margin}
    slice (assoc :slice slice) (seq spent) (assoc :spent spent) own-s (assoc :own-s own-s) own-cut (assoc :own-cut true)
    (and range near) (assoc :range range :near near) see (assoc :see see)
    (some pos? (vals guess-risk)) (assoc :guess-risk guess-risk :worth-s worth-s)))

(defn range-opts
  "Where the go-to call c's dig routes to pos may end: :range, :near (pos's cell) and under :see :see, the block the end stand must see."
  [c pos]
  (cond-> {:range (:range (:args c) 1) :near (mapv #(js/Math.floor (% pos)) [:x :y :z])}
    (goal-aim/see-cell c) (assoc :see (goal-aim/see-cell c))))

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
  "Blocks round its two ends a leg's route (dig-leg) may stray: a leg runs along the line, and every cell the route
  search weighs is a zone and tool check."
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

(defn call-budget-s
  "Seconds the go-to call c may spend on a way: the caller's :worth, else the call's job time left."
  [c]
  (or (:worth (:args c)) (get-in (ctx/left c) [:spend :job-time] ##Inf)))

(defn dig-leg
  "A buried body's next leg of a dig toward a goal: {:step
  :dig-route :cells :leg-s} to the farthest of leg-ends that route-cells reaches, :leg-s its dig seconds; nil when none
  does; {:step :searching} while opts :slice's search goes on; {:step :none :why :dig-budget} when the call's legs so far
  (memory :leg-dig-s) and this one would dig longer than the caller's :worth, else the call's job time left."
  [c from goal may-dig? {:keys [own? skip fetch? slice] :as opts}]
  (when-not (skip :dig-route)
    (let [p (:primitives c)
          budget (call-budget-s c)
          found (loop [ends (leg-ends from goal)]
                  (when-let [end (first ends)]
                    (let [cells (route-cells p from end may-dig? own? false fetch? (assoc (search-opts opts) :margin leg-margin))]
                      (cond (= :searching cells) :searching
                            (seq cells) [(vec cells) (route-s p (map :cell cells) own? fetch?)]
                            :else (recur (rest ends))))))]
      (if (= :searching found)
        {:step :searching}
        (when-let [[cells s] found]
          (if (<= (+ (:leg-dig-s (ctx/mem c) 0) s) budget)
            {:step :dig-route :cells cells :leg-s s}
            {:step :none :why :dig-budget :dug-s (:leg-dig-s (ctx/mem c) 0) :budget-s budget}))))))

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
     (some-> slice (charge! (.-length cells) flood-cells-per-node))
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
          _ (some-> slice (charge! (.-length cells) flood-cells-per-node))
          to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
          nearer? #(< (to-goal %) (dec (to-goal feet)))
          stand? (fn [[x y z]] (reach/standable-cell? p {:x x :y y :z z} kind-at))
          off (fn [[x y z]] (for [[dx dz] escape/cardinals
                                  :when (not-any? #(land [(+ x dx) % (+ z dz)]) [(dec y) y (inc y)])]
                              [(+ x dx) y (+ z dz)]))
          below (fn [[x y z]] (some #(let [c [x % z]] (when (stand? c) c)) (range (inc y) (dec places/min-y) -1)))
          stands (->> land (mapcat off) (keep below) (remove land) distinct (filter nearer?) (sort-by to-goal))
          edge (delay (->> land (filter #(seq (off %))) (remove #(contains? approached %)) (filter nearer?) (sort-by to-goal) first))]
      (or (->> stands
               (take route-searches)
               (some #(let [cells (route-cells p feet % may-dig? own? false fetch? (search-opts opts))]
                        (cond (= :searching cells) {:step :searching}
                              (seq cells) {:step :dig-route :cells (vec cells)}))))
          (some->> @edge (hash-map :step :approach :pos))))))

(defn goal-way
  "A free body's way into a walled-in goal: escape/choose from where it stands, or, while the cell it can walk to
  nearest the goal (nearest-stand) is more than a block nearer, from there, as
  {:step :approach :pos cell} first. Only a door, or a stair or pillar up to a higher goal, leads in (a walk along
  the wall too, from where it stands, to a goal not below it), else the fewest cells dug (dig-way, :searching while its
  search goes on); else {:step :none :why :no-dig}, from the nearest cell once it has been walked to."
  [c feet goal may-dig? opts]
  (let [p (:primitives c)
        to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
        near (nearest-stand p goal 0 (:slice opts))
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
      (= :searching (:step e)) e
      here? e
      ;; from where it stands it may misread the land between (rock it has not seen): it walks nearer and looks
      (and (= :none (:step e)) (contains? (:approached opts) near)) e
      :else {:step :approach :pos near})))

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
          opts (merge {:own? (own-placed-fn c) :skip (skips c) :fetch? (fetch-tool? c) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c) :stands (ledger/stair-stands c) :slice slice}
                      (range-opts c pos))
          may-dig? (may-dig-fn c)
          e (dig-way p feet goal may-dig? opts)]
      (keep-routes! slice)
      (or e
          (when (> (second goal) (second feet))
            (let [e (escape/choose p feet goal may-dig? opts)]
              (when (= :stair (:step e)) e)))))))

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
  re-plan inside a walk is held to :dives only), when escalating is on, the body is not in its shelter and escape/choose would stair (steps n):
  - :dives: under a cave floor (c :cave-floor, with c :dive-stair) the plan goes below it on a cell that is no stair of
    the body's own (climb/dives?), or below c :own-floor back down a stair of its own (climb/returns?);
  - :cheaper: it is a whole path costing more seconds than cutting a new stair up to the goal's height (new-stair-step-s a step; a stair cut in the dark costs as much more as the dark walk does), the goal is stair-first-rise or more above
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
        chosen (delay (escape/choose (:primitives c) feet goal (may-dig-fn c) {:own? (own-placed-fn c) :skip (skips c) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c) :stands (ledger/stair-stands c)}))
        stair (delay (when (= :stair (:step @chosen)) @chosen))
        way (delay (not (#{:none :approach} (:step @chosen))))]
    (when (and (ways/way? c :stair) (not (sh-inside/sheltered-in c)) (or cave cheaper? back retreats?))
      (fn [{:keys [status steps ^js r]} first?]
        (let [seconds (some-> r .-path .-cost .-priced)]
          (cond
            (and cave (climb/dives? steps cave (ledger/ways-of c)) @stair) :dives
            (and back (climb/returns? steps back (ledger/stair-stands c)) @stair) :dives
            (and retreats? (= "partial" status) (climb/returns? steps (second feet) (ledger/stair-stands c)) @way) :retreats
            (and first? cheaper? (= "found" status) (number? seconds) @stair (> seconds (* (stair-dark-factor c) (- (second goal) (second feet)) (new-stair-step-s (:primitives c))))) :cheaper))))))

(defn escalate?
  "Whether a give-up after tries fruitless walks with result may escalate instead: a carried block fills the 1-cell gap it was refused (gap-fill), its only way dives (dive?), the goal is walled in (goal-walled?), or the walker failed max-blocked walks running (walker-fault?; one stuck walk of a :tries 1 caller is no proof) and no door or gate borders where it can walk
  (escape/door-beside?: that is its way out; an iron door, or any door under :doors :never, is a wall), or the planner's
  reason proves the way needs the world changed and the body is shut in (reach-room/enclosed?, which already counts doors
  as opened: a door that leads nowhere, e.g. onto a step under its lintel, is no way out), or the search ran out of
  land (:exhausted, after the walks that looked at every edge) in a mine or cave larger than a room. Never out of
  the body's own shelter: it is shut in on purpose."
  [c tries result]
  (let [doors (some-> (:doors (:args c)) keyword)
        shut? (delay (escape/walk-enclosed? c))]
    (and (ways/escalating? c)
         (or (shelter-exit? c result) (not (sh-inside/sheltered-in c)))
         ;; the cheap checks first: drop-fill and air-pocket? flood the body's land
         (or (shelter-exit? c result)
             (some? (gap-fill c result (skips c)))
             (sprint-only? result)
             (dive? c result)
             (goal-walled? c result shut?)
             (some? (drop-fill c result (skips c) (end/feet-cell c)))
             (air-pocket? c result)
             (and (walker-fault? result) (>= tries (max-blocked))
                  (not (escape/door-beside? (:primitives c) (end/feet-cell c) doors)))
             (and (escalate-reason? result doors)
                  (or (= :exhausted (some-> (:reason result) keyword))
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
    (if (and fill (or (not (#{:stair :pillar :clear-path :dig-route :bridge} (:step way))) (<= (price p feet fill) (price p feet way))))
      fill
      way)))
