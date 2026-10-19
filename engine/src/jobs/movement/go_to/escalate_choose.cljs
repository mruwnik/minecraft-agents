(ns jobs.movement.go-to.escalate-choose
  "go-to's choice of a way out of a shut-in body, or in to a walled-in goal: whether a give-up may escalate, and which
  pillar, stair, clear-path, dig route or walk to the wall to take."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.holes :refer [may-dig-fn own-placed-fn spare-own?]]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.tuning :refer [max-blocked]]
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
  through a door that :doors :never refuses (:abilities of kind :open): such a door is a wall."
  [result doors]
  (let [reason (some-> (:reason result) keyword)]
    (boolean (or (contains? escalate-reasons reason)
                 (and (= :abilities reason) (= :never doors) (= :open (some-> (:kind result) keyword)))))))

(defn dive?
  "Whether a give-up with result is a way that dives (:way-dives) that go-to may climb out of by a stair (c :dive-stair,
  the setting)."
  [c result]
  (boolean (and (:dive-stair c) (ways/way? c :stair) (= :way-dives (some-> (:reason result) keyword)))))

(defn shelter-exit?
  "Whether a give-up with result may leave the body's own shelter instead: the walk proved the way needs the world
  changed, the body is shut in its shelter and it is not night (dig-in-leave/leave-unsafe)."
  [c result]
  (boolean (and (not (contains? (skips c) :leave-shelter))
                (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                (sh-inside/sheltered-in c)
                (nil? (dig-leave/leave-unsafe (:primitives c))))))

(defn fetch-tool?
  "Whether the escalation may fetch a pickaxe (stair :fetch, a :dig-route dig's): the body is free (a shut-in body
  cannot walk to one; by-hand is its fallback) and carries one pickaxe or none, so the last one breaking is no stop."
  [c]
  (let [p (:primitives c)]
    (and (not (escape/enclosed? p (some-> (:doors (:args c)) keyword)))
         (<= (count (filter #(str/ends-with? (str (:name %)) "_pickaxe") (u/inventory p))) 1))))


(defn gap-fill
  "{:step :fill-gap :cell :item} when result (a planner no-path) is a gap jump refused for a low ceiling, a carried
  pillar block can fill the gap's floor, and no zone refuses the place; else nil."
  [c result skip]
  (let [p (:primitives c)
        {:keys [item]} (escape/pillar-item c)
        cell (when (and (= :abilities (some-> (:reason result) keyword)) (= :gap-low-ceiling (some-> (:kind result) keyword))
                        (not (skip :fill-gap)) item (:at result) (:from result))
               (escape/gap-floor (:at result) (:from result)))]
    (when (and cell (or (:ignore-zones? (:args c)) (nil? (tidy/refusal c :place (zipmap [:x :y :z] cell)))))
      {:step :fill-gap :cell cell :item item})))

(defn own-stair
  "The body's own stair cut again (climb/recut) as the way out for a body standing on it under goal: what stands in its
  walk space (a cap the body put there) is cleared before any other way is cut, its floors are not. nil once it failed
  this call (memory :own-way-done) and on a climb away from a dive (climb/own-route chose the way there)."
  [c feet goal]
  (when (and (> (second goal) (second feet)) (ways/way? c :stair) (not-any? (ctx/mem c) [:own-way-done :dive-climb]))
    (climb/recut feet (ledger/ways-of c))))

(defn goal-walled?
  "Whether a give-up with result is a free body's (not escape/enclosed?) whose goal the planner proved walled in: the
  ways that change the world run toward the goal (goal-way)."
  [c result]
  (and (= :goal-enclosed (some-> (:reason result) keyword))
       (not (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword)))))

(defn route-cells
  "[{:cell :block :guess}]: the cheapest cells to dig for a walk from feet cell from down or across into goal
  (jobs.lib.dig-route priced by jobs.lib.cost: a block's walk and dig-cost; at most the dig-route settings' digs and fall),
  in walking order, every one escape/diggable?, :guess on a cell not seen (read as rock); [] when nothing is left to
  dig, nil when no such route."
  [p from goal may-dig? own?]
  (let [block-at (escape/block-at-of p)
        ;; a floor never seen under a cell seen open may be a void: dig-route reads nil as no floor
        seen-floor (fn [[x y z :as cell]]
                     (when-not (and (dig-look/unknown? p cell) (dig-route/open? (block-at [x (inc y) z]))
                                    (not (dig-look/unknown? p [x (inc y) z])))
                       (block-at cell)))
        cells (:dig (dig-route/route seen-floor from goal {:dig-s cost/dig-cost
                                                         :step-s (:cost (cost/walk-cost {:distance 1}))}))]
    (when (and cells (every? #(escape/diggable? block-at may-dig? own? %) cells))
      (mapv (fn [cell] {:cell cell :block (block-at cell) :guess (dig-look/unknown? p cell)}) cells))))

(defn dig-way
  "{:step :dig-route :cells (route-cells)} when there are cells to dig, else nil."
  [p from goal may-dig? {:keys [own? skip]}]
  (when-not (skip :dig-route)
    (when-let [cells (seq (route-cells p from goal may-dig? own?))]
      {:step :dig-route :cells (vec cells)})))

(defn nearest-stand
  "A cell [x y z] the body can walk to (a flood of at most reach/room-cells cells) nearest goal: of the cells no farther
  than slack (default 0) beyond the nearest, in the whole distance and in the distance along the ground, the first the
  flood reaches (the shortest walk)."
  ([p goal] (nearest-stand p goal 0))
  ([p goal slack]
   (let [kind-at (reach/lookup p reach/body-kind-of)
         to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
         across #(u/flat-dist {:x (% 0) :z (% 2)} {:x (goal 0) :z (goal 2)})
         cells (->> (:cells (reach/flood-visit #(reach/forward kind-at %1 %2 %3) (reach/standing-vec p) reach/room-cells))
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
          opts {:own? (own-placed-fn c) :skip (skips c) :pillar-item (escape/pillar-item c)}
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

(defn stair-check
  "A fn of a plan and whether it is the first of its round, saying why it is not walked and a stair up is cut instead (a
  re-plan inside a walk is held to :dives only), when escalating is on, the body is not in its shelter and escape/choose would stair (steps n):
  - :dives: under a cave floor (c :cave-floor, with c :dive-stair) the plan goes below it on a cell that is no stair of
    the body's own (climb/dives?), or below c :own-floor back down a stair of its own (climb/returns?);
  - :cheaper: it is a whole path costing more seconds than 3 such stairs, the goal is stair-first-rise or more above
    the feet and the stair has not been tried this call.
  Else nil."
  [c pos]
  (let [m (ctx/mem c)
        feet (end/feet-cell c)
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        cave (when (:dive-stair c) (:cave-floor c))
        back (:own-floor c)
        cheaper? (and (not (:stair-tried m)) (>= (- (second goal) (second feet)) stair-first-rise))
        stair (delay (let [e (escape/choose (:primitives c) feet goal (may-dig-fn c) {:own? (own-placed-fn c) :skip (skips c) :pillar-item (escape/pillar-item c)})]
                       (when (= :stair (:step e)) e)))]
    (when (and (ways/way? c :stair) (not (sh-inside/sheltered-in c)) (or cave cheaper? back))
      (fn [{:keys [status steps ^js r]} first?]
        (let [seconds (some-> r .-path .-cost .-priced)]
          (cond
            (and cave (climb/dives? steps cave (ledger/ways-of c)) @stair) :dives
            (and back (climb/returns? steps back (ledger/stair-stands c)) @stair) :dives
            (and first? cheaper? (= "found" status) (number? seconds) @stair (> seconds (* 3 stair-step-s (:steps @stair)))) :cheaper))))))

(defn escalate?
  "Whether a give-up after tries fruitless walks with result may escalate instead: a carried block fills the 1-cell gap it was refused (gap-fill), its only way dives (dive?), the goal is walled in (goal-walled?), or the walker failed max-blocked walks running (walker-fault?; one stuck walk of a :tries 1 caller is no proof) or the body is shut in (reach/enclosed?), and no door or
  gate borders where it can walk (escape/door-beside?: that is its way out; an iron door, or any door under :doors :never,
  is a wall). Never out of
  the body's own shelter: it is shut in on purpose."
  [c tries result]
  (and (ways/escalating? c)
       (or (shelter-exit? c result) (not (sh-inside/sheltered-in c)))
       (or (shelter-exit? c result)
           (some? (gap-fill c result (skips c)))
           (sprint-only? result)
           (dive? c result)
           (goal-walled? c result)
           (let [doors (some-> (:doors (:args c)) keyword)]
             (and (or (and (walker-fault? result) (>= tries max-blocked))
                      (and (escalate-reason? result doors) (escape/enclosed? (:primitives c) doors)))
                  (not (escape/door-beside? (:primitives c) (end/feet-cell c) doors)))))))

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
