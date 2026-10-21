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
            [jobs.lib.escape.span :as span]
            [jobs.lib.ledger :as ledger]
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
            [jobs.movement.go-to.tuning :refer [max-blocked max-route-s]]
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
  "Whether the escalation may fetch a pickaxe (the stair's and a :dig-route dig's ways): the body is free (a shut-in body
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
  dig, nil when no such route or when its digging takes the body longer than max-route-s, with what it carries, or with
  a wooden pickaxe when fetch? (the dig fetches one, fetch-tool?), else bare-handed (a player does not punch a tunnel through rock). A floor never seen under a cell seen open may be a void: dig-route reads it as no floor,
  unless floors-unseen-ok? (a pocket's floor shows only from inside it)."
  ([p from goal may-dig? own?] (route-cells p from goal may-dig? own? false false))
  ([p from goal may-dig? own? floors-unseen-ok?] (route-cells p from goal may-dig? own? floors-unseen-ok? false))
  ([p from goal may-dig? own? floors-unseen-ok? fetch?]
  (let [block-at (escape/block-at-of p)
        seen-floor (fn [[x y z :as cell]]
                     (when-not (and (not floors-unseen-ok?) (dig-look/unknown? p cell) (dig-route/open? (block-at [x (inc y) z]))
                                    (not (dig-look/unknown? p [x (inc y) z])))
                       (block-at cell)))
        cells (:dig (dig-route/route seen-floor from goal {:dig-s cost/dig-cost
                                                         :step-s (:cost (cost/walk-cost {:distance 1}))}))]
    (when (and cells (every? #(escape/diggable? block-at may-dig? own? %) cells)
               (<= (transduce (map #(/ (cond-> (tools/clear-ms p (block-at %))
                                                 fetch? (min (js/Number (.clearTime p (block-at %) "wooden_pickaxe"))))
                                               1000))
                              + 0 cells)
                   (max-route-s)))
      (mapv (fn [cell] {:cell cell :block (block-at cell) :guess (dig-look/unknown? p cell)}) cells)))))

(defn dig-way
  "{:step :dig-route :cells (route-cells)} when there are cells to dig, else nil."
  [p from goal may-dig? {:keys [own? skip fetch?]}]
  (when-not (skip :dig-route)
    (when-let [cells (seq (route-cells p from goal may-dig? own? false fetch?))]
      {:step :dig-route :cells (vec cells)})))

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

(defn way-opts
  "The opts of escape/choose and the spans for this call's escalations."
  [c]
  {:own? (own-placed-fn c) :skip (skips c) :fetch? (fetch-tool? c) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c)})

(defn walled-step
  "escape/choose from cell from for a goal that is walled in: only a door, or a stair or pillar up to a higher goal (a
  walk along the wall too, from where it stands, to a goal not below it) leads in, else the fewest cells dug (dig-way);
  else {:step :none :why :no-dig}."
  [c from goal here? may-dig? opts]
  (let [p (:primitives c)
        e (escape/choose p from goal may-dig? opts)
        in? (case (:step e)
              :clear-path true
              (:stair :pillar) (> (second goal) (second from))
              :approach (and here? (>= (second goal) (second from)))
              false)]
    (cond in? e
          :else (or (dig-way p from goal may-dig? opts)
                    (if (= :none (:step e)) e {:step :none :why :no-dig})))))

(defn price
  "Seconds the escalation e from feet is reckoned to take (jobs.lib.cost): a stair, a pillar or a bridge by its blocks, a
  dug door or route by what its cells take to dig with the tool carried, plus a step a cell; 0 for a walk to the wall."
  [c feet {:keys [step] :as e}]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        dig-s (fn [cells] (transduce (map #(+ (/ (tools/clear-ms p (block-at %)) 1000) (:cost (cost/walk-cost {:distance 1})))) + 0 cells))]
    (case step
      :stair (+ (cost/stair-cost (:steps e)) (* (:aside e 0) (or (cost/dig-cost "stone") 1)))
      :pillar (cost/pillar-cost (:height e))
      :bridge (cost/bridge-cost (:length e) (:short e 0))
      :dig-route (dig-s (map :cell (:cells e)))
      :clear-path (dig-s (:cells (escape/door block-at (escape/unseen-of p) feet (escape/heading-dirs (:heading e)) (escape/max-door))))
      0)))

(defn spans
  "The stair down and the bridge toward goal from cell from (jobs.lib.escape.span) that opts allow, as a vector."
  [c from goal may-dig? opts]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)]
    (into [] (remove nil?)
          [(span/down-stair block-at from goal may-dig? (:own? opts) (:skip opts))
           (span/bridge-span block-at (escape/unseen-of p) from goal opts)])))

(defn with-spans
  "e (a way from escape/choose, walled-step or none, planned from cell from) or, when e is none or a walk to the wall
  (spans fill what it lacks) or price (jobs.lib.cost) says a span from the feet is cheaper than the real
  way e: the cheapest span."
  [c feet from goal may-dig? opts e]
  (let [extra (spans c feet goal may-dig? opts)
        real? (not (#{:none :approach} (:step e)))]
    (if (seq extra)
      (apply min-key #(price c (if (= e %) from feet) %) (cond-> extra real? (conj e)))
      e)))

(defn goal-way-plan
  "{:best way :near cell :here? bool}: goal-way's choice before the walk to the nearest cell is added."
  [c feet goal may-dig? opts]
  (let [p (:primitives c)
        to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
        near (nearest-stand p goal)
        here? (or ((:skip opts) :approach) (>= (to-goal near) (dec (to-goal feet))))
        from (if here? feet near)
        e (walled-step c from goal here? may-dig? opts)]
    {:best (with-spans c feet from goal may-dig? opts e) :e e :near near :here? here?}))

(defn goal-way
  "A free body's way to a goal it cannot walk to (walled in, or cut off by a drop), the cheapest by price over: what
  walled-step finds, a stair down to a goal below and a bridge over a gap toward it (with-spans; spans start where the
  body stands). While the cell it can walk to nearest the goal (nearest-stand) is more than a block nearer, the way
  of walled-step is chosen from there, as {:step :approach :pos cell} first. {:step :none :why ...} when there is none."
  [c feet goal may-dig? opts]
  (let [{:keys [best e near here?]} (goal-way-plan c feet goal may-dig? opts)]
    (cond
      (= :none (:step best)) best
      (or here? (not= e best)) best
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

(def stair-first-rise
  "How far above the feet a goal is for a found plan to be weighed against a stair up."
  8)

(defn stair-step-s-here
  "stair-step-s, dearer by the dark factor where the body stands dark: the plan's seconds carry its dark surcharge, and the
  stair is cut in that dark too."
  [c]
  (cond-> (cost/stair-cost 1)
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

(defn cut-off-way?
  "Whether a give-up with result is a free body's whose goal pos the planner proved cut off by a drop and goal-way finds
  a way that changes the world and starts at once (no walk to a wall): a stair, pillar or bridge."
  [c pos result]
  (boolean
   (and pos (= :goal-cut-off (some-> (:reason result) keyword))
        (not (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword)))
        (not (#{:none :approach} (:step (:best (goal-way-plan c (end/feet-cell c) (mapv #(js/Math.floor (% pos)) [:x :y :z]) (may-dig-fn c) (way-opts c)))))))))

(defn escalate?
  "Whether a give-up after tries fruitless walks with result may escalate instead: a carried block fills the 1-cell gap it was refused (gap-fill), its only way dives (dive?), the goal is walled in (goal-walled?), or the walker failed max-blocked walks running (walker-fault?; one stuck walk of a :tries 1 caller is no proof) or the body is shut in (reach-room/enclosed?), and no door or
  gate borders where it can walk (escape/door-beside?: that is its way out; an iron door, or any door under :doors :never,
  is a wall). Never out of
  the body's own shelter: it is shut in on purpose."
  ([c tries result] (escalate? c tries result nil))
  ([c tries result pos]
  (and (ways/escalating? c)
       (or (shelter-exit? c result) (not (sh-inside/sheltered-in c)))
       (or (shelter-exit? c result)
           (some? (gap-fill c result (skips c)))
           (sprint-only? result)
           (dive? c result)
           (goal-walled? c result)
           (cut-off-way? c pos result)
           (let [doors (some-> (:doors (:args c)) keyword)]
             (and (or (and (walker-fault? result) (>= tries (max-blocked)))
                      (and (escalate-reason? result doors) (escape/enclosed? (:primitives c) doors)))
                  (not (escape/door-beside? (:primitives c) (end/feet-cell c) doors))))))))

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
