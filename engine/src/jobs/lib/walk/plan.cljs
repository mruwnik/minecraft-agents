(ns jobs.lib.walk.plan
  "The walk driver's planning: plan the walk from the body's cell to a goal within the executor's abilities (walk/plan-walk!, plan-within!),
  in slices that yield to the event loop, and say when a plan is not walked (no-walk)."
  (:require [engine.ctx :as ctx]
            [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.escape :as escape]
            [jobs.lib.walk.one-way :as one-way]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.world :as wworld]))

(def wide-box
  "The planner's search box (options margin and yMargin, blocks round start and goal) of the walks' searches. A way
  round can run far past the planner's default box (64 and 48). The walks search the wide box from the start:
  A* goes no wider than it must, and a search in the default box that hit its edge would only have to be repeated."
  {:margin 256 :yMargin 96})

(defn box-of
  "wide-box with the policy's :y-margin (go-to's) as its yMargin and its :y-below (go-to's dive floor) as its yBelow,
  the blocks below the lower of body and goal alone."
  [policy]
  (cond-> wide-box
    (some? (:y-margin policy)) (assoc :yMargin (:y-margin policy))
    (some? (:y-below policy)) (assoc :yBelow (:y-below policy))))

(def chunk-expansions
  "Expansions a search of the walking plans (plan-from! and plan-within!) runs between yields to the event
  loop, so the body's HTTP API, perception and the other jobs run during a long search. An expansion costs some tens
  of microseconds."
  1000)

(defn yield!
  "A promise that resolves once the event loop has run what was waiting (I/O callbacks included)."
  []
  (js/Promise. (fn [resolve] (js/setImmediate resolve))))

(defn stop-if-cut!
  "Throws the cut error (what an act raises) when c's round was cut: a search loop with no act checks this per slice."
  [c]
  (when-not (ctx/alive? c)
    (throw (doto (js/Error. "cut: the ownership token changed") (aset "code" "cut")))))

(defn ^:async run-plan!
  "planner/plan in slices of chunk-expansions (planner/create-plan) with a yield! between them: the same result. A cut
  round's call throws the cut error before its next slice."
  [c snapshot query options]
  (let [^js p (planner/create-plan snapshot query options)]
    (loop []
      (stop-if-cut! c)
      (when-not (.step p chunk-expansions)
        (await (yield!))
        (recur)))
    (.result p)))

(defn plan-query
  "The planner query from the body's cell to within range of the goal cell to."
  [c to range]
  (let [pos (.-pos (.self (:primitives c)))
        [gx gy gz] to]
    #js {:from #js {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))
                    :px (.-x pos) :py (.-y pos) :pz (.-z pos)}
         :goal #js {:kind "near" :x gx :y gy :z gz :range range}}))

(defn plan-options
  "The planner options over pw with weight, limits and the search box (nil: the planner's default); pw's dangers
  (with-dangers) as options.dangers, its dark (with-dark) as options.dark and its snapshot's mask (wworld/path-world with
  unknown) as options.unknown."
  [pw weight limits box]
  (let [unknown (wworld/unknown-of pw)]
    (js/Object.assign #js {:table (.-table pw) :space (.-space pw) :weight weight :limits limits :dangers (.-dangers pw) :avoid (.-avoid pw)
                           :dark (.-dark pw) :tolls (.-tolls pw) :unknown unknown}
                      (when unknown #js {:maxNodes (wworld/unknown-max-nodes)})
                      (clj->js box))))

(defn goal-stand-problem
  "The planner's verdict on the goal alone, before any search or goal flood: :goal-not-standable (no cell within range
  of to [x y z] stands) or :goal-on-magma, else nil (a cell may end a route, or the start does not stand). pw: a
  pathWorld (wworld/path-world)."
  [c ^js pw to range]
  (let [^js s (planner/create-search (.-snapshot pw) (plan-query c to range)
                                     (doto (plan-options pw 1 nil nil) (unchecked-set "preFlood" 0)))]
    (when (.step s 0)
      (#{:goal-not-standable :goal-on-magma} (keyword (.-reason ^js (.result s)))))))

(defn with-drops
  "options (the planner's, a JS object) with the policy's :drop-cost: a number is the planner's costs.dropFactor (nil: as
  it is), false takes no drop of 2 or 3 (maxDrop 1). The policy's :max-drop is maxDrop, its :fall-factor fallFactor, and
  :damage-budget and :damage-weight the planner's damageBudget and damageWeight (none: the planner's defaults). Its :landing (block
  name -> damage factor) is the planner's landing over cost/default-landing (no bounce under :gait :sneak), seen (wworld/landing-seen) its landingSeen. Its :gait (:walk, :sneak), or :sprint false (:walk), sets the costs walkS and sprintS under the :costs; its :air-drain, :air-grace, :air-used the costs airDrain, airGrace and airUsed, its :swim-h (Depth Strider) swimH under the caller's :costs; its :health, :drown-prices and :air-seen the planner's health, drownPrices and airSeen; its :goal-seen the planner's goalSeen."
  ([options policy] (with-drops options policy nil))
  ([^js options policy seen]
  (let [k (:drop-cost policy)
        gait (or (:gait policy) (when (false? (:sprint policy)) :walk))]
    (doseq [[opt key] [["maxDrop" :max-drop] ["fallFactor" :fall-factor] ["damageBudget" :damage-budget] ["damageWeight" :damage-weight] ["dangerCap" :danger-cap]
                       ["health" :health]]]
      (when-some [v (get policy key)] (unchecked-set options opt v)))
    (when-some [p (:drown-prices policy)] (unchecked-set options "drownPrices" (js/Float64Array.from (clj->js p))))
    (when-some [f (:air-seen policy)] (unchecked-set options "airSeen" f))
    (unchecked-set options "landing" (cost/planner-landing (:landing policy) gait))
    (when (some? seen) (unchecked-set options "landingSeen" seen))
    (when-some [f (:goal-seen policy)] (unchecked-set options "goalSeen" f))
    (when (or (seq (:costs policy)) (contains? #{:walk :sneak} gait) (:air-drain policy) (:air-limit policy) (:air-grace policy) (:air-used policy) (:swim-h policy))
      (unchecked-set options "costs" (js/Object.assign (cost/gait-costs gait)
                                                       (cost/air-costs policy)
                                                       (cost/planner-costs (:costs policy)))))
    (cond
      (false? k) (doto options (unchecked-set "maxDrop" 1))
      (and (number? k) (not= 1 k)) (doto options (unchecked-set "costs" (js/Object.assign (or (.-costs options) #js {}) #js {:dropFactor k})))
      :else options))))

(defn ^:async plan-from!
  "Plan from the body's cell to the goal in wide-box, within limits (the planner's options.limits, nil for none): the
  planner's JS result, searched in slices (run-plan!) with the event loop run between them; policy's :drop-cost as with-drops."
  ([c pw to range weight limits] (plan-from! c pw to range weight limits nil))
  ([c pw to range weight limits policy]
   (await (run-plan! c (.-snapshot pw) (plan-query c to range) (with-drops (plan-options pw weight limits (box-of policy)) policy (wworld/landing-seen (:primitives c)))))))

(defn with-damage
  "steps (of js-steps) with the hp each is planned to cost (:damage, the planner's step damage) where it costs any."
  [steps ^js js-steps]
  (mapv (fn [s ^js js] (cond-> s (some? (unchecked-get js "damage")) (assoc :damage (unchecked-get js "damage"))))
        steps (array-seq js-steps)))

(defn with-light
  "steps (of js-steps) with :light true where the planner lights the step's floor with a torch at its takeoff."
  [steps ^js js-steps]
  (mapv (fn [s ^js js] (cond-> s (true? (unchecked-get js "light")) (assoc :light true))) steps (array-seq js-steps)))

(defn unknown-floor-fn
  "With a mask on pw's snapshot (.unknown, its unseen cells read as the mask's id): a fn [x y z] true for a floor cell the
  mask hides under a stand that stands on its floor (not in a door's lower half or a climbable, whose floor the planner
  does not ask for); nil without a mask."
  [^js pw]
  (when-let [^js u (some-> pw .-snapshot .-unknown)]
    (let [^js snapshot (.-snapshot pw) ^js table (.-table pw) id (.-id u)]
      (fn [x y z]
        (let [own (.stateAt snapshot x (inc y) z)]
          (and (== id (.stateAt snapshot x y z))
               (not (== 1 (aget (.-doorHalf table) own)))
               (zero? (aget (.-climb table) own))))))))

(defn path-steps
  "The executor's steps for a planner path over pw: corner free sides and hops, high corners, gap ceilings and drops onto a
  bouncing block marked, the planned damage of each, and over a masked snapshot the unseen floors (jobs.lib.walk.sight) and
  the ones a torch lights (:light)."
  [pw ^js path]
  (let [solid? (wworld/solid-fn pw)
        unknown-floor? (unknown-floor-fn pw)
        steps (executor/with-bounces
                executor/policy
                (executor/with-gap-ceilings
                  executor/policy
                  (executor/with-high-corners
                    executor/policy
                    (executor/with-corner-hops (executor/with-free-sides (with-light (with-damage (executor/steps-of (.-steps path)) (.-steps path)) (.-steps path)) solid?) solid?)
                    solid?)
                  solid?)
                (wworld/bounce-fn pw)
                (wworld/wall-fn pw))]
    (cond-> steps unknown-floor? (wsight/mark-steps unknown-floor?))))

(defn plan-steps
  "The executor's steps for a found plan r over pw (path-steps of its path)."
  [pw r]
  (path-steps pw (.-path r)))

(defn within-of [pw r] {:r r :steps (when (.-path r) (plan-steps pw r))})

(defn with-beyond
  "within with :beyond, the policy's refusal of the path a search without the limits found (wide), when it found one. A search
  that ran out of land short of the goal (unseen floors on, a drop it did not take) counts by the partial path to its
  nearest node: the refused step lies on the way there."
  [within pw policy wide]
  (let [steps (when (or (= "found" (.-status wide)) (and (= "partial" (.-status wide)) (= "exhausted" (.-reason wide))))
                (plan-steps pw wide))
        refusal (some->> steps (executor/refusal policy))
        from (when refusal (some (fn [[prev s]] (when (= (:at refusal) [(:x s) (:y s) (:z s)]) prev)) (map vector steps (rest steps))))]
    (cond-> within
      refusal (assoc :beyond (cond-> refusal from (assoc :from [(:x from) (:y from) (:z from)]))))))

(defn beyond-needed?
  "Whether a search within the walker's limits that found no whole path leaves a search without them to run: only when
  the limits turned some move away (r.limited). Otherwise the search without them would search the very same moves. A
  masked search (pw) that ran out of nodes flooded unseen cells: the search without the limits would flood them again."
  [r pw]
  (and (not= "found" (.-status r)) (true? (.-limited r))
       (not (and (wworld/unknown-of pw) (= "budget" (.-reason r))))))

(defn ^:async plan-within!
  "Plan within the executor's abilities (policy, default executor/policy): {:r :steps} (steps nil when r has no path), in
  slices that yield to the event loop. When that finds no whole path, and the limits turned a move away (beyond-needed?),
  but a search without the limits finds one, also :beyond, the executor's refusal of that path: no path within abilities,
  and the kind of step that would have made one."
  [c pw to range weight policy]
  (let [r (await (plan-from! c pw to range weight (executor/planner-limits policy (wworld/solid-fn pw)) policy))
        within (within-of pw r)]
    (if (beyond-needed? r pw)
      (with-beyond within pw policy (await (plan-from! c pw to range weight nil policy)))
      within)))

(defn head-wet-fn
  "(fn [step]): whether the step's head cell over pw is water or waterlogged (a ladder, scaffolding), not a bubble column."
  [pw]
  (let [^js snapshot (.-snapshot pw) ^js table (.-table pw)]
    (fn [{:keys [x y z]}]
      (let [id (.stateAt snapshot x (inc y) z)]
        (and (not (== id planner/UNLOADED)) (pos? (aget (.-wet table) id)) (zero? (aget (.-bubble table) id)))))))

(defn masked-wet-ok
  "(fn [before step]) for dry-end of a masked walk, called on its swimming steps in order with the plan's steps before each:
  one is kept while the body, going on through the swim it ends (the swimming steps right before it, and it) and back, keeps
  the air margin at its last breath (jobs.lib.cost/wet-ok?): a long climb back turns it earlier than a flat swim."
  [c]
  (let [policy (wworld/body-policy c)]
    (fn [before step]
      (cost/wet-ok? policy (conj (mapv :move (reverse (take-while :swim (rseq before)))) (:move step))))))

(def max-surfacings "Walks to air one go-to call makes at most." 3)

(defn swim-outlasts-air?
  "Whether a body under water (not on a walk to air: c :to-air) that has been in air since this go-to call began (mem :last-air) and has surfaced less than
  max-surfacings times should turn back: the swim back to its last breath leaves it under the air margin
  (jobs.lib.cost/wet-ok?), and either the steps' swim (jobs.lib.cost/wet-way-s, each step at its real pace) takes more than the air left, or the
  plan surfaces at a cell whose air it has not seen (an unseen head cell reads as air to the planner; a player does not swim on for a guess)."
  [c steps]
  (let [policy (wworld/body-policy c)
        m (ctx/mem c)
        exit (first (remove :swim steps))]
    (when (:air-used policy)
      (and (:last-air m) (not (:to-air c)) (< (:surfacings m 0) max-surfacings) (not (cost/wet-ok? policy []))
           (or (> (cost/wet-way-s policy (map :move (filter :swim steps))) (cost/air-left-s policy))
               (and exit (not (breath/air? (breath/seen-water-name (:primitives c) {:x (:x exit) :y (inc (:y exit)) :z (:z exit)})))))))))

(defn masked-cut
  "A found masked plan's steps up to its first swimming step wet-ok? refuses, and whether it cut: the unseen cells above a dark
  swim read as air to breathe, so only the plan's first masked-wet-ok swim is trusted (of a body not yet under water: one under it swims to air)."
  [steps wet-ok?]
  (let [k (first (keep-indexed (fn [i s] (when (and (:swim s) (not (wet-ok? (subvec steps 0 i) s))) i)) steps))]
    (if k [(subvec steps 0 k) true] [steps false])))

(defn dry-end
  "A partial plan up to its last step out of water: a walk that cannot reach the goal never leaves the body swimming (at a
  bank too high to climb out, say), nor with its head in water (wet?, head-wet-fn: on a waterlogged ladder). With wet-ok? (a
  masked walk), the plan is first cut before the first swimming step it refuses, and the swimming steps left count as out of water too (a plan cut short by unseen water, not one that ran out of air)."
  ([steps] (dry-end steps (constantly false)))
  ([steps wet?] (dry-end steps wet? nil))
  ([steps wet? wet-ok?]
   (let [steps (if wet-ok? (first (masked-cut steps wet-ok?)) steps)
         swim-dry? (if wet-ok? (comp boolean :swim) (constantly false))
         k (last (keep-indexed (fn [i s] (when (or (not (or (:swim s) (wet? s))) (swim-dry? s)) i)) steps))]
     (if k (subvec steps 0 (inc k)) []))))

(defn- near-blocker-fn
  "(fn [step]): whether the step is within margin blocks more than the hostile-near radius of a danger with block true (a seen
  mob in dangers, the planner's options.dangers); nil when there is no such danger."
  [^js dangers margin]
  (let [r (+ margin (danger-q/hostile-radius))
        r2 (* r r)
        blockers (filterv #(true? (.-block ^js %)) (some-> dangers vec))]
    (when (seq blockers)
      (fn [{:keys [x y z]}]
        (some (fn [^js d] (< (+ (js/Math.pow (- (+ x 0.5) (.-x d)) 2) (js/Math.pow (- y (.-y d)) 2) (js/Math.pow (- (+ z 0.5) (.-z d)) 2)) r2))
              blockers)))))

(defn stop-short
  "A partial plan's steps without its tail within the hostile-near radius of a seen mob that blocks the way: a body stops out of
  reach of what it cannot pass, and escalates from there."
  [steps dangers]
  (if-let [near? (near-blocker-fn dangers 0)]
    (loop [k (count steps)]
      (if (and (pos? k) (near? (nth steps (dec k)))) (recur (dec k)) (subvec steps 0 k)))
    steps))

(defn retreat-cell
  "[x y z] of the last of steps (the way walked so far) a block clear of the hostile-near radius of every blocking seen mob
  of dangers, nil when the body is already clear of them or none is."
  [steps dangers]
  (when-let [near? (near-blocker-fn dangers 0)]
    (when (near? (peek steps))
      (when-let [s (first (remove (near-blocker-fn dangers 1) (rseq steps)))]
        [(:x s) (:y s) (:z s)]))))

(defn near-goal
  "How far, in blocks to 1 decimal, the step is from the goal cell to."
  [{:keys [x y z]} [gx gy gz]]
  (/ (js/Math.round (* 10 (js/Math.hypot (- gx x) (- gy y) (- gz z)))) 10))

(defn one-way-of
  "The planner's oneWay of a result as {:kind :at}: the first step on the way to a node nearer the goal that the body cannot undo
  (a drop of more than a block, a gap jump down), nil when there is none."
  [r]
  (when-let [^js ow (.-oneWay r)]
    {:kind (nth executor/move-names (.-move ow)) :at [(.-x ow) (.-y ow) (.-z ow)]}))

(defn stopped-one-way
  "The no-path result of a plan whose nearer end lies behind a step that cannot be undone: that step, and how near to the goal
  the walk got (from: the last step kept, or the start)."
  [r from to one-way]
  (assoc {:status :no-path :reason :one-way :planner (some-> (.-reason r) keyword)}
         :one-way one-way :near (near-goal from to)))

(defn open-path
  "The planner's path past r's one-way step when the land there runs on into unloaded land (oneWay.open: its end stands at the
  edge of what is loaded), nil otherwise: a region below that is all loaded and gets no nearer (a pit) has no open path."
  [r]
  (when-let [^js ow (.-oneWay r)]
    (when (true? (.-open ow)) (.-path ow))))

(defn one-way-priced?
  "Whether the one-way step landing at `at` is worth taking (jobs.lib.walk.one-way/taken?): the EV of its landing against the goal's worth
  (the call's :worth), and, for a drop of more than a block, a way out of the landing exists (escape/choose does not say :no-dig: a landing with nothing
  it could dig round it is a certain trap, not a chance of one). True when there is no step (nil at)."
  [c to at small?]
  (or (nil? at)
      (let [p (:primitives c)
            pos (.-pos (.self p))
            [gx gy gz] to]
        (and (one-way/taken? p at small? (js/Math.hypot (- gx (.-x pos)) (- gy (.-y pos)) (- gz (.-z pos))) (:worth (:args c)))
             (or small? (not= {:step :none :why :no-dig} (select-keys (escape/choose p (vec at) to) [:step :why])))))))

(defn drop-index
  "Index in steps of the first step that lands after a drop of 2 or more, a gap jump down (a step the body cannot undo) or any
  drop onto a floor not seen (it may be a deeper fall), nil when none."
  [steps]
  (first (keep-indexed (fn [i [a b]] (when (and (#{:drop :gap} (:move b))
                                                (> (:y a) (:y b))
                                                (or (:unseen-floor b) (> (- (:y a) (:y b)) (if (= :gap (:move b)) 0 1))))
                                       (inc i)))
                       (partition 2 1 steps))))

(defn cut-refused
  "steps cut short before its first step that cannot be undone when priced? (a fn of the landing [x y z] and whether the drop is
  one block) refuses it, with that step's landing as :refused; else {:steps steps}."
  [steps priced?]
  (let [i (drop-index steps)
        {:keys [x y z]} (when i (nth steps i))
        small? (when i (and (= 1 (- (:y (nth steps (dec i))) y)) (not= :gap (:move (nth steps i)))))]
    (if (and i (not (priced? [x y z] small?)))
      {:steps (subvec steps 0 i) :refused [x y z]}
      {:steps steps})))

(defn frontier-path
  "The planner's path to r's frontier (the searched node at the edge of what is loaded, within its reach of the goal, the
  search having run out of land) and that node's cell [x y z], with :known true when the node lies in land earlier
  searches knew to their end (known-land); nil when r has none."
  [r]
  (when-let [^js f (.-frontier r)]
    (cond-> {:path (.-path f) :at [(.-x f) (.-y f) (.-z f)]}
      (true? (.-known f)) (assoc :known true)
      (some? (.-target f)) (assoc :target (vec (.-target f))))))

(defn walk-plan
  "plan-walk!'s answer from its plan-within! answer {:r :steps :beyond} over walled (pw with the walls).
  With frontier, a search that ran out of loaded land (no path within abilities beyond it) walks to its frontier, not
  its nearest end. Every way the loaded land holds is known and none arrives, so a way can only go on past what is
  loaded. A body standing at its frontier walks nowhere, not even to the nearest end: that would swing between
  the two for ever. A search whose only edges lie in land earlier searches knew to their end, with none left open
  (the planner's searchedOut, known-land), walks nowhere either (:searched-out, no-walk: :no-path :exhausted)."
  [c pw walled to one-way frontier {:keys [r steps beyond]}]
  (let [edge (when (and frontier (not beyond)) (frontier-path r))
        out (and frontier (not beyond) (not edge) (true? (.-searchedOut r)))
        past (when (and (not edge) (not out) (= :open one-way)) (open-path r))
        {:keys [steps refused]} (let [all (cond edge (path-steps walled (:path edge)) past (path-steps walled past) :else steps)]
                                  (if (and (= :open one-way) (or (:unknown c) (not= "found" (.-status r)))) (cut-refused all #(one-way-priced? c to %1 %2)) {:steps all}))
        step (or (one-way-of r) (when refused {:kind :drop :at refused}))
        [steps cut] (if (and (:unknown c) (= "found" (.-status r)) (not (or edge past)) (nil? (:air-used (wworld/body-policy c))))
                      (masked-cut steps (masked-wet-ok c))
                      [steps false])
        status (if (or edge past cut refused) "partial" (.-status r))
        dry (if (= "partial" status)
              (dry-end steps (head-wet-fn walled) (if (and (:unknown c) (not= "air" (.-reason r))) (masked-wet-ok c) (constantly false)))
              steps)
        walked (if (= "partial" status) (stop-short dry (.-dangers walled)) dry)]
    {:r r :steps walked :beyond beyond :status status :pw pw
     :ms (or (some-> r .-ms) 0)
     :one-way-taken (when (and past (not refused)) step)
     :frontier-taken (when (and edge (> (count walked) 1)) (cond-> {:at (:at edge)} (:known edge) (assoc :known true) (:target edge) (assoc :target (:target edge))))
     :searched-out out
     :stopped-short (< (count walked) (count dry))
     :stop (when (and step (or refused (not (or past edge)))) (stopped-one-way r (or (peek walked) (first steps) (wworld/body-cell c)) to step))}))

(defn planner-reason
  "A no-path result's reason as go-to's: the planner's goal-unseen (no stand in range sees the block, found before any
  search) is the same verdict as a search that ran out of land under :see."
  [^js r]
  (let [k (some-> (.-reason r) keyword)]
    (if (= :goal-unseen k) :exhausted k)))

(defn no-walk
  "The result of a plan that is not walked, nil when it is: no path within abilities (:beyond), a goal the planner proved
  walled in or cut off by a drop (:goal-enclosed, :goal-cut-off: its partial plan's nearer end gets the body no nearer to arriving), a plan cut at a one-way step with no step left, no path, a plan the policy (default
  executor/policy) refuses. replans goes in the result; :damage-refused true when the search found no way for want of
  damage budget (the planner's damageRefused)."
  ([plan replans] (no-walk plan replans executor/policy))
  ([{:keys [r steps beyond status stop searched-out fresh stopped-short]} replans policy]
   (let [partial? (= "partial" status)]
     (cond
       (= "searching" status)
       (cond-> {:status :searching :replans replans} fresh (assoc :fresh true))

       beyond
       {:status :no-path :reason :abilities :kind (:kind beyond) :at (:at beyond) :from (:from beyond) :replans replans}

       (contains? #{"goal-enclosed" "goal-cut-off"} (some-> r .-reason))
       {:status :no-path :reason (keyword (.-reason r)) :replans replans}

       searched-out
       (cond-> {:status :no-path :reason :exhausted :searched-out true :replans replans}
         (true? (some-> r .-damageRefused)) (assoc :damage-refused true))

       (and stop (< (count steps) 2))
       (assoc stop :replans replans)

       (or (= "none" status) (and partial? (< (count steps) 2)))
       (cond-> {:status :no-path :reason (planner-reason r) :replans replans}
         stopped-short (assoc :stopped-short true)
         (true? (some-> r .-damageRefused)) (assoc :damage-refused true))

       :else
       (some-> (executor/refusal policy steps) (assoc :replans replans))))))
