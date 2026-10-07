(ns jobs.lib.walk.plan
  "The walk driver's planning: plan the walk from the body's cell to a goal within the executor's abilities (walk/plan-walk!, plan-within!),
  in slices that yield to the event loop, and say when a plan is not walked (no-walk)."
  (:require [engine.ctx :as ctx]
            [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.cost :as cost]
            [jobs.lib.walk.world :as wworld]))

(def wide-box
  "The planner's search box (options margin and yMargin, blocks round start and goal) of the walks' searches. A way
  round can run far past the planner's default box (64 and 48). The walks search the wide box from the start:
  A* goes no wider than it must, and a search in the default box that hit its edge would only have to be repeated."
  {:margin 256 :yMargin 96})

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
  (with-dangers) as options.dangers and its dark (with-dark) as options.dark."
  [pw weight limits box]
  (js/Object.assign #js {:table (.-table pw) :space (.-space pw) :weight weight :limits limits :dangers (.-dangers pw) :avoid (.-avoid pw)
                         :dark (.-dark pw) :tolls (.-tolls pw)}
                    (clj->js box)))

(defn with-drops
  "options (the planner's, a JS object) with the policy's :drop-cost: a number is the planner's costs.dropFactor (nil: as
  it is), false takes no drop of 2 or 3 (maxDrop 1). The policy's :max-drop is maxDrop, its :fall-factor fallFactor, and
  :damage-budget and :damage-weight the planner's damageBudget and damageWeight (none: the planner's defaults). Its :landing (block
  name -> damage factor) is the planner's landing over cost/default-landing (no bounce under :gait :sneak), seen (wworld/landing-seen) its landingSeen. Its :gait (:walk, :sneak), or :sprint false (:walk), sets the costs walkS and sprintS under the :costs; its :air-drain, :air-grace and :air-used the costs airDrain, airGrace and airUsed."
  ([options policy] (with-drops options policy nil))
  ([^js options policy seen]
  (let [k (:drop-cost policy)
        gait (or (:gait policy) (when (false? (:sprint policy)) :walk))]
    (doseq [[opt key] [["maxDrop" :max-drop] ["fallFactor" :fall-factor] ["damageBudget" :damage-budget] ["damageWeight" :damage-weight] ["dangerCap" :danger-cap]]]
      (when-some [v (get policy key)] (unchecked-set options opt v)))
    (unchecked-set options "landing" (cost/planner-landing (:landing policy) gait))
    (when (some? seen) (unchecked-set options "landingSeen" seen))
    (when (or (seq (:costs policy)) (contains? #{:walk :sneak} gait) (:air-drain policy) (:air-grace policy) (:air-used policy))
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
   (await (run-plan! c (.-snapshot pw) (plan-query c to range) (with-drops (plan-options pw weight limits wide-box) policy (wworld/landing-seen (:primitives c)))))))

(defn with-damage
  "steps (of js-steps) with the hp each is planned to cost (:damage, the planner's step damage) where it costs any."
  [steps ^js js-steps]
  (mapv (fn [s ^js js] (cond-> s (some? (unchecked-get js "damage")) (assoc :damage (unchecked-get js "damage"))))
        steps (array-seq js-steps)))

(defn path-steps
  "The executor's steps for a planner path over pw: corner free sides and hops, high corners, gap ceilings and drops onto a
  bouncing block marked, and the planned damage of each."
  [pw ^js path]
  (let [solid? (wworld/solid-fn pw)]
    (executor/with-bounces
      executor/policy
      (executor/with-gap-ceilings
        executor/policy
        (executor/with-high-corners
          executor/policy
          (executor/with-corner-hops (executor/with-free-sides (with-damage (executor/steps-of (.-steps path)) (.-steps path)) solid?) solid?)
          solid?)
        solid?)
      (wworld/bounce-fn pw)
      (wworld/wall-fn pw))))

(defn plan-steps
  "The executor's steps for a found plan r over pw (path-steps of its path)."
  [pw r]
  (path-steps pw (.-path r)))

(defn within-of [pw r] {:r r :steps (when (.-path r) (plan-steps pw r))})

(defn with-beyond
  "within with :beyond, the policy's refusal of the path a search without the limits found (wide), when it found one."
  [within pw policy wide]
  (cond-> within
    (= "found" (.-status wide)) (assoc :beyond (executor/refusal policy (plan-steps pw wide)))))

(defn beyond-needed?
  "Whether a search within the walker's limits that found no whole path leaves a search without them to run: only when
  the limits turned some move away (r.limited). Otherwise the search without them would search the very same moves."
  [r]
  (and (not= "found" (.-status r)) (true? (.-limited r))))

(defn ^:async plan-within!
  "Plan within the executor's abilities (policy, default executor/policy): {:r :steps} (steps nil when r has no path), in
  slices that yield to the event loop. When that finds no whole path, and the limits turned a move away (beyond-needed?),
  but a search without the limits finds one, also :beyond, the executor's refusal of that path: no path within abilities,
  and the kind of step that would have made one."
  [c pw to range weight policy]
  (let [r (await (plan-from! c pw to range weight (executor/planner-limits policy (wworld/solid-fn pw)) policy))
        within (within-of pw r)]
    (if (beyond-needed? r)
      (with-beyond within pw policy (await (plan-from! c pw to range weight nil policy)))
      within)))

(defn dry-end
  "A partial plan up to its last step out of water: a walk that cannot reach the goal never leaves the body swimming (at a
  bank too high to climb out, say)."
  [steps]
  (let [k (last (keep-indexed (fn [i s] (when-not (:swim s) i)) steps))]
    (if k (subvec steps 0 (inc k)) [])))

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
        steps (cond edge (path-steps walled (:path edge)) past (path-steps walled past) :else steps)
        status (if (or edge past) "partial" (.-status r))
        walked (if (= "partial" status) (dry-end steps) steps)
        step (one-way-of r)]
    {:r r :steps walked :beyond beyond :status status :pw pw
     :ms (or (some-> r .-ms) 0)
     :one-way-taken (when past step)
     :frontier-taken (when (and edge (> (count walked) 1)) (cond-> {:at (:at edge)} (:known edge) (assoc :known true) (:target edge) (assoc :target (:target edge))))
     :searched-out out
     :stop (when (and step (not past) (not edge)) (stopped-one-way r (or (peek walked) (first steps) (wworld/body-cell c)) to step))}))

(defn no-walk
  "The result of a plan that is not walked, nil when it is: no path within abilities (:beyond), a goal the planner proved
  walled in or cut off by a drop (:goal-enclosed, :goal-cut-off: its partial plan's nearer end gets the body no nearer to arriving), a plan cut at a one-way step with no step left, no path, a plan the policy (default
  executor/policy) refuses. replans goes in the result; :damage-refused true when the search found no way for want of
  damage budget (the planner's damageRefused)."
  ([plan replans] (no-walk plan replans executor/policy))
  ([{:keys [r steps beyond status stop searched-out fresh]} replans policy]
   (let [partial? (= "partial" status)]
     (cond
       (= "searching" status)
       (cond-> {:status :searching :replans replans} fresh (assoc :fresh true))

       beyond
       {:status :no-path :reason :abilities :kind (:kind beyond) :at (:at beyond) :replans replans}

       (contains? #{"goal-enclosed" "goal-cut-off"} (some-> r .-reason))
       {:status :no-path :reason (keyword (.-reason r)) :replans replans}

       searched-out
       (cond-> {:status :no-path :reason :exhausted :searched-out true :replans replans}
         (true? (some-> r .-damageRefused)) (assoc :damage-refused true))

       (and stop (< (count steps) 2))
       (assoc stop :replans replans)

       (or (= "none" status) (and partial? (< (count steps) 2)))
       (cond-> {:status :no-path :reason (some-> (.-reason r) keyword) :replans replans}
         (true? (some-> r .-damageRefused)) (assoc :damage-refused true))

       :else
       (some-> (executor/refusal policy steps) (assoc :replans replans))))))
