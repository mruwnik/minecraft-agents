(ns jobs.lib.walk
  "The walk driver: plan from the body's cell to a goal within the executor's abilities, follow the plan with steer, plan
  again when the body ends off it. Jobs that walk (jobs.debug.walk-plan, the stair, tunnel and cleanup jobs) call this
  namespace; walk-to! is the whole loop, the other functions are its pieces."
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]))

(def max-timeout-s 120)
(def max-settle-waits 20)
(def default-weight 1.2)
(def held-in
  "Blocks at the feet that hold a body still enough to plan from: climbables, and water (it swims from there)."
  #{"ladder" "vine" "water"})

(defn path-world
  "The primitives' pathWorld sensing, nil when they have none or cannot sense now."
  [p]
  (when (fn? (.-pathWorld p))
    (.pathWorld p)))

(defn ^:async settle!
  "Wait (100 ms at a time, at most 20 waits) until the body stands on the ground, in a climbable or in water."
  [c]
  (let [p (:primitives c)
        grounded? (fn []
                    (let [s (.self p)
                          pos (.-pos s)
                          b (.blockAt p #js {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))})]
                      (or (.-onGround s) (contains? held-in (some-> b .-name)))))]
    (loop [n 0]
      (when (and (< n max-settle-waits) (not (grounded?)))
        (await (ctx/act c :wait #js {:ms 100}))
        (recur (inc n))))))

(defn path-length
  "Horizontal length of the steps' stand points, in blocks, to 1 decimal."
  [steps]
  (let [d (reduce + (map (fn [a b] (js/Math.hypot (- (:px b) (:px a)) (- (:pz b) (:pz a))))
                         steps (rest steps)))]
    (/ (js/Math.round (* 10 d)) 10)))

(defn pose-of [pose]
  {:x (.-x pose) :y (.-y pose) :z (.-z pose) :vx (.-vx pose) :vy (.-vy pose) :vz (.-vz pose) :on-ground (.-onGround pose)
   :on-climbable (.-onClimbable pose) :in-water (.-inWater pose) :collided (.-collided pose)})

(defn steer-args
  "The act args of one walk. decide is not enumerable: the engine's act wrapper writes (js->clj args) into
  an event, and a function there would break the event stream."
  [timeout-s decide]
  (doto (js-obj "timeoutS" (min max-timeout-s timeout-s))
    (js/Object.defineProperty "decide" #js {:value decide})))

(def centre-tolerance "How near (blocks, each axis) a centring nudge ends to its target." 0.2)
(def centre-max-ticks "Ticks a centring nudge walks before it gives up." 30)

(defn centre-decider
  "A steer decide function that walks straight at [tx tz], done within centre-tolerance of it or after centre-max-ticks."
  [tx tz]
  (let [ticks (volatile! 0)]
    (fn [js-pose]
      (let [dx (- tx (.-x js-pose)) dz (- tz (.-z js-pose))]
        (if (or (and (<= (js/Math.abs dx) centre-tolerance) (<= (js/Math.abs dz) centre-tolerance))
                (>= (vswap! ticks inc) centre-max-ticks))
          #js {:done true}
          #js {:controls #js {:forward true} :yaw (js/Math.atan2 (- dx) (- dz))})))))

(defn ^:async centre!
  "A short walk (no planning, no jump) to within centre-tolerance of [tx tz] on the cell the body is in: the step a
  go-to to a cell cannot make, since a body on the edge of the cell already counts as in it."
  [c tx tz]
  (await (ctx/act c :steer (steer-args 3 (centre-decider tx tz)))))

(defn wall-id
  "A state id of a full block with no collision tricks (stone, the first state that is one), of a state table."
  [table]
  (let [top (.-top table) kind (.-kind table) special (.-special table) openable (.-openable table) hazard (.-hazard table)]
    (first (filter (fn [id] (and (== 16 (aget top id)) (== 1 (aget kind id)) (zero? (aget special id))
                                 (zero? (aget openable id)) (zero? (aget hazard id))))
                   (range 1 (.-length top))))))

(defn wall-cells
  "The cells [x y z] of walls ({:x :y :z} maps), each with the cell over it when the block there now stands taller than
  a block (a fence gate, 1.5): the body can no more jump onto it than through it."
  [snapshot table walls]
  (let [top (.-top table)]
    (into #{} (mapcat (fn [{:keys [x y z]}]
                        (if (> (aget top (.stateAt snapshot x y z)) 16) [[x y z] [x (inc y) z]] [[x y z]])))
          walls)))

(defn with-walls
  "pw (the primitives' pathWorld) with the cells walls ({:x :y :z} maps) read as stone: a cell the walk found it cannot
  pass (wall-cells: the cell over a block taller than a block too)."
  [pw walls]
  (if (empty? walls)
    pw
    (let [snapshot (.-snapshot pw)
          id (wall-id (.-table pw))
          wall? (wall-cells snapshot (.-table pw) walls)
          walled (js/Object.create snapshot)]
      (set! (.-stateAt walled) (fn [x y z] (if (contains? wall? [x y z]) id (.stateAt snapshot x y z))))
      #js {:snapshot walled :table (.-table pw) :space (.-space pw) :dangers (.-dangers pw)})))

(def wide-box
  "The planner's search box (options margin and yMargin, blocks round start and goal) of the walks' searches. A way
  round can run far past the planner's default box (64 and 48). The walks search the wide box from the start:
  A* goes no wider than it must, and a search in the default box that hit its edge would only have to be repeated."
  {:margin 256 :yMargin 96})

(def chunk-expansions
  "Expansions a search of the walking plans (plan-from! and the functions over it) runs between yields to the event
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
  (with-dangers) as options.dangers."
  [pw weight limits box]
  (js/Object.assign #js {:table (.-table pw) :space (.-space pw) :weight weight :limits limits :dangers (.-dangers pw)}
                    (clj->js box)))

(defn with-dangers
  "pw with dangers (the planner's options.dangers, a JS array; nil for none) for plan-options to pass on."
  [pw dangers]
  (if (nil? dangers)
    pw
    #js {:snapshot (.-snapshot pw) :table (.-table pw) :space (.-space pw) :dangers dangers}))

(defn danger-key
  "What a kept search's key holds of pw's dangers: the mob and place of each, rounded to 4 blocks (a mob that moved on,
  died or came along is a new search)."
  [pw]
  (some->> (.-dangers pw) array-seq
           (mapv (fn [^js d] [(.-mob d) (js/Math.round (/ (.-x d) 4)) (js/Math.round (/ (.-y d) 4)) (js/Math.round (/ (.-z d) 4))]))))

(defn plan-from
  "Plan from the body's cell to the goal in wide-box, within limits (the planner's options.limits, nil for none); the
  planner's JS result. In one go: the walks plan with plan-from!, which yields to the event loop."
  [c pw to range weight limits]
  (planner/plan (.-snapshot pw) (plan-query c to range) (plan-options pw weight limits wide-box)))

(defn ^:async plan-from!
  "plan-from in slices (run-plan!), yielding to the event loop between them."
  [c pw to range weight limits]
  (await (run-plan! c (.-snapshot pw) (plan-query c to range) (plan-options pw weight limits wide-box))))

(defn solid-fn
  "solid? for executor/with-free-sides over a pathWorld."
  [pw]
  (let [snapshot (.-snapshot pw) tops (.-top (.-table pw))]
    (fn [x y z] (pos? (aget tops (.stateAt snapshot x y z))))))

(defn path-steps
  "The executor's steps for a planner path over pw: corner free sides and hops, high corners and gap ceilings marked."
  [pw ^js path]
  (let [solid? (solid-fn pw)]
    (executor/with-gap-ceilings
      executor/policy
      (executor/with-high-corners
        executor/policy
        (executor/with-corner-hops (executor/with-free-sides (executor/steps-of (.-steps path)) solid?) solid?)
        solid?)
      solid?)))

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

(defn body-policy
  "executor/policy for the body: with food 6 or less the client does not sprint, so :sprint is false (a corner jump past
  a high block is then refused)."
  [c]
  (let [food (.-food (.self (:primitives c)))]
    (cond-> executor/policy
      (and (number? food) (<= food 6)) (assoc :sprint false))))

(defn plan-within
  "Plan within the executor's abilities (policy, default executor/policy): {:r :steps} (steps nil when r has no path). When
  that finds no whole path, and the limits turned a move away (beyond-needed?), but a search without the limits finds one,
  also :beyond, the executor's refusal of that path: no path within abilities, and the kind of step that would have made
  one."
  ([c pw to range weight] (plan-within c pw to range weight (body-policy c)))
  ([c pw to range weight policy]
   (let [r (plan-from c pw to range weight (executor/planner-limits policy (solid-fn pw)))
         within (within-of pw r)]
     (if (beyond-needed? r)
       (with-beyond within pw policy (plan-from c pw to range weight nil))
       within))))

(defn ^:async plan-within!
  "plan-within with plan-from! (yields to the event loop between search slices)."
  [c pw to range weight policy]
  (let [r (await (plan-from! c pw to range weight (executor/planner-limits policy (solid-fn pw))))
        within (within-of pw r)]
    (if (beyond-needed? r)
      (with-beyond within pw policy (await (plan-from! c pw to range weight nil)))
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

(defn body-cell
  "The cell the body stands in, as a step's {:x :y :z}."
  [c]
  (let [pos (.-pos (.self (:primitives c)))]
    {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))}))

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
  "plan-walk's answer from its plan-within answer {:r :steps :beyond} over walled (pw with the walls).
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
     :stop (when (and step (not past) (not edge)) (stopped-one-way r (or (peek walked) (first steps) (body-cell c)) to step))}))

(defn plan-walk
  "Plan the next walk from where the body stands: plan-within, and for a partial plan only the steps up to its last
  dry step (dry-end). The planner ends a partial plan at the nearest node the body can come back from.
  Answer {:r :steps :beyond :status :stop :one-way-taken}. :stop is the no-path result (stopped-one-way) for a plan
  whose nearer end lies behind a step that cannot be undone, else nil.
  opts:
  - :policy, the executor policy the plan must fit (default executor/policy).
  - :walls, cells {:x :y :z} to read as walls.
  - :one-way :open, to take a one-way step when the land past it runs on into unloaded land (open-path; a far goal
    past a cliff). The plan is then the partial path past it, with no :stop and :one-way-taken {:kind :at}. By default
    it never takes one.
  - :frontier true, so a search that ran out of loaded land walks to its frontier (walk-plan), with
    :frontier-taken {:at [x y z]}."
  ([c pw to range weight] (plan-walk c pw to range weight nil))
  ([c pw to range weight {:keys [policy walls one-way frontier] :or {policy (body-policy c)}}]
   (let [walled (with-walls pw walls)]
     (walk-plan c pw walled to one-way frontier (plan-within c walled to range weight policy)))))

;; ---------------------------------------------------------------- one bounded search a round (go-to)

(def round-budget
  "Expansions (each newly flooded cell of the goal flood counts as one) a budgeted plan-walk! runs in one call, at most
  round-ms of it: about 100 ms of search however dear the land makes an expansion (10-20 us on the bench). A search that
  needs more goes on at the next call (searches)."
  6000)

(def round-ms
  "The time a budgeted plan-walk! stops searching after (checked between slices of chunk-expansions, which take up to
  ~20 ms)."
  80)

(def refresh-rounds
  "Rounds of round-budget a replan during a walk (a refresh, a change) may search: the body stands while it plans, and a
  search from each new cell that stops after one round never ends, so a dead end in view goes unseen until the frontier."
  4)

(defn replan-budget
  "The expansions of one replan's search (nil, none, stays nil) for a round's budget."
  [budget]
  (some-> budget (* refresh-rounds)))

(def progress-blocks
  "Blocks nearer the goal an unfinished search's progress end must be for a budgeted plan-walk! to walk to it."
  8)

(defonce ^{:doc "The unfinished budgeted search of each body (by name): {:key :t :walled :limited :r :unlimited}. key says
  what it plans (start cell, goal, range, weight, policy, walls); t when it began (ms); walled the pathWorld it plans over;
  limited the search within the walker's limits (planner/create-plan), r its result once over, unlimited the search
  without them when that is needed (beyond-needed?)."}
  searches (atom {}))

(defonce ^{:doc "Land go-to's ended searches knew to their end, per body (by name): {:goal [to range] :cells :edges},
  cells a js/Set of the planner's knownKey (options.knownCells), edges one of the cells those searches found at the loaded
  edge and none has known since (options.knownEdges). The loaded land follows the body, so a search from afar reads
  land searched before as a loaded edge again once it has unloaded: without this memory its frontier swings back there
  (live: soak j29, a walled walkway whose far end lay over the goal, 38 rounds end to end). A new goal starts afresh;
  go-to forgets it at its start and after an escalation changed the world (forget-known!). Kept out of job memory,
  which is persisted: it can hold thousands of cells."}
  known-land (atom {}))

(defn known-cells!
  "The body's known land toward to within range (known-land), {:cells :edges}, a new empty one when it held another goal."
  [c to range]
  (let [who (.-username (.self (:primitives c)))
        goal [to range]
        kept (get @known-land who)]
    (if (= goal (:goal kept))
      kept
      (let [fresh {:goal goal :cells (js/Set.) :edges (js/Set.)}]
        (swap! known-land assoc who fresh)
        fresh))))

(defonce ^{:doc "Per body: {:key :t :memo}, memo the planner's options.goalFloodMemo shared by go-to's searches toward one
  goal (key [to range weight policy walls]), kept for search-max-age-ms; forgotten with the known land."}
  goal-floods (atom {}))

(defn forget-known!
  "Forget the body's known land (known-land) and its kept goal flood (goal-floods)."
  [c]
  (let [who (.-username (.self (:primitives c)))]
    (swap! known-land dissoc who)
    (swap! goal-floods dissoc who)))

(defn learn-known!
  "Add the cells a planner result knew to its end (its known, set when it ran out of land) to the known land's cells,
  dropping them from its edges, and add the result's edges that are not known."
  [{:keys [^js cells ^js edges]} ^js r]
  (when-let [^js known (some-> r .-known)]
    (.forEach known (fn [k] (.add cells k) (.delete edges k))))
  (when-let [^js found (some-> r .-edges)]
    (.forEach found (fn [k] (when-not (.has cells k) (.add edges k))))))

(def search-max-age-ms
  "A kept search older than this is not gone on with (the land it read may have changed): a new one begins."
  60000)

(defn body-name [c] (.-username (.self (:primitives c))))

(defn goal-flood!
  "The body's kept goal flood memo for searches of to within range (goal-floods), a new one when it held another goal or
  is older than search-max-age-ms."
  [c to range weight policy walls]
  (let [who (body-name c)
        k [to range weight policy walls]
        kept (get @goal-floods who)]
    (if (and (= k (:key kept)) (< (- (js/Date.now) (:t kept)) search-max-age-ms))
      (:memo kept)
      (let [memo #js {}]
        (swap! goal-floods assoc who {:key k :t (js/Date.now) :memo memo})
        memo))))

(defn search-key [c to range weight policy walls & [pw]]
  (let [{:keys [x y z]} (body-cell c)]
    (cond-> [[x y z] to range weight policy walls]
      pw (conj (danger-key pw)))))

(defn goal-unloaded?
  "Whether the snapshot reads the goal cell to [x y z] as unloaded (the planner's goal-unloaded: no goal flood runs)."
  [^js snapshot [x y z]]
  (== planner/UNLOADED (.stateAt snapshot x y z)))

(defn new-search
  "A budgeted search from the body's cell over walled: its limited search begun (wide-box, the policy's limits, known
  land {:cells :edges} as the planner's options.knownCells and knownEdges when not nil); :goal-unloaded whether its
  snapshot read the goal unloaded. With known land (go-to's frontier walks), :edge-stop true: its searches set the
  planner's stopAtEdge, so one toward a goal that is unloaded ends at the first loaded-edge node it expands, its frontier,
  not after searching all loaded land (card 7a031d15: over max-searching rounds when no node got progress-blocks nearer)."
  [c walled to range weight policy key known & [flood]]
  {:key key :t (js/Date.now) :walled walled :r nil :unlimited nil
   :goal-unloaded (goal-unloaded? (.-snapshot walled) to)
   :edge-stop (some? known)
   :limited (planner/create-plan (.-snapshot walled) (plan-query c to range)
                                 (cond-> (plan-options walled weight (executor/planner-limits policy (solid-fn walled)) wide-box)
                                   flood (doto (unchecked-set "goalFloodMemo" flood))
                                   known (doto (unchecked-set "knownCells" (:cells known))
                                               (unchecked-set "knownEdges" (:edges known))
                                               (unchecked-set "stopAtEdge" true))))})

(defn go-on?
  "Whether the kept search goes on at this call: it plans the same (key k), is younger than search-max-age-ms, and did
  not begin with the goal unloaded that pw (this call's pathWorld) has loaded since. A search's snapshot keeps the
  land as it first read it, so one begun with the goal unloaded never floods the goal; a new one can prove it walled in."
  [kept k ^js pw to]
  (and (= k (:key kept))
       (< (- (js/Date.now) (:t kept)) search-max-age-ms)
       (not (and (:goal-unloaded kept) (not (goal-unloaded? (.-snapshot pw) to))))))

(defn ^:async run-search!
  "Run search on for at most budget expansions and round-ms, in slices of chunk-expansions with a yield! between them. [search within]:
  within, plan-within's answer, once the search is over (the search without the limits run after the limited one when
  beyond-needed?), else nil and the search to go on with."
  [c search budget policy to range weight]
  (let [walled (:walled search)
        t0 (js/performance.now)]
    (loop [search search used 0]
      (stop-if-cut! c)
      (let [^js phase (or (:unlimited search) (:limited search))
            over ^boolean (.step phase chunk-expansions)
            used (+ used chunk-expansions)]
        (cond
          (and over (:unlimited search))
          [search (with-beyond (within-of walled (:r search)) walled policy (.result phase))]

          over
          (let [r (.result phase)]
            (if (beyond-needed? r)
              (recur (assoc search :r r :unlimited (planner/create-plan (.-snapshot walled) (plan-query c to range)
                                                                        (cond-> (plan-options walled weight nil wide-box)
                                                                          (:edge-stop search) (doto (unchecked-set "stopAtEdge" true)))))
                     used)
              [search (within-of walled r)]))

          (or (>= used budget) (>= (- (js/performance.now) t0) (* round-ms (max 1 (/ budget round-budget))))) [search nil]

          :else (do (await (yield!))
                    (recur search used)))))))

(defn unfinished-plan
  "The plan-walk answer of a search still going on: the path to its progress end (planner progress) when that is at least
  progress-blocks nearer the goal than the start, walked as a partial plan; else status \"searching\" with no steps
  (no-walk: :searching), and the search goes on at the next call. With one-way :open, a progress whose nearest node lies
  past a step the body cannot undo and stands at the loaded edge (progress oneWay.open, the rule open-path applies to a
  search that ended) walks the path to that node instead when it is progress-blocks nearer, with :one-way-taken (live: a
  gap jump down as go-to's first move kept every round of a 300-block search from walking, and it gave up :searching).
  The search without the limits has no progress to walk, nor has one with progress false (go-to after a walk to a
  frontier: the nearest node of a search that has not ended may be the dead end an ended one walked away from)."
  ([search ms] (unfinished-plan search ms nil true))
  ([search ms one-way] (unfinished-plan search ms one-way true))
  ([search ms one-way progress]
   (let [walled (:walled search)
         ^js pr (when (and progress (not (:unlimited search))) (.progress ^js (:limited search)))
         nearer? (fn [distance] (>= (- (.-startDistance pr) distance) progress-blocks))
         ^js ow (when pr (.-oneWay pr))
         past (when (and (= :open one-way) ow (true? (.-open ow)) (nearer? (.-distance ow))) (.-path ow))
         path (or past (when (and pr (.-path pr) (nearer? (.-distance pr))) (.-path pr)))
         steps (when path (dry-end (path-steps walled path)))
         walk? (>= (count steps) 2)
         r #js {:status (if walk? "partial" "searching") :reason "searching" :path (when walk? path) :ms ms}]
     (cond-> {:r r :status (.-status r) :pw walled :ms ms :steps (when walk? steps)}
       (and walk? past) (assoc :one-way-taken {:kind (nth executor/move-names (.-move ow)) :at [(.-x ow) (.-y ow) (.-z ow)]})))))

(defn ^:async plan-walk-budgeted!
  "plan-walk! that runs at most budget expansions of search (run-search!), going on with the body's unfinished search
  (searches) when it plans the same thing from the same cell. A search that ends is plan-walk's answer; one that does not
  is unfinished-plan's (progress, default true: whether it may walk to where the search has got to). With frontier, the
  searches read and add to the body's known land toward the goal (known-land): a frontier in land an earlier search
  knew to its end is walked to only when there is no other (:frontier-taken :known)."
  [c pw to range weight {:keys [policy walls one-way frontier budget progress] :or {progress true}}]
  (let [t (js/performance.now)
        who (body-name c)
        k (search-key c to range weight policy walls pw)
        kept (get @searches who)
        known (when frontier (known-cells! c to range))
        fresh? (not (go-on? kept k pw to))
        search (if-not fresh?
                 kept
                 (new-search c (with-walls pw walls) to range weight policy k known (goal-flood! c to range weight policy walls)))
        [search within] (await (run-search! c search budget policy to range weight))
        ms (- (js/performance.now) t)]
    (if within
      (do (swap! searches dissoc who)
          (when known (learn-known! known (:r within)))
          (walk-plan c (:walled search) (:walled search) to one-way frontier within))
      (let [plan (assoc (unfinished-plan search ms one-way progress) :fresh fresh?)]
        (if (= "partial" (:status plan))
          (swap! searches dissoc who)
          (swap! searches assoc who search))
        plan))))

(defn ^:async plan-walk!
  "plan-walk with plan-within! (yields to the event loop between search slices): what the walks (jobs.lib.near, walk-to!)
  plan with, so a long search never holds the body's API. With :budget (go-to: round-budget), one call searches at most
  that many expansions (plan-walk-budgeted!): a search that needs more walks to where it has got to, or nowhere
  (\"searching\"), and goes on at the next call; with :progress false only nowhere until the search ends."
  ([c pw to range weight] (plan-walk! c pw to range weight nil))
  ([c pw to range weight {:keys [policy walls one-way frontier budget] :or {policy (body-policy c)} :as opts}]
   (if budget
     (await (plan-walk-budgeted! c pw to range weight (assoc opts :policy policy)))
     (let [walled (with-walls pw walls)]
       (walk-plan c pw walled to one-way frontier (await (plan-within! c walled to range weight policy)))))))

(defn no-walk
  "The result of a plan that is not walked, nil when it is: no path within abilities (:beyond), a goal the planner proved
  walled in (:goal-enclosed: its partial plan's nearer end gets the body no nearer to arriving), a plan cut at a one-way step with no step left, no path, a plan the policy (default
  executor/policy) refuses. replans goes in the result."
  ([plan replans] (no-walk plan replans executor/policy))
  ([{:keys [r steps beyond status stop searched-out fresh]} replans policy]
   (let [partial? (= "partial" status)]
     (cond
       (= "searching" status)
       (cond-> {:status :searching :replans replans} fresh (assoc :fresh true))

       beyond
       {:status :no-path :reason :abilities :kind (:kind beyond) :at (:at beyond) :replans replans}

       (= "goal-enclosed" (some-> r .-reason))
       {:status :no-path :reason :goal-enclosed :replans replans}

       searched-out
       {:status :no-path :reason :exhausted :searched-out true :replans replans}

       (and stop (< (count steps) 2))
       (assoc stop :replans replans)

       (or (= "none" status) (and partial? (< (count steps) 2)))
       {:status :no-path :reason (some-> (.-reason r) keyword) :replans replans}

       :else
       (some-> (executor/refusal policy steps) (assoc :replans replans))))))

;; ---------------------------------------------------------------- the look-ahead (watch)

(def watch-policy
  "The look-ahead's numbers. window: plan legs ahead whose cells are checked; check-every: on a long straight or diagonal leg,
  ticks between checks (a check also runs whenever the body reaches a step); min-refresh-ticks: a partial plan is planned
  again at most this often (4 s), planner-share: and the planner gets at most this share of the walk (a slow plan spaces the
  refreshes out); better-by: a refreshed partial plan is taken only when its end is this many blocks nearer the goal; mob-waits
  and mob-wait-ms: a body stuck behind a mob waits this often this long for it to move on."
  {:window 10 :check-every 5 :min-refresh-ticks 80 :planner-share 0.05 :tick-ms 50 :better-by 2
   :mob-waits 3 :mob-wait-ms 1000 :mob-reach 2.5})

(def max-watch-replans
  "Replans a look-ahead may start in one follow! (changes, refreshes, mobs); past it the plan is walked unwatched."
  12)

(def no-stop-moves
  "Steps a walk is never stopped before or on: the body is in the air, on a ladder, swimming, or in a gap's run-up."
  #{:gap :climb-up :climb-down :jump-climb :open :swim :swim-up :swim-down :exit})

(def body-half 0.3)

(defn step-cells
  "The cells [x y z] the body passes going from prev to step: the columns its footprint (body-half either side) touches
  along the line between their stand points, from the floor under the lower one to two over the higher one's feet."
  [prev step]
  (let [ax (:px prev) az (:pz prev) bx (:px step) bz (:pz step)
        n (max 1 (js/Math.ceil (/ (js/Math.hypot (- bx ax) (- bz az)) 0.25)))
        cols (into #{} (for [k (range (inc n))
                             :let [t (/ k n) x (+ ax (* t (- bx ax))) z (+ az (* t (- bz az)))]
                             dx [(- body-half) body-half] dz [(- body-half) body-half]]
                         [(js/Math.floor (+ x dx)) (js/Math.floor (+ z dz))]))
        lo (dec (min (:y prev) (:y step)))
        hi (+ 2 (max (:y prev) (:y step)))]
    (for [[x z] cols y (range lo (inc hi))] [x y z])))

(defn opens-cells
  "The cells of the blocks the steps open by hand, with the cell over and under each (a door's other half): the walker
  changes them itself."
  [steps]
  (into #{} (for [s steps {:keys [x y z]} (:opens s) dy [-1 0 1]] [x (+ y dy) z])))

(defn window-cells
  "The distinct cells of the legs into steps i .. i+n-1 (each from the step before it), without the cells the steps open."
  [steps i n]
  (let [skip (opens-cells steps)]
    (->> (range (max 1 i) (min (count steps) (+ i n)))
         (mapcat (fn [k] (step-cells (nth steps (dec k)) (nth steps k))))
         (remove skip)
         distinct)))

(defn state-keys
  "The names of the state table's per-state arrays that the planner reads (every typed array with one entry per state, but
  boxStart, an index into boxes)."
  [table]
  (let [n (.-length (.-top table))]
    (filterv (fn [k] (let [a (unchecked-get table k)]
                       (and (js/ArrayBuffer.isView a) (== n (.-length a)) (not= k "boxStart"))))
             (js/Object.keys table))))

(def state-keys-of (memoize state-keys))

(defn same-boxes? [table a b]
  (let [boxes (.-boxes table) starts (.-boxStart table) n (* 6 (aget (.-boxCount table) a))
        sa (* 6 (aget starts a)) sb (* 6 (aget starts b))]
    (every? (fn [k] (== (aget boxes (+ sa k)) (aget boxes (+ sb k)))) (range n))))

(defn same-for-planner?
  "Whether the state ids a and b are the same to the planner: equal, or equal in every per-state array of the table and in
  their collision boxes (a crop's age is no change; a block placed, dug, a door shut or opened is). An id outside the
  table (unloaded) is the same only as itself."
  [table a b]
  (or (== a b)
      (let [n (.-length (.-top table))]
        (and (< a n) (< b n)
             (every? (fn [k] (let [arr (unchecked-get table k)] (== (aget arr a) (aget arr b)))) (state-keys-of table))
             (same-boxes? table a b)))))

(defn boundary?
  "Whether a walk may stop here to plan again: the body stands on the ground (not in the air, water or on a climbable), the
  step it heads for (i2, i before this tick) and the one it left are none of no-stop-moves, and it has just reached a step
  (i2 > i) or, every check-every ticks, walks a plain straight or diagonal leg."
  [steps i i2 tick {:keys [on-ground in-water on-climbable]}]
  (let [target (:move (get steps i2))
        left (:move (get steps (dec i2)))]
    (boolean (and on-ground (not in-water) (not on-climbable) (pos? i2)
                  (not (contains? no-stop-moves target)) (not (contains? no-stop-moves left))
                  (or (> i2 i)
                      (and (zero? (mod tick (:check-every watch-policy))) (contains? #{:walk :diagonal} target)))))))

(defn refresh-ticks
  "Ticks between refreshes of a partial plan whose last plan took ms: at least min-refresh-ticks, more for slow plans."
  [ms]
  (let [{:keys [min-refresh-ticks planner-share tick-ms]} watch-policy]
    (max min-refresh-ticks (js/Math.ceil (/ ms (* planner-share tick-ms))))))

(defn refresh-due?
  "A partial plan walked ticks ticks is due to be planned again after interval ticks; a whole plan never is."
  [status ticks interval]
  (and (= "partial" status) (>= ticks interval)))

(defn unfinished?
  "Whether plan walks to where a search still going on has got to (unfinished-plan: its r's reason \"searching\")."
  [plan]
  (= "searching" (some-> ^js (:r plan) .-reason)))

(defn take-refresh?
  "Whether a refreshed plan (:status :steps) replaces the old steps: a whole plan always, a partial one when its end is at
  least better-by blocks nearer the goal to than the old end (no weaving between near-equal ends). With old-plan, the plan
  in force: a plan whose search ended (a walk to its frontier, its nearest end) is never replaced by an unfinished search's
  walk (unfinished?): that one knows less, and its nearest node may be the dead end the ended search left. A walk to a
  frontier is replaced by one to another frontier of an ended search, however far its end: the old one is a dead end."
  ([old-steps fresh to] (take-refresh? old-steps fresh to nil))
  ([old-steps {:keys [status steps frontier-taken] :as fresh} to old-plan]
   (and (>= (count steps) 2)
        (not (and old-plan (unfinished? fresh) (not (unfinished? old-plan))))
        (or (= "found" status)
            (and frontier-taken (:frontier-taken old-plan) (not (unfinished? fresh))
                 (not= (:at frontier-taken) (:at (:frontier-taken old-plan))))
            (<= (+ (near-goal (peek steps) to) (:better-by watch-policy)) (near-goal (peek old-steps) to))))))

(defn watch-stop
  "The look-ahead at one tick: nil, or the done map that stops the walk to plan again: {:status :replan :why :changed :cells}
  when a cell of the window ahead differs for the planner between the plan's snapshot (base) and a fresh one, else
  {:status :replan :why :refresh} for a partial plan due a refresh. Only at a boundary?. steps: the steps walked; i the
  executor's index before the tick; state its state after; watch {:base :fresh :ahead :skip :status :interval}."
  [{:keys [base fresh ahead skip status interval]} steps i {i2 :i tick :tick} pose]
  (when (boundary? steps i i2 tick pose)
    (let [all (into steps ahead)
          here (assoc all (dec i2) (assoc (nth all (dec i2)) :px (:x pose) :pz (:z pose)))
          ^js now (fresh)
          at [(:x pose) (:y pose) (:z pose)]
          changed (when now
                    (let [^js bs (.-snapshot base) ^js ns (.-snapshot now) table (.-table base)]
                      (filterv (fn [[x y z]] (not (same-for-planner? table (.stateAt bs x y z) (.stateAt ns x y z))))
                               (remove (or skip #{}) (window-cells here i2 (:window watch-policy))))))]
      (cond
        (seq changed) {:status :replan :why :changed :cells changed :at at :step i2}
        (refresh-due? status tick interval) {:status :replan :why :refresh :at at :step i2}))))

(defn ^:async walk!
  "Follow steps once. [result ms]: the executor's done map, or {:status :stuck ...} on a timeout,
  {:status :failed ...}; ms is the wall time of the steer act. With a watch (see watch-stop), the walk also stops at a step
  boundary with {:status :replan ...} when the way ahead changed or a partial plan is due a refresh."
  ([c steps timeout-s] (walk! c steps timeout-s nil))
  ([c steps timeout-s watch]
  (let [policy (body-policy c)
        state (volatile! (executor/start steps 0 (let [pos (.-pos (.self (:primitives c)))] {:x (.-x pos) :z (.-z pos)})))
        last-done (volatile! nil)
        decide (fn [js-pose]
                 (let [pose (pose-of js-pose)
                       i (:i @state)
                       {:keys [state' done controls yaw pitch]}
                       (let [r (executor/tick policy @state pose)]
                         {:state' (:state r) :done (:done r) :controls (:controls r) :yaw (:yaw r) :pitch (:pitch r)})
                       done (or done (when watch (watch-stop watch steps i state' pose)))]
                   (vreset! state state')
                   (if done
                     (do (vreset! last-done done) #js {:done (clj->js done)})
                     #js {:controls (clj->js controls) :yaw yaw :pitch pitch})))
        t (js/Date.now)
        r (await (ctx/act c :steer (steer-args timeout-s decide)))
        ms (- (js/Date.now) t)]
    [(case (.-status r)
       "done" @last-done
       "timeout" {:status :stuck :why (str "walk timed out after " timeout-s " s")
                  :at (let [p (.-pose r)] [(.-x p) (.-y p) (.-z p)])}
       {:status :failed :reason (.-reason r)})
     ms])))

(defn partial-end
  "A partial plan that was walked to its end is not an arrival unless the body is in range of the goal: then the walk
  plans again, unless the plan was cut at a step that cannot be undone, which ends it (stopped-one-way)."
  [done planner-status to range steps stop]
  (let [[x y z] (:at done)]
    (cond
      (not (and (= :arrived (:status done)) (= "partial" planner-status)
                (not (u/within? {:x x :y y :z z} (zipmap [:x :y :z] to) range))))
      done

      stop (merge stop {:at (:at done)})

      :else {:status :off-plan :at (:at done) :step (dec (count steps))})))

(defn watch-of
  "The look-ahead for walking plan: its own snapshot as the base, a fresh pathWorld per check, the cells the plan opens
  skipped, and the refresh interval from the plan's ms."
  [c plan]
  {:base (:pw plan) :fresh #(path-world (:primitives c)) :ahead [] :skip (opens-cells (:steps plan))
   :status (:status plan) :interval (refresh-ticks (:ms plan))})

(defn mob-cells
  "The cells {:x :y :z} (feet and head) of the entities, not items and not the body, that stand on the leg the body is
  stuck on or the next one (steps k-1 to k+1) within mob-reach of the body."
  [c steps k]
  (let [p (:primitives c)
        me (.-username (.self p))
        here (.-pos (.self p))
        legs (into #{} (mapcat (fn [j] (when (< 0 j (count steps)) (step-cells (nth steps (dec j)) (nth steps j)))))
                   [k (inc k)])]
    (vec (for [^js e (array-seq (.entities p #js {:radius 8 :max 64}))
               :let [pos (.-pos e)
                     cell [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))]]
               :when (and (not= "item" (.-kind e)) (not= me (.-username e)) (contains? legs cell)
                          (<= (js/Math.hypot (- (.-x pos) (.-x here)) (- (.-z pos) (.-z here))) (:mob-reach watch-policy)))
               dy [0 1]]
           {:x (cell 0) :y (+ dy (cell 1)) :z (cell 2)}))))

(defn ^:async replan-round-mob!
  "A walk stuck with mobs on its way: wait for them (mob-wait-ms, at most mob-waits times) and plan with the cells they
  stand in as walls; the first plan that can be walked, or, after the waits, a plan with no walls. [plan ms] (ms the
  planning time), the plan possibly not walkable."
  [c plan-fn walkable? steps k]
  (let [{:keys [mob-waits mob-wait-ms]} watch-policy]
    (loop [n 1]
      (await (ctx/act c :wait #js {:ms mob-wait-ms}))
      (let [t (js/performance.now)
            walls (mob-cells c steps k)
            plan (await (plan-fn walls))
            ms (- (js/performance.now) t)]
        (if (or (walkable? plan) (>= n mob-waits))
          (if (or (walkable? plan) (empty? walls))
            [plan ms]
            (let [t (js/performance.now) plan (await (plan-fn []))] [plan (- (js/performance.now) t)]))
          (recur (inc n)))))))

(defn ^:async follow!
  "Walk plan (a plan-walk result) and plan again from the body's cell, in the same call, whenever the walk stops for it:
  the look-ahead saw the way change (:changed: the new plan is walked), a partial plan is due a refresh (:refresh: the new
  plan is walked only when take-refresh? says it is clearly better, else the rest of the old one), or the body is stuck with
  a mob in its way (:mob: wait, plan round it). At most max-watch-replans; past that the plan is walked unwatched.
  opts: :plan-fn (fn [walls]) -> a plan-walk result (or a promise of one: plan-walk!) from where the body stands now, the
  cells {:x :y :z} read as walls;
  :walk-fn (fn [steps watch]) -> [done ms] (walk! or jobs.lib.pass/walk!); :to the goal cell; :policy for no-walk;
  :announce! (fn [:replan data]) per replan, data {:why :ms :kept :replans :at :text}.
  {:done :plan :ms :walked :replans}: done the last walk's done map, or the no-walk result of a replan that has no way; plan
  the plan in force at the end; ms the time in walks; walked the blocks of plan walked."
  [c plan {:keys [plan-fn walk-fn to policy announce!] :or {policy (body-policy c) announce! (fn [_ _])}}]
  (let [walkable? (fn [pl] (nil? (no-walk pl 0 policy)))
        cut-length (fn [steps k] (path-length (subvec steps 0 (min (count steps) (max 1 k)))))]
    (loop [plan plan steps (:steps plan) n 0 ms 0 walked 0]
      (let [watch (when (< n max-watch-replans) (watch-of c plan))
            [done wms] (await (walk-fn steps watch))
            ms (+ ms wms)
            k (or (:step done) (count steps))
            walked' (+ walked (cut-length steps k))
            finish (fn [d pl w] {:done d :plan pl :ms ms :walked w :replans n})
            tell! (fn [why plan-ms kept]
                    (announce! :replan {:why why :ms (/ (js/Math.round (* 10 plan-ms)) 10) :kept kept :replans (inc n)
                                        :at (:at done)
                                        :text (str "re-plan " (inc n) " (" (name why) (when kept ", kept the plan") ")")}))]
        (cond
          (= :replan (:status done))
          (let [t (js/performance.now)
                fresh (await (plan-fn []))
                plan-ms (- (js/performance.now) t)
                fresh (assoc fresh :ms plan-ms)]
            (if (= :refresh (:why done))
              (let [take? (and (walkable? fresh) (take-refresh? steps fresh to plan))]
                (tell! :refresh plan-ms (not take?))
                (if take?
                  (recur fresh (:steps fresh) (inc n) ms walked')
                  (recur (assoc plan :ms plan-ms) (subvec steps (max 0 (dec k))) (inc n) ms walked')))
              (do (tell! :changed plan-ms false)
                  (if (walkable? fresh)
                    (recur fresh (:steps fresh) (inc n) ms walked')
                    (finish (no-walk fresh 0 policy) fresh walked')))))

          (and (= :stuck (:status done)) (< n max-watch-replans) (seq (mob-cells c steps k)))
          (let [[fresh plan-ms] (await (replan-round-mob! c plan-fn walkable? steps k))]
            (tell! :mob plan-ms false)
            (if (walkable? fresh)
              (recur fresh (:steps fresh) (inc n) ms walked')
              (finish done plan walked')))

          :else (finish done plan walked'))))))

(defn ^:async walk-to!
  "Walk the body to within range of the goal cell to ([x y z]): settle, plan, follow the plan (follow!: at most timeout-s
  seconds a walk, planning again in the same call when the way ahead changes, a partial plan is refreshed or a mob is in the
  way), plan again when the body ends off it (at most 5 times). The caller has checked that path-world is there.
  {:result :walked :walk-ms}: result is {:status ...} as in the walk-plan job doc, with :replans; walked is the blocks of
  every plan followed, walk-ms the time inside steer acts. announce! is called (kind data) with :plan before each walk,
  :replan when the body is off its plan and for each of follow!'s replans (those have :why)."
  [c {:keys [to range weight timeout-s announce!] :or {announce! (fn [_ _])}}]
  (let [p (:primitives c)
        plan-fn (fn [walls] (plan-walk! c (path-world p) to range weight {:walls walls}))
        end (fn [result walked walk-ms] {:result result :walked walked :walk-ms walk-ms})]
    (loop [replans 0 walked 0 walk-ms 0]
      (await (settle! c))
      (let [plan (await (plan-fn []))]
        (if-let [no (no-walk plan replans (body-policy c))]
          (end no walked walk-ms)
          (let [{:keys [r steps status]} plan]
            (announce! :plan {:steps (count steps) :summary (some-> (.-path r) .-summary js->clj)
                              :status status :ms (.-ms r) :replans replans
                              :text (str "plan " status ", " (count steps) " steps")})
            (let [{walk-result :done last-plan :plan ms :ms length :walked}
                  (await (follow! c plan {:plan-fn plan-fn :to to :announce! announce!
                                          :walk-fn (fn [steps watch] (walk! c steps timeout-s watch))}))
                  walked (+ walked length)
                  walk-ms (+ walk-ms ms)
                  done (partial-end walk-result (:status last-plan) to range (:steps last-plan) (:stop last-plan))
                  after (if (#{:arrived :off-plan :stuck} (:status done))
                          (executor/after-walk executor/policy replans done)
                          {:finish done})]
              (if-let [n (:replan after)]
                (do (announce! :replan {:at (:at done) :replans n :text (str "off the plan, re-plan " n)})
                    (recur n walked walk-ms))
                (end (assoc (:finish after) :replans replans) walked walk-ms)))))))))
