(ns jobs.lib.walk.search
  "The walk driver's budgeted search (go-to): one bounded slice of search a round, kept per body over the rounds, with the
  known land and the goal flood kept for one goal."
  (:require [engine.heap :as heap]
            [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

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

(defonce ^{:doc "The unfinished budgeted search of each body (keyed by its primitives, so a new body of the same name starts clean): {:key :t :walled :limited :r :unlimited}. key says
  what it plans (start cell, goal, range, weight, policy, walls); t when it began (ms); walled the pathWorld it plans over;
  limited the search within the walker's limits (planner/create-plan), r its result once over, unlimited the search
  without them when that is needed (beyond-needed?)."}
  searches (atom {}))

(defonce ^{:doc "Land go-to's ended searches knew to their end, per body (keyed by its primitives): {:goal [to range] :cells :edges},
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
  (let [who (:primitives c)
        goal [to range]
        kept (get @known-land who)]
    (if (= goal (:goal kept))
      kept
      (let [fresh {:goal goal :cells (js/Set.) :edges (js/Set.)}]
        (swap! known-land assoc who fresh)
        fresh))))

(defonce ^{:doc "Per body: {:key :t :memo}, memo the planner's options.goalFloodMemo shared by go-to's searches toward one
  goal (key [to range weight policy walls mask], mask wworld/unknown-key), kept for search-max-age-ms; forgotten with the known land."}
  goal-floods (atom {}))

(defn known-size "The summed size of the js/Set under k of every body's known land." [k]
  (transduce (map #(if-let [^js s (k %)] (.-size s) 0)) + (vals @known-land)))

(heap/gauge! :known-cells #(known-size :cells))
(heap/gauge! :known-edges #(known-size :edges))
(heap/gauge! :goal-floods #(count @goal-floods))
(heap/gauge! :searches #(count @searches))

(defn forget-known!
  "Forget the body's known land (known-land) and its kept goal flood (goal-floods)."
  [c]
  (let [who (:primitives c)]
    (swap! known-land dissoc who)
    (swap! goal-floods dissoc who)))

(defn release!
  "Forget everything the planner kept for the body (searches, known-land, goal-floods): a go-to call has ended."
  [c]
  (let [who (:primitives c)]
    (swap! searches dissoc who)
    (forget-known! c)))

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

(defn policy-key
  "policy as a search key: a submerged body's :air-seen (a new fn each round) and :air-used (it drops while the body
  waits) would make every round search afresh, so a search that needs more than one round never ends; only whether
  the head was under water counts. A search gone on with plans with the air it began with, so go-on? bounds it
  (air-drift). Likewise a drowning body's hp (:health, set only with the head under) and what follows from it
  (:drown-prices, :damage-weight, :damage-budget, :max-drop) change at every hit: go-on? bounds them (health-drift).
  A :goal-seen fn (go-to's :see, new each round, the goal is in the key) counts only as being there."
  [policy]
  (cond-> (dissoc policy :air-seen :air-used :goal-seen)
    (contains? policy :air-used) (assoc :under true)
    (contains? policy :goal-seen) (assoc :goal-seen true)
    (contains? policy :health) (dissoc :health :drown-prices :damage-weight :damage-budget :max-drop)))

(def air-drift
  "Seconds of air a body may use after its kept search began and still go on with it (one 0.75 s bubble): its plan
  counts the air it began with, and the planner's airLimit leaves airSupply less 3 s."
  1)

(def health-drift
  "Hp a submerged body may lose after its kept search began and still go on with it (one drowning hit): its plan counts
  the hp it began with; the walk's watch replans when the damage ahead is more than the body may spend."
  2)

(defn goal-flood!
  "The body's kept goal flood memo for searches of to within range (goal-floods), a new one when it held another goal or
  is older than search-max-age-ms. mask: the pathWorld's wworld/unknown-key (a masked flood leaks at unseen cells)."
  [c to range weight policy walls & [mask]]
  (let [who (:primitives c)
        k [to range weight (policy-key policy) walls mask]
        kept (get @goal-floods who)]
    (if (and (= k (:key kept)) (< (- (js/Date.now) (:t kept)) search-max-age-ms))
      (:memo kept)
      (let [memo #js {}]
        (swap! goal-floods assoc who {:key k :t (js/Date.now) :memo memo})
        memo))))

(defn search-key [c to range weight policy walls & [pw]]
  (let [{:keys [x y z]} (wworld/body-cell c)]
    (cond-> [[x y z] to range weight (policy-key policy) walls]
      pw (conj (wworld/danger-key pw) (wworld/avoid-key pw) (wworld/tolls-key pw) (wworld/unknown-key pw)))))

(defn goal-unloaded?
  "Whether the snapshot reads the goal cell to [x y z] as unloaded (the planner's goal-unloaded: no goal flood runs)."
  [^js snapshot [x y z]]
  (== planner/UNLOADED (.stateAt snapshot x y z)))

(defn new-search
  "A budgeted search from the body's cell over walled: its limited search begun (wide-box, the policy's limits, known
  land {:cells :edges} as the planner's options.knownCells and knownEdges when not nil); :goal-unloaded whether its
  snapshot read the goal unloaded. With known land (go-to's frontier walks), :edge-stop true: its searches set the
  planner's stopAtEdge, so one toward a goal that is unloaded names the first loaded-edge node it expands as its frontier
  and ends after a sweep of the land round it, not after searching all loaded land."
  [c walled to range weight policy key known & [flood]]
  {:key key :t (js/Date.now) :walled walled :r nil :unlimited nil :air-used (:air-used policy) :health (:health policy)
   :goal-unloaded (goal-unloaded? (.-snapshot walled) to)
   :edge-stop (some? known)
   :limited (planner/create-plan (.-snapshot walled) (wplan/plan-query c to range)
                                 (cond-> (wplan/with-drops (wplan/plan-options walled weight (executor/planner-limits policy (wworld/solid-fn walled)) (wplan/box-of policy)) policy (wworld/landing-seen (:primitives c)))
                                   flood (doto (unchecked-set "goalFloodMemo" flood))
                                   known (doto (unchecked-set "knownCells" (:cells known))
                                               (unchecked-set "knownEdges" (:edges known))
                                               (unchecked-set "stopAtEdge" true))))})

(defn go-on?
  "Whether the kept search goes on at this call: it plans the same (key k), is younger than search-max-age-ms, and did
  not begin with the goal unloaded that pw (this call's pathWorld) has loaded since. A search's snapshot keeps the
  land as it first read it, so one begun with the goal unloaded never floods the goal; a new one can prove it walled in.
  policy: this call's; its :air-used (seconds of air used now, nil with the head out) past the search's own by more than
  air-drift, or its :health (hp now under water) below the search's own by more than health-drift: a new search plans
  with the air or hp left."
  [kept k ^js pw to {:keys [air-used health]}]
  (and (= k (:key kept))
       (<= (- (or air-used 0) (or (:air-used kept) 0)) air-drift)
       (or (nil? health) (nil? (:health kept)) (<= (- (:health kept) health) health-drift))
       (< (- (js/Date.now) (:t kept)) search-max-age-ms)
       (not (and (:goal-unloaded kept) (not (goal-unloaded? (.-snapshot pw) to))))))

(defn ^:async run-search!
  "Run search on for at most budget expansions and round-ms, in slices of chunk-expansions with a yield! between them. [search within]:
  within, plan-within!'s answer, once the search is over (the search without the limits run after the limited one when
  beyond-needed?), else nil and the search to go on with. A search sweeping after its edge stop when the round is spent
  ends there (the planner's endSweep), with its frontier."
  [c search budget policy to range weight]
  (let [walled (:walled search)
        t0 (js/performance.now)]
    (loop [search search used 0]
      (wplan/stop-if-cut! c)
      (let [^js phase (or (:unlimited search) (:limited search))
            over ^boolean (.step phase wplan/chunk-expansions)
            used (+ used wplan/chunk-expansions)]
        (cond
          (and over (:unlimited search))
          [search (wplan/with-beyond (wplan/within-of walled (:r search)) walled policy (.result phase))]

          over
          (let [r (.result phase)]
            (if (wplan/beyond-needed? r)
              (recur (assoc search :r r :unlimited (planner/create-plan (.-snapshot walled) (wplan/plan-query c to range)
                                                                        (cond-> (wplan/with-drops (wplan/plan-options walled weight nil (wplan/box-of policy)) policy (wworld/landing-seen (:primitives c)))
                                                                          (:edge-stop search) (doto (unchecked-set "stopAtEdge" true)))))
                     used)
              [search (wplan/within-of walled r)]))

          (or (>= used budget) (>= (- (js/performance.now) t0) (* round-ms (max 1 (/ budget round-budget)))))
          (if ^boolean (.endSweep phase) (recur search used) [search nil])

          :else (do (await (wplan/yield!))
                    (recur search used)))))))

(defn unfinished-plan
  "The plan-walk! answer of a search still going on: the path to its progress end (planner progress) when that is at least
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
         steps (when path (wplan/dry-end (wplan/path-steps walled path) (wplan/head-wet-fn walled)))
         walk? (>= (count steps) 2)
         r #js {:status (if walk? "partial" "searching") :reason "searching" :path (when walk? path) :ms ms}]
     (cond-> {:r r :status (.-status r) :pw walled :ms ms :steps (when walk? steps)}
       (and walk? past) (assoc :one-way-taken {:kind (nth executor/move-names (.-move ow)) :at [(.-x ow) (.-y ow) (.-z ow)]})))))

(defn ^:async plan-walk-budgeted!
  "plan-walk! that runs at most budget expansions of search (run-search!), going on with the body's unfinished search
  (searches) when it plans the same thing from the same cell. A search that ends is plan-walk!'s answer; one that does not
  is unfinished-plan's (progress, default true: whether it may walk to where the search has got to). With frontier, the
  searches read and add to the body's known land toward the goal (known-land): a frontier in land an earlier search
  knew to its end is walked to only when there is no other (:frontier-taken :known)."
  [c pw to range weight {:keys [policy walls one-way frontier budget progress] :or {progress true}}]
  (let [t (js/performance.now)
        who (:primitives c)
        k (search-key c to range weight policy walls pw)
        kept (get @searches who)
        known (when frontier (known-cells! c to range))
        fresh? (not (go-on? kept k pw to policy))
        search (if-not fresh?
                 kept
                 (new-search c (wworld/with-walls pw walls) to range weight policy k known (goal-flood! c to range weight policy walls (wworld/unknown-key pw))))
        [search within] (await (run-search! c search budget policy to range weight))
        ms (- (js/performance.now) t)]
    (if within
      (do (swap! searches dissoc who)
          (when known (learn-known! known (:r within)))
          (wplan/walk-plan c (:walled search) (:walled search) to one-way frontier within))
      (let [plan (assoc (unfinished-plan search ms one-way progress) :fresh fresh?)]
        (if (= "partial" (:status plan))
          (swap! searches dissoc who)
          (swap! searches assoc who search))
        plan))))
