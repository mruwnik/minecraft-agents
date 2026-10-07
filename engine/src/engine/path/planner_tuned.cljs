(ns engine.path.planner-tuned
  "The path planner's search: an A* over typed arrays of cells, moves and costs (climbing, water, doors, tight
   cells). It returns a status, reason, expanded count, path, costs and summary. Written for speed.

   How it stays fast:
   - All search state is in the mutable fields of one Search object (engine.path.planner.search), and every function of
     the search is a method of it (added by part in the engine.path.planner.* namespaces). The emitted JavaScript is property reads, typed-array indexing and direct method calls.
   - No persistent data, no seqs, no keywords in the hot code. Every test is a comparison or a ^boolean hinted call,
     so no truthiness check is emitted (the compiled planner has one cljs.core.truth_, in count-steps).
   - `let`, `do` and `loop` only in statement or tail position: in expression position they compile to a closure
     called on the spot.
   - Values that a JS function returns on the side (support, touch, enter-risk, enter-slow, enter-extra, ty) are
     fields. Where a closure would be passed (swimEdge's emit, the opening pass's edge) the call is split in a begin
     and an end, or `edge-mode` is switched.

   Called through interop, not written in the search: the snapshot (stateAt, sectionHas, hasColumn), the state table's
   arrays, and the free-space masks of space.mjs (options.space: boxesNear, freeMask, labelRegions).

   Goal sets: query.goals is an array of goals ({kind x y z range}, like query.goal). One search plans to the nearest
   of them by cost. The goal test is the cell in any goal's area, and the heuristic is the least of the goals'
   (admissible and consistent, as each is). A found result's goal is the index of the goal reached. A goal set runs no
   goal flood and answers no goal-not-standable or goal-unloaded, since those prove things about one goal. query.goal
   is not read then.

   Options:
   - options.avoid {kinds, cells, factor}: a search for another path (see avoidCost).
   - options.limits {kinds, gap, corner}: what the walker can do. Without it the search is unrestricted.
       kinds: bits of the move kinds never planned.
       gap(x, y, z, h, move, lx, ly, lz, lh): false refuses a gap jump from the takeoff node (feet cell x,y,z, stand h
         in 1/16, reached by move) to the landing.
       corner(x, y, z, h, lx, ly, lz, lh): false refuses a diagonal jump with one blocked side (a corner slide).
     engine.path.executor/planner-limits builds them.
   - options.knownCells: a Set of the cells (knownKey) earlier searches toward the same goal knew to their end. The
     frontier avoids them while another edge is loaded (frontierNode, result frontier.known), and a search that ends
     exhausted puts the cells it adds in the result's known (a Set; nil without knownCells), and the cells of its
     nodes at the loaded edge that are not known in the result's edges. jobs.lib.walk keeps both for go-to.
   - options.knownEdges: a Set of the cells (knownKey) earlier searches found at the loaded edge that no search has
     known to its end since. When it is empty, a frontier in known land is never taken: every edge was searched past,
     and the result says so as searchedOut true (the way on, if any, is not in the land this goal's searches can reach).
   - options.dangers: known hostiles, an array of {x y z close radius rate} (at most 8): a move adds to its risk rate
     (hp a second) times its seconds within close blocks of a danger's point, falling linearly to 0 at radius; all the
     dangers together add at most options.dangerCap (4) a second (see dangerRisk). jobs.lib.threats builds them.
   - options.dark {at, factor}: a cell that at(x, y, z) calls dark (returns 1) costs factor times its own seconds
     more, in g and in cost.darkSeconds (see darkOf; jobs.lib.look builds at).
   - options.tolls {cells}: cells is a Map of cell-key (planner/cell-key) to a factor: entering such a cell costs factor
     times its own seconds more, in g and in cost.darkSeconds (the caller's price of a cell; no zone knowledge here).
   - options.costs.dropFactor (1): scales the fall seconds and fall damage of every drop on land (0: free); options.maxDrop
     (3) refuses a drop of more than that many blocks (1: none of 2 or 3). go-to's :drop-cost sets them.
   - options.stopAtEdge: with the goal unloaded, the search ends at the first node it expands at the loaded edge (edgeStop)
     and names it as its frontier, not after searching all loaded land. go-to's budgeted searches set it (walk.search/new-search)."
  (:require [engine.path.planner.base :as base :refer [OCTILE-SLACK REGIONS TABLE WHOLE next-pow2]]
            [engine.path.planner.search :refer [Search ->Search]]
            [engine.path.planner.world]
            [engine.path.planner.nodes]
            [engine.path.planner.tight]
            [engine.path.planner.bends]
            [engine.path.planner.water]
            [engine.path.planner.moves]
            [engine.path.planner.doors]
            [engine.path.planner.flood]
            [engine.path.planner.run]
            [engine.path.planner.results]
            [engine.path.planner.danger]
            [engine.path.planner.dark]))

(set! *warn-on-infer* true)

;; the planner's public constants
(def UNLOADED base/UNLOADED)
(def AVOID-CLIMB base/AVOID-CLIMB)
(def AVOID-WATER base/AVOID-WATER)
(def AVOID-OPEN base/AVOID-OPEN)
(def MOVE base/MOVE)
(def DEFAULT-COSTS base/DEFAULT-COSTS)
(def cell-key base/cell-key)

(defn- option [^js options k default]
  (let [v (unchecked-get options k)]
    (if (undefined? v) default v)))

(defn- or-else [v default] (if (some? v) v default))

(def ^:const KEY-REACH 1000) ; knownKey holds cells within 1024 of the goal along x and z

(defn- reach-of
  "How far from the goal (along x and z) a frontier node may lie: options.frontierReach (256) past the start's own
  distance, so the land round the body always counts (a way down far behind it on a walkway), at most KEY-REACH."
  [^js options ^js from ^js goal]
  (js/Math.min KEY-REACH (+ (option options "frontierReach" 256)
                            (js/Math.max (js/Math.abs (- (.-x from) (.-x goal))) (js/Math.abs (- (.-z from) (.-z goal)))))))

(defn- goal-bounds
  "#js [x0 x1 z0 z1 y0 y1]: the extent of the start and the goals (y: the start's and the sphere goals')."
  [^js from ^js goals]
  (let [b #js [(.-x from) (.-x from) (.-z from) (.-z from) (.-y from) (.-y from)]]
    (doseq [^js g (array-seq goals)]
      (aset b 0 (js/Math.min (aget b 0) (.-x g)))
      (aset b 1 (js/Math.max (aget b 1) (.-x g)))
      (aset b 2 (js/Math.min (aget b 2) (.-z g)))
      (aset b 3 (js/Math.max (aget b 3) (.-z g)))
      (when (identical? (.-kind g) "near")
        (aset b 4 (js/Math.min (aget b 4) (.-y g)))
        (aset b 5 (js/Math.max (aget b 5) (.-y g)))))
    b))

(defn- goal-array [ctor ^js goals f] (new ctor (.map goals f)))

(def ^:const DANGER-STRIDE 6)
(def ^:const MAX-DANGERS 8)

(defn- danger-array
  "options.dangers ({x y z close radius rate}, at most MAX-DANGERS used) as a Float64Array of DANGER-STRIDE numbers per
  danger; nil for none."
  [^js ds]
  (when (and (some? ds) (pos? (.-length ds)))
    (let [n (js/Math.min MAX-DANGERS (.-length ds))
          a (js/Float64Array. (* n DANGER-STRIDE))]
      (dotimes [i n]
        (let [^js d (aget ds i)
              o (* i DANGER-STRIDE)]
          (aset a o (.-x d)) (aset a (+ o 1) (.-y d)) (aset a (+ o 2) (.-z d))
          (aset a (+ o 3) (.-close d)) (aset a (+ o 4) (.-radius d)) (aset a (+ o 5) (.-rate d))))
      a)))

(defn- danger-box
  "#js [x0 x1 y0 y1 z0 z1]: the box round every danger's radius (an empty box for none)."
  [^js a]
  (let [b #js [js/Infinity js/-Infinity js/Infinity js/-Infinity js/Infinity js/-Infinity]]
    (when (some? a)
      (dotimes [i (/ (.-length a) DANGER-STRIDE)]
        (let [o (* i DANGER-STRIDE)
              r (aget a (+ o 4))]
          (aset b 0 (js/Math.min (aget b 0) (- (aget a o) r)))
          (aset b 1 (js/Math.max (aget b 1) (+ (aget a o) r)))
          (aset b 2 (js/Math.min (aget b 2) (- (aget a (+ o 1)) r)))
          (aset b 3 (js/Math.max (aget b 3) (+ (aget a (+ o 1)) r)))
          (aset b 4 (js/Math.min (aget b 4) (- (aget a (+ o 2)) r)))
          (aset b 5 (js/Math.max (aget b 5) (+ (aget a (+ o 2)) r))))))
    b))

(defn- search-from ^Search [^js snapshot ^js query ^js options]
  (let [^js table (.-table options)
        ^js from (.-from query)
        ^js set-goals (.-goals query)
        n-goals (if (some? set-goals) (.-length set-goals) 0)
        multi (pos? n-goals)
        ^js goal (if multi (aget set-goals 0) (.-goal query))
        ^js bounds (goal-bounds from (if multi set-goals #js [goal]))
        ^js costs (js/Object.assign #js {} DEFAULT-COSTS (.-costs options))
        max-nodes (option options "maxNodes" 200000)
        ^js avoid (.-avoid options)
        ^js limits (.-limits options)
        margin (option options "margin" 64)
        y-margin (option options "yMargin" 48)
        goal-range (or-else (.-range goal) 0)
        ;; a goal set has no goal flood and no goal-not-standable or goal-unloaded answers (near false): those prove
        ;; things of one goal
        near (and (not multi) (identical? (.-kind goal) "near"))
        goal-unloaded (cond
                        multi false
                        near (== (.stateAt snapshot (.-x goal) (.-y goal) (.-z goal)) UNLOADED)
                        :else (false? (.hasColumn snapshot (bit-shift-right (.-x goal) 4) (bit-shift-right (.-z goal) 4))))
        y-low (aget bounds 4)
        y-high (aget bounds 5)
        cap (js/Math.min max-nodes 1024)
        slots (next-pow2 (* cap 2))
        ^js dangers (danger-array (.-dangers options))
        n-dangers (if (some? dangers) (/ (.-length dangers) DANGER-STRIDE) 0)
        ^js dbox (danger-box dangers)
        ^js dark (.-dark options)
        ^js tolls (.-tolls options)
        dark-at (when (some? dark) (.-at dark))
        dark-factor (if (some? dark) (or-else (.-factor dark) 1) 0)]
    (->Search
     ;; the world
     snapshot table (.-space options)
     (.-top table) (.-base table) (.-kind table) (.-hazard table) (.-stairUp table) (.-partial table) (.-climb table)
     (.-climbName table) (.-facing table) (.-floor table) (.-special table)
     (.-flowing table) (.-bubble table) (.-magma table) (.-dripleaf table) (.-openable table) (.-openState table)
     (.-openKind table) (.-doorHalf table) (.-activator table)
     (.-attach table) (.-farmland table) (.-minY snapshot)
     ;; the query
     (.-x from) (.-y from) (.-z from) (or-else (.-px from) (+ (.-x from) 0.5)) (or-else (.-pz from) (+ (.-z from) 0.5))
     (.-x goal) (.-y goal) (.-z goal) goal-range (* OCTILE-SLACK goal-range) near goal-unloaded
     ;; the goal set
     n-goals
     (when multi (goal-array js/Int32Array set-goals (fn [^js g] (.-x g))))
     (when multi (goal-array js/Int32Array set-goals (fn [^js g] (or-else (.-y g) 0))))
     (when multi (goal-array js/Int32Array set-goals (fn [^js g] (.-z g))))
     (when multi (goal-array js/Float64Array set-goals (fn [^js g] (let [r (or-else (.-range g) 0)] (* r r)))))
     (when multi (goal-array js/Float64Array set-goals (fn [^js g] (* OCTILE-SLACK (or-else (.-range g) 0)))))
     (when multi (goal-array js/Uint8Array set-goals (fn [^js g] (if (identical? (.-kind g) "near") 1 0))))
     ;; options
     max-nodes (option options "maxDrop" 3) (option options "weight" 1) (option options "riskWeight" 2)
     (option options "goalFlood" 4000) (option options "floodAfter" 3000) (option options "preFlood" 256)
     (reach-of options from goal)
     ;; avoid
     (some? avoid) (if (some? avoid) (.-kinds avoid) 0) (if (some? avoid) (.-cells avoid) nil) (if (some? avoid) (.-factor avoid) 0)
     ;; limits
     (if (some? limits) (or-else (.-kinds limits) 0) 0) (if (some? limits) (.-gap limits) nil) (if (some? limits) (.-corner limits) nil)
     ;; costs
     (unchecked-get costs "climbUp") (unchecked-get costs "climbDown") (unchecked-get costs "jumpClimb") (unchecked-get costs "open")
     (unchecked-get costs "openRedstone") (unchecked-get costs "openLever") (unchecked-get costs "openPlate") (unchecked-get costs "besideMagmaColumn")
     (unchecked-get costs "swimH") (unchecked-get costs "swimUp") (unchecked-get costs "swimDown")
     (unchecked-get costs "exit") (unchecked-get costs "current") (unchecked-get costs "bubbleUp") (unchecked-get costs "bubbleDown")
     (unchecked-get costs "airSupply") (unchecked-get costs "airLimit") (unchecked-get costs "maxWaterDrop")
     (unchecked-get costs "dripleaf") (unchecked-get costs "dripleafRisk") (unchecked-get costs "dropFactor")
     ;; search box
     (- (aget bounds 0) margin) (+ (aget bounds 1) margin)
     (- (aget bounds 2) margin) (+ (aget bounds 3) margin)
     (- y-low y-margin) (+ y-high y-margin)
     ;; directions
     (js/Int8Array. #js [1 -1 0 0 1 1 -1 -1]) (js/Int8Array. #js [0 0 1 -1 1 -1 1 -1])
     ;; node storage: cap slots hash-table node-keys xs ys zs hs moves slows corners shapes parents secs risks airs peaks wsecs opens gs fs heap-pos heap n-nodes heap-n
     cap slots (.fill (js/Int32Array. slots) -1) (js/Float64Array. cap)
     (js/Int32Array. cap) (js/Int32Array. cap) (js/Int32Array. cap) (js/Uint8Array. cap) (js/Uint8Array. cap) (js/Uint8Array. cap) (js/Uint8Array. cap)
     (js/Uint32Array. cap)
     (js/Int32Array. cap) (js/Float64Array. cap) (js/Float64Array. cap)
     (js/Float64Array. cap) (js/Float64Array. cap) (js/Float64Array. cap) (js/Int32Array. cap)
     (js/Float64Array. cap) (js/Float64Array. cap)
     (js/Int32Array. cap)
     (js/Int32Array. cap) 0 0
     ;; support touch enter-risk enter-slow enter-extra ty gap-y quiet open-mode allow-shut after-exit gap-seen air-seen enters-shut
     0 0 0 0 0 0 0 false false false false false false false
     ;; move-air move-peak move-water move-open open-lists activators view
     0 0 0 0 #js [] (js/Map.) nil
     ;; edge-mode seen-edges door-arrival door-here door-through open-x open-y open-z
     0 nil nil nil false 0 0 0
     ;; tight cells
     (js/Float64Array. TABLE) (js/Uint8Array. TABLE) (js/Float64Array. TABLE) (js/Uint8Array. TABLE) (js/Map.) (js/Set.) (js/Int8Array. REGIONS)
     (js/Float64Array. REGIONS) (js/Int16Array. REGIONS) (js/Map.) (js/Map.)
     0 0 0 0
     ;; the goal flood: flooding fx fy fz fr hit flooded pre-flooded flood-pending leaked
     false 0 0 0 -1 false 0 0
     (and near (pos? (option options "goalFlood" 4000))) false
     ;; flood-memo flood-base lf-imported verifying
     (when (and near (pos? (option options "goalFlood" 4000))) (.-goalFloodMemo options)) 0 0 false
     ;; flood-moves flood-out flood-h-keys flood-h-vals flood-target
     (js/Map.) nil nil nil 0
     ;; progress: started finished reason over-budget boxed goal-node best-node
     false false nil false false -1 -1
     ;; best-distance start-distance expanded t0 elapsed start-h start-slow
     js/Infinity 0 0 (js/performance.now) 0 -1 0
     ;; returnable held replaying
     (true? (option options "returnable" false)) #js [] false
     ;; lf-seen lf-queue lf-head lf-budget lf-active lf-open lf-seed-open lf-end cut-off limit-refused
     nil nil 0 0 false false false false false false
     ;; known-cells known-new known-edges edges-new
     (.-knownCells options) nil (.-knownEdges options) nil
     ;; searched-out
     false
     ;; stop-at-edge edge-node
     (and goal-unloaded (true? (option options "stopAtEdge" false))) -1
     ;; dangers n-dangers danger-cap dbx0 dbx1 dby0 dby1 dbz0 dbz1
     dangers n-dangers (option options "dangerCap" 4)
     (aget dbox 0) (aget dbox 1) (aget dbox 2) (aget dbox 3) (aget dbox 4) (aget dbox 5)
     ;; dark-at dark-factor dark-keys dark-flags darks
     dark-at dark-factor
     (when (some? dark-at) (js/Float64Array. TABLE)) (when (some? dark-at) (js/Uint8Array. TABLE)) (js/Float64Array. cap)
     ;; tolls
     (when (some? tolls) (.-cells tolls)))))

;; the body's hitbox reaches this far from its centre in x and z
(def ^:const HITBOX-HALF 0.3)

(defn- overlap [lo hi c] (- (js/Math.min hi (inc c)) (js/Math.max lo c)))

(defn- hitbox-cells
  "The cells [x z] other than (fx fz) that the body's hitbox at px pz overlaps, most overlap first, then lower x, then
  lower z."
  [fx fz px pz]
  (let [x0 (- px HITBOX-HALF) x1 (+ px HITBOX-HALF) z0 (- pz HITBOX-HALF) z1 (+ pz HITBOX-HALF)]
    (->> (for [x (range (js/Math.floor x0) (inc (js/Math.floor x1)))
               z (range (js/Math.floor z0) (inc (js/Math.floor z1)))
               :let [area (* (overlap x0 x1 x) (overlap z0 z1 z))]
               :when (and (pos? area) (not (and (== x fx) (== z fz))))]
           [area x z])
         (sort-by (fn [[area x z]] [(- area) x z]))
         (map (fn [[_ x z]] [x z])))))

(defn- start-query
  "query with its start cell moved for a body on the edge of a block: when from (the floored body position) is no
  place to stand but from.px/pz is given, the first cell the 0.6-wide hitbox overlaps (hitbox-cells) that stands with
  the feet at from.py (when given, within 1/16). Unchanged when the floored cell stands or no overlapped cell does.
  search: a search of query, initialised."
  [^Search search ^js query]
  (let [^js from (.-from query)
        px (.-px from) pz (.-pz from) py (.-py from)
        fx (.-x from) fy (.-y from) fz (.-z from)]
    (if (or (>= (.-start-h search) 0) (nil? px) (nil? pz))
      query
      (if-some [[x z] (first (filter (fn [[x z]]
                                      (let [h (.nodeH search x fy z)]
                                        (and (>= h 0) (or (nil? py) (<= (js/Math.abs (- (+ fy (/ h WHOLE)) py)) (/ 1 WHOLE))))))
                                    (hitbox-cells fx fz px pz)))]
        (js/Object.assign #js {} query #js {:from (js/Object.assign #js {} from #js {:x x :z z})})
        query))))

(defn- new-search
  "The search of query, its start moved off the edge of a block (start-query); initialised (init again is harmless)."
  ^Search [^js snapshot ^js query ^js options]
  (let [^Search search (search-from snapshot query options)]
    (.init search)
    (let [q (start-query search query)]
      (if (identical? q query) search (search-from snapshot q options)))))

(defn- clean-options
  "The options of the returnable search behind a one-way step of search: options.returnable, no goal flood, no stop at
  the loaded edge (stopAtEdge), and no more nodes than the search itself made. It only finds where a partial plan ends. With no flood to prove a walled-in
  goal it would otherwise search the whole box."
  [^Search search options]
  (js/Object.assign #js {} options #js {:returnable true :goalFlood 0 :stopAtEdge false
                                        :maxNodes (js/Math.min (option options "maxNodes" 200000) (js/Math.max 1 (.-n-nodes search)))}))

(defn- result-of
  "The result of a finished search. When the path to its nearest node holds a one-way step, a second search
  (clean-options) supplies the partial end."
  [^Search search snapshot query options]
  (.settle search)
  (let [node (.oneWayNode search)]
    (if (neg? node)
      (.resultFrom search -1 nil)
      (let [^Search clean (new-search snapshot query (clean-options search options))]
        (.init clean)
        (.step clean js/Infinity)
        (.resultFrom search node (.nearest clean))))))

(defn create-search
  "A search to run in steps: #js {step(n) result() nearest()}. options.table and options.space are required."
  [snapshot query options]
  (let [search (new-search snapshot query options)]
    (.init search)
    #js {:step (fn [max-expansions] (.step search max-expansions))
         :result (fn [] (result-of search snapshot query options))
         :nearest (fn [] (.nearest search))}))

(defn plan [snapshot query options]
  (let [search (new-search snapshot query options)]
    (.init search)
    (.step search js/Infinity)
    (result-of search snapshot query options)))

(defn create-plan
  "plan in slices, for a caller that yields to the event loop between them: #js {step result progress}.
  step(n) runs at most about n expansions (each newly flooded cell of the goal flood counts as one). It is true once
  the plan is ready, and result() is then what plan answers. The returnable search behind a one-way step (result-of)
  runs in the same slices. progress() is where an unfinished search has got to (Search.progress), nil once the
  search itself is over."
  [snapshot query options]
  (let [search (new-search snapshot query options)
        clean (volatile! nil)
        node (volatile! -1)
        ready (volatile! false)]
    (.init search)
    #js {:step (fn [max-expansions]
                 (cond
                   ^boolean @ready true
                   (some? @clean) (vreset! ready ^boolean (.step ^Search @clean max-expansions))
                   (not ^boolean (.step search max-expansions)) false
                   :else (do (.settle search)
                             (vreset! node (.oneWayNode search))
                             (if (neg? @node)
                               (vreset! ready true)
                               (let [^Search c (new-search snapshot query (clean-options search options))]
                                 (.init c)
                                 (vreset! clean c)
                                 false)))))
         :result (fn [] (if (neg? @node)
                          (.resultFrom search -1 nil)
                          (.resultFrom search @node (.nearest ^Search @clean))))
         ;; while the search itself is not over: where it has got to (Search.progress), else nil
         :progress (fn [] (when-not ^boolean (.-finished search) (.progress search)))}))
