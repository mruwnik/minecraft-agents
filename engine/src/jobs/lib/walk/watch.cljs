(ns jobs.lib.walk.watch
  "The walk driver's look-ahead: the cells and mobs of the way ahead watched while a plan is walked (watch-stop), when a walk may
  stop to plan again, and whether a refreshed plan replaces the old one."
  (:require [engine.hurt :as hurt]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.threats :as threats]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

;; ---------------------------------------------------------------- the look-ahead (watch)

(def watch-policy
  "The look-ahead's numbers. window: plan legs ahead whose cells are checked; check-every: on a long straight or diagonal leg,
  ticks between checks (a check also runs whenever the body reaches a step); min-refresh-ticks: a partial plan is planned
  again at most this often (4 s), planner-share: and the planner gets at most this share of the walk (a slow plan spaces the
  refreshes out); better-by: a refreshed partial plan is taken only when its end is this many blocks nearer the goal; mob-waits
  and mob-wait-ms: a body stuck behind a mob waits this often this long for it to move on; mob-still-ticks: a mob that stands
  this many ticks (1 s) in a 1-wide way ahead is planned round at once."
  {:window 10 :check-every 5 :min-refresh-ticks 80 :planner-share 0.05 :tick-ms 50 :better-by 2
   :mob-waits 3 :mob-wait-ms 1000 :mob-reach 2.5 :mob-still-ticks 20})

(def danger-reach
  "Blocks from a step of the way ahead within which a newly sensed danger makes the walk plan again: the radius of a
  sensed danger's cost (jobs.lib.cost.danger/danger-shape), past which it costs nothing."
  (get-in danger/danger-shape [:sensed :radius]))

(def max-watch-replans
  "Replans a look-ahead may start in one follow! (changes, refreshes, mobs); past it the plan is walked unwatched."
  12)

(def no-stop-moves
  "Steps a walk is never stopped before or on: the body is in the air, on a ladder, swimming, or in a gap's run-up."
  #{:gap :climb-up :climb-down :jump-climb :open :swim :swim-up :swim-down :exit})

(def body-half 0.3)

(def fall-margin "Default hp a walk's falls may cost over the plan before it is a mismatch (rounding of the fall)." 1)

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
            (<= (+ (wplan/near-goal (peek steps) to) (:better-by watch-policy)) (wplan/near-goal (peek old-steps) to))))))

(defn one-wide?
  "Whether the way is 1-wide at cell [x y z]: across the leg from prev to step, both side cells are solid at the feet and
  over the head, in the solid? of a pathWorld."
  [solid? prev step [x y z]]
  (let [along-x? (>= (js/Math.abs (- (:px step) (:px prev))) (js/Math.abs (- (:pz step) (:pz prev))))
        sides (if along-x? [[0 0 -1] [0 0 1]] [[-1 0 0] [1 0 0]])]
    (every? (fn [[dx _ dz]] (and (solid? (+ x dx) y (+ z dz)) (solid? (+ x dx) (inc y) (+ z dz)))) sides)))

(defn still-mob-cells
  "The cells {:x :y :z} (feet and head) of the entities (not items, not the body) that have stood in the same cell of a
  1-wide leg in the window ahead for mob-still-ticks ticks. seen: an atom {id [cell first-tick]}, kept up to date."
  [{:keys [mobs seen]} here i2 tick pw]
  (let [solid? (wworld/solid-fn pw)
        legs (into {} (mapcat (fn [k] (let [prev (nth here (dec k)) step (nth here k)]
                                        (map (fn [cell] [cell [prev step]]) (step-cells prev step))))
                              (range (max 1 i2) (min (count here) (+ i2 (:window watch-policy))))))
        found (into {} (for [^js e (mobs)
                             :let [pos (.-pos e)
                                   cell [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))]]
                             :when (and (not= "item" (.-kind e)) (contains? legs cell))]
                         [(.-id e) cell]))]
    (swap! seen (fn [m] (into {} (for [[id cell] found] [id (if (= cell (first (m id))) (m id) [cell tick])]))))
    (vec (for [[id cell] found
               :let [[_ t0] (@seen id)]
               :when (and (>= (- tick t0) (:mob-still-ticks watch-policy)) (apply one-wide? solid? (conj (legs cell) cell)))
               dy [0 1]]
           {:x (cell 0) :y (+ dy (cell 1)) :z (cell 2)}))))

(defn new-danger-keys
  "The keys of the sensed dangers ([{:key :pos}], watch :sense) not yet in known that lie within danger-reach of a step
  from index i on."
  [sensed known steps i]
  (let [ahead (subvec (vec steps) (min i (count steps)))]
    (vec (for [{:keys [key pos]} sensed
               :when (and (not (contains? known key))
                          (some (fn [s] (<= (js/Math.hypot (- (:x pos) (:px s)) (- (:z pos) (:pz s))) danger-reach)) ahead))]
           key))))

(defn danger-stop
  "The :replan done map for the dangers newly sensed near the way ahead (new-danger-keys; their keys join watch :known), or nil."
  [{:keys [sense known]} steps i at]
  (when sense
    (when-let [ks (seq (new-danger-keys (sense) @known steps i))]
      (swap! known into ks)
      {:status :replan :why :danger :at at :step i})))

(defn damage-ahead
  "The hp the steps from index i on plan to cost (their :damage, the planner's)."
  [steps i]
  (transduce (keep :damage) + 0 (subvec (vec steps) (min i (count steps)))))

(defn health-stop
  "The :replan done map when the damage still planned from step i on is more than the walk may spend now (watch :budget, a
  fn: the hp of jobs.lib.cost/damage-budget at the body's health, food and effects now: a mob, a fall or hunger since the
  plan was made), else nil."
  [{:keys [budget]} steps i at]
  (when (and budget (> (damage-ahead steps i) (budget)))
    {:status :replan :why :health :at at :step i}))

(defn damage-mismatch
  "{:planned :lost} when the falls in hurts (raw :hurt memory entries since the walk began) cost more than margin hp
  (default fall-margin) over planned, the hp the walked steps planned for drops; else nil. Hits of other causes do not count."
  ([planned hurts] (damage-mismatch planned hurts fall-margin))
  ([planned hurts margin]
  (let [lost (transduce (comp (filter #(= "fall" (hurt/cause (:data %)))) (map #(:amount (:data %)))) + 0 hurts)]
    (when (> lost (+ planned margin))
      {:planned planned :lost lost}))))

(defn watch-stop
  "The look-ahead at one tick: nil, or the done map that stops the walk to plan again: {:status :replan :why :changed :cells}
  when a cell of the window ahead differs for the planner between the plan's snapshot (base) and a fresh one, {:status
  :replan :why :mob :cells} (the cells as walls) when a mob has stood in a 1-wide way ahead (still-mob-cells), else
  {:status :replan :why :danger} when a danger sensed now, not known when the walk began, lies within danger-reach of the
  way ahead (watch :sense, :known: the keys already planned for, which the stop adds its own to), else
  {:status :replan :why :health} when the damage planned ahead is more than the body may spend now (health-stop; watch
  :budget), else {:status :replan :why :refresh} for a partial plan due a refresh. Only at a boundary?. steps: the steps walked; i the
  executor's index before the tick; state its state after; watch {:base :fresh :ahead :skip :status :interval :mobs :seen}."
  [{:keys [base fresh ahead skip status interval mobs sense known] :as watch} steps i {i2 :i tick :tick} pose]
  (when (boundary? steps i i2 tick pose)
    (let [all (into steps ahead)
          here (assoc all (dec i2) (assoc (nth all (dec i2)) :px (:x pose) :pz (:z pose)))
          ^js now (fresh)
          at [(:x pose) (:y pose) (:z pose)]
          still (when (and mobs now) (still-mob-cells watch here i2 tick now))
          changed (when now
                    (let [^js bs (.-snapshot base) ^js ns (.-snapshot now) table (.-table base)]
                      (filterv (fn [[x y z]] (not (same-for-planner? table (.stateAt bs x y z) (.stateAt ns x y z))))
                               (remove (or skip #{}) (window-cells here i2 (:window watch-policy))))))
          quiet? (and (empty? still) (empty? changed))
          dstop (when quiet? (danger-stop watch all i2 at))
          hstop (when (and quiet? (not dstop)) (health-stop watch all i2 at))]
      (cond
        (seq changed) {:status :replan :why :changed :cells changed :at at :step i2}
        (seq still) {:status :replan :why :mob :cells still :at at :step i2}
        dstop dstop
        hstop hstop
        (refresh-due? status tick interval) {:status :replan :why :refresh :at at :step i2}))))

(defn watch-of
  "The look-ahead for walking plan: its own snapshot as the base, a fresh pathWorld per check, the cells the plan opens
  skipped, and the refresh interval from the plan's ms. With known (an atom of danger keys, kept over a follow!'s plans:
  a mob is planned for once), the dangers sensed now join it and a newly sensed one near the way ahead stops the walk."
  ([c plan] (watch-of c plan nil))
  ([c plan known]
  (when known (swap! known into (map :key (threats/sensed-mobs (:primitives c)))))
  (cond-> {:base (:pw plan) :fresh #(wworld/path-world (:primitives c)) :ahead [] :skip (opens-cells (:steps plan))
   :mobs #(combat/sensed (:primitives c) {:radius 8 :max 64}) :seen (atom {})
   :status (:status plan) :interval (refresh-ticks (:ms plan)) :budget #(wworld/damage-budget c)}
    known (assoc :known known :sense #(threats/sensed-mobs (:primitives c))))))

(defn mob-cells
  "The cells {:x :y :z} (feet and head) of the entities, not items and not the body, that stand on the leg the body is
  stuck on or the next one (steps k-1 to k+1) within mob-reach of the body."
  [c steps k]
  (let [p (:primitives c)
        me (.-username (.self p))
        here (.-pos (.self p))
        legs (into #{} (mapcat (fn [j] (when (< 0 j (count steps)) (step-cells (nth steps (dec j)) (nth steps j)))))
                   [k (inc k)])]
    (vec (for [^js e (combat/sensed p {:radius 8 :max 64})
               :let [pos (.-pos e)
                     cell [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))]]
               :when (and (not= "item" (.-kind e)) (not= me (.-username e)) (contains? legs cell)
                          (<= (js/Math.hypot (- (.-x pos) (.-x here)) (- (.-z pos) (.-z here))) (:mob-reach watch-policy)))
               dy [0 1]]
           {:x (cell 0) :y (+ dy (cell 1)) :z (cell 2)}))))
