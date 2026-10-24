(ns jobs.lib.walk.drive
  "The walk driver's loop over jobs.lib.walk's pieces: follow! (walk a plan, plan again in the same call), walk-to! (the whole
  loop: settle, plan, follow, plan again when off the plan) and way-back."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.path.executor :as executor]
            [engine.settings :as settings]
            [jobs.lib.clock :as clock]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.watch :as wwatch]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::look-over-edge {:default true :doc "Whether a walk that learns a floor is not there looks over the edge (look-over-edge!); false for a flee step: its seconds are few, the next step plans again, and the gate still looks at each floor before the drop." :spec boolean?}})

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

      :else {:status :off-plan :partial true :at (:at done) :step (dec (count steps))})))

(defn ^:async replan-round-mob!
  "A walk stuck with mobs on its way: wait for them (mob-wait-ms, at most mob-waits times) and plan with the cells they
  stand in as walls; the first plan that can be walked, or, after the waits, a plan with no walls. [plan ms] (ms the
  planning time), the plan possibly not walkable."
  [c plan-fn walkable? steps k]
  (let [{:keys [mob-waits mob-wait-ms]} wwatch/watch-policy]
    (loop [n 1]
      (await (ctx/act c :wait #js {:ms mob-wait-ms}))
      (let [t (clock/now)
            walls (wwatch/mob-cells c steps k)
            plan (await (plan-fn walls))
            ms (- (clock/now) t)]
        (if (or (walkable? plan) (>= n mob-waits))
          (if (or (walkable? plan) (empty? walls))
            [plan ms]
            (let [t (clock/now) plan (await (plan-fn []))] [plan (- (clock/now) t)]))
          (recur (inc n)))))))

(defn ^:async back-off!
  "Walk the body back along the way it came to a cell clear of the hostile-near radius of the seen mob that blocks the way
  (fresh's pw dangers), so what follows (a stair, another way) does not start in reach of it. No-op when none is blocking or clear."
  [c fresh walked walk-fn policy]
  (when-let [cell (wplan/retreat-cell walked (.-dangers ^js (:pw fresh)))]
    (let [{:keys [steps]} (await (wplan/plan-within! c (:pw fresh) cell 0 (walk/default-weight) policy))]
      (when (> (count steps) 1)
        (await (walk-fn steps nil))))))

(def free-learned
  "Learned replans a follow! does not count: a void edge teaches a column per replan, each a search."
  6)

(defn learned-progress
  "The learned-replan tally {:best :stale} after a learned replan with the body at [x y z] bound for to: stale counts the
  learned replans since the body's best distance to the goal last improved by a block (go-to's idiom), this one included.
  A walk that keeps getting nearer never runs out; backing off or going back and forth never resets it."
  [{:keys [best stale] :or {stale 0}} at to]
  (let [left (when (and at to) (js/Math.hypot (- (at 0) (to 0)) (- (at 1) (to 1)) (- (at 2) (to 2))))
        improved? (and left best (< left (dec best)))]
    {:best (if (or improved? (nil? best)) (or left best) best)
     :stale (inc (if improved? 0 stale))}))

(defn edge-points
  "The points a look over an edge aims at: the ground round self ({:x :z}) at 3 and 7 blocks in eight directions at height y,
  those whose cell or a neighbour is still unknown (unknown? takes [x y z]); ground already seen is not looked at again."
  [{:keys [x z]} y unknown?]
  (for [d [3 7] [dx dz] [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]
        :let [pt {:x (+ x (* d dx)) :y (+ y 0.5) :z (+ z (* d dz))}]
        :when (unknown? (mapv #(js/Math.floor (% pt)) [:x :y :z]))]
    pt))

(defn unseen-round?
  "Whether the cell or any of its 6 neighbours is unknown: a look glances all seven."
  [p cell]
  (boolean (some #(dig-look/unknown? p (dig-look/add cell %)) (cons [0 0 0] dig-look/neighbours))))

(def lip-reach "How far from the middle of its cell toward a drop the body leans to look over the edge: sneaking stops the box at the edge, so only the centre overhangs." 0.44)

(def lean-ticks "Ticks a lean to the lip may take." 60)

(defn lip
  "The point {:x :z} a body on cell from ([x y z]) leans to, to look over its edge toward cell to."
  [[fx _ fz] [tx _ tz]]
  {:x (+ fx 0.5 (* lip-reach (compare tx fx))) :z (+ fz 0.5 (* lip-reach (compare tz fz)))})

(defn lean-to!
  "Sneaks to the point {:x :z} (the game's sneak keeps the body from going off the edge), within a tenth of a block."
  [c {:keys [x z]}]
  (let [ticks (volatile! 0)]
    (ctx/act c :steer (walk/steer-args (walk/steer-s c 3)
                                       (fn [pose]
                                         (let [dx (- x (.-x pose)) dz (- z (.-z pose))]
                                           (if (or (and (< (js/Math.abs dx) 0.1) (< (js/Math.abs dz) 0.1)) (> (vswap! ticks inc) lean-ticks))
                                             #js {:done true}
                                             #js {:controls #js {:forward true :sneak true} :yaw (js/Math.atan2 (- dx) (- dz))})))))))

(defn column-point
  "The point a look down the column under cell [x y z] aims at, depth blocks below it, when a cell of that column is still
  unknown (unknown? takes [x y z])."
  [[x y z] depth unknown?]
  (when (some #(unknown? [x (- y %) z]) (range 1 (inc depth)))
    {:x (+ x 0.5) :y (+ (- y depth) 0.5) :z (+ z 0.5)}))

(defn lip-unsafe?
  "Whether looking over the edge of drop is held back: a hostile the body knows of near the lip, or lava seen in the drop's column or beside it."
  [c {:keys [from floor]}]
  (let [p (:primitives c)
        [fx fy fz] from
        [tx ty tz] floor]
    (or (seq (danger-q/hostiles-near-way p {:x fx :y fy :z fz} (danger-q/hostile-radius) {:ranged-radius (danger-q/ranged-radius)}))
        (some (fn [[dx dz]] (some #(some-> (u/seen-block p {:x (+ tx dx) :y % :z (+ tz dz)}) .-name (= "lava")) (range ty (inc fy))))
              [[0 0] [1 0] [-1 0] [0 1] [0 -1]]))))

(defn ^:async look-over-edge!
  "A floor the body learned is not there (a drop at the takeoff's edge): lean to the lip toward it when it is beside the
  body's cell (from the middle of a step, the step itself hides what lies beside and under it), look down the column it is
  missing from, to the deepest lit drop the planner takes as seen (see-lit-depth), then at the unseen ground round the
  body at several distances, as a player looks over an edge once, so the next plan drops neither one cell deeper nor at
  the next unseen column. The lean is held back while a hostile is near or lava is seen at the edge (lip-unsafe?); the looks are not."
  [c [x y z :as cell]]
  (let [p (:primitives c)
        pos (u/self-pos c)
        [ax _ az :as at] (mapv #(js/Math.floor (% pos)) [:x :y :z])]
    (when (and (<= (js/Math.abs (- x ax)) 1) (<= (js/Math.abs (- z az)) 1) (not= [x z] [ax az])
               (not (lip-unsafe? c {:from at :floor cell})))
      (await (lean-to! c (lip at cell))))
    (loop [points (keep identity (cons (column-point cell (:see-lit-depth (cost/walk-unknown-prices)) #(dig-look/unknown? p %))
                                       (edge-points pos y #(unseen-round? p %))))]
      (when-let [pt (first points)]
        (await (ctx/act c :look (clj->js {:pos pt})))
        (look/see! c)
        (recur (rest points))))))

(defn ^:async follow!
  "Walk plan (a plan-walk! result) and plan again from the body's cell, in the same call, whenever the walk stops for it:
  the look-ahead saw the way change (:changed: the new plan is walked), a partial plan is due a refresh (:refresh: the new
  plan is walked only when take-refresh? says it is clearly better, else the rest of the old one), or the body is stuck with
  a mob in its way (:mob: wait, plan round it), or a plan cut short of what it does not know yet (:cut-rest, the steps
  after the cut: walked to its end, look at them, then plan again). A replan that proves the goal cut off or enclosed ends the call with that
  no-walk; so does one the caller refused (plan-fn's {:refused true}, with its :stop) for any why but :refresh, whose old
  plan nothing speaks against. A plan short of the goal (not found) whose walk the caller's (:walk-spent? c) says has cost more than the goal is worth (go-to's tour price) ends the call at its next re-plan or cut instead, the done map {:status :tour-spent :at}: two ways whose cuts or budget-cut searches send the body back and forth have no other end. At most max-watch-replans; past that the plan is walked unwatched (a kept plan counts as a replan, and is watched; a re-plan at a cut does not, each walks on to a new cut). A replan with :learned (a gate or the look-ahead gave an unseen cell up or found it solid) is not counted against it, up to free-learned of them: a void edge teaches one column per replan. The max-learned-th learned replan since the body last got a block nearer :to (learned-progress), or one past the replan cap, ends the call instead: the done map with :learn-spent and the cells learned.
  opts: :plan-fn (fn [walls]) -> a plan-walk! result (or a promise of one: plan-walk!) from where the body stands now, the
  cells {:x :y :z} read as walls;
  :walk-fn (fn [steps watch]) -> [done ms] (walk! or jobs.lib.pass/walk!); :to the goal cell; :policy for no-walk;
  :dangers true: a danger newly sensed near the way ahead is planned round once (watch-stop); a search that has not
  finished ends the call with its :searching no-walk (the old plan is never walked on past the danger);
  a :health stop (the damage still planned is over what the body may spend now) plans again and walks the new plan, or
  ends the call with its no-walk when that plan is not walkable (the caller decides: go-to heals or goes over its budget);
  :announce! (fn [:replan data]) per replan, data {:why :ms :kept :replans :at :learned :text} (learned: the cells a learned replan found missing or solid).
  {:done :plan :ms :walked :replans :damage}: done the last walk's done map, or the no-walk result of a replan that has no way; plan
  the plan in force at the end; ms the time in walks; walked the blocks of plan walked; damage the hp the steps walked planned."
  [c plan {:keys [plan-fn walk-fn to policy announce! dangers] :or {policy (wworld/body-policy c) announce! (fn [_ _])}}]
  (let [spent? (:walk-spent? c)
        known (when dangers (atom #{}))
        learned (atom #{})
        cuts (atom 0)
        track (atom {})
        walkable? (fn [pl] (nil? (wplan/no-walk pl 0 policy)))
        proved-none? (fn [pl] (let [nw (wplan/no-walk pl 0 policy)]
                                (or (contains? #{:goal-cut-off :goal-enclosed} (:reason nw)) (:stopped-short nw))))
        searching? (fn [pl] (= :searching (:status (wplan/no-walk pl 0 policy))))
        cut-length (fn [steps k] (walk/path-length (subvec steps 0 (min (count steps) (max 1 k)))))]
    (loop [plan plan steps (:steps plan) n 0 ms 0 walked 0 damage 0]
      (let [spent (- n (min free-learned (count @learned)) @cuts)
            watch (when (< spent wwatch/max-watch-replans) (wwatch/watch-of c plan known))
            [done wms] (await (walk-fn steps watch))

            ms (+ ms wms)
            k (or (:step done) (count steps))
            walked' (+ walked (cut-length steps k))
            damage' (+ damage (wwatch/damage-ahead (subvec steps 0 (min (count steps) k)) 0))
            ;; a kept plan walks again from step k-1: its damage is counted once, with the walk that ends there
            kept-damage (+ damage (wwatch/damage-ahead (subvec steps 0 (min (count steps) (max 0 (dec k)))) 0))
            finish (fn [d pl w] {:done d :plan pl :ms ms :walked w :replans n :damage damage'})
            tell! (fn [why plan-ms kept]
                    (announce! :replan {:why why :ms (/ (js/Math.round (* 10 plan-ms)) 10) :kept kept :replans (inc n)
                                        :at (:at done) :learned (when (:learned done) (:cells done))
                                        :text (str "re-plan " (inc n) " (" (name why) (when kept ", kept the plan") ")")}))]
        (cond
          (and (= :replan (:status done)) (= :mob (:why done)))
          (let [t (clock/now)
                fresh (await (plan-fn (:cells done)))
                plan-ms (- (clock/now) t)]
            (tell! :mob plan-ms false)
            (cond
              (walkable? fresh) (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              (:refused fresh) (finish (wplan/no-walk fresh 0 policy) fresh walked')
              :else (recur plan (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))

          (and (= :replan (:status done)) (= :air (:why done)))
          (finish {:status :stair-cheaper :why {:air-spent true}} plan walked')

          (and (= :replan (:status done)) (= :health (:why done)))
          (let [t (clock/now)
                fresh (await (plan-fn []))
                plan-ms (- (clock/now) t)]
            (tell! :health plan-ms false)
            (if (walkable? fresh)
              (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              (finish (wplan/no-walk fresh 0 policy) fresh walked')))

          (and (= :replan (:status done)) (= :danger (:why done)))
          (let [t (clock/now)
                fresh (await (plan-fn []))
                plan-ms (- (clock/now) t)]
            (tell! :danger plan-ms (not (or (walkable? fresh) (searching? fresh) (:refused fresh))))
            (when (or (:stopped-short (wplan/no-walk fresh 0 policy)) (and (searching? fresh) (:stopped-short fresh)))
              (await (back-off! c fresh (subvec steps 0 (min (count steps) (inc k))) walk-fn policy)))
            (cond
              (walkable? fresh) (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              ;; an unfinished search: the old plan runs past the danger, so the caller searches on from here
              (or (proved-none? fresh) (searching? fresh) (:refused fresh)) (finish (wplan/no-walk fresh 0 policy) fresh walked')
              :else (recur (assoc plan :ms plan-ms) (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))

          (and spent? (not= "found" (:status plan)) (or (= :replan (:status done)) (and (= :arrived (:status done)) (:cut-rest plan))) (spent?))
          (finish (cond-> {:status :tour-spent :at (:at done)} (:step done) (assoc :step (:step done))) plan walked')

          (and (= :replan (:status done)) (:learned done) (or (>= (:stale (learned-progress @track (:at done) to)) (wwatch/max-learned)) (>= spent wwatch/max-watch-replans)))
          (finish (assoc done :learn-spent true :cells (vec (take 6 (concat (:cells done) (mapcat first @learned))))) plan walked')

          (and (= :replan (:status done)) (< spent wwatch/max-watch-replans))
          (let [_ (wsight/note-missed! (:pw plan) done)
                _ (when (:learned done) (do (swap! learned conj [(:cells done) (:why done)]) (swap! track learned-progress (:at done) to)))
                _ (when (and (:learned done) (= :changed (:why done)) (not (:felt done)) (settings/get settings ::look-over-edge))
                    (await (look-over-edge! c (first (:cells done)))))
                t (clock/now)
                fresh (await (plan-fn []))
                plan-ms (- (clock/now) t)
                fresh (assoc fresh :ms plan-ms)]
            (if (= :refresh (:why done))
              (let [take? (and (walkable? fresh) (wwatch/take-refresh? steps fresh to plan))
                    none? (proved-none? fresh)]
                (tell! :refresh plan-ms (not (or take? none?)))
                (cond
                  take? (recur fresh (:steps fresh) (inc n) ms walked' damage')
                  none? (finish (wplan/no-walk fresh 0 policy) fresh walked')
                  :else (recur (assoc plan :ms plan-ms) (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))
              (do (tell! :changed plan-ms false)
                  (if (walkable? fresh)
                    (recur fresh (:steps fresh) (inc n) ms walked' damage')
                    (finish (wplan/no-walk fresh 0 policy) fresh walked')))))

          (and (= :stuck (:status done)) (< spent wwatch/max-watch-replans) (seq (wwatch/mob-cells c steps k)))
          (let [[fresh plan-ms] (await (replan-round-mob! c plan-fn walkable? steps k))]
            (tell! :mob plan-ms false)
            (if (walkable? fresh)
              (recur fresh (:steps fresh) (inc n) ms walked' damage')
              (finish done plan walked')))

          (and (= :arrived (:status done)) (:cut-rest plan))
          (do (when (:unknown c) (await (walk/look-ahead! c (into [(peek steps)] (:cut-rest plan)))))
              (if (< spent wwatch/max-watch-replans)
                (let [t (clock/now)
                      fresh (await (plan-fn []))
                      plan-ms (- (clock/now) t)]
                  (tell! :cut plan-ms false)
                  (swap! cuts inc)
                  (if (walkable? fresh)
                    (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
                    (finish (wplan/no-walk fresh 0 policy) fresh walked')))
                (finish done plan walked')))

          :else (finish done plan walked'))))))

(defn note-mismatch!
  "Emit the info :damage-mismatch when the falls hurt since began cost more than margin (default the fall-margin setting) over planned hp."
  [c planned began margin]
  (when-let [m (wwatch/damage-mismatch planned (ctx/since c :hurt began) (or margin (wwatch/fall-margin)))]
    (ctx/emit! c :damage-mismatch :info m)))

(defn ^:async walk-to!
  "Walk the body to within range of the goal cell to ([x y z]): settle, plan, follow the plan (follow!: at most timeout-s
  seconds a walk, planning again in the same call when the way ahead changes, a partial plan is refreshed or a mob is in the
  way), plan again when the body ends off it (at most 5 times). The caller has checked that path-world is there.
  {:result :walked :walk-ms}: result is a {:status ...} map with :replans; walked is the blocks of
  every plan followed, walk-ms the time inside steer acts. announce! is called (kind data) with :plan before each walk,
  :replan when the body is off its plan and for each of follow!'s replans (those have :why). :fall-margin the hp falls may cost over
  the plan before an info :damage-mismatch (default the fall-margin setting of jobs.lib.walk.watch)."
  [c {:keys [to range weight timeout-s announce! fall-margin] :or {announce! (fn [_ _])}}]
  (let [p (:primitives c)
        plan-fn (fn [walls] (walk/plan-walk! c (wworld/path-world p) to range weight {:walls walls}))
        end (fn [result walked walk-ms] {:result result :walked walked :walk-ms walk-ms})]
    (loop [replans 0 walked 0 walk-ms 0]
      (await (walk/settle! c))
      (let [plan (await (plan-fn []))]
        (if-let [no (wplan/no-walk plan replans (wworld/body-policy c))]
          (end no walked walk-ms)
          (let [{:keys [r steps status]} plan]
            (announce! :plan {:steps (count steps) :summary (some-> (.-path r) .-summary js->clj)
                              :status status :ms (.-ms r) :replans replans
                              :text (str "plan " status ", " (count steps) " steps")})
            (let [began (ctx/now c)
                  {walk-result :done last-plan :plan ms :ms length :walked planned :damage}
                  (await (follow! c plan {:plan-fn plan-fn :to to :announce! announce!
                                          :walk-fn (fn [steps watch] (walk/walk! c steps timeout-s watch))}))
                  walked (+ walked length)
                  walk-ms (+ walk-ms ms)
                  _ (note-mismatch! c planned began fall-margin)
                  done (partial-end walk-result (:status last-plan) to range (:steps last-plan) (:stop last-plan))
                  after (if (#{:arrived :off-plan :stuck} (:status done))
                          (executor/after-walk executor/policy replans done)
                          {:finish done})]
              (if-let [n (:replan after)]
                (do (announce! :replan {:at (:at done) :replans n :text (str "off the plan, re-plan " n)})
                    (recur n walked walk-ms))
                (end (assoc (:finish after) :replans replans) walked walk-ms)))))))))

(defn ^:async way-back
  "nil when a whole plan the executor can walk leads from the body to origin on a fresh pathWorld, else the stop (a
  promise: the search yields to the event loop)."
  [c origin]
  (let [pw (wworld/path-world (:primitives c))]
    (if (nil? pw)
      {:reason :no-way-back :why :unsupported}
      (let [{:keys [r steps beyond]} (await (wplan/plan-within! c pw origin 0 (walk/default-weight) (wworld/body-policy c)))
            status (.-status r)
            stop (fn [refused] {:reason :no-way-back :why :refused :kind (:kind refused) :step (:at refused)})]
        (cond
          beyond (stop beyond)
          (not= "found" status) {:reason :no-way-back :why (keyword status) :planner (some-> (.-reason r) keyword)}
          :else (some-> (executor/refusal (wworld/body-policy c) steps) stop))))))
