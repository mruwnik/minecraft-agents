(ns jobs.lib.walk.drive
  "The walk driver's loop over jobs.lib.walk's pieces: follow! (walk a plan, plan again in the same call), walk-to! (the whole
  loop: settle, plan, follow, plan again when off the plan) and way-back."
  (:require [engine.ctx :as ctx]
            [engine.path.executor :as executor]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.watch :as wwatch]
            [jobs.lib.walk.world :as wworld]))

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
      (let [t (js/performance.now)
            walls (wwatch/mob-cells c steps k)
            plan (await (plan-fn walls))
            ms (- (js/performance.now) t)]
        (if (or (walkable? plan) (>= n mob-waits))
          (if (or (walkable? plan) (empty? walls))
            [plan ms]
            (let [t (js/performance.now) plan (await (plan-fn []))] [plan (- (js/performance.now) t)]))
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

(defn ^:async look-over-edge!
  "A floor the body learned is not there (a drop at the takeoff's edge): look at the ground round the body at several
  distances, as a player looks over an edge once, so the next plan does not drop at the next unseen column."
  [c [_ y _]]
  (let [{:keys [x z]} (u/self-pos c)]
    (loop [points (for [d [3 7] [dx dz] [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]]
                    {:x (+ x (* d dx)) :y (+ y 0.5) :z (+ z (* d dz))})]
      (when-let [pt (first points)]
        (await (ctx/act c :look (clj->js {:pos pt})))
        (look/see! c)
        (recur (rest points))))))

(defn ^:async follow!
  "Walk plan (a plan-walk! result) and plan again from the body's cell, in the same call, whenever the walk stops for it:
  the look-ahead saw the way change (:changed: the new plan is walked), a partial plan is due a refresh (:refresh: the new
  plan is walked only when take-refresh? says it is clearly better, else the rest of the old one), or the body is stuck with
  a mob in its way (:mob: wait, plan round it). A replan that proves the goal cut off or enclosed ends the call with that
  no-walk; so does one the caller refused (plan-fn's {:refused true}, with its :stop) for any why but :refresh, whose old
  plan nothing speaks against. At most max-watch-replans; past that the plan is walked unwatched (a kept plan counts as a replan, and is watched). A replan with :learned (a gate or the look-ahead gave an unseen cell up or found it solid) is not counted against it, up to free-learned of them: a void edge teaches one column per replan.
  opts: :plan-fn (fn [walls]) -> a plan-walk! result (or a promise of one: plan-walk!) from where the body stands now, the
  cells {:x :y :z} read as walls;
  :walk-fn (fn [steps watch]) -> [done ms] (walk! or jobs.lib.pass/walk!); :to the goal cell; :policy for no-walk;
  :dangers true: a danger newly sensed near the way ahead is planned round once (watch-stop); a search that has not
  finished ends the call with its :searching no-walk (the old plan is never walked on past the danger);
  a :health stop (the damage still planned is over what the body may spend now) plans again and walks the new plan, or
  ends the call with its no-walk when that plan is not walkable (the caller decides: go-to heals or goes over its budget);
  :announce! (fn [:replan data]) per replan, data {:why :ms :kept :replans :at :text}.
  {:done :plan :ms :walked :replans :damage}: done the last walk's done map, or the no-walk result of a replan that has no way; plan
  the plan in force at the end; ms the time in walks; walked the blocks of plan walked; damage the hp the steps walked planned."
  [c plan {:keys [plan-fn walk-fn to policy announce! dangers] :or {policy (wworld/body-policy c) announce! (fn [_ _])}}]
  (let [known (when dangers (atom #{}))
        learned (atom #{})
        walkable? (fn [pl] (nil? (wplan/no-walk pl 0 policy)))
        proved-none? (fn [pl] (let [nw (wplan/no-walk pl 0 policy)]
                                (or (contains? #{:goal-cut-off :goal-enclosed} (:reason nw)) (:stopped-short nw))))
        searching? (fn [pl] (= :searching (:status (wplan/no-walk pl 0 policy))))
        cut-length (fn [steps k] (walk/path-length (subvec steps 0 (min (count steps) (max 1 k)))))]
    (loop [plan plan steps (:steps plan) n 0 ms 0 walked 0 damage 0]
      (let [spent (- n (min free-learned (count @learned)))
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
                                        :at (:at done)
                                        :text (str "re-plan " (inc n) " (" (name why) (when kept ", kept the plan") ")")}))]
        (cond
          (and (= :replan (:status done)) (= :mob (:why done)))
          (let [t (js/performance.now)
                fresh (await (plan-fn (:cells done)))
                plan-ms (- (js/performance.now) t)]
            (tell! :mob plan-ms false)
            (cond
              (walkable? fresh) (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              (:refused fresh) (finish (wplan/no-walk fresh 0 policy) fresh walked')
              :else (recur plan (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))

          (and (= :replan (:status done)) (= :air (:why done)))
          (finish {:status :stair-cheaper :why {:air-spent true}} plan walked')

          (and (= :replan (:status done)) (= :health (:why done)))
          (let [t (js/performance.now)
                fresh (await (plan-fn []))
                plan-ms (- (js/performance.now) t)]
            (tell! :health plan-ms false)
            (if (walkable? fresh)
              (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              (finish (wplan/no-walk fresh 0 policy) fresh walked')))

          (and (= :replan (:status done)) (= :danger (:why done)))
          (let [t (js/performance.now)
                fresh (await (plan-fn []))
                plan-ms (- (js/performance.now) t)]
            (tell! :danger plan-ms (not (or (walkable? fresh) (searching? fresh) (:refused fresh))))
            (when (or (:stopped-short (wplan/no-walk fresh 0 policy)) (and (searching? fresh) (:stopped-short fresh)))
              (await (back-off! c fresh (subvec steps 0 (min (count steps) (inc k))) walk-fn policy)))
            (cond
              (walkable? fresh) (recur (assoc fresh :ms plan-ms) (:steps fresh) (inc n) ms walked' damage')
              ;; an unfinished search: the old plan runs past the danger, so the caller searches on from here
              (or (proved-none? fresh) (searching? fresh) (:refused fresh)) (finish (wplan/no-walk fresh 0 policy) fresh walked')
              :else (recur (assoc plan :ms plan-ms) (subvec steps (max 0 (dec k))) (inc n) ms walked' kept-damage)))

          (and (= :replan (:status done)) (< spent wwatch/max-watch-replans))
          (let [_ (wsight/note-missed! (:pw plan) done)
                _ (when (:learned done) (swap! learned conj [(:cells done) (:why done)]))
                _ (when (and (:learned done) (= :changed (:why done)) (not (:felt done)))
                    (await (look-over-edge! c (first (:cells done)))))
                t (js/performance.now)
                fresh (await (plan-fn (or (:walls done) [])))
                plan-ms (- (js/performance.now) t)
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
