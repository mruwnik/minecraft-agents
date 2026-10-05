(ns jobs.movement.go-to
  (:require [engine.ctx :as ctx]
            [engine.jobs.blocks :as b]
            [engine.jobs.escape :as escape]
            [engine.jobs.reach :as reach]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]
            [engine.jobs.watch :as watch]
            [engine.path.near :as near]
            [engine.path.walk :as walk]
            [engine.places :as places]))

(def doc
  "Walk to :pos ([x y z] or {:x :y :z}, fractions floored to the cell) until the body's cell is within :range cells of
  it (range 0: in that cell, 1: next to it).
  - Refused at once, before any walk: a :pos that is not one (:bad-pos), a body with no pathWorld sensing
    (:unsupported). Both give a :refused warn and {:arrived false :reason <it>}.
  - One round is one plan and one walk (engine.path.walk, about 100 ms of search, at most 60 s of walking). A search
    that needs more rounds walks on toward where it has got to, or not at all while it goes on. A goal in unloaded
    land is walked toward round by round, and to the edge of loaded land when that is the only way on.
  - A round that gets more than 1 block nearer is progress. Three rounds in a row without progress give up, and so
    does a goal the planner proves walled in (:goal-enclosed), at once.
  - :escalate (default true): a body that is shut in (engine.jobs.reach/enclosed?), not in its own shelter, and whose
    search ran out of land (:exhausted or :goal-enclosed) makes a way instead of giving up, at most 3 times per go-to, one child job per
    round (engine.jobs.escape/choose): jobs.access.pillar up out of a pit when it carries enough blocks; else
    jobs.access.clear-path through a wall up to 3 thick toward the goal; else jobs.access.stair up out of a pit or
    toward a higher goal; else a walk to the nearest wall first. Each emits go-to.escalated {:step :why :n}. Once
    through, it puts back what clear-path or stair dug (jobs.blocks.place, the dug block or its drop, when
    carried): info go-to.restored, warn go-to.restore-skipped for cells it could not. Pillar blocks are left to
    jobs.access.cleanup. Its children's walks never escalate. Jobs that must not change the world on the way pass
    :escalate false.
  - Gives up with {:arrived false :reason :unreachable :why ...} and an :unreachable warn: :why is the planner's
    reason (:exhausted, :goal-enclosed, :door-stuck with :cells, :one-way with :near and :one-way ...), or :stuck
    (:kind the step, :detail the executor's text), :off-plan, :steer-failed, :no-progress or :searching. A failed
    escalation adds :escalation {:step :reason ...}, the child's reason or wait.
  - Success is {:arrived true}. The result is also a :result info event.
  - Every walking round writes a :moved memory entry {:from :to :status :target} (arrived, partial or blocked) for
    the stuck trigger.
  - :doors (engine.path.pass): :shut (default) opens a shut door, gate or trapdoor with an empty hand, passes and
    shuts what it opened. :leave-open leaves it open. :never treats them as walls. A door in or beside another
    owner's zone is always shut again. A door that will not open is a wall for that walk (:door-stuck). Iron
    doors are walls.")

(def args
  {:pos {:doc "target position {:x :y :z}" :default nil}
   :range {:doc "how close counts as there, in cells" :default 1}
   :doors {:doc "what to do at shut doors, gates and trapdoors: :shut (open, pass, shut again what the walk opened), :leave-open (open and pass), :never (walls)"
           :default :shut}
   :escalate {:doc "when shut in with no way out, pillar, stair or dig a door to get out (and put back what was dug); false: give up"
              :default true}})

(def max-blocked 3)

(def max-searching
  "Rounds in a row whose search is still going on (walk/round-budget expansions each) before go-to gives up: far more than
  any one search takes (it ends after the planner's maxNodes); only a body moved off its search's start every round
  (pushed, drifting) starts afresh each time."
  100)

(defn check [_c] true)

(defn finish!
  "Hand result over (ctx/result!), emit it as a :result info event (its :kind is the event's :refused-kind: an event's own
  :kind is what it is), end the job."
  [c result]
  (ctx/result! c result)
  (ctx/emit! c :result :info (cond-> (dissoc result :kind)
                               (:kind result) (assoc :refused-kind (:kind result))))
  :done)

(defn ^:async arrived!
  "Look round once if the place is risky (engine.jobs.watch), then finish: the next job starts facing along the walk."
  [c]
  (await (watch/watch! c {}))
  (finish! c {:arrived true}))

(defn give-up-fields
  "What a fruitless round's result says about why: {:why :kind :detail ...}, only the keys it has. A :no-path says the planner's
  :reason (:why, :no-path when it has none), the :kind of step that cannot be walked, and what its reason carries (:door-stuck's
  :cells, :one-way's :near and :one-way); a walk that got stuck says :stuck, the :move it was stuck on as :kind and the
  executor's :why text as :detail; one that left its plan says :off-plan; a failed steer says :steer-failed with its reason
  as :detail; a walk that ended with the body no nearer says :no-progress. A body sealed in its own dig-in shelter adds
  :inside-own-shelter (its cell) and a :hint (the shelter job lets it out by day, or run dig-in leave)."
  [{:keys [status reason kind move step] :as result}]
  (case status
    :stuck (cond-> {:why :stuck}
             move (assoc :kind move)
             (:why result) (assoc :detail (:why result)))
    :off-plan (cond-> {:why :off-plan}
                step (assoc :detail (str "left the plan at step " step)))
    :failed (cond-> {:why :steer-failed}
              reason (assoc :detail (str reason)))
    (cond
      (or reason (= :no-path status)) (merge {:why (if reason (keyword reason) :no-path)}
                                             (select-keys result [:kind :cells :near :one-way]))
      :else {:why :no-progress})))

(defn give-up!
  ([c pos tries status result] (give-up! c pos tries status result nil))
  ([c pos tries status result extra]
   (let [{:keys [why kind] :as fields} (merge (give-up-fields result) (sh/shelter-hint c) extra)
         more (dissoc fields :why :kind)]
     (ctx/emit! c :unreachable :warn (cond-> (merge {:target pos :tries tries :status status :why why
                                                     :text (str "gave up walking to " pos)}
                                                    more)
                                       kind (assoc :refused-kind kind)))
     (finish! c (merge {:arrived false :reason :unreachable} fields)))))

(defn refuse! [c {:keys [reason message]}]
  (ctx/emit! c :refused :warn {:reason reason :text message})
  (finish! c {:arrived false :reason reason}))

;; ------------------------------------------------------------------ escalation

(def max-escalations 3)

(def escalate-reasons
  "Planner reasons that prove the way needs the world changed. A stuck or off-plan walk is the walker's fault and
  never escalates: changing the world there would hide a go-to bug."
  #{:exhausted :goal-enclosed})

(defn feet-cell [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn escalation-job
  "[job args] of the child that carries out escalation e (engine.jobs.escape/choose)."
  [{:keys [step] :as e}]
  (case step
    :pillar ['jobs.access.pillar {:height (:height e) :item (:item e)}]
    :stair ['jobs.access.stair {:dir :up :heading (:heading e) :steps (:steps e)}]
    :clear-path ['jobs.access.clear-path {:heading (:heading e)}]
    :approach ['jobs.movement.go-to {:pos (zipmap [:x :y :z] (:pos e)) :range 0 :escalate false}]))

(defn succeeded? [{:keys [step]} result]
  (if (= :approach step) (true? (:arrived result)) (= :done (:status result))))

(defn ^:async escalate!
  "Start the next escalation for the give-up kept in memory (:give-up), or give up with it when there is none."
  [c pos]
  (let [{:keys [tries status result]} (:give-up (ctx/mem c))
        n (inc (:escalations (ctx/mem c) 0))
        e (escape/choose (:primitives c) (feet-cell c) (mapv #(js/Math.floor (% pos)) [:x :y :z]))]
    (cond
      (> n max-escalations) (give-up! c pos tries status result {:escalation {:step :spent :n max-escalations}})
      (= :none (:step e)) (give-up! c pos tries status result {:escalation {:step :none}})
      :else
      (do (ctx/emit! c :go-to.escalated :info {:step (:step e) :why (:why (give-up-fields result)) :n n :at (feet-cell c)
                                               :text (str "no way out on foot: " (name (:step e)))})
          (ctx/update-mem! c assoc :escalations n :escalation e)
          :continue))))

(defn escalate?
  "Whether a give-up with result may escalate instead. Never out of the body's own shelter: it is shut in on purpose."
  [c result]
  (and (:escalate (:args c))
       (contains? escalate-reasons (some-> (:reason result) keyword))
       (not (sh/sheltered-in c))
       (reach/enclosed? (:primitives c))))

(defn ^:async give-up-or-escalate! [c pos tries status result]
  (if (escalate? c result)
    (do (ctx/update-mem! c assoc :give-up {:tries tries :status status
                                           :result (select-keys result [:status :reason :kind :cells :near :one-way])})
        (await (escalate! c pos)))
    (give-up! c pos tries status result)))

(defn escalation-failed! [c pos detail]
  (let [{:keys [tries status result]} (:give-up (ctx/mem c))]
    (give-up! c pos tries status result {:escalation detail})))

(defn escalated!
  "The child made a way: walk again from a fresh count. Cells stair or clear-path dug are put back first."
  [c {:keys [step]} result]
  (let [dug (when (#{:stair :clear-path} step) (vec (sort-by (comp second :cell) (:dug result))))]
    (ctx/update-mem! c #(cond-> (-> % (dissoc :escalation :best) (assoc :blocked 0 :searching 0 :frontier-best {}))
                          (= :approach step) (assoc :escalate-now true)
                          (seq dug) (assoc :restore dug)))
    :continue))

(defn ^:async escalation-round!
  "One round of the escalation child. Its wait or its failure gives up, with the child's reason."
  [c pos]
  (let [{:keys [step] :as e} (:escalation (ctx/mem c))
        [job args] (escalation-job e)]
    (if-let [wait (b/child-wait c :escalation job args)]
      (escalation-failed! c pos (merge {:step step} wait))
      (let [r (await (ctx/call-child c :escalation job args))
            res (ctx/child-result c :escalation)]
        (cond
          (= :continue r) :continue
          (= :declined r) (escalation-failed! c pos {:step step :reason :not-ready})
          (succeeded? e res) (escalated! c e res)
          :else (escalation-failed! c pos (merge {:step step}
                                                 (select-keys res [:reason :why :cell :block :at :short :detail]))))))))

(defn restore-done!
  "Tell what was put back and what was not, and forget both."
  [c]
  (let [{:keys [restored skipped]} (ctx/mem c)]
    (when (seq restored)
      (ctx/emit! c :go-to.restored :info {:cells restored :text (str "put back " (count restored) " dug blocks")}))
    (when (seq skipped)
      (ctx/emit! c :go-to.restore-skipped :warn {:cells skipped
                                                 :text (str "could not put back " (count skipped) " dug blocks")}))
    (ctx/update-mem! c dissoc :restore :restored :skipped)))

(defn ^:async restore-round!
  "Put back the next cell an escalation dug (jobs.blocks.place, the dug block or what it drops), one child round per
  round. A cell that is no longer air, or that the place child would wait on (the body in it, nothing to place), is
  skipped."
  [c]
  (let [p (:primitives c)
        {:keys [cell block]} (first (:restore (ctx/mem c)))
        args {:pos cell :any-of (vec (distinct (cons block (b/drops-of p block))))}
        next! (fn [k entry]
                (ctx/update-mem! c #(-> % (update :restore (comp vec rest)) (update k (fnil conj []) entry)))
                (when (empty? (:restore (ctx/mem c))) (restore-done! c))
                :continue)
        skip! (fn [why] (next! :skipped {:cell cell :block block :why why}))
        now (u/block-name p (zipmap [:x :y :z] cell))]
    (if-not (b/air now)
      (skip! :changed)
      (if-let [wait (b/child-wait c :restore 'jobs.blocks.place args)]
        (skip! (:reason wait))
        (let [r (await (ctx/call-child c :restore 'jobs.blocks.place args))
              res (ctx/child-result c :restore)]
          (cond
            (not= :done r) :continue
            (:placed res) (next! :restored cell)
            :else (skip! (:reason res))))))))

(defn forget-known-land!
  "Forget the land earlier searches knew to their end (walk/known-land) at the go-to's first walking round and after each
  escalation (:escalations, the world changed): a new go-to, or a dug way, may open a way through it."
  [c]
  (let [epoch (:escalations (ctx/mem c) 0)]
    (when-not (= epoch (:known-epoch (ctx/mem c)))
      (walk/forget-known! c)
      (ctx/update-mem! c assoc :known-epoch epoch))))

(defn known-frontier-result
  "A round's walk result as go-to judges it: a walk to a frontier in land earlier searches knew to their end
  (:frontier-known: there was no other edge) explores nothing, so it is the search's own answer, :exhausted, and never
  progress."
  [result]
  (if (:frontier-known result)
    {:status :no-path :reason :exhausted :frontier-known true}
    result))

(defn ^:async walk! [c pos range doors]
  (let [from (u/self-pos c)
        d (u/dist from pos)
        pw (walk/path-world (:primitives c))]
    (cond
      (u/within? from pos range)
      (arrived! c)

      (nil? pw)
      (refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})

      :else
      (let [_ (forget-known-land! c)
            {walked :result status :status to :to} (await (near/walk-round! c pos range {:doors doors :explore true
                                                                                          :budget walk/round-budget
                                                                                          :progress (empty? (:frontier-best (ctx/mem c)))}))
            result (known-frontier-result walked)
            left (u/dist to pos)
            best (:best (ctx/mem c) d)
            frontier (:frontier result)
            fcell (when frontier (zipmap [:x :y :z] frontier))
            fbests (:frontier-best (ctx/mem c) {})
            fbest (when frontier (get fbests frontier (u/dist from fcell)))
            explored? (boolean (and frontier (< (u/dist to fcell) (dec fbest))))]
        (cond
          (= "arrived" status)
          (arrived! c)

          (= "searching" status)
          (let [n (inc (:searching (ctx/mem c) 0))]
            (ctx/update-mem! c assoc :searching n)
            (if (< n max-searching)
              :continue
              (give-up! c pos (:blocked (ctx/mem c) 0) :searching {:status :no-path :reason :searching})))

          :else
          (let [progress? (or (< left (dec best)) explored?)
                tries (if progress? 0 (inc (:blocked (ctx/mem c) 0)))]
            (ctx/update-mem! c assoc :blocked tries :searching 0 :best (if (< left (dec best)) left best)
                             :frontier-best (cond-> fbests frontier (assoc frontier (min fbest (u/dist to fcell)))))
            (if (and (< tries max-blocked) (not= :goal-enclosed (:reason result)))
              :continue
              (await (give-up-or-escalate! c pos tries status result)))))))))

(defn ^:async round [c]
  (let [parsed (places/parse-pos (:pos (:args c)))
        pos (:pos parsed)
        m (ctx/mem c)]
    (cond
      (:reason parsed) (refuse! c parsed)
      (seq (:restore m)) (await (restore-round! c))
      (:escalation m) (await (escalation-round! c pos))
      (:escalate-now m) (do (ctx/update-mem! c dissoc :escalate-now)
                            (await (escalate! c pos)))
      :else (await (walk! c pos (:range (:args c)) (some-> (:doors (:args c)) keyword))))))
