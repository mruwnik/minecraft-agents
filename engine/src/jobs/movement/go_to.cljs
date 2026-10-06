(ns jobs.movement.go-to
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.escape :as escape]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.walk :as walk]
            [jobs.lib.places :as places]
            [jobs.lib.world :as known]))

(def doc
  "Walk to :pos ([x y z] or {:x :y :z}, fractions floored to the cell) until the body's cell is within :range cells of
  it (range 0: in that cell, 1: next to it).
  - Refused at once, before any walk: a :pos that is not one (:bad-pos), a body with no pathWorld sensing
    (:unsupported). Both give a :refused warn and {:arrived false :reason <it>}.
  - One round is one plan and one walk (jobs.lib.walk, about 100 ms of search, at most 60 s of walking). A search
    that needs more rounds walks on toward where it has got to, or not at all while it goes on. A goal in unloaded
    land is walked toward round by round, and to the edge of loaded land when that is the only way on.
  - A round that gets more than 1 block nearer is progress. Three rounds in a row without progress give up, and so
    does a goal the planner proves walled in (:goal-enclosed), at once.
  - :escalate (default true): a body that is shut in (jobs.lib.reach/enclosed?), not in its own shelter, and whose
    search ran out of land (:exhausted or :goal-enclosed) makes a way instead of giving up, at most 3 times per go-to, one child job per
    round (jobs.lib.escape/choose): jobs.access.pillar up out of a pit when it carries enough blocks; else
    jobs.access.clear-path through a wall up to 3 thick toward the goal; else jobs.access.stair up out of a pit or
    toward a higher goal; else a walk to the nearest wall first. Each emits go-to.escalated {:step :why :n}. Once
    through, it puts back what clear-path or stair dug (jobs.blocks.place, the dug block or its drop, when
    carried): info go-to.restored, warn go-to.restore-skipped for cells it could not. Pillar blocks are left to
    jobs.access.cleanup. Its children's walks never escalate. Jobs that must not change the world on the way pass
    :escalate false.
  - Gives up with {:arrived false :reason :unreachable :why ...} and an :unreachable warn: :why is the planner's
    reason (:exhausted, :goal-enclosed, :door-stuck with :cells, :one-way with :near and :one-way ...), or :stuck
    (:kind the step, :detail the executor's text), :off-plan, :steer-failed, :no-progress or :moved-while-searching; :at is the
    body's feet cell and :near its blocks from the goal. A failed
    escalation adds :escalation {:step :reason ...}, the child's reason or wait.
  - Success is {:arrived true}. The result is also a :result info event.
  - Every walking round writes a :moved memory entry {:from :to :status :target} (arrived, partial or blocked) for
    the stuck trigger.
  - :doors (jobs.lib.pass): :shut (default) opens a shut door, gate or trapdoor with an empty hand, passes and
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
  "Rounds in a row whose search is still going on and began afresh (walk/round-budget expansions each) before go-to gives
  up: a search that goes on from the same cell always ends after the planner's maxNodes; only a body moved off its
  search's start every round (pushed, drifting) starts afresh each time."
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
  "Look round once if the place is risky (jobs.lib.watch), then finish: the next job starts facing along the walk."
  [c]
  (await (watch/watch! c {}))
  (finish! c {:arrived true}))

(defn feet-cell [c]
  (let [{:keys [x y z]} (reach/standing-cell (:primitives c))]
    [x y z]))

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
   (let [{:keys [why kind] :as fields} (merge {:at (feet-cell c) :near (js/Math.round (u/dist (u/self-pos c) pos))}
                                              (give-up-fields result) (sh/shelter-hint c) extra)
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
  never escalates: changing the world there would hide a go-to bug. :start-enclosed is the search running out of land
  with no loaded edge reached (a sealed pen, the goal far and unloaded)."
  #{:exhausted :goal-enclosed :start-enclosed})

(defn escalate-reason?
  "Whether a walk result's reason proves the way needs the world changed (escalate-reasons), or is the planner's way
  through a door that :doors :never refuses (:abilities of kind :open): such a door is a wall."
  [result doors]
  (let [reason (some-> (:reason result) keyword)]
    (boolean (or (contains? escalate-reasons reason)
                 (and (= :abilities reason) (= :never doors) (= :open (some-> (:kind result) keyword)))))))

(defn escalation-job
  "[job args] of the child that carries out escalation e (jobs.lib.escape/choose)."
  [{:keys [step] :as e}]
  (case step
    :pillar ['jobs.access.pillar {:height (:height e) :item (:item e)}]
    :stair ['jobs.access.stair {:dir :up :heading (:heading e) :steps (:steps e)}]
    :clear-path ['jobs.access.clear-path {:heading (:heading e)}]
    :approach ['jobs.movement.go-to {:pos (zipmap [:x :y :z] (:pos e)) :range 0 :escalate false}]))

(defn succeeded? [{:keys [step]} result]
  (if (= :approach step) (true? (:arrived result)) (= :done (:status result))))

(defn may-dig-fn
  "(may-dig? cell): no other owner's zone, claim or plan footprint refuses a dig there (jobs.lib.tidy/refusal)."
  [c]
  (fn [cell] (nil? (tidy/refusal c :dig (zipmap [:x :y :z] cell)))))

(def placed-policy
  "Body memory policy of :escalation-placed, the cells go-to put a block back in: later escalations may dig them."
  {:cap 100 :ttl (* 6 60 60 1000)})

(defn own-placed-fn
  "(own? cell block): go-to itself placed block at cell (a put-back), so it is not someone's build."
  [c]
  (let [placed (set (map (comp (juxt :cell :block) :data) (ctx/entries c :escalation-placed)))]
    (fn [cell block] (contains? placed [(vec cell) block]))))

(defn note-placed! [c cell]
  (when-let [block (u/block-name (:primitives c) (zipmap [:x :y :z] cell))]
    (ctx/remember! c :escalation-placed {:cell (vec cell) :block block} placed-policy)))

(defn planned-cells
  "The solid cells escalation e from feet would cut, [{:cell :block}]: the stair's cuts or clear-path's door."
  [p {:keys [step heading steps]} feet]
  (let [block-at (escape/block-at-of p)
        cells (case step
                :stair (escape/stair-cuts feet (escape/heading-dirs heading) steps)
                :clear-path (:cells (escape/door block-at feet (escape/heading-dirs heading) escape/max-door))
                nil)]
    (vec (for [cell cells :let [n (block-at cell)] :when (escape/solid? n)] {:cell cell :block n}))))

(defn own-holes
  "The ledger entries (jobs.lib.tidy :tidy, body memory) of cells this go-to's escalations dug, lowest first."
  [c]
  (->> (tidy/entries c)
       (filter #(and (:escalation %) (= (:root c) (:job %)) (= (:id c) (:go-to %))))
       (sort-by (comp second :cell))
       vec))

(defn note-holes!
  "Write every cell of dug ({:cell :block}) that is air now and not yet noted to the ledger, as a :dig entry with
  :escalation true and :any-of, the items that put it back (the block or what it drops). Body memory outlives the
  round, a cut, a cancel and a restart: jobs.survival.restore-broken puts back what go-to did not."
  [c dug]
  (let [p (:primitives c)
        noted (set (map :cell (tidy/entries c)))]
    (doseq [{:keys [cell block]} (distinct dug)
            :when (and (not (noted cell)) (b/air (u/block-name p (zipmap [:x :y :z] cell))))]
      (tidy/record! c {:cell cell :action :dig :was block :escalation true :go-to (:id c)
                       :any-of (vec (distinct (cons block (b/drops-of p block))))}
                    "air"))))

(defn ^:async escalate!
  "Start the next escalation for the give-up kept in memory (:give-up), or give up with it when there is none (or
  with failed, the detail of the escalation that just failed, when there is none left after it)."
  [c pos failed]
  (let [{:keys [tries status result]} (:give-up (ctx/mem c))
        n (inc (:escalations (ctx/mem c) 0))
        feet (feet-cell c)
        e (escape/choose (:primitives c) feet (mapv #(js/Math.floor (% pos)) [:x :y :z]) (may-dig-fn c)
                       {:own? (own-placed-fn c) :skip (:skipped-steps (ctx/mem c) #{})})]
    (cond
      (> n max-escalations) (give-up! c pos tries status result {:escalation {:step :spent :n max-escalations}})
      (= :none (:step e)) (give-up! c pos tries status result {:escalation (or failed e)})
      :else
      (do (ctx/emit! c :go-to.escalated :info {:step (:step e) :why (:why (give-up-fields result)) :n n :at feet
                                               :text (str "no way out on foot: " (name (:step e)))})
          (ctx/update-mem! c assoc :escalations n :escalation e :escalation-from feet
                           :planned (planned-cells (:primitives c) e feet) :holes-before (count (own-holes c)))
          :continue))))

(defn escalate?
  "Whether a give-up with result may escalate instead: the body is shut in (reach/enclosed?) and no door or gate
  borders where it can walk (escape/door-beside?: that is its way out; an iron door, or any door under :doors :never,
  is a wall). Never out of
  the body's own shelter: it is shut in on purpose."
  [c result]
  (and (:escalate (:args c))
       (escalate-reason? result (some-> (:doors (:args c)) keyword))
       (not (sh/sheltered-in c))
       (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword))
       (not (escape/door-beside? (:primitives c) (feet-cell c) (some-> (:doors (:args c)) keyword)))))

(defn ^:async give-up-or-escalate! [c pos tries status result]
  (if (escalate? c result)
    (do (ctx/update-mem! c assoc :give-up {:tries tries :status status
                                           :result (select-keys result [:status :reason :kind :cells :near :one-way])})
        (await (escalate! c pos nil)))
    (give-up! c pos tries status result)))

(defn escalated!
  "The child made a way (or part of one): walk again from a fresh count. Its holes are put back once a walk round gets
  on past it (restore-next!)."
  [c {:keys [step]}]
  (ctx/update-mem! c #(cond-> (-> % (dissoc :escalation :escalation-from :planned :holes-before :best)
                                  (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {}))
                        (= :approach step) (assoc :escalate-now true)
                        (not= :approach step) (assoc :restore-pending true)))
  :continue)

(defn partly-made?
  "Whether the escalation changed something before it stopped: it dug a cell or moved the body. Then walking again
  may get through (a stair stopped short of a stone step may already reach the rim)."
  [c]
  (let [m (ctx/mem c)]
    (or (> (count (own-holes c)) (:holes-before m 0))
        (not= (:escalation-from m) (feet-cell c)))))

(defn ^:async escalation-failed!
  "The child stopped or would wait: when it changed something, walk again (escalated!) after a go-to.escalation-stopped
  warn; else try the next method (escalate!, never the same step twice, at most max-escalations in all), giving up
  with detail when none is left."
  [c pos {:keys [step] :as detail}]
  (if (partly-made? c)
    (do (ctx/emit! c :go-to.escalation-stopped :warn (assoc detail :text (str (name step) " stopped part way: "
                                                                              (some-> (:reason detail) name)
                                                                              "; walking again")))
        (escalated! c detail))
    (do (ctx/emit! c :go-to.escalation-stopped :warn (assoc detail :text (str (name step) " failed: "
                                                                              (some-> (:reason detail) name)
                                                                              "; trying another way")))
        (ctx/update-mem! c update :skipped-steps (fnil conj #{}) step)
        (await (escalate! c pos detail)))))

(defn ^:async escalation-round!
  "One round of the escalation child. After it, every planned cell now dug is in the ledger (note-holes!). Its wait or
  its failure gives up, with the child's reason, unless it got part of the way."
  [c pos]
  (let [{:keys [step] :as e} (:escalation (ctx/mem c))
        [job args] (escalation-job e)]
    (if-let [wait (b/child-wait c :escalation job args)]
      (escalation-failed! c pos (merge {:step step} wait))
      (let [r (await (ctx/call-child c :escalation job args))
            res (ctx/child-result c :escalation)]
        (note-holes! c (concat (:planned (ctx/mem c)) (when (not= :continue r) (:dug res))))
        (cond
          (= :continue r) :continue
          (= :declined r) (escalation-failed! c pos {:step step :reason :not-ready})
          (succeeded? e res) (escalated! c e)
          :else (escalation-failed! c pos (merge {:step step}
                                                 (select-keys res [:reason :why :cell :block :at :short :detail]))))))))

(defn restore-done!
  "Tell what was put back and what was left, and end the put-back (the walk goes on from a fresh count)."
  [c]
  (let [{:keys [restored skipped]} (ctx/mem c)]
    (when (seq restored)
      (ctx/emit! c :go-to.restored :info {:cells restored :text (str "put back " (count restored) " dug blocks")}))
    (when (seq skipped)
      (ctx/emit! c :go-to.restore-skipped :warn {:cells skipped
                                                 :text (str "left " (count skipped) " dug blocks for restore-broken")}))
    (ctx/update-mem! c #(-> % (dissoc :restore-now :restore-seen :restored :skipped :changed :best)
                            (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {})))))

(defn skip-why
  "Why hole e must not be filled now, or nil: :changed (no longer air: forgotten), :occupied (the body is in it),
  :needed (filling it would shut in a body that has room now: the stair it stands on, its way out)."
  [p {:keys [cell]}]
  (cond
    (not (b/air (u/block-name p (zipmap [:x :y :z] cell)))) :changed
    (tidy/in-body? p cell) :occupied
    (and (not (reach/enclosed? p)) (reach/enclosed? p #{(vec cell)})) :needed))

(defn ^:async restore-round!
  "Put back the next hole this job's escalations dug (own-holes), lowest first, with jobs.blocks.place (the dug block or
  what it drops), one child round per round. It runs once the body got on past the escalation (restore-next!). A hole
  skip-why refuses, or the place child would wait on (nothing to place), stays in the ledger for restore-broken (a
  :changed one is forgotten). A placed one is forgotten."
  [c]
  (let [p (:primitives c)
        seen (:restore-seen (ctx/mem c) #{})
        e (first (remove #(seen (:cell %)) (own-holes c)))]
    (if (nil? e)
      (do (restore-done! c) :continue)
      (let [{:keys [cell was any-of]} e
            args {:pos cell :any-of any-of}
            done! (fn [k entry]
                    (ctx/update-mem! c #(-> % (update :restore-seen (fnil conj #{}) cell) (update k (fnil conj []) entry)))
                    :continue)
            skip! (fn [why]
                    (if (= :changed why)
                      (do (tidy/forget-cell! c cell) (done! :changed cell))
                      (done! :skipped {:cell cell :block was :why why})))]
        (if-let [why (skip-why p e)]
          (skip! why)
          (if-let [wait (b/child-wait c :restore 'jobs.blocks.place args)]
            (skip! (:reason wait))
            (let [r (await (ctx/call-child c :restore 'jobs.blocks.place args))
                  res (ctx/child-result c :restore)]
              (cond
                (not= :done r) :continue
                (:placed res) (do (tidy/forget-cell! c cell) (note-placed! c cell) (done! :restored cell))
                :else (skip! (:reason res))))))))))

(defn restore-next!
  "The body got on past an escalation (a walk round that arrived or made progress): put back its holes next round
  (restore-round!). Not before: a stair or door short of the way on is still the body's way, and refilling it under
  the body would strand it."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :restore-pending) (assoc :restore-now true)))
  :continue)

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
  progress, unless it brings an edge seen earlier and now out of view nearer (:frontier-target)."
  [result]
  (if (:frontier-known result)
    (cond-> {:status :no-path :reason :exhausted :frontier-known true}
      (:frontier-target result) (assoc :frontier-target (:frontier-target result)))
    result))

(defn foreign?
  "Whether cell [x y z] lies in, or next to, a zone of an owner other than self-name."
  [zones self-name [x y z]]
  (boolean (some (fn [{:keys [owner min max]}]
                   (and (not= owner self-name)
                        (every? true? (map (fn [v lo hi] (<= (dec lo) v (inc hi))) [x y z] min max))))
                 zones)))

(defn shut-foreign
  "The walker's :shut-also (jobs.lib.pass): a block opened in or next to another owner's zone is shut again whatever
  :doors says, so a pass through somebody's pen lets nothing out."
  [c]
  (fn [cell] (foreign? (known/zones c) (ctx/self-name c) cell)))

(defn ^:async walk! [c pos range doors]
  (let [from (u/self-pos c)
        d (u/dist from pos)
        pw (walk/path-world (:primitives c))]
    (cond
      (or (u/within? from pos range) (u/within? (reach/standing-cell (:primitives c)) pos range))
      (if (:restore-pending (ctx/mem c)) (restore-next! c) (arrived! c))

      (nil? pw)
      (refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})

      :else
      (let [_ (forget-known-land! c)
            {walked :result status :status to :to} (await (near/walk-round! c pos range {:doors doors :explore true
                                                                                          :shut-also (shut-foreign c)
                                                                                          :budget walk/round-budget
                                                                                          :progress (empty? (:frontier-best (ctx/mem c)))}))
            result (known-frontier-result walked)
            left (u/dist to pos)
            best (:best (ctx/mem c) d)
            frontier (:frontier result)
            fcell (when frontier (zipmap [:x :y :z] frontier))
            fbests (:frontier-best (ctx/mem c) {})
            fbest (when frontier (get fbests frontier (u/dist from fcell)))
            explored? (boolean (and frontier (< (u/dist to fcell) (dec fbest))))
            target (:frontier-target result)
            tbests (:target-best (ctx/mem c) {})
            tdist (when target (u/dist to {:x (first target) :y (:y to) :z (second target)}))
            nearer? (boolean (and target (< tdist (dec (get tbests target js/Infinity)))))]
        (cond
          (= "arrived" status)
          (if (:restore-pending (ctx/mem c)) (restore-next! c) (arrived! c))

          (= "searching" status)
          (let [n (cond-> (:searching (ctx/mem c) 0) (:fresh walked) inc)]
            (ctx/update-mem! c assoc :searching n)
            (if (< n max-searching)
              :continue
              (give-up! c pos (:blocked (ctx/mem c) 0) :searching {:status :no-path :reason :moved-while-searching})))

          :else
          (let [progress? (or (< left (dec best)) explored? nearer?)
                tries (if progress? 0 (inc (:blocked (ctx/mem c) 0)))]
            (ctx/update-mem! c assoc :blocked tries :searching 0 :best (if (< left (dec best)) left best)
                             :frontier-best (cond-> fbests frontier (assoc frontier (min fbest (u/dist to fcell))))
                             :target-best (cond-> tbests target (assoc target (min tdist (get tbests target js/Infinity)))))
            (when (and progress? (:restore-pending (ctx/mem c))) (restore-next! c))
            (if (and (< tries max-blocked) (not= :goal-enclosed (:reason result)))
              :continue
              (await (give-up-or-escalate! c pos tries status result)))))))))

(defn ^:async round [c]
  (let [parsed (places/parse-pos (:pos (:args c)))
        pos (:pos parsed)
        m (ctx/mem c)]
    (cond
      (:reason parsed) (refuse! c parsed)
      (:restore-now m) (await (restore-round! c))
      (:escalation m) (await (escalation-round! c pos))
      (:escalate-now m) (do (ctx/update-mem! c dissoc :escalate-now)
                            (await (escalate! c pos nil)))
      :else (await (walk! c pos (:range (:args c)) (some-> (:doors (:args c)) keyword))))))
