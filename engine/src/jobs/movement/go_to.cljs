(ns jobs.movement.go-to
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
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
  it (range 0: in that cell, 1: next to it). :place (a name such as :home, set by jobs.memory.set-place) walks to
  that place's recorded position instead of :pos.
  - Refused at once, before any walk: a :pos that is not one (:bad-pos), a :place that is not a valid name (:bad-name), a :tolls entry that is not {:x :y :z :factor} of finite numbers (:bad-tolls)
    or has no recorded position (:unknown-place), a body with no pathWorld sensing (:unsupported). Both give a :refused warn and {:status :stopped :arrived false :reason <it> :text}.
  - One call is one whole attempt: it plans and walks (jobs.lib.walk: slices of about 100 ms of search, walks of at
    most 60 s each) until it arrives or gives up. A search that needs more slices walks on toward where it has got
    to, or not at all while it goes on; every iteration awaits a pace-ms timer. A goal in unloaded land is walked
    toward walk by walk, and to the edge of loaded land when that is the only way on. It returns :continue only while
    an escalation or put-back child is waiting on the world.
  - :dark false plans dark cells like lit ones (default: a dark cell costs twice a lit one, so a lit route up to about 2x
    longer is taken: a cell seen and dark, or one never seen at night; look/dark-fn).
  - :dangers false plans straight past known dangers (default: the plans keep away from them, jobs.lib.threats). :leg-s n
    walks one leg of at most n s and, when it got more than 1 block nearer, ends {:arrived false :leg true} (job :done)
    for a caller chasing a moving target to call again; a leg that got no nearer goes on as any round.
  - A call starts afresh from the world: its counters are per call, and a saved escalation is dropped when the body
    is already there (a cut call's memory is a hint).
  - A walk that gets more than 1 block nearer is progress. Three walks in a row without progress give up, and so
    does a goal the planner proves walled in (:goal-enclosed, :goal-cut-off), at once.
  - A walk stuck or off its plan at a cell is a go-to.walker-fault warn; the next plans of the call cost that cell
    (up to 8) far more (walk/with-avoid), so a way round is taken when there is one.
  - :escalate (default true): a body that is shut in (jobs.lib.reach/enclosed?), not in its own shelter, and whose
    search ran out of land (:exhausted, :goal-enclosed or :goal-cut-off) makes a way instead of giving up, at most 3 times per go-to, one child job
    each (jobs.lib.escape/choose): jobs.access.pillar up out of a pit when it carries enough blocks; else
    jobs.access.clear-path through a wall up to 3 thick toward the goal; else jobs.access.stair up out of a pit or
    toward a higher goal; else a walk to the nearest wall first. Each emits go-to.escalated {:step :why :n}. Once
    through, it puts back what clear-path or stair dug (jobs.blocks.place, the dug block or its drop, when
    carried): info go-to.restored, warn go-to.restore-skipped for cells it could not. Pillar blocks are left to
    jobs.access.cleanup. Its children's walks never escalate. Jobs that must not change the world on the way pass
    :escalate false.
  - Gives up with {:status :stopped :arrived false :reason :unreachable :why ... :text words} (the job ends :stopped) and an :unreachable warn: :why is the planner's
    reason (:exhausted, :goal-enclosed, :goal-cut-off, :door-stuck with :cells, :one-way with :near and :one-way ...), or :stuck
    (:kind the step, :detail the executor's text), :off-plan, :steer-failed, :no-progress or :moved-while-searching; :at is the
    body's feet cell and :near its blocks from the goal. A failed
    escalation adds :escalation {:step :reason ...}, the child's reason or wait.
  - Success is {:arrived true}. The result is also a :result info event.
  - Every walk writes a :moved memory entry {:from :to :status :target} (arrived, partial or blocked) for
    the stuck trigger.
  - :doors (jobs.lib.pass): :shut (default) opens a shut door, gate or trapdoor with an empty hand, passes and
    shuts what it opened. :leave-open leaves it open. :never treats them as walls. A door in or beside another
    owner's zone is always shut again. A door that will not open is a wall for that walk (:door-stuck). Iron
    doors are walls.")

(def args
  {:pos {:doc "target position [x y z] or {:x :y :z}" :type :pos :default nil}
   :place {:doc "name of a place in body memory (:home, :bed, ...) to walk to instead of :pos" :default nil}
   :range {:doc "how close counts as there, in cells" :default 1}
   :doors {:doc "what to do at shut doors, gates and trapdoors: :shut (open, pass, shut again what the walk opened), :leave-open (open and pass), :never (walls)"
           :default :shut}
   :escalate {:doc "when shut in with no way out, pillar, stair or dig a door to get out (and put back what was dug); false: give up"
              :default true}
   :dangers {:doc "false: plan straight past known dangers (a walk up to the hostile being fought); true: keep away from them" :default true}
   :dark {:doc "false: plan dark cells like lit ones; true: a dark cell (seen dark, or unseen at night) costs twice a lit one" :default true}
   :tolls {:doc "cells to cross only as a last resort, [{:x :y :z :factor}]: each costs factor times its own seconds more (jobs.lib.cost farm-tolls, zone-tolls)" :default nil}
   :leg-s {:doc "walk one leg of at most this many seconds, then end {:arrived false :leg true} so the caller can re-aim at a moving target; nil: the whole way" :default nil}
   :retry {:doc "false: a walk that got no nearer gives up at once instead of walking again (up to 3 times), for a caller that re-aims itself" :default true}
   :look-round {:doc "false: no look round on arrival, for a caller that keeps moving" :default true}
   :warn {:doc "false: a give-up or refusal is an info event, not a warn, for a caller that reports the failure itself"
          :default true}
   :ignore-zones? {:doc "act regardless of zones and claims in the escalation (pillar, stair, clear-path); the rules of the game allow it" :default false}})

(def max-blocked 3)

(def max-fault-cells "Cells a call keeps its plans away from (the walker's faults: fault-cells), the oldest dropped." 8)

(def max-searching
  "Search slices in a row whose search is still going on and began afresh (walk/round-budget expansions each) before
  go-to gives up: a search that goes on from the same cell always ends after the planner's maxNodes; only a body moved
  off its search's start every slice (pushed, drifting) starts afresh each time."
  100)

(def pace-ms "The timer each iteration of a call's loop awaits, so a search loop never starves the event loop." 50)

(defn pace!
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))

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
  (when-not (false? (:look-round (:args c)))
    (await (watch/watch! c {})))
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

(def kind-words
  {:corner-jump "needs a sprint jump past a corner and food is too low"
   :gap-up "needs a step up the body cannot make"
   :gap-down "needs a drop the body cannot survive"
   :gap "needs a jump across a gap the body cannot make"
   :gap-sprint "needs a gap jump that takes a sprint and food is too low"
   :gap-width "needs a jump across a gap wider than the body can jump"})

(def step-words {:stair "dig a stair out" :pillar "pillar out" :clear-path "dig through the wall"})

(def escalation-reason-words
  {:no-tool "no pickaxe" :no-dig "nothing it may dig" :no-headroom "no room above the head"})

(defn escalation-words
  "A failed way out of a shut-in body in words: {:step :reason|:why :n}."
  [{:keys [step reason why n]}]
  (let [r (some-> (or reason why) (as-> k (or (escalation-reason-words k) (str/replace (name k) "-" " "))))]
    (case step
      :spent (str "gave up after " n " ways out")
      :none (str "no way out" (some->> r (str ": ")))
      (str "could not " (or (step-words step) "make a way out") (some->> r (str ": "))))))

(defn give-up-words
  "The give-up reason in words, for the :text of the :unreachable event and the result: no map dumps."
  [pos {:keys [why kind detail escalation]}]
  (str "gave up walking to " (if (map? pos) (let [{:keys [x y z]} pos] [x y z]) pos) ": "
       (if escalation
         (str "shut in here; " (escalation-words escalation))
         (case why
           :abilities (str "it " (or (kind-words kind) (str "needs a " (some-> kind name) " move the body cannot make")))
           :goal-enclosed "the goal is walled in with no way through"
           :goal-cut-off "the goal is cut off by a drop: no walkable way leads to it"
           :start-enclosed "the body is shut in and nothing it can walk reaches out"
           :exhausted "no walkable way leads there from here"
           :no-progress "the walk ended no nearer"
           :stuck (str "the body got stuck" (some->> kind name (str " on ")) (some->> detail (str ": ")))
           :off-plan (or detail "the walk left its plan")
           :steer-failed (str "steering failed" (some->> detail (str ": ")))
           :moved-while-searching "the body was pushed about while the path was searched"
           :one-way "the way back is one-way"
           :door-stuck "a door in the way will not open"
           (str "no path (" (some-> why name) ")")))))

(defn warn-level [c] (if (false? (:warn (:args c))) :info :warn))

(defn give-up!
  ([c pos tries status result] (give-up! c pos tries status result nil))
  ([c pos tries status result extra]
   (let [{:keys [why kind] :as fields} (merge {:at (feet-cell c) :near (js/Math.round (u/dist (u/self-pos c) pos))}
                                              (give-up-fields result) (sh/shelter-hint c) extra)
         more (dissoc fields :why :kind)
         text (give-up-words pos fields)]
     (ctx/emit! c :unreachable (warn-level c) (cond-> (merge {:target pos :tries tries :status status :why why
                                                     :text text}
                                                    more)
                                       kind (assoc :refused-kind kind)))
     (finish! c (merge {:status :stopped :arrived false :reason :unreachable :text text} fields)))))

(defn refuse! [c {:keys [reason message]}]
  (ctx/emit! c :refused (warn-level c) {:reason reason :text message})
  (finish! c {:status :stopped :arrived false :reason reason :text message}))

;; ------------------------------------------------------------------ escalation

(def max-escalations 3)

(def escalate-reasons
  "Planner reasons that prove the way needs the world changed. A stuck or off-plan walk is the walker's fault and
  never escalates: changing the world there would hide a go-to bug. :start-enclosed is the search running out of land
  with no loaded edge reached (a sealed pen, the goal far and unloaded)."
  #{:exhausted :goal-enclosed :goal-cut-off :start-enclosed})

(defn escalate-reason?
  "Whether a walk result's reason proves the way needs the world changed (escalate-reasons), or is the planner's way
  through a door that :doors :never refuses (:abilities of kind :open): such a door is a wall."
  [result doors]
  (let [reason (some-> (:reason result) keyword)]
    (boolean (or (contains? escalate-reasons reason)
                 (and (= :abilities reason) (= :never doors) (= :open (some-> (:kind result) keyword)))))))

(defn escalation-job
  "[job args] of the child that carries out escalation e (jobs.lib.escape/choose); tag marks the holes it digs."
  [{:keys [step] :as e} tag ignore-zones?]
  (case step
    :pillar ['jobs.access.pillar {:height (:height e) :item (:item e) :ignore-zones? ignore-zones?}]
    :stair ['jobs.access.stair {:dir :up :heading (:heading e) :steps (:steps e) :note tag :ignore-zones? ignore-zones?}]
    :clear-path ['jobs.access.clear-path {:heading (:heading e) :note tag :ignore-zones? ignore-zones?}]
    :approach ['jobs.movement.go-to {:pos (zipmap [:x :y :z] (:pos e)) :range 0 :escalate false}]))

(defn succeeded? [{:keys [step]} result]
  (if (= :approach step) (true? (:arrived result)) (= :done (:status result))))

(defn may-dig-fn
  "(may-dig? cell): no other owner's zone, claim or plan footprint refuses a dig there (jobs.lib.tidy/refusal)."
  [c]
  (fn [cell] (or (boolean (:ignore-zones? (:args c))) (nil? (tidy/refusal c :dig (zipmap [:x :y :z] cell))))))

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

(defn hole-tag
  "What marks a hole as this go-to's in the tidy ledger; its stair and clear-path children write it (:note) as they dig."
  [c]
  {:escalation true :go-to (:id c)})

(defn note-holes!
  "Write every cell of dug ({:cell :block}) that is air now and not yet noted to the ledger, as a :dig entry with
  :escalation true and :any-of, the items that put it back (the block or what it drops). Body memory outlives the
  round, a cut, a cancel and a restart: jobs.survival.restore-broken puts back what go-to did not."
  [c dug]
  (doseq [{:keys [cell block]} (distinct dug)]
    (escape/note-hole! c (hole-tag c) cell block)))

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
          :again))))

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
  :again)

(defn partly-made?
  "Whether the escalation changed something before it stopped: it dug a cell or moved the body. Then walking again
  may get through (a stair stopped short of a stone step may already reach the rim)."
  [c]
  (let [m (ctx/mem c)]
    (or (> (count (own-holes c)) (:holes-before m 0))
        (not= (:escalation-from m) (feet-cell c)))))

(defn skip-key
  "What a failed escalation e rules out: a stair only its heading (the other headings may be open), else its step."
  [{:keys [step heading]}]
  (if (= :stair step) [:stair heading] step))

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
        (ctx/update-mem! c #(cond-> (update % :skipped-steps (fnil conj #{}) (skip-key (:escalation (ctx/mem c))))
                              (= :stair step) (update :escalations dec)))
        (await (escalate! c pos detail)))))

(defn ^:async escalation-round!
  "One call of the escalation child: the whole pillar, stair or door. After it, every planned cell now dug is in the
  ledger (note-holes!). Its wait or its failure gives up, with the child's reason, unless it got part of the way."
  [c pos]
  (let [{:keys [step] :as e} (:escalation (ctx/mem c))
        [job args] (escalation-job e (hole-tag c) (boolean (:ignore-zones? (:args c))))]
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

(defn deep-below?
  "Whether cell lies more than 2 below the body's feet: a place walk (range 3) has to go down into the shaft for it."
  [c [_ y _]]
  (> (- (second (feet-cell c)) y) 2))

(defn skip-why
  "Why hole e must not be filled now, or nil: :changed (no longer air: forgotten), :occupied (the body is in it),
  :needed (filling it would shut in a body that has room now: the stair it stands on, its way out), :away (out of
  reach and deep below the feet: walking down to it would put the body inside its own refilled stair)."
  [c {:keys [cell]}]
  (let [p (:primitives c)]
    (cond
      (not (b/air (u/block-name p (zipmap [:x :y :z] cell)))) :changed
      (tidy/in-body? p cell) :occupied
      (and (not (reach/enclosed? p)) (reach/enclosed? p #{(vec cell)})) :needed
      (and (not (b/in-reach? c cell)) (deep-below? c cell)) :away)))

(defn ^:async restore-round!
  "Put back the next hole this job's escalations dug (own-holes), lowest first, with jobs.blocks.place (the dug block or
  what it drops), one child call each. It runs once the body got on past the escalation (restore-next!). A hole
  skip-why refuses, or the place child would wait on (nothing to place), stays in the ledger for restore-broken (a
  :changed one is forgotten). A placed one is forgotten."
  [c]
  (let [p (:primitives c)
        seen (:restore-seen (ctx/mem c) #{})
        e (first (remove #(seen (:cell %)) (own-holes c)))]
    (if (nil? e)
      (do (restore-done! c) :again)
      (let [{:keys [cell was any-of]} e
            args {:pos cell :any-of any-of}
            done! (fn [k entry]
                    (ctx/update-mem! c #(-> % (update :restore-seen (fnil conj #{}) cell) (update k (fnil conj []) entry)))
                    :again)
            skip! (fn [why]
                    (if (= :changed why)
                      (do (tidy/forget-cell! c cell) (done! :changed cell))
                      (done! :skipped {:cell cell :block was :why why})))]
        (if-let [why (skip-why c e)]
          (skip! why)
          (if-let [wait (b/child-wait c :restore 'jobs.blocks.place args)]
            (skip! (:reason wait))
            (let [r (await (ctx/call-child c :restore 'jobs.blocks.place args))
                  res (ctx/child-result c :restore)]
              (cond
                (= :continue r) :continue
                (= :declined r) (skip! (:reason (b/child-wait c :restore 'jobs.blocks.place args) :declined))
                (:placed res) (do (tidy/forget-cell! c cell) (note-placed! c cell) (done! :restored cell))
                :else (skip! (:reason res))))))))))

(defn restore-next!
  "The body got on past an escalation (a walk round that arrived or made progress): put back its holes next round
  (restore-round!). Not before: a stair or door short of the way on is still the body's way, and refilling it under
  the body would strand it."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :restore-pending) (assoc :restore-now true)))
  :again)

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

(defn here?
  "Whether the body is within range of pos, by its position or by the cell it stands on."
  [c pos range]
  (or (u/within? (u/self-pos c) pos range) (u/within? (reach/standing-cell (:primitives c)) pos range)))

(defn fault-cells
  "The cells a walk that failed at the walker (:stuck at :target, :off-plan at :at) says it cannot pass, [x y z] each.
  A partial plan walked to its end (:partial) is a normal end."
  [{:keys [status target at partial]}]
  (case status
    :stuck (some-> target vector)
    :off-plan (when-not partial (some-> at vector))
    nil))

(defn note-fault!
  "A walk that failed at the walker is a bug to see (a go-to.walker-fault warn) and a cell to keep the next plans off
  (mem :fault-cells, at most max-fault-cells; a new call starts with none): the walks go round it when there is a way."
  [c {:keys [status] :as result}]
  (when-let [cells (seq (fault-cells result))]
    (ctx/emit! c :walker-fault :warn (cond-> {:why status :target (first cells)}
                                       (:move result) (assoc :kind (:move result))
                                       (:why result) (assoc :detail (:why result))))
    (ctx/update-mem! c update :fault-cells #(vec (take-last max-fault-cells (distinct (into (vec %) cells)))))))

(defn ^:async walk! [c pos range doors]
  (let [from (u/self-pos c)
        d (u/dist from pos)
        pw (walk/path-world (:primitives c))]
    (cond
      (here? c pos range)
      (if (:restore-pending (ctx/mem c)) (restore-next! c) (arrived! c))

      (nil? pw)
      (refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})

      :else
      (let [_ (forget-known-land! c)
            {walked :result status :status to :to} (await (near/walk-round! c pos range {:doors doors :explore true
                                                                                          :dangers (not (false? (:dangers (:args c))))
                                                                                          :dark (not (false? (:dark (:args c))))
                                                                                          :timeout-s (or (:leg-s (:args c)) near/walk-timeout-s)
                                                                                          :shut-also (shut-foreign c)
                                                                                          :budget walk/round-budget
                                                                                          :avoid (set (:fault-cells (ctx/mem c)))
                                                                                          :tolls (:tolls (:args c))
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

          (and (:leg-s (:args c)) (< left (dec d)))
          (finish! c {:arrived false :leg true})

          (= "searching" status)
          (let [n (cond-> (:searching (ctx/mem c) 0) (:fresh walked) inc)]
            (ctx/update-mem! c assoc :searching n)
            (if (< n max-searching)
              :again
              (give-up! c pos (:blocked (ctx/mem c) 0) :searching {:status :no-path :reason :moved-while-searching})))

          :else
          (let [_ (note-fault! c walked)
                progress? (or (< left (dec best)) explored? nearer?)
                tries (if progress? 0 (inc (:blocked (ctx/mem c) 0)))]
            (ctx/update-mem! c assoc :blocked tries :searching 0 :best (if (< left (dec best)) left best)
                             :frontier-best (cond-> fbests frontier (assoc frontier (min fbest (u/dist to fcell))))
                             :target-best (cond-> tbests target (assoc target (min tdist (get tbests target js/Infinity)))))
            (when (and progress? (:restore-pending (ctx/mem c))) (restore-next! c))
            (if (and (if (false? (:retry (:args c))) progress? (< tries max-blocked)) (not (#{:goal-enclosed :goal-cut-off} (:reason result))))
              :again
              (await (give-up-or-escalate! c pos tries status result)))))))))

(defn start-attempt!
  "Begin a call from the world, with memory as a hint: the counters of an earlier call (cut, or ended :continue) start
  again, and a saved escalation, or a pending one, is dropped when the body is already there (its holes, if any, are
  put back as after a made way)."
  [c pos]
  (let [m (ctx/mem c)
        there? (here? c pos (:range (:args c)))]
    (ctx/update-mem! c #(cond-> (-> % (dissoc :best :fault-cells) (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {}))
                          (and there? (:escalate-now m)) (dissoc :escalate-now)
                          (and there? (:escalation m))
                          (-> (dissoc :escalation :escalation-from :planned :holes-before)
                              (assoc :restore-pending true))))))

(defn ^:async step!
  "One iteration of a call: :again to go on in the same call, else what the call returns."
  [c pos]
  (let [m (ctx/mem c)]
    (cond
      (:restore-now m) (await (restore-round! c))
      (:escalation m) (await (escalation-round! c pos))
      (:escalate-now m) (do (ctx/update-mem! c dissoc :escalate-now)
                            (await (escalate! c pos nil)))
      :else (await (walk! c pos (:range (:args c)) (some-> (:doors (:args c)) keyword))))))

(defn target
  "{:pos {:x :y :z}} to walk to: the recorded position of :place when given, else :pos; else a refusal."
  [c]
  (let [{:keys [place pos]} (:args c)]
    (if (nil? place)
      (places/parse-pos pos)
      (let [named (places/parse-name place)
            there (when-not (:reason named) (mem/place (ctx/view c) (:name named)))]
        (cond
          (:reason named) named
          (nil? there) (places/refusal :unknown-place (str "no place called " (name (:name named)) " is recorded in memory"))
          :else (places/parse-pos there))))))

(defn ^:async round
  "One whole attempt: step! until it arrives, gives up, or waits on a child (:continue), with pace! between steps. A
  cut ends it at once (ctx/alive? per iteration, else at the next memory write or act, which throws)."
  [c]
  (let [parsed (target c)
        pos (:pos parsed)
        tolls-problem (walk/tolls-problem (:tolls (:args c)))]
    (cond
      (:reason parsed)
      (refuse! c parsed)

      tolls-problem
      (refuse! c {:reason :bad-tolls :message tolls-problem})

      :else
      (do (start-attempt! c pos)
          (loop []
            (let [r (await (step! c pos))]
              (cond
                (not= :again r) r
                (ctx/alive? c) (do (await (pace!)) (recur))
                :else :continue)))))))
