(ns jobs.movement.go-to.escalation
  "go-to's way out of a shut-in body: choose pillar, stair, clear-path or a walk to the wall, run the child, note the
  holes it dug, put them back once the walk is on past them."
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.escape :as escape]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.movement.go-to.result :as end]))

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
    :stair ['jobs.access.stair {:dir :up :heading (:heading e) :steps (:steps e) :note tag :fetch false :ignore-zones? ignore-zones?}]
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
  (when-let [block (u/seen-name (:primitives c) (zipmap [:x :y :z] cell))]
    (ctx/remember! c :escalation-placed {:cell (vec cell) :block block} placed-policy)))

(defn planned-cells
  "The solid cells escalation e from feet would cut, [{:cell :block}]: the stair's cuts or clear-path's door, those
  the body has sensed (the child notes what it digs of the rest)."
  [p {:keys [step heading steps]} feet]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        cells (case step
                :stair (escape/stair-cuts feet (escape/heading-dirs heading) steps)
                :clear-path (:cells (escape/door block-at unseen? feet (escape/heading-dirs heading) (escape/max-door)))
                nil)]
    (vec (for [cell cells :let [n (block-at cell)] :when (and (escape/solid? n) (not (unseen? cell)))] {:cell cell :block n}))))

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

(def look-levels
  "Levels from the feet up whose cells beside the body escalation looks at before it chooses (pit-depth, headroom)."
  4)

(defn ^:async look-round!
  "Before choosing: look at each cell beside the body (feet up look-levels) and over its head that it has not sensed,
  so the choice reads walls it sees; a look shows the cells round the one looked at too. Cells inside rock stay
  unknown (escape reads them as rock)."
  [c feet]
  (let [p (:primitives c)
        cells (cons (escape/up feet 2)
                    (for [lvl (range look-levels) d escape/cardinals] (escape/up (escape/ahead feet d 1) lvl)))]
    (loop [todo cells]
      (when-let [cell (first todo)]
        (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
        (recur (rest todo))))))

(defn ^:async escalate!
  "Start the next escalation for the give-up kept in memory (:give-up), or give up with it when there is none (or
  with failed, the detail of the escalation that just failed, when there is none left after it)."
  [c pos failed]
  (let [{:keys [tries status result]} (:give-up (ctx/mem c))
        n (inc (:escalations (ctx/mem c) 0))
        feet (end/feet-cell c)
        _ (when (<= n max-escalations) (await (look-round! c feet)))
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        opts {:own? (own-placed-fn c) :skip (:skipped-steps (ctx/mem c) #{})}
        e (escape/choose (:primitives c) feet goal (may-dig-fn c) opts)
        e (if (and (= :none (:step e)) (not= :none (:step (escape/choose (:primitives c) feet goal (constantly true) opts))))
            {:step :none :why :zone}
            e)]
    (cond
      (> n max-escalations) (end/give-up! c pos tries status result {:escalation {:step :spent :n max-escalations}})
      (= :none (:step e)) (end/give-up! c pos tries status result {:escalation (or failed e)})
      :else
      (do (ctx/emit! c :go-to.escalated :info {:step (:step e) :why (:why (end/give-up-fields result)) :n n :at feet
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
       (not (escape/door-beside? (:primitives c) (end/feet-cell c) (some-> (:doors (:args c)) keyword)))))

(defn ^:async give-up-or-escalate! [c pos tries status result]
  (if (escalate? c result)
    (do (ctx/update-mem! c assoc :give-up {:tries tries :status status
                                           :result (select-keys result [:status :reason :kind :cells :near :one-way])})
        (await (escalate! c pos nil)))
    (end/give-up! c pos tries status result)))

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
        (not= (:escalation-from m) (end/feet-cell c)))))

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

(defn changed?
  "Whether hole cell is seen filled (not air); a cell the body does not know is not."
  [p cell]
  (let [n (u/seen-name p (zipmap [:x :y :z] cell))]
    (boolean (and n (not (b/air n))))))

(defn deep-below?
  "Whether cell lies more than 2 below the body's feet: a place walk (range 3) has to go down into the shaft for it."
  [c [_ y _]]
  (> (- (second (end/feet-cell c)) y) 2))

(defn skip-why
  "Why hole e must not be filled now, or nil: :changed (no longer air: forgotten), :occupied (the body is in it),
  :needed (filling it would shut in a body that has room now: the stair it stands on, its way out), :away (out of
  reach and deep below the feet: walking down to it would put the body inside its own refilled stair)."
  [c {:keys [cell]}]
  (let [p (:primitives c)]
    (cond
      (changed? p cell) :changed
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
            args {:pos cell :any-of any-of :fetch false}
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
