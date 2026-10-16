(ns jobs.access.cleanup
  (:require [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [engine.game :as game]
            [engine.memory :as mem]
            [jobs.access.cleanup.step :refer [centre distance plan-cells blocker next-step]]
            [jobs.access.stair :as stair]
            [jobs.lib.reach :as reach]
            [jobs.lib.body :as body]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.shelter :as sh]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.look :as look]
            [jobs.lib.threats :as threats]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.placement :as placement]
            [jobs.lib.result :as result]
            [jobs.lib.walk :as walk]
            [jobs.lib.world :as known]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]))

(def doc
  "Take back this body's temporary blocks: the open entries of the scaffold ledger (jobs.lib.ledger, body
  memory).

  Which entries (:job):
  - nil: entries whose placing job is no longer live and that no cleanup held in the last 10 minutes.
  - an instance id: that instance's and its children's. A parent cleaning up after its own pillar passes its
    own id.
  - :all: every entry.
  :near {:pos :radius} narrows any of these to the entries within :radius of :pos; :within n, to those within n blocks of the
  body now. With :job nil and neither given, :within is 32: far scaffold waits for the body to come near, or for :job :all, an
  instance id, :near or a larger :within.

  One run: every pass first settles the entries it works on from their cells. The recorded item still there: ours. A
  cell marked :removing that is now air: removed. Anything else (swapped by someone, or gone): the entry is
  dropped with an info cleanup.dropped {:cell :item :found} and the cell is never dug. An unloaded cell keeps
  its entry.

  Then one step, passes repeated (paced) until nothing is left to do:
  - Dig the highest (then nearest) cell within :reach of the eye that may be dug. The entry is marked
    :removing before the dig, so a cut or restart is decided from the cell.
  - Else walk to within 3 of the nearest (jobs.movement.go-to as a child, :ways #{:walk}: never digging a way), in legs of
    :leg-s. Before each leg a real danger within :danger-radius of the body, or a known hostile within it of the cell,
    holds that cell :danger instead (the hostile reflex acts; the cell is offered again after the hold).
  - Else finish.

  A body standing on a ledger row (a bridge) never digs the last way back to firm ground: that cell is :stranding.
  With nothing else to dig it walks to the ground at the row's end (go-to, range 0, 3 tries) and digs the rest from
  there.

  The body's own column is dug only from on top: the block under the feet when the cell below is solid floor
  (the body falls one block and lands). Deeper cells wait for the descent (:under-body). Over a hole it stops
  (:no-floor-below).

  A block on the footprint of a plan this body made is the plan's: its entry is dropped (cleanup.plan-cell) and the cell
  never dug, unless :spare-own-builds is false.

  An unowned entry (:job nil, no :near) out of reach is walked to only when safe, else held with the reason: :unarmed / :low-health (no weapon carried,
  or health under jobs.lib.body/risky-min-health: ::hold-unarmed?), :far-below (more than jobs.lib.ledger/max-below under the feet), :too-high (more than ::walk-range over the feet: up a tree, no way a walk finds),
  :died-near (within ::died-radius of a :died entry in body memory), :roofed (a solid non-scaffold block within
  ::roof-height above it while the body is in the open). A walk leg also stops while a mob the body fled from within
  jobs.lib.threats/fled-memory-ms was last known within jobs.lib.threats/fled-radius of the cell or the body (:danger).

  A shelter's block (purpose :shelter or :retreat-plug) stays while a known hostile is near (:hostile-near) and all night (:night; arg :keep-at-night?): it keeps the hostile out of the body's refuge.

  Every dig asks jobs.lib.access.rules/may-dig? (zones, plan footprints, ledger cells) when chosen and again
  right before the dig. A refusal (:zone :footprint :no-zones :not-loaded) or a hazard :accept does not name
  (:hazard; lava beside is :lava-adjacent) keeps the entry open, and so does :no-tool (the block needs a tool no
  carried one is: dug by hand it drops nothing). A go-to that proves no way (see permanent-walk?) holds the cell
  :unreachable at once. After :give-up walks ending out of reach the cell is held :out-of-reach, after
  :give-up failed digs :dig-failed.

  Ends by collecting the removed items' drops (jobs.forestry.collect-drops, filtered to the items they drop;
  a wall torch gives the torch). Data {:removed [{:cell :item}] :dropped [{:cell :item :found}] :open [{:cell
  :item :reason}] :collected n}, info cleanup.done, and warn cleanup.left while :open is not empty. Open cells
  are held (:scaffold-held, 10 minutes) so the conditions and the scaffold-left trigger do not offer them again at
  once. Ends :done when nothing is left open, else nothing-possible :left-open (what could be taken back, was);
  :too-many-passes (nothing-possible) after ::max-passes steps.

  Impossible: :bad-args (:job is not nil, :all or an instance id). Condition: :no-zones (no zone list has been read, with one warn
  cleanup.declined, unless :ignore-zones?). Done at once when nothing is offered (after a look around when a cell was
  never seen). Quote: from the entries it would work on.")

(a/defargs args
  {:job {:doc "nil: entries no live job owns; \"jN\": that instance's and its children's; :all: every entry" :spec (a/or-of #{:all} a/name?) :default nil}
   :near {:doc "only the entries within :radius of :pos" :spec (a/map-with {:pos a/point? :radius (a/num-in 0 nil)}) :default nil}
   :within {:doc "only the entries within this many blocks of the body, read afresh each time (32 when :job is nil); :near wins" :spec (a/num-in 0 nil) :default nil}
   :accept {:doc "dig hazards accepted: :fluid-adjacent (water beside), :lava-adjacent, :falling-block"
            :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block :under-feet}) :default #{:fluid-adjacent}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :reach {:doc "cells whose centre is this close to the eye are dug from where the body stands, in blocks" :spec (a/num-in 0 nil) :default u/bucket-reach}
   :keep-at-night? {:doc "a shelter's block (:shelter, :retreat-plug) stays all night; false takes it back at night too" :spec boolean? :default true}
   :spare-own-builds {:doc "a block on the footprint of a plan this body made is the plan's, not scaffold: its entry is dropped and never dug" :spec boolean? :default true}
   :danger-radius {:doc "a walk is not made while a real danger is this close to the body, or a known hostile this close to the cell walked to: the cell is held :danger" :spec (a/num-in 0 nil) :default 16}
   :leg-s {:doc "a walk goes in legs of this many seconds; the danger is weighed again after each" :spec (a/num-in 0.1 120) :default 6}
   :give-up {:doc "walks ending out of reach, or failed digs, after which a cell is held" :spec (a/int-in 1 nil) :default 2}})


(a/defargs settings
  {::walk-range {:default 3 :doc "How near a scaffold block the body walks before it digs it." :spec (a/int-in 0 nil)}
   ::max-collect-radius {:default 48 :doc "Blocks around the body in which dropped scaffold is collected, at most." :spec (a/int-in 0 nil)}
   ::roof-height {:default 4 :doc "Blocks above a scaffold block within which a solid block (not scaffold) roofs it; a roofed one is not walked to from the open (:roofed)." :spec (a/int-in 0 nil)}
   ::died-radius {:default 24 :doc "Blocks around a place the body died within which a scaffold is not walked to (:died-near)." :spec (a/num-in 0 nil)}
   ::hold-unarmed? {:default true :doc "Whether an unarmed body, or one below jobs.lib.body/risky-min-health, walks to no scaffold (:unarmed, :low-health)." :spec boolean?}
   ::job-time-s {:default 600 :doc "Seconds a cleanup may take: the digs, the walks, the collecting." :spec (a/num-in 0 nil)}
   ::max-passes {:default 200 :doc "Steps (dig, walk, collect) in one run before it stops." :spec (a/int-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(defn roof-height [] (settings/get settings ::roof-height))

(defn died-radius [] (settings/get settings ::died-radius))

(defn walk-range [] (settings/get settings ::walk-range))

(defn max-collect-radius [] (settings/get settings ::max-collect-radius))

(defn max-passes [] (settings/get settings ::max-passes))

(defn eye-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [x (+ y game/eye-height) z]))

(defn hostile-near?
  "Whether a known hostile (heard or seen) is within the longest follow range (ranged ones within the danger ranged radius)."
  [p]
  (boolean (seq (danger-q/known-hostiles p threats/max-follow-range {:ranged-radius (danger-q/ranged-radius)}))))

(defn danger-ahead?
  "Whether walking to cell is unsafe: a real danger near the body, a known hostile (seen or heard) near the cell, or a mob
  the body fled from lately last known near the cell or the body."
  [c cell]
  (let [p (:primitives c)
        r (:danger-radius (:args c))
        [x y z] cell]
    (boolean (or (danger-q/nearest-danger p r {} {})
                 (threats/fled-near? p (ctx/view c) {:x x :y y :z z})
                 (threats/fled-near? p (ctx/view c) (u/self-pos c))
                 (some #(<= (u/dist (danger-q/mob-pos p %) {:x x :y y :z z}) r)
                       (danger-q/known-hostiles p (+ r (danger-q/known-scan)) {}))))))

(defn died-near-of
  "A fn of a cell: whether a :died entry of view lies within died-radius of it."
  [view]
  (let [spots (keep (comp :pos :data) (mem/entries view :died))]
    (fn [[x y z]] (boolean (some #(<= (u/dist % {:x x :y y :z z}) (died-radius)) spots)))))

(defn walk-hold-of
  "The reason {:reason :unarmed|:low-health} the body of primitives p walks to no scaffold, or nil."
  [p]
  (when (settings/get settings ::hold-unarmed?)
    (if (nil? (combat/best-weapon p combat/default-weapons))
      {:reason :unarmed}
      (some-> (body/too-hurt p) (select-keys [:reason :health :min-health])))))

(defn inputs
  "next-step's input now: entries to work on, the whole ledger l for the rules."
  [c l entries zones]
  (let [{:keys [accept reach]} (:args c)]
    (merge (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c))})
           {:feet (reach/standing-vec (:primitives c)) :eye (eye-of c) :block-at (access/sensed-at (:primitives c) blocks/hidden-guess) :entries entries
            :can-clear? #(not (tools/needs-tool-to-clear? (:primitives c) %))
            :hostile-near? (hostile-near? (:primitives c)) :night? (sh/night? (:primitives c)) :keep-at-night? (:keep-at-night? (:args c))
            :ledger (ledger/cells l) :zones zones :accept (set accept) :reach reach
            :spare-own-builds (:spare-own-builds (:args c))
            :guard-walks? (and (nil? (:job (:args c))) (nil? (:near (:args c))))
            :max-below (ledger/max-below) :walk-range (walk-range) :roof-height (roof-height)
            :died-near? (died-near-of (ctx/view c)) :walk-hold (walk-hold-of (:primitives c))
            :held (:held (ctx/mem c) {}) :retreats (:retreats (ctx/mem c) 0)})))

(defn bad-job? [job] (not (or (nil? job) (= :all job) (string? job))))

;; ------------------------------------------------------------------ check

(defn unseen-entries?
  "Whether an entry this cleanup would work on lies in a cell the body has never seen (it reads as not loaded)."
  [c]
  (let [v (ctx/view c)
        cells (->> (ledger/select (ledger/open-entries v) v (:job (:args c)) (ledger/near-of-args c (:args c)))
                   (map (fn [{[x y z] :cell}] {:x x :y y :z z})))]
    (look/unseen? (:primitives c) cells)))

(def bad-args
  "Never :bad-args unless :job is nil, :all or an instance id."
  (checks/args-fit "bad :job" #(not (bad-job? (:job (:args %))))))

(defn offered-now [c]
  (ledger/offered (ctx/view c) (access/sensed-at (:primitives c) nil) (:job (:args c)) (ledger/near-of-args c (:args c))))

(defn no-zones
  "Later :no-zones while no zone list has been read, with one kind warn."
  [c]
  (let [s (checks/zones-read c)]
    (when-not (= :ok (first s))
      (ctx/warn-once! c :no-zones :cleanup.declined
                      {:reason "no zone list has been read" :text "cleanup declines: no zone list has been read"}))
    s))

(defn nothing-to-do?
  "Nothing is offered, no pick-up is due, and a cell never seen has been looked at (or none is unseen): the goal holds."
  [c]
  (not (or (:collect (ctx/mem c)) (seq (offered-now c)) (and (not (look/surveyed? c)) (unseen-entries? c)))))

(def can-start? (cond/and bad-args no-zones))

(def safe? ready/safe-if-lit)

(def fit? ready/ok)

(def target [:job :near])

(def defaults {:spend {:job-time ::job-time-s}})

(defn picked-entries [c]
  (let [view (ctx/view c)]
    (ledger/select (ledger/open-entries view) view (:job (:args c)) (ledger/near-of-args c (:args c)))))

(defn result-of
  "The result of the cleanup now, with the cells left open."
  [c open]
  (let [m (ctx/mem c)]
    {:removed (:removed m []) :dropped (:dropped m []) :open open :collected (:collected m 0)}))

(defn done?
  "Ok whenever nothing is offered to clean up and no pick-up is due; else the cells removed so far."
  [c]
  (let [done (count (:removed (ctx/mem c)))]
    (if (nothing-to-do? c)
      (contract/ok :nothing-to-do "nothing is offered to clean up" :units :cells :done done :left 0 :result (result-of c []))
      (contract/later :cleaning "cells are offered" :units :cells :done done :left (count (picked-entries c))))))

(defn quote-of [c]
  (let [n (max 1 (count (picked-entries c)))]
    {:uses {:job-time [(* 0.5 n) (* 4 n) (max (* 4 n) (time-s))]}}))

;; ------------------------------------------------------------------ steps

(defn count-fail
  "m with one more failure on cell; held with why at the give-up-th."
  [m cell why give-up]
  (let [n (inc (get-in m [:fails cell] 0))]
    (if (>= n give-up)
      (-> m (update :fails dissoc cell) (assoc-in [:held cell] why))
      (assoc-in m [:fails cell] n))))

(defn ^:async dig!
  "Ask the rules once more, equip, mark the entry :removing and dig; the next round settles the cell."
  [c {:keys [cell item]}]
  (let [p (:primitives c)
        l (ledger/open-entries (ctx/view c))
        e (ledger/entry-at l cell)
        under? (= cell (update (reach/standing-vec (:primitives c)) 1 dec))]
    (if-let [why (if e (blocker (inputs c l [e] (known/zones c)) e) {:reason :gone})]
      (do (ctx/update-mem! c count-fail cell why (:give-up (:args c)))
          :again)
      (do
        (await (tools/equip-tool! c item {:fast true}))
        (ledger/remember! c (ledger/begin-removal l cell))
        (let [[x y z] cell
              outcome (await (blocks/dig-cell! c {:x x :y y :z z} {:accept (into #{:fluid-adjacent :falling-block :under-feet}
                                                                         (filter #{:lava-adjacent}) (:accept (:args c)))
                                                                   :ignore-zones? true}))]
          (when (and under? (not= :continue outcome)) (await (walk/settle! c)))
          (cond
            (= :continue outcome) :continue
            (#{:dug :missing} outcome) (ctx/update-mem! c assoc :collect true)
            :else (ctx/update-mem! c count-fail cell {:reason :dig-failed :dig outcome} (:give-up (:args c))))
          (if (= :continue outcome) :continue :again))))))


(defn ^:async walk!
  "Walk one leg toward within 3 of cell, or hold it :danger (no walk is made: the reflex acts). A permanent verdict
  holds it :unreachable, an end out of reach or a go-to that gave up for a passing reason counts a failure; a leg that
  got nearer does not. A walk that did not end counts a failure, as the round goes on."
  [c {:keys [cell]}]
  (if (danger-ahead? c cell)
    (do (ctx/update-mem! c assoc-in [:held cell] {:reason :danger}) :again)
    (let [e (await (child/call-walk* c :walk {:pos cell :range (walk-range) :ways #{:walk} :zone-tolls true
                                              :ignore-zones? (boolean (:ignore-zones? (:args c)))
                                              :spend {:job-time (:leg-s (:args c))}}))
          status (if (= :done (:kind e)) :done :stopped)]
      (cond
        (:leg (:data e)) nil
        (result/permanent-walk? e)
        (ctx/update-mem! c assoc-in [:held cell] {:reason :unreachable :walk status})
        (or (= :continue e) (= :stopped status) (> (distance (eye-of c) (centre cell)) (:reach (:args c))))
        (ctx/update-mem! c count-fail cell {:reason :out-of-reach :walk status} (:give-up (:args c))))
      :again)))

(defn ^:async retreat!
  "Walk to the ground at the end of the row the body stands on (go-to, range 0, no digging); counts a try."
  [c {:keys [to]}]
  (let [[x y z] to
        r (await (child/call-walk* c :retreat {:pos {:x x :y y :z z} :range 0 :ways #{:walk}
                                               :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (when-not (= :continue r) (ctx/update-mem! c update :retreats (fnil inc 0)))
    :again))

(defn collect-radius [c removed]
  (let [{:keys [x y z]} (u/self-pos c)]
    (min (max-collect-radius)
         (+ 4 (js/Math.ceil (apply max 0 (map #(distance [x y z] (centre (:cell %))) removed)))))))

(defn ^:async collect!
  "One collect-drops call. :continue means it waits on the world: asked again on the next pass; else the drops are
  not asked for again."
  [c removed]
  (let [r (await (ctx/call c :collect 'jobs.forestry.collect-drops
                           {:radius (collect-radius c removed) :filter (vec (distinct (map (comp placement/item-of :item) removed)))
                            :holes (mapv :cell removed)}))]
    (when-not (= :continue r)
      (ctx/update-mem! c #(-> % (dissoc :collect)
                              (assoc :collected (if (= :done (:kind r)) (:collected (:data r) 0) 0)))))
    :again))

(defn hold-open!
  "Hold the open cells, with earlier held cells that are still in the ledger."
  [c open]
  (let [view (ctx/view c)
        before (ledger/held-cells view)
        in-ledger (ledger/cells (ledger/open-entries view))
        held (into (set (filter in-ledger before)) (map :cell open))]
    (when (not= held before)
      (ledger/remember-held! c held))))

(defn cell-text [{:keys [cell reason]}] (str (pr-str cell) " " (name reason)))

(defn done-event!
  "Tell cleanup.done for a result."
  [c result]
  (ctx/emit! c :cleanup.done :info {:removed (count (:removed result)) :dropped (count (:dropped result))
                                    :open (count (:open result)) :collected (:collected result)
                                    :text (str "cleanup removed " (count (:removed result)) ", dropped "
                                               (count (:dropped result)) " not ours, " (count (:open result)) " left")}))

(defn ^:async cleanup
  "After a cleanup that did its round and ended :done: cleanup.done is told."
  [c ending]
  (when (and (= :done (:kind ending)) (:started (ctx/mem c)))
    (done-event! c (:data ending)))
  (ctx/end c :done nil {}))

(defn finish! [c open]
  (let [result (result-of c open)]
    (hold-open! c open)
    (when (seq open)
      (ctx/emit! c :cleanup.left :warn {:open open
                                        :text (str "cleanup left " (count open) " blocks: "
                                                   (apply str (interpose ", " (map cell-text open))))})
      (done-event! c result))
    (if (seq open)
      (ctx/end c :nothing-possible :left-open result)
      (ctx/end c :done nil result))))

(defn settle!
  "Settle the entries this cleanup works on from their cells; store the ledger when it changed, book removed and
  dropped entries (one cleanup.dropped info each). The ledger after."
  [c l picked]
  (let [{:keys [footprints own-plans]} (access/zone-input c)
        planned (plan-cells (filterv #(picked (:cell %)) l) footprints own-plans (:spare-own-builds (:args c)))
        {:keys [ledger removed dropped]} (ledger/settle (filterv #(not (planned (:cell %))) l) #(picked (:cell %)) (access/sensed-at (:primitives c) nil))]
    (when (not= ledger l) (ledger/remember! c ledger))
    (doseq [cell planned]
      (ctx/emit! c :cleanup.plan-cell :info {:cell cell :plan (get footprints cell)
                                             :text (str "ledger block at " (pr-str cell) " is on the plan " (get footprints cell) ": kept, entry dropped")}))
    (doseq [{:keys [cell item found job]} dropped]
      (ctx/emit! c :cleanup.dropped :info {:cell cell :item item :found found :job job
                                           :text (str "scaffold " item " at " (pr-str cell) " is now " found
                                                      ": not ours any more, entry dropped")}))
    (ctx/update-mem! c #(-> %
                            (update :removed (fnil into []) (map (fn [e] (select-keys e [:cell :item]))) removed)
                            (update :dropped (fnil into []) (map (fn [e] (select-keys e [:cell :item :found]))) dropped)))
    ledger))

(defn ^:async work!
  "One pass: settle, then one step. Resolves :again, :continue (a dig waits) or the ending."
  [c zones]
  (let [view (ctx/view c)
        l (ledger/open-entries view)
        picked (set (map :cell (ledger/select l view (:job (:args c)) (ledger/near-of-args c (:args c)))))
        settled (settle! c l picked)
        step (next-step (inputs c settled (filterv #(picked (:cell %)) settled) zones))
        m (ctx/mem c)]
    (case (:step step)
      :dig (await (dig! c step))
      :walk (await (walk! c step))
      :retreat (await (retreat! c step))
      (if (and (:collect m) (seq (:removed m)))
        (await (collect! c (:removed m)))
        (finish! c (:open step))))))

(defn ^:async round [c]
  (let [zones (known/zones c)
        n (atom 0)]
    ;; a block behind the body is not seen until it looks; a reflex may not yield, so it goes on in the same round
    (if (and (not (:started (ctx/mem c))) (empty? (offered-now c))
             (not (and (await (look/survey! c)) (seq (offered-now c)))))
      (ctx/end c :done nil nil)
      (do (ctx/update-mem! c assoc :started true)
          (await (pace/steps! c #(if (<= (max-passes) (swap! n inc))
                                   (ctx/end c :nothing-possible :too-many-passes {:text "cleanup did not settle"})
                                   (work! c zones))))))))
