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
            [jobs.lib.danger :as danger-q]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.keep :as stay]
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
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.world :as known]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]))

(def doc
  "Take back this body's temporary blocks: the open entries of the scaffold ledger (jobs.lib.ledger, body
  memory).

  Which entries (:of):
  - :orphans (default): entries whose placing job is no longer live and that no cleanup held in the last 10 minutes.
  - an instance id: that instance's and its children's. A parent cleaning up after its own pillar passes its
    own id.
  - :all: every entry.
  :purpose (a keyword or a set) narrows any of these to the entries of that purpose (:pillar :doorway :bed :pour ...).
  :near {:radius n :pos p} narrows any of these to the entries within n of p (the body's position now when :pos is left out). With
  :of :orphans and no :near it is {:radius 32}: far scaffold waits for the body to come near, or for :of :all, an instance id or
  a larger :near.

  One run: every pass first settles the entries it works on from their cells. The recorded item still there: ours. A
  cell marked :removing that is now air: removed. Anything else (swapped by someone, or gone): the entry is
  dropped with an info cleanup.dropped {:cell :item :found} and the cell is never dug. An unloaded cell keeps
  its entry.

  Then one step, passes repeated (paced) until nothing is left to do:
  - Dig the highest (then nearest) cell within ::reach of the eye that may be dug. The entry is marked
    :removing before the dig, so a cut or restart is decided from the cell.
  - Else walk to within 3 of the nearest (jobs.movement.go-to as a child, :ways #{:walk}: never digging a way), in legs of
    ::leg-s. Before each leg a known hostile within ::danger-radius of the cell, or a flight from one near it or the body,
    holds that cell :danger instead (the hostile reflex acts; the cell is offered again after the hold).
  - Else finish.

  A poured water source (item water, purpose :pour) is scooped, not dug: with an empty bucket carried, jobs.items.use clicks
  it from within bucket reach (zones are not asked: the pour was this body's own). Without a bucket it stays open (:no-bucket).

  A body standing on a ledger row (a bridge) never digs the last way back to firm ground: that cell is :stranding.
  With nothing else to dig it walks to the ground at the row's end (go-to, range 0, 3 tries) and digs the rest from
  there. Standing on a row cell it cannot dig (a drop below), it steps one cell back toward the ground and digs that
  cell: a row over a drop is taken back end first. Ground with no head room beside the row is no way off it.

  The body's own column is dug only from on top: the block under the feet when the cell below is solid floor
  (the body falls one block and lands). Deeper cells wait for the descent (:under-body). Over a hole it stops
  (:no-floor-below).

  A block on the footprint of a plan this body made is the plan's: its entry is dropped (cleanup.plan-cell) and the cell
  never dug, unless the spare-own-builds setting is false.

  Ready: safe? is jobs.lib.ready/safe-if-lit (:hostile-near, :dark); fit? is jobs.lib.ready/fit-armed (:low-health, :poisoned,
  :unarmed: no weapon carried). Both are the :checks defaults, so a caller or trigger overrides them (:checks {:fit? '(always)}).
  Which scaffold is worth a walk (far below, near a death, roofed) is the scaffold-left trigger's call, not this job's. A walk leg also stops while a mob the body fled from within
  jobs.lib.threats/fled-memory-ms was last known within jobs.lib.threats/fled-radius of the cell or the body (:danger).

  A shelter's block (purpose :shelter or :retreat-plug) stays while a known hostile is near (:hostile-near) and while it opens onto a dark cell (:night; setting jobs.lib.shelter.keep ::at-night?): a neighbour that is not solid and not lit by what the body knows (an unseen one is guessed by the sky, unless the body's own cell is lit), whatever the hour: it keeps the hostile out of the body's refuge. A run that removed a block and leaves only :night cells holds none of them (the light of the opened side may not have reached the body yet).

  Every dig asks jobs.lib.access.rules/may-dig? (zones, plan footprints, ledger cells) when chosen and again
  right before the dig. A refusal (:zone :footprint :no-zones :not-loaded) or a hazard :accept does not name
  (:hazard; lava beside is :lava-adjacent) keeps the entry open, and so does :no-tool (the block needs a tool no
  carried one is: dug by hand it drops nothing). A cell the planner proves no walk reaches to within ::walk-range of (cut off, closed in: up a tree, behind a wall), asked
  before the walk, is held :no-way with no walk made; a go-to that proves no way (see permanent-walk?) holds it
  :unreachable at once. After ::give-up walks ending out of reach the cell is held :out-of-reach, after
  ::give-up failed digs :dig-failed.

  Ends by collecting the removed items' drops (jobs.items.collect-drops, filtered to the items they drop;
  a wall torch gives the torch). Data {:removed [{:cell :item}] :dropped [{:cell :item :found}] :open [{:cell
  :item :reason}] :collected n} (the ending carries it, a reflex run's too; a :left-open ending's :text lists the open cells). Open cells
  are held (:scaffold-held, 10 minutes) so the conditions and the scaffold-left trigger do not offer them again at
  once. Ends :done when nothing is left open, else nothing-possible :left-open (what could be taken back, was).

  Impossible: :bad-args (:of is not :orphans, :all or an instance id). Condition: :no-zones (no zone list has been read, unless :ignore-zones?). Done at once when nothing is offered (after a look around when a cell was
  never seen). Quote: from the entries it would work on.")

(a/defargs args
  {:of {:doc ":orphans: entries no live job owns; \"jN\": that instance's and its children's; :all: every entry" :spec (a/or-of #{:orphans :all} a/name?) :default :orphans}
   :near {:doc "only the entries within :radius of :pos (default the body's position now); {:radius 32} for :orphans when absent" :spec (a/map-with {:pos a/point? :radius (a/num-in 0 nil)}) :default nil}
   :purpose {:doc "only the entries of these purposes (a keyword or a set), e.g. :doorway; nil: any" :spec (a/or-of keyword? (a/coll-of keyword?)) :default nil}
   :accept {:doc "dig hazards accepted: :fluid-adjacent (water beside), :lava-adjacent, :falling-block"
            :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block :under-feet}) :default #{:fluid-adjacent}}
   :ignore-zones? :shared})


(a/defargs settings
  {::reach {:default u/bucket-reach :doc "Cells whose centre is this close to the eye are dug from where the body stands, in blocks." :spec (a/num-in 0 nil)}
   ::leg-s {:default 6 :doc "A walk goes in legs of this many seconds; the danger is weighed again after each." :spec (a/num-in 0.1 120)}
   ::give-up {:default 2 :doc "Walks ending out of reach, or failed digs, after which a cell is held." :spec (a/int-in 1 nil)}
   ::walk-range {:default 3 :doc "How near a scaffold block the body walks before it digs it." :spec (a/int-in 0 nil)}
   ::max-collect-radius {:default 48 :doc "Blocks around the body in which dropped scaffold is collected, at most." :spec (a/int-in 0 nil)}
   ::danger-radius {:default 16 :doc "Blocks around a scaffold block within which a known hostile holds it (:danger) instead of a walk." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds a cleanup may take: the digs, the walks, the collecting." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(defn walk-range [] (settings/get settings ::walk-range))

(defn max-collect-radius [] (settings/get settings ::max-collect-radius))

(defn dig-reach [] (settings/get settings ::reach))

(defn leg-s [] (settings/get settings ::leg-s))

(defn give-up [] (settings/get settings ::give-up))

(defn danger-radius [] (settings/get settings ::danger-radius))

(defn eye-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [x (+ y game/eye-height) z]))

(defn hostile-near?
  "Whether a known hostile (heard or seen) is within the longest follow range (ranged ones within the danger ranged radius)."
  [p]
  (boolean (seq (danger-q/known-hostiles p threats/max-follow-range {:ranged-radius (danger-q/ranged-radius)}))))

(defn danger-ahead?
  "Whether walking to cell is unsafe: a known hostile (seen or heard) near the cell, or a mob the body fled from lately
  last known near the cell or the body (a danger near the body is safe?'s)."
  [c cell]
  (let [p (:primitives c)
        r (danger-radius)
        [x y z] cell]
    (boolean (or (threats/fled-near? p (ctx/view c) {:x x :y y :z z})
                 (threats/fled-near? p (ctx/view c) (u/self-pos c))
                 (some #(<= (u/dist (danger-q/mob-pos p %) {:x x :y y :z z}) r)
                       (danger-q/known-hostiles p (+ r (danger-q/known-scan)) {}))))))

(defn inputs
  "next-step's input now: entries to work on, the whole ledger l for the rules."
  [c l entries zones]
  (let [{:keys [accept]} (:args c)]
    (merge (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c))})
           {:feet (reach/standing-vec (:primitives c)) :eye (eye-of c) :block-at (access/sensed-at (:primitives c) blocks/hidden-guess) :floor-at (access/seen-at (:primitives c)) :entries entries
            :can-clear? #(not (tools/needs-tool-to-clear? (:primitives c) %))
            :bucket? (boolean (some #(= "bucket" (:name %)) (u/inventory (:primitives c))))
            :hostile-near? (hostile-near? (:primitives c)) :opens-dark? (stay/opens-dark? (:primitives c)) :keep-at-night? (stay/at-night?)
            :ledger (ledger/cells l) :zones zones :accept (set accept) :reach (dig-reach)
            :spare-own-builds (access/spare-own-builds)
            :held (:held (ctx/mem c) {}) :retreats (:retreats (ctx/mem c) 0)})))

(defn bad-of? [of] (not (or (= :orphans of) (= :all of) (string? of))))

;; ------------------------------------------------------------------ check

(defn purposes-of [c]
  (let [p (:purpose (:args c))]
    (cond (nil? p) nil (keyword? p) #{p} :else (set p))))

(defn unseen-entries?
  "Whether an entry this cleanup would work on lies in a cell the body has never seen (it reads as not loaded)."
  [c]
  (let [v (ctx/view c)
        cells (->> (ledger/select (ledger/open-entries v) v (:of (:args c)) (ledger/near-of-args c (:args c)) (purposes-of c))
                   (map (fn [{[x y z] :cell}] {:x x :y y :z z})))]
    (look/unseen? (:primitives c) cells)))

(def bad-args
  "Never :bad-args unless :of is :orphans, :all or an instance id."
  (checks/args-fit "bad :of" #(not (bad-of? (:of (:args %))))))

(defn offered-now [c]
  (ledger/offered (ctx/view c) (access/sensed-at (:primitives c) nil) (:of (:args c)) (ledger/near-of-args c (:args c)) (purposes-of c)))

(defn nothing-to-do?
  "Nothing is offered, no pick-up is due, and a cell never seen has been looked at (or none is unseen): the goal holds."
  [c]
  (not (or (:collect (ctx/mem c)) (seq (offered-now c)) (and (not (look/surveyed? c)) (unseen-entries? c)))))

(def can-start? (cond/and bad-args checks/zones-read))

(def meta {:category :phase :part-of '[jobs.movement.go-to jobs.access.stair]})

(def safe? ready/safe-if-lit)

(def fit? ready/fit-armed)

(def target [:of :near])

(def defaults {:checks {:safe? '(and (not (threatened)) (not (exposed))) :fit? '(and (healthy) (armed))} :spend {:job-time ::job-time-s}})

(defn picked-entries [c]
  (let [view (ctx/view c)]
    (ledger/select (ledger/open-entries view) view (:of (:args c)) (ledger/near-of-args c (:args c)) (purposes-of c))))

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
  "m with one more failure on cell; held with why at the limit-th."
  [m cell why limit]
  (let [n (inc (get-in m [:fails cell] 0))]
    (if (>= n limit)
      (-> m (update :fails dissoc cell) (assoc-in [:held cell] why))
      (assoc-in m [:fails cell] n))))

(defn ^:async gone-water!
  "Wait a few quarter seconds for the block update that shows the scooped cell no longer water: the bucket is full by then, so a
  stale water read would hold the entry :no-bucket."
  [c cell]
  (let [[x y z] cell]
    (loop [n 0]
      (when (and (< n 8) (= "water" (u/block-name (:primitives c) {:x x :y y :z z})))
        (await (ctx/act c :wait #js {:ms 250}))
        (recur (inc n))))))

(defn ^:async scoop!
  "Scoop the poured water at cell with the empty bucket (jobs.items.use; the pour was this body's own, so no zone is asked);
  the entry is marked :removing first and the next round settles the cell."
  [c l cell]
  (ledger/remember! c (ledger/begin-removal l cell))
  (let [r (await (child/run!* c :scoop 'jobs.items.use {:on cell :item "bucket" :ignore-zones? true}))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) (do (await (gone-water! c cell)) :again)
      :else (do (ctx/update-mem! c count-fail cell {:reason :scoop-failed :scoop (or (get-in r [:data :reason]) :failed)} (give-up))
                :again))))

(defn ^:async dig!
  "Ask the rules once more, equip, mark the entry :removing and dig (a poured water source is scooped); the next round settles the cell."
  [c {:keys [cell item]}]
  (let [p (:primitives c)
        l (ledger/open-entries (ctx/view c))
        e (ledger/entry-at l cell)
        under? (= cell (update (reach/standing-vec (:primitives c)) 1 dec))]
    (if-let [why (if e (blocker (inputs c l [e] (known/zones c)) e) {:reason :gone})]
      (do (ctx/update-mem! c count-fail cell why (give-up))
          :again)
      (if (= "water" item)
        (await (scoop! c l cell))
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
              :else (ctx/update-mem! c count-fail cell {:reason :dig-failed :dig outcome} (give-up)))
            (if (= :continue outcome) :continue :again)))))))


(defn ^:async no-way?
  "Whether the planner proves no walk from the body reaches within ::walk-range of cell (the verdicts go-to holds a cell unreachable for)."
  [c cell]
  (if-let [pw (wworld/path-world (:primitives c))]
    (let [r (await (wplan/plan-from! c pw cell (walk-range) 1 nil))]
      (and (not= "found" (.-status r)) (contains? result/permanent-why (keyword (.-reason r)))))
    false))

(defn ^:async walk!
  "Walk one leg toward within 3 of cell, or hold it :danger (no walk is made: the reflex acts). A permanent verdict
  holds it :unreachable, an end out of reach or a go-to that gave up for a passing reason counts a failure; a leg that
  got nearer does not. A walk that did not end counts a failure, as the round goes on."
  [c {:keys [cell]}]
  (cond
    (danger-ahead? c cell)
    (do (ctx/update-mem! c assoc-in [:held cell] {:reason :danger}) :again)
    (await (no-way? c cell))
    (do (ctx/update-mem! c assoc-in [:held cell] {:reason :no-way}) :again)
    :else
    (let [e (await (child/call-chase* c :walk {:pos cell :range (walk-range) :leg-s (leg-s)
                                               :walk {:ways #{:walk} :ignore-zones? (boolean (:ignore-zones? (:args c)))}}))
          status (if (= :done (:kind e)) :done :stopped)]
      (cond
        (:nearer (:data e)) nil
        (result/permanent-walk? e)
        (ctx/update-mem! c assoc-in [:held cell] {:reason :unreachable :walk status})
        (or (= :continue e) (= :stopped status) (> (distance (eye-of c) (centre cell)) (dig-reach)))
        (ctx/update-mem! c count-fail cell {:reason :out-of-reach :walk status} (give-up)))
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
  (let [r (await (ctx/call c :collect 'jobs.items.collect-drops
                           {:radius (collect-radius c removed) :items (vec (distinct (map (comp placement/item-of :item) (remove #(= "water" (:item %)) removed))))
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

(defn cells-to-hold
  "The open cells to hold: none when the run removed a block and only :night cells are left (the light of the opened side may lag)."
  [result open]
  (if (and (seq (:removed result)) (every? #(= :night (:reason %)) open)) [] open))

(defn finish! [c open]
  (let [result (result-of c open)]
    (hold-open! c (cells-to-hold result open))
    (if (seq open)
      (ctx/end c :nothing-possible :left-open
               (assoc result :text (str "cleanup left " (count open) " blocks: " (apply str (interpose ", " (map cell-text open))))))
      (ctx/end c :done nil result))))

(defn settle!
  "Settle the entries this cleanup works on from their cells; store the ledger when it changed, book removed and
  dropped entries (one cleanup.dropped info each). The ledger after."
  [c l picked]
  (let [{:keys [footprints own-plans]} (access/zone-input c)
        planned (plan-cells (filterv #(picked (:cell %)) l) footprints own-plans (access/spare-own-builds))
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
        picked (set (map :cell (ledger/select l view (:of (:args c)) (ledger/near-of-args c (:args c)) (purposes-of c))))
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
  (let [zones (known/zones c)]
    ;; a block behind the body is not seen until it looks; a reflex may not yield, so it goes on in the same round
    (if (and (not (:started (ctx/mem c))) (empty? (offered-now c))
             (not (and (await (look/survey! c)) (seq (offered-now c)))))
      (ctx/end c :done nil nil)
      (do (ctx/update-mem! c assoc :started true)
          (await (pace/steps! c #(work! c zones)))))))
