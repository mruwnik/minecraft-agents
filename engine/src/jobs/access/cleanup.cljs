(ns jobs.access.cleanup
  (:require [engine.access.ledger :as ledger]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]
            [engine.placement :as placement]
            [engine.path.walk :as walk]))

(def doc
  "Take back this body's temporary blocks: the open entries of the scaffold ledger (engine.access.ledger, body
  memory).

  Which entries (:job):
  - nil: entries whose placing job is no longer live and that no cleanup held in the last 10 minutes.
  - an instance id: that instance's and its children's. A parent cleaning up after its own pillar passes its
    own id.
  - :all: every entry.

  Every round first settles the entries it works on from their cells. The recorded item still there: ours. A
  cell marked :removing that is now air: removed. Anything else (swapped by someone, or gone): the entry is
  dropped with an info cleanup.dropped {:cell :item :found} and the cell is never dug. An unloaded cell keeps
  its entry.

  Then one step:
  - Dig the highest (then nearest) cell within :reach of the eye that may be dug. The entry is marked
    :removing before the dig, so a cut or restart is decided from the cell.
  - Else walk to within 3 of the nearest (jobs.debug.walk-plan as a child, never digging a way).
  - Else finish.

  The body's own column is dug only from on top: the block under the feet when the cell below is solid floor
  (the body falls one block and lands). Deeper cells wait for the descent (:under-body). Over a hole it stops
  (:no-floor-below).

  Every dig asks engine.access.rules/may-dig? (zones, plan footprints, ledger cells) when chosen and again
  right before the dig. A refusal (:zone :footprint :no-zones :not-loaded) or a hazard :accept does not name
  (:hazard; lava beside is :lava-adjacent) keeps the entry open. A walk with no plan holds the cell
  :unreachable at once. After :give-up walks ending out of reach the cell is held :out-of-reach, after
  :give-up failed digs :dig-failed.

  Ends by collecting the removed items' drops (jobs.forestry.collect-drops, filtered to the items they drop;
  a wall torch gives the torch). Result {:removed [{:cell :item}] :dropped [{:cell :item :found}] :open [{:cell
  :item :reason}] :collected n}, info cleanup.done, and warn cleanup.left while :open is not empty. Open cells
  are held (:scaffold-held, 10 minutes) so the check and the scaffold-left trigger do not offer them again at
  once.

  The check declines while nothing is offered. It also declines while no zone list has been read, with one warn
  cleanup.declined.")

(def args
  {:job {:doc "nil: entries no live job owns; \"jN\": that instance's and its children's; :all: every entry" :default nil}
   :accept {:doc "dig hazards accepted: :fluid-adjacent (water beside), :lava-adjacent, :falling-block"
            :default #{:fluid-adjacent}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :reach {:doc "cells whose centre is this close to the eye are dug from where the body stands, in blocks" :default 4.5}
   :give-up {:doc "walks ending out of reach, or failed digs, after which a cell is held" :default 2}})

(def eye-height 1.62)

(def walk-range 3)

(def max-collect-radius 48)

;; ------------------------------------------------------------------ the step, pure

(defn centre [[x y z]] [(+ x 0.5) (+ y 0.5) (+ z 0.5)])

(defn distance [a b] (apply js/Math.hypot (map - a b)))

(defn hazard-reason
  "The :accept key of a dig hazard: lava beside is :lava-adjacent, kept apart from water."
  [{:keys [reason fluid]}]
  (if (and (= :fluid-adjacent reason) (= "lava" fluid)) :lava-adjacent reason))

(defn open? [block-at pos] (let [n (block-at pos)] (boolean (and n (rules/replaceable n)))))

(defn exit-after-dig?
  "True when a body standing on cell, dropping onto the block below it, has a way on: a side cell at its new feet
  level that is open with open head room (walked off), or solid with two open cells above it (a step up)."
  [block-at cell]
  (boolean
   (some (fn [[dx dz]]
           (let [side (rules/offset cell dx 0 dz)]
             (or (and (open? block-at side) (open? block-at (rules/offset side 0 1 0)))
                 (and (not (open? block-at side)) (block-at side)
                      (open? block-at (rules/offset side 0 1 0)) (open? block-at (rules/offset side 0 2 0))))))
         [[1 0] [-1 0] [0 1] [0 -1]])))

(defn blocker
  "Why entry e cannot be dug now, as {:reason ...detail}, or nil. in: {:feet :block-at :zones :footprints :ledger
  :accept}."
  [{:keys [feet block-at accept] :as in} {:keys [cell]}]
  (let [[fx fy fz] feet
        [x y z] cell
        below (rules/offset cell 0 -1 0)]
    (cond
      (nil? (block-at cell)) {:reason :not-loaded}
      (and (= [x z] [fx fz]) (< y (dec fy))) {:reason :under-body}
      (and (= cell [fx (dec fy) fz]) (not (rules/solid-floor? block-at below)))
      {:reason :no-floor-below :block (block-at below)}
      (and (= cell [fx (dec fy) fz]) (not (exit-after-dig? block-at cell)))
      {:reason :no-exit}
      :else
      (let [v (rules/may-dig? (-> (select-keys in [:block-at :feet :zones :footprints :claims :self :now :ignore-zones? :ledger])
                                  (update :footprints #(into {} (remove (comp (:own-plans in #{}) val)) %))
                                  (assoc :cell cell)))
            hazards (mapv hazard-reason (:hazards v))]
        (cond
          (not (:ok v)) (select-keys v [:reason :zone :claim :plan :block])
          (not-every? accept hazards) {:reason :hazard :hazards hazards})))))

(defn next-step
  "The next step over entries (each holding its item, or unloaded), from {:feet :eye [x y z] :block-at :entries
  :ledger #{cells} :zones :footprints :accept :reach :held {cell {:reason ...}}}: {:step :dig :cell :item},
  {:step :walk :cell} or {:step :finish :open [{:cell :item :reason ...}]}. Held cells are given up for this run."
  [{:keys [entries eye reach held] :as in}]
  (let [judged (map (fn [e] [e (or (held (:cell e)) (blocker in e))]) entries)
        ready (map first (remove second judged))
        d #(distance eye (centre (:cell %)))
        near (filter #(<= (d %) reach) ready)
        far (remove #(<= (d %) reach) ready)]
    (cond
      (seq near) (let [e (first (sort-by (juxt #(- (get (:cell %) 1)) d) near))]
                   {:step :dig :cell (:cell e) :item (:item e)})
      (seq far) {:step :walk :cell (:cell (first (sort-by d far)))}
      :else {:step :finish :open (mapv (fn [[e why]] (merge {:cell (:cell e) :item (:item e)} why)) judged)})))

;; ------------------------------------------------------------------ reading the world

(defn block-at-of [p] (fn [[x y z]] (u/block-name p {:x x :y y :z z})))

(defn feet-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn eye-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [x (+ y eye-height) z]))

(defn inputs
  "next-step's input now: entries to work on, the whole ledger l for the rules."
  [c l entries zones]
  (let [{:keys [accept reach]} (:args c)]
    (merge (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c))})
           {:feet (feet-of c) :eye (eye-of c) :block-at (block-at-of (:primitives c)) :entries entries
            :ledger (ledger/cells l) :zones zones :accept (set accept) :reach reach
            :held (:held (ctx/mem c) {})})))

(defn bad-job? [job] (not (or (nil? job) (= :all job) (string? job))))

;; ------------------------------------------------------------------ check

(defn check [c]
  (cond
    (and (nil? (ctx/zones c)) (not (:ignore-zones? (:args c))))
    (do (ctx/warn-once! c :no-zones :cleanup.declined
                        {:reason "no zone list has been read" :text "cleanup declines: no zone list has been read"})
        false)
    (:started (ctx/mem c)) true
    :else (boolean (seq (ledger/offered (ctx/view c) (block-at-of (:primitives c)) (:job (:args c)))))))

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
        under? (= cell (update (feet-of c) 1 dec))]
    (if (or (nil? e) (blocker (inputs c l [e] (ctx/zones c)) e))
      :continue
      (do
        (await (tools/equip-tool! c item {:fast true}))
        (ledger/remember! c (ledger/begin-removal l cell))
        (let [[x y z] cell
              status (.-status (await (ctx/act c :dig #js {:pos #js {:x x :y y :z z}})))
              _ (await (tools/note-wear! c))]
          (when under? (await (walk/settle! c)))
          (if (#{"dug" "missing"} status)
            (ctx/update-mem! c assoc :collect true)
            (ctx/update-mem! c count-fail cell {:reason :dig-failed :dig status} (:give-up (:args c))))
          :continue)))))

(defn ^:async walk!
  "Walk to within 3 of cell; no plan holds it :unreachable, an end out of reach counts a failure."
  [c {:keys [cell]}]
  (let [r (await (ctx/call-child c :walk 'jobs.debug.walk-plan {:to cell :range walk-range}))]
    (when (= :done r)
      (let [status (:status (ctx/child-result c :walk))]
        (cond
          (#{:no-path :refused :unsupported :bad-args} status)
          (ctx/update-mem! c assoc-in [:held cell] {:reason :unreachable :walk status})
          (> (distance (eye-of c) (centre cell)) (:reach (:args c)))
          (ctx/update-mem! c count-fail cell {:reason :out-of-reach :walk status} (:give-up (:args c))))))
    :continue))

(defn collect-radius [c removed]
  (let [{:keys [x y z]} (u/self-pos c)]
    (min max-collect-radius
         (+ 4 (js/Math.ceil (apply max 0 (map #(distance [x y z] (centre (:cell %))) removed)))))))

(defn ^:async collect! [c removed]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 {:radius (collect-radius c removed) :filter (vec (distinct (map (comp placement/item-of :item) removed)))}))]
    (when (= :done r)
      (ctx/update-mem! c #(-> % (dissoc :collect) (assoc :collected (:collected (ctx/child-result c :collect) 0)))))
    :continue))

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

(defn finish! [c open]
  (let [m (ctx/mem c)
        result {:removed (:removed m []) :dropped (:dropped m []) :open open :collected (:collected m 0)}]
    (hold-open! c open)
    (when (seq open)
      (ctx/emit! c :cleanup.left :warn {:open open
                                        :text (str "cleanup left " (count open) " blocks: "
                                                   (apply str (interpose ", " (map cell-text open))))}))
    (ctx/emit! c :cleanup.done :info {:removed (count (:removed result)) :dropped (count (:dropped result))
                                      :open (count open) :collected (:collected result)
                                      :text (str "cleanup removed " (count (:removed result)) ", dropped "
                                                 (count (:dropped result)) " not ours, " (count open) " left")})
    (ctx/result! c result)
    :done))

(defn settle!
  "Settle the entries this cleanup works on from their cells; store the ledger when it changed, book removed and
  dropped entries (one cleanup.dropped info each). The ledger after."
  [c l picked]
  (let [{:keys [ledger removed dropped]} (ledger/settle l #(picked (:cell %)) (block-at-of (:primitives c)))]
    (when (not= ledger l) (ledger/remember! c ledger))
    (doseq [{:keys [cell item found job]} dropped]
      (ctx/emit! c :cleanup.dropped :info {:cell cell :item item :found found :job job
                                           :text (str "scaffold " item " at " (pr-str cell) " is now " found
                                                      ": not ours any more, entry dropped")}))
    (ctx/update-mem! c #(-> %
                            (update :removed (fnil into []) (map (fn [e] (select-keys e [:cell :item]))) removed)
                            (update :dropped (fnil into []) (map (fn [e] (select-keys e [:cell :item :found]))) dropped)))
    ledger))

(defn ^:async work! [c zones]
  (let [view (ctx/view c)
        l (ledger/open-entries view)
        picked (set (map :cell (ledger/select l view (:job (:args c)))))
        settled (settle! c l picked)
        step (next-step (inputs c settled (filterv #(picked (:cell %)) settled) zones))
        m (ctx/mem c)]
    (case (:step step)
      :dig (await (dig! c step))
      :walk (await (walk! c step))
      (if (and (:collect m) (seq (:removed m)))
        (await (collect! c (:removed m)))
        (finish! c (:open step))))))

(defn ^:async round [c]
  (let [zones (ctx/zones c)
        {:keys [job]} (:args c)]
    (cond
      (bad-job? job) (do (ctx/result! c {:status :bad-args :text ":job must be nil, :all or an instance id"}) :done)
      (and (nil? zones) (not (:ignore-zones? (:args c)))) :declined
      (and (not (:started (ctx/mem c)))
           (empty? (ledger/offered (ctx/view c) (block-at-of (:primitives c)) job))) :declined
      :else (do (ctx/update-mem! c assoc :started true)
                (await (work! c zones))))))
