(ns jobs.access.stair.step
  "The pieces of a stair step that call the world: look, judge, peek, seal lava, plug water, dig a cell, bridge a missing
  floor. Shared by jobs.access.stair and jobs.access.tunnel."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.access.stair.cells :as cells]
            [jobs.access.stair.memory :as mem]
            [jobs.access.stair.limits :as sset]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.bridge :as bridge]
            [jobs.lib.choose :as choose]
            [jobs.lib.dig-look :refer [unknown? look-at! see-round!]]
            [jobs.lib.escape :as escape]
            [jobs.lib.lava :as lava]
            [jobs.lib.tools :as tools]
            [jobs.access.stock :as stock]
            [jobs.lib.trip :as trip]
            [jobs.lib.walk :as walk]
            [plan.shape :as shape]))

(defn judged-in
  "in for the step about to be taken: a floor the body has not seen stops it (:floor-unseen)."
  [c in]
  (assoc in :unseen? #(unknown? (:primitives c) %)))

(defn ^:async look-ahead!
  "Once per step from feet (memory :looked): look at the next cell and the floor under it, at each cut cell and the cell
  over the top one (lava or a falling block there is a hazard) still unknown, and at the unknown neighbours of the feet and head, so what a player would see of the step is seen when it is judged."
  [c feet next cut]
  (when-not (= [feet next] (:looked (ctx/mem c)))
    (ctx/update-mem! c assoc :looked [feet next])
    (await (look-at! c next))
    (loop [cells (concat cut [(shape/add (first cut) [0 1 0])] (map #(shape/add next [0 % 0]) [-1 -2])
                         (for [b [feet (shape/add feet [0 1 0])] d rules/neighbour-deltas] (shape/add b d)))]
      (when-let [cell (first cells)]
        (when (unknown? (:primitives c) cell) (await (look-at! c cell)))
        (recur (rest cells))))))

(def lean-reach "How far from its cell's centre toward the next cell the body leans to see that cell's floor (blocks)." 0.3)

(defn ^:async lean!
  "Walk the body inside its own cell (feet) toward next, lean-reach from the centre: from the back of the cell the edge
  of its own floor hides a floor below ahead."
  [c [fx _ fz :as feet] [nx _ nz :as next]]
  (when (and feet next)
    (await (walk/centre! c (+ fx 0.5 (* lean-reach (js/Math.sign (- nx fx)))) (+ fz 0.5 (* lean-reach (js/Math.sign (- nz fz))))))))

(defn ^:async judge!
  "stop-of for the step about to be taken. A floor still unseen once the cut is open gets one more look (memory
  :floor-looked), then a look after a lean toward it (memory :floor-leaned), each judged again, so a glance that
  missed is not a final stop."
  [c in cells accept]
  (loop [tries [:floor-looked :floor-leaned]]
    (let [stop (cells/stop-of (judged-in c in) cells accept)
          cell (:cell stop)
          k (first tries)]
      (cond
        (or (nil? k) (not= :floor-unseen (:reason stop))) stop
        (= cell (k (ctx/mem c))) (recur (rest tries))
        :else (do (ctx/update-mem! c assoc k cell)
                  (when (= :floor-leaned k) (await (lean! c (:feet in) (:next cells))))
                  (await (look-at! c cell))
                  (recur (rest tries)))))))

(defn ^:async peek!
  "Before digging cell: when it is still unknown, look at it once (memory :peeked) and give :again so the step is
  judged on what shows; else nil."
  [c cell]
  (when (and (unknown? (:primitives c) cell) (not= cell (:peeked (ctx/mem c))))
    (ctx/update-mem! c assoc :peeked cell)
    (await (look-at! c cell))
    :again))

(defn ^:async lava-step!
  "Exposed lava by the job's :on-lava: nil when there is none; :stop gives the stop :lava-exposed, else the first is
  sealed (jobs.lib.lava/seal!, event kind)."
  [c kind block-at feet cut]
  (when-let [cell (first (cells/exposed-lava block-at feet cut))]
    (if (= :stop (:on-lava (:args c)))
      {:reason :lava-exposed :cell cell :fluid "lava"}
      (await (lava/seal! c kind block-at cell {:fetch (:fetch (:args c)) :ignore-zones? (:ignore-zones? (:args c))})))))

(defn ^:async water-step!
  "Water a dig laid open: a cell the stair dug that now touches seen water is filled with a building block at once
  (jobs.lib.lava/seal!, event stair.plugged), before the water pours in. nil when there is none; :continue while the
  place child waits; the stop {:reason :hazard :cell :plugged :hazards} once plugged, else {:reason :water-unplugged}."
  [c block-at accept cut]
  (when-let [[cell w] (first (cells/exposed-water block-at (:dug (ctx/mem c)) accept cut))]
    (let [r (await (lava/seal! c :stair.plugged block-at cell {:fetch (:fetch (:args c)) :ignore-zones? (:ignore-zones? (:args c))}))]
      (cond
        (= :continue r) :continue
        (= :again r) (do (mem/plug! c cell) {:reason :hazard :cell cell :plugged cell :hazards [{:reason :fluid-adjacent :fluid "water" :at w}]})
        :else (assoc r :reason :water-unplugged)))))

(defn by-hand?
  "Whether block is dug bare-handed: `:need-drop false` asks for it, or the fetch of a tool for it failed (the way out must open
  even with no pickaxe to be had)."
  [c block]
  (and (tools/needs-tool-to-clear? (:primitives c) block)
       (or (false? (:need-drop (:args c)))
           (mem/tool-failed? c block))))

(defn lacks-tool?
  "A tool is needed and not carried, and the block is not dug by hand (by-hand?)."
  [c block]
  (and (not (by-hand? c block)) (tools/needs-tool-to-clear? (:primitives c) block)))

(defn drop-room?
  "Room for block's drop, or no drop to keep: a :need-drop false dig with no tool that harvests it loses it."
  [c block]
  (or (by-hand? c block)
      (blocks/room-for? (:primitives c) (blocks/item-name {:block block}))))

(defn ^:async dig!
  "Equip the best tool, check the cell again, write the intent and dig it (a blocks.dig child). :continue (dug, go on), :yield (the child waits on the world), or a stop map."
  [c in cell cut accept]
  (let [p (:primitives c)
        block ((:block-at in) cell)
        seen? (not (unknown? p cell))
        tries (get-in (ctx/mem c) [:cell-tries cell] 0)]
    (cond
      (>= tries (if (rules/falling? block) (sset/max-fall-digs) (sset/max-cell-digs))) (cond-> {:reason :refills :cell cell :block block}
                                   (not (rules/falling? block))
                                   (assoc :why "put back by something else, e.g. the night shelter closing an opened refuge again"))
      (rules/unbreakable block) {:reason :unbreakable :cell cell :block block}
      (and seen? (lacks-tool? c block)) {:reason :no-tool :cell cell :block block :tool (tools/needed-kind p block)}
      (and (not (:spent-ok (:args c))) (trip/spent-tool? p block))
      (if (= :continue (await (stock/spare! c block nil)))
        :yield
        (if (trip/spent-tool? p block)
          {:reason :tool-spent :cell cell :block block :tool (tools/needed-kind p block)}
          :continue))
      (and seen? (not (drop-room? c block))) {:reason :inventory-full :cell cell :block block}
      :else
      (do
        (await (tools/equip! c block))
        (let [v (cells/cell-verdict (assoc in :cell cell) cut)
              block ((:block-at in) cell)]
          (cond
            (not (:ok v)) (assoc (dissoc v :ok) :cell cell)
            (not-every? (comp accept access/stair-hazard-key) (:hazards v)) {:reason :hazard :cell cell :hazards (:hazards v) :why (cells/hazards-why (:hazards v))}
            (rules/air block) :continue
            :else
            (do
              ;; a waiting child is resumed on the same cell: only the first round of a dig counts a try
              (ctx/update-mem! c #(cond-> (assoc % :digging {:cell cell :block block} :counted cell)
                                    (not= cell (:counted %)) (update-in [:cell-tries cell] (fnil inc 0))))
              (let [[x y z] cell
                    outcome (await (blocks/dig-cell! c {:x x :y y :z z} {:accept #{:fluid-adjacent :lava-adjacent :falling-block}
                                                                         :on-lava :leave :ignore-zones? true}))]
                (if (= :continue outcome)
                  :yield
                  (do
                    (ctx/update-mem! c #(-> % (dissoc :counted) (cells/record-dug (:block-at in))))
                    (when-let [tag (when (= :dug outcome) (:note (:args c)))] (escape/note-hole! c tag cell block))
                    (when (#{:dug :missing} outcome)
                      (ctx/update-mem! c assoc :dug-at (ctx/now c))
                      (await (see-round! c cell)))
                    (case outcome
                      (:dug :missing) :continue
                      {:reason :dig-failed :cell cell :block block :dig outcome})))))))))))

(defn take-back-floor?
  "Whether a floor this stair places goes into the scaffold ledger: a go-to stair (:note), unless the setting keeps it."
  [c]
  (boolean (and (:note (:args c)) (sset/take-back-floors?))))

(defn bridge-opts
  "The bridge/bridge-plans opts of the stair: a stair down may bridge back along its line (a stair up would place into its own cut)."
  [c]
  (let [{:keys [dir heading]} (:args c)
        [dx _ dz] (cells/delta dir heading)]
    (when (= :down dir) {:back [(- dx) (- dz)] :depth (sset/bridge-depth)})))

(defn ^:async shut-opening!
  "A stair down that cannot floor the cave it broke into shuts the cut cell over the missing floor with carried filler, so
  the way into the cave is closed behind the body: the stop with :sealed [cell], or with :open-to [cell] when it stays open."
  [c in stop {:keys [floor]} item]
  (let [cell (update floor 1 inc)
        open (assoc stop :open-to [cell])]
    (cond
      (not (and (= :down (:dir (:args c))) (#{:no-support-floor :no-floor} (:reason stop)))) stop
      (or (nil? item) (not (rules/air ((:block-at in) cell)))) open
      (await (bridge/place-floor! c in item cell (take-back-floor? c))) open
      :else (assoc stop :sealed [cell]))))

(defn ^:async look-back!
  "Once per cell (memory :back-looked), look at the cells of the bridge chain behind floor that are still unknown: the body
  looks forward, so the support it would bridge from is often unseen."
  [c floor]
  (let [{:keys [back depth]} (bridge-opts c)]
    (loop [cells (when back (for [j (range 1 (inc depth))] (mapv + floor [(* j (first back)) 0 (* j (second back))])))]
      (when-let [cell (first cells)]
        (when (and (unknown? (:primitives c) cell) (not (contains? (:back-looked (ctx/mem c)) cell)))
          (ctx/update-mem! c update :back-looked (fnil conj #{}) cell)
          (await (look-at! c cell)))
        (recur (rest cells))))))

(defn ^:async bridge!
  "Place carried filler on the missing floor of the step (bridge/bridge-plans, a plan whose placing found no support gives way
  to the next). :again, or a stop map (:no-support-floor when no plan works), with the opening shut-opening! leaves."
  [c in {:keys [floor] :as cs}]
  (await (look-back! c floor))
  (let [item (choose/filler c :build)
        plans (bridge/bridge-plans (:block-at in) floor (bridge/eye-y c) (bridge-opts c))
        stop (cond
               (nil? item) {:reason :no-floor :cell floor :block ((:block-at in) floor) :filler :none
                            :why (str "no floor and no filler block carried (" (str/join ", " (take 4 (choose/fillers :build))) " ...)")}
               (empty? plans) {:reason :no-support-floor :cell floor :tried [floor (update floor 1 dec)]
                               :why (str "no block beside or under " (pr-str floor) " to place " item " against: the floor opens into a void")}
               :else (await (bridge/place-plans! c in item plans (take-back-floor? c))))]
    ;; a refusal may be the body's own entity in the cell: one more round before giving up
    (cond
      (and (= :place-failed (:reason stop)) (not= (:cell stop) (:refused (ctx/mem c))))
      (do (ctx/update-mem! c assoc :refused (:cell stop)) :again)
      stop (await (shut-opening! c in stop cs item))
      :else :again)))
