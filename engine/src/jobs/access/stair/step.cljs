(ns jobs.access.stair.step
  "The pieces of a stair step that call the world: look, judge, peek, seal lava, plug water, dig a cell, bridge a missing
  floor. Shared by jobs.access.stair and jobs.access.tunnel."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.need :as need]
            [clojure.string :as str]
            [engine.constants :as constants]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.stair-cells :as cells]
            [jobs.lib.stair-memory :as mem]
            [jobs.access.stair.limits :as sset]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.aim :as aim]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.bridge :as bridge]
            [jobs.lib.choose :as choose]
            [jobs.lib.dig-look :refer [unknown? look-at! see-round!]]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.span :as span]
            [jobs.lib.lava :as lava]
            [jobs.lib.reach :as reach]
            [jobs.lib.tools :as tools]
            [jobs.lib.torch-stock :as stock]
            [jobs.lib.trip :as trip]
            [jobs.lib.walk :as walk]
            [engine.game :as game]
            [jobs.lib.util :as u]
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

(defn ^:async see-over!
  "After the top cut cell is dug: lean toward it and look at the cell over it when that is still unseen. From the back of
  the cell the rock beside hides it, and lava over the cell would pour in as soon as the flow delay has passed."
  [c in cell cut]
  (let [over (shape/add cell [0 1 0])]
    (when (and (= cell (first cut)) (unknown? (:primitives c) over))
      (await (lean! c (:feet in) cell))
      (await (look-at! c over))
      (let [[fx _ fz] (:feet in)] (await (walk/centre! c (+ fx 0.5) (+ fz 0.5)))))))

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

(defn ^:async judge-after-lava!
  "judge!, but a lava hazard that shows only now (lava poured in after this round's seal check) gives :again, so the
  next round seals it instead of stopping."
  [c in cells accept cut feet]
  (let [stop (await (judge! c in cells accept))]
    (if (and (= :hazard (:reason stop))
             (not= :stop (:on-lava (:args c)))
             (some #(= "lava" (:fluid %)) (:hazards stop))
             (seq (cells/exposed-lava (:block-at in) feet cut)))
      :again
      stop)))

(defn ^:async peek!
  "Before digging cell: when it is still unknown, look at it once (memory :peeked) and give :again so the step is
  judged on what shows; else nil."
  [c cell]
  (when (and (unknown? (:primitives c) cell) (not= cell (:peeked (ctx/mem c))))
    (ctx/update-mem! c assoc :peeked cell)
    (await (look-at! c cell))
    :again))

(def seal-settle-ms "How long after a seal the stair waits, looking, before it goes on: lava put back beside the cell shows within a tick or two and reaches the body's cell after about 1.5 s." 500)

(defn ^:async lava-step!
  "Exposed lava by the job's :on-lava: nil when there is none; :stop gives the stop :lava-exposed, else the first is
  sealed (jobs.lib.lava/seal!, event kind). After a seal :continue until seal-settle-ms have passed, so lava that
  comes back is seen before the stair digs or walks on beside it."
  [c kind block-at feet cut]
  (let [sealed-at (:sealed-at (ctx/mem c))]
    (if (and sealed-at (< (- (ctx/now c) sealed-at) seal-settle-ms))
      :continue
      (when-let [cell (first (cells/exposed-lava block-at feet cut))]
        (if (= :stop (:on-lava (:args c)))
          {:reason :lava-exposed :cell cell :fluid "lava"}
          (let [r (await (lava/seal! c kind block-at cell {:ways (need/call-ways c) :ignore-zones? (:ignore-zones? (:args c))}))]
            (when (= :again r) (ctx/update-mem! c assoc :sealed-at (ctx/now c)))
            r))))))

(defn ^:async look-at-lava!
  "Once per lava cell and state of the cut (memory :lava-looked), look at the first exposed lava: cells between the eye
  and it never sensed count as clear to body-sees?, and the seal's place child finds them solid only after its own look.
  A cut cell dug since the last look opens a new line, so it is looked again."
  [c block-at feet cut]
  (when-let [lava (first (cells/exposed-lava block-at feet cut))]
    (let [looked [lava (set (filter #(bs/air (block-at %)) cut))]]
      (when (not= looked (:lava-looked (ctx/mem c)))
        (ctx/update-mem! c assoc :lava-looked looked)
        (await (look-at! c lava))))))

(defn blind-cell
  "The first cut cell still solid when the body does not see the first exposed lava cell and the cell does not touch it:
  digging it first gives the seal a line, before the lava flows (nil when the lava is in sight or no such cell)."
  [c block-at feet cut]
  (when-let [lava (first (cells/exposed-lava block-at feet cut))]
    (when-not (aim/body-sees? c lava)
      (let [near? (set (map #(shape/add lava %) rules/neighbour-deltas))]
        (first (remove #(or (near? %) (bs/air (block-at %)) (bs/fluids (block-at %))) cut))))))

(defn ^:async water-step!
  "Water a dig laid open: a cell the stair dug that now touches seen water is filled with a building block at once
  (jobs.lib.lava/seal!, event stair.plugged), before the water pours in. nil when there is none; :continue while the
  place child waits; the stop {:reason :hazard :cell :plugged :hazards} once plugged, else {:reason :water-unplugged}."
  [c block-at accept cut]
  (when-let [[cell w] (first (cells/exposed-water block-at (:dug (ctx/mem c)) accept cut))]
    (let [r (await (lava/seal! c :stair.plugged block-at cell {:ways (need/call-ways c) :ignore-zones? (:ignore-zones? (:args c))}))]
      (cond
        (= :continue r) :continue
        (= :again r) (do (mem/plug! c cell) {:reason :hazard :cell cell :plugged cell :hazards [{:reason :fluid-adjacent :fluid "water" :at w}]})
        :else (assoc r :reason :water-unplugged)))))

(defn by-hand?
  "Whether block is dug bare-handed: `:need-drop false` asks for it."
  [c block]
  (and (tools/needs-tool-to-clear? (:primitives c) block)
       (false? (:need-drop (:args c)))))

(defn lacks-tool?
  "A tool is needed and not carried, and the block is not dug by hand (by-hand?)."
  [c block]
  (and (not (by-hand? c block)) (tools/needs-tool-to-clear? (:primitives c) block)))

(defn fetch-tool?
  "Whether the call's ways let the dig child get a missing pickaxe (it fetches inside the dig, so a missing tool is no stop)."
  [c]
  (seq (need/fetch-how c)))

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
      (>= tries (sset/max-digs block)) (cond-> {:reason :refills :cell cell :block block}
                                   (not (rules/falling? block))
                                   (assoc :why "put back by something else, e.g. the night shelter closing an opened refuge again"))
      (rules/unbreakable block) {:reason :unbreakable :cell cell :block block}
      (and seen? (not (fetch-tool? c)) (lacks-tool? c block)) {:reason :no-tool :cell cell :block block :tool (tools/needed-kind p block)}
      (trip/spent-tool? p block)
      (if (= :continue (await (trip/tool-ready! c block nil)))
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
            (bs/air block) :continue
            :else
            (do
              ;; a waiting child is resumed on the same cell: only the first round of a dig counts a try
              (ctx/update-mem! c #(cond-> (assoc % :digging {:cell cell :block block} :counted cell)
                                    (not= cell (:counted %)) (update-in [:cell-tries cell] (fnil inc 0))))
              (let [[x y z] cell
                    fetch? (and (fetch-tool? c) (lacks-tool? c block))
                    e (await (blocks/dig-cell-ending! c {:x x :y y :z z}
                                                      (cond-> {:accept #{:fluid-adjacent :lava-adjacent :falling-block} :on-lava :leave :ignore-zones? true}
                                                        fetch? (assoc :ways (need/call-ways c) :need-drop (not (false? (:need-drop (:args c))))))))
                    outcome (blocks/dig-outcome e)]
                (if (= :continue outcome)
                  :yield
                  (do
                    (ctx/update-mem! c #(-> % (dissoc :counted) (cells/record-dug (:block-at in))))
                    (when-let [tag (when (= :dug outcome) (:note (:args c)))] (escape/note-hole! c tag cell block))
                    (when (#{:dug :missing} outcome)
                      (ctx/update-mem! c assoc :dug-at (ctx/now c))
                      (await (see-round! c cell))
                      (await (see-over! c in cell cut)))
                    (case outcome
                      (:dug :missing) :continue
                      :no-tool {:reason :no-tool :cell cell :block block :tool (tools/needed-kind p block)
                                :because (let [cause (:cause e)] (when (map? cause) (:why cause)))}
                      {:reason :dig-failed :cell cell :block block :dig outcome})))))))))))

(def fall-wait-ms "How long after a dig the stair waits for a falling block over the open cut to drop before it walks in." 4000)

(defn fall-wait
  "Falling blocks (gravel, sand) still standing over the open cut drop into it one after another once the cell under them
  is open, and one landing on the body suffocates it: :continue until they have dropped, at most fall-wait-ms after the
  last dig (a block that does not fall is not waited for); else nil."
  [c block-at cut]
  (when (and (cells/hanging-above block-at cut)
             (< (ctx/now c) (+ (:dug-at (ctx/mem c) 0) fall-wait-ms)))
    :continue))

(def max-landings "Falling blocks that may land on the body in one stair before it stops: each one suffocates it." 2)

(defn ^:async clear-own-cell!
  "A falling block (gravel, sand) landed in the body's own head or feet cell as it walked in, so no walk out of it starts
  standable: dug, head first, until max-landings have landed (then the stop :hazard). Nil when both cells are open, else dig!'s result."
  [c in feet accept]
  (when-let [cell (first (filter #(rules/falling? ((:block-at in) %)) [(update feet 1 inc) feet]))]
    (if (>= (:landed (ctx/mem c) 0) max-landings)
      {:reason :hazard :cell cell :hazards [{:reason :falling-block :block ((:block-at in) cell) :at cell :on-body true}]
       :why "falling blocks keep landing on the body"}
      (let [r (await (dig! c in cell [cell] accept))]
        (when (= :continue r) (ctx/update-mem! c update :landed (fnil inc 0)))
        r))))

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
      (or (nil? item) (not (bs/air ((:block-at in) cell)))) open
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

(defn box-overlaps?
  "Whether the body's hitbox (half-width half: the server's by default, the client physics' for a jump) reaches into cell."
  ([pos cell] (box-overlaps? pos cell game/hitbox-half))
  ([pos [cx cy cz] h]
   (and (pos? (reach/overlap (- (:x pos) h) (+ (:x pos) h) cx))
        (pos? (reach/overlap (- (:z pos) h) (+ (:z pos) h) cz))
        (< (:y pos) (+ cy 1)) (> (+ (:y pos) game/body-height) cy))))

(def centre-tolerance "How near the middle of its cell a body stepping off an edge ends (blocks, each axis): under the walk's, which leaves a body 0.2 off." 0.1)

(defn ^:async clear-box!
  "Step to the middle of the body's own cell when its hitbox overlaps cell, so the server takes a block there (or, with half,
  so a jump onto a block there does not start inside it)."
  ([c cell] (clear-box! c cell game/hitbox-half))
  ([c cell half]
   (let [pos (u/self-pos c)]
     (when (box-overlaps? pos cell half)
       (await (walk/centre! c (+ (js/Math.floor (:x pos)) 0.5) (+ (js/Math.floor (:z pos)) 0.5) centre-tolerance))))))

(defn ^:async clear-jump-edge!
  "Before the walk into the next cell of a stair up: step to the middle of the body's cell when its box (the client's width)
  reaches into the block it jumps onto. A floor placed beside a body that coasted to the edge leaves it overlapping that
  block by a hair; the client physics then moves into the block and the server throws every move back."
  [c next-feet]
  (let [[nx _ nz] next-feet
        [_ fy _] (reach/standing-vec (:primitives c))]
    (await (clear-box! c [nx fy nz] constants/body-half-width))))

(defn ^:async plan-run!
  "The flat bridge a stair down takes when the floor of its next step has nothing to place against (bridge/flat-run along the
  heading, the cells ahead looked at once each): {:length n}, or nil when no run reaches support, the body carries no filler or the ledge is not open."
  [c in]
  (let [p (:primitives c)
        feet (reach/standing-vec p)
        h (shape/headings (:heading (:args c)))
        item (choose/filler c :build)
        at (fn [k dy] (mapv + feet [(* k (first h)) dy (* k (second h))]))]
    (when item
      (loop [k 1]
        (when (<= k (span/max-bridge))
          (doseq [cell [(at k 0) (at k 1) (at k -1) (at (inc k) -2)]]
            (when (and (unknown? p cell) (not (contains? (:run-looked (ctx/mem c)) cell)))
              (ctx/update-mem! c update :run-looked (fnil conj #{}) cell)
              (await (look-at! c cell))))
          (if-let [n (bridge/flat-run (:block-at in) feet h (bridge/eye-y c) (bridge-opts c) k)]
            {:length n :item item}
            (recur (inc k))))))))

(defn ^:async run-bridge!
  "The flat bridge of memory :run (a jobs.access.bridge child along the heading): :continue while it works. Once done the
  stair goes on from the bridge's end, which is its new origin (the cells above it are recorded as a way); :again, or a
  stop map when the bridge failed."
  [c]
  (let [{:keys [run origin at-step]} (ctx/mem c)
        {:keys [dir heading ignore-zones?]} (:args c)
        e (await (ctx/call c :run 'jobs.access.bridge {:heading heading :length (:length run) :item (:item run) :ways (need/call-ways c)
                                                      :ignore-zones? ignore-zones? :checks {:fit? '(always)}}))]
    (if (= :continue e)
      :continue
      (let [feet (reach/standing-vec (:primitives c))]
        (ctx/update-mem! c dissoc :run)
        (if (not= :done (:kind e))
          {:reason :no-support-floor :cell feet :why (str "the flat bridge over the void stopped: " (some-> (contract/cause-key e) name))}
          (do (when (pos? at-step) (mem/record-way! c origin dir heading at-step))
              (ctx/update-mem! c #(-> % (assoc :origin feet :at-step 0 :checked nil) (update :target - at-step)))
              :again))))))

(defn ^:async bridge-stop!
  "Place the floor of the step with the plans (bridge/bridge-plans): nil once placed, else the stop that ends it (:no-floor, :no-support-floor)."
  [c in {:keys [floor]} item plans]
  (let [stop (cond
               (nil? item) {:reason :no-floor :cell floor :block ((:block-at in) floor) :filler :none
                            :why (str "no floor and no filler block carried (" (str/join ", " (take 4 (choose/fillers :build))) " ...)")}
               (empty? plans) {:reason :no-support-floor :cell floor :tried [floor (update floor 1 dec)]
                               :why (str "no block beside or under " (pr-str floor) " to place " item " against: the floor opens into a void")}
               :else (await (bridge/place-plans! c in item plans (take-back-floor? c))))]
    stop))

(def fetch-blocks "Filler blocks a bridge fetches when it carries none." 6)

(defn ^:async fetch-filler!
  "With no filler carried and a fetch allowed: get some (jobs.items.obtain; need/return! walks the body back). :continue
  while it goes on, :again once it got some, the stop map when it failed, nil when there is nothing to fetch."
  [c floor]
  (when (and (nil? (choose/filler c :build)) (seq (need/fetch-how c)))
    (let [e (await (need/blocks! c (choose/filler-names c :build) fetch-blocks {:return? true :explore true :how (need/fetch-how c)}))
          why (contract/cause-key e)]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) :again
        :else {:reason :no-floor :cell floor :filler :none :fetch why
               :why (str "no floor and no filler block carried; getting some failed: " (pr-str why))}))))

(defn ^:async bridge!
  "Place carried filler on the missing floor of the step (bridge/bridge-plans, a plan whose placing found no support gives way
  to the next); with none carried, first get some (fetch-filler!). A stair down with no face to place against anywhere sets memory :run, a flat bridge ahead (plan-run!) that
  run-bridge! lays before the stair goes on. :again, :continue while a fetch goes on, or a stop map (:no-support-floor when no plan works), with the opening shut-opening! leaves."
  [c in {:keys [floor] :as cs}]
  (or (await (need/return! c))
      (await (fetch-filler! c floor))
      (do
        (await (look-back! c floor))
        (await (clear-box! c floor))
        (let [item (choose/filler c :build)
              plans (bridge/bridge-plans (:block-at in) floor (bridge/eye-y c) (bridge-opts c))
              run (when (and (empty? plans) (= :down (:dir (:args c)))) (await (plan-run! c in)))
              stop (when-not run (await (bridge-stop! c in cs item plans)))
              run (or run (when (and (= :no-support-floor (:reason stop)) (= :down (:dir (:args c)))) (await (plan-run! c in))))]
          ;; a refusal may be the body's own entity in the cell: one more round before giving up
          (cond
            run (do (ctx/update-mem! c assoc :run run) :again)
            (and (= :place-failed (:reason stop)) (not= (:cell stop) (:refused (ctx/mem c))))
            (do (ctx/update-mem! c assoc :refused (:cell stop)) :again)
            stop (await (shut-opening! c in stop cs item))
            :else :again)))))
