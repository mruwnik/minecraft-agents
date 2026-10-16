(ns jobs.survival.dig-in
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [jobs.lib.pen-gate :as pen-gate]
            [jobs.lib.reach :as reach]
            [jobs.lib.blocks :as lb]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.choose :as choose]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-in.mode :as mode]
            [jobs.survival.dig-in.pit :as pit]
            [jobs.survival.dig-in.site :as site]
            [jobs.survival.dig-in.walls :as walls]))

(def doc
  "Roof the body in for the night.
  Conditions (at the start only): night, else condition-failed :day; no roof over open cells (not with :enclose, which walls
  them in), else :roofed-not-enclosed {:pos}; no :dig-in-futile entry within 8 blocks that blocks it (see below), else
  :futile {:pos :why}. Already shut in (a roof within :roof-height above and no open cell around the feet, or a closed
  room) is :done before anything is placed.
  One call is a whole attempt. It ends :done {:pos :mode :roof} when the world shows the body shut in. Otherwise a fact of the
  site is condition-failed (:no-blocks, :no-tool, :spared, :no-roof-support, :no-floor, :fluid-here, :fluid-adjacent,
  :fluid-above, :hazard-below) and a try that failed is nothing-possible (:walls-server-refused,
  :walls-failed, :dig-failed, :descent-stalled, :left-pit, :roof-failed, :plug-failed, :no-progress, :unsealed); the data is
  {:text :pos}.
  The mode is the first of these whose cells the zone rules permit. If none is permitted it takes the first
  as a last resort, with one dig-in.trespass-last-resort warning.
  - Blocks it places: building blocks (planks, stone kinds, dirt), then logs as a last resort.
  A pit that would cut into the footprint of a plan the body made (a pen) is not dug (:spared, a
  dig_in_failed warning naming the cell and the plan): the shelter looks elsewhere. Walls and plug are not affected.
  - plug: the body is in a closed room with a door and a hole in the roof over it. One carried block mends the hole.
  - walls: enough of the call's :use :blocks (default the shelter fillers) are carried for every open cell. Calls jobs.survival.plug-in (walls way, :purpose :shelter), which places the four sides at feet height, the four at head height,
    a support beside the roof cell, then the roof cell, at most :max-places per step, and takes the open cells from the current feet cell. If the blocks run out, it chooses again; a walls attempt that fails ends the call.
  - :enclose: walls mode only, for a body under an overhang or in a cave: the walls it needs, else :no-blocks.
  - dig: digs a pit two deep (three on flat ground, where the start cell has no solid side to roof against),
    then places one block at the roof cell from a carried or dug block.
    The body never drops onto a floor it has not seen solid (it cannot see under the block it stands on): straight down
    only when every floor is known, else it zigzags between its column and an open side column (every dig beside or
    below-beside it, lava a dig lays open sealed while it stands above), plugs the side cell by its head, then roofs.
    With no such pit it ends :no-floor.
    It digs only where the block under is solid, the feet and head cells are dry, and no fluid borders the cell
    (water, lava, bubble column, kelp, seagrass, or a waterlogged block), nor the roof cell or the cell above it
    (:fluid-above).
    It holds the best carried tool for each block first. After each dig it looks at what the dig laid open and does
    not walk into the hole when the cell under it is seen not solid (:no-floor; rock it cannot see counts as stone).
    If the body leaves the column, it chooses again.
  With no full block beside the roof cell at either height (a flower or crop is no support) it does not dig
  (dig_in_failed warning, :no-roof-support).
  Walls: a cell the server refuses is retried after the others, and given up after two refusals; a cell occupied by a block a mob walks through (torch, sapling, cobweb) is dug once and placed again;
  a door, gate or trapdoor beside the body counts as a wall only when shut, an open one is shut with one click, an iron one is given up on.
  Fails (dig_in_failed warning) after three failures in a row to dig, or at once when a walls attempt fails or:
  a hazard is below, no floor is under the pit, no block can roof the pit, or the carried tools cannot harvest the block below.
  Every end with no roof over the body leaves a :dig-in-futile {:pos :reason} entry,
  and the start declines while one lies within 8 blocks. An entry with no :reason (a lack of blocks or tools)
  is retried once blocks are carried or a carried tool harvests the block.
  Walls need blocks worth at most :wall-value each; dearer ones (logs, planks) wall the body in only when no pit can be dug.
  Events: dig-in.sealed when it placed blocks and the world shows the body shut in, with :spent {item count} (its text names them)
  (a warning when :resealed, meaning the latest :shelter entry was already at this cell),
  else dig-in.unsealed (warning) naming the cells still open.
  Memory: reads :dig-in-futile. Writes :dig-in-futile (cap 5, ten minutes)
  and, on any end, :shelter {:pos :roof :state :built} (cap 10, one in-game day).
  :roof is the cell it placed above the body, absent if none. Walls mode adds :door, the feet and head cells of one side it placed.
  Pit mode adds :start, the cell dug from. Walls mode at the bottom of a shaft has :start at the shaft's top.
  :take-back lists the placed {:cell :item} dearer than :take-back-value, and any within 2 of a fence gate a plan wants, which leave! digs again by day.
  Plug mode adds :room true. A shelter sealed again where the latest entry stood keeps that entry's :start and :door.
  Leaving is jobs.lib.dig-in-leave/leave!, which jobs.survival.night calls by day.
  Quote: time by the open cells to place or the pit to dig; done 1 shelter. The pit's step into its own cell is a raw moveTo
  (the planner has no standable goal inside the pit being dug).")

(a/defargs args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :wall-value {:doc "a block worth more than this each (jobs.lib.cost/item-value; dirt 0.35, cobblestone 0.85, stone 2.85, planks 3.4, log 12.8) walls the body in only when no pit can be dug" :spec (a/num-in 0 nil) :default 3}
   :take-back-value {:doc "a placed block worth more than this each (jobs.lib.cost/item-value; dirt 0.35, cobblestone 0.85) is dug again and collected when the shelter is left by day" :spec (a/num-in 0 nil) :default 0.5}
   :max-places {:doc "placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava the dig lays open in or beside the hole is filled with a carried block; :stop: the dig is given up (:fluid-adjacent)" :spec #{:seal :stop} :default :seal}
   :enclose {:doc "wall in under a roof already overhead: walls only, never a pit; ends :no-blocks when too few blocks are carried" :spec boolean? :default false}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(def gate-take-back-range
  "A cheap seal block this near a planned gate is taken back with the dear ones: it would box the gate in."
  2)

(defn near-gate?
  "Whether cell {:x :y :z} lies within gate-take-back-range of a door or fence gate some plan wants (jobs.lib.pen-gate)."
  [c {:keys [x y z]}]
  (boolean (some (fn [[gx gy gz]] (<= (u/dist {:x x :y y :z z} {:x gx :y gy :z gz}) gate-take-back-range))
                 (keys (pen-gate/opening-index (:world (:engine c)))))))

(a/defargs settings
  {::futile-radius {:default 8 :doc "Blocks around a failed dig-in site that count as the same site." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 300 :doc "Seconds a shelter may take: the placements, or the pit and its roof." :spec (a/num-in 0 nil)}
   ::cell-s {:default 1.5 :doc "Seconds one placement takes, for the quote." :spec (a/num-in 0 nil)}
   ::pit-s {:default 8 :doc "Seconds a pit takes to dig, for the quote." :spec (a/num-in 0 nil)}})
(defn futile-radius [] (settings/get settings ::futile-radius))
(defn time-s [] (settings/get settings ::job-time-s))

(defn futile-entry-here?
  "Whether any :dig-in-futile entry, whatever its reason, lies within futile-radius of the body."
  [c]
  (let [here (u/self-pos c)]
    (boolean (some #(<= (u/dist here (:pos (:data %))) (futile-radius)) (ctx/entries c :dig-in-futile)))))

(defn note-unroofed!
  "A dig-in that ends with no roof over the body leaves a :dig-in-futile entry, so a caller that holds the body (the
  night shelter) does not run it again every round: the entry its failure wrote, else {:pos feet :reason :unsealed}."
  [c]
  (when-not (or (sh/roofed? (:primitives c) (:roof-height (:args c))) (futile-entry-here? c))
    (ctx/remember! c :dig-in-futile {:pos (sh/feet (:primitives c)) :reason :unsealed} site/futile-policy)))

(defn futile-site
  "The :dig-in-futile entry data that blocks digging here, or nil. Material-only failures (no :reason) can be retried
  once blocks are carried, or, when the entry names the block it could not harvest (:needs), once a carried tool
  harvests it; unsafe or inaccessible sites (a :reason) cannot."
  [c]
  (let [here (u/self-pos c)
        have-blocks (seq (lb/carried c (mode/blocks c)))
        retry? (fn [{:keys [reason needs]}]
                 (and (not reason)
                      (or have-blocks (and needs (tools/can-harvest? (:primitives c) needs)))))]
    (some #(when (and (not (retry? (:data %)))
                      (<= (u/dist here (:pos (:data %))) (futile-radius)))
             (:data %))
          (ctx/entries c :dig-in-futile))))

(defn sheltered?
  "Whether the world shows the body shut in: walls round it under a roof, or (not :enclose) a roof over a closed room."
  [c]
  (let [p (:primitives c)
        roof-height (:roof-height (:args c))]
    (boolean (or (dig-cells/sealed-in? p :walls roof-height)
                 (and (not (:enclose (:args c))) (sh/roofed? p roof-height) (reach/closed-room? p (sh/feet p)))))))

(defn site-status
  "Not :roofed-not-enclosed (a roof over open sides, without :enclose), nor :futile (a site that failed here, with its
  :pos and :why)."
  [c]
  (let [p (:primitives c)]
    (if (and (not (:enclose (:args c))) (dig-cells/sealed-in? p :open (:roof-height (:args c))))
      (contract/later :roofed-not-enclosed "roofed but open at the sides" :pos (sh/feet p))
      (if-let [site (futile-site c)]
        (contract/status :later :futile "a dig-in failed here" (merge (select-keys site [:pos]) (when (:reason site) {:why (:reason site)})))
        (contract/ok :site "a site to dig in")))))

(def can-start? (cond/and (checks/night :day) site-status))

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)


(defn done?
  "Shut in already, before this call placed anything, or once it ended (a call that placed blocks reports its own end)."
  [c]
  (let [m (ctx/mem c)
        done (and (sheltered? c) (or (:ended m) (and (empty? (:placed m)) (not (checks/running? c)))))]
    (checks/goal done [:sheltered "shut in"] [:exposed "not shut in"]
                 {:units :shelters :done (if done 1 0) :left (if done 0 1)})))

(defn quote-of [c]
  (let [p (:primitives c)
        n (count (dig-cells/open-cells p (sh/feet p)))
        cell-s (settings/get settings ::cell-s)
        pit-s (settings/get settings ::pit-s)
        mean (cond (sheltered? c) 0 (zero? n) cell-s :else (min (* n cell-s) (+ pit-s cell-s)))]
    {:uses {:job-time [(if (sheltered? c) 0 cell-s) mean (max (* 3 mean) (time-s))]} :done [0 1 1]}))

(defn ^:async step [c]
  (await (mode/choose-mode c))
  (let [{:keys [mode target-y]} (ctx/mem c)]
    (cond
      (= :plug mode) (await (walls/plug-round c))
      (= :walls mode) (await (walls/walls-round c))
      (= :no-roof-support mode) (mode/no-roof-round c)
      (= :no-floor mode) (mode/no-floor-round c)
      (= :spared mode) (mode/spared-round c)
      (= :no-blocks mode) (do (site/remember-material! c {:pos (sh/feet (:primitives c))}) :done)
      (> (:y (sh/feet (:primitives c))) target-y) (await (if (seq (:plugs (:shape (ctx/mem c)))) (pit/zigzag-round c) (pit/descend-round c)))
      :else (await (pit/roof-round c)))))

(defn spent-text [spent]
  (str/join ", " (map (fn [[item n]] (str n " " item)) (sort-by (comp - val) spent))))

(def fact-stops
  "The stops that name a fact of the site (condition-failed); the others are tries that failed (nothing-possible)."
  #{:no-blocks :no-tool :spared :no-roof-support :no-floor :fluid-here :fluid-adjacent :fluid-above :hazard-below})

(defn stop-ending
  "The ending of a call that left the body unroofed, by the reason noted in memory (:unsealed when none)."
  [c feet]
  (let [reason (or (:stop (ctx/mem c)) :unsealed)]
    (ctx/end c (if (fact-stops reason) :condition-failed :nothing-possible) reason
             {:pos feet :text (str "no roof over the body: " (name reason))})))

(defn end!
  "The call's end: a :dig-in-futile entry when unroofed, the sealed/unsealed event, the :shelter entry; then the ending:
  :done {:pos :roof :mode} when the world shows the body shut in, else the stop noted (:unsealed when none), see stop-ending."
  [c]
  (note-unroofed! c)
  (let [p (:primitives c)
        feet (sh/feet p)
        placed (:placed (ctx/mem c) #{})
        mode (:mode (ctx/mem c))
        judged (if (:enclose (:args c)) :walls mode)
        roof (case mode
               :walls (update feet :y + 2)
               :plug (:plug (ctx/mem c))
               (:roof (ctx/mem c)))
        door (when (= :walls (:mode (ctx/mem c))) (dig-cells/door placed feet))
        start (when (#{:dig :walls} (:mode (ctx/mem c))) (:start (ctx/mem c)))
        spent (:spent (ctx/mem c) {})
        cheap (set (choose/cheap-blocks (keys spent) (:take-back-value (:args c))))
        prev (:data (ctx/latest c :shelter))
        resealed (= feet (:pos prev))
        take-back (->> (concat (when resealed (:take-back prev))
                               (for [[cell item] (:placed-items (ctx/mem c)) :when (or (not (cheap item)) (near-gate? c cell))] {:cell cell :item item}))
                       (reduce #(assoc %1 (:cell %2) %2) {}) vals vec)
        start (or start (when resealed (:start prev)))
        door (or door (when resealed (:door prev)))]
    (when (and (seq placed) (not (dig-cells/sealed-in? p judged (:roof-height (:args c)))))
      (let [open (dig-cells/open-cells p feet)]
        (ctx/emit! c :dig-in.unsealed :warn
                   {:pos feet :placed (vec placed) :open open
                    :text (str "NOT sealed in: placed " (count placed) " blocks at " (pr-str (mapv (juxt :x :y :z) placed))
                               ", but the world still shows " (site/open-text open) (when (empty? open) " (no roof)"))})))
    (when (and (seq placed) (dig-cells/sealed-in? p judged (:roof-height (:args c))))
      (ctx/emit! c :dig-in.sealed (if resealed :warn :info)
                 {:pos feet :placed (vec placed) :resealed resealed :spent spent
                  :text (str (cond
                               (= :plug mode) "mended the roof of a closed room over the body: "
                               resealed "sealed the shelter again: "
                               :else "sealed in for the night: ")
                             "placed " (count placed) " blocks (" (spent-text spent) ") at " (pr-str (mapv (juxt :x :y :z) placed))
                             ". At night an open shelter is closed again; it is left by day")}))
    (ctx/remember! c :shelter (cond-> {:pos feet :state :built}
                                (contains? placed roof) (assoc :roof roof)
                                door (assoc :door door)
                                start (assoc :start start)
                                (seq take-back) (assoc :take-back take-back)
                                (= :plug mode) (assoc :room true))
                   sh/shelter-policy)
    (ctx/update-mem! c assoc :ended true)
    (if (dig-cells/sealed-in? p judged (:roof-height (:args c)))
      (ctx/end c :done nil (cond-> {:pos feet :mode mode} (contains? placed roof) (assoc :roof roof)))
      (stop-ending c feet))))

(def max-steps
  "Steps one call takes at most (walls or a pit take about ten); past it the call ends :no-progress."
  64)

(defn ^:async round
  "One call is a whole attempt: steps (a placement batch, a dig, a descent, the roof) until the body is roofed or the
  site fails, a timer between steps. A place refused for a mob in the cell yields :continue and is retried
  (dig-cells/mob-wait-ms), then fails naming the mob."
  [c]
  (ctx/update-mem! c dissoc :stop :plug-occupied)
  (let [n (atom 0)
        part (fn ^:async part []
               (let [r (if (<= (swap! n inc) max-steps)
                         (await (step c))
                         (do (site/stop-reason! c :no-progress) :done))]
                 (case r :continue :again r)))
        r (await (pace/steps! c part))]
    (if (#{:yield :continue} r)
      :continue
      (do (dig-cells/forget-wait! c :mob-since)
          (end! c)))))
