(ns jobs.survival.dig-in
  (:require [jobs.lib.blocks :as lb]
            [jobs.lib.args :as jargs]
            [jobs.lib.trees :as trees]
            [jobs.lib.tidy :as tidy]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.dig-look :as look]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.lib.result :as result]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Roof the body in for the night.
  Declines (waiting) with :day, :already-sealed {:pos} (something solid within :roof-height above; with :enclose, only
  when no open cell is left around the feet),
  or :futile {:pos :why} (a :dig-in-futile entry within 8 blocks that blocks it, see below).
  One call is a whole attempt. It ends with the result {:pos :mode :roof} when the world shows the body shut in, else
  stopped {:reason :text :pos}: the :dig-in-futile reasons below, :no-blocks, :no-tool, :unsealed or :no-progress.
  The mode is the first of these whose cells the zone rules permit. If none is permitted it takes the first
  as a last resort, with one dig-in.trespass-last-resort warning.
  - Blocks it places: building blocks (planks, stone kinds, dirt), then logs as a last resort.
  - plug: the body is in a closed room with a door and a hole in the roof over it. One carried block mends the hole.
  - walls: enough :blocks are carried for every open cell. Places the four sides at feet height, the four at head height,
    a support beside the roof cell, then the roof cell, at most :max-places per step.
    Every step it recomputes the open cells from the current feet cell. If the blocks run out, it chooses again.
  - :enclose: walls mode only, for a body under an overhang or in a cave: the walls it needs, else stopped :no-blocks.
  - dig: digs a pit two deep (three on flat ground, where the start cell has no solid side to roof against),
    then places one block at the roof cell from a carried or dug block.
    It digs only where the block under is solid, the feet and head cells are dry, and no fluid borders the cell
    (water, lava, bubble column, kelp, seagrass, or a waterlogged block), nor the roof cell or the cell above it
    (:fluid-above).
    It holds the best carried tool for each block first. After each dig it looks at what the dig laid open and does
    not walk into the hole when the cell under it is seen not solid (:no-floor; rock it cannot see counts as stone).
    If the body leaves the column, it chooses again.
  With no full block beside the roof cell at either height (a flower or crop is no support) it does not dig
  (dig_in_failed warning, :no-roof-support).
  A wall cell the server refuses is retried after the others, and given up after two refusals.
  A cell occupied by a block a mob walks through (torch, sapling, cobweb) is dug once and placed again.
  A door, gate or trapdoor beside the body counts as a wall only when shut. An open one is shut with one click.
  An iron one cannot be shut by hand and is given up on.
  Fails (dig_in_failed warning) after three failures in a row to place or dig, or at once when:
  a hazard is below, no floor is under the pit, no block can roof the pit, or the carried tools cannot harvest the block below.
  Every end with no roof over the body leaves a :dig-in-futile {:pos :reason} entry,
  and the check declines while one lies within 8 blocks. An entry with no :reason (a lack of blocks or tools)
  is retried once blocks are carried or a carried tool harvests the block.
  Events: dig-in.sealed when it placed blocks and the world shows the body shut in
  (a warning when :resealed, meaning the latest :shelter entry was already at this cell),
  else dig-in.unsealed (warning) naming the cells still open.
  Memory: reads :dig-in-futile. Writes :dig-in-futile (cap 5, ten minutes)
  and, on any end, :shelter {:pos :roof :state :built} (cap 10, one in-game day).
  :roof is the cell it placed above the body, absent if none. Walls mode adds :door, the feet and head cells of one side it placed.
  Pit mode adds :start, the cell dug from. Walls mode at the bottom of a shaft has :start at the shaft's top.
  Plug mode adds :room true. A shelter sealed again where the latest entry stood keeps that entry's :start and :door.
  Leaving is jobs.survival.dig-in-leave/leave!, which jobs.survival.night calls by day.")

(def shelter-blocks
  "What dig-in places: the building blocks, then logs (jobs.lib.trees/log-names; worth more, a last resort)."
  (into lb/building-blocks trees/log-names))

(def args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :type :int :min 0 :default sh/default-roof-height}
   :blocks {:doc "names of the blocks it may place" :default shelter-blocks}
   :max-places {:doc "placements per step" :type :int :min 1 :default 4}
   :on-lava {:doc ":seal: lava the dig lays open in or beside the hole is filled with a carried block; :stop: the dig is given up (:fluid-adjacent)" :type :enum :values [:seal :stop] :default :seal}
   :enclose {:doc "wall in under a roof already overhead: walls only, never a pit; stops :no-blocks when too few blocks are carried" :type :bool :default false}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :type :bool :default false}})

(def shelter-policy {:cap 10 :ttl sh/ms-per-day})

(def futile-policy {:cap 5 :ttl 600000})

(def futile-radius 8)

(defn stop-reason!
  "Note in job memory why this call ends without a roof: the call's stopped result."
  [c reason]
  (ctx/update-mem! c assoc :stop reason))

(defn remember-failed-site! [c reason]
  (stop-reason! c reason)
  (ctx/remember! c :dig-in-futile
                 {:pos (or (:roof (ctx/mem c)) (sh/feet (:primitives c))) :reason reason}
                 futile-policy))

(defn remember-material!
  "A lack of blocks (data {:pos}) or of a tool (data {:pos :needs}): a :dig-in-futile entry retried once that changes."
  [c data]
  (stop-reason! c (if (:needs data) :no-tool :no-blocks))
  (ctx/remember! c :dig-in-futile data futile-policy))

(defn fail-site! [c reason text]
  (let [result (u/fail! c :dig_in_failed text)]
    (when (= :done result) (remember-failed-site! c reason))
    result))

(def max-refusals
  "Times the place primitive may refuse one wall cell before it is given up on."
  2)

(defn cells-to-try
  "The open cells still worth a placement: not occupied, not refused max-refusals times, the least refused first (a cell
  the server refused is retried after the others, which may give it a support)."
  [c open]
  (let [{:keys [occupied refused]} (ctx/mem c)
        tries #(get refused % 0)]
    (->> open
         (remove (or occupied #{}))
         (remove #(>= (tries %) max-refusals))
         (sort-by tries))))

(defn open-text [cells]
  (str "open cells " (pr-str (mapv (juxt :x :y :z) cells))))

(defn ^:async walls-round
  "One round of walls mode. Out of blocks (the carried ones were used up or lost, or the body moved to a cell with more
  open cells than blocks) it forgets the mode so the next round chooses again: a pit dug with what it can harvest."
  [c]
  (let [{:keys [blocks max-places roof-height]} (:args c)
        p (:primitives c)
        open (dig-cells/open-cells p (sh/feet p))
        cells (cells-to-try c open)
        status (await (dig-cells/place-all! c blocks (take max-places cells)))]
    (cond
      (empty? cells) (do (when-not (sh/roofed? p roof-height) (remember-failed-site! c :walls-refused))
                         :done)
      (= "no-item" status) (do (ctx/update-mem! c dissoc :mode :start) :continue)
      (not= :ok status) (fail-site! c :walls-failed (str "cannot place a block: " status "; " (open-text open)))
      (dig-cells/sealed-in? p :walls roof-height) :done
      :else :continue)))

(defn ^:async look-below!
  "Look at the cell to dig when it or one of its sides is unknown, so the checks read what is there."
  [c {:keys [x y z]}]
  (let [cells (cons [x y z] (for [[dx dz] dig-cells/sides] [(+ x dx) y (+ z dz)]))]
    (when (some #(look/unknown? (:primitives c) %) cells)
      (await (look/look-at! c [x y z])))))

(defn ^:async hole-round
  "The cell below the feet is dug and the body is about to walk into it (for the drops or to descend): looks at the cell
  under it. :done (a :no-floor or :fluid-adjacent stop) when that is seen not solid or fluid shows in or beside the
  hole, else nil once the flow delay has passed and the
  cut was looked at again (look/wait-settled!)."
  [c {:keys [x y z]}]
  (let [p (:primitives c)
        under-cell [x (- y 2) z]
        hole {:x x :y (dec y) :z z}
        _ (when (look/unknown? p under-cell) (await (look/look-at! c under-cell)))
        lavas (when (= :seal (:on-lava (:args c))) (seq (dig-cells/lava-around p hole dig-cells/beside-deltas)))
        _ (when lavas
            (await (dig-cells/seal-lava! c (:blocks (:args c)) lavas))
            (await (look/look-at! c [x (dec y) z])))
        under (u/block-name-or p {:x x :y (- y 2) :z z} "stone")]
    (cond
      (not (solid/solid? under))
      (do (remember-failed-site! c :no-floor)
          (ctx/emit! c :dig_in_failed :warn {:text (str under " under the hole; not stepping into it")})
          :done)
      (or (dig-cells/wet? p hole) (dig-cells/lateral-fluid p hole))
      (do (remember-failed-site! c :fluid-adjacent)
          (ctx/emit! c :dig_in_failed :warn {:text (str (or (u/seen-name p hole) "fluid") " in or beside the hole; not stepping into it")})
          :done)
      :else (await (look/wait-settled! c [[x (dec y) z]])))))

(defn ^:async descend-round
  "One step down toward the pit: dig the block below the feet, look at what the dig laid open, collect what it dropped,
  and step into the hole (hole-round). Gives up (dig_in_failed warn, done) rather than dig when the block below is a
  hazard or the cell under it is seen not to be solid (a thin floor over water, lava or air)."
  [c]
  (let [{:keys [blocks]} (:args c)
        p (:primitives c)
        {:keys [x y z]} (sh/feet p)
        below {:x x :y (dec y) :z z}
        _ (await (look-below! c below))
        name (u/seen-name p below)
        under (u/seen-name p {:x x :y (- y 2) :z z})
        fluid (dig-cells/lateral-fluid p below)
        here (first (filter #(dig-cells/wet? p %) [{:x x :y y :z z} {:x x :y (inc y) :z z}]))
        roof (:roof (ctx/mem c))
        over (when roof (first (filter #(dig-cells/wet? p %) [roof (update roof :y inc)])))]
    (cond
      here (do (remember-failed-site! c :fluid-here)
               (ctx/emit! c :dig_in_failed :warn {:text (str (u/seen-name p here) " where the body stands; not digging down")})
               :done)
      fluid (do (remember-failed-site! c :fluid-adjacent)
                (ctx/emit! c :dig_in_failed :warn {:text (str fluid " beside the descent cell; not opening the pit")})
                :done)
      (dig-cells/wet? p below) (do (remember-failed-site! c :hazard-below)
                         (ctx/emit! c :dig_in_failed :warn {:text (str name " below the body; not digging down")})
                         :done)
      over (do (remember-failed-site! c :fluid-above)
               (ctx/emit! c :dig_in_failed :warn {:text (str (u/seen-name p over) " at or above the roof cell; not digging further")})
               :done)
      (and (sh/solid-at? p below) under (not (solid/solid? under)))
      (do (remember-failed-site! c :no-floor)
          (ctx/emit! c :dig_in_failed :warn {:text (str under " under the floor; not digging through it")})
          :done)
      (and (sh/solid-at? p below) (nil? (lb/pick c blocks)) (not (tools/can-harvest? p name)))
      (do (remember-material! c {:pos (:roof (ctx/mem c)) :needs name})
          (ctx/emit! c :dig_in_failed :warn
                     {:text (str "cannot harvest " name " without a "
                                 (tools/harvest-need (map :name (u/inventory p)) (js->clj (.harvestTools p name)))
                                 "; nothing to roof the pit with, so not digging")})
          :done)
      (not (sh/solid-at? p below))
      (or (await (hole-round c (sh/feet p)))
          (let [before (:y (sh/feet p))
                ;; raw moveTo kept: a step into the cell the job is digging, range 0.5, inside its own pit; the planner has no standable goal there.
                r (await (ctx/act c :moveTo (clj->js {:pos below :range 0.5})))]
            (if (< (:y (sh/feet p)) before)
              (do (u/progress! c) :continue)
              (fail-site! c :descent-stalled (str "cannot descend into the pit: " (.-status r))))))
      :else (let [_ (await (tools/equip-for! c name {:fast true}))
                  r (await (tidy/dig! c below true))]
              (if (= "dug" (.-status r))
                (let [placeable (some #(some #{(.-name %)} blocks) (array-seq (.-drops r)))]
                  (u/progress! c)
                  (ctx/update-mem! c assoc :dug-at (ctx/now c))
                  (await (look/see-round! c [x (dec y) z]))
                  (or (await (hole-round c {:x x :y y :z z}))
                      (do (await (dig-cells/collect-drops! c blocks (.-drops r)))
                          (if (or placeable (some? (lb/pick c blocks)))
                            :continue
                            (do (remember-material! c {:pos (:roof (ctx/mem c))})
                                (ctx/emit! c :dig_in_failed :warn {:text "nothing to roof the pit with"})
                                :done)))))
                (fail-site! c :dig-failed (str "cannot dig down: " (.-status r))))))))

(defn ^:async roof-round
  "In the pit: place one block at the cell the body started in."
  [c]
  (let [{:keys [blocks]} (:args c)
        item (lb/pick c blocks)
        roof (:roof (ctx/mem c))]
    (if (nil? item)
      (do (remember-material! c {:pos roof}) :done)
      (let [r (await (tidy/place! c roof item true))]
        (if (#{"placed" "occupied"} (.-status r))
          (do (when (= "placed" (.-status r)) (ctx/update-mem! c update :placed (fnil conj #{}) roof))
              :done)
          (fail-site! c :roof-failed (str "cannot roof the pit: " (.-status r))))))))

(defn mode-choice
  "[mode refusal] for the shelter from start: the first of :plug (only with a room-plug cell), :walls (only when
  walls-ok?) and :dig whose cells are all permitted, else the first of them with its refusal (nil when permitted).
  dig-plan is the pit's {:roof :depth}, nil when it cannot be roofed (the start cell's rules are then checked)."
  [c start walls-cells walls-ok? dig-plan plug]
  (let [in (access/rules-input c)
        {:keys [roof depth]} (or dig-plan {:roof start :depth 2})
        walls-v (some #(access/trespass-refusal in :place %) walls-cells)
        dig-v (or (some #(access/trespass-refusal in :dig %) (map #(update start :y - %) (range 1 (inc depth))))
                  (access/trespass-refusal (assoc in :feet nil) :place roof))
        plug-v (when plug (access/trespass-refusal (assoc in :feet nil) :place plug))
        options (cond-> [] plug (conj [:plug plug-v]) walls-ok? (conj [:walls walls-v]) :always (conj [:dig dig-v]))]
    (or (first (filter (comp nil? second) options)) (first options))))

(defn choose-mode
  "Record in job memory how this shelter is built, once. Chosen again only when the body leaves a dig-mode column.
  :walls stores :mode, and :start at the shaft top when the body is at the bottom of a 1x1 shaft.
  :dig stores :roof, :start and :target-y. :plug stores the one cell to fill.
  See mode-choice for the zone rule."
  [c]
  (let [p (:primitives c)
        {:keys [mode roof]} (ctx/mem c)
        {:keys [x z] :as start} (sh/feet p)
        moved (and (= :dig mode) (not (and (= x (:x roof)) (= z (:z roof)))))]
    (when moved (ctx/update-mem! c dissoc :mode :roof :target-y))
    (when (or moved (not mode))
      (let [cells (dig-cells/open-cells p start)
            have (reduce + (map :count (lb/carried c (:blocks (:args c)))))
            enclose (:enclose (:args c))
            plan (when-not enclose (dig-cells/dig-plan p start))
            plug (when (and (pos? have) (not enclose)) (dig-cells/room-plug p start (:roof-height (:args c))))
            [chosen refusal] (if enclose
                               [(if (>= have (count cells)) :walls :no-blocks)
                                (some #(access/trespass-refusal (access/rules-input c) :place %) cells)]
                               (mode-choice c start cells (>= have (count cells)) plan plug))]
        (access/trespass! c "dig-in" refusal)
        (cond
          (= :no-blocks chosen) (ctx/update-mem! c assoc :mode :no-blocks)
          (= :plug chosen) (ctx/update-mem! c assoc :mode :plug :plug plug)
          (= :walls chosen) (ctx/update-mem! c #(cond-> (assoc % :mode :walls)
                                                  (dig-cells/shaft-top p start) (assoc :start {:x x :y (dig-cells/shaft-top p start) :z z})))
          (nil? plan) (ctx/update-mem! c assoc :mode :no-roof-support)
          :else (ctx/update-mem! c assoc :mode :dig :roof (:roof plan) :start start
                                 :target-y (- (:y start) (:depth plan))))))))

(defn futile-entry-here?
  "Whether any :dig-in-futile entry, whatever its reason, lies within futile-radius of the body."
  [c]
  (let [here (u/self-pos c)]
    (boolean (some #(<= (u/dist here (:pos (:data %))) futile-radius) (ctx/entries c :dig-in-futile)))))

(defn note-unroofed!
  "A dig-in that ends with no roof over the body leaves a :dig-in-futile entry, so a caller that holds the body (the
  night shelter) does not run it again every round: the entry its failure wrote, else {:pos feet :reason :unsealed}."
  [c]
  (when-not (or (sh/roofed? (:primitives c) (:roof-height (:args c))) (futile-entry-here? c))
    (ctx/remember! c :dig-in-futile {:pos (sh/feet (:primitives c)) :reason :unsealed} futile-policy)))

(defn futile-site
  "The :dig-in-futile entry data that blocks digging here, or nil. Material-only failures (no :reason) can be retried
  once blocks are carried, or, when the entry names the block it could not harvest (:needs), once a carried tool
  harvests it; unsafe or inaccessible sites (a :reason) cannot."
  [c]
  (let [here (u/self-pos c)
        have-blocks (seq (lb/carried c (:blocks (:args c))))
        retry? (fn [{:keys [reason needs]}]
                 (and (not reason)
                      (or have-blocks (and needs (tools/can-harvest? (:primitives c) needs)))))]
    (some #(when (and (not (retry? (:data %)))
                      (<= (u/dist here (:pos (:data %))) futile-radius))
             (:data %))
          (ctx/entries c :dig-in-futile))))

(defn check-run
  "Night, no roof over the body and no futile site here; a decline says why (ctx/wait): :day, :already-sealed, or
  :futile with the failed site's :pos and :reason (none: it needs blocks to roof with)."
  [c]
  (let [p (:primitives c)]
    (cond
      (not (sh/night? p)) (ctx/wait c {:reason :day})
      (dig-cells/sealed-in? p (if (:enclose (:args c)) :walls :open) (:roof-height (:args c)))
      (ctx/wait c {:reason :already-sealed :pos (sh/feet p)})
      :else (if-let [site (futile-site c)]
              (ctx/wait c (merge {:reason :futile} (select-keys site [:pos]) (when (:reason site) {:why (:reason site)})))
              true))))

(defn no-roof-round
  "No cell of the pit could be roofed (nothing solid beside the start cell or the ground cell under it): do not dig,
  since the body would be left in an open pit."
  [c]
  (remember-failed-site! c :no-roof-support)
  (ctx/emit! c :dig_in_failed :warn {:text "nothing solid beside the roof cell to place against; not digging a pit"})
  :done)

(defn ^:async plug-round
  "In a closed room: place one block in the hole of the roof over the body (:plug)."
  [c]
  (let [{:keys [blocks]} (:args c)
        item (lb/pick c blocks)
        plug (:plug (ctx/mem c))]
    (if (nil? item)
      (fail-site! c :plug-failed "cannot mend the roof: no-item")
      (let [r (await (tidy/place! c plug item true))]
        (if (#{"placed" "occupied"} (.-status r))
          (do (when (= "placed" (.-status r)) (ctx/update-mem! c update :placed (fnil conj #{}) plug))
              :done)
          (fail-site! c :plug-failed (str "cannot mend the roof: " (.-status r))))))))

(defn ^:async step [c]
  (choose-mode c)
  (let [{:keys [mode target-y]} (ctx/mem c)]
    (cond
      (= :plug mode) (await (plug-round c))
      (= :walls mode) (await (walls-round c))
      (= :no-roof-support mode) (no-roof-round c)
      (= :no-blocks mode) (do (remember-material! c {:pos (sh/feet (:primitives c))}) :done)
      (> (:y (sh/feet (:primitives c))) target-y) (await (descend-round c))
      :else (await (roof-round c)))))

(defn end!
  "The call's end: a :dig-in-futile entry when unroofed, the sealed/unsealed event, the :shelter entry; then the result:
  {:pos :roof :mode} when the world shows the body shut in, else stopped with the reason noted (:unsealed when none)."
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
        prev (:data (ctx/latest c :shelter))
        resealed (= feet (:pos prev))
        start (or start (when resealed (:start prev)))
        door (or door (when resealed (:door prev)))]
    (when (and (seq placed) (not (dig-cells/sealed-in? p judged (:roof-height (:args c)))))
      (let [open (dig-cells/open-cells p feet)]
        (ctx/emit! c :dig-in.unsealed :warn
                   {:pos feet :placed (vec placed) :open open
                    :text (str "NOT sealed in: placed " (count placed) " blocks at " (pr-str (mapv (juxt :x :y :z) placed))
                               ", but the world still shows " (open-text open) (when (empty? open) " (no roof)"))})))
    (when (and (seq placed) (dig-cells/sealed-in? p judged (:roof-height (:args c))))
      (ctx/emit! c :dig-in.sealed (if resealed :warn :info)
                 {:pos feet :placed (vec placed) :resealed resealed
                  :text (str (cond
                               (= :plug mode) "mended the roof of a closed room over the body: "
                               resealed "sealed the shelter again: "
                               :else "sealed in for the night: ")
                             "placed " (count placed) " blocks at " (pr-str (mapv (juxt :x :y :z) placed))
                             ". At night an open shelter is closed again; it is left by day")}))
    (ctx/remember! c :shelter (cond-> {:pos feet :state :built}
                                (contains? placed roof) (assoc :roof roof)
                                door (assoc :door door)
                                start (assoc :start start)
                                (= :plug mode) (assoc :room true))
                   shelter-policy)
    (if (dig-cells/sealed-in? p judged (:roof-height (:args c)))
      (result/finish! c (cond-> {:pos feet :mode mode} (contains? placed roof) (assoc :roof roof)))
      (let [reason (or (:stop (ctx/mem c)) :unsealed)]
        (result/stop! c reason (str "no roof over the body: " (name reason)) :pos feet)))))

(def max-steps
  "Steps one call takes at most (walls or a pit take about ten); past it the call stops :no-progress."
  64)

(defn ^:async round
  "One call is a whole attempt: steps (a placement batch, a dig, a descent, the roof) until the body is roofed or the
  site fails, a timer between steps."
  [c]
  (ctx/update-mem! c dissoc :stop)
  (loop [i 0]
    (let [r (if (< i max-steps) (await (step c)) (do (stop-reason! c :no-progress) :done))]
      (if (= :continue r)
        (do (await (child/pace!)) (recur (inc i)))
        (end! c)))))

(def bad-lists
  "Args checked by jobs.lib.args."
  {:blocks :names})

(defn check
  "check-run once the list args are well formed, else declines :bad-args."
  [c]
  (jargs/guard c bad-lists check-run))
