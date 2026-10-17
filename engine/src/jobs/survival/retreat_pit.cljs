(ns jobs.survival.retreat-pit
  "A cornered body's pit and side pocket (jobs.survival.retreat): where one fits, digging it and plugging it."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as lb]
            [jobs.lib.dig-look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.seal :as seal]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]))

(def drop-of
  "The block a dug block drops, where it is another (an unlisted one drops itself)."
  {"grass_block" "dirt" "stone" "cobblestone" "deepslate" "cobbled_deepslate" "podzol" "dirt" "mycelium" "dirt"})

(def default-radius
  "Blocks within which a melee hostile stands in the way of a pit cell, where the call has no :radius."
  8)

(defn hostiles
  "The cells the melee hostiles within the call's :radius overlap."
  [c]
  (seal/hostile-cells (:primitives c) (:radius (:args c) default-radius)))

(defn side-open?
  "Whether the side column at feet height (cell and the one over it) is seen open, dry and free of hostiles: a way to
  look down the zigzag pit's next column and step into it."
  [c cell]
  (let [p (:primitives c)
        mobs (hostiles c)]
    (every? #(and (dig-cells/open-cell? p %) (not (mobs %))) [cell (update cell :y inc)])))

(defn stop
  "A pit's stop: why it cannot go on (a dig-in stop reason), with the text to say and, for :no-tool, the block it cannot harvest."
  ([reason text] {:stop reason :text text})
  ([reason text needs] {:stop reason :text text :needs needs}))

(defn here-stop
  "The stop for fluid where the body stands (feet or head), nil when dry."
  [p feet]
  (when-let [here (first (filter #(dig-cells/wet? p %) [feet (update feet :y inc)]))]
    (stop :fluid-here (str (u/seen-name p here) " where the body stands; not digging down"))))

(defn over-stop
  "The stop for fluid at or above the roof cell, nil when dry."
  [p roof]
  (when-let [over (first (filter #(dig-cells/wet? p %) [roof (update roof :y inc)]))]
    (stop :fluid-above (str (u/seen-name p over) " at or above the roof cell; not digging further"))))

(defn cell-stop
  "The stop for a cell to dig that is beside fluid, wet, a hazard or open air, nil when it is solid rock to dig."
  [p cell]
  (let [name (dig-cells/rock-name p cell)
        beside (dig-cells/lateral-fluid p cell)]
    (cond
      beside (stop :fluid-adjacent (str beside " beside the cell to dig; not opening the pit"))
      (or (dig-cells/wet? p cell) (dig-cells/hazards name)) (stop :hazard-below (str name " in the cell to dig; not digging"))
      (not (dig-cells/rock-solid? p cell)) (stop :no-floor "open air where the pit would go; not digging"))))

(defn dug-cells-ok?
  "Whether cells are all solid rock to dig (rock the body has not looked into reads stone, dig-cells/rock-name) with no
  hazard in or fluid beside them."
  [p cells]
  (every? #(nil? (cell-stop p %)) cells))

(defn material-stop
  "Whether the body can roof the pit with what it has and digs, cell by cell in dig order: a stop (stop) when a cell needs a
  tool it lacks with no block to roof with yet or that the hand breaks slower than the call's :hand-clear-ms (default tools/quick-clear-ms) (:no-tool, naming the block), or none is
  carried or dropped by the end (:no-blocks); nil otherwise. A cell it cannot harvest is still dug by hand once a block is in hand."
  [c blocks cells]
  (let [p (:primitives c)]
    (loop [[cell & more] cells have (boolean (lb/pick c blocks))]
      (if (nil? cell)
        (when-not have (stop :no-blocks "nothing to roof the pit with"))
        (let [name (dig-cells/rock-name p cell)
              ok (tools/can-harvest? p name)]
          (if (and (not ok) (or (not have) (> (tools/clear-ms p name) (or (:hand-clear-ms (:args c)) (tools/quick-clear-ms)))))
            (stop :no-tool (str "cannot harvest " name (if have " in good time" "; nothing to roof the pit with") ", so not digging") name)
            (recur more (or have (and ok (boolean (some #{(get drop-of name name)} blocks)))))))))))

(defn candidates
  "The pits the shapes allow from feet (dig-cells/pit-shapes) with a side column open and free of hostiles; with none, the
  zigzags that take a rock side: :prep is its two cells at feet and head height, dug first."
  [c feet]
  (let [p (:primitives c)
        prepped (fn [shape]
                  (let [side (update (:to (first (:steps shape))) :y inc)]
                    (assoc shape :prep [side (update side :y inc)])))
        mobs (hostiles c)]
    (concat (dig-cells/pit-shapes p feet #(side-open? c %))
            (->> (dig-cells/pit-shapes p feet #(not (mobs %)))
                 (filter (comp seq :plugs))
                 (map prepped)))))

(defn pit-plan
  "A pit to dig down from feet and plug over the head (candidates) with blocks (default the call's, flight/blocks):
  {:steps :plugs :roof :prep}, or nil: no fluid where the body stands or at the roof, every cell to dig solid, no
  fluid in or beside it, solid under the bottom, and material-stop none."
  ([c feet] (pit-plan c feet (flight/blocks c)))
  ([c feet blocks]
  (let [p (:primitives c)
        fits? (fn [{:keys [steps prep roof]}]
                (let [cells (concat prep (mapcat :dig steps))]
                  (and (dug-cells-ok? p cells)
                       (nil? (over-stop p roof))
                       (dig-cells/rock-solid? p (:floor (peek steps)))
                       (nil? (material-stop c blocks cells)))))
        shape (when-not (here-stop p feet)
                (first (filter fits? (candidates c feet))))]
    shape)))

(defn refused-column
  "The straight pit down from feet when the body sees a hazard in the first cell to dig or not solid under it: kept, so
  the stop names it (a thin floor, water) rather than a zigzag hiding that. A bad cell deeper down leaves the pit to the
  zigzag, which never digs under the feet onto a floor it has not seen. nil otherwise."
  [p feet {:keys [depth]}]
  (let [shape (dig-cells/pit-steps feet nil depth)
        bad? (fn [cell] (let [n (u/seen-name p cell)] (and n (or (not (solid/solid? n)) (dig-cells/wet? p cell)))))]
    (when (some bad? (let [{:keys [dig floor]} (first (:steps shape))] (conj dig floor)))
      shape)))

(defn shape-stop
  "The first thing that rules shape out, step by step as the digs would meet it, as a stop, nil when none does."
  [c blocks {:keys [steps roof prep]}]
  (let [p (:primitives c)]
    (or (some (fn [{:keys [dig floor]}]
                (or (some #(cell-stop p %) dig)
                    (when-not (dig-cells/rock-solid? p floor)
                      (stop :no-floor (str (or (u/seen-name p floor) "an unseen cell") " under the hole; not stepping into it")))))
              steps)
        (over-stop p roof)
        (material-stop c blocks (concat prep (mapcat :dig steps))))))

(defn stop-of
  "Why no pit fits at feet: a stop. Water where the body stands, a pit that fits only with no hostile in the way (:hostile-near), no roof to place against (:no-roof-support), no pit whose
  floor the body can see solid (:no-floor), else what rules out the first pit the shapes allow (shape-stop)."
  [c blocks feet]
  (let [p (:primitives c)
        plan (dig-cells/dig-plan p feet)
        shape (or (when plan (refused-column p feet plan)) (first (candidates c feet)))
        mobless (pit-plan (assoc-in c [:args :radius] 0) feet blocks)]
    (cond
      (here-stop p feet) (here-stop p feet)
      mobless (stop :hostile-near "a hostile stands in the way of the pit; not digging down")
      (nil? shape) (if plan
                     (stop :no-floor "no floor the body sees solid under a pit; not digging down")
                     (stop :no-roof-support "nothing solid beside the roof cell to place against; not digging a pit"))
      :else (or (shape-stop c blocks shape) (stop :no-floor "no pit fits here")))))

(defn ^:async plan-of!
  "The pit that fits at the feet (pit-plan with blocks, default the call's, after a look at the roof's surroundings and at the
  side columns it has not sensed), else the stop of why none does (stop-of; it has :stop)."
  ([c] (plan-of! c (flight/blocks c)))
  ([c blocks]
   (let [p (:primitives c)
         feet (sh-sense/feet p)
         _ (await (sense/decide! c ::roof-support #(dig-cells/dig-plan p feet)))
         plan #(pit-plan c feet blocks)]
     (or (plan)
         (do (await (look/look-unknown! c (for [[dx dz] dig-cells/sides, dy [0 1]]
                                            [(+ (:x feet) dx) (+ (:y feet) dy) (+ (:z feet) dz)])))
             (plan))
         (stop-of c blocks feet)))))

(defn plan-cells
  "Every cell the pit plan digs (the prep cells and each drop's) and fills."
  [{:keys [steps plugs roof prep]}]
  (mapv lb/cell (distinct (concat prep (mapcat :dig steps) plugs [roof]))))

(defn ^:async plan!
  "plan-of!'s pit as the refuge (:kind :pit, :anchor the feet cell, :cells every cell it digs or fills), checked against the zone
  rules (a last resort when a cell is another's); a stop when none fits."
  [c]
  (let [plan (await (plan-of! c))]
    (if (:stop plan)
      plan
      (let [in (access/rules-input c)
            digs (concat (:prep plan) (mapcat :dig (:steps plan)))]
        (access/trespass! c "retreat" (or (some #(access/trespass-refusal in :dig %) digs)
                                          (some #(access/trespass-refusal (assoc in :feet nil) :place %) (conj (:plugs plan) (:roof plan)))))
        (ctx/update-mem! c dissoc :pit-drops)
        (assoc plan :kind :pit :anchor (sh-sense/feet (:primitives c)) :cells (plan-cells plan))))))

(defn pocket-cells
  "The two cells [feet head] of a side pocket dug from feet towards [dx dz], or nil: both solid, harvestable with what is
  carried, with no hazard or fluid in or beside them, solid all round them except the way in (floor, roof and the three
  far sides), and a dug block that can seal the way in (carried or dropped)."
  [c feet [dx dz]]
  (let [p (:primitives c)
        blocks (flight/blocks c)
        at (fn [dy] {:x (+ (:x feet) dx) :y (+ (:y feet) dy) :z (+ (:z feet) dz)})
        [lo hi] [(at 0) (at 1)]
        walls (concat [(at -1) (at 2)]
                      (for [c [lo hi] [sx sz] dig-cells/sides :when (not= [sx sz] [(- dx) (- dz)])]
                        (assoc c :x (+ (:x c) sx) :z (+ (:z c) sz))))
        names (map #(dig-cells/rock-name p %) [lo hi])]
    (when (and (every? #(dig-cells/rock-solid? p %) (concat [lo hi] walls))
               (not-any? dig-cells/hazards names)
               (not-any? #(dig-cells/lateral-fluid p %) [lo hi])
               (every? #(tools/can-harvest? p %) names)
               (or (lb/pick c blocks) (some (set blocks) (map #(get drop-of % %) names))))
      [lo hi])))

(defn fluid-shown?
  "Whether the body sees fluid in a dug cell or beside, over or under it."
  [p cell]
  (boolean (or (dig-cells/wet? p cell) (dig-cells/lateral-fluid p cell)
               (dig-cells/wet? p (update cell :y inc)) (dig-cells/wet? p (update cell :y dec)))))

(defn open-shell
  "The first cell beside, over or under the dug cell, outside keep (the refuge's own cells and the way in), that the body
  sees not solid: the dig laid open a cave or a gap."
  [p cell keep]
  (first (for [[dx dy dz] rules/neighbour-deltas
               :let [n (assoc cell :x (+ (:x cell) dx) :y (+ (:y cell) dy) :z (+ (:z cell) dz))]
               :when (and (not (keep n)) (some-> (u/seen-name p n) (as-> nm (not (solid/solid? nm)))))]
           n)))

(defn ^:async lays-open?
  "After a dig of the last of cells (those dug so far): why what the digs laid open rules the refuge out, :fluid-adjacent
  for fluid or :lays-open for a seen open cell outside keep, else nil. Lava is first filled with a carried block (:on-lava :seal, the default); any fluid still
  shown stops it."
  [c cells keep]
  (let [p (:primitives c)
        on-lava (:on-lava (:args c))
        blocks (flight/blocks c)]
    (when (= :seal on-lava)
      (loop [[cell & more] cells]
        (when cell
          (when-let [lavas (seq (dig-cells/lava-around p cell dig-cells/around-deltas))]
            (await (dig-cells/seal-lava! c blocks lavas))
            (await (look/look-at! c [(:x cell) (:y cell) (:z cell)])))
          (recur more))))
    (cond (some #(fluid-shown? p %) cells) :fluid-adjacent
          (some #(open-shell p % keep) cells) :lays-open)))

(defn ^:async pocket!
  "Dig a side pocket (feet and head cell beside the body, closed on every other side), collecting the blocks, and step
  into it: :again, so the next step seals the way in with them. nil when no side fits or a dig fails."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)]
    (when-let [cells (some #(pocket-cells c feet %) dig-cells/sides)]
      (let [in (access/rules-input c)]
        (access/trespass! c "retreat" (some #(access/trespass-refusal in :dig %) cells)))
      (loop [[cell & more] cells]
        (if (nil? cell)
          (do ;; raw moveTo kept: a step into the body's own pocket, as the pit's drop; the planner has no standable goal there.
              (await (ctx/act c :moveTo (clj->js {:pos (first cells) :range 0.5})))
              :again)
          (let [_ (await (tools/equip-for! c (dig-cells/rock-name p cell) {:fast true}))
                r (await (tidy/dig! c cell true))]
            (when (= "dug" (.-status r))
              (ctx/update-mem! c assoc :dug-at (ctx/now c))
              (await (look/see-round! c [(:x cell) (:y cell) (:z cell)]))
              (await (look/wait-settled! c [[(:x cell) (:y cell) (:z cell)]]))
              (await (dig-cells/collect-drops! c (flight/blocks c) (.-drops r)))
              (when-not (await (lays-open? c (take (- (count cells) (count more)) cells) (into (set cells) [feet (update feet :y inc)]))) (recur more)))))))))

(defn pit-index
  "Which drop of the pit the feet are at (0 at the anchor), nil when the body is not in its column."
  [{:keys [anchor steps]} feet]
  (first (keep-indexed #(when (= feet %2) %1) (cons anchor (map :to steps)))))

(defn retry-or-stop
  "A failed dig or descent counts against the failures in a row (u/progress! resets): :again until u/max-failures, then the stop."
  [c reason text]
  (if (u/count-fail! c) (stop reason text) :again))

(def max-refills
  "Times a dug cell may fill again (sand, gravel, a sealed lava cell) before the pit is given up."
  3)

(def max-occupied
  "Times a plug cell may answer occupied by a block that does not seal before the pit is given up."
  2)

(defn ^:async plug!
  "Place a carried block at the pit's first open plug cell (the side beside the head, then the roof over it), ledgered
  as flight/plug-purpose: :again, :hidden once all are sealed, else a stop (no block carried, a cell occupied by a block that does not
  seal, or the place refused)."
  [c {:keys [plugs roof]}]
  (let [p (:primitives c)
        item (lb/pick c (flight/blocks c))
        cell (first (remove #(dig-cells/sealed? p %) (conj plugs roof)))]
    (cond
      (nil? cell) :hidden
      (nil? item) (stop :no-blocks "nothing to roof the pit with")
      :else
      (let [l (ledger/intend (ledger/open-entries (ctx/view c))
                             {:cell (lb/cell cell) :item item :before (u/seen-name p cell) :job (:id c) :purpose (flight/plug-purpose c)})
            _ (ledger/remember! c l)
            r (await (tidy/place! c cell item true))]
        (ledger/remember! c (ledger/reconcile l (escape/block-at-of p)))
        (cond
          (= "placed" (.-status r)) (do (dig-cells/forget-wait! c :seal-mob-since) (u/progress! c) (flight/note-placed! c cell item) :again)
          (and (= "occupied" (.-status r)) (dig-cells/sealed? p cell)) :again
          (and (= "occupied" (.-status r)) (not= cell roof))
          (do (ctx/update-mem! c update :plug-occupied (fnil inc 0))
              (if (< (:plug-occupied (ctx/mem c)) max-occupied)
                :again
                (stop :roof-failed (str "cannot seal the pit: the plug cell is occupied by " (or (u/seen-name p cell) "a block") " that does not seal"))))
          (dig-cells/keep-waiting! c :seal-mob-since r) :again
          :else (do (dig-cells/forget-wait! c :seal-mob-since)
                    (retry-or-stop c :roof-failed (str "cannot roof the pit: " (.-status r) (dig-cells/mob-text r)))))))))

(defn ^:async dig-pit-cell!
  "Dig one cell of the pit (its placeable drops noted for dig-cells/collect-pit-drops!), look at what it laid open and let it
  settle: :again, or a stop when the dig fails, lays open fluid or a cave (lava is sealed first, lays-open?), or the cell fills again."
  [c cell keep]
  (let [p (:primitives c)
        v (lb/cell cell)
        blocks (set (flight/blocks c))
        _ (await (tools/equip-for! c (dig-cells/rock-name p cell) {:fast true}))
        r (await (tidy/dig! c cell true))]
    (if (not= "dug" (.-status r))
      (retry-or-stop c :dig-failed (str "cannot dig down: " (.-status r)))
      (do (u/progress! c)
          (ctx/update-mem! c assoc :dug-at (ctx/now c))
          (ctx/update-mem! c update :pit-drops (fnil into [])
                           (for [d (array-seq (.-drops r)) :when (blocks (.-name d))] {:id (.-id d) :cell cell}))
          (await (look/see-round! c v))
          (await (look/wait-settled! c [v]))
          (let [opened (await (lays-open? c [cell] keep))
                fluid (first (filter #(dig-cells/wet? p %) [cell (update cell :y inc) (update cell :y dec)]))]
            (cond
              (= :fluid-adjacent opened) (stop :fluid-adjacent (str (or (some->> fluid (u/seen-name p)) (dig-cells/lateral-fluid p cell) "fluid") " in or beside the dug cell; not going on"))
              opened (let [n (open-shell p cell keep)] (stop opened (str (u/seen-name p n) " at " (pr-str (lb/cell n)) " laid open by the dig; not going on")))
              (not (dig-cells/open-cell? p cell))
              (do (ctx/update-mem! c update :refills (fnil inc 0))
                  (if (< (:refills (ctx/mem c)) max-refills)
                    :again
                    (stop :dig-failed "the dug cell fills again")))
              :else :again))))))

(defn ^:async look-cell!
  "Look at the cell to dig when it or one of its sides is unknown, so the checks read what is there."
  [c {:keys [x y z]}]
  (let [cells (cons [x y z] (for [[dx dz] dig-cells/sides] [(+ x dx) y (+ z dz)]))]
    (when (some #(look/unknown? (:primitives c) %) cells)
      (await (look/look-at! c [x y z])))))

(defn ^:async pit-round!
  "One step of the pit (pit-plan): dig the next drop's cells, then drop into it once its floor is seen solid, or at the
  bottom plug. The body never digs where it sees fluid, a hazard or open air, never digs the cell under its feet unless it
  knows the floor under that, nor drops onto a floor it has not seen solid: a stop instead (stop). :again, :hidden, or plug!'s result."
  [c {:keys [anchor steps plugs roof prep] :as refuge}]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        at (pit-index refuge feet)
        keep (into #{anchor (update anchor :y inc) roof}
                   (concat plugs prep (mapcat :dig steps) (mapcat (fn [{:keys [to]}] [to (update to :y inc)]) steps)))]
    (when at (await (dig-cells/collect-pit-drops! c feet)))
    (cond
      (nil? at) (stop :left-pit "the body left the pit's cells")
      (= at (count steps)) (await (plug! c refuge))
      (here-stop p feet) (here-stop p feet)
      (over-stop p roof) (over-stop p roof)
      :else
      (let [{:keys [dig to floor]} (nth steps at)
            cell (first (remove #(dig-cells/open-cell? p %) (if (zero? at) (concat prep dig) dig)))
            under? (= cell (update feet :y dec))
            _ (when cell (await (look-cell! c cell)))
            material (when cell (material-stop c (flight/blocks c) (remove #(dig-cells/open-cell? p %) (concat prep (mapcat :dig steps)))))]
        (cond
          (and cell (cell-stop p cell)) (cell-stop p cell)
          (and under? (not (dig-cells/known-solid? p floor))) (stop :no-floor (str (or (u/seen-name p floor) "an unseen cell") " under the hole; not digging through it"))
          (and cell material) material
          cell (await (dig-pit-cell! c cell keep))
          :else
          (do (when (look/unknown? p (lb/cell floor)) (await (look/look-at! c (lb/cell floor))))
              (if-not (dig-cells/known-solid? p floor)
                (stop :no-floor (str (or (u/seen-name p floor) "an unseen cell") " under the hole; not stepping into it"))
                ;; raw moveTo kept: a drop into the body's own pit; the planner has no standable goal there.
                (let [r (await (ctx/act c :moveTo (clj->js {:pos to :range 0.5})))]
                  (if (= to (sh-sense/feet p))
                    (do (u/progress! c) (ctx/update-mem! c dissoc :refills) :again)
                    (retry-or-stop c :descent-stalled (str "cannot descend into the pit: " (.-status r))))))))))))
