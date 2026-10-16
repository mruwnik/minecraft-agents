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
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]))

(def drop-of
  "The block a dug block drops, where it is another (an unlisted one drops itself)."
  {"grass_block" "dirt" "stone" "cobblestone" "deepslate" "cobbled_deepslate" "podzol" "dirt" "mycelium" "dirt"})

(defn side-open?
  "Whether the side column at feet height (cell and the one over it) is seen open, dry and free of hostiles: a way to
  look down the zigzag pit's next column and step into it."
  [c cell]
  (let [p (:primitives c)
        mobs (seal/hostile-cells p (:radius (:args c) 0))]
    (every? #(and (dig-cells/open-cell? p %) (not (mobs %))) [cell (update cell :y inc)])))

(defn dug-cells-ok?
  "Whether cells are all solid rock to dig (rock the body has not looked into reads stone, dig-cells/rock-name) with no
  hazard in or fluid beside them, harvestable with what is carried; names are their block names."
  [p cells]
  (let [names (map #(dig-cells/rock-name p %) cells)]
    (and (every? #(dig-cells/rock-solid? p %) cells)
         (not-any? dig-cells/hazards names)
         (not-any? #(dig-cells/lateral-fluid p %) cells)
         (every? #(tools/can-harvest? p %) names))))

(defn pit-plan
  "A pit to dig down from feet and plug over the head (dig-cells/pit-shapes): {:steps :plugs :roof :prep :target-y}, or nil:
  every cell to dig solid, no fluid in or beside it, harvestable with what is carried, solid under the bottom, and a block
  to plug with carried or dug. With no side column open the zigzag takes a rock side: :prep is its two cells at feet and
  head height, dug first."
  [c feet]
  (let [p (:primitives c)
        blocks (flight/blocks c)
        fits? (fn [{:keys [steps prep]}]
                (let [cells (mapcat :dig steps)
                      names (map #(dig-cells/rock-name p %) cells)]
                  (and (dug-cells-ok? p (concat prep cells))
                       (dig-cells/rock-solid? p (:floor (peek steps)))
                       (or (lb/pick c blocks) (some (set blocks) (map #(get drop-of % %) names))))))
        prepped (fn [shape]
                  (let [side (update (:to (first (:steps shape))) :y inc)]
                    (assoc shape :prep [side (update side :y inc)])))
        mobs (seal/hostile-cells p (:radius (:args c) 0))
        shapes (dig-cells/pit-shapes p feet #(side-open? c %))
        shape (or (first (filter fits? shapes))
                  (->> (dig-cells/pit-shapes p feet #(not (mobs %)))
                       (filter (comp seq :plugs))
                       (map prepped)
                       (filter fits?)
                       first))]
    (when shape
      (assoc shape :target-y (:y (:to (peek (:steps shape))))))))

(defn ^:async plan!
  "The pit refuge (pit-plan with :kind :pit, :anchor the feet cell and :cells every cell it digs or fills) when one
  fits here, else nil."
  [c]
  (let [feet (sh/feet (:primitives c))]
    (when-let [{:keys [steps plugs roof prep] :as plan} (pit-plan c feet)]
      (let [in (access/rules-input c)
            digs (concat prep (mapcat :dig steps))]
        (access/trespass! c "retreat" (or (some #(access/trespass-refusal in :dig %) digs)
                                          (some #(access/trespass-refusal (assoc in :feet nil) :place %) (conj plugs roof))))
        (ctx/update-mem! c dissoc :pit-drops)
        (assoc plan :kind :pit :anchor feet :cells (mapv lb/cell (distinct (concat digs plugs [roof]))))))))

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
  "After a dig of the last of cells (those dug so far): whether what the digs laid open rules the refuge out, a fluid or a
  seen open cell outside keep. Lava is first filled with a carried block (:on-lava :seal, the default); any fluid still
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
    (boolean (some #(or (fluid-shown? p %) (open-shell p % keep)) cells))))

(defn ^:async pocket!
  "Dig a side pocket (feet and head cell beside the body, closed on every other side), collecting the blocks, and step
  into it: :again, so the next step seals the way in with them. nil when no side fits or a dig fails."
  [c]
  (let [p (:primitives c)
        feet (sh/feet p)]
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

(defn ^:async plug!
  "Place a carried block at the pit's first open plug cell (the side beside the head, then the roof over it), ledgered
  as flight/plug-purpose: :again, :hidden once all are sealed, :failed when none is carried or the place is refused."
  [c {:keys [plugs roof]}]
  (let [p (:primitives c)
        item (lb/pick c (flight/blocks c))
        cell (first (remove #(dig-cells/sealed? p %) (conj plugs roof)))]
    (cond
      (nil? cell) :hidden
      (nil? item) :failed
      :else
      (let [l (ledger/intend (ledger/open-entries (ctx/view c))
                             {:cell (lb/cell cell) :item item :before (u/seen-name p cell) :job (:id c) :purpose (flight/plug-purpose c)})
            _ (ledger/remember! c l)
            r (await (tidy/place! c cell item true))]
        (ledger/remember! c (ledger/reconcile l (escape/block-at-of p)))
        (cond
          (= "placed" (.-status r)) (do (dig-cells/forget-wait! c :seal-mob-since) (flight/note-placed! c cell item) :again)
          (dig-cells/keep-waiting! c :seal-mob-since r) :again
          :else (do (dig-cells/forget-wait! c :seal-mob-since)
                    :failed))))))

(defn ^:async dig-pit-cell!
  "Dig one cell of the pit (its placeable drops noted for dig-cells/collect-pit-drops!), look at what it laid open and let it
  settle: :again, or :failed when the dig fails, lays open fluid or a cave (lava is sealed first,
  lays-open?), or the cell fills again."
  [c cell keep]
  (let [p (:primitives c)
        v (lb/cell cell)
        blocks (set (flight/blocks c))
        _ (await (tools/equip-for! c (dig-cells/rock-name p cell) {:fast true}))
        r (await (tidy/dig! c cell true))]
    (if (not= "dug" (.-status r))
      :failed
      (do (ctx/update-mem! c assoc :dug-at (ctx/now c))
          (ctx/update-mem! c update :pit-drops (fnil into [])
                           (for [d (array-seq (.-drops r)) :when (blocks (.-name d))] {:id (.-id d) :cell cell}))
          (await (look/see-round! c v))
          (await (look/wait-settled! c [v]))
          (if (or (await (lays-open? c [cell] keep)) (not (dig-cells/open-cell? p cell)))
            :failed
            :again)))))

(defn ^:async pit-round!
  "One step of the pit (pit-plan): dig the next drop's cells, then drop into it once its floor is seen solid, or at the
  bottom plug. The body never digs the cell under its feet unless it knows the floor under that, nor drops onto a floor
  it has not seen solid: :failed instead. :again, or plug!'s result."
  [c {:keys [anchor steps plugs roof prep] :as refuge}]
  (let [p (:primitives c)
        feet (sh/feet p)
        at (first (keep-indexed #(when (= feet %2) %1) (cons anchor (map :to steps))))
        keep (into #{anchor (update anchor :y inc) roof}
                   (concat plugs prep (mapcat :dig steps) (mapcat (fn [{:keys [to]}] [to (update to :y inc)]) steps)))]
    (when at (await (dig-cells/collect-pit-drops! c feet)))
    (cond
      (nil? at) :failed
      (= at (count steps)) (await (plug! c refuge))
      :else
      (let [{:keys [dig to floor]} (nth steps at)
            cell (first (remove #(dig-cells/open-cell? p %) (if (zero? at) (concat prep dig) dig)))
            under? (= cell (update feet :y dec))]
        (when (and cell (look/unknown? p (lb/cell cell))) (await (look/look-at! c (lb/cell cell))))
        (cond
          (and cell (not (dig-cells/rock-solid? p cell))) :failed
          (and under? (not (dig-cells/known-solid? p floor))) :failed
          cell (await (dig-pit-cell! c cell keep))
          :else
          (do (when (look/unknown? p (lb/cell floor)) (await (look/look-at! c (lb/cell floor))))
              (if (dig-cells/known-solid? p floor)
                ;; raw moveTo kept: a drop into the body's own pit, as dig-in's descent; the planner has no standable goal there.
                (do (await (ctx/act c :moveTo (clj->js {:pos to :range 0.5})))
                    (if (= to (sh/feet p)) :again :failed))
                :failed)))))))
