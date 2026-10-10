(ns jobs.survival.retreat-refuge
  "A cornered body's refuges (jobs.survival.retreat): sealed in with blocks, up a pillar, down a pit or in a side pocket,
  and the hiding in them."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [engine.ctx :as ctx]
            [jobs.lib.pillar :as pl]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.combat :as combat]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.spent :as spent]
            [jobs.lib.threats :as threats]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]))

(def mob-half-width
  "Half the width of a zombie-sized mob's hitbox."
  0.3)

(defn hitbox-cells
  "The feet and head cells a mob standing at pos overlaps (its hitbox is 0.6 wide, so one straddling a cell
  boundary is in both cells: the server refuses a block there)."
  [{:keys [x y z]}]
  (let [span (fn [v] (distinct [(js/Math.floor (- v mob-half-width)) (js/Math.floor (+ v mob-half-width))]))
        fy (js/Math.floor y)]
    (for [cx (span x) cz (span z) cy [fy (inc fy)]] {:x cx :y cy :z cz})))

(defn hostile-cells
  "The cells the hostiles within radius overlap: no block goes there."
  [p radius]
  (set (mapcat #(hitbox-cells (danger-q/mob-pos p %)) (danger-q/known-hostiles p radius {}))))

(defn occupied? [c cell] (contains? (:seal-occupied (ctx/mem c)) cell))

(defn ^:async place-seal!
  "Place carried blocks at cells in order. :ok, :wait while a mob stands in a cell (the place is refused and tried again
  for dig-cells/mob-wait-ms), or :failed at the first placement refused or with nothing left to place. A door, gate or trapdoor standing open is shut instead
  (dig-cells/shut-open!); a cell the place answers occupied (a torch, a chest, a bed: a block the seal leaves alone), or an
  open iron door, is remembered in :seal-occupied and never tried again this flight."
  [c cells]
  (loop [cells cells]
    (let [item (lb/pick c (:blocks (:args c)))
          cell (first cells)]
      (cond
        (empty? cells) :ok
        (dig-cells/sealed? (:primitives c) cell) (recur (rest cells))
        (nil? item) :failed
        :else (let [door (await (dig-cells/shut-open! c cell))
                    r (when-not door (await (tidy/place! c cell item true)))
                    status (some-> r .-status)]
                (cond
                  (or (= :shut door) (= "placed" status))
                  (do (dig-cells/forget-wait! c :seal-mob-since)
                      (recur (rest cells)))
                  (or (= :open door) (= "occupied" status))
                  (do (ctx/update-mem! c update :seal-occupied (fnil conj #{}) cell)
                      (recur (rest cells)))
                  (dig-cells/keep-waiting! c :seal-mob-since r) :wait
                  :else (do (dig-cells/forget-wait! c :seal-mob-since)
                            :failed)))))))

(defn off-centre?
  "Whether the body's hitbox (0.6 wide) reaches out of its cell into a side cell."
  [p]
  (let [{:keys [x z]} (u/self-pos {:primitives p})
        frac #(- % (js/Math.floor %))]
    (boolean (some #(or (< (frac %) 0.3) (> (frac %) 0.7)) [x z]))))

(defn ^:async centre!
  "Step to the middle of the body's own cell, so the side cells around it can take blocks."
  [c]
  (let [{:keys [x y z]} (sh/feet (:primitives c))]
    ;; raw moveTo kept: a step inside the body's own cell, range 0.15; the planner has no goal finer than a cell.
    (await (ctx/act c :moveTo #js {:pos #js {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0.15}))))

(defn shooters
  "Where the seen ranged hostiles within the flight's :ranged-radius stand."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)
        p (:primitives c)]
    (->> (danger-q/known-hostiles p radius {:ranged-radius ranged-radius})
         (filter #(and (combat/ranged? %) (danger-q/seen-only? %)))
         (mapv #(danger-q/mob-pos p %)))))

(defn facing-first
  "cells (dig-cells/fill-cells order) with the sides towards the nearest of shooters first, each side's feet cell then its
  head cell, so the first blocks stop arrows; the roof support and the roof stay last. As given with no shooter
  (seal! then orders by nearest-first of the melee hostiles)."
  [feet shooters cells]
  (if (empty? shooters)
    cells
    (let [side-of (fn [{:keys [x y z]}] (when (<= (:y feet) y (inc (:y feet))) [x z]))
          gap (fn [[x z]] (apply min (map #(u/dist {:x (+ x 0.5) :y (:y feet) :z (+ z 0.5)} %) shooters)))
          sides (->> cells (keep side-of) distinct (sort-by gap))
          rank (zipmap sides (range))]
      (sort-by (fn [{:keys [y] :as cell}]
                 (if-let [r (rank (side-of cell))] [0 r (- y (:y feet))] [1 0 0]))
               cells))))

(defn nearest-first
  "cells (dig-cells/fill-cells order) with the side cells (feet and head height) nearest the nearest of threats ({:x :y :z})
  first; the roof support and the roof keep their order after them (a block needs a neighbour to be placed against).
  As given with no threat."
  [feet threats cells]
  (if (empty? threats)
    cells
    (let [side? (fn [{:keys [y]}] (<= (:y feet) y (inc (:y feet))))
          gap (fn [{:keys [x y z]}]
                (apply min (map #(u/dist {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)} %) threats)))]
      (concat (sort-by gap (filter side? cells)) (remove side? cells)))))

(defn melee-spots
  "Where the seen melee hostiles within the flight's :radius stand."
  [c]
  (let [p (:primitives c)]
    (->> (danger-q/known-hostiles p (:radius (:args c)) {})
         (filter #(and (not (combat/ranged? %)) (danger-q/seen-only? %)))
         (mapv #(danger-q/mob-pos p %)))))

(defn sealing-here?
  "Whether a seal was begun at the body's feet cell this flight and has not failed: it is finished before the walk goes on."
  [c]
  (and (= (:seal-at (ctx/mem c)) (sh/feet (:primitives c))) (not (flight/tried? c :seal))))

(defn ^:async seal!
  "Fill the open cells around the body (dig-in's 1x1: sides at feet and head
  height, a roof support, the roof) with carried :blocks, at most
  :max-places this step. :sealed when none is left open, :again while
  more are owed, :failed when a hostile stands in one, none is carried, a
  placement is refused, or only cells it cannot fill are left open (place-seal!'s :seal-occupied: a torch, chest or bed
  in a side cell, which the seal leaves alone; the option then counts as failed this flight, so a cornered body moves
  on to the next one instead of placing there step after step).
  A door standing open in a side cell is no wall (dig-cells/sealed?): it is shut."
  [c]
  (let [{:keys [radius max-places blocks]} (:args c)
        p (:primitives c)
        open (dig-cells/open-cells p (sh/feet p))
        free (remove #(occupied? c %) open)
        cells (if (seq (shooters c)) (facing-first (sh/feet p) (shooters c) free) (nearest-first (sh/feet p) (melee-spots c) free))
        forget! #(dig-cells/forget-wait! c :seal-mob-since)
        unfillable (fn [] (forget!) (flight/tried! c :seal) :failed)]
    (cond
      (empty? open) (do (forget!) :sealed)
      (empty? cells) (unfillable)
      (some (hostile-cells p radius) cells) (do (forget!) :failed)
      (nil? (lb/pick c blocks)) (do (forget!) :failed)
      :else
      (do (when (off-centre? p) (await (centre! c)))
          (access/trespass! c "retreat" (some #(access/trespass-refusal (access/rules-input c) :place %) cells))
          (ctx/update-mem! c #(-> % (update :seal-cells (fnil into #{}) (map (juxt :x :y :z) (take max-places cells)))
                                  (assoc :seal-at (sh/feet p))))
          (case (await (place-seal! c (take max-places cells)))
            :failed (do (flight/tried! c :seal) :failed)
            :wait :again
            (let [left (dig-cells/open-cells p (sh/feet p))]
              (cond
                (empty? left) (do (forget!) :sealed)
                (every? #(occupied? c %) left) (unfillable)
                :else :again)))))))

(declare hide-now!)

(defn ^:async hide!
  "Seal the body in and hide there (a :seal refuge): :again while sealing or once sealed, nil when it cannot seal."
  [c]
  (case (await (seal! c))
    :failed nil
    :again :again
    :sealed (let [feet (sh/feet (:primitives c))]
              (hide-now! c {:kind :seal :anchor feet :cells (vec (:seal-cells (ctx/mem c)))}
                         "cornered: sealed in with blocks until the hostile leaves"))))

(a/defargs settings
  {::pillar-height {:default 3 :doc "Height of the pillar built as a refuge, in blocks." :spec (a/int-in 0 nil)}})
(defn pillar-height [] (settings/get settings ::pillar-height))

(defn pillar-item
  "The carried :blocks block with the most, when it is at least pillar-height; else nil."
  [c]
  (->> (lb/carried c (:blocks (:args c)))
       (filter #(>= (:count %) (pillar-height)))
       (sort-by :count >)
       first
       :name))

(defn up [[x y z] n] [x (+ y n) z])

(defn pillar-ok?
  "Whether a pillar-height pillar fits here: a solid floor, and the head cell and every cell the body rises into free."
  [block-at feet]
  (and (rules/solid-floor? block-at (up feet -1))
       (every? #(pl/clear? (block-at (up feet %))) (range 1 (+ 2 (pillar-height))))))

(defn cell-map [[x y z]] {:x x :y y :z z})

(declare refuge-round!)

(defn ^:async start-refuge!
  "Note refuge (a map with :kind, :anchor the feet cell it starts from, :cells [x y z] the cells it fills) in job
  memory and run its first step."
  [c refuge]
  (ctx/update-mem! c assoc :refuge refuge)
  (await (refuge-round! c)))

(defn ^:async pillar!
  "Start a pillar-height pillar (jobs.access.pillar, ledgered) when it fits: a step's result, else nil. The cells it
  rises into that the body has not sensed are looked at first, so open sky is not taken for rock."
  [c]
  (let [p (:primitives c)
        feet (pl/feet-cell c)
        item (pillar-item c)]
    (when item
      (doseq [cell (map #(up feet %) (range 1 (+ 2 (pillar-height))))]
        (when (look/unknown? p cell) (await (look/look-at! c cell)))))
    (when (and item (pillar-ok? (escape/block-at-of p) feet))
      (access/trespass! c "retreat" (some #(access/trespass-refusal (access/rules-input c) :place (cell-map (up feet %)))
                                          (range (pillar-height))))
      (await (start-refuge! c {:kind :pillar :item item :anchor (cell-map feet)
                               :cells (mapv #(up feet %) (range (pillar-height)))})))))

(def drop-of
  "The block a dug block drops, where it is another (an unlisted one drops itself)."
  {"grass_block" "dirt" "stone" "cobblestone" "deepslate" "cobbled_deepslate" "podzol" "dirt" "mycelium" "dirt"})

(defn side-open?
  "Whether the side column at feet height (cell and the one over it) is seen open, dry and free of hostiles: a way to
  look down the zigzag pit's next column and step into it."
  [c cell]
  (let [p (:primitives c)
        mobs (hostile-cells p (:radius (:args c) 0))]
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
        blocks (:blocks (:args c))
        fits? (fn [{:keys [steps prep]}]
                (let [cells (mapcat :dig steps)
                      names (map #(dig-cells/rock-name p %) cells)]
                  (and (dug-cells-ok? p (concat prep cells))
                       (dig-cells/rock-solid? p (:floor (peek steps)))
                       (or (lb/pick c blocks) (some (set blocks) (map #(get drop-of % %) names))))))
        prepped (fn [shape]
                  (let [side (update (:to (first (:steps shape))) :y inc)]
                    (assoc shape :prep [side (update side :y inc)])))
        mobs (hostile-cells p (:radius (:args c) 0))
        shapes (dig-cells/pit-shapes p feet #(side-open? c %))
        shape (or (first (filter fits? shapes))
                  (->> (dig-cells/pit-shapes p feet #(not (mobs %)))
                       (filter (comp seq :plugs))
                       (map prepped)
                       (filter fits?)
                       first))]
    (when shape
      (assoc shape :target-y (:y (:to (peek (:steps shape))))))))

(defn cell-vec [{:keys [x y z]}] [x y z])

(defn ^:async pit!
  "Start digging down and plugging (pit-plan) when it can: a step's result, else nil."
  [c]
  (let [feet (sh/feet (:primitives c))]
    (when-let [{:keys [steps plugs roof prep] :as plan} (pit-plan c feet)]
      (let [in (access/rules-input c)
            digs (concat prep (mapcat :dig steps))]
        (access/trespass! c "retreat" (or (some #(access/trespass-refusal in :dig %) digs)
                                          (some #(access/trespass-refusal (assoc in :feet nil) :place %) (conj plugs roof))))
        (ctx/update-mem! c dissoc :pit-drops)
        (await (start-refuge! c (assoc plan :kind :pit :anchor feet
                                       :cells (mapv cell-vec (distinct (concat digs plugs [roof]))))))))))

(defn pocket-cells
  "The two cells [feet head] of a side pocket dug from feet towards [dx dz], or nil: both solid, harvestable with what is
  carried, with no hazard or fluid in or beside them, solid all round them except the way in (floor, roof and the three
  far sides), and a dug block that can seal the way in (carried or dropped)."
  [c feet [dx dz]]
  (let [p (:primitives c)
        blocks (:blocks (:args c))
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
        {:keys [on-lava blocks]} (:args c)]
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
      (flight/tried! c :pocket)
      (loop [[cell & more] cells]
        (if (nil? cell)
          (do (ctx/update-mem! c update :tried disj :seal)
              ;; raw moveTo kept: a step into the body's own pocket, as the pit's drop; the planner has no standable goal there.
              (await (ctx/act c :moveTo (clj->js {:pos (first cells) :range 0.5})))
              :again)
          (let [_ (await (tools/equip-for! c (dig-cells/rock-name p cell) {:fast true}))
                r (await (tidy/dig! c cell true))]
            (when (= "dug" (.-status r))
              (ctx/update-mem! c assoc :dug-at (ctx/now c))
              (await (look/see-round! c [(:x cell) (:y cell) (:z cell)]))
              (await (look/wait-settled! c [[(:x cell) (:y cell) (:z cell)]]))
              (await (dig-cells/collect-drops! c (:blocks (:args c)) (.-drops r)))
              (when-not (await (lays-open? c (take (- (count cells) (count more)) cells) (into (set cells) [feet (update feet :y inc)]))) (recur more)))))))))

;; ------------------------------------------------------------------ up a pillar or down a pit

(defn ^:async abandon-refuge!
  "The refuge failed: forget it, never try that option again this flight."
  [c]
  (let [kind (:kind (:refuge (ctx/mem c)))]
    (ctx/update-mem! c dissoc :refuge)
    (flight/tried! c kind)
    :again))

(defn refuge-danger?
  "Whether a hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius), the dead skipped,
  would have a walkable way to the refuge's anchor cell were the refuge's own cells open (jobs.lib.danger): a danger
  the refuge keeps off."
  [c {:keys [anchor cells]}]
  (let [p (:primitives c)
        open (set cells)
        {:keys [ranged-radius]} (:args c)
        reach-of (fn [e] (cond-> (threats/follow-range (.-name e)) (combat/ranged? e) (max ranged-radius)))
        known (flight/near-known p (set (flight/dead-ids c)) threats/max-follow-range ranged-radius)]
    (boolean (some #(and (<= (danger-q/mob-distance p %) (reach-of %))
                         (reach/walkable-way? p (danger-q/mob-pos p %) anchor #{} open))
                   known))))

(defn kept-off-text
  "The hostiles the refuge still keeps off, as \"zombie at [x y z]\", for the hand-back message."
  [c {:keys [anchor cells]}]
  (let [p (:primitives c)
        known (flight/near-known p (set (flight/dead-ids c)) threats/max-follow-range (:ranged-radius (:args c)))]
    (->> known
         (filter #(reach/walkable-way? p (danger-q/mob-pos p %) anchor #{} (set cells)))
         (map #(str (.-name %) " at " (pr-str (mapv (fn [k] (js/Math.floor (k (danger-q/mob-pos p %)))) [:x :y :z]))))
         (str/join ", "))))

(defn ^:async hide-hold!
  "Sealed in, up the pillar or down the pit: hold (a :wait, why hiding) while the refuge keeps a danger off
  (refuge-danger?) and :quiet-s after the last one, then end the flight :hidden. Past :hide-s in the refuge with no real
  danger on the body as it stands (danger-q/danger-near?, the hostile-near trigger's test) it ends :hidden at once,
  saying which hostile it still keeps off (info retreat.hide-ended): the trigger fires again should one reach it."
  [c]
  (let [now (ctx/now c)]
    (when-not (:quiet-from (ctx/mem c)) (ctx/update-mem! c assoc :quiet-from now))
    (when-not (:hidden-since (ctx/mem c)) (ctx/update-mem! c assoc :hidden-since now)))
  (loop []
    (when (refuge-danger? c (:refuge (ctx/mem c))) (ctx/update-mem! c assoc :quiet-from (ctx/now c)))
    (let [{:keys [radius ranged-radius quiet-s hide-s]} (:args c)
          {:keys [quiet-from hidden-since refuge]} (ctx/mem c)
          quiet? (>= (- (ctx/now c) quiet-from) (* 1000 quiet-s))
          spent? (and (>= (- (ctx/now c) hidden-since) (* 1000 hide-s))
                      (not (danger-q/danger-near? (:primitives c) radius ranged-radius)))]
      (cond
        quiet? (do (ctx/update-mem! c dissoc :quiet-from :hidden-since)
                   (flight/end-flight! c :hidden))
        spent? (let [held (js/Math.round (/ (- (ctx/now c) hidden-since) 1000))
                     kept (kept-off-text c refuge)]
                 (ctx/emit! c :retreat.hide-ended :info
                            {:ended :hidden :held-s held :pos (sh/feet (:primitives c))
                             :text (str "hid " held " s; no hostile can reach or shoot the body here"
                                        (when (seq kept) (str ", the refuge still keeps off " kept)) ": handing back")})
                 (ctx/update-mem! c dissoc :quiet-from :hidden-since)
                 (flight/end-flight! c :hidden))
        :else (do (await (ctx/act c :wait #js {:ms (flight/wait-ms) :why "hiding"}))
                  (await (pace/pace!))
                  (recur))))))

(defn hide-now! [c refuge text]
  (ctx/update-mem! c assoc :refuge (assoc refuge :hidden true))
  (let [placed (spent/log c)]
    (ctx/emit! c :retreat_sealed :warn (merge {:text (str text (some->> (spent/text placed "refuge") (str "; "))) :pos (sh/feet (:primitives c))}
                                              (when (seq placed) (spent/summary placed)))))
  :again)

(defn ^:async pillar-round!
  "One block of the pillar (the child jobs.access.pillar); hidden once it is at least 2 high."
  [c {:keys [item] :as refuge}]
  (let [r (await (ctx/call-child c :pillar 'jobs.access.pillar {:height (pillar-height) :item item :ignore-zones? true}))]
    (case r
      :declined (await (abandon-refuge! c))
      :done (if (>= (:built (ctx/child-result c :pillar) 0) 2)
              (hide-now! c refuge "cornered: pillared up out of reach until the hostile leaves")
              (await (abandon-refuge! c)))
      :again)))

(defn ^:async plug!
  "Place a carried block at the pit's first open plug cell (the side beside the head, then the roof over it), ledgered
  as :retreat-plug; hidden once all are sealed."
  [c {:keys [plugs roof] :as refuge}]
  (let [p (:primitives c)
        item (lb/pick c (:blocks (:args c)))
        cell (first (remove #(dig-cells/sealed? p %) (conj plugs roof)))]
    (cond
      (nil? cell) (hide-now! c refuge "cornered: dug down and plugged the hole until the hostile leaves")
      (nil? item) (await (abandon-refuge! c))
      :else
      (let [l (ledger/intend (ledger/open-entries (ctx/view c))
                             {:cell (cell-vec cell) :item item :before (u/seen-name p cell) :job (:id c) :purpose :retreat-plug})
            _ (ledger/remember! c l)
            r (await (tidy/place! c cell item true))]
        (ledger/remember! c (ledger/reconcile l (escape/block-at-of p)))
        (cond
          (= "placed" (.-status r)) (do (dig-cells/forget-wait! c :seal-mob-since) :again)
          (dig-cells/keep-waiting! c :seal-mob-since r) :again
          :else (do (dig-cells/forget-wait! c :seal-mob-since)
                    (await (abandon-refuge! c))))))))

(defn ^:async dig-pit-cell!
  "Dig one cell of the pit (its placeable drops noted for dig-cells/collect-pit-drops!), look at what it laid open and let it
  settle: :again, or the refuge abandoned when the dig fails, lays open fluid or a cave (lava is sealed first,
  lays-open?), or the cell fills again."
  [c cell keep]
  (let [p (:primitives c)
        v (cell-vec cell)
        blocks (set (:blocks (:args c)))
        _ (await (tools/equip-for! c (dig-cells/rock-name p cell) {:fast true}))
        r (await (tidy/dig! c cell true))]
    (if (not= "dug" (.-status r))
      (await (abandon-refuge! c))
      (do (ctx/update-mem! c assoc :dug-at (ctx/now c))
          (ctx/update-mem! c update :pit-drops (fnil into [])
                           (for [d (array-seq (.-drops r)) :when (blocks (.-name d))] {:id (.-id d) :cell cell}))
          (await (look/see-round! c v))
          (await (look/wait-settled! c [v]))
          (if (or (await (lays-open? c [cell] keep)) (not (dig-cells/open-cell? p cell)))
            (await (abandon-refuge! c))
            :again)))))

(defn ^:async pit-round!
  "One step of the pit (pit-plan): dig the next drop's cells, then drop into it once its floor is seen solid, or at the
  bottom plug. The body never digs the cell under its feet unless it knows the floor under that, nor drops onto a floor
  it has not seen solid: the refuge is abandoned instead."
  [c {:keys [anchor steps plugs roof prep] :as refuge}]
  (let [p (:primitives c)
        feet (sh/feet p)
        at (first (keep-indexed #(when (= feet %2) %1) (cons anchor (map :to steps))))
        keep (into #{anchor (update anchor :y inc) roof}
                   (concat plugs prep (mapcat :dig steps) (mapcat (fn [{:keys [to]}] [to (update to :y inc)]) steps)))]
    (when at (await (dig-cells/collect-pit-drops! c feet)))
    (cond
      (nil? at) (await (abandon-refuge! c))
      (= at (count steps)) (await (plug! c refuge))
      :else
      (let [{:keys [dig to floor]} (nth steps at)
            cell (first (remove #(dig-cells/open-cell? p %) (if (zero? at) (concat prep dig) dig)))
            under? (= cell (update feet :y dec))]
        (when (and cell (look/unknown? p (cell-vec cell))) (await (look/look-at! c (cell-vec cell))))
        (cond
          (and cell (not (dig-cells/rock-solid? p cell))) (await (abandon-refuge! c))
          (and under? (not (dig-cells/known-solid? p floor))) (await (abandon-refuge! c))
          cell (await (dig-pit-cell! c cell keep))
          :else
          (do (when (look/unknown? p (cell-vec floor)) (await (look/look-at! c (cell-vec floor))))
              (if (dig-cells/known-solid? p floor)
                ;; raw moveTo kept: a drop into the body's own pit, as dig-in's descent; the planner has no standable goal there.
                (do (await (ctx/act c :moveTo (clj->js {:pos to :range 0.5})))
                    (if (= to (sh/feet p)) :again (await (abandon-refuge! c))))
                (await (abandon-refuge! c)))))))))

(defn ^:async refuge-round!
  "A step in a refuge: building it (pillar or pit), or hidden in it (to the end of the flight)."
  [c]
  (let [{:keys [kind hidden] :as refuge} (:refuge (ctx/mem c))]
    (cond
      hidden (await (hide-hold! c))
      (= :pillar kind) (await (pillar-round! c refuge))
      :else (await (pit-round! c refuge)))))
