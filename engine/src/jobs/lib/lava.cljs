(ns jobs.lib.lava
  "Lava a dig laid open: which seen lava cells border a dug cell, sealing one with a building block from the rim (a
  jobs.blocks.place child), and stepping off when it cannot be sealed. Shared by jobs.blocks.dig and jobs.access.stair.
  Cells are [x y z]."
  (:require [jobs.lib.choose :as choose]
            [jobs.lib.need :as need]
            [jobs.lib.child :as child]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.aim :as aim]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.reach :as reach]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]))

(defn add [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])

(defn seen-lava? [p [x y z]] (= "lava" (u/seen-name p {:x x :y y :z z})))

(defn exposed
  "The seen lava cells among the six neighbours of the dug cell. The dug cell itself is the caller's work and is left."
  [p cell]
  (filterv #(seen-lava? p %) (map #(add cell %) rules/neighbour-deltas)))

(defn face-aim
  "The point just inside neighbour n of the dug cell on the face towards it (d is the step from the cell to n)."
  [n d]
  (mapv #(+ %1 0.5 (* -0.45 %2)) n d))

(defn ^:async look-round!
  "Look at each unsensed neighbour of the dug cell, the cell below first, with a sight pass after each look
  (jobs.lib.sense), so lava the dig laid open is seen before exposed reads. The look aims at the neighbour's face on
  the dug cell: from the top of a 1-wide shaft the line to its centre runs through the rock over it."
  [c cell]
  (let [p (:primitives c)]
    (loop [[d & more] (cons [0 -1 0] rules/neighbour-deltas) looks 0]
      (when (and d (< looks dig-look/max-floor-looks))
        (let [n (add cell d)]
          (if (dig-look/unknown? p n)
            (do (await (sense/look-at! c n (face-aim n d)))
                (recur more (inc looks)))
            (recur more looks)))))))

(def peek-height "Blocks above a cell's column a peek stance may stand." 3)

(defn peek-stance
  "The cell to stand on to see the neighbours of the dug cell still unseen, or nil when the body sees as many from where
  it stands: of the cells within step-off reach of the column over the dug cell, the first with the most clear lines
  from its eye to the faces on the dug cell."
  [p feet ok? cell unknown-ds]
  (let [kind-at (aim/seen-kind-at p)
        targets (map (fn [d] (face-aim (add cell d) d)) unknown-ds)
        clear-count (fn [[ex ey ez]] (count (filter #(reach/ray-clear? kind-at [ex ey ez] %) targets)))
        here (clear-count [(:x feet) (+ (:y feet) 1.62) (:z feet)])
        stands (distinct (mapcat #(step-off/candidates p {:x (first cell) :y (+ (second cell) %) :z (nth cell 2)}
                                                       {:avoid #{cell} :ok? ok?})
                                 (range 0 (inc peek-height))))
        scored (map (fn [{:keys [x y z] :as st}] [(clear-count [(+ x 0.5) (+ y 1.62) (+ z 0.5)]) st]) stands)
        best (when (seq scored) (apply max (map first scored)))]
    (when (and best (> best here))
      (some (fn [[n st]] (when (= n best) st)) scored))))

(defn ^:async peek!
  "After a dig, when a neighbour of the dug cell is still unseen (the body dug it from a spot whose line to that face
  runs through rock), walk to a standing cell that sees the faces on the dug cell and look again, once per cell.
  :continue while the walk goes on, else nil. Memory :peek-to is the stance in hand, :peeked the cell done."
  [c cell]
  (let [p (:primitives c)
        {:keys [peek-to peeked]} (ctx/mem c)
        unknown-ds (filterv #(dig-look/unknown? p (add cell %)) rules/neighbour-deltas)
        to (or peek-to (when (and (seq unknown-ds) (not= cell peeked)) (peek-stance p (u/self-pos c) (step-off/zone-ok (access/rules-input c)) cell unknown-ds)))]
    (when to
      (ctx/update-mem! c assoc :peek-to to)
      (let [e (await (child/call-walk* c :peek {:pos to :range 0 :ways #{:walk}}))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c #(-> % (dissoc :peek-to) (assoc :peeked cell)))
              (await (look-round! c cell))
              nil))))))

(def max-seals "Places per lava cell (a seal and one reseal) before the seal counts as failed." 2)

(defn ^:async seal!
  "Fill lava cell with a building block (a jobs.blocks.place child in slot :place, fetching one as opts :fetch (the old switch) allows,
  zones as opts :ignore-zones?). Memory :seals counts places per cell, :sealing the cell in hand. :again once placed
  (event kind, info), :continue while the child waits on the world, else the stop {:reason :lava-unsealed :cell}
  with the place outcome (:place; :need when no block is carried) or :tries."
  [c kind block-at cell {:keys [fetch ignore-zones?]}]
  (let [tries (get-in (ctx/mem c) [:seals cell] 0)
        [x y z] cell]
    (if (>= tries max-seals)
      {:reason :lava-unsealed :cell cell :tries tries}
      (do
        ;; a waiting child is resumed on the same cell: only its first round counts a try
        (ctx/update-mem! c #(cond-> (assoc % :sealing cell)
                              (not= cell (:sealing %)) (update-in [:seals cell] (fnil inc 0))))
        (let [outcome (await (blocks/place-cell! c {:x x :y y :z z} nil
                                                 {:use {:blocks (choose/filler-names c :build)} :ways (need/ways-of-fetch fetch)
                                                  :ignore-zones? (boolean ignore-zones?)}))]
          (if (= :continue outcome)
            :continue
            (do (ctx/update-mem! c dissoc :sealing)
                (if (#{:placed :already} outcome)
                  (do (ctx/emit! c kind :info {:cell cell :now (block-at cell) :text (str "sealed lava at " (pr-str cell))})
                      :again)
                  {:reason :lava-unsealed :cell cell :place outcome}))))))))

(defn near-cells
  "The cells [x y z] a body should not stand on beside open lava: the columns of cells and their side neighbours,
  from one below the lowest to two above the highest."
  [cells]
  (let [ys (map second cells)]
    (set (for [[x _ z] cells dx [-1 0 1] dz [-1 0 1] y (range (dec (apply min ys)) (+ 3 (apply max ys)))]
           [(+ x dx) y (+ z dz)]))))

(def away-tries "Cells step-away! walks to, one after another, before it gives up; the last walk escalates." 3)

(defn ^:async step-away!
  "Walk off the cells beside the open lava and the dug cell (jobs.lib.step-off candidates, zones obeyed, up to 3
  blocks): the nearest free cell first, then the next, up to away-tries cells; the last walk tried (the last
  candidate or the away-tries-th) escalates, so a lone cell gets an escalated walk at once. Memory :away-to is the
  cell in hand (:away-escalate its walk's escalation), :away-tried the cells given up on. :arrived (also when the
  body already stands clear), :continue, {:unreachable :no-cell} (no cell to go to) or {:unreachable
  :all-walks-failed :walk why} (cells were tried; why is the last go-to's)."
  [c dug lavas]
  (let [avoid (near-cells (cons dug lavas))
        clear! (fn [] (ctx/update-mem! c dissoc :away-to :away-escalate :away-tried :away-why))]
    (loop []
      (let [{:keys [x y z]} (u/self-pos c)
            feet [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]
            feet-cell (zipmap [:x :y :z] feet)]
        (if-not (contains? avoid feet)
          (do (clear!) :arrived)
          (let [{:keys [away-to away-escalate away-tried away-why] :or {away-tried #{}}} (ctx/mem c)
                _ (when-not away-to
                    (await (dig-look/look-unknown! c (step-off/floors feet-cell 3))))
                left (when-not away-to
                       (remove #(contains? away-tried ((juxt :x :y :z) %))
                               (step-off/candidates (:primitives c) feet-cell
                                                    {:avoid avoid :reach 3 :ok? (step-off/zone-ok (access/rules-input c))})))
                to (or away-to (first left))
                escalate (if away-to
                           away-escalate
                           (or (= (inc (count away-tried)) away-tries) (empty? (rest left))))]
            (if-not (and to (< (count away-tried) away-tries))
              (do (clear!)
                  (if (seq away-tried) {:unreachable :all-walks-failed :walk away-why} {:unreachable :no-cell}))
              (let [_ (ctx/update-mem! c assoc :away-to to :away-escalate escalate)
                    e (await (child/call-walk* c :step-off (cond-> {:pos to :range 0} (not escalate) (assoc :ways #{:walk :clear-seal}))))
                    res (:data e)]
                (cond
                  (= :continue e) :continue
                  (and (= :done (:kind e)) (:arrived res)) (do (clear!) :arrived)
                  :else (do (ctx/update-mem! c #(-> % (dissoc :away-to :away-escalate)
                                                    (assoc :away-why (or (:why res) (contract/cause-key e)))
                                                    (update :away-tried (fnil conj #{}) ((juxt :x :y :z) to))))
                            (recur)))))))))))
