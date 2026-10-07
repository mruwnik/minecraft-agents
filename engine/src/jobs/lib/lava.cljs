(ns jobs.lib.lava
  "Lava a dig laid open: which seen lava cells border a dug cell, sealing one with a building block from the rim (a
  jobs.blocks.place child), and stepping off when it cannot be sealed. Shared by jobs.blocks.dig and jobs.access.stair.
  Cells are [x y z]."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.util :as u]))

(defn add [[x y z] [dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)])

(defn seen-lava? [p [x y z]] (= "lava" (u/seen-name p {:x x :y y :z z})))

(defn exposed
  "The seen lava cells among the six neighbours of the dug cell. The dug cell itself is the caller's work and is left."
  [p cell]
  (filterv #(seen-lava? p %) (map #(add cell %) rules/neighbour-deltas)))

(def max-seals "Places per lava cell (a seal and one reseal) before the seal counts as failed." 2)

(defn ^:async seal!
  "Fill lava cell with a building block (a jobs.blocks.place child in slot :place, fetching one as opts :fetch allows,
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
                                                 {:any-of blocks/building-blocks :fetch fetch
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

(def away-tries "Cells step-away! walks to, one after another, before it gives up; the last walk may escalate." 3)

(defn ^:async step-away!
  "Walk off the cells beside the open lava and the dug cell (jobs.lib.step-off candidates, zones obeyed, up to 3
  blocks): the nearest free cell first, then the next, up to away-tries cells, the last walk with go-to escalation.
  Memory :away-to is the cell in hand, :away-tried the cells given up on. :arrived (also when the body already
  stands clear), :continue, or {:unreachable why}."
  [c dug lavas]
  (let [avoid (near-cells (cons dug lavas))
        {:keys [x y z]} (u/self-pos c)
        feet [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]
        feet-cell (zipmap [:x :y :z] feet)]
    (if-not (contains? avoid feet)
      :arrived
      (loop []
        (let [{:keys [away-to away-tried] :or {away-tried #{}}} (ctx/mem c)
              _ (when-not away-to
                  (await (dig-look/look-unknown! c (step-off/floors feet-cell 3))))
              to (or away-to
                     (first (remove #(contains? away-tried ((juxt :x :y :z) %))
                                    (step-off/candidates (:primitives c) feet-cell
                                                         {:avoid avoid :reach 3 :ok? (step-off/zone-ok (access/rules-input c))}))))]
          (if-not (and to (< (count away-tried) away-tries))
            (do (ctx/update-mem! c dissoc :away-to :away-tried)
                {:unreachable (if to :unreachable :no-cell)})
            (let [r (await (ctx/call-child c :step-off 'jobs.movement.go-to
                                           {:pos to :range 0 :escalate (= (inc (count away-tried)) away-tries)}))
                  res (ctx/child-result c :step-off)]
              (ctx/update-mem! c assoc :away-to to)
              (cond
                (= :continue r) :continue
                (and (= :done r) (:arrived res)) (do (ctx/update-mem! c dissoc :away-to :away-tried) :arrived)
                :else (do (ctx/update-mem! c #(-> % (dissoc :away-to) (update :away-tried (fnil conj #{}) ((juxt :x :y :z) to))))
                          (recur))))))))))
