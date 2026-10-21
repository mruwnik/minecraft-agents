(ns jobs.lib.spot
  "A free cell beside the body to put a block in (a crafting table, a furnace): found, or dug."
  (:require [jobs.lib.block-sets :as bs]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.waits :as waits]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as b]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(def table-offsets [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 1] [1 -1] [-1 -1]])

(defn solid? [n] (and n (not (bs/air n)) (not (bs/fluids n)) (not (b/clearable n))))

(defn free? [n] (boolean (and n (or (bs/air n) (b/clearable n)))))

(defn home-cell?
  "Whether a table in pos would spoil a room: on a bed, or in the cell beside a door (the way in and out)."
  [p {:keys [x y z] :as pos}]
  (let [name-at #(or (u/seen-name p %) "")
        ends? (fn [n suffix] (str/ends-with? n suffix))]
    (or (ends? (name-at (update pos :y dec)) "_bed")
        (some (fn [[dx dz]] (some #(ends? (name-at {:x (+ x dx) :y (+ y %) :z (+ z dz)}) "_door") [0 1]))
              [[1 0] [-1 0] [0 1] [0 -1]]))))

(defn spot-cells
  "The candidate table cells beside the body, outside its box (the server refuses a place the body touches), not in :avoid, not in a plan's cells, not over a bed or beside a door (home-cell?) and not in a stair or tunnel this body cut (its way out): [pos floor-pos] for each offset at its level, then (a 1-wide stair has
  no free cell at its level) each one level up. dig-spot-cells keeps to the level ones."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)
        cols (access/body-columns x z)
        avoid (into (ledger/own-cells c) (map (comp (juxt :x :y :z) a/cell)) (:avoid (:args c)))
        plan-cells (known/footprints c {:except (:for-plan (:args c))})
        [x y z] (mapv #(js/Math.floor %) [x y z])]
    (for [dy [0 1]
          [dx dz] table-offsets
          :let [pos {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
          :when (and (not (cols [(:x pos) (:z pos)])) (not (avoid [(:x pos) (:y pos) (:z pos)]))
                     (not (contains? plan-cells [(:x pos) (:y pos) (:z pos)]))
                     (not (home-cell? (:primitives c) pos)))]
      [pos (update pos :y dec)])))

(defn table-spot
  "A free cell beside the body to put a table in: air or a plant or snow layer (placing digs it out) at the body's level with a solid block under it, or nil. Where
  fewer than three of the four cells beside the body are open (a tunnel, a pocket), a cell that leads on to more open
  air is a passage and is not taken: it would shut the way along the corridor."
  [c]
  (let [p (:primitives c)
        {:keys [x y z]} (u/self-pos c)
        [x y z] (mapv #(js/Math.floor %) [x y z])
        sides [[1 0] [-1 0] [0 1] [0 -1]]
        open? (fn [cx cz] (free? (u/seen-name p {:x cx :y y :z cz})))
        cramped? (< (count (filter (fn [[dx dz]] (open? (+ x dx) (+ z dz))) sides)) 3)
        passage? (fn [{px :x pz :z}]
                   (some (fn [[dx dz]] (and (not= [(+ px dx) (+ pz dz)] [x z]) (open? (+ px dx) (+ pz dz)))) sides))]
    (some (fn [[pos floor]]
            (when (and (free? (u/seen-name p pos)) (solid? (u/seen-name p floor))
                       (not (and cramped? (= y (:y pos)) (= 1 (+ (js/Math.abs (- x (:x pos))) (js/Math.abs (- z (:z pos)))))
                                 (passage? pos))))
              pos))
          (spot-cells c))))

(defn ^:async find-spot!
  "table-spot, and when the cells beside the body are not all seen: look at each unseen floor cell once (per standing
  cell), then read again."
  [c]
  (or (table-spot c)
      (let [here (look/cell-of (u/self-pos c))]
        (when-not (= here (:looked-spot (ctx/mem c)))
          (ctx/update-mem! c assoc :looked-spot here)
          (doseq [[pos floor] (spot-cells c)
                  :when (or (nil? (u/seen-name (:primitives c) pos)) (nil? (u/seen-name (:primitives c) floor)))]
            (await (ctx/act c :look (clj->js {:pos {:x (+ (:x floor) 0.5) :y (+ (:y floor) 0.5) :z (+ (:z floor) 0.5)}})))
            (look/see! c))
          (table-spot c)))))

(defn dig-spot-cells
  "With no free cell beside the body (a shut-in pocket): the cells beside it at its level that would be a table spot
  once dug, quickest to break first: seen natural terrain (jobs.lib.rules/natural?, never a build) on a solid floor
  or one not seen yet (rock hides it), not tried yet (memory :spot-dug)."
  [c]
  (let [p (:primitives c)
        tried (:spot-dug (ctx/mem c) #{})]
    (->> (spot-cells c)
         (filter (fn [[pos _]] (= (:y pos) (js/Math.floor (:y (u/self-pos c))))))
         (keep (fn [[pos floor]]
                 (let [n (u/seen-name p pos)
                       f (u/seen-name p floor)]
                   (when (and (rules/natural? n) (solid? n) (or (nil? f) (solid? f)) (not (tried pos)))
                     pos))))
         (sort-by #(tools/clear-ms p (u/seen-name p %))))))

(defn ^:async dig-spot!
  "Make a table spot where there is none: dig the first dig-spot-cells cell (a jobs.blocks.dig child, bare-handed when
  no carried tool harvests it; the drop is not kept; a cell beside water or lava is passed over, :on-fluid :fail), then
  look at its floor. A cell that could not be dug goes to the next. The spot, :continue while the child waits, or nil
  when no cell can be dug."
  [c]
  (loop []
    (when-let [cell (or (:spot-digging (ctx/mem c)) (first (dig-spot-cells c)))]
      (ctx/update-mem! c assoc :spot-digging cell)
      (let [r (waits/waiting-out c (await (ctx/call c :spot-dig 'jobs.blocks.dig {:pos cell :collect false :need-drop false :ways #{:carried} :on-fluid :fail :for-plan (:for-plan (:args c))})))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c #(-> % (dissoc :spot-digging) (update :spot-dug (fnil conj #{}) cell)))
              (if (and (= :done (:kind r)) (:dug (:data r)))
                (do (await (ctx/act c :look (clj->js {:pos {:x (+ (:x cell) 0.5) :y (- (:y cell) 0.5) :z (+ (:z cell) 0.5)}})))
                    (look/see! c)
                    (when (and (bs/air (u/seen-name (:primitives c) cell)) (solid? (u/seen-name (:primitives c) (update cell :y dec))))
                      cell))
                (recur))))))))

(defn ^:async spot!
  "A cell beside the body to put a table in: a free one (find-spot!), else one dug (dig-spot!). :continue while the dig
  waits, nil when there is none."
  [c]
  (or (await (find-spot! c)) (await (dig-spot! c))))

(defn ^:async place-beside!
  "Put the carried block item down in a cell beside the body (spot!) with a jobs.blocks.place child (never fetching). The
  cell is kept at spot-path in the job's memory until the child ends. :continue while a child waits, {:placed cell}, or
  {:why text}. opts: :args more place args; :wrap (fn [call] promise) runs the child call inside something else."
  [c item spot-path {:keys [args wrap]}]
  (let [spot (or (get-in (ctx/mem c) spot-path) (await (spot! c)))]
    (if-not spot
      {:cause :no-spot :why "no free cell beside the body"}
      (if (= :continue spot)
        :continue
        (do (ctx/update-mem! c assoc-in spot-path spot)
            (let [call #(ctx/call c :place 'jobs.blocks.place (merge {:item item :pos spot :ways #{:carried}} args))
                  e (await (if wrap (wrap call) (call)))]
              (if (= :continue e)
                :continue
                (do (ctx/update-mem! c (fn [m] (if (next spot-path) (update-in m (pop spot-path) dissoc (peek spot-path)) (dissoc m (first spot-path)))))
                    (if (and (= :done (:kind e)) (get-in e [:data :placed]))
                      {:placed spot}
                      {:cause (contract/cause-key e)
                       :why (str (or (get-in e [:data :why]) (some-> (contract/cause-key e) name) "it was not placed"))})))))))))
