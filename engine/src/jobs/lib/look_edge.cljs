(ns jobs.lib.look-edge
  "Look first, judge after: an unseen cell is open until looked at (a player walks up and looks). The unseen sides of the
  closed region the body stands in under a cover, and the unseen cells of a roof column, are looked at, nearest first, and
  walked towards when a look cannot show them. Memory keys :edge-looked, :edge-phase, :edge-steps are the calling job's."
  (:require [jobs.lib.cost.health :as cost-health]
            [engine.ctx :as ctx]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.look :as look]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shore :as shore]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def max-edge-looks
  "Most cells of a closed region's unseen edge walked to in a night after a look failed to show them, nearest first."
  15)

(def max-edge-steps "Most looks and walks of the edge in a night." 200)

(def edge-stride
  "How far past the nearest unseen cell one walk of the edge may reach, in blocks: the ground nearest the farthest unseen cell
  within this of the nearest, so a long tunnel is seen in a few strides."
  6)

(def max-edge-walk
  "Farthest, in blocks from the feet, a walk to look at an edge may go: a player judging a room does not cross a cave system
  the dig opened into, which the closed-region flood then covers."
  24)

(defn lit-side?
  "A predicate on an unseen side cell [x y z]: within near-of of the feet (a dark wall that close shows), or some flood cell beside
  it is bright enough to be seen, so a look or a walk can show it. A side of a dark stretch farther off is never shown (a dark
  cell is not recorded), so the region stays open there."
  [p feet]
  (let [see-lit (look/see-lit-fn p)
        near (look/near-of p)
        cells (set (map vec (:cells (reach-room/room-flood p (reach-room/room-kind-at p (reach-room/shut-kind-at p)) (:x feet) (:y feet) (:z feet)))))]
    (fn [[x y z]]
      (or (nil? see-lit)
          (and near (<= (u/dist feet {:x x :y y :z z}) near))
          (some (fn [[dx dz]] (and (contains? cells [(+ x dx) y (+ z dz)]) (== 1 (see-lit (+ x dx) y (+ z dz)))))
                [[1 0] [-1 0] [0 1] [0 -1]])))))

(defn unseen-roof?
  "Whether a cell in the roof column over the {:x :y :z} cell (2..6 above it) is unseen with no seen block under it: a roof
  hidden from afar, which a walk up to the room shows."
  [p {:keys [x y z]}]
  (loop [dy 2]
    (cond
      (> dy 6) false
      (sh-sense/solid-at? p {:x x :y (+ y dy) :z z}) false
      (look/unseen? p [{:x x :y (+ y dy) :z z}]) true
      :else (recur (inc dy)))))

(defn covered?
  "Whether the body is under a roof, leaves or earth, or has a cell unseen over its head where a roof may be: only then is
  the region round it a shelter to be judged."
  [p]
  (or (sh-sense/roofed? p (sh-sense/room-roof-height)) (sh-sense/leaf-roofed? p (sh-sense/room-roof-height)) (sh-sense/sealed? p)
      (unseen-roof? p (sh-sense/feet p))))

(defn pending
  "The cells [x y z] to look at before the body's region is judged: unseen cells beside the closed region the body stands in
  under a cover, nearest first, not looked at in c's job memory and within the caps. A dark cell stays unseen after a look and
  is not looked at twice. Reads no memory of a frame without a job: pass `looked` and `steps` then."
  ([c] (pending c (:edge-looked (ctx/mem c) #{}) (:edge-steps (ctx/mem c) 0)))
  ([c looked steps]
   (let [p (:primitives c)
         feet (sh-sense/feet p)]
     (when (and (< (count looked) max-edge-looks) (< steps max-edge-steps) (covered? p))
       (->> (reach-room/unseen-sides p feet)
            (remove looked)
            (filter (lit-side? p feet))
            (sort-by (fn [[x y z]] (u/dist feet {:x x :y y :z z})))
            vec)))))

(defn walkable-near-fn
  "walkable-near as a fn of the cell [x y z], over the closed region round `region` ({:x :y :z}) read once, max-edge-walk
  counted from `from` ({:x :y :z})."
  [p from region]
  (let [flood (set (map vec (:cells (reach-room/room-flood p (reach-room/room-kind-at p (reach-room/shut-kind-at p)) (:x region) (:y region) (:z region)))))
        roofed (set (filter (fn [[cx cy cz]] (sh-sense/roofed? p (sh-sense/room-roof-height) {:x cx :y cy :z cz})) flood))
        exit? (fn [[cx cy cz]] (and (sh-sense/doorway? p {:x cx :y cy :z cz})
                                    (some (fn [[dx dz]] (let [n [(+ cx dx) cy (+ cz dz)]] (and (flood n) (not (roofed n))))) [[1 0] [-1 0] [0 1] [0 -1]])))
        stands (remove exit? (filter #(shore/standable? p {:x (% 0) :y (% 1) :z (% 2)}) roofed))]
    (fn [[x y z]]
      (let [best (first (sort-by (fn [[cx cy cz]] (+ (js/Math.abs (- cx x)) (js/Math.abs (- cy y)) (js/Math.abs (- cz z)))) stands))]
        (when (and best (<= (u/dist from {:x (best 0) :y (best 1) :z (best 2)}) max-edge-walk))
          {:x (best 0) :y (best 1) :z (best 2)})))))

(defn walkable-near
  "The cell {:x :y :z} of the closed region the body stands in (doors shut, as pending reads it) nearest cell [x y z], or nil.
  Only dry stand cells with a seen roof, and not a doorway beside a cell without one: a player judging a room does not walk out through
  its doorway to look at the outside, nor stand in the doorway (the flood runs past an open one over seen cells), where the
  night could not fill it. Nil when that cell lies farther than max-edge-walk from the feet."
  [p cell]
  (let [feet (sh-sense/feet p)] ((walkable-near-fn p feet feet) cell)))

(defn far-side
  "{:side [x y z] :to {:x :y :z}}: the unseen side, not in `looked`, of the closed region round `region` whose walkable-near
  cell `to` (within max-edge-walk of `from`, and not beside it: a look from there shows as much) lies farthest from `from`,
  nil when none: a walk to `to` and a look there shows `side` or the region past it, however far `to` lies from `side`
  (a tunnel seen without its floor has no stand cell nearer). The end of a dark tunnel a look from here cannot make out."
  [p from region looked]
  (let [near-fn (delay (walkable-near-fn p from region))]
    (some->> (seq (remove looked (reach-room/unseen-sides p region)))
             (keep (fn [side] (when-let [to (@near-fn side)] (when-not (u/within? from to 1) {:side side :to to}))))
             seq
             (apply max-key #(u/dist from (:to %))))))

(defn ^:async look!
  "A closed region with sides never seen is not yet known shut (an unseen cell may be an opening). The nearest of cells is
  looked at; what the look shows is read on the next pass, once the sight has settled. A side it did not show (a far wall
  of a long tunnel) is walked towards, on foot (walkable-near, edge-stride) and looked at again, as a player would; :again.
  The cell counts as tried when that second look has been read and the cell is still unseen. `to`: the cell to walk to, in
  place of walkable-near's."
  ([c cells] (look! c cells nil))
  ([c cells to]
   (ctx/update-mem! c update :edge-steps (fnil inc 0))
   (let [cell (first cells)
         phase (get-in (ctx/mem c) [:edge-phase cell] 0)
         advance! #(ctx/update-mem! c assoc-in [:edge-phase cell] %)]
     (case phase
       0 (do (advance! 1) (await (dig-look/look-at! c cell)))
       1 (let [reach-to (last (take-while #(<= (u/dist {:x (first cell) :y (second cell) :z (nth cell 2)} {:x (first %) :y (second %) :z (nth % 2)}) edge-stride) cells))
               near (or to (walkable-near (:primitives c) reach-to))
               w (if near (await (ctx/call c :edge-walk 'jobs.movement.go-to {:pos near :range 0 :ways #{:walk} :floor cost-health/spend-down})) :none)]
           (when (not= :continue w) (advance! 2)))
       2 (do (advance! 3) (await (dig-look/look-at! c cell)))
       (ctx/update-mem! c update :edge-looked (fnil conj #{}) cell))
     :again)))
