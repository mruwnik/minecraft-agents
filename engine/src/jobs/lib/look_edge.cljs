(ns jobs.lib.look-edge
  "Look first, judge after: an unseen cell is open until looked at (a player walks up and looks). The unseen sides of the
  closed region the body stands in under a cover, and the unseen cells of a roof column, are looked at, nearest first, and
  walked towards when a look cannot show them, a walk only while its hp (walk-risk/look-way) are bought by the call's worth.
  Memory keys :edge-looked, :edge-phase, :edge-from are the calling job's."
  (:require [jobs.lib.cost.health :as cost-health]
            [engine.ctx :as ctx]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shore :as shore]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk-risk :as walk-risk]))

(def edge-stride
  "How far past the nearest unseen cell one walk of the edge may reach, in blocks: the ground nearest the farthest unseen cell
  within this of the nearest, so a long tunnel is seen in a few strides."
  6)

(def max-edge-walk
  "Farthest, in blocks from the feet, a walk to look at an edge may go: a player judging a room does not cross a cave system
  the dig opened into, which the closed-region flood then covers."
  24)

(defn lit-side?
  "A predicate on an unseen cell [x y z]: a cell the body has seen open is beside it (a look shows a face only across open
  air: a cell inside rock, reached by the flood over unseen cells, never shows), and that cell is bright enough to be seen
  or the unseen cell is within near-of of the feet (a dark wall that close shows). A side of a dark stretch farther off is
  never shown (a dark cell is not recorded), so the region stays open there."
  [p feet]
  (let [see-lit (look/see-lit-fn p)
        near (look/near-of p)
        open-seen? (fn [x y z] (let [b (u/seen-block p {:x x :y y :z z})] (and b (not= :solid (reach/kind-of b)))))]
    (fn [[x y z]]
      (let [near? (and near (<= (u/dist feet {:x x :y y :z z}) near))]
        (some (fn [[dx dy dz]] (let [nx (+ x dx) ny (+ y dy) nz (+ z dz)]
                                 (and (open-seen? nx ny nz) (or (nil? see-lit) near? (== 1 (see-lit nx ny nz))))))
              [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])))))

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
  under a cover, and its unseen ground (the flood stands on seen blocks under unseen cells), that lit-side? shows, nearest
  first, not looked at in c's job memory. A dark cell stays unseen after a look and
  is not looked at twice. Reads no memory of a frame without a job: pass `looked` then."
  ([c] (pending c (:edge-looked (ctx/mem c) #{})))
  ([c looked]
   (let [p (:primitives c)
         feet (sh-sense/feet p)]
     (when (covered? p)
       (->> (reach-room/room-flood p (reach-room/room-kind-at p (reach-room/shut-kind-at p)) (:x feet) (:y feet) (:z feet))
            :cells
            (map vec)
            (filter (fn [[x y z]] (nil? (u/seen-block p {:x x :y y :z z}))))
            (concat (reach-room/unseen-sides p feet))
            distinct
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

(defn walk-bought?
  "Whether the hp of the walk to look at cell `to` and back (walk-risk/look-way) are bought by the call's worth
  (walk-risk/buys-hp): a walk toward an unseen wall is priced, not counted."
  [c to]
  (let [self (.self (:primitives c))]
    (<= (:hp (walk-risk/look-way c to 1)) (walk-risk/buys-hp (.-health self) (.-food self) (ctx/worth c)))))

(defn ^:async look!
  "A closed region with sides never seen is not yet known shut (an unseen cell may be an opening). The nearest of cells is
  looked at; what the look shows is read on the next pass, once the sight has settled. A side it did not show (a far wall
  of a long tunnel) is walked towards, on foot (walkable-near, edge-stride) and looked at again, as a player would; :again.
  max-edge-walk counts from :edge-from, where the walks began (the feet again once the body is farther off), so walks
  one after another do not creep down a tunnel.
  The cell counts as tried when that second look has been read and the cell is still unseen. `to`: the cell to walk to, in
  place of walkable-near's."
  ([c cells] (look! c cells nil))
  ([c cells to]
   (let [cell (first cells)
         phase (get-in (ctx/mem c) [:edge-phase cell] 0)
         advance! #(ctx/update-mem! c assoc-in [:edge-phase cell] %)]
     (case phase
       0 (do (advance! 1) (await (dig-look/look-at! c cell)))
       1 (let [p (:primitives c)
               feet (sh-sense/feet p)
               kept (:edge-from (ctx/mem c))
               from (if (and kept (u/within? feet kept (+ max-edge-walk 2))) kept feet)
               _ (ctx/update-mem! c assoc :edge-from from)
               reach-to (last (filter #(<= (u/dist {:x (first cell) :y (second cell) :z (nth cell 2)} {:x (first %) :y (second %) :z (nth % 2)}) edge-stride) cells))
               near (or to ((walkable-near-fn p from feet) reach-to))
               w (if (and near (walk-bought? c near)) (await (ctx/call c :edge-walk 'jobs.movement.go-to {:pos near :range 0 :ways #{:walk} :floor cost-health/spend-down})) :none)]
           (when (not= :continue w) (advance! 2)))
       2 (do (advance! 3) (await (dig-look/look-at! c cell)))
       (ctx/update-mem! c update :edge-looked (fnil conj #{}) cell))
     :again)))
