(ns jobs.lib.look-edge
  "Look first, judge after: an unseen cell is open until looked at (a player walks up and looks). The unseen sides of the
  closed region the body stands in under a cover, and the unseen cells of a roof column, are looked at, nearest first, and
  walked towards when a look cannot show them, a walk only while its price (walk-risk/look-cost) and the walks before it
  are bought by the call's worth. Memory keys :edge-looked, :edge-phase, :edge-spent, :edge-walked, :edge-t0 are the calling job's."
  (:require [jobs.lib.cost :as cost]
            [jobs.lib.clock :as clock]
            [jobs.lib.cost.health :as cost-health]
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
       (let [flood (some->> (reach-room/room-flood p (reach-room/room-kind-at p (reach-room/shut-kind-at p)) (:x feet) (:y feet) (:z feet))
                            :cells
                            (map vec))
             h (sh-sense/room-roof-height)
             sky-open? (some (fn [[x y z]] (sh-sense/open-air-above? p {:x x :y y :z z} (+ y h))) flood)
             under-roof? (fn [[x y z]] (let [cell {:x x :y y :z z}] (or (sh-sense/roofed? p h cell) (sh-sense/leaf-roofed? p h cell))))]
         (->> flood
              (filter (fn [[x y z]] (nil? (u/seen-block p {:x x :y y :z z}))))
              (concat (reach-room/unseen-sides p feet))
              distinct
              (remove looked)
              (filter (if sky-open? under-roof? any?))
              (filter (lit-side? p feet))
              (sort-by (fn [[x y z]] (u/dist feet {:x x :y y :z z})))
              vec))))))

(defn walkable-near-fn
  "walkable-near as a fn of the cell [x y z], over the closed region round `region` ({:x :y :z}) read once."
  [p region]
  (let [flood (set (map vec (:cells (reach-room/room-flood p (reach-room/room-kind-at p (reach-room/shut-kind-at p)) (:x region) (:y region) (:z region)))))
        roofed (set (filter (fn [[cx cy cz]] (sh-sense/roofed? p (sh-sense/room-roof-height) {:x cx :y cy :z cz})) flood))
        exit? (fn [[cx cy cz]] (and (sh-sense/doorway? p {:x cx :y cy :z cz})
                                    (some (fn [[dx dz]] (let [n [(+ cx dx) cy (+ cz dz)]] (and (flood n) (not (roofed n))))) [[1 0] [-1 0] [0 1] [0 -1]])))
        stands (remove exit? (filter #(shore/standable? p {:x (% 0) :y (% 1) :z (% 2)}) roofed))]
    (fn [[x y z]]
      (let [best (first (sort-by (fn [[cx cy cz]] (+ (js/Math.abs (- cx x)) (js/Math.abs (- cy y)) (js/Math.abs (- cz z)))) stands))]
        (when best {:x (best 0) :y (best 1) :z (best 2)})))))

(defn walkable-near
  "The cell {:x :y :z} of the closed region the body stands in (doors shut, as pending reads it) nearest cell [x y z], or nil.
  Only dry stand cells with a seen roof, and not a doorway beside a cell without one: a player judging a room does not walk out through
  its doorway to look at the outside, nor stand in the doorway (the flood runs past an open one over seen cells), where the
  night could not fill it."
  [p cell]
  ((walkable-near-fn p (sh-sense/feet p)) cell))

(defn far-side
  "{:side [x y z] :to {:x :y :z}}: the unseen side, not in `looked`, of the closed region round `region` whose walkable-near
  cell `to` (not beside `from`: a look from there shows as much) lies farthest from `from`,
  nil when none: a walk to `to` and a look there shows `side` or the region past it, however far `to` lies from `side`
  (a tunnel seen without its floor has no stand cell nearer). The end of a dark tunnel a look from here cannot make out."
  [p from region looked]
  (let [near-fn (delay (walkable-near-fn p region))]
    (some->> (seq (remove looked (reach-room/unseen-sides p region)))
             (keep (fn [side] (when-let [to (@near-fn side)] (when-not (u/within? from to 1) {:side side :to to}))))
             seq
             (apply max-key #(u/dist from (:to %))))))

(defn affords-walk?
  "Whether a walk of s seconds is bought from `worth` after the seconds the job's edge walks before it spent (:edge-spent)."
  [c s worth]
  (cost/affords? (+ s (:edge-spent (ctx/mem c) 0)) worth))

(defn pays?
  "Whether looking at the pending `cells` is bought from `worth`: the region is a shelter only if every one of them proves
  closed (cost/closed-chance of that many unseen), a saving of worth at that chance, against looking at all of them at the
  seconds each has taken on the clock so far (:edge-t0), look-s before the first. Many unseen cells, or cells no look from
  here shows (they stay unseen and each costs its seconds), are never bought."
  [c worth cells]
  (let [{:keys [edge-looked edge-t0]} (ctx/mem c)
        n (count edge-looked)
        each-s (if (and edge-t0 (pos? n)) (/ (- (clock/now) edge-t0) 1000 n) (cost/look-s 1))
        k (count cells)]
    (> (* worth (cost/closed-chance 0 k)) (* k each-s))))

(defn walk-price
  "Seconds the walk to look at cell `to` and back costs (walk-risk/look-cost), nil when it is not bought: its hp more than
  `worth` buys, or its seconds and the walks before it (affords-walk?) over it. A walk toward an unseen wall is priced,
  not counted."
  [c to worth]
  (let [s (walk-risk/look-cost c (walk-risk/look-way c to 1) worth)]
    (when (and s (affords-walk? c s worth)) s)))

(defn ^:async look!
  "A closed region with sides never seen is not yet known shut (an unseen cell may be an opening). The nearest of cells is
  looked at; what the look shows is read on the next pass, once the sight has settled. A side it did not show (a far wall
  of a long tunnel) is walked towards, on foot (walkable-near, edge-stride) and looked at again, as a player would; :again.
  A cell begun stays the one worked on while it is still pending, however the walk reorders the nearest-first list. A walk is priced once, at its start, with the seconds the job's walks spent before it (:edge-spent), so walks one after
  another are bought from what the call's worth leaves.
  The cell counts as tried when that second look has been read and the cell is still unseen. `to`: the cell to walk to, in
  place of walkable-near's. `worth`: what the walks are bought from, the call's (ctx/worth) unless given."
  ([c cells] (look! c cells nil))
  ([c cells to] (look! c cells to (ctx/worth c)))
  ([c cells to worth]
   (let [phases (:edge-phase (ctx/mem c))
         cell (or (first (filter #(<= 1 (get phases % 0) 4) cells)) (first cells))
         cells (cons cell (remove #{cell} cells))
         phase (get-in (ctx/mem c) [:edge-phase cell] 0)
         advance! #(ctx/update-mem! c assoc-in [:edge-phase cell] %)]
     (case phase
       0 (do (advance! 1)
             (when-not (:edge-t0 (ctx/mem c)) (ctx/update-mem! c assoc :edge-t0 (clock/now)))
             (await (dig-look/look-at! c cell)))
       (1 2) (let [p (:primitives c)
                   reach-to (last (filter #(<= (u/dist {:x (first cell) :y (second cell) :z (nth cell 2)} {:x (first %) :y (second %) :z (nth % 2)}) edge-stride) cells))
                   near (or to ((walkable-near-fn p (sh-sense/feet p)) reach-to))
                   spent (:edge-spent (ctx/mem c) 0)
                   price (when (and near (= 1 phase)) (walk-price c near worth))
                   _ (when price (ctx/update-mem! c assoc :edge-spent (+ spent price)))
                   _ (when price (ctx/update-mem! c assoc :edge-walked true))
                   w (if (and near (or price (= 2 phase)))
                       (await (ctx/call c :edge-walk 'jobs.movement.go-to {:pos near :range 0 :ways #{:walk} :floor cost-health/spend-down}))
                       :none)]
               (advance! (cond (= :continue w) 2 (= :none w) 4 :else 3)))
       3 (do (advance! 4) (await (dig-look/look-at! c cell)))
       (ctx/update-mem! c update :edge-looked (fnil conj #{}) cell))
     :again)))
