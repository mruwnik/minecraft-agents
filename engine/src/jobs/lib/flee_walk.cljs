(ns jobs.lib.flee-walk
  "Where a fleeing body can walk: the open cells along a direction and the walk target away from the threats (pure,
  over a block-at fn)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.breath :as breath]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::route-cone {:default 45 :doc "Degrees either side of the line to a destination (goal or home) within which a hostile nearer than the destination lies on the way: the flight does not go there." :spec (a/num-in 0 180)}
   ::keep-away {:default 4 :doc "Blocks a flee step's end keeps from every known hostile (a creeper's blast reaches about 3); a step that ends nearer is not taken, and a body with no other is cornered." :spec (a/num-in 0 nil)}
   ::hazard-clearance {:default 2.5 :doc "Distance in blocks kept from a hazard on a flight path." :spec (a/num-in 0 nil)}
   ::flank-turn {:default 45 :doc "Degrees a flight turns from straight away toward a refuge that lies behind the chasers, to go round them, when it still gains on them so (jobs.lib.cost/outruns-at?)." :spec (a/num-in 0 90)}
   ::max-descent {:default 3 :doc "Blocks lower than where a flight began that it may end a step under rock (a cave, not a hillside); a step further down is not taken while another is open, else the body is cornered." :spec (a/num-in 0 nil)}
   ::min-open {:default 2 :doc "Fewest clear cells a direction needs to be worth walking." :spec (a/int-in 0 nil)}
   ::roof-reach {:default 8 :doc "Blocks above the head searched for rock over a cell." :spec (a/int-in 0 nil)}})
(defn min-open [] (settings/get settings ::min-open))
(defn roof-reach [] (settings/get settings ::roof-reach))
(defn keep-away [] (settings/get settings ::keep-away))
(defn hazard-clearance [] (settings/get settings ::hazard-clearance))
(defn flank-turn [] (settings/get settings ::flank-turn))
(defn max-descent [] (settings/get settings ::max-descent))
(defn route-cone [] (settings/get settings ::route-cone))

(def turns
  "Angles to try, in degrees from the preferred direction, best first."
  [0 30 -30 60 -60 90 -90 120 -120])

(defn passable?
  "Whether a block name lets the body walk through (water is swum); an unloaded cell (nil) counts as open."
  [block-name]
  (or (nil? block-name) (bs/passable-or-water? block-name)))

(defn floorless?
  "Whether a block name under a walker's feet is no floor: a loaded passable
  block other than water (which a walker swims on). An unloaded cell (nil) is not."
  [block-name]
  (and (some? block-name) (passable? block-name) (not= "water" block-name)))

(defn unit
  "[ux uz] for the vector (dx dz), or nil when it is zero."
  [dx dz]
  (let [n (js/Math.hypot dx dz)]
    (when (pos? n) [(/ dx n) (/ dz n)])))

(defn rotate [[ux uz] degrees]
  (let [a (* degrees (/ js/Math.PI 180))
        c (js/Math.cos a)
        s (js/Math.sin a)]
    [(- (* ux c) (* uz s)) (+ (* ux s) (* uz c))]))

(defn direction
  "The preferred unit [ux uz] from from: directly away from the threats (positions), each weighing 1/distance so the
  nearer pushes harder (+x when they cancel out), blended with the direction of home when that lies on the away side
  (not through the threats). flank?: home is a refuge, so when it lies behind the threats the way away turns flank-turn
  degrees toward its side (a fixed side when straight behind), to go round them."
  ([from threats home] (direction from threats home false))
  ([from threats home flank?]
  (let [push (fn [t] (let [d (max 1 (js/Math.hypot (- (:x from) (:x t)) (- (:z from) (:z t))))
                           [ux uz] (or (unit (- (:x from) (:x t)) (- (:z from) (:z t))) [0 0])]
                       [(/ ux d) (/ uz d)]))
        sum (reduce (fn [[ax az] t] (let [[ux uz] (push t)] [(+ ax ux) (+ az uz)])) [0 0] threats)
        away (or (unit (first sum) (second sum)) [1 0])
        to-home (when home (unit (- (:x home) (:x from)) (- (:z home) (:z from))))
        along (when to-home (+ (* (first away) (first to-home)) (* (second away) (second to-home))))]
    (cond
      (and along (pos? along)) (or (unit (+ (first away) (first to-home)) (+ (second away) (second to-home))) away)
      (and along flank?) (let [cross (- (* (first away) (second to-home)) (* (second away) (first to-home)))]
                           (rotate away (if (neg? cross) (- (flank-turn)) (flank-turn))))
      :else away))))

(defn on-the-way?
  "Whether any threat (position) lies toward dest from `from`: nearer than dest and within cone degrees (default
  route-cone) of the line to it, so a walk there leads to or past the hostile."
  ([from threats dest] (on-the-way? from threats dest (route-cone)))
  ([from threats dest cone]
   (let [[dx dz] [(- (:x dest) (:x from)) (- (:z dest) (:z from))]
         d (js/Math.hypot dx dz)
         cos-cone (js/Math.cos (* cone (/ js/Math.PI 180)))]
     (boolean (some (fn [t]
                      (let [[tx tz] [(- (:x t) (:x from)) (- (:z t) (:z from))]
                            td (js/Math.hypot tx tz)]
                        (and (< td d)
                             (or (< td 1) (>= (/ (+ (* dx tx) (* dz tz)) (* d td)) cos-cone)))))
                    threats)))))

(defn column-along
  "The [x z] column k blocks along [ux uz] from the centre of from's cell (a
  body at x 58.5 stands in column 58 and probes from 58.5, not 59)."
  [from [ux uz] k]
  [(js/Math.floor (+ (js/Math.floor (:x from)) 0.5 (* ux k)))
   (js/Math.floor (+ (js/Math.floor (:z from)) 0.5 (* uz k)))])

(defn point-along
  "The cell step blocks from from along [ux uz], same height."
  [from dir step]
  (let [[x z] (column-along from dir step)]
    {:x x :y (:y from) :z z}))

(defn near-hazard?
  "Whether the walk from from to target passes within clearance of a hazard
  (checked at the middle and the end)."
  [hazards from target]
  (let [mid {:x (/ (+ (:x from) (:x target)) 2) :y (:y from) :z (/ (+ (:z from) (:z target)) 2)}]
    (boolean (some #(or (< (u/dist % target) (hazard-clearance)) (< (u/dist % mid) (hazard-clearance))) hazards))))

(defn free-at?
  "Whether feet and head cells at feet height y in column x z are passable."
  [block-at x y z]
  (and (passable? (block-at {:x x :y y :z z})) (passable? (block-at {:x x :y (inc y) :z z}))))

(defn next-y
  "The feet height a walker at feet height y in column [px pz] reaches in the
  next column [x z], or nil when it cannot enter it: the same height, one
  lower when the floor there is open and the cell under it is not (a step
  down, never into water over the head: a swimmer keeps to the surface; a column with no floor, a drop of two or more, is not entered), else one higher when that is free and there is headroom above the
  column it steps from (a step up)."
  [block-at [px pz] [x z] y]
  (cond
    (free-at? block-at x y z)
    (let [below (block-at {:x x :y (dec y) :z z})
          below2 (block-at {:x x :y (- y 2) :z z})]
      (cond
        (and (passable? below) (not (passable? below2)) (not= "water" (block-at {:x x :y y :z z}))) (dec y)
        (floorless? below) nil
        :else y))
    (and (free-at? block-at x (inc y) z) (passable? (block-at {:x px :y (+ y 2) :z pz}))) (inc y)
    :else nil))

(defn corner-shut?
  "Whether a diagonal step from column [px pz] to [x z] at feet height y passes between two blocked columns: a body
  0.6 wide cannot squeeze through that corner."
  [block-at [px pz] [x z] y]
  (and (not= px x) (not= pz z)
       (not (free-at? block-at px y z))
       (not (free-at? block-at x y pz))))

(defn walk-cells
  "The feet cells {:x :y :z}, one per block along [ux uz] from from, up to n,
  that a walker passes before the first column it cannot enter, per block-at
  (a cell -> block name or nil). A column met twice (a diagonal) repeats its cell;
  a diagonal step between two blocked columns (corner-shut?) is not taken."
  [block-at from dir n]
  (loop [k 1
         prev [(js/Math.floor (:x from)) (js/Math.floor (:z from))]
         y (js/Math.floor (:y from))
         out []]
    (let [col (column-along from dir k)
          ny (when (<= k n)
               (cond
                 (= col prev) y
                 (corner-shut? block-at prev col y) nil
                 :else (next-y block-at prev col y)))]
      (if (nil? ny)
        out
        (recur (inc k) col ny (conj out {:x (first col) :y ny :z (second col)}))))))

(defn open-cells
  "How many blocks along [ux uz] from from, up to n, a walker gets before the
  first obstacle (see walk-cells)."
  [block-at from dir n]
  (count (walk-cells block-at from dir n)))

(defn worth?
  "Whether walking the open blocks along a direction from from, to the cell
  end, is worth it against threat: it must not end closer, and must either be
  a real walk (min-open + 1 blocks) or gain at least 2 blocks of distance. A
  short side step in a dead end is neither, so a body that only has those
  left is cornered."
  [from threats {:keys [open end]}]
  (and (>= open (min-open))
       (let [now (apply min (map #(u/dist from %) threats))
             then (apply min (map #(u/dist end %) threats))]
         (and (>= then now)
              (or (> open (min-open)) (>= (- then now) 2))))))

(defn wet?
  "Whether a walker in feet cell swims there: water (or kelp, seagrass, a bubble column: breath/swimmable) at its feet,
  or as its floor (it sinks into it), or a waterlogged? block (cell -> bool: a block name says nothing of it) in either."
  ([block-at cell] (wet? block-at (constantly false) cell))
  ([block-at waterlogged? {:keys [x y z]}]
   (let [feet {:x x :y y :z z} floor {:x x :y (dec y) :z z}]
     (or (contains? breath/swimmable (block-at feet)) (contains? breath/swimmable (block-at floor))
         (boolean (waterlogged? feet)) (boolean (waterlogged? floor))))))

(defn walk-time
  "Blocks of walking cells take: a wet? one counts swim-factor."
  [block-at waterlogged? cells swim-factor]
  (reduce + 0 (map #(if (wet? block-at waterlogged? %) swim-factor 1) cells)))

(defn gain
  "Blocks farther from the threats at the end of a walk (at least 1; 1 for a walk with no end or no threat) the walk puts between the body and them: what its time buys."
  [from threats end]
  (if (and end (seq threats))
    (max 1 (- (apply min (map #(u/dist end %) threats)) (apply min (map #(u/dist from %) threats))))
    1))

(defn roofed?
  "Whether a block that is not passable lies within roof-reach above the head of feet cell end."
  [block-at {:keys [x y z]}]
  (some #(not (passable? (block-at {:x x :y (+ y 2 %) :z z}))) (range (roof-reach))))

(defn too-close?
  "Whether pos lies within keep-away of any of threats (positions)."
  [threats pos]
  (some #(< (u/dist pos %) (keep-away)) threats))

(defn choose-target
  "The walk target, a feet cell, among the directions (turns from the preferred one) that avoid every hazard and are
  worth walking (see worth?): first one whose end (covered? true: out of a ranged mob's line of fire) gives cover, a
  full step before a short one; else the full step that takes least time per block of distance gained (walk-time: a swim is slow), turn order
  breaking ties; else the most open. With a swim-factor above 1 a direction that ends in water counts only when none ends dry (a swim is
  slow and drowned wait in it); :dry-only? (a drowned is a threat: it hunts in water) leaves wet ends out even then. nil when cornered. opts {:swim-factor (default 1) :dry-only? :covered? (end -> bool)
  :flank? (home is a refuge: direction) :refused (set of [x y z] ends go-to refused, never chosen) :avoid (end -> bool fns, most wanted first: ends one is true of are left out unless that leaves none) :waterlogged? (cell -> bool: a waterlogged block is wet) :floor (lowest feet y an end under rock may have, unless it is out of a ranged mob's line of fire or on a floor of known, a set of [x y z] floors of stairs the body cut)}."
  ([block-at from threats home hazards step] (choose-target block-at from threats home hazards step nil))
  ([block-at from threats home hazards step {:keys [swim-factor dry-only? covered? flank? floor known avoid refused waterlogged?] :or {swim-factor 1 waterlogged? (constantly false)}}]
   (let [dir (direction from threats home flank?)
         options (->> turns
                      (map #(rotate dir %))
                      (map (fn [d] (let [cells (walk-cells block-at from d step)]
                                     {:open (count cells) :dir d :end (peek cells)
                                      :cost (/ (walk-time block-at waterlogged? cells swim-factor) (gain from threats (peek cells)))})))
                      (remove #(near-hazard? hazards from (point-along from (:dir %) step)))
                      (filter #(worth? from threats %))
                      (remove #(too-close? threats (:end %)))
                      (remove #(contains? refused (mapv (:end %) [:x :y :z])))
                      (remove #(and floor (< (:y (:end %)) floor) (roofed? block-at (:end %)) (not (and covered? (covered? (:end %)))) (not (contains? known [(:x (:end %)) (dec (:y (:end %))) (:z (:end %))])))))
         dry (remove #(wet? block-at waterlogged? (:end %)) options)
         options (if (or dry-only? (and (> swim-factor 1) (seq dry))) dry options)
         options (reduce (fn [os bad?] (let [kept (remove #(bad? (:end %)) os)] (if (seq kept) kept os))) options avoid)
         full? #(>= (:open %) step)
         pick (or (when covered?
                    (first (filter #(covered? (:end %)) (sort-by (juxt (complement full?) :cost) options))))
                  (first (sort-by :cost (filter full? options)))
                  (last (sort-by (juxt :open (comp - :cost)) options)))]
     (:end pick))))

(defn way-target
  "The feet cell, a map, of the highest floor of floors ([x y z], the stairs this body cut) within reach blocks and
  above the body whose centre is no nearer any threat than the body: the way back up. nil when none."
  [from threats floors reach]
  (let [near-threat (fn [pos] (apply min (map #(u/dist pos %) threats)))
        now (near-threat from)
        cells (for [[x y z] floors
                    :let [cell {:x x :y (inc y) :z z} centre {:x (+ x 0.5) :y (inc y) :z (+ z 0.5)}]
                    :when (and (> (inc y) (+ (:y from) 0.5)) (<= (u/dist from centre) reach) (>= (near-threat centre) now) (not (too-close? threats centre)))]
                [cell (u/dist from centre)])]
    (some->> (seq cells) (sort-by (juxt (comp - :y first) second)) ffirst)))
