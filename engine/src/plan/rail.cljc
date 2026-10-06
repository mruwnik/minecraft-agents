(ns plan.rail
  "Rail lines in plans: pure, no IO (.cljc, so the dashboard and the tools can share it).
    1. the chain   line: the rail cells of a plan in order along the track, or why they are not one line
    2. the proof   judge-line: does the world hold a line a ridden cart runs from end to end
    3. layout      layout: a line of straight legs (corners, slopes of 1 per cell) as plan parts (every cell written
                   out), with its materials, or why the waypoints cannot be a line
  A plan stays a plan: rails are cells whose want names a *rail block. Positions are [x y z]. A world block is nil
  (unseen) or {:name n :state {k v}} as plan.shape takes it. Facts behind the numbers were measured on the server
  (rail facts, 2026-10-04): a source lights a powered rail and 8 more each way along its run, a lit powered rail
  beside a buffer throws a cart back, a cart runs into a buffer over normal rails and stops."
  (:require [clojure.string :as str]
            [plan.shape :as shape]))

;; ---------------------------------------------------------------- the chain

(defn rail-name? [name] (boolean (and name (re-find #"(^|_)rail$" name))))

(defn rail-cell? [{:keys [want]}] (rail-name? (shape/want-block want)))

(defn joined?
  "Whether two rail positions can follow each other on a track: side by side in plan view, at most 1 apart in height."
  [[x1 y1 z1] [x2 y2 z2]]
  (and (= 1 (+ (abs (- x1 x2)) (abs (- z1 z2)))) (<= (abs (- y1 y2)) 1)))

(defn neighbours [ps p] (filterv #(joined? p %) ps))

(defn walk
  "The positions of a chain from end start, in order."
  [graph start]
  (loop [chain [start] seen #{start}]
    (let [next (first (remove seen (graph (peek chain))))]
      (if next (recur (conj chain next) (conj seen next)) chain))))

(defn hole
  "The one missing position between the loose ends of two chains, if they are two cells apart in a straight line at
  the same height with no rail between, else nil."
  [graph ends]
  (first (for [[x1 y1 z1 :as a] ends [x2 y2 z2 :as b] ends
               :when (and (pos? (compare b a)) (= y1 y2)
                          (or (and (= x1 x2) (= 2 (abs (- z1 z2)))) (and (= z1 z2) (= 2 (abs (- x1 x2))))))
               :let [between [(quot (+ x1 x2) 2) y1 (quot (+ z1 z2) 2)]]
               :when (not (contains? graph between))]
           between)))

(defn line
  "The rail cells of cells ({:pos :want}) as one chain in track order, from the end with the smaller position; or
  {:error why} with why :no-rails, :branch (with :at, a rail joined to three or more), :gap (with :at, the one
  missing cell between two chains), :two-chains (or more) or :loop."
  [cells]
  (let [rails (filter rail-cell? cells)
        ps (mapv :pos rails)
        graph (into {} (map (juxt identity #(neighbours ps %))) ps)
        ends (sort (filter #(<= (count (graph %)) 1) ps))
        branch (first (sort (filter #(> (count (graph %)) 2) ps)))
        chain (when (seq ends) (walk graph (first ends)))
        by-pos (into {} (map (juxt :pos identity)) rails)]
    (cond
      (empty? rails) {:error :no-rails}
      branch {:error :branch :at branch}
      (empty? ends) {:error :loop}
      (< (count chain) (count ps)) (if-let [at (hole graph ends)] {:error :gap :at at} {:error :two-chains})
      :else (mapv by-pos chain))))

;; ---------------------------------------------------------------- reading blocks

(def loose
  "Blocks with nothing to stand a rail on."
  #{"air" "cave_air" "void_air" "water" "lava" "bubble_column" "fire" "soul_fire" "short_grass" "tall_grass" "grass"
    "fern" "large_fern" "snow" "vine" "dead_bush" "seagrass" "tall_seagrass" "light"})

(def thin
  "Blocks without a full top face (a rail pops off them)."
  #"^(farmland|dirt_path|soul_sand|ladder|torch|wall_torch|lantern|soul_lantern|chain|scaffolding|cobweb|cake|lectern|hopper|anvil|chipped_anvil|damaged_anvil|bell|enchanting_table|brewing_stand|composter|grindstone|stonecutter|chest|trapped_chest|ender_chest|lever|redstone_torch|redstone_wall_torch|redstone_wire)$|_(slab|stairs|fence|fence_gate|wall|pane|door|trapdoor|carpet|sign|button|pressure_plate|bed|torch|rail|leaves|sapling|flower)$|^glass_pane$")

(def falling #"^(gravel|sand|red_sand|suspicious_sand|suspicious_gravel)$|_concrete_powder$")

(defn state [block k] (some-> (shape/state-of (:state block) k) shape/state-text))

(defn sturdy?
  "Whether a rail stands on block: a full top face (a top or double slab and upside-down stairs count)."
  [{:keys [name] :as block}]
  (boolean
   (and name (not (loose name))
        (cond
          (str/ends-with? name "_slab") (contains? #{"top" "double"} (state block :type))
          (str/ends-with? name "_stairs") (= "top" (state block :half))
          :else (not (re-find thin name))))))

(defn clear? [{:keys [name]}] (or (shape/air? name) (contains? #{"light" "short_grass" "fern"} name)))

(defn lit? [block] (= "true" (state block :powered)))

;; ---------------------------------------------------------------- the proof

(def sides {[1 0] "east" [-1 0] "west" [0 1] "south" [0 -1] "north"})

(defn side-of [[x _ z] [x2 _ z2]] (sides [(- x2 x) (- z2 z)]))

(defn shape-joining
  "The rail shape that joins pos to its chain neighbours (one or two positions)."
  [pos joins]
  (let [up (first (filter #(> (second %) (second pos)) joins))
        dirs (set (map #(side-of pos %) joins))]
    (cond
      up (str "ascending_" (side-of pos up))
      (every? #{"east" "west"} dirs) "east_west"
      (every? #{"north" "south"} dirs) "north_south"
      :else (str (first (filter #{"north" "south"} dirs)) "_" (first (filter #{"east" "west"} dirs))))))

(defn step [[x y z] [x2 _ z2]] [(+ x2 (- x2 x)) y (+ z2 (- z2 z))])

(defn beyond
  "The cell past the end of chain ps where the buffer stands: on from the last two cells, at the end's height."
  [ps]
  (let [[a b] (take-last 2 ps)
        [bx by bz] b]
    (assoc (step a b) 1 by)))

(defn below [[x y z]] [x (dec y) z])
(defn above [[x y z]] [x (inc y) z])

(defn rail-breaks
  "The breaks at rail cell {:pos :want} of the chain, joins being its neighbours' positions."
  [block-at {:keys [pos want]} joins]
  (let [rail (block-at pos)
        bed (block-at (below pos))
        head (block-at (above pos))
        climbs? (and (seq joins) (str/starts-with? (shape-joining pos joins) "ascending"))
        high (when climbs? (block-at (above (above pos))))
        wanted (when (map? want) (shape/state-text (get want :powered "")))
        free? #(or (clear? %) (= "water" (:name %)))]
    (cond
      (some nil? [rail bed head]) [:unloaded]
      (and climbs? (nil? high)) [:unloaded]
      (not (rail-name? (:name rail))) [:gap]
      :else (cond-> []
              (and (seq joins) (not= (shape-joining pos joins) (state rail :shape))) (conj :shape)
              (and (= "true" wanted) (not (lit? rail))) (conj :unlit)
              (and (= "false" wanted) (lit? rail)) (conj :lit-brake)
              (not (sturdy? bed)) (conj :no-bed)
              (not (and (free? head) (or (not climbs?) (free? high)))) (conj :blocked)
              (or (= "true" (state rail :waterlogged)) (= "water" (:name head))) (conj :wet)))))

(defn end-breaks
  "The breaks at an end rail at pos with the buffer cell past it."
  [block-at pos buffer-pos]
  (let [rail (block-at pos)
        buffer (block-at buffer-pos)]
    (cond
      (not (rail-name? (:name rail))) []
      (nil? buffer) [:unloaded]
      (not (sturdy? buffer)) [:no-buffer]
      (and (= "powered_rail" (:name rail)) (lit? rail)) [:launch-trap]
      :else [])))

(defn judge-line
  "Whether the world (block-at: [x y z] -> block or nil) holds the line the rail cells of cells plan:
  {:ok? :breaks [{:pos :why}] :hazards [{:pos :why}]}, in chain order. :why is :gap (no rail), :shape (it does not
  join the cells before and after), :unlit (a powered rail wanted :powered true is not), :lit-brake (one wanted
  :powered false is), :no-bed (no full top face under the rail), :blocked (the cell above is not free), :wet
  (waterlogged or water above), :no-buffer (no sturdy block past an end), :launch-trap (a lit powered rail at an end
  beside its buffer, which throws an arriving cart back) or :unloaded (a cell the proof needs was not seen). Any
  sturdy block is a bed: what the plan's bed part wants is for filling, not judged. A bed that falls (gravel, sand,
  concrete powder) is a hazard, not a break. Cells that are not one line give {:ok? false :error why :breaks []}."
  [cells block-at]
  (let [chain (line cells)]
    (if (:error chain)
      {:ok? false :error (:error chain) :breaks [] :hazards []}
      (let [ps (mapv :pos chain)
            joins (fn [i] (keep #(get ps %) [(dec i) (inc i)]))
            ends (when (> (count ps) 1)
                   {(peek ps) (beyond ps) (first ps) (beyond (rseq ps))})
            breaks (vec (for [[i cell] (map-indexed vector chain)
                              why (concat (rail-breaks block-at cell (joins i))
                                          (when-let [b (get ends (:pos cell))] (end-breaks block-at (:pos cell) b)))]
                          {:pos (:pos cell) :why why}))
            hazards (vec (for [p ps
                               :let [bed (block-at (below p))]
                               :when (and bed (re-find falling (:name bed)))]
                           {:pos (below p) :why :falling-bed}))]
        {:ok? (empty? breaks) :breaks breaks :hazards hazards}))))

(def sources #{"redstone_block" "redstone_torch" "lever"})

(defn ground
  "The positions of cells where any sturdy block will do (the plan's want there only says what to fill with): under
  every rail, the buffer past each end and the cell under it, and under every torch or lever. A cell that wants a power
  source is never ground."
  [cells]
  (let [chain (line cells)
        ps (when-not (:error chain) (mapv :pos chain))
        buffers (when (> (count ps) 1) [(beyond ps) (beyond (rseq ps))])
        standing (keep #(when (#{"redstone_torch" "lever"} (shape/want-block (:want %))) (below (:pos %))) cells)
        wants-source (set (keep #(when (sources (shape/want-block (:want %))) (:pos %)) cells))]
    (into #{} (remove wants-source) (concat (map below ps) buffers (map below buffers) standing))))

;; ---------------------------------------------------------------- layout

(def power-every
  "Default cells between single lit powered rails on a spaced line. Measured on the server: one lit powered rail every
  34 cells holds 0.40 blocks per tick on a long run, 35 fails, and 34 sits 0.003 from the edge; 30 keeps a margin."
  30)

(def reach "Powered rails one source lights each way along its run (measured)." 8)

(def defaults
  {:style :spaced :launch 4 :launch-ends :both :power-every power-every :power-side :left
   :fill #{"cobblestone" "stone" "dirt"}})

(def source-block {:block "redstone_block" :torch "redstone_torch" :lever "lever"})

(defn opts-error [{:keys [style launch launch-ends power power-every power-side fill]}]
  (cond
    (not (#{:spaced :all-powered} style)) :style
    (not (and (int? launch) (<= 3 launch 5))) :launch
    (not (#{:both :first} launch-ends)) :launch-ends
    (not (source-block power)) :power
    (not (and (int? power-every) (>= power-every 2))) :power-every
    (not (#{:left :right} power-side)) :power-side
    (not (and (set? fill) (seq fill) (every? string? fill))) :fill))

(def tight-run
  "Lit powered rails in a row on each outer side of corners under 4 apart (an S bend or a U). The cart drops below the
  0.30 blocks per tick floor there (0.28 measured, accepted); four lit rails take it from 0.28 back to 0.35 and over,
  as the live trace of the S with two lit rails each side showed it back at 0.35 after four 10-tick windows."
  4)

(defn powered-indices
  "The indices of a line of n rails that are lit powered rails. shape ({:corners set :slopes set :lits set} of indices,
  default none) is what the route holds: a lit rail beside each corner, and tight-run lit rails in a row outside a pair of corners under 4 apart (the corner itself is a normal rail, so are
  slope cells that are not in :lits). Spaced: the launch groups, the corner neighbours and :lits are fixed, and between
  two fixed rails a lit one every power-every cells, counted from the earlier one, so the count restarts at each."
  ([n o] (powered-indices n o {}))
  ([n {:keys [style launch launch-ends power-every]} {:keys [corners slopes lits] :or {corners #{} slopes #{} lits #{}}}]
   (let [limit (- n 2)
         first-group (range 2 (+ 2 launch))
         far-group (when (= :both launch-ends) (range (- limit launch) limit))
         sorted-corners (sort corners)
         tight (filter (fn [[a b]] (< (- b a) 4)) (partition 2 1 sorted-corners))
         beside (remove #(or (contains? corners %) (< % 2) (>= % limit)) (concat (mapcat (fn [c] [(dec c) (inc c)]) corners)
                        (mapcat (fn [[a b]] (concat (range (- a tight-run) a) (range (inc b) (+ b 1 tight-run)))) tight)))]
     (if (= :all-powered style)
       (set (remove corners (range 2 limit)))
       (let [fixed (sort (distinct (concat first-group far-group beside lits)))
             spaced (for [[a b] (partition 2 1 (concat fixed [limit]))
                          t (range (+ a power-every) b power-every)
                          :when (not (or (contains? corners t) (contains? slopes t)))]
                      t)]
         (set (concat fixed spaced)))))))

(def curved #{:south_west :south_east :north_west :north_east})

(defn tight-bends
  "The pairs of corner positions [[a b] ...] of a chain (plan.rail/line) that lie under 4 cells apart: the cart slows
  there to about 0.28 blocks per tick, under the 0.30 floor of the rest of the line, which is accepted; layout lights
  tight-run rails on each outer side so it is back at cruise speed."
  [chain]
  (let [corners (keep-indexed (fn [i c] (when (curved (get-in c [:want :shape])) [i (:pos c)])) chain)]
    (vec (for [[[i a] [j b]] (partition 2 1 corners) :when (< (- j i) 4)] [a b]))))

(defn consecutive-runs
  "The runs of consecutive numbers in the ascending indices, as vectors."
  [indices]
  (reduce (fn [rs i] (if (= (dec i) (peek (peek rs))) (conj (pop rs) (conj (peek rs) i)) (conj rs [i]))) [] indices))

(defn runs
  "The contiguous runs of the sorted indices, each cut into pieces of at most one source's reach (2 * reach + 1)."
  [indices]
  (->> (sort indices)
       consecutive-runs
       (mapcat #(partition-all (inc (* 2 reach)) %))))

(defn middle [run] (nth run (quot (dec (count run)) 2)))

(defn sorted-any [fill] (into [:any] (sort fill)))

;; ---------------------------------------------------------------- layout: the route

(defn heading [[x1 _ z1] [x2 _ z2]] [(compare x2 x1) (compare z2 z1)])

(defn left-of [[dx dz]] [dz (- dx)])

(defn right-of [[dx dz]] [(- dz) dx])

(defn leg-cells
  "The cells of leg i from waypoint a to waypoint b, without a: one step at a time along x or z, the height changing
  1 per cell all the way (a slope) or not at all; else {:error :not-straight|:bad-slope :leg i}."
  [i [x1 y1 z1] [x2 y2 z2]]
  (let [dx (compare x2 x1)
        dz (compare z2 z1)
        len (+ (abs (- x2 x1)) (abs (- z2 z1)))
        dy (- y2 y1)]
    (cond
      (not= 1 (+ (abs dx) (abs dz))) {:error :not-straight :leg i}
      (not (or (zero? dy) (= len (abs dy)))) {:error :bad-slope :leg i}
      :else (mapv (fn [k] [(+ x1 (* k dx)) (+ y1 (* k (compare dy 0))) (+ z1 (* k dz))]) (range 1 (inc len))))))

(defn route
  "The waypoints ([x y z] of rail cells, each leg along x or z) as {:cells [pos ...] the chain, :bounds [index ...] of
  the waypoints in it, :corners {index :left|:right} where the line turns}, or {:error why :leg i ...}."
  [waypoints]
  (let [legs (vec (map-indexed (fn [i [a b]] (leg-cells i a b)) (partition 2 1 waypoints)))
        heads (mapv heading waypoints (rest waypoints))
        cells (into [(first waypoints)] cat (remove map? legs))
        bounds (vec (reductions + 0 (map count (remove map? legs))))
        turn (fn [k] (let [[d1 d2] [(heads (dec k)) (heads k)]]
                       (cond (= d1 d2) nil
                             (= d2 (left-of d1)) :left
                             (= d2 (right-of d1)) :right
                             :else :reversal)))
        turns (when (empty? (filter map? legs)) (into {} (keep (fn [k] (when-let [t (turn k)] [k t]))) (range 1 (count legs))))
        reversed (first (sort (keep (fn [[k t]] (when (= :reversal t) k)) turns)))]
    (cond
      (empty? legs) {:error :no-legs}
      (some map? legs) (first (filter map? legs))
      reversed {:error :reversal :leg reversed :at (cells (bounds reversed))}
      :else {:cells cells :bounds bounds
             :corners (into {} (map (fn [[k t]] [(bounds k) t])) turns)})))

(defn leg-of
  "The leg a chain index lies on (a waypoint belongs to the leg that ends there)."
  [bounds i]
  (count (take-while #(< % i) (rest bounds))))

(defn touching
  "The first cell that lies beside a cell of the line it does not follow in the chain (rails side by side join each
  other), or on one: {:error :touching :at pos :leg l}."
  [cells bounds]
  (let [index (zipmap cells (range))]
    (some (fn [j]
            (let [[x y z] (cells j)
                  beside? (fn [[dx dz]]
                            (some (fn [dy] (when-let [i (index [(+ x dx) (+ y dy) (+ z dz)])] (< i (dec j)))) [-1 0 1]))]
              (when (or (not= j (index (cells j))) (some beside? [[1 0] [-1 0] [0 1] [0 -1]]))
                {:error :touching :at (cells j) :leg (leg-of bounds j)})))
          (range (count cells)))))

(defn slope-cells
  "The indices of the cells whose rail climbs: a neighbour in the chain is one higher (ys: the height of each)."
  [ys]
  (let [n (count ys)
        higher? (fn [i j] (and (< -1 j n) (> (ys j) (ys i))))]
    (set (filter #(or (higher? % (dec %)) (higher? % (inc %))) (range n)))))

(defn valley
  "The first cell lower than both its neighbours: no rail shape joins them: {:error :valley :at pos :leg l}."
  [cells bounds]
  (some (fn [i] (let [y (get-in cells [i 1])]
                  (when (and (pos? i) (< (inc i) (count cells))
                             (> (get-in cells [(dec i) 1]) y) (> (get-in cells [(inc i) 1]) y))
                    {:error :valley :at (cells i) :leg (leg-of bounds i)})))
        (range (count cells))))

(defn slope-lits
  "The slope cells that are lit powered rails: every other one of each run of them, from the run's lower end (a climb
  is powered whichever way the line is ridden)."
  [ys slopes]
  (->> (consecutive-runs (sort slopes))
       (mapcat (fn [r] (take-nth 2 (if (<= (ys (first r)) (ys (peek r))) r (rseq r)))))
       set))

(defn structure-error
  "The first thing in the route that the line cannot have, as {:error why :leg l ...}, or nil: a corner or a slope in
  the launch groups or the two normal rails at an end or right beside the group (:leg-too-short for a corner, naming the
  leg that is too short; :slope-into-launch), two corners closer than 3 cells (:leg-too-short), a corner on or beside a
  climbing cell (:slope-into-corner)."
  [cells bounds corners {:keys [launch launch-ends]}]
  (let [n (count cells)
        ys (mapv second cells)
        near (+ launch 3)
        far (if (= :both launch-ends) (+ launch 3) 3)
        slopes (slope-cells ys)
        cs (sort (keys corners))
        leg (partial leg-of bounds)]
    (or (some (fn [c] (cond (< c near) {:error :leg-too-short :leg (leg c)}
                            (< (- n 1 c) far) {:error :leg-too-short :leg (inc (leg c))}))
              cs)
        (some (fn [[a b]] (when (< (- b a) 3) {:error :leg-too-short :leg (inc (leg a))})) (partition 2 1 cs))
        (some (fn [c] (when (or (not= (ys (dec c)) (ys c) (ys (inc c))) (slopes (dec c)) (slopes (inc c)))
                        {:error :slope-into-corner :at (cells c) :leg (leg c)}))
              cs)
        (some (fn [i] (when (or (< i near) (< (- n 1 i) far)) {:error :slope-into-launch :at (cells i) :leg (leg i)}))
              (sort slopes)))))

;; ---------------------------------------------------------------- layout: sources

(defn heading-at
  "The direction the chain ps runs in at index i, as [dx dz]."
  [ps i]
  (if (< (inc i) (count ps)) (heading (ps i) (ps (inc i))) (heading (ps (dec i)) (ps i))))

(defn blocked?
  "Whether the line holds a cell where a torch at cell [x y z] or its bed would stand: a rail from 2 below (its clear
  cell is the bed) to 1 above (its bed is the torch's cell)."
  [occupied [x y z]]
  (boolean (some #(contains? occupied [x % z]) (range (- y 2) (+ y 2)))))

(defn power-cell
  "The cell of the torch or lever beside rail i of the chain ps: {:cell [x y z]} on the caller-side if one is given, else
  on the outer side when the rail is near a bend (:outer, :left or :right), else on the left, else on the other side;
  or {:error :power-side :at rail-pos} (and :side, the caller's) when every side it may take is a cell of the line
  (occupied)."
  [ps occupied {:keys [caller-side outer]} i]
  (let [d (heading-at ps i)
        [x y z] (ps i)
        cell (fn [side] (let [[sx sz] (if (= :left side) (left-of d) (right-of d))] [(+ x sx) y (+ z sz)]))
        preferred (or caller-side outer :left)
        sides (if caller-side [caller-side] [preferred ({:left :right :right :left} preferred)])]
    (if-let [side (first (remove #(blocked? occupied (cell %)) sides))]
      {:cell (cell side)}
      (cond-> {:error :power-side :at (ps i)} caller-side (assoc :side caller-side)))))

(defn layout
  "A line of track as plan parts, every cell written out: (layout waypoints opts) with waypoints the [x y z] of rail
  cells, each leg along x or z, flat or a slope of 1 up or down per cell (the height of the next waypoint says which);
  (layout from to opts) is the one flat straight leg (it reports only the reason of an error). Options (defaults in
  `defaults`):
    :style        :spaced (a launch group, then ONE lit powered rail every :power-every cells) or :all-powered
    :launch       lit powered rails in each launch group, 3-5
    :launch-ends  :both (a launch group at each end, so the line is ridden either way) or :first
    :power-every  cells between single lit rails on a :spaced line
    :power        :torch (beside the rail, at its height, on a bed cell of its own), :block (a redstone block as the
                  bed under the rail) or :lever (in the torch's place; it is placed off and must be switched on).
                  Default :torch for :spaced, :block for :all-powered. One source per run of up to 17 powered rails.
    :power-side   :left or :right of the way from -> to, for a torch or lever
    :fill         the SET of blocks the body may place where a bed or buffer is missing; any sturdy block already
                  there is kept
  Each end: a buffer block past the end rail, then two normal rails (the get-in and stop cells), then the launch
  group, so no lit powered rail is ever beside a buffer. The cell above every rail is :clear.
  A corner is a normal rail (powered rails cannot curve) with a lit powered rail in the straight cell on each side
  (a cart is slowed in a corner and the line is ridden both ways); the spacing count restarts at each of them. On a
  slope every other climbing cell, from its lower end, is a lit powered rail with a source of its own (an unpowered
  climb stops after 6), and the two cells above each climbing cell are :clear. A torch or lever stands on the outer side
  of a bend, else on :power-side or the left; when the caller names :power-side and that side is a cell of the line the
  result is an error, not the other side.
  -> {:parts [...] :materials {:items {item n} :fill n}} (:fill: at most this many fill blocks), or {:error why ...}:
  :not-flat, :not-straight, :bad-slope, :reversal, :too-short, :touching (rails side by side would join), :valley,
  :leg-too-short (a corner in an end's launch group or beside a buffer, or two corners within 3 cells: :leg names the
  leg), :slope-into-corner, :slope-into-launch, :power-side (:at the rail), or the option at fault."
  ([from to opts]
   (if (not= (second from) (second to))
     {:error :not-flat}
     (let [laid (layout [from to] opts)]
       (if (:error laid) (select-keys laid [:error]) laid))))
  ([waypoints opts]
   (let [r (route waypoints)
         o (merge defaults
                  {:power (if (= :all-powered (:style opts)) :block :torch)}
                  opts)
         {:keys [cells bounds corners]} r
         n (count cells)
         ends-needed (+ 2 (:launch o) 2 (if (= :both (:launch-ends o)) (:launch o) 0))]
     (or (when (:error r) r)
         (when-let [error (opts-error o)] {:error error})
         (when (< n ends-needed) {:error :too-short})
         (touching cells bounds)
         (valley cells bounds)
         (structure-error cells bounds corners o)
         (let [ys (mapv second cells)
               slopes (slope-cells ys)
               corner-set (set (keys corners))
               powered (powered-indices n o {:corners corner-set :slopes slopes :lits (slope-lits ys slopes)})
               sources (map middle (runs powered))
               occupied (set cells)
               outer (fn [i] (when-let [c (first (filter #(<= (abs (- i %)) 2) corner-set))]
                               (if (= :left (corners c)) :right :left)))
               beside (map #(power-cell cells occupied {:caller-side (:power-side opts) :outer (outer %)} %) sources)]
           (or (first (filter :error beside))
               (let [block? (= :block (:power o))
                     source-cells (if block? (mapv #(below (cells %)) sources) (vec (distinct (map :cell beside))))
                     buffers [(beyond (rseq cells)) (beyond cells)]
                     beds (vec (concat (map below cells) (map below buffers) (when-not block? (map below source-cells))))
                     shape-of (mapv (fn [i] (keyword (shape-joining (cells i) (keep #(get cells %) [(dec i) (inc i)]))))
                                    (range n))
                     groups (sort-by (fn [[[lit? _] idxs]] [lit? (first idxs)])
                                     (group-by (fn [i] [(contains? powered i) (shape-of i)]) (range n)))
                     rail-parts (mapv (fn [[[lit? shape] idxs]]
                                        {:id (str (if lit? "powered" "rails") (when (not= shape (shape-of 0)) (str "-" (name shape))))
                                         :cells (mapv cells idxs)
                                         :want (if lit?
                                                 {:block "powered_rail" :shape shape :powered true}
                                                 {:block "rail" :shape shape})})
                                      groups)
                     head (vec (distinct (concat (map above cells) (map #(above (above (cells %))) (sort slopes)))))]
                 {:parts (into [{:id "bed" :cells beds :want (sorted-any (:fill o))}
                                {:id "buffers" :cells buffers :want (sorted-any (:fill o))}
                                {:id "power" :cells source-cells :want (source-block (:power o))}]
                               (conj rail-parts {:id "head" :cells head :want :clear}))
                  :materials {:items {"rail" (- n (count powered)) "powered_rail" (count powered)
                                      (source-block (:power o)) (count source-cells)}
                              :fill (- (+ (count beds) (count buffers)) (if block? (count source-cells) 0))}})))))))
