(ns plan.rail
  "Rail lines in plans: pure, no IO (.cljc, so the dashboard and the tools can share it).
    1. the chain   line: the rail cells of a plan in order along the track, or why they are not one line
    2. the proof   judge-line: does the world hold a line a ridden cart runs from end to end
    3. layout      layout: one straight flat segment as plan parts (every cell written out), with its materials
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
        wanted (when (map? want) (shape/state-text (get want :powered "")))]
    (cond
      (some nil? [rail bed head]) [:unloaded]
      (not (rail-name? (:name rail))) [:gap]
      :else (cond-> []
              (and (seq joins) (not= (shape-joining pos joins) (state rail :shape))) (conj :shape)
              (and (= "true" wanted) (not (lit? rail))) (conj :unlit)
              (and (= "false" wanted) (lit? rail)) (conj :lit-brake)
              (not (sturdy? bed)) (conj :no-bed)
              (not (or (clear? head) (= "water" (:name head)))) (conj :blocked)
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

(defn powered-indices
  "The indices of a line of n rails that are lit powered rails."
  [n {:keys [style launch launch-ends power-every]}]
  (let [far-launch (= :both launch-ends)
        first-group (range 2 (+ 2 launch))
        last-normal (- n 2)
        far-start (if far-launch (- last-normal launch) last-normal)
        far-group (range far-start last-normal)
        last-launch (+ 1 launch)]
    (set (concat first-group
                 (when far-launch far-group)
                 (if (= :all-powered style)
                   (range last-launch far-start)
                   (range (+ last-launch power-every) far-start power-every))))))

(defn runs
  "The contiguous runs of the sorted indices, each cut into pieces of at most one source's reach (2 * reach + 1)."
  [indices]
  (->> (sort indices)
       (reduce (fn [rs i] (if (= (dec i) (peek (peek rs))) (conj (pop rs) (conj (peek rs) i)) (conj rs [i]))) [])
       (mapcat #(partition-all (inc (* 2 reach)) %))))

(defn middle [run] (nth run (quot (dec (count run)) 2)))

(defn sorted-any [fill] (into [:any] (sort fill)))

(defn layout
  "One straight flat segment of track as plan parts, every cell written out, from rail cell from to rail cell to
  ([x y z], same y, along x or z), with options (defaults in `defaults`):
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
  -> {:parts [...] :materials {:items {item n} :fill n}} (:fill: at most this many fill blocks), or {:error why}:
  :not-flat, :not-straight, :too-short, or the option at fault."
  [from to opts]
  (let [o (merge defaults
                 {:power (if (= :all-powered (:style opts)) :block :torch)}
                 opts)
        [x1 y1 z1] from
        [x2 y2 z2] to
        dx (compare x2 x1)
        dz (compare z2 z1)
        n (inc (+ (abs (- x2 x1)) (abs (- z2 z1))))
        ends-needed (+ 2 (:launch o) 2 (if (= :both (:launch-ends o)) (:launch o) 0))]
    (cond
      (not= y1 y2) {:error :not-flat}
      (not= 1 (+ (abs dx) (abs dz))) {:error :not-straight}
      (opts-error o) {:error (opts-error o)}
      (< n ends-needed) {:error :too-short}
      :else
      (let [at (fn [i] [(+ x1 (* i dx)) y1 (+ z1 (* i dz))])
            rail-shape (if (zero? dz) :east_west :north_south)
            [sx sz] (if (= :left (:power-side o)) [dz (- dx)] [(- dz) dx])
            powered (powered-indices n o)
            sources (map middle (runs powered))
            source-cells (if (= :block (:power o))
                           (mapv #(below (at %)) sources)
                           (mapv #(let [[x y z] (at %)] [(+ x sx) y (+ z sz)]) sources))
            buffers [(at -1) (at n)]
            beds (concat (map (comp below at) (range n)) (map below buffers)
                         (when-not (= :block (:power o)) (map below source-cells)))
            fill (sorted-any (:fill o))
            normal (vec (remove powered (range n)))]
        {:parts [{:id "bed" :cells (vec beds) :want fill}
                 {:id "buffers" :cells buffers :want fill}
                 {:id "power" :cells source-cells :want (source-block (:power o))}
                 {:id "rails" :cells (mapv at normal) :want {:block "rail" :shape rail-shape}}
                 {:id "powered" :cells (mapv at (sort powered))
                  :want {:block "powered_rail" :shape rail-shape :powered true}}
                 {:id "head" :cells (mapv (comp above at) (range n)) :want :clear}]
         :materials {:items {"rail" (count normal) "powered_rail" (count powered)
                             (source-block (:power o)) (count source-cells)}
                     :fill (- (+ (count beds) (count buffers)) (if (= :block (:power o)) (count source-cells) 0))}}))))
