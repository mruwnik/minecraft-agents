(ns jobs.lib.strip
  "Reading an abandoned mineshaft (a strip site) from seen blocks, as compact lines and never block lists.

  A support is two wooden fences 2 high, 3 cells apart, with planks over the three cells; it gives a corridor's centre
  line. Rails give it too. Evidence on one line becomes a run, runs are cut where a perpendicular run crosses them, and
  each piece is walked outward over passable cells to its true end. An end is open when the cell past it was never seen.

  A segment is {:id :y :axis :x|:z :line n :from a :to b :open-ends #{:from :to}}: y is the rail level, the axis the
  direction it runs, line the other horizontal coordinate. The id comes from the geometry (\"x-27-290:-180..-160\").
  Memory is a map id -> segment, with :state kept by the caller."
  (:require [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]))

(def run-gap "Evidence further apart than this on one line belongs to different runs." 9)
(def join "Reach at which two perpendicular runs count as crossing." 7)
(def end-reach "Cells walked past the last evidence looking for a corridor's end." 6)
(def min-rails "Rails alone make a run from this many." 3)

(defn index
  "Seen blocks [{:name :pos {:x :y :z}}] as {[x y z] name}."
  [blocks]
  (into {} (map (fn [{:keys [name pos]}] [[(:x pos) (:y pos) (:z pos)] name])) blocks))

(defn fence? [n] (boolean (and n (re-find #"_fence$" n) (not= n "nether_brick_fence"))))
(defn planks? [n] (boolean (and n (re-find #"_planks$" n))))
(defn rail? [n] (boolean (and n (re-find #"(^|_)rail$" n))))
(defn passable? [n] (boolean (and n (or (bs/air n) (= "cobweb" n) (rail? n) (re-find #"torch$" n)))))

(defn at
  "The cell at coordinate a along axis, on line, at y."
  [axis y line a]
  (if (= axis :x) [a y line] [line y a]))

(defn supports
  "The support frames in idx: [{:axis :y :line :a}]."
  [idx]
  (for [[[x y z] n] idx
        :when (and (fence? n) (fence? (idx [x (inc y) z])))
        axis [:x :z]
        :let [[ox _ oz] (if (= axis :x) [x y (+ z 2)] [(+ x 2) y z])]
        :when (and (fence? (idx [ox y oz])) (fence? (idx [ox (inc y) oz]))
                   (every? (fn [k] (planks? (idx (if (= axis :x) [x (+ y 2) (+ z k)] [(+ x k) (+ y 2) z])))) [0 1 2]))]
    (if (= axis :x)
      {:axis :x :y y :line (inc z) :a x :kind :support}
      {:axis :z :y y :line (inc x) :a z :kind :support})))

(defn rails
  "Rail cells in idx with a rail beside them along an axis: [{:axis :y :line :a}]."
  [idx]
  (for [[[x y z] n] idx
        :when (rail? n)
        [axis ok] [[:x (or (rail? (idx [(inc x) y z])) (rail? (idx [(dec x) y z])))]
                   [:z (or (rail? (idx [x y (inc z)])) (rail? (idx [x y (dec z)])))]]
        :when ok]
    {:axis axis :y y :line (if (= axis :x) z x) :a (if (= axis :x) x z) :kind :rail}))

(defn runs
  "Runs of evidence in idx: [{:axis :y :line :from :to}], each holding a support or min-rails rails."
  [idx]
  (for [[[axis y line] evidence] (group-by (juxt :axis :y :line) (concat (supports idx) (rails idx)))
        piece (reduce (fn [acc e] (if (and (seq acc) (<= (- (:a e) (:a (peek (peek acc)))) run-gap))
                                    (conj (pop acc) (conj (peek acc) e))
                                    (conj acc [e])))
                      [] (sort-by :a (distinct evidence)))
        :let [as (map :a piece)]
        :when (or (some #(= :support (:kind %)) piece) (>= (count piece) min-rails))]
    {:axis axis :y y :line line :from (apply min as) :to (apply max as)}))

(defn cuts
  "The lines of the perpendicular runs that cross run."
  [run all]
  (->> all
       (filter (fn [s] (and (not= (:axis s) (:axis run)) (= (:y s) (:y run))
                            (<= (- (:from s) join) (:line run) (+ (:to s) join))
                            (<= (- (:from run) join) (:line s) (+ (:to run) join)))))
       (map :line) distinct sort))

(defn split
  "run cut at its crossings: pieces with the ends that sit on a crossing in :snapped."
  [run cs]
  (let [{:keys [from to snapped interior]}
        (reduce (fn [st c]
                  (cond (<= c (inc (:from st))) (assoc st :from c :snapped (conj (:snapped st) :from))
                        (>= c (dec (:to st))) (assoc st :to c :snapped (conj (:snapped st) :to))
                        :else (update st :interior conj c)))
                {:from (:from run) :to (:to run) :snapped #{} :interior []} cs)
        bounds (vec (concat [from] interior [to]))
        n (dec (count bounds))]
    (for [i (range n)]
      (assoc run :from (bounds i) :to (bounds (inc i))
             :snapped (cond-> #{}
                        (or (pos? i) (snapped :from)) (conj :from)
                        (or (< i (dec n)) (snapped :to)) (conj :to))))))

(defn walk-end
  "Walks from coordinate e in direction d over passable cells: [end open?]. Open when the cell past the end was never
  seen (block-at gives nil) or the corridor goes on past end-reach."
  [block-at {:keys [axis y line]} e d]
  (loop [k 1]
    (let [end (+ e (* d (dec k)))
          n (when (<= k end-reach) (block-at (at axis y line (+ e (* d k)))))]
      (cond (> k end-reach) [(+ e (* d end-reach)) true]
            (nil? n) [end true]
            (passable? n) (recur (inc k))
            :else [end false]))))

(defn seg-id [{:keys [axis y line from to]}] (str (name axis) y "-" line ":" from ".." to))

(defn finish
  "A split piece with its free ends walked out: the segment."
  [block-at {:keys [snapped] :as piece}]
  (let [[from from-open] (if (snapped :from) [(:from piece) false] (walk-end block-at piece (:from piece) -1))
        [to to-open] (if (snapped :to) [(:to piece) false] (walk-end block-at piece (:to piece) 1))
        seg {:y (:y piece) :axis (:axis piece) :line (:line piece) :from from :to to
             :open-ends (cond-> #{} from-open (conj :from) to-open (conj :to))}]
    (assoc seg :id (seg-id seg))))

(defn segments
  "The corridors the blocks show, as segment lines, one per straight run between crossings. block-at (cell -> block
  name, nil when never seen) defaults to the blocks given."
  ([blocks] (segments blocks nil))
  ([blocks {:keys [block-at]}]
   (let [idx (index blocks)
         all (vec (runs idx))
         block-at (or block-at idx)]
     (->> all
          (mapcat #(split % (cuts % all)))
          (map #(finish block-at %))
          (sort-by :id) vec))))

(defn overlaps?
  "Same line and more than the one shared cell (crossing pieces of a line only touch)."
  [a b]
  (let [lo (max (:from a) (:from b)) hi (min (:to a) (:to b))]
    (and (= (:axis a) (:axis b)) (= (:y a) (:y b)) (= (:line a) (:line b))
         (or (< lo hi) (and (= lo hi) (or (= (:from a) (:to a)) (= (:from b) (:to b))))))))

(defn merge-one [r n]
  (let [from (min (:from r) (:from n))
        to (max (:to r) (:to n))
        open? (fn [end v] (every? #(or (not= v (end %)) (contains? (:open-ends %) end)) [r n]))]
    (assoc r :from from :to to
           :open-ends (cond-> #{} (open? :from from) (conj :from) (open? :to to) (conj :to)))))

(defn merge-segments
  "Folds newly read segments into the remembered map id -> segment. A match (same line, touching range) keeps the
  remembered id and state and takes the union of the ranges, so it never shrinks; an end stays open only while no read
  has seen past it. A read spanning two remembered segments (a crossing cut earlier, not in this read) is dropped. New segments start :open."
  [remembered news]
  (reduce (fn [mem n]
            (let [hit (filter #(overlaps? % n) (vals mem))]
              (cond (next hit) mem
                    (seq hit) (assoc mem (:id (first hit)) (merge-one (first hit) n))
                    :else (assoc mem (:id n) (assoc n :state :open)))))
          remembered news))

(defn gap
  "Manhattan distance from cell [x y z] to the segment."
  [seg [x y z]]
  (let [[a l] (if (= :x (:axis seg)) [x z] [z x])]
    (+ (js/Math.abs (- y (:y seg))) (js/Math.abs (- l (:line seg)))
       (max 0 (- (:from seg) a) (- a (:to seg))))))

(defn touch? [a b]
  (cond (= a b) false
        (and (= (:axis a) (:axis b)) (= (:line a) (:line b))) (and (= (:y a) (:y b)) (<= (:from a) (:to b)) (<= (:from b) (:to a)))
        (= (:axis a) (:axis b)) false
        :else (and (= (:y a) (:y b))
                   (<= (:from a) (:line b) (:to a)) (<= (:from b) (:line a) (:to b)))))

(defn order
  "The segments as a tree from entry cell [x y z]: BFS over shared crossings from the nearest segment.
  {:root id :order [ids] :parent {id id} :children {id [ids]} :leaves [ids] :unreached [ids]}."
  [segs entry]
  (let [segs (vec (sort-by :id segs))
        root (first (sort-by (juxt #(gap % entry) :id) segs))]
    (if-not root
      {:order [] :leaves [] :parent {} :children {} :unreached []}
      (loop [queue [root] seen #{(:id root)} order [] parent {}]
        (if-let [cur (first queue)]
          (let [kids (->> segs (remove #(seen (:id %))) (filter #(touch? cur %)))]
            (recur (into (subvec queue 1) kids) (into seen (map :id kids)) (conj order (:id cur))
                   (into parent (map (fn [k] [(:id k) (:id cur)]) kids))))
          (let [children (update-vals (group-by parent order) #(vec (sort %)))]
            {:root (:id root) :order order :parent parent
             :children (into {} (for [id order] [id (get children id [])]))
             :leaves (vec (remove children order))
             :unreached (vec (remove (set order) (map :id segs)))}))))))

(defn falling? [n] (boolean (and n (or (#{"gravel" "sand" "red_sand"} n) (re-find #"_concrete_powder$" n)))))
(defn wood? [n] (boolean (and n (or (planks? n) (fence? n) (re-find #"_(log|wood)$" n)))))

(defn kind
  "The class a block is taken as, nil when it is not a target."
  [n]
  (cond (wood? n) :wood
        (= n "cobweb") :cobweb
        (rail? n) :rail
        (and n (re-find #"_ore$" n)) :ore
        (= n "spawner") :spawner))

(def faces rules/neighbour-deltas)

(defn why-kept
  "Why the wood at cell must stay (:lava :bridge :falling), nil when it may go. floor? is a cell of the corridor floor."
  [block-at cell floor?]
  (let [[x y z] cell
        beside (fn [[dx dy dz]] (block-at [(+ x dx) (+ y dy) (+ z dz)]))
        below (beside [0 -1 0])]
    (cond (some #(= "lava" (beside %)) faces) :lava
          (and floor? (or (nil? below) (bs/air below) (contains? #{"water" "lava"} below))) :bridge
          (falling? (beside [0 1 0])) :falling)))

(defn targets
  "What to take in a segment, from the seen blocks: {:targets [{:pos :name :class}] :kept [{:pos :why}]}.
  Classes :wood (planks, fences, logs in the 3x3 corridor and its floor row), :cobweb, :rail, :ore (exposed to air),
  :spawner; :take limits them (default all). Wood that is next to lava, floors a bridge or holds up gravel or sand is kept,
  never a target. Ordered outward from the end nearest :entry [x y z] (default :from), a cobweb on the walk line first
  within a step."
  [blocks {:keys [axis y line from to] :as _seg} {:keys [entry take block-at] :or {take #{:wood :cobweb :rail :ore :spawner}}}]
  (let [block-at (or block-at (index blocks))
        ea (when entry (if (= axis :x) (first entry) (nth entry 2)))
        start (if (and ea (< (js/Math.abs (- ea to)) (js/Math.abs (- ea from)))) to from)
        cells (for [a (range from (inc to)) w (range -2 3) h (range -1 4)
                    :let [cell (at axis y line a) cell (update cell 1 + h) cell (if (= axis :x) (update cell 2 + w) (update cell 0 + w))
                          n (block-at cell)
                          k (kind n)]
                    :when k
                    :let [inside? (and (<= (js/Math.abs w) 1) (<= h 2))]
                    :when (case k :wood inside? true)
                    :when (or (not= k :ore) (some (fn [[dx dy dz]] (bs/air (block-at (mapv + cell [dx dy dz])))) faces))]
                {:a a :w w :h h :cell cell :name n :class k})
        kept (for [c cells :when (= :wood (:class c))
                   :let [why (why-kept block-at (:cell c) (= -1 (:h c)))]
                   :when why]
               [c why])
        kept-cells (set (map (comp :cell first) kept))
        pos (fn [[x yy z]] {:x x :y yy :z z})]
    {:targets (->> cells
                   (remove #(kept-cells (:cell %)))
                   (filter #(take (:class %)))
                   (sort-by (fn [{:keys [a w h cell] k :class}]
                             [(js/Math.abs (- a start)) (if (and (= k :cobweb) (zero? w) (<= 0 h 1)) 0 1) cell]))
                   (mapv (fn [{:keys [cell name] k :class}] {:pos (pos cell) :name name :class k})))
     :kept (mapv (fn [[c why]] {:pos (pos (:cell c)) :why why}) kept)}))
