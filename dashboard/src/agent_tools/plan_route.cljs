(ns agent-tools.plan-route
  "`plans route`: the rail plan between two cells over the ground a body has seen. Reads the seen blocks and the zones
  (cells in another owner's zone that does not allow :place are not ground), asks plan.rail-route/connect for the way and
  plan.rail/layout (inside connect) for the cells; the plan carries the tunnel and bridge :spans rail-line builds first.
  The route grades the ground: it cuts, fills, tunnels and bridges where that costs less than going round, each block
  priced as jobs.lib.cost prices it (route-prices)."
  (:require [clojure.string :as str]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.farm :as farm]
            [jobs.lib.cost.table :as table]
            [jobs.lib.cost.value :as value]
            [jobs.lib.zone-file :as zone-file]
            [plan.rail :as rail]
            [plan.rail-route :as route]
            [plan.surface :as surface]
            [jobs.lib.surface :as ground]))

(def depth-over-ends "Blocks above and below the ends' levels a column is read in." 6)

(defn cell-arg
  "[x y z] of an \"x,y,z\" option, or nil."
  [text]
  (let [cell (when (string? text) (mapv js/Number (str/split text #",")))]
    (when (and (= 3 (count cell)) (every? js/Number.isSafeInteger cell)) cell)))

(defn limit-arg
  "The span limit of an option text: {:limit n} for an integer from 0, {:limit nil} (no limit but the price) for no text,
  nil when it is neither."
  [text]
  (if (nil? text)
    {:limit nil}
    (let [n (js/Number text)]
      (when (and (js/Number.isSafeInteger n) (<= 0 n)) {:limit n}))))

(defn nodes-arg
  "The search's node budget of an option text: a positive integer, route/max-nodes for no text, nil otherwise."
  [text]
  (if (nil? text)
    route/max-nodes
    (let [n (js/Number text)]
      (when (and (js/Number.isSafeInteger n) (pos? n)) n))))

(defn in-box? [[[x0 y0 z0] [x1 y1 z1]] [x y z]] (and (<= x0 x x1) (<= y0 y y1) (<= z0 z z1)))

(defn in-launch-run?
  "Whether cell is the end's or on a straight run of n cells from it along an axis, within the levels a level rail there
  may stand at (one off the end's, and its head cell)."
  [[ex ey ez] n [x y z]]
  (and (<= (dec ey) y (+ ey 2))
       (or (and (= z ez) (<= (abs (- x ex)) n)) (and (= x ex) (<= (abs (- z ez)) n)))))

(defn zone-refuses
  "(fn [[x y z]]) -> whether (memoised) the cell is claimed for action (:place, :dig): in a zone of the maker by, or of
  another owner that does not let others do it, or in one of planned (a collection of the cell sets other plans want).
  A route whose end lies in a claim of the maker or in a plan touches that claim already: the cells of the end's launch
  run (ends: [[cell n] ...]) in that same claim are not refused."
  [zones by action planned ends]
  (let [claims (concat (into [] (comp (filter (fn [{:keys [owner allow]}] (or (= owner by) (not (contains? (set allow) action)))))
                                      (map (fn [{:keys [owner min max]}] {:holds? #(in-box? [min max] %) :own? (= owner by)})))
                             zones)
                       (map (fn [cells] {:holds? #(contains? cells %) :own? true}) planned))
        held-by (fn [cell] (filter #((:holds? %) cell) claims))
        exempt (into [] (comp (mapcat (fn [[end n]] (for [c (held-by end) :when (:own? c)] [c end n]))))
                     ends)]
    (memoize (fn [cell]
               (boolean (some (fn [c] (not-any? (fn [[ec end n]] (and (identical? c ec) (in-launch-run? end n cell))) exempt))
                              (held-by cell)))))))

(defn height-fn
  "(fn [x z]) -> the y a rail would have in that column: on the seen ground top within the ends' levels, nil when unknown,
  fluid or claimed for :place (zone-refuses). name-at takes {:x :y :z}, returns the seen block name or nil."
  [name-at [_ fy _ :as from] [_ ty _ :as to] zones by planned runs]
  (let [mid (quot (+ fy ty) 2)
        depth (+ depth-over-ends (abs (- fy ty)))
        refused? (zone-refuses zones by :place planned [[from (first runs)] [to (second runs)]])
        name-fn (fn [pos] (let [n (name-at pos)] (if (and n (or (re-find surface/on-ground n) (rail/rail-name? n))) "air" n)))
        memo (volatile! {})]
    (fn [x z]
      (let [k [x z]
            y (if (contains? @memo k)
                (@memo k)
                (let [top (ground/column-top name-fn x z mid depth)
                      y (when top (inc top))]
                  (vswap! memo assoc k y)
                  y))]
        (when (and y (not (refused? [x y z])) (not (refused? [x (inc y) z])))
          y)))))

(defn water-fn
  "(fn [x z]) -> the least height a rail may have over a column height-fn knows no ground of, else nil: one above the
  top water within the ends' levels (its bed goes in the top water cell), or one above the window's bottom when every
  cell from the ends' level down to it was seen open (a chasm: a player bridges it)."
  [name-at [_ fy _] [_ ty _]]
  (let [mid (quot (+ fy ty) 2)
        depth (+ depth-over-ends (abs (- fy ty)))
        low (- mid depth)]
    (fn [x z]
      (loop [y (+ mid depth) seen-open? true]
        (if (> y low)
          (let [n (name-at {:x x :y y :z z})
                unseen? (and (nil? n) (<= y mid))]
            (cond
              (or (nil? n) (ground/open? n) (ground/canopy? n) (re-find surface/on-ground n) (rail/rail-name? n)) (recur (dec y) (and seen-open? (not unseen?)))
              (= "water" n) (inc y)))
          (when seen-open? (inc y)))))))

(defn route-prices
  "The :prices of plan.rail-route/connect, in seconds as jobs.lib.cost prices them: a cell of line its rail's worth and a
  step; a dug block its dig time and a step (an unseen one as stone; nil where claimed for :dig (zone-refuses) or for a
  block never dug); a placed block the levelling price of one (jobs.lib.cost.farm/earthwork-s; nil where claimed for
  :place); a tunnel cell its share of a torch every torch-gap cells."
  [name-at zones by planned ends]
  (let [step (value/per-block)
        open? (fn [n] (or (ground/open? n) (re-find surface/on-ground n) (rail/rail-name? n)))
        no-dig? (zone-refuses zones by :dig planned ends)
        no-place? (zone-refuses zones by :place planned ends)
        dig-s (memoize (fn [n] (if (open? n) 0 (some-> (table/dig-cost n) (+ step)))))]
    {:cell (+ (:value (value/item-value [{:name "rail" :count 1}])) step)
     :dig (fn [[x y z :as pos]]
            (when-not (no-dig? pos)
              (dig-s (or (name-at {:x x :y y :z z}) "stone"))))
     :fill (let [s (farm/earthwork-s 1)] (fn [pos] (when-not (no-place? pos) s)))
     :dark (/ (:cost (value/light-cost {:carried 1})) cost/torch-gap)}))

(defn zone-list
  "The zones of the zone file text (nil text: none); a file that does not parse is a refusal text."
  [text]
  (if (nil? text)
    {:zones []}
    (let [parsed (zone-file/parse text)]
      (if (seq (:errors parsed)) {:error (str "zones.edn does not parse: " (first (:errors parsed)))} {:zones (:value parsed)}))))

(defn launch-runs
  "The straight cells a line's first and last leg run before a turn ([start end], plan.rail-route/connect's near and far)
  for opts :launch and :launch-ends (nil: plan.rail/defaults)."
  [{:keys [launch launch-ends]}]
  (let [{:keys [launch launch-ends]} (merge rail/defaults (cond-> {} launch (assoc :launch launch) launch-ends (assoc :launch-ends launch-ends)))
        near (+ launch 3)]
    [near (if (= :both launch-ends) near 3)]))

(defn run-free?
  "Whether n cells straight on from [x y z] heading [dx dz] each have known ground (height, (fn [x z]) -> y) within a block
  of y: a cut or a fill keeps the rail level there."
  [height [x y z] [dx dz] n]
  (every? (fn [i] (some-> (height (+ x (* i dx)) (+ z (* i dz))) (- y) abs (<= 1))) (range 1 (inc n))))

(defn hemmed-ends
  "The ends, of [[:from from n] [:to to n]], with no heading along which n cells run free (run-free?)."
  [height ends]
  (filterv (fn [[_ cell n]] (not-any? #(run-free? height cell % n) [[1 0] [-1 0] [0 1] [0 -1]])) ends))

(defn end-text [[end cell n]]
  (str "--" (name end) " " (pr-str cell) " has no heading with " n " straight cells of seen ground within a block of its level"))

(defn no-route-hint [{:keys [tunnel bridge spent nodes hemmed]}]
  (str (cond spent (str "the search ran out of its " nodes " nodes (--nodes) before it found a rail line")
             (seq hemmed) (str "no rail line can leave its ends: " (str/join "; " (map end-text hemmed))
                               " (a line's first and last cells run straight and level for its launch groups before a turn); move that end to where they do")
             :else "no rail line fits the seen ground")
       " between the cells (longest tunnel " (or tunnel "any") ", bridge " (or bridge "any")
       "). Walk and look along the way: unseen columns are not ground, zones and other plans' cells are not touched, and a line needs room for its launch groups at both ends"))

(defn bad-route-text
  "Why the cheapest route cannot be laid (plan.rail/layout's error): a :touching one names the straight launch runs at its
  ends, which make a hemmed-in end loop out and back beside itself."
  [{:keys [error at]} [near far]]
  (if (= :touching error)
    (str "the cheapest route runs beside itself" (some->> at (str " at ")) " (rails side by side join): its first " near
         " and last " far " cells must run straight and level before a turn, so an end hemmed in loops round; start and end where "
         near " cells run free toward the other end")
    (str "the cheapest route cannot be laid: " (name error) (some->> at (str " at ")))))

(defn route-plan
  "{:plan <plan map> :summary {...}} for the route from to between the two cells, or {:error reason :message text}.
  opts: :id, :by, :tunnel, :bridge, :launch (nil: layout's default), :nodes (the search's node budget), :planned (the
  cells other plans want, one set of [x y z] per plan)."
  [{:keys [id by tunnel bridge launch nodes planned] :or {nodes route/max-nodes planned []}} from to name-at zones]
  (let [opts (cond-> {:tunnel tunnel :bridge bridge} launch (assoc :launch launch))
        [near far :as runs] (launch-runs opts)
        height (height-fn name-at from to zones by planned runs)
        ends [[from near] [to far]]
        routed (route/connect height from to
                              (assoc opts :max-nodes nodes :gap (water-fn name-at from to) :prices (route-prices name-at zones by planned ends)))]
    (if (:error routed)
      {:error (if (= :no-route (:error routed)) :no-route :bad-route)
       :message (if (= :no-route (:error routed))
                  (no-route-hint {:tunnel tunnel :bridge bridge :spent (:spent routed) :nodes nodes
                                  :hemmed (when-not (:spent routed) (hemmed-ends height [[:from from near] [:to to far]]))})
                  (bad-route-text routed runs))}
      (let [laid (rail/layout (:waypoints routed) (dissoc opts :tunnel :bridge))
            spans (mapv #(select-keys % [:kind :from :to]) (:spans routed))]
        (if (:error laid)
          {:error :bad-route :message (str "the route cannot be laid: " (name (:error laid)))}
          {:plan (cond-> {:id id :note (str "rail line " (pr-str from) " to " (pr-str to)) :parts (:parts laid)}
                   (seq spans) (assoc :spans spans))
           :summary {:from from :to to :waypoints (:waypoints routed) :spans (:spans routed)
                     :materials (:materials laid)
                     :next (str "plans check " id " judges it; jobs submit '(jobs.build.rail-line {:plan \"" id "\" :worth <seconds>})' builds it"
                                (when (seq spans) ", cutting the tunnels and bridges first"))}})))))
