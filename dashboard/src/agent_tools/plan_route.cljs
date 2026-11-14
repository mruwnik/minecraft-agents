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

(defn zone-refuses
  "(fn [[x y z]]) -> whether (memoised) another owner's zone that does not let others do action (:place, :dig) holds the cell."
  [zones by action]
  (let [boxes (into [] (comp (filter (fn [{:keys [owner allow]}] (and (not= owner by) (not (contains? (set allow) action)))))
                             (map (juxt :min :max)))
                    zones)]
    (memoize (fn [[x y z]] (boolean (some (fn [[[x0 y0 z0] [x1 y1 z1]]] (and (<= x0 x x1) (<= y0 y y1) (<= z0 z z1))) boxes))))))

(defn height-fn
  "(fn [x z]) -> the y a rail would have in that column: on the seen ground top within the ends' levels, nil when unknown,
  fluid or in a zone the body may not build in. name-at takes {:x :y :z}, returns the seen block name or nil."
  [name-at [_ fy _] [_ ty _] zones by]
  (let [mid (quot (+ fy ty) 2)
        depth (+ depth-over-ends (abs (- fy ty)))
        refused? (zone-refuses zones by :place)
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
  "(fn [x z]) -> the least height a rail may have over the column when its top within the ends' levels is seen water
  (one above the water: its bed goes in the top water cell), else nil."
  [name-at [_ fy _] [_ ty _]]
  (let [mid (quot (+ fy ty) 2)
        depth (+ depth-over-ends (abs (- fy ty)))]
    (fn [x z]
      (loop [y (+ mid depth)]
        (when (> y (- mid depth))
          (let [n (name-at {:x x :y y :z z})]
            (cond
              (or (nil? n) (ground/open? n) (ground/canopy? n) (re-find surface/on-ground n) (rail/rail-name? n)) (recur (dec y))
              (= "water" n) (inc y))))))))

(defn route-prices
  "The :prices of plan.rail-route/connect, in seconds as jobs.lib.cost prices them: a cell of line its rail's worth and a
  step; a dug block its dig time and a step (an unseen one as stone; nil in a zone that refuses :dig or for a block never
  dug); a placed block the levelling price of one (jobs.lib.cost.farm/earthwork-s; nil in a zone that refuses :place); a
  tunnel cell its share of a torch every torch-gap cells."
  [name-at zones by]
  (let [step (value/per-block)
        open? (fn [n] (or (ground/open? n) (re-find surface/on-ground n) (rail/rail-name? n)))
        no-dig? (zone-refuses zones by :dig)
        no-place? (zone-refuses zones by :place)
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

(defn no-route-hint [{:keys [tunnel bridge spent nodes]}]
  (str (if spent (str "the search ran out of its " nodes " nodes (--nodes) before it found a rail line") "no rail line fits the seen ground")
       " between the cells (longest tunnel " (or tunnel "any") ", bridge " (or bridge "any")
       "). Walk and look along the way: unseen columns are not ground, and a line needs room for its launch groups at both ends"))

(defn route-plan
  "{:plan <plan map> :summary {...}} for the route from to between the two cells, or {:error reason :message text}.
  opts: :id, :by, :tunnel, :bridge, :launch (nil: layout's default), :nodes (the search's node budget)."
  [{:keys [id by tunnel bridge launch nodes] :or {nodes route/max-nodes}} from to name-at zones]
  (let [opts (cond-> {:tunnel tunnel :bridge bridge} launch (assoc :launch launch))
        routed (route/connect (height-fn name-at from to zones by) from to
                              (assoc opts :max-nodes nodes :gap (water-fn name-at from to) :prices (route-prices name-at zones by)))]
    (if (:error routed)
      {:error (if (= :no-route (:error routed)) :no-route :bad-route)
       :message (if (= :no-route (:error routed))
                  (no-route-hint {:tunnel tunnel :bridge bridge :spent (:spent routed) :nodes nodes})
                  (str "the cheapest route cannot be laid: " (name (:error routed)) (some->> (:at routed) (str " at "))))}
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
