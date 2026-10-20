(ns agent-tools.plan-route
  "`plans route`: the rail plan between two cells over the ground a body has seen. Reads the seen blocks and the zones
  (cells in another owner's zone that does not allow :place are not ground), asks plan.rail-route/connect for the way and
  plan.rail/layout (inside connect) for the cells; the plan carries the tunnel and bridge :spans rail-line builds first."
  (:require [clojure.string :as str]
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
  "The span limit of an option text (nil: 0, none), or nil when it is not an integer from 0 to 64."
  [text]
  (let [n (js/Number (or text 0))]
    (when (and (js/Number.isSafeInteger n) (<= 0 n 64)) n)))

(defn blocked-by-zone?
  "Whether another owner's zone that lets others not :place holds the cell."
  [zones by [x y z]]
  (some (fn [{:keys [min max owner allow]}]
          (and (not= owner by) (not (contains? (set allow) :place))
               (<= (nth min 0) x (nth max 0)) (<= (nth min 1) y (nth max 1)) (<= (nth min 2) z (nth max 2))))
        zones))

(defn height-fn
  "(fn [x z]) -> the y a rail would have in that column: on the seen ground top within the ends' levels, nil when unknown,
  fluid or in a zone the body may not build in. name-at takes {:x :y :z}, returns the seen block name or nil."
  [name-at [_ fy _] [_ ty _] zones by]
  (let [mid (quot (+ fy ty) 2)
        depth (+ depth-over-ends (abs (- fy ty)))
        name-fn (fn [pos] (let [n (name-at pos)] (if (and n (re-find surface/on-ground n)) "air" n)))
        memo (volatile! {})]
    (fn [x z]
      (let [k [x z]
            y (if (contains? @memo k)
                (@memo k)
                (let [top (ground/column-top name-fn x z mid depth)
                      y (when top (inc top))]
                  (vswap! memo assoc k y)
                  y))]
        (when (and y (not (blocked-by-zone? zones by [x y z])) (not (blocked-by-zone? zones by [x (inc y) z])))
          y)))))

(defn zone-list
  "The zones of the zone file text (nil text: none); a file that does not parse is a refusal text."
  [text]
  (if (nil? text)
    {:zones []}
    (let [parsed (zone-file/parse text)]
      (if (seq (:errors parsed)) {:error (str "zones.edn does not parse: " (first (:errors parsed)))} {:zones (:value parsed)}))))

(defn no-route-hint [{:keys [tunnel bridge]}]
  (str "no rail line fits the seen ground between the cells (tunnel " tunnel ", bridge " bridge "). Walk and look along the "
       "way (unseen columns are not ground), or allow --tunnel N through rock 2+ above the rail, --bridge N over a drop 2+ below"))

(defn route-plan
  "{:plan <plan map> :summary {...}} for the route from to between the two cells, or {:error reason :message text}.
  opts: :id, :by, :tunnel, :bridge, :launch (nil: layout's default)."
  [{:keys [id by tunnel bridge launch]} from to name-at zones]
  (let [opts (cond-> {:tunnel tunnel :bridge bridge} launch (assoc :launch launch))
        routed (route/connect (height-fn name-at from to zones by) from to opts)]
    (if (:error routed)
      {:error (if (= :no-route (:error routed)) :no-route :bad-route)
       :message (if (= :no-route (:error routed))
                  (no-route-hint {:tunnel tunnel :bridge bridge})
                  (str "the cheapest route cannot be laid: " (name (:error routed)) (some->> (:at routed) (str " at "))))}
      (let [laid (rail/layout (:waypoints routed) (dissoc opts :tunnel :bridge))
            spans (mapv #(select-keys % [:kind :from :to]) (:spans routed))]
        (if (:error laid)
          {:error :bad-route :message (str "the route cannot be laid: " (name (:error laid)))}
          {:plan (cond-> {:id id :note (str "rail line " (pr-str from) " to " (pr-str to)) :parts (:parts laid)}
                   (seq spans) (assoc :spans spans))
           :summary {:from from :to to :waypoints (:waypoints routed) :spans (:spans routed)
                     :materials (:materials laid)
                     :next (str "plans check " id " judges it; jobs submit '(jobs.build.rail-line {:plan \"" id "\"})' builds it"
                                (when (seq spans) ", cutting the tunnels and bridges first"))}})))))
