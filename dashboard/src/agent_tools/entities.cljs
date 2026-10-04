(ns agent-tools.entities
  "A bounded, read-only projection of a body's short-lived entity cache."
  (:require [clojure.string :as str]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(def usage "entities.mjs BODY --world WORLD [--type TYPE] [--player NAME] [--dimension DIM] [--center X,Y,Z] [--radius N] [--limit N] [--offset N] [--raw] [--worlds DIR --state LEGACY_PARENT]")
(def default-radius 64)
(def default-limit 10)
(def max-radius 512)
(def max-limit 50)
(def max-offset 10000)
(def stale-origin-ms 90000)
(def future-origin-skew-ms 5000)

(defn- number [value]
  (let [n (js/Number value)] (when (js/Number.isFinite n) n)))

(defn- dimension-name [value]
  (let [value (str value)]
    (get {"minecraft:overworld" "overworld"
          "minecraft:the_nether" "the_nether"
          "minecraft:the_end" "the_end"} value value)))

(defn- parse-center [text]
  (let [parts (str/split (str text) #"," -1)
        coords (mapv number parts)]
    (when-not (and (= 3 (count coords)) (every? #(not (str/blank? %)) parts) (every? some? coords))
      (throw (data/fail :invalid-center "--center must be three comma-separated finite coordinates: X,Y,Z")))
    (zipmap [:x :y :z] coords)))

(defn options [argv]
  (let [{:keys [positionals values]} (map-tool/parse-options argv
           (merge {:world {:type "string"} :state {:type "string"} :worlds {:type "string"}
                   :type {:type "string"} :player {:type "string"} :dimension {:type "string"}
                   :center {:type "string"} :radius {:type "string"} :limit {:type "string"} :offset {:type "string"}
                   :raw {:type "boolean"}}))
        [body & extra] positionals
        allowed #{:world :worlds :state :type :player :dimension :center :radius :limit :offset :raw}
        radius (number (or (:radius values) default-radius))
        limit (number (or (:limit values) default-limit))
        offset (number (or (:offset values) 0))
        center (when (:center values) (parse-center (:center values)))
        dimension (when (:dimension values) (dimension-name (:dimension values)))]
    (when (or (nil? body) (seq extra) (not (re-matches #"^[A-Za-z0-9_-]{1,64}$" body)))
      (throw (data/fail :invalid-body usage)))
    (doseq [key (keys values)]
      (when-not (allowed key)
        (throw (data/fail :invalid-option (str "--" (name key) " is not supported")))))
    (when-not (:world values)
      (throw (data/fail :invalid-world "give an explicit --world")))
    (when (and (:type values) (not (re-matches #"^[a-z0-9_.:-]{1,80}$" (:type values))))
      (throw (data/fail :invalid-type "--type must be an entity type such as zombie or player")))
    (when (and (:player values) (not (re-matches #"^[A-Za-z0-9_]{1,16}$" (:player values))))
      (throw (data/fail :invalid-player "--player must be a Minecraft username")))
    (when (and (:dimension values) (not (re-matches #"^(?:[a-z0-9_.-]+:)?[a-z0-9_./-]{1,64}$" (:dimension values))))
      (throw (data/fail :invalid-dimension "--dimension must be a dimension name")))
    (when-not (and (some? radius) (<= 0 radius max-radius))
      (throw (data/fail :invalid-radius "--radius must be between 0 and 512 blocks")))
    (when-not (and (some? limit) (js/Number.isInteger limit) (<= 1 limit max-limit))
      (throw (data/fail :invalid-limit "--limit must be an integer from 1 to 50")))
    (when-not (and (some? offset) (js/Number.isInteger offset) (<= 0 offset max-offset))
      (throw (data/fail :invalid-offset "--offset must be an integer from 0 to 10000")))
    (let [ctx (data/context (select-keys values [:state :worlds :world]))]
      {:ctx ctx :body body :type (:type values) :player (:player values)
       :dimension dimension :center center :radius radius :limit limit :offset offset :raw? (true? (:raw values))})))

(defn- distance-squared [a b]
  (let [dx (- (:x a) (:x b)) dy (- (:y a) (:y b)) dz (- (:z a) (:z b))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- round-tenth [n] (/ (js/Math.round (* n 10)) 10))

(defn- valid-position? [pos]
  (and (map? pos)
       (every? (fn [axis]
                 (let [value (get pos axis)]
                   (and (number? value) (js/Number.isFinite value))))
               [:x :y :z])))

(defn- fresh-pose-origin [request now]
  (let [{:keys [world-dir world]} (:ctx request)
        file (when (and world-dir (string? (:body request)))
               (.join path world-dir "agents" (:body request) "view" "pose.json"))]
    (try
      (when (and file (.existsSync fs file) (<= (.-size (.statSync fs file)) 1048576))
        (let [pose (js->clj (js/JSON.parse (.readFileSync fs file "utf8")) :keywordize-keys true)
              seen (:t pose)
              dimension (:dimension pose)
              pos (:pos pose)]
          (when (and (= world (:world pose))
                     (= "online" (:status pose))
                     (string? dimension) (not (str/blank? dimension))
                     (number? seen) (js/Number.isFinite seen)
                     (<= (- future-origin-skew-ms) (- now seen) stale-origin-ms)
                     (valid-position? pos))
            {:pos pos :dimension (dimension-name dimension) :observed-at seen :source :pose})))
      (catch :default _ nil))))

(defn- compact [now entity]
  (let [age (max 0 (- now (:observed-at entity)))
        pos (:pos entity)]
    (cond-> {:type (:type entity)
             :pos (mapv round-tenth ((juxt :x :y :z) pos))
             :age-ms age}
      (:uuid entity) (assoc :uuid (:uuid entity))
      (nil? (:uuid entity)) (assoc :key (:key entity))
      (:username entity) (assoc :player (:username entity)))))

(defn project
  "Filter only the snapshot rows and timestamps the body supplied. Never refreshes observations."
  [request snapshot]
  (when-not (map? snapshot)
    (throw (data/fail :bad-response "the body returned an invalid entity snapshot")))
  (if (false? (:ok snapshot))
    (select-keys snapshot [:ok :reason :error :online? :body :world])
    (let [now (:now snapshot)
          ttl (:ttl-ms snapshot)
          rows (vec (:entities snapshot))
          radius (or (:radius request) default-radius)
          limit (or (:limit request) default-limit)
          offset (or (:offset request) 0)
          self (->> rows
                    (filter #(and (:self? %)
                                  (number? (:expires-at %)) (> (:expires-at %) now)
                                  (number? (:observed-at %))
                                  (valid-position? (:pos %))
                                  (string? (:dimension %))))
                    (sort-by :observed-at >)
                    first)
          pose-origin (when (and (nil? self) (true? (:online? snapshot))
                                 (or (nil? (:center request)) (nil? (:dimension request))))
                        (fresh-pose-origin request now))
          origin (or self pose-origin)
          center (or (:center request) (:pos origin))
          dimension (or (:dimension request) (:dimension origin))]
      (when-not (and (number? now) (js/Number.isFinite now) (number? ttl) (pos? ttl))
        (throw (data/fail :bad-response "the entity snapshot is missing its server timestamps")))
      (when-not center
        (throw (data/fail :origin-unavailable "no fresh self position; pass --center X,Y,Z")))
      (when-not dimension
        (throw (data/fail :dimension-unavailable "no cached self dimension; pass --dimension with --center")))
      (when (and (:dimension request) origin (not (:center request))
                 (not= (:dimension request) (:dimension origin)))
        (throw (data/fail :dimension-origin-mismatch "--dimension differs from the cached body dimension; pass --center in the requested dimension")))
      (let [radius-squared (* radius radius)
            matches (->> rows
                         (filter #(and (not (:self? %))
                                       (number? (:expires-at %)) (> (:expires-at %) now)
                                       (number? (:observed-at %)) (:pos %)
                                       (= dimension (:dimension %))
                                       (<= (distance-squared center (:pos %)) radius-squared)
                                       (or (nil? (:type request)) (= (:type request) (:type %)))
                                       (or (nil? (:player request)) (= (:player request) (:username %)))))
                         (sort-by (fn [entity] [(distance-squared center (:pos entity)) (or (:uuid entity) (:key entity) "")]))
                         vec)
            selected (vec (take limit (drop offset matches)))
            shown (if (:raw? request) selected (mapv #(compact now %) selected))
            next-offset (+ offset (count selected))
            more? (or (< next-offset (count matches)) (true? (:truncated? snapshot)))]
        (if (:raw? request)
          (cond-> {:ok true :world (:world snapshot) :body (:body snapshot) :source :entity-cache
                   :online? (true? (:online? snapshot)) :now now :ttl-ms ttl
                   :dimension dimension :center (mapv round-tenth ((juxt :x :y :z) center))
                   :radius radius :total (count matches) :offset offset :returned (count selected)
                   :items shown :more? more? :cached-count (:cached-count snapshot)
                   :snapshot-cap-bytes (:snapshot-cap-bytes snapshot)}
            (and more? (seq selected)) (assoc :next-offset next-offset)
            (true? (:truncated? snapshot)) (assoc :dropped (:dropped snapshot)))
          (cond-> {:ok true :dimension dimension
                   :center (mapv round-tenth ((juxt :x :y :z) center))
                   :total (count matches) :items shown}
            (false? (:online? snapshot)) (assoc :online? false)
            more? (assoc :more? true)
            (and (< next-offset (count matches)) (seq selected)) (assoc :next-offset next-offset)))))))
