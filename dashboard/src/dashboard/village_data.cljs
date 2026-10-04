(ns dashboard.village-data
  "Village intentions are native world plans. Villagers are independent UUID world-map observations."
  (:require ["path" :as path]
            [clojure.string :as str]
            [dashboard.edn-data :as data]
            [plan.shape :as shape]))

(defn report-by-id [report kind id] (first (filter #(= id (:id %)) (get report kind))))

(defn roles [population report fresh]
  (mapv (fn [role]
          (let [found (report-by-id report :roles (:id role)) n (when (:uuids found) (count (:uuids found)))]
            {:id (:id role) :profession (:profession role) :required (get role :count 1)
             :observed (when fresh n) :lastObserved n :status (if fresh (get found :status "unknown") "unknown")
             :workstation (:workstation role) :trade (:trade role)
             :stock (mapv (fn [s] {:uuid (:uuid s) :status (if fresh (:status s) "unknown") :lastStatus (:status s)
                                  :observedAt (:observedAt s) :restock (get s :restock "not observed")}) (:stock found))}))
        (:roles population)))

(defn workspaces [population report fresh]
  (mapv (fn [workspace]
          (let [found (report-by-id report :workspaces (:id workspace))]
            {:id (:id workspace) :localAt (:at workspace) :profession (:profession workspace) :trade (:trade workspace)
             :at (:at found) :block (when fresh (:block found)) :status (if fresh (get found :status "unknown") "unknown")
             :lastStatus (get found :status "unknown") :uuids (if fresh (get found :uuids []) []) :lastUuids (get found :uuids [])
             :associations (mapv (fn [a] (assoc (select-keys a [:uuid :reason :source :basis :observedAt])
                                               :status (if fresh (:status a) "unknown") :lastStatus (:status a))) (:associations found))}))
        (concat (:workspaces population)
                (for [role (:roles population) :when (:workstation role)]
                  {:id (:id role) :at (:workstation role) :profession (:profession role) :trade (:trade role)}))))

(def unknown-housing {:state "unknown" :usableBeds nil :residentBedCapacity nil :requiredBeds nil :residentBeds []
                      :missingCells nil :unknownCells nil :shelter {:status "unknown" :issues []}})

(defn plan-projection [p world]
  (let [metadata (:metadata p)
        original (:legacy-place metadata)
        inspection (:legacy-inspection metadata)
        population (or (:population metadata) (:population inspection))
        report (:report inspection)
        [x y z] (:at p)]
    ;; Inspection metadata is historical evidence. A proposed plan, including
    ;; one with no cells yet, never claims current population or housing.
    {:name (:id p) :world world :kind "village" :x x :y y :z z :by (:by original) :note (:note p)
     :planId (:id p) :planStatus (:status p)
     :geometry (get metadata :geometry (if (seq (:parts p)) :planned :incomplete))
     :population population :blueprintId (get-in metadata [:source :id])
     :state (if (:observedAt inspection) "stale" (if population "not-inspected" "unplanned"))
     :fresh false :bounds nil :error nil :observedAt (:observedAt inspection) :observed nil
     :lastObservedPopulation (:population report) :unknownResidents nil :surplus nil
     :roles (roles population report false) :workspaces (workspaces population report false)
     :housing unknown-housing :restock "unknown; trade uses are last observed on merchant windows"
     ;; Map entities are never enrolled as plan members by this display.
     :members []}))

(defn checked-world [world]
  (when-not (and (string? world) (re-matches #"[A-Za-z0-9_-]{1,64}" world))
    (throw (js/Error. "invalid world"))) world)

(defn snapshot
  ([root places] (snapshot root places {:worlds (vec (distinct (keep :world places)))}))
  ([root _places {:keys [worlds worlds-dir]}]
   (try
     (let [results (for [world (map checked-world worlds)
                         :let [dir (.join path (or worlds-dir (.join path root "worlds")) world "plans")]
                         file (data/files dir #"[A-Za-z0-9_.-]+\.edn")]
                     (try
                       (let [id (str/replace file #"\.edn$" "") p (data/read-file (.join path dir file))
                             errors (shape/plan-errors p id)]
                         (when (seq errors) (throw (js/Error. (str/join "; " (map :error errors)))))
                         {:value (when (= :village (:kind p)) (plan-projection p world))})
                       (catch :default e {:error (str world "/" file ": " (ex-message e))})))
           errors (keep :error results)]
       {:villages (vec (sort-by (juxt :world :name) (keep :value results))) :unassigned []
        :error (when (seq errors) (str/join "; " errors))})
     (catch :default e {:villages [] :unassigned [] :error (str "village plans unavailable: " (ex-message e))}))))

(defn attach-status [places villages]
  (let [by-key (into {} (map (fn [v] [[(:world v) (:name v)] v])) villages)
        keys (set (map (juxt :world :name) places))]
    (into (mapv (fn [p] (if-let [v (get by-key [(:world p) (:name p)])] (assoc p :village v) p)) places)
          (for [v villages :when (and (not (contains? keys [(:world v) (:name v)]))
                                      (every? #(js/Number.isFinite (get v %)) [:x :y :z]))]
            (assoc (select-keys v [:name :world :kind :x :y :z :by]) :village v)))))
