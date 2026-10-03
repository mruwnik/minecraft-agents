(ns dashboard.villages-view
  "What the villages page says, as plain data and strings. Ported from tools/dashboard/villages.html."
  (:require [clojure.string :as str]))

(defn age-text [now-ms iso]
  (let [s (max 0 (js/Math.floor (/ (- now-ms (js/Date.parse iso)) 1000)))]
    (cond
      (< s 60) (str s "s ago")
      (< s 3600) (str (quot s 60) "m ago")
      :else (str (quot s 3600) "h ago"))))

(defn short-uuid [uuid] (subs (str uuid) 0 (min 8 (count (str uuid)))))

(defn trade-label [trade]
  (if-not trade
    ""
    (->> [(:output trade) (:enchant trade)
          (when (:level trade) (str (when (:atLeast trade) "at least ") "level " (:level trade)))]
         (filter some?)
         (remove #(and (string? %) (empty? %)))
         (str/join " · "))))

(defn village-haystack [{:keys [name kind population workspaces]}]
  (str/lower-case
   (str name " " kind " "
        (str/join " " (for [r (:roles population)] (str (:id r) " " (:profession r)))) " "
        (str/join " " (for [w workspaces] (str (:id w) " " (:profession w) " " (get-in w [:trade :output]) " " (get-in w [:trade :enchant])))))))

(defn village-matches? [village query]
  (str/includes? (village-haystack village) (str/lower-case (str/trim (or query "")))))

(defn filter-villages [villages query]
  (filterv #(village-matches? % query) villages))

(defn population-text [{:keys [population fresh observed lastObservedPopulation]}]
  (let [target (:target population)]
    (cond
      (nil? population) "population plan unknown"
      fresh (str (or observed "unknown") " / " target " residents")
      (some? lastObservedPopulation) (str "last observed " lastObservedPopulation " / " target)
      :else (str "unknown / " target " residents"))))

(defn state-text [{:keys [state]}] (str/replace (str state) "-" " "))

(defn housing-lines [{:keys [fresh housing]}]
  (if-not fresh
    [{:text "housing unknown until a fresh inspection"}]
    (let [{:keys [usableBeds requiredBeds state shelter residentBedCapacity residentBeds missingCells unknownCells]} housing]
      (vec
       (concat
        [{:text (str (or usableBeds "?") " reachable beds / " (or requiredBeds "?") " needed · " state)}
         {:text (str "enclosure " (:status shelter) " · minimum light " (or (:lightMinimum shelter) "unknown")
                     " · beds reachable from residents " (or residentBedCapacity "unknown"))
          :muted true}]
        (when (seq residentBeds)
          [{:text (str "last-known resident bed reachability: "
                       (str/join "; " (for [r residentBeds] (str (short-uuid (:uuid r)) " " (count (:beds r)) " bed(s)"))))
            :muted true}])
        (when (or (pos? (or missingCells 0)) (pos? (or unknownCells 0)))
          [{:text (str (or missingCells 0) " missing cells · " (or unknownCells 0) " unloaded cells") :muted true}])
        (when (seq (:issues shelter))
          [{:text (str/join "; " (:issues shelter)) :muted true}]))))))

(defn status-with-last [{:keys [status lastStatus]}]
  (if (and (= status "unknown") (some? lastStatus) (not= lastStatus "unknown"))
    (str "unknown now (last " lastStatus ")")
    status))

(defn association-text [{:keys [uuid status reason basis]}]
  (str (short-uuid uuid) ": " status (when reason (str " (" reason ")")) (when basis (str " · " basis))))

(defn workstation-note [workspaces {:keys [id workstation]}]
  (if-let [w (first (filter #(= id (:id %)) workspaces))]
    (str "workspace " (:status w)
         (when (seq (:associations w))
           (str ": " (str/join ", " (for [a (:associations w)] (str (short-uuid (:uuid a)) " " (:status a)))))))
    (str "workstation local coordinates " (str/join ", " workstation) "; association unknown")))

(defn stock-note [stock]
  (str "merchant stock: "
       (str/join "; " (for [{:keys [uuid status lastStatus restock]} stock]
                        (str (short-uuid uuid) " " status
                             (when (and (= status "unknown") (some? lastStatus) (not= lastStatus "unknown")) (str " (last " lastStatus ")"))
                             " · restock " restock)))))

(defn role-row [workspaces {:keys [id profession observed lastObserved required status trade workstation stock] :as role}]
  {:title (str id " · " profession)
   :count (str (or observed (if (some? lastObserved) (str "last " lastObserved) "?")) " / " required)
   :status status
   :notes (vec (concat
                (when trade [(str "desired offer: " (trade-label trade))])
                (when workstation [(workstation-note workspaces role)])
                (when (seq stock) [(stock-note stock)])))})

(defn role-rows [{:keys [roles workspaces]}]
  (mapv #(role-row workspaces %) roles))

(defn workspace-row [{:keys [id profession at localAt block trade status associations] :as w}]
  (let [expected (trade-label trade)
        where (if at
                (str (:x at) ", " (:y at) ", " (:z at))
                (str "local " (if localAt (str/join "," localAt) "unknown") " (world location not observed)"))]
    {:title (str id " · " profession)
     :where (str where " · " (or block "block unknown") " · " (if (str/blank? expected) "any offer" expected))
     :status status
     :status-text (status-with-last w)
     :association (if (seq associations)
                    (str/join "; " (map association-text associations))
                    "No exact UUID workstation association was verified.")}))

(defn evidence-lines [now-ms {:keys [error observedAt fresh unknownResidents restock surplus]}]
  (vec
   (concat
    (when error [{:text error :cls "state unknown"}])
    [{:text (if observedAt
              (str (if fresh "fresh" "stale") " snapshot · observed " (age-text now-ms observedAt))
              "not inspected yet")
      :cls "muted"}
     {:text (if (nil? unknownResidents)
              "current uncertainty unknown"
              (str unknownResidents " residents with stale or unknown identity facts"))
      :cls "muted"}
     {:text (or restock "unknown") :cls "muted"}]
    (when (some? surplus)
      [{:text (str surplus " observed surplus residents; nobody is removed automatically") :cls "muted"}]))))

(defn member-text [now-ms {:keys [profession uuid lockVerifiedAt lastSeenAt offersObservedAt]}]
  (str profession ": " (short-uuid uuid) " · "
       (if lockVerifiedAt "trade lock verified" "lock unverified")
       " · last seen " (if lastSeenAt (age-text now-ms lastSeenAt) "unknown")
       " · " (if offersObservedAt (str "offers " (age-text now-ms offersObservedAt)) "offers unobserved")))

(defn map-link-text [{:keys [bounds]}]
  (if bounds (str "map footprint " (:width bounds) " × " (:depth bounds)) "show map marker"))

(defn status-text [n clock error]
  (str n " places · refreshed " clock (when error (str " · " error))))

(defn empty-text [total]
  (if (pos? total) "No villages match this filter." "No saved villages or village inspections yet."))
