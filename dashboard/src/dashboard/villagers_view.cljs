(ns dashboard.villagers-view
  "What the villager roster page says, as plain data and strings. Ported from tools/dashboard/villagers.html."
  (:require [clojure.string :as str]))

(defn ago [now-ms iso]
  (let [s (max 0 (js/Math.floor (/ (- now-ms (js/Date.parse iso)) 1000)))]
    (cond
      (< s 60) (str s "s ago")
      (< s 3600) (str (quot s 60) "m ago")
      (< s 86400) (str (quot s 3600) "h ago")
      :else (str (quot s 86400) "d ago"))))

(defn records [roster]
  (vec (sort-by (juxt #(or (:profession %) "") #(str (:uuid %))) (vals (:villagers roster)))))

(defn haystack [{:keys [uuid profession age place offers]}]
  (str/lower-case (str uuid " " (or profession "") " " (or age "") " " (or (:name place) "") " " (pr-str (:items offers)))))

(defn record-matches? [record query]
  (str/includes? (haystack record) (str/lower-case (str/trim (or query "")))))

(defn filter-records [recs query]
  (filterv #(record-matches? % query) recs))

(defn summary-lines [now-ms {:keys [profession age level place lastSeenAt lastSeenBy]}]
  [(str (or profession "profession unknown") " · " (or age "age unknown"))
   (str "Lv " (or level "?"))
   (if (:name place) (str "place: " (:name place)) "place not assigned")
   (if lastSeenAt (str "seen " (ago now-ms lastSeenAt) " by " (or lastSeenBy "unknown")) "no sighting time")])

(defn offer-text [{:keys [outputItem inputItem1 inputItem2 nbTradeUses maximumNbTradeUses tradeDisabled]}]
  (let [enchants (str/join ", " (for [e (:enchants outputItem)] (str (:name e) " " (:lvl e))))
        input (str/join " + " (for [i [inputItem1 inputItem2] :when i] (str (:count i) " " (:name i))))]
    {:input (if (str/blank? input) "unknown cost" input)
     :output (str (or (:count outputItem) "?") " " (or (:name outputItem) "unknown")
                  (when-not (str/blank? enchants) (str " (" enchants ")")))
     :uses (str (or nbTradeUses "?") " / " (or maximumNbTradeUses "?") " uses" (when tradeDisabled " · disabled"))}))

(defn offers-heading [now-ms {:keys [observedAt observedBy]}]
  (str "Offers observed " (ago now-ms observedAt) " by " observedBy))

(defn detail-lines [now-ms {:keys [uuid lastPosition workstationObservation lockEvidence purchases]}]
  (vec
   (concat
    [{:text (str "UUID " uuid) :cls nil}]
    (when lastPosition
      [{:text (str "last coordinates " (.toFixed (:x lastPosition) 1) ", " (.toFixed (:y lastPosition) 1) ", " (.toFixed (:z lastPosition) 1))
        :cls "coord"}])
    (when workstationObservation
      [{:text (str "matching block observed at " (:x workstationObservation) "," (:y workstationObservation) "," (:z workstationObservation)
                   "; POI claim unverified")
        :cls nil}])
    (when lockEvidence
      [{:text (str "Trade lock verified " (ago now-ms (:at lockEvidence)) " by " (:by lockEvidence) ": " (:basis lockEvidence))
        :cls "proof"}])
    (when (seq purchases)
      [{:text (str "Confirmed purchases: "
                   (str/join "; " (for [p purchases] (str/trim (str (:boughtCount p) " " (or (:bought p) "item") " (offer " (:offer p) ")")))))
        :cls nil}]))))

(defn status-text [n clock] (str n " UUIDs · refreshed " clock))
