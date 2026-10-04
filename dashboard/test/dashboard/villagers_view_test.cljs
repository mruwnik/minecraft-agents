(ns dashboard.villagers-view-test
  (:require [clojure.string :as str]
            [cljs.test :refer [deftest is]]
            [dashboard.villagers-view :as vv]))

(def now (js/Date.parse "2026-01-10T12:00:00Z"))

(deftest ago
  (doseq [[iso expected] [["2026-01-10T11:59:30Z" "30s ago"] ["2026-01-10T11:50:00Z" "10m ago"]
                          ["2026-01-10T07:00:00Z" "5h ago"] ["2026-01-08T12:00:00Z" "2d ago"]]]
    (is (= expected (vv/ago now iso)))))

(deftest roster-records-sorted-by-profession-then-uuid
  (let [roster {:villagers {:b {:uuid "b" :profession "farmer"} :a {:uuid "a" :profession "farmer"}
                            :c {:uuid "c" :profession "armorer"} :d {:uuid "d"}}}]
    (is (= ["d" "c" "a" "b"] (map :uuid (vv/records roster))))
    (is (= [] (vv/records nil)))))

(def v {:uuid "u-1" :profession "librarian" :age "adult" :place {:name "library"}
        :offers {:items [{:outputItem {:name "book"}}]}})

(deftest record-matches
  (doseq [[q expected] [["" true] ["U-1" true] ["libr" true] ["adult" true] ["LIBRARY" true] ["book" true] ["zzz" false]]]
    (is (= expected (vv/record-matches? v q)) q)))

(deftest filter-records
  (is (= ["u-1"] (map :uuid (vv/filter-records [v {:uuid "x"}] " u-1 ")))))

(deftest summary-lines
  (doseq [[r expected] [[{:profession "farmer" :age "adult" :level 2 :place {:name "p"} :lastSeenAt "2026-01-10T11:00:00Z" :lastSeenBy "Bob"}
                         ["farmer · adult" "Lv 2" "place: p" "seen 1h ago by Bob"]]
                        [{} ["profession unknown · age unknown" "Lv ?" "place not assigned" "no sighting time"]]
                        [{:lastSeenAt "2026-01-10T11:00:00Z"} ["profession unknown · age unknown" "Lv ?" "place not assigned" "seen 1h ago by unknown"]]]]
    (is (= expected (vv/summary-lines now r)))))

(deftest offer-text
  (doseq [[offer expected] [[{:outputItem {:name "book" :count 1 :enchants [{:name "mending" :lvl 1} {:name "unbreaking" :lvl 3}]}
                              :inputItem1 {:count 5 :name "emerald"} :inputItem2 {:count 1 :name "book"}
                              :nbTradeUses 2 :maximumNbTradeUses 12 :tradeDisabled true}
                             {:input "5 emerald + 1 book" :output "1 book (mending 1, unbreaking 3)" :uses "2 / 12 uses · disabled"}]
                            [{} {:input "unknown cost" :output "? unknown" :uses "? / ? uses"}]]]
    (is (= expected (vv/offer-text offer)))))

(deftest detail-lines
  (is (= [{:text "UUID u" :cls nil}
          {:text "last coordinates 1.0, 2.5, -3.0" :cls "coord"}
          {:text "matching block observed at 1,2,3; POI claim unverified" :cls nil}
          {:text "Trade lock verified 1h ago by Bob: basis" :cls "proof"}
          {:text "Confirmed purchases: 2 emerald (offer 1); item (offer 2)" :cls nil}]
         (vv/detail-lines now {:uuid "u" :lastPosition {:x 1 :y 2.5 :z -3}
                               :workstationObservation {:x 1 :y 2 :z 3}
                               :lockEvidence {:at "2026-01-10T11:00:00Z" :by "Bob" :basis "basis"}
                               :purchases [{:boughtCount 2 :bought "emerald" :offer 1} {:offer 2}]}))))

(deftest offers-heading
  (is (= "Offers observed 1h ago by Bob" (vv/offers-heading now {:observedAt "2026-01-10T11:00:00Z" :observedBy "Bob"}))))

(deftest status-text
  (is (= "3 UUIDs · refreshed 10:00:00" (vv/status-text 3 "10:00:00"))))

(deftest expired-observations-are-hidden-and-old-engines-identified
  (is (= [] (vv/live-records {:villagers {"u" {:uuid "u" :t 10 :until 20}}} 20)))
  (is (= 1 (count (vv/live-records {:villagers {"u" {:uuid "u" :t 10 :until 20}}} 19))))
  (is (str/includes? (first (vv/capability-text [{:body {:name "Bob"} :status :unsupported}])) "restart")))
