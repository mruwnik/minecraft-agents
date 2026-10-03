(ns dashboard.villages-view-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.villages-view :as vv]))

(def now (js/Date.parse "2026-01-01T12:00:00Z"))

(deftest age-text
  (doseq [[iso expected] [["2026-01-01T12:00:00Z" "0s ago"]
                          ["2026-01-01T12:00:30Z" "0s ago"]
                          ["2026-01-01T11:59:15Z" "45s ago"]
                          ["2026-01-01T11:55:00Z" "5m ago"]
                          ["2026-01-01T09:00:00Z" "3h ago"]]]
    (is (= expected (vv/age-text now iso)))))

(deftest trade-label
  (doseq [[trade expected] [[nil ""]
                            [{:output "book"} "book"]
                            [{:output "book" :enchant "mending" :level 1} "book · mending · level 1"]
                            [{:output "book" :level 2 :atLeast true} "book · at least level 2"]]]
    (is (= expected (vv/trade-label trade)))))

(def village
  {:name "Oakford" :kind "village" :x 1 :y 64 :z 2 :state "below-target" :fresh true :observed 3
   :population {:target 5 :blueprintId "starter-hut" :roles [{:id "farmer" :profession "farmer"}]}
   :workspaces [{:id "farmer" :profession "farmer" :trade {:output "bread" :enchant "x"}}]
   :roles [] :members []})

(deftest village-matches
  (doseq [[q expected] [["" true] ["oak" true] ["VILLAGE" true] ["farmer" true] ["bread" true] ["zzz" false]]]
    (is (= expected (vv/village-matches? village q)) q)))

(deftest filter-villages-trims-and-keeps-order
  (is (= ["Oakford"] (map :name (vv/filter-villages [village {:name "Elm" :kind "x" :state "s"}] "  oak ")))))

(deftest population-text
  (doseq [[v expected] [[village "3 / 5 residents"]
                        [(assoc village :observed nil) "unknown / 5 residents"]
                        [(assoc village :fresh false :lastObservedPopulation 2) "last observed 2 / 5"]
                        [(assoc village :fresh false) "unknown / 5 residents"]
                        [(dissoc village :population) "population plan unknown"]]]
    (is (= expected (vv/population-text v)))))

(deftest state-text
  (is (= "below target" (vv/state-text village))))

(deftest short-uuid
  (is (= "12345678" (vv/short-uuid "1234567890abcdef"))))

(def housing-fresh
  {:usableBeds 2 :requiredBeds 4 :state "short" :residentBedCapacity 3 :missingCells 1 :unknownCells nil
   :shelter {:status "enclosed" :lightMinimum 7 :issues ["a" "b"]}
   :residentBeds [{:uuid "abcdefghijk" :beds [1 2]}]})

(deftest housing-lines
  (doseq [[v expected] [[(assoc village :housing housing-fresh)
                         [{:text "2 reachable beds / 4 needed · short"}
                          {:text "enclosure enclosed · minimum light 7 · beds reachable from residents 3" :muted true}
                          {:text "last-known resident bed reachability: abcdefgh 2 bed(s)" :muted true}
                          {:text "1 missing cells · 0 unloaded cells" :muted true}
                          {:text "a; b" :muted true}]]
                        [(assoc village :fresh false) [{:text "housing unknown until a fresh inspection"}]]
                        [(assoc village :housing {:shelter {:status "open"}})
                         [{:text "? reachable beds / ? needed · "}
                          {:text "enclosure open · minimum light unknown · beds reachable from residents unknown" :muted true}]]]]
    (is (= expected (vv/housing-lines v)))))

(def role-village
  (assoc village
         :roles [{:id "farmer" :profession "farmer" :observed 1 :required 2 :status "below-target" :stock []
                  :trade {:output "bread"} :workstation [1 2 3]}
                 {:id "lib" :profession "librarian" :lastObserved 4 :required 1 :status "stale"
                  :stock [{:uuid "abcdefghij" :status "unknown" :lastStatus "ok" :restock "soon"}]}
                 {:id "x" :profession "x" :required 1 :status "unknown" :stock []}]
         :workspaces [{:id "farmer" :status "ok" :associations [{:uuid "aaaaaaaabb" :status "verified"}]}]))

(deftest role-rows
  (is (= [{:title "farmer · farmer" :count "1 / 2" :status "below-target"
           :notes ["desired offer: bread" "workspace ok: aaaaaaaa verified"]}
          {:title "lib · librarian" :count "last 4 / 1" :status "stale"
           :notes ["merchant stock: abcdefgh unknown (last ok) · restock soon"]}
          {:title "x · x" :count "? / 1" :status "unknown" :notes []}]
         (vv/role-rows role-village))))

(deftest role-rows-workstation-without-workspace
  (is (= "workstation local coordinates 1, 2, 3; association unknown"
         (second (:notes (first (vv/role-rows (assoc-in role-village [:workspaces] []))))))))

(deftest workspace-rows
  (doseq [[w expected] [[{:id "w" :profession "p" :status "ok" :at {:x 1 :y 2 :z 3} :block "composter" :trade {:output "bread"}
                          :associations [{:uuid "abcdefghij" :status "ok" :reason "r" :basis "b"}]}
                         {:title "w · p" :where "1, 2, 3 · composter · bread" :status "ok" :status-text "ok"
                          :association "abcdefgh: ok (r) · b"}]
                        [{:id "w" :profession "p" :status "unknown" :lastStatus "ok" :localAt [1 2 3] :associations []}
                         {:title "w · p" :where "local 1,2,3 (world location not observed) · block unknown · any offer"
                          :status "unknown" :status-text "unknown now (last ok)"
                          :association "No exact UUID workstation association was verified."}]]]
    (is (= expected (vv/workspace-row w)))))

(deftest evidence-lines
  (is (= [{:text "boom" :cls "state unknown"}
          {:text "fresh snapshot · observed 5m ago" :cls "muted"}
          {:text "2 residents with stale or unknown identity facts" :cls "muted"}
          {:text "r" :cls "muted"}
          {:text "1 observed surplus residents; nobody is removed automatically" :cls "muted"}]
         (vv/evidence-lines now (assoc village :error "boom" :observedAt "2026-01-01T11:55:00Z" :unknownResidents 2
                                       :restock "r" :surplus 1))))
  (is (= [{:text "not inspected yet" :cls "muted"}
          {:text "current uncertainty unknown" :cls "muted"}
          {:text "unknown" :cls "muted"}]
         (vv/evidence-lines now (assoc village :observedAt nil :unknownResidents nil)))))

(deftest member-text
  (is (= "farmer: abcdefgh · trade lock verified · last seen 5m ago · offers 3h ago"
         (vv/member-text now {:profession "farmer" :uuid "abcdefghij" :lockVerifiedAt "x"
                              :lastSeenAt "2026-01-01T11:55:00Z" :offersObservedAt "2026-01-01T09:00:00Z"})))
  (is (= "farmer: abcdefgh · lock unverified · last seen unknown · offers unobserved"
         (vv/member-text now {:profession "farmer" :uuid "abcdefghij"}))))

(deftest map-link-text
  (is (= "map footprint 3 × 4" (vv/map-link-text {:bounds {:width 3 :depth 4}})))
  (is (= "show map marker" (vv/map-link-text {}))))

(deftest status-text
  (is (= "2 places · refreshed 10:00:00 · oops" (vv/status-text 2 "10:00:00" "oops")))
  (is (= "2 places · refreshed 10:00:00" (vv/status-text 2 "10:00:00" nil))))

(deftest empty-text
  (is (= "No villages match this filter." (vv/empty-text 3)))
  (is (= "No saved villages or village inspections yet." (vv/empty-text 0))))
