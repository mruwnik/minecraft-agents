(ns dashboard.migrate-data-test
  (:require ["fs" :as fs]
            ["path" :as path]
            ["os" :as os]
            [cljs.test :refer [deftest is]]
            [dashboard.edn-data :as data]
            [dashboard.migrate-data :as migration]
            [dashboard.village-data :as villages]
            [plan.shape :as shape]))

(def marker {:name "old-village" :kind "village" :x 10 :y 64 :z -3 :by "Founder" :note "historic intention"
             :unknown {:nested ["keep" nil false 0] "odd key" "preserved"}})
(def inspection {:version 1 :place "inspected-village" :at {:x 20 :y 64 :z 30}
                 :observedAt "2025-01-02T00:00:00.000Z" :population {:target 5 :roles [{:id "books" :profession "librarian" :count 1}]}
                 :source {:id "old-house" :dimensions [5 4 6]}
                 :report {:satisfied true :population 5 :assigned ["historical-uuid"]
                          :roles [{:id "books" :uuids ["historical-uuid"] :status "satisfied"}]}})
(def existing {:id "inspected-village" :note "Preserve the native plan's intention."
               :parts [{:id "house" :cells [[20 63 30]] :want "stone"}]
               :assign [{:spot "house" :body "Builder" :use :build}]
               :metadata {:native-extra "preserved"}})

(defn put [root relative value json?]
  (let [file (.join path root relative)]
    (.mkdirSync fs (.dirname path file) #js {:recursive true})
    (.writeFileSync fs file (if json? (js/JSON.stringify (clj->js value)) (pr-str value))) file))

(defn fixture []
  (let [root (.mkdtempSync fs (.join path (.tmpdir os) "village-plan-migration-"))]
    (put root "worlds/claude/world.json" {:name "claude"} true)
    (put root "worlds/other/world.json" {:name "other"} true)
    (put root "worlds/claude/places.json" [marker {:name "shelter" :kind "shelter" :x 0 :y 64 :z 0}] true)
    (put root "state/village-inspections/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.json" inspection true)
    (put root "worlds/claude/plans/inspected-village.edn" existing false)
    root))

(deftest dry-run-preflights-and-apply-preserves-existing-layout-with-exact-backup
  (let [root (fixture) opts {:root root :world "claude" :apply? false}
        source (.readFileSync fs (.join path root "worlds/claude/places.json") "utf8")
        original (data/read-text (.join path root "worlds/claude/plans/inspected-village.edn"))
        dry (migration/execute opts)]
    (is (= 2 (:village-count dry)))
    (is (= #{:create :enrich} (set (map :status (:files dry)))))
    (is (not (.existsSync fs (.join path root "worlds/claude/plans/old-village.edn"))))
    (is (= original (data/read-text (.join path root "worlds/claude/plans/inspected-village.edn"))))
    (let [applied (migration/execute (assoc opts :apply? true))
          p (data/read-file (.join path root "worlds/claude/plans/inspected-village.edn"))
          new (data/read-file (.join path root "worlds/claude/plans/old-village.edn"))
          backup (:backup (first (filter :backup (:files applied))))]
      (is (= #{:created :enriched} (set (map :status (:files applied)))))
      (is (= original (data/read-text backup)))
      (is (= (dissoc existing :metadata) (select-keys p (keys (dissoc existing :metadata))))))
    (let [p (data/read-file (.join path root "worlds/claude/plans/inspected-village.edn"))
          new (data/read-file (.join path root "worlds/claude/plans/old-village.edn"))]
      (is (= (dissoc existing :metadata) (select-keys p (keys (dissoc existing :metadata)))))
      (is (= "preserved" (get-in p [:metadata :native-extra])))
      (is (= inspection (get-in p [:metadata :legacy-inspection])))
      (is (= :planned (get-in p [:metadata :geometry])))
      (is (not (contains? new :status)))
      (is (= [] (:parts new)))
      (is (= marker (get-in new [:metadata :legacy-place])))
      (is (= :incomplete (get-in new [:metadata :geometry])))
      (is (= [] (shape/plan-errors p "inspected-village")))
      (is (= [] (shape/plan-errors new "old-village"))))
    (is (every? #(= :unchanged (:status %)) (:files (migration/execute (assoc opts :apply? true)))))
    (is (= source (.readFileSync fs (.join path root "worlds/claude/places.json") "utf8")))
    (is (not (.existsSync fs (.join path root "worlds/claude/notes"))))))

(deftest a-non-village-place-with-a-village-s-name-is-not-the-marker
  (let [root (fixture)
        same-name {:name "old-village" :kind "shelter" :x 99 :y 64 :z 99 :note "not the village"}]
    (put root "worlds/claude/places.json" [marker same-name] true)
    (migration/execute {:root root :world "claude" :apply? true})
    (is (= marker (get-in (data/read-file (.join path root "worlds/claude/plans/old-village.edn")) [:metadata :legacy-place])))))

(deftest metadata-conflicts-prevent-all-mutations
  (let [root (fixture)]
    (put root "worlds/claude/plans/inspected-village.edn"
         (assoc existing :metadata {:population {:target 99}}) false)
    (is (thrown-with-msg? js/Error #"metadata conflicts" (migration/execute {:root root :world "claude" :apply? true})))
    (is (not (.existsSync fs (.join path root "worlds/claude/plans/old-village.edn"))))
    (is (not (.existsSync fs (.join path root "worlds/claude/.migration-backups"))))))

(deftest edits-after-preflight-are-preserved
  (let [root (fixture) report (migration/prepare {:root root :world "claude"})
        write (first (filter #(= :enrich (:status %)) (:writes report)))
        changed (assoc existing :note "Later human edit")]
    (put root "worlds/claude/plans/inspected-village.edn" changed false)
    (is (thrown-with-msg? js/Error #"after migration preflight" (migration/enrich! write)))
    (is (= changed (data/read-file (:file write))))
    (is (not (.existsSync fs (:file (:backup write)))))))

(deftest village-display-reads-plans-and-keeps-inspections-historical
  (let [root (fixture)]
    (migration/execute {:root root :world "claude" :apply? true})
    (let [snapshot (villages/snapshot root [{:name "JSON-only" :kind "village" :world "claude"}] {:worlds ["claude"]})
          v (first (filter #(= "inspected-village" (:name %)) (:villages snapshot)))]
      (is (nil? (:error snapshot)))
      (is (= #{"old-village" "inspected-village"} (set (map :name (:villages snapshot)))))
      (is (= [] (:members v)))
      (is (= "stale" (:state v)))
      (is (false? (:fresh v)))
      (is (nil? (:observed v)))
      (is (= 5 (get-in v [:population :target])))
      (is (= [] (:villages (villages/snapshot root [] {:worlds ["other"]})))))))

(deftest strict-edn-and-bounds
  (is (thrown? js/Error (data/one-form "{} {}")))
  (is (thrown? js/Error (data/one-form "")))
  (is (= {:a 1} (data/one-form "{:a 1} ; end\n")))
  (let [root (fixture) file (.join path root "too-big.edn")]
    (.writeFileSync fs file (.alloc js/Buffer (inc data/max-bytes)))
    (is (thrown-with-msg? js/Error #"8 MiB" (data/read-file file)))))
