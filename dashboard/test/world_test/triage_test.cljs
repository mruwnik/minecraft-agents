(ns world-test.triage-test
  (:require ["fs" :as fs]
            ["path" :as path]
            [cljs.reader :as reader]
            [cljs.test :refer [deftest is]]
            [world-test.triage :as t]))

(defn fail [id check] {:id id :file (first (.split id "/")) :status :fail :expects [{:expect check :status :fail}]})
(defn ok [id] {:id id :file (first (.split id "/")) :status :pass})

(def prev [(fail "a/x" {:event 1}) (fail "a/y" {:event 1}) (ok "a/z") (fail "b/w" {:event 1}) {:id "b/e" :file "b" :status :error :evidence "boom"}])
(def cur [(fail "a/x" {:event 1})                        ; same check as before
          (fail "a/y" {:event 2})                        ; other check
          (fail "a/z" {:event 1})                        ; passed before
          (ok "b/w")
          {:id "b/e" :file "b" :status :error :evidence "boom"}
          {:id "b/f" :file "b" :status :flaky}
          {:id "b/g" :file "b" :status :inconclusive}
          (fail "c/new" {:event 3})])

(defn classes [cases previous] (into {} (map (juxt :id :class)) (t/classify cases previous)))

(deftest failures-are-known-new-or-flaky
  (is (= {"a/x" :known "a/y" :new "a/z" :new "b/w" :pass "b/e" :known "b/f" :flaky "b/g" :inconclusive "c/new" :new}
         (classes cur [prev]))))

(deftest later-previous-files-win
  (is (= :new (:class (first (t/classify [(fail "a/x" {:event 1})] [prev [(ok "a/x")]]))))))

(deftest the-pools-known-flag-counts-when-no-previous-sweep-is-given
  (is (= :known (:class (first (t/classify [(assoc (fail "a/x" {:event 1}) :known-failure true)] []))))))

(def board
  [{:id "c1" :column "live" :title "one" :commits ["aaa"] :description "unit tests: a, b\nLive plan: fixture b (all) and a/x"}
   {:id "c2" :column "live" :title "two" :commits ["aaa"] :description "x" :plan-notes ["Live plan: c/new should succeed"]}
   {:id "c3" :column "live" :title "three" :commits ["aaa"] :description "no cases named"}
   {:id "c4" :column "needs_fixture" :title "four" :commits ["bbb"] :description "live: b/w"}
   {:id "c5" :column "live" :title "five" :commits ["aaa"] :description "live: b/w, b/g"}
   {:id "c6" :column "live" :title "six" :commits ["aaa"] :description "live: a/z"}])

(def report (t/triage {:results (conj cur (ok "d/p")) :previous [prev]
                       :cards board
                       :bugs [{:id "bug1" :title "Bug: c/new fails live"}]
                       :ancestor? #(not= % "bbb")}))

(defn verdicts [] (into {} (map (juxt :id :verdict)) (:cards report)))

(deftest cards-get-a-verdict-from-the-cases-their-live-plan-names
  (is (= {"c1" :pending "c2" :pending "c3" :no-plan "c4" :not-in-rev "c5" :hold "c6" :pending}
         (verdicts))))

(deftest a-plan-restricts-to-its-named-cases-and-a-fixture-name-takes-all
  (is (= #{"b/w" "b/e" "b/f" "b/g" "a/x"} (set (map :id (:cases (first (:cards report))))))))

(deftest tickable-cards-need-all-named-cases-passing-flaky-is-not-a-pass
  (let [r (t/triage {:results [(ok "a/x") (ok "b/w") {:id "b/f" :file "b" :status :flaky}]
                     :previous [] :cards [(first board) {:id "c9" :column "live" :commits [] :description "live: b/w, b/f"}]
                     :ancestor? (constantly true)})]
    (is (= [] (:tick r)))
    (is (= ["c1" "c9"] (:hold r)))))

(def bug-description
  "found 2 failing case(s) in fixture f.\n\n- f/one (day pass, fail)\n  first failing check: x\n  trace: /t/f__one.edn\n- f/two (day pass, fail)\n  first failing check: y\n\nGrouped per fixture.")

(defn bug-card [notes] {:id "bug" :column "live" :commits [] :description bug-description :plan-notes notes})

(deftest a-bug-card-ticks-only-when-all-cases-of-its-description-passed
  (let [run (fn [results] (t/triage {:results results :previous [] :cards [(bug-card ["Live plan (cases):\nf/one"])]
                                     :ancestor? (constantly true) :fixture-files #{"f"}}))]
    (is (= {:tick [] :pending ["bug"]} (select-keys (run [(ok "f/one") (fail "f/two" {:event 1})]) [:tick :pending])))
    (is (= [] (:tick (run [(ok "f/one")]))))
    (is (= ["bug"] (:tick (run [(ok "f/one") (ok "f/two")]))))))

(deftest new-failures-list-their-cards-and-existing-bug-cards
  (let [nb (into {} (map (juxt :id identity)) (:new-bugs report))]
    (is (= #{"a/y" "a/z" "c/new"} (set (keys nb))))
    (is (= ["bug1"] (:existing (nb "c/new"))))
    (is (= ["c2"] (:cards (nb "c/new"))))
    (is (= [] (:existing (nb "a/y"))))))

(deftest the-text-summary-counts-everything
  (let [s (t/summary report)]
    (is (re-find #"known 2" s))
    (is (re-find #"new 3" s))
    (is (re-find #"flaky 1" s))))

;; A reduced copy of sweep c60f (every non-pass case, the passes of the fixtures the cards below name), the 5e7b sweep before it and nine cards as they were then.
(def fx (.resolve path js/__dirname ".." "test" "fixtures" "sweep-triage"))
(defn read-edn [f] (reader/read-string (fs/readFileSync (.resolve path fx f) "utf8")))

(def real
  (let [board (t/with-plans {:cards (read-edn "board.edn")} {"fb4f9725" "go-to-torch-fetch" "74665b91" "result-predicates"})]
    (t/triage {:results (read-edn "current/sweep-day.edn") :previous [(read-edn "previous/sweep-day.edn")]
               :cards (:cards board) :bugs [] :ancestor? (constantly true)})))

(deftest every-failure-the-pool-skipped-as-known-is-known
  (let [flagged (set (map :id (filter :known-failure (read-edn "current/sweep-day.edn"))))
        known (set (map :id (filter #(= :known (:class %)) (:failures real))))]
    (is (= 110 (count flagged)))
    (is (every? known flagged))))

(deftest the-real-sweep-has-the-counts-the-sweep-reported
  (is (= {:flaky 30 :known 114 :new 34 :inconclusive 1} (dissoc (:counts real) :pass))))

(deftest real-cards-get-the-sweep-agents-verdicts
  (is (= ["a52ca0e4" "c04521b0" "d86c8544" "c1c072f8"] (map #(subs % 0 8) (:tick real))))
  (is (= ["fb4f9725"] (map #(subs % 0 8) (:pending real))))
  (is (= ["afdc32d0" "b812e8f8" "1436e972" "74665b91"] (map #(subs % 0 8) (:no-plan real)))))

(defn skipped [id check] {:id id :file (first (.split id "/")) :status :skipped-known :previous-status :fail :previous-failure check})

(deftest a-skipped-known-failure-is-its-own-class-and-keeps-its-failure-for-the-next-sweep
  (is (= {"a/x" :skipped-known "a/y" :new "a/z" :pass}
         (classes [(skipped "a/x" {:event 1}) (fail "a/y" {:event 2}) (ok "a/z")] [prev])))
  (is (= :known (:class (first (t/classify [(fail "a/x" {:event 1})] [[(skipped "a/x" {:event 1})]]))))
      "a later sweep that runs the case again compares with the failure the skip carried")
  (is (= :new (:class (first (t/classify [(fail "a/x" {:event 2})] [[(skipped "a/x" {:event 1})]]))))))

(deftest a-card-whose-case-was-skipped-is-held-never-ticked-and-adds-no-bug-card
  (let [r (t/triage {:results [(skipped "a/x" {:event 1}) (ok "b/w")] :previous [prev]
                     :cards [{:id "c1" :column "live" :title "one" :commits ["aaa"] :description "live: a/x"}
                             {:id "c2" :column "live" :title "two" :commits ["aaa"] :description "live: a/x and b/w"}]
                     :bugs [] :ancestor? (constantly true)})]
    (is (= ["c1" "c2"] (:hold r)))
    (is (= [] (:tick r)))
    (is (= [] (:new-bugs r)))))

(deftest unskip-ids-are-the-known-failures-named-by-cards-past-coding-whose-commits-are-in
  (let [cards [{:id "c1" :column "live" :title "one" :commits ["aaa"] :description "live: a/x, a/z"}
               {:id "c2" :column "live" :title "two" :commits ["bbb"] :description "live: b/w"}
               {:id "c3" :column "needs_fixture" :title "three" :commits ["aaa"] :description "live: a/y"}
               {:id "c4" :column "pending" :title "four" :commits ["aaa"] :description "live: b/e"}
               {:id "c5" :column "testing" :title "five" :commits ["aaa"] :plan-notes ["Live plan (cases):\nb/w"]}
               {:id "c6" :column "in_review" :title "six" :commits ["aaa"] :plan-notes ["Live plan (cases):\nb/e"]}
               {:id "c7" :column "needs_fixture" :title "seven" :commits ["bbb"] :plan-notes ["Live plan (cases):\na/y"]}]
        ids #(sort (t/unskip-ids (merge {:previous [prev] :cards cards} %)))]
    (is (= ["a/x" "b/e" "b/w"] (ids {:ancestor? #(not= % "bbb")})))
    (is (= ["a/x" "a/y" "b/e" "b/w"] (ids {:ancestor? (constantly true)})))))

(deftest known-failures-of-changed-fixture-files-are-unskipped
  (is (= ["b/e" "b/w"] (t/unskip-ids {:previous [prev] :cards [] :ancestor? (constantly true) :changed-files #{"b"}}))))

(deftest changed-fixture-files-come-from-the-diff-of-engine-fixtures-world
  (is (= #{"a" "b-c"} (t/fixture-stems "engine/fixtures/world/a.edn\nengine/fixtures/world/sub/b-c.edn\nREADME.md\n"))))

(def cases-note "Unit green, b/f too.\nLive plan (cases):\na/x\nb/w")

(deftest a-case-named-under-the-live-plan-heading-but-absent-from-the-results-is-missing
  (let [r (t/triage {:results [(ok "a/x") (ok "a/z") (ok "b/w")] :previous []
                     :cards [{:id "c1" :column "live" :title "one" :commits [] :plan-notes ["Live plan (cases):\na/x\na/gone"]}
                             {:id "c2" :column "live" :title "two" :commits [] :plan-notes ["Live plan (cases):\na/x\nb/w"]}
                             {:id "c3" :column "live" :title "three" :commits [] :plan-notes ["Live plan (cases):\nnew-fixture/case"]}]
                     :bugs [] :ancestor? (constantly true)})]
    (is (= ["c2"] (:tick r)))
    (is (= ["c1" "c3"] (:no-plan r)))
    (is (= ["a/gone"] (:missing (first (:cards r)))))))

(deftest only-the-newest-live-plan-note-counts
  (let [r (t/triage {:results [(ok "a/x")] :previous []
                     :cards [{:id "c1" :column "live" :title "one" :commits []
                              :plan-notes ["Live plan (cases):\na/gone" "Live plan (cases):\na/x"]}]
                     :bugs [] :ancestor? (constantly true)})]
    (is (= ["c1"] (:tick r)))
    (is (= [] (:missing (first (:cards r)))))))

(deftest a-shortened-case-name-under-the-heading-is-not-missing
  (let [r (t/triage {:results [(ok "a/long-case-name")] :previous []
                     :cards [{:id "c1" :column "live" :title "one" :commits [] :plan-notes ["Live plan (cases):\na/long-case"]}]
                     :bugs [] :ancestor? (constantly true)})]
    (is (= ["c1"] (:tick r)))))

(deftest only-lines-under-the-live-plan-heading-name-cases
  (is (= ["a/x" "b/w"] (map :id (t/named-cases (t/card-plan-text {:title "mentions b/f" :plan-notes [cases-note]})
                                              (t/classify [(ok "a/x") (ok "a/z") (ok "b/w") (ok "b/f")] []))))))

(deftest plan-tokens-that-are-no-fixture-case-are-not-missing
  (let [note "Live plan (cases):\nengine/fixtures/world/a.edn/x\nb.edn/\nunit/integration/review\nengine/fixtures/world/b.edn\nnot-a-fixture/case"
        r (t/triage {:results [(ok "a/x") (ok "b/w") (ok "b/f")] :previous []
                     :cards [{:id "c1" :column "live" :title "one" :commits [] :plan-notes [note]}]
                     :bugs [] :ancestor? (constantly true)})
        card (first (:cards r))]
    (is (= [] (:missing card)))
    (is (= ["c1"] (:tick r)))
    (is (= ["a/x" "b/f" "b/w"] (sort (map :id (t/named-cases (t/card-plan-text {:plan-notes [note]})
                                                           (t/classify [(ok "a/x") (ok "b/w") (ok "b/f")] []))))))))

(deftest case-of-a-fixture-file-absent-from-the-run-is-missing
  (let [note "Live plan (cases):\nnew/x\na/x\nnot-a-fixture/case"
        r (t/triage {:results [(ok "a/x")] :previous [] :fixture-files #{"a" "new"}
                     :cards [{:id "c1" :column "live" :title "one" :commits [] :plan-notes [note]}]
                     :bugs [] :ancestor? (constantly true)})]
    (is (= ["new/x"] (:missing (first (:cards r)))))
    (is (= ["c1"] (:no-plan r)))
    (is (= [] (:tick r)))))
