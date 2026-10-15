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

(deftest tickable-cards-need-all-named-cases-passing
  (let [r (t/triage {:results [(ok "a/x") (ok "b/w") {:id "b/f" :file "b" :status :flaky}]
                     :previous [] :cards [(first board) {:id "c9" :column "live" :commits [] :description "live: b/w, b/f"}]
                     :ancestor? (constantly true)})]
    (is (= ["c1" "c9"] (:tick r)))))

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
