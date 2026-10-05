(ns engine.planner-bench-test
  "The recorded benchmark query set (the frozen world queries of bench-lang/courses.mjs and the live tester's courses)
  planned with the ClojureScript planner and checked against what the JS planner answered, recorded once in
  test/planner-bench.json as [status reason seconds risk end-cell expanded] per query (seconds and risk to 3 decimals).
  The world queries need the frozen bench (engine/test/fixtures/pathfinding/claude-1, or PLANNER_BENCH_DIR). Without it
  the world test FAILS unless PLANNER_BENCH_SKIP_WORLD=1 is set (a checkout or CI without the fixtures sets it, and then
  only the world queries go unchecked). Re-record the pins with `npm run record:planner-bench` (engine/)."
  (:require [cljs.test :refer [deftest is]]
            ["fs" :as fs]
            ["path" :as path]
            [engine.planner-fixture :as pf]
            [engine.test-util :as tu]))

(def recorded
  "the JS planner's answers: {:world {id rec} :course {name rec} :options {set {sample rec}} :goals {k rec} ...}"
  (delay (-> (fs/readFileSync (path/join (js/process.cwd) "test/planner-bench.json") "utf8")
             js/JSON.parse
             (js->clj :keywordize-keys true))))

(defn record
  "[status reason seconds risk end-cell expanded] of a planner result, as the recorded answers have them (the JS planner
  knew no :start-enclosed: it said goal-unloaded)."
  [r]
  (let [cost (get-in r [:path :cost])]
    [(:status r) (if (= "start-enclosed" (:reason r)) "goal-unloaded" (:reason r)) (:seconds cost) (:risk cost) (pf/last-cell r) (:expanded r)]))

(defn close? [a b]
  (or (and (nil? a) (nil? b))
      (and (number? a) (number? b) (< (js/Math.abs (- a b)) 0.0011))))

(defn same-answer?
  "does the planner's record equal the recorded one: status, reason, end cell and expanded exactly, costs within rounding"
  [[s1 r1 sec1 risk1 end1 e1] [s2 r2 sec2 risk2 end2 e2]]
  (and (= [s1 r1 end1 e1] [s2 r2 end2 e2]) (close? sec1 sec2) (close? risk1 risk2)))

(defn disagreements
  "ids whose planned record differs from the recorded one: [[id got expected] ...]"
  [ids planned expected]
  (->> ids
       (keep (fn [id] (let [got (record (planned id))] (when-not (same-answer? got (expected id)) [id got (expected id)]))))
       vec))

(defn course-names [] (sort (map name (keys (:course @recorded)))))

(deftest every-recorded-course-plans-as-the-js-planner-did
  (let [names (course-names)]
    (is (= 144 (count names)))
    (is (= [] (disagreements names pf/course-plan (fn [n] (get-in @recorded [:course (keyword n)])))))))

(defn bench-dir [] (or js/process.env.PLANNER_BENCH_DIR (path/resolve js/__dirname "../test/fixtures/pathfinding/claude-1")))

(defn world-state
  "what the world half has to work with: :present, :skipped (bench absent and skip asked for) or :missing (a failure)"
  [queries-exist? skip?]
  (cond queries-exist? :present
        skip? :skipped
        :else :missing))

(defn skip-world? [] (= "1" js/process.env.PLANNER_BENCH_SKIP_WORLD))

(defn world-queries
  "[{:id :query :snapshot}] of the frozen world, [] when the bench is not on this machine (see world-state)."
  []
  (let [dir (bench-dir)
        queries (path/join dir "queries.json")]
    (cond
      (not (fs/existsSync queries))
      []
      :else
      (let [snapshots ^js @(delay (tu/require-here "./js/path/snapshot.mjs"))
            snapshot (.createSnapshot snapshots #js {})]
        (.loadRecordedWorld snapshots snapshot (path/join dir "chunks"))
        (->> (js->clj (js/JSON.parse (fs/readFileSync queries "utf8")) :keywordize-keys true)
             (mapv (fn [{:keys [id from goal]}] {:id id :snapshot snapshot :query {:from from :goal goal}})))))))

(deftest the-world-half-fails-without-the-bench-unless-skipped
  (is (= [:present :present :skipped :missing]
         [(world-state true false) (world-state true true) (world-state false true) (world-state false false)])))

(deftest every-recorded-world-query-plans-as-the-js-planner-did
  (let [qs (world-queries)
        state (world-state (fs/existsSync (path/join (bench-dir) "queries.json")) (skip-world?))
        _ (when (= :skipped state) (println "WARNING: no frozen bench at" (bench-dir) ", PLANNER_BENCH_SKIP_WORLD=1: world queries skipped"))
        _ (is (not= :missing state)
              (str "no frozen bench at " (bench-dir) ": the " (count (:world @recorded)) " world queries were not checked. "
                   "Set PLANNER_BENCH_DIR to it, or PLANNER_BENCH_SKIP_WORLD=1 on a machine that has none (CI)."))
        by-id (into {} (map (juxt :id identity)) qs)
        ids (map :id qs)]
    (is (= [] (disagreements ids
                             (fn [id] (let [{:keys [snapshot query]} (by-id id)] (pf/plan snapshot query)))
                             (fn [id] (get-in @recorded [:world (keyword id)])))))))

(defn changes
  "how recorded answers (maps id -> record) differ: {:added [id] :removed [id] :changed [[id old new]]}, ids sorted"
  [before after]
  (let [ids (fn [pred] (vec (sort (filter pred (distinct (concat (keys before) (keys after)))))))]
    {:added (ids #(and (contains? after %) (not (contains? before %))))
     :removed (ids #(and (contains? before %) (not (contains? after %))))
     :changed (mapv (fn [id] [id (before id) (after id)])
                    (ids (fn [id] (and (contains? before id) (contains? after id) (not (same-answer? (before id) (after id)))))))}))

(deftest changes-names-added-removed-and-changed-ids
  (let [a ["found" nil 1.0 0 [1 2 3] 5]
        b ["found" nil 2.0 0 [1 2 3] 5]]
    (is (= {:added [:c] :removed [:b] :changed [[:d a b]]}
           (changes {:a a :b a :d a} {:a a :c a :d b})))
    (is (= {:added [] :removed [] :changed []}
           (changes {:a a} {:a ["found" nil 1.0004 0.0 [1 2 3] 5]})))))

(deftest the-recorded-answers-cover-found-partial-and-none
  (let [answers (concat (vals (:world @recorded)) (vals (:course @recorded)))]
    (is (= #{"found" "none" "partial"} (set (map first answers))))
    (is (> (count (filter #(= "found" (first %)) answers)) 200))))
