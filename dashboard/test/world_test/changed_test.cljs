(ns world-test.changed-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.changed :as c]))

(def src
  {"engine/src/jobs/a/one.cljs" "(ns jobs.a.one (:require [jobs.lib.util :as u] [jobs.a.two :as t]))"
   "engine/src/jobs/a/two.cljs" "(ns jobs.a.two)"
   "engine/src/jobs/lib/util.cljs" "(ns jobs.lib.util)"
   "engine/src/jobs/b/other.cljs" "(ns jobs.b.other)"
   "engine/src/jobs/c/night.cljs" "(ns jobs.c.night (:require [triggers.survival.hungry :as h]))"
   "engine/src/triggers/survival/hungry.cljs" "(ns triggers.survival.hungry (:require [triggers.survival.died :as d]))"
   "engine/src/triggers/survival/died.cljs" "(ns triggers.survival.died)"
   "engine/src/triggers/survival/hostile_near.cljs" "(ns triggers.survival.hostile-near)"})

(def fixture "{:register [{:trigger :hostile-near}] :cases [{:act [[:job (jobs.a.one {})]]}]}")

(deftest the-closure-holds-named-jobs-triggers-and-what-their-source-names
  (is (= #{"engine/src/jobs/a/one.cljs" "engine/src/jobs/a/two.cljs" "engine/src/jobs/lib/util.cljs"
           "engine/src/triggers/survival/hostile_near.cljs"}
         (c/closure fixture src))))

(deftest requires-of-trigger-namespaces-are-followed
  (let [cl (c/closure "(jobs.c.night {})" src)]
    (is (contains? cl "engine/src/triggers/survival/hungry.cljs"))
    (is (contains? cl "engine/src/triggers/survival/died.cljs"))
    (is (c/stale? cl "engine/fixtures/world/f.edn" ["engine/src/triggers/survival/died.cljs"]))))

(deftest a-change-selects-by-closure-and-shared-code-selects-everything
  (let [cl (c/closure fixture src)
        stale? #(c/stale? cl "engine/fixtures/world/f.edn" %)]
    (is (not (stale? [])))
    (is (stale? ["engine/src/jobs/a/two.cljs"]))
    (is (stale? ["engine/fixtures/world/f.edn"]))
    (is (not (stale? ["engine/src/jobs/b/other.cljs" "engine/fixtures/world/g.edn" "docs/x.md"
                      "engine/test/jobs/a_test.cljs" "mark"])))
    (is (stale? ["engine/src/engine/core.cljs"]))
    (is (stale? ["engine/js/raw_world.js"]))
    (is (stale? ["engine/src/triggers/defaults.edn"]))
    (is (stale? ["engine/src/jobs/hooks.edn"]))))

(deftest only-fixtures-unchanged-since-their-recorded-pass-are-skipped
  (let [fixtures {"d/f.edn" fixture "d/g.edn" fixture "d/h.edn" fixture}
        record {"d/f.edn" "r1" "d/g.edn" "r2"}
        changed {"r1" [] "r2" ["engine/src/jobs/a/one.cljs"]}]
    (is (= #{"d/g.edn" "d/h.edn"} (c/stale-stems {:fixtures fixtures :record record :src src :changed-since #(get changed %)})))
    (is (= #{"d/f.edn" "d/g.edn" "d/h.edn"} (c/stale-stems {:fixtures fixtures :record record :src src :changed-since (constantly nil)})))
    (is (= #{"d/f.edn"} (c/stale-stems {:fixtures {"d/f.edn" fixture} :record {"d/f.edn" "r1"} :src src
                                        :changed-since (constantly ["d/f.edn"])}))
        "the fixture's own path, wherever it lives")))

(deftest a-pass-is-recorded-only-when-head-and-changes-held-over-the-run
  (let [args {:passed #{"d/f.edn"} :fixtures {"d/f.edn" fixture} :src src}
        at (fn [rev changed] {:rev rev :changed changed})]
    (is (= #{"d/f.edn"} (c/recordable (assoc args :start (at "r1" []) :end (at "r1" [])))))
    (is (= #{"d/f.edn"} (c/recordable (assoc args :start (at "r1" ["docs/x.md"]) :end (at "r1" ["docs/x.md"])))))
    (is (= #{} (c/recordable (assoc args :start (at "r1" []) :end (at "r2" [])))) "committed mid-run")
    (is (= #{} (c/recordable (assoc args :start (at "r1" []) :end (at "r1" ["docs/x.md"])))) "tree changed mid-run")
    (is (= #{} (c/recordable (assoc args :start (at "r1" ["engine/src/jobs/a/two.cljs"]) :end (at "r1" ["engine/src/jobs/a/two.cljs"])))))
    (is (= #{} (c/recordable (assoc args :start (at nil nil) :end (at nil nil)))))))

(deftest a-run-that-did-not-pass-drops-its-record
  (is (= {"a" "r9" "c" "r1"}
         (c/next-record {"a" "r0" "b" "r0" "c" "r1"} "r9" {:clean #{"a"} :ran #{"a" "b"}}))))

(deftest a-pass-is-recorded-only-for-a-complete-all-pass-file
  (let [expected {"f" 2 "g" 1 "h" 1 "k" 1}
        results [{:file "f" :status :pass} {:file "f" :status :pass} {:file "g" :status :fail}
                 {:file "h" :status :flaky} {:file "k" :status :pass}]]
    (is (= #{"f" "k"} (c/passed-stems expected results)))
    (is (= #{"k"} (c/passed-stems expected (rest results))) "f ran 1 of 2 times")))
