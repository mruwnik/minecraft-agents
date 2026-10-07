(ns world-test.changed-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.changed :as c]))

(def src
  {"engine/src/jobs/a/one.cljs" "(ns jobs.a.one (:require [jobs.lib.util :as u] [jobs.a.two :as t]))"
   "engine/src/jobs/a/two.cljs" "(ns jobs.a.two)"
   "engine/src/jobs/lib/util.cljs" "(ns jobs.lib.util)"
   "engine/src/jobs/b/other.cljs" "(ns jobs.b.other)"
   "engine/src/triggers/survival/hostile_near.cljs" "(ns triggers.survival.hostile-near)"})

(def fixture "{:register [{:trigger :hostile-near}] :cases [{:act [[:job (jobs.a.one {})]]}]}")

(deftest the-closure-holds-named-jobs-triggers-and-what-their-source-names
  (is (= #{"engine/src/jobs/a/one.cljs" "engine/src/jobs/a/two.cljs" "engine/src/jobs/lib/util.cljs"
           "engine/src/triggers/survival/hostile_near.cljs"}
         (c/closure fixture src))))

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
  (let [fixtures {"f" fixture "g" fixture "h" fixture}
        record {"f" "r1" "g" "r2"}
        changed {"r1" [] "r2" ["engine/src/jobs/a/one.cljs"]}]
    (is (= #{"g" "h"} (c/stale-stems {:fixtures fixtures :record record :src src :dir "engine/fixtures/world"
                                      :changed-since #(get changed %)})))
    (is (= #{"f" "g" "h"} (c/stale-stems {:fixtures fixtures :record record :src src :dir "engine/fixtures/world"
                                          :changed-since (constantly nil)})))))

(deftest a-pass-is-recorded-only-for-a-complete-all-pass-file
  (let [expected {"f" 2 "g" 1 "h" 1 "k" 1}
        results [{:file "f" :status :pass} {:file "f" :status :pass} {:file "g" :status :fail}
                 {:file "h" :status :flaky} {:file "k" :status :pass}]]
    (is (= #{"f" "k"} (c/passed-stems expected results)))
    (is (= #{"k"} (c/passed-stems expected (rest results))) "f ran 1 of 2 times")))
