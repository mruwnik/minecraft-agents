(ns world-test.build-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.build :as b]))

(deftest a-bundle-recorded-with-another-hash-is-stale
  (is (true? (b/stale? ["aaaa" "c1"] ["bbbb" "c1"]))))

(deftest a-bundle-recorded-with-the-trees-hash-is-current
  (is (false? (b/stale? ["aaaa" "c1"] ["aaaa" "c2-dirty"]))))

(deftest a-bundle-without-a-record-is-stale
  (is (true? (b/stale? nil ["aaaa" "c1"]))))

(deftest parse-src-reads-hash-and-commit
  (is (= ["0fb3" "76e9-dirty"] (b/parse-src "0fb3 76e9-dirty\n")))
  (is (nil? (b/parse-src "\n")))
  (is (true? (b/dirty? ["0fb3" "76e9-dirty"])))
  (is (false? (b/dirty? ["0fb3" "76e9"]))))
