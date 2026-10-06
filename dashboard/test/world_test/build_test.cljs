(ns world-test.build-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.build :as b]))

(deftest stale-when-a-source-is-newer-than-the-build
  (is (true? (b/stale? 100 [50 101 20])))
  (is (true? (b/stale? 100 [200]))))

(deftest fresh-when-no-source-is-newer
  (is (false? (b/stale? 100 [50 100 20])))
  (is (false? (b/stale? 100 []))))

(deftest a-missing-build-is-stale
  (is (true? (b/stale? nil [1 2])))
  (is (true? (b/stale? nil []))))
