(ns engine.result-test
  (:require [cljs.test :refer [deftest is]]
            [engine.core.base :as base]))

(deftest a-round-may-return-an-ending
  (is (= {:status :ended :ending {:kind :nothing-possible :cause :no-tree}}
         (base/normalize-result {:kind :nothing-possible :cause :no-tree})))
  (is (= {:status :continue} (base/normalize-result :continue)))
  (is (= :error (:status (base/normalize-result {:kind :gave-up})))))
