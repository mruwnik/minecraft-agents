(ns dashboard.ui.buildid-test
  (:require [cljs.test :refer [deftest are]]
            [dashboard.ui.buildid :as buildid]))

(deftest observe
  (are [known incoming expected] (= expected (buildid/observe known incoming))
    nil "a" {:known "a" :reload? false}
    "a" "a" {:known "a" :reload? false}
    "a" "b" {:known "a" :reload? true}
    "a" nil {:known "a" :reload? false}
    nil nil {:known nil :reload? false}))
