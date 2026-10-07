(ns engine.reach-const-test
  "The shared reach constants of jobs.lib.util."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.util :as u]))

(deftest eye-reach-keeps-a-margin-under-bucket-reach
  (is (= 4.2 u/eye-reach))
  (is (= 4.5 u/bucket-reach)))
