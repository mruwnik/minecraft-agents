(ns world-test.craft-steps-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.craft-steps :as c]))

(defn row [edge action t kind] {:kind kind :edge edge :action action :t t})

(deftest the-setting-is-the-mean-seconds-of-a-craft-action
  (is (= {:n 2 :mean 1.5 :setting 1.5}
         (c/report [(row :start "a" 0 :craft) (row :end "a" 1000 :craft) (row :start "b" 5000 :craft) (row :end "b" 7000 :craft)
                    (row :start "c" 0 :dig) (row :end "c" 90000 :dig)])) "other actions do not count")
  (is (= {:n 0} (c/report []))))
