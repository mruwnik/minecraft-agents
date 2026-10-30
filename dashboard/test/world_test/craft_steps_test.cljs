(ns world-test.craft-steps-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.craft-steps :as c]))

(defn row [edge action t kind & [fail?]] {:kind kind :edge edge :action action :t t :fail? fail?})

(deftest the-setting-is-the-mean-seconds-of-a-craft-action
  (is (= {:n 2 :timeouts 0 :mean 1.5 :setting 1.5}
         (c/report [(row :start "a" 0 :craft) (row :end "a" 1000 :craft) (row :start "b" 5000 :craft) (row :end "b" 7000 :craft)
                    (row :start "c" 0 :dig) (row :end "c" 90000 :dig)])) "other actions do not count")
  (is (= {:n 1 :timeouts 1 :timeout-s 30 :mean 1 :setting 1}
         (c/report [(row :start "a" 0 :craft) (row :end "a" 1000 :craft) (row :start "b" 0 :craft) (row :end "b" 30000 :craft true)])) "a timeout is counted apart")
  (is (= {:n 0 :timeouts 0} (c/report []))))
