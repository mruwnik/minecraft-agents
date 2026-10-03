(ns engine.registry-test
  (:require [cljs.test :refer [deftest is]]
            [engine.registry :as registry]))

(deftest the-registry-holds-every-job-namespace
  (let [eat (get registry/jobs 'jobs.survival.eat)]
    (is (fn? (:check eat)))
    (is (fn? (:round eat)))
    (is (string? (:doc eat)))
    (is (= nil (get-in eat [:args :item :default])))))
