(ns dashboard.ui.api-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.ui.api :as api]))

(deftest edn-outcome-cases
  (are [ok? status text expected] (= expected (api/edn-outcome ok? status text))
    true 200 "{:a 1}" [:ok {:a 1}]
    false 500 "{:error \"boom\"}" [:err "boom"]
    false 502 "<html>" [:err "http 502"]
    false 404 "nil" [:err "http 404"]))

(deftest an-unparseable-ok-body-is-an-error
  (let [[outcome message] (api/edn-outcome true 200 "{:a")]
    (is (= :err outcome))
    (is (re-find #"^bad reply: " message))))
