(ns dashboard.server-settings-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.server.settings :as settings]))

(deftest the-dashboard-request-becomes-an-engine-call
  (is (= ["/settings/set" {:key :jobs.items.smelt/wait-ms :value 9}]
         (settings/engine-request {:op :set :key :jobs.items.smelt/wait-ms :value 9})))
  (is (= ["/settings/set" {:key :a/b :value nil}] (settings/engine-request {:op :set :key :a/b :value nil})))
  (is (= ["/settings/unset" {:key :a/b}] (settings/engine-request {:op :unset :key :a/b :value 1})))
  (is (= ["/settings/reload" {}] (settings/engine-request {:op :reload}))))

(deftest a-malformed-dashboard-request-has-no-engine-call
  (doseq [bad [nil [] {} {:op :set :key :a/b} {:op :set :key "a/b" :value 1} {:op :unset} {:op :rm :key :a/b}]]
    (is (nil? (settings/engine-request bad)) (pr-str bad))))

(deftest an-engine-refusal-keeps-its-status
  (let [refused (fn [status] (doto (js/Error. "x") (aset "status" status) (aset "engineBody" {:reason "r"})))]
    (is (= 400 (settings/failure-status (refused 400))))
    (is (= 409 (settings/failure-status (refused 409))))
    (is (= 409 (settings/failure-status (doto (js/Error. "x") (aset "engineBody" {:reason "r"})))))
    (is (= 503 (settings/failure-status (js/Error. "socket"))))))
