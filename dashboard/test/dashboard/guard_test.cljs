(ns dashboard.guard-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.guard :as guard]))

(def good {:method "POST" :host "127.0.0.1:3705" :origin "http://127.0.0.1:3705" :content-type "application/json" :port 3705})

(deftest refusal
  (are [overrides expected] (= expected (guard/refusal (merge good overrides)))
    {} nil
    {:origin nil} nil
    {:host "localhost:3705" :origin "http://localhost:3705"} nil
    {:host "[::1]:3705" :origin "http://[::1]:3705"} nil
    {:content-type "application/json; charset=utf-8"} nil
    {:host "evil.com:3705"} {:status 403 :error "bad Host"}
    {:host "127.0.0.1:3701"} {:status 403 :error "bad Host"}
    {:host "127.0.0.1"} {:status 403 :error "bad Host"}
    {:host nil} {:status 403 :error "bad Host"}
    {:host "127.0.0.1:3705.evil.com"} {:status 403 :error "bad Host"}
    {:origin "http://evil.com"} {:status 403 :error "bad Origin"}
    {:origin "null"} {:status 403 :error "bad Origin"}
    {:origin "https://127.0.0.1:3705"} {:status 403 :error "bad Origin"}
    {:origin "http://localhost:3705"} {:status 403 :error "bad Origin"}
    {:content-type "application/x-www-form-urlencoded"} {:status 415 :error "Content-Type must be application/json"}
    {:content-type "text/plain"} {:status 415 :error "Content-Type must be application/json"}
    {:content-type nil} {:status 415 :error "Content-Type must be application/json"}))

(deftest method-gate
  (are [method expected] (= expected (guard/method-refusal method))
    "POST" nil
    "GET" {:status 405 :error "POST only"}
    "PUT" {:status 405 :error "POST only"}
    "OPTIONS" {:status 405 :error "POST only"}))

(deftest body-limit
  (is (= 4096 guard/max-body-bytes)))
