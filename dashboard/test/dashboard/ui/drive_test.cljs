(ns dashboard.ui.drive-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.drive :as d]))

(deftest ours-cases
  (are [who expected] (= expected (d/ours-who? who))
    "dashboard" true
    "view" true
    "manual-bob" false
    nil false))

(deftest parse-message-cases
  (are [data expected] (= expected (d/parse-message data))
    {:type "drive" :driving true :manual {:who "dashboard"} :expiresAt 5000} {:driving? true :manual {:who "dashboard"} :expires-at 5000}
    {:type "drive" :driving false :manual nil} {:driving? false :manual nil :expires-at nil}
    {:type "other"} nil
    nil nil
    "x" nil))

(deftest countdown-cases
  (are [expires now expected] (= expected (d/countdown-text expires now))
    nil 0 nil
    13000 1000 "auto-release in 12 s"
    1500 1000 "auto-release in 1 s"
    1000 1000 "auto-release now"
    500 1000 "auto-release now"))

(deftest banner-cases
  (are [drive expected] (= expected (d/banner drive 1000))
    {} {:kind :none}
    {:manual nil} {:kind :none}
    {:manual {:who "dashboard"}} {:kind :ours :text "MANUAL: you are driving (dashboard)" :countdown nil}
    {:manual {:who "dashboard"} :expires-at 13000} {:kind :ours :text "MANUAL: you are driving (dashboard)" :countdown "auto-release in 12 s"}
    {:manual {:who "view"}} {:kind :ours :text "MANUAL: you are driving (view)" :countdown nil}
    {:manual {:who "someone"} :driving? true} {:kind :ours :text "MANUAL: you are driving (someone)" :countdown nil}
    {:manual {:who "bob" :why "testing"}} {:kind :other :text "driven by bob: testing"}
    {:manual {:who "bob"}} {:kind :other :text "driven by bob"}))

(deftest apply-poll-cases
  (are [drive manual expected] (= expected (d/apply-poll drive manual 7))
    {:manual {:who "dashboard"} :expires-at 99} nil {:manual nil :expires-at nil :driving? false :at 7}
    {} {:who "bob"} {:manual {:who "bob"} :driving? false :expires-at nil :at 7}
    {} {:who "dashboard" :expiresAt 42} {:manual {:who "dashboard" :expiresAt 42} :expires-at 42 :at 7}
    {:expires-at 99 :manual {:who "dashboard"}} {:who "dashboard"} {:manual {:who "dashboard"} :expires-at 99 :at 7}
    {:driving? true :expires-at 99} {:who "bob"} {:manual {:who "bob"} :driving? false :expires-at nil :at 7}))

(deftest apply-message-cases
  (are [msg expected] (= expected (d/apply-message {:at 1} msg 7))
    {:driving? true :manual {:who "dashboard"} :expires-at 50} {:driving? true :manual {:who "dashboard"} :expires-at 50 :at 7}
    {:driving? false :manual nil :expires-at nil} {:driving? false :manual nil :expires-at nil :at 7}))

(deftest requests
  (are [actual expected] (= expected actual)
    (d/take-request) {:op "take" :who "dashboard" :why "dashboard"}
    (d/release-request {:manual {:who "view"}}) {:op "release" :who "view"}
    (d/release-request {:manual {:who "bob"}}) {:op "release" :who "dashboard"}
    (d/release-request {}) {:op "release" :who "dashboard"}))

(deftest driving-now-cases
  (are [drive expected] (= expected (d/driving-now? drive))
    {} false
    {:manual {:who "bob"}} false
    {:manual {:who "dashboard"}} true
    {:driving? true :manual {:who "x"}} true))
