(ns dashboard.ui.drive-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.drive :as d]))

(def me "dashboard-k3x9ab")

(deftest ours-cases
  (are [driver expected] (= expected (d/ours-who? me driver))
    "dashboard-k3x9ab" true
    "dashboard" false
    "view" false
    "dashboard-zzzzzz" false
    "manual-bob" false
    nil false)
  (is (false? (d/ours-who? nil nil))))

(deftest new-who-format
  (are [r expected] (= expected (d/new-who (constantly r)))
    0 "dashboard-000000"
    0.999 "dashboard-zzzzzz")
  (is (re-matches #"dashboard-[0-9a-z]{6}" (d/new-who js/Math.random)))
  (is (not= (d/new-who js/Math.random) (d/new-who js/Math.random))))

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
  (are [drive expected] (= expected (d/banner drive 1000 me))
    {} {:kind :none}
    {:manual nil} {:kind :none}
    {:manual {:who "dashboard-k3x9ab"}} {:kind :ours :text "MANUAL: you are driving (dashboard-k3x9ab)" :countdown nil}
    {:manual {:who "dashboard-k3x9ab"} :expires-at 13000} {:kind :ours :text "MANUAL: you are driving (dashboard-k3x9ab)" :countdown "auto-release in 12 s"}
    {:manual {:who "view"}} {:kind :other :text "driven by view"}
    {:manual {:who "dashboard-other1"}} {:kind :other :text "driven by dashboard-other1"}
    {:manual {:who "someone"} :driving? true} {:kind :ours :text "MANUAL: you are driving (someone)" :countdown nil}
    {:manual {:who "bob" :why "testing"}} {:kind :other :text "driven by bob: testing"}
    {:manual {:who "bob"}} {:kind :other :text "driven by bob"}))

(deftest apply-poll-cases
  (are [drive manual expected] (= expected (d/apply-poll drive manual 7 me))
    {:manual {:who "dashboard-k3x9ab"} :expires-at 99} nil {:manual nil :expires-at nil :driving? false :at 7}
    {} {:who "bob"} {:manual {:who "bob"} :driving? false :expires-at nil :at 7}
    {} {:who "dashboard-k3x9ab" :expiresAt 42} {:manual {:who "dashboard-k3x9ab" :expiresAt 42} :expires-at 42 :at 7}
    {:expires-at 99 :manual {:who "dashboard-k3x9ab"}} {:who "dashboard-k3x9ab"} {:manual {:who "dashboard-k3x9ab"} :expires-at 99 :at 7}
    {} {:who "dashboard"} {:manual {:who "dashboard"} :driving? false :expires-at nil :at 7}
    {:driving? true :expires-at 99} {:who "bob"} {:manual {:who "bob"} :driving? false :expires-at nil :at 7}))

(deftest apply-message-cases
  (are [msg expected] (= expected (d/apply-message {:at 1} msg 7))
    {:driving? true :manual {:who "dashboard"} :expires-at 50} {:driving? true :manual {:who "dashboard"} :expires-at 50 :at 7}
    {:driving? false :manual nil :expires-at nil} {:driving? false :manual nil :expires-at nil :at 7}))

(deftest requests
  (are [actual expected] (= expected actual)
    (d/take-request me) {:op "take" :who me :why "dashboard"}
    (d/release-request {:manual {:who "bob"}} me) {:op "release" :who me}
    (d/release-request {} me) {:op "release" :who me}))

(deftest driving-now-cases
  (are [drive expected] (= expected (d/driving-now? drive me))
    {} false
    {:manual {:who "bob"}} false
    {:manual {:who "dashboard"}} false
    {:manual {:who "dashboard-k3x9ab"}} true
    {:driving? true :manual {:who "x"}} true))
