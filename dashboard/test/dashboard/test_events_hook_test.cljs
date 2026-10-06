(ns dashboard.test-events-hook-test
  "Not much of a test: loading it installs the shared live-tests @@test reporter (engine.test-events) when TEST_EVENTS=1."
  (:require [cljs.test :refer [deftest is]]
            [engine.test-events :as ev]))

(ev/install!)

(deftest reporter-hooks-follow-the-env
  (is (= (= "1" (some-> js/process .-env (aget "TEST_EVENTS"))) (ev/installed?))))
