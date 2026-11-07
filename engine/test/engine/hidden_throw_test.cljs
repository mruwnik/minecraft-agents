(ns engine.hidden-throw-test
  "The harness in engine.timing-test: a throw inside a call-guarded check fails the test unless it is allowed."
  (:require [cljs.test :refer [deftest is]]
            [engine.core :as core]
            [engine.core.base :as base]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.timing-test :as timing]))

(defn guarded-throw! [what]
  (let [eng (core/create {:primitives (tu/seeing-all (tu/fake {})) :jobs registry/jobs :dir (tu/tmp-dir)
                          :events (events/make {:sinks [(fn [_])]})})]
    (base/call-guarded eng what :fallback #(throw (js/Error. "boom")))
    (core/shutdown! eng)))

(deftest a-guarded-throw-is-recorded-for-the-harness
  (guarded-throw! "probe check")
  (is (= ["probe check threw: Error: boom"] @timing/throws))
  (reset! timing/throws []))

(deftest an-expected-guarded-throw-is-not-a-failure
  (timing/allow-throws! #"probe check threw")
  (guarded-throw! "probe check"))

(deftest a-throw-between-tests-is-not-charged-to-a-test
  (let [was @timing/current]
    (reset! timing/current nil)
    (guarded-throw! "late check")
    (reset! timing/current was)
    (is (= [] @timing/throws))
    (is (= ["late check threw: Error: boom"] @timing/between))
    (reset! timing/between [])))
