(ns engine.scenario-register-test
  "A scenario with no :register gets the default trigger set in its listed order; an explicit :register [] gets none;
  an explicit list gets exactly that list."
  (:require [cljs.test :refer [deftest is]]
            [engine.core :as core]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.trigger-api :as trigger-api]
            [engine.triggers :as triggers]))

(def all-triggers (trigger-api/with-conditions triggers/all trigger-api/compile-condition))

(defn loaded-ids
  "The register ids of a fresh engine after loading the scenario, as main does (defaults filled in)."
  [plan]
  (let [eng (core/create {:primitives (tu/fake-on-floor {}) :jobs registry/jobs :triggers all-triggers :dir (tu/tmp-dir)})
        filled (scenario/with-defaults plan)]
    (is (= [] (scenario/problems registry/jobs all-triggers filled)))
    (trigger-api/load-scenario! eng filled)
    (mapv :id (:register (core/state eng)))))

(deftest missing-register-gives-the-defaults-in-order
  (is (= [:suffocating :burning :wedged :hostile-near :night :hungry :door-left :died
          :inventory-nearly-full :scaffold-left :tidy-pending :pen-gate :mounted]
         (loaded-ids {})
         (loaded-ids '{:queue [(jobs.movement.look-around)]}))))

(deftest every-default-trigger-is-in-the-default-register
  (is (= (set (keys triggers/all)) (set (loaded-ids {})))))

(deftest empty-register-gives-no-triggers
  (is (= [] (loaded-ids {:register []}))))

(deftest explicit-register-gives-exactly-that-list
  (is (= [:hungry :night] (loaded-ids {:register [{:trigger :hungry} {:trigger :night}]}))))
