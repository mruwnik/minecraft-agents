(ns engine.lit-support
  "A :once fixture for the tests of jobs bound to jobs.lib.ready/safe-at: the fake world has no light data, so a place
  would stay unjudged (:look) for ever; here the exposure limit is above any chance, so only the threat check is left."
  (:require [engine.settings :as settings]))

(def fixture
  (let [saved (atom nil)]
    {:before #(do (reset! saved @settings/state)
                  (swap! settings/state assoc-in [:values :jobs.lib.ready/risk-limit] 1))
     :after #(reset! settings/state @saved)}))
