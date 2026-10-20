(ns engine.pace-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.test-util :as tu]
            [jobs.lib.pace :as pace]))

(defn ^:async run-steps [c results]
  (let [left (atom results)]
    (await (pace/steps! c (fn [] (let [r (first @left)] (swap! left rest) r))))))

(deftest a-reflex-never-gives-continue-from-its-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :continue (await (run-steps {} [:continue]))) "a listed job yields")
        (is (= [:done] (await (run-steps {:reflex :needs-weapon} [:continue :continue [:done]]))) "a reflex's :continue is another step")
        (is (= (contract/ending :condition-failed {:reason :dark} nil)
               (await (run-steps {:reflex :needs-weapon :child-wait (atom {:reason :dark})} [:continue [:done]])))
            "a waiting child ends the reflex with its reason")))))
