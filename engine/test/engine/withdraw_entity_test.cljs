(ns engine.withdraw-entity-test
  "jobs.storage.withdraw-entity against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.loot-cart-test :as lc]
            [engine.test-util :as tu :refer [child-outcome]]))

(def job 'jobs.storage.withdraw-entity)

(deftest moves-every-stack-of-the-entity
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lc/setup {:entities [(lc/cart 50 [2 64 0])]})
              r (await (child-outcome eng job {:id 50} 12))]
          (is (= {"iron_ingot" 3 "rail" 16 "bread" 2} (lc/inv p)))
          (is (= [:done false] [(:status r) (:full r)])))))))

(deftest a-missing-entity-is-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (lc/setup {})
              r (await (child-outcome eng job {:id 77} 12))]
          (is (= [:stopped :missing] [(:status r) (:reason r)])))))))
