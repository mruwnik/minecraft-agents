(ns engine.withdraw-entity-test
  "jobs.storage.withdraw-entity against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.loot-cart-test :as lc]
            [jobs.lib.world-files :as ew]
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

(deftest a-failed-transfer-is-nothing-possible-not-a-full-bag
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lc/setup {:entities [(lc/cart 50 [2 64 0])]})
              _ (.override (.-world p) "transfer" (fn [_ _ _] #js {:status "timeout" :moved 0}))
              r (await (child-outcome eng job {:id 50} 12))]
          (is (= {:kind :nothing-possible :cause :timeout} (select-keys (:ending r) [:kind :cause]))))))))

(deftest a-transfer-with-no-room-from-the-start-is-bag-full-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lc/setup {:entities [(lc/cart 50 [2 64 0])]})
              _ (.override (.-world p) "transfer" (fn [_ _ _] #js {:status "full" :moved 0}))
              r (await (child-outcome eng job {:id 50} 12))]
          (is (= {:kind :condition-failed :cause :bag-full} (select-keys (:ending r) [:kind :cause]))))))))

(deftest a-transfer-with-no-room-after-others-moved-ends-done-full
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lc/setup {:entities [(lc/cart 50 [2 64 0])]})
              calls (atom 0)
              _ (.override (.-world p) "transfer"
                           (fn [_ _ _] (if (= 1 (swap! calls inc)) #js {:status "ok" :moved 3} #js {:status "full" :moved 0})))
              r (await (child-outcome eng job {:id 50} 12))]
          (is (= [:done true] [(:status r) (:full r)])))))))

(deftest a-call-without-an-id-or-into-another-owners-zone-does-not-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args zones reason] [[{} [] :bad-args]
                                     [{:id 50} [{:name "vault" :min [1 60 -1] :max [3 70 1] :owner "Miles"}] :refused]]]
          (let [{:keys [eng p]} (lc/setup {:entities [(lc/cart 50 [2 64 0])]} (ew/of-data {} {} zones))
                r (await (child-outcome eng job args 12))]
            (is (= reason (get-in r [:ending :cause :reason])) (pr-str args))
            (is (= {} (lc/inv p)) (pr-str args))))))))

(deftest ignore-zones-empties-an-entity-in-another-owners-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "vault" :min [1 60 -1] :max [3 70 1] :owner "Miles"}]
              {:keys [eng p]} (lc/setup {:entities [(lc/cart 50 [2 64 0])]} (ew/of-data {} {} zones))
              r (await (child-outcome eng job {:id 50 :ignore-zones? true} 12))]
          (is (= :done (:status r)))
          (is (= {"iron_ingot" 3 "rail" 16 "bread" 2} (lc/inv p))))))))
