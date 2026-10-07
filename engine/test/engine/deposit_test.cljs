(ns engine.deposit-test
  "jobs.storage.deposit against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.withdraw-test :as wt]
            [engine.test-util :as tu]))

(def job 'jobs.storage.deposit)

(defn setup-far
  "wt/setup with the body seeing the world, so go-to can plan the walk."
  [world]
  (let [r (wt/setup world)]
    (assoc r :p (tu/seeing-all (:p r)))))
(def chest {:x 10 :y 64 :z 0})

(deftest deposit-puts-the-item-in-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})
              result (await (tu/child-outcome eng job {:chest chest :items ["dirt"]} 8))]
          (is (= {} (wt/inv p)))
          (is (= {:gave-up false} result)))))))

(deftest deposit-gives-up-when-the-transfer-says-ok-but-moves-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] #js {:status "ok" :moved 0}))
          (let [result (await (tu/child-outcome eng job {:chest chest :items ["dirt"]} 12))]
            (is (= true (:gave-up result)))
            (is (<= (count (wt/calls p "transfer")) 3))))))))

(deftest deposit-of-everything-keeps-the-three-day-food-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5} {:name "bread" :count 20}] :containers {"10,64,0" []}})
              result (await (tu/child-outcome eng job {:chest chest} 16))]
          (is (= {"bread" 12} (wt/inv p)) "bread 12 is the 60 points; dirt and the 8 spare bread go")
          (is (= {:gave-up false} result)))))))

(deftest deposit-gives-up-when-the-known-chest-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (wt/setup {:inventory [{:name "dirt" :count 5}]})]
          (wt/know-place! eng :chest chest)
          (is (= {:gave-up true :reason "missing" :status :stopped :moved 0}
                 (await (tu/child-outcome eng job {:items ["dirt"]} 8))))
          (is (some #(= :chest_missing (:kind %)) @seen))
          (is (= [] (:list (core/state eng)))))))))

(deftest deposit-waits-with-a-reason-when-no-chest-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (wt/setup {:inventory [{:name "dirt" :count 5}]})]
          (core/submit! eng (list job {:items ["dirt"]}) {})
          (core/tick! eng)
          (is (some #(and (= :waiting (:kind %)) (= :no-chest (:reason %))) @seen)))))))

(deftest deposit-walks-and-puts-every-stack-away-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-far {:inventory [{:name "dirt" :count 5} {:name "gravel" :count 7} {:name "sand" :count 2}]
                                          :containers {"20,64,0" []}})
              result (await (tu/child-outcome eng job {:chest {:x 20 :y 64 :z 0}} 1))]
          (is (= {} (wt/inv p)))
          (is (= {:gave-up false} result))
          (is (= 3 (count (wt/calls p "transfer"))))
          (is (= 1 (count (tu/walked-to eng)))))))))

(deftest deposit-into-a-chest-that-fills-stops-with-the-count-moved
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5} {:name "gravel" :count 7}] :containers {"10,64,0" []}})
              n (atom 0)]
          (.override (.-world p) "transfer"
                     (fn ^:async f [token args impl]
                       (if (= 1 (swap! n inc))
                         (await (impl token args))
                         #js {:status "full" :moved 0})))
          (let [result (await (tu/child-outcome eng job {:chest chest :items ["dirt" "gravel"]} 1))]
            (is (= {"gravel" 7} (wt/inv p)))
            (is (= {:gave-up true :reason "full" :status :stopped :moved 5} result))))))))

(defn timed-out
  "An override of a primitive that does the work, then answers an act timeout carrying the inventory change."
  [change]
  (fn ^:async f [token args impl]
    (await (impl token args))
    #js {:status "timeout" :inventoryChange (clj->js change)}))

(deftest deposit-counts-what-moved-before-a-transfer-timed-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})]
          (.override (.-world p) "transfer" (timed-out {"dirt" -5}))
          (let [result (await (tu/child-outcome eng job {:chest chest :items ["dirt"]} 12))]
            (is (= {:gave-up false} result))
            (is (= {} (wt/inv p)))
            (is (= 1 (count (wt/calls p "transfer"))))))))))

(deftest deposit-gives-up-on-a-timeout-that-moved-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] #js {:status "timeout"}))
          (let [result (await (tu/child-outcome eng job {:chest chest :items ["dirt"]} 12))]
            (is (= true (:gave-up result)))))))))
