(ns engine.deposit-test
  "jobs.storage.deposit against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.withdraw-test :as wt]
            [engine.test-util :as tu]))

(def job 'jobs.storage.deposit)
(def chest {:x 10 :y 64 :z 0})

(deftest deposit-puts-the-item-in-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})
              result (await (wt/child-outcome eng job {:chest chest :items ["dirt"]} 8))]
          (is (= {} (wt/inv p)))
          (is (= {:gave-up false} result)))))))

(deftest deposit-gives-up-when-the-transfer-says-ok-but-moves-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5}] :containers {"10,64,0" []}})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] #js {:status "ok" :moved 0}))
          (let [result (await (wt/child-outcome eng job {:chest chest :items ["dirt"]} 12))]
            (is (= true (:gave-up result)))
            (is (<= (count (wt/calls p "transfer")) 3))))))))

(deftest deposit-of-everything-keeps-the-three-day-food-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wt/setup {:inventory [{:name "dirt" :count 5} {:name "bread" :count 12}] :containers {"10,64,0" []}})
              result (await (wt/child-outcome eng job {:chest chest} 16))]
          (is (= {"bread" 8} (wt/inv p)) "bread 8 is 40 of the 36 points; dirt and the 4 spare bread go")
          (is (= {:gave-up false} result)))))))
