(ns engine.apiary-rounds-test
  "The apiary jobs run as one whole attempt: a single tick of the engine works every fire or hive."
  (:require [cljs.test :refer [deftest is async]]
            [engine.apiary-guard-test :as g]
            [engine.apiary-harvest-test :as h]
            [engine.apiary-maintain-test :as m]
            [engine.core :as core]
            [engine.test-util :as tu]))

(defn two-fires [inventory]
  (let [a (g/fire-world {:inventory inventory})
        b (g/fire-world {:z 6})]
    (-> a (update :blocks merge (:blocks b)) (update :states merge (:states b)))))

(deftest guard-carpets-every-fire-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (g/setup (two-fires (g/inv "white_carpet" 2)) true)
              result (await (tu/child-outcome eng g/job {} 1))]
          (is (= 2 (:carpeted result)))
          (is (= :guarded (:reason result)))
          (is (= "white_carpet" (g/block-name p 2 65 0)))
          (is (= "white_carpet" (g/block-name p 2 65 6))))))))

(deftest harvest-takes-every-ripe-hive-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (h/world {:inventory (h/inv "shears" 1)})
              w (-> w
                    (update :blocks assoc "2,64,6" "beehive" "2,63,6" "campfire")
                    (update :states assoc "2,64,6" {:honey_level 5} "2,63,6" {:lit true}))
              {:keys [eng p]} (h/setup w)
              result (await (tu/child-outcome eng h/job {} 1))]
          (is (= 2 (:harvested result)))
          (is (= 6 (h/carried p "honeycomb"))))))))

(deftest maintain-runs-every-step-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (m/scenario {} (m/world {:inventory m/kit}) 1))]
          (is (= 1 (:carpeted (m/step s :guard))))
          (is (= 1 (:harvested (m/step s :harvest))))
          (is (true? (m/finished? s))))))))
