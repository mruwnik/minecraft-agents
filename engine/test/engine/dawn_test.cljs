(ns engine.dawn-test
  "jobs.lib.dawn/hostile-hold?: how long a body in shelter stays in at first light with a hostile known near."
  (:require [cljs.test :refer [deftest is are]]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.dawn :as dawn]))

(def husk {:id 7 :name "husk" :kind "hostile" :pos {:x 12 :y 64 :z 0}})

(defn holds? [time armed? entities]
  (dawn/hostile-hold? (tu/fake-on-floor {:floor [-10 -10 10 10] :entities entities}) armed? (dawn/ticks-since-dawn time)))

(deftest an-unarmed-body-stays-in-past-the-dawn-grace-while-a-hostile-is-near
  (are [time armed? expected] (= expected (holds? time armed? [husk]))
    400 true true
    1000 true false
    1000 false true
    6000 false false)
  (is (false? (holds? 1000 false [])) "no known hostile, nothing to wait for")
  (is (false? (settings/with-settings {::dawn/with-hostile true} #(holds? 1000 false [husk])))))
