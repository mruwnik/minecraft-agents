(ns engine.access-foreign-test
  "jobs.lib.access/foreign-holder: a query over zones, claims and other bodies' plans, never a refusal."
  (:require [cljs.test :refer [deftest is are]]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.world :as known]))

(def zone {:name "garden" :owner "Miles" :min [0 60 0] :max [9 70 9]})

(defn holder [pos]
  (with-redefs [ctx/self-name (constantly "Bot")
                ctx/now (constantly 1000)
                known/zones (constantly [zone])
                known/claims (constantly [])
                known/plan-authors (constantly {"hut" "Miles" "mine-hut" "bot"})
                known/footprints (fn ([_] {[20 64 20] "hut" [30 64 30] "mine-hut"}) ([_ _] {[20 64 20] "hut" [30 64 30] "mine-hut"}))]
    (access/foreign-holder {} pos)))

(deftest foreign-holder-sees-zones-and-plans-of-others-only
  (are [pos expected] (= expected (holder pos))
    {:x 1.5 :y 64 :z 1.5} {:kind :zone :name "garden" :owner "Miles"}
    {:x 20.5 :y 64 :z 20.5} {:kind :plan :plan "hut" :owner "Miles"}
    {:x 30.5 :y 64 :z 30.5} nil
    {:x 50 :y 64 :z 50} nil))
