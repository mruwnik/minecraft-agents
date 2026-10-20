(ns engine.stuck-underground-test
  "The :stuck-underground trigger: an idle body deep underground walks home."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [triggers.survival.stuck-underground :as su]))

(def roof {"0,52,0" "stone"})

(defn holds?
  "The trigger over a fake body at (0 50 0) under a roof, with places [[kind x y z] ..] remembered and job memory [id ..] listed."
  [places & [over jobs]]
  (let [store (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (doseq [[kind x y z] places]
      (mem/write! store kind {:pos {:x x :y y :z z}} mem/place-policy))
    (doseq [id jobs]
      (mem/write! store (mem/job-kind id) {:started true} mem/place-policy))
    (su/stuck-underground (tu/seeing-all (tu/fake-on-floor (merge {:floor [-4 -4 4 4] :self {:pos {:x 0.5 :y 50 :z 0.5}} :blocks roof} over)))
                          (mem/view store) su/defaults nil nil)))

(deftest an-idle-buried-body-far-below-its-home-is-stuck
  (are [places expected] (= expected (holds? places))
    [[:home 30 64 0]] true
    [[:bed 30 64 0]] true
    [[:home 30 54 0]] false
    [[:home 1 55 1]] false
    [] false))

(deftest a-body-under-the-open-sky-or-with-a-listed-job-is-not-stuck
  (is (false? (holds? [[:home 30 64 0]] {:blocks {}})))
  (is (false? (holds? [[:home 30 64 0]] nil ["j1"]))))
