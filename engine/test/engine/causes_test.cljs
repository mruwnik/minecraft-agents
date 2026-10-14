(ns engine.causes-test
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.causes :as causes]))

(deftest a-cause-reads-as-its-family
  (are [cause family] (= family (causes/family cause))
    :no-sapling :need
    :chest-lacks :need
    :unreachable :no-path
    {:reason :unreachable :why :exhausted} :no-path
    {:line :food :reason :hungry} :food
    :nothing-to-tidy :already
    :duplicate-call :duplicate-call
    :walked-into-a-wall nil))

(deftest an-ending-reads-as-the-family-of-its-cause
  (is (= :tool (causes/family {:kind :condition-failed :cause :tool-spent})))
  (is (nil? (causes/family {:kind :done :cause nil}))))

(deftest no-reason-sits-in-two-families
  (is (= (count (distinct (mapcat val causes/families))) (count (mapcat val causes/families)))))
