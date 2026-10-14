(ns engine.fetch-plan-test
  "jobs.lib.fetch/plan-for: which fetch a wait reason asks for."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.fetch :as fetch]))

(def opts {:what #{:tool :item} :explore false})

(deftest a-contract-childs-cause-map-is-read-as-the-wait
  (let [pl (fetch/plan-for {:kind :condition-failed :cause {:reason :no-tool :block "stone"}} opts)]
    (is (= 'jobs.items.get-tool (:job pl)))
    (is (= [:tool "stone"] (:key pl)))))

(deftest the-waits-own-reason-wins-over-its-cause
  (let [pl (fetch/plan-for {:reason :need :item "coal" :cause {:reason :no-tool :block "stone"}} opts)]
    (is (= 'jobs.items.obtain (:job pl)))
    (is (= [:item ["coal"]] (:key pl)))))

(deftest a-wait-without-a-cause-map-is-unchanged
  (is (= [:tool "stone"] (:key (fetch/plan-for {:reason :no-tool :block "stone" :cause :x} opts))))
  (is (nil? (fetch/plan-for {:reason :cooking} opts))))
