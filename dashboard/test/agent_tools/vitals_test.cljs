(ns agent-tools.vitals-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.vitals :as vitals]))

(def calm {:health 20 :food 18 :position {:x 0 :y 64 :z 0}})

(deftest a-fall-in-health-is-said-first-with-the-amount
  (let [v (vitals/view calm (assoc calm :health 14) {})]
    (is (= :hurt (ffirst v)))
    (is (= "lost 6 health during the wait (20 -> 14)" (:hurt v)))
    (is (= [14 18] ((juxt :health :food) v)))))

(deftest no-drop-means-no-hurt-line-and-a-timeout-still-reports-health
  (let [v (vitals/view calm calm {})]
    (is (not (contains? v :hurt)))
    (is (= 20 (:health v)))))

(deftest an-exclusive-run-adds-the-reflexes-off-reminder
  (is (re-find #"reflexes are off" (:reflexes-off (vitals/view calm (assoc calm :exclusive {:job "j1"}) {}))))
  (is (not (contains? (vitals/view calm calm {}) :reflexes-off))))

(deftest hostiles-seen-are-named-with-distance-and-heard-ones-listed
  (let [rows [{:type "zombie" :id 1 :sense :seen :pos {:x 3 :y 64 :z 4}}
              {:type "cow" :id 2 :sense :seen :pos {:x 1 :y 64 :z 1}}
              {:type "zombie" :id 3 :sense :seen :pos {:x 90 :y 64 :z 0}}
              {:type "skeleton" :sense :heard :direction :north :band :near}]
        seen (vitals/hostiles-near rows "26.1" (:position calm))
        v (vitals/view calm calm seen)]
    (is (= ["zombie 5"] (:hostiles v)))
    (is (= ["skeleton north"] (:heard v)))))

(deftest read-never-rejects-when-the-engine-is-silent
  (async done
    (-> (vitals/read! "/nowhere/agents/B/engine/events.sock" (fn [& _] (js/Promise.reject (js/Error. "down"))))
        (.then (fn [status] (is (= {} status)) (done))))))
