(ns engine.checks-test
  "jobs.lib.checks: the statuses of the checks jobs share."
  (:require [cljs.test :refer [deftest is are]]
            [engine.ctx :as ctx]
            [engine.test-util :as tu]
            [jobs.lib.checks :as checks]
            [jobs.lib.submitted :as submitted]
            [jobs.lib.world :as known]))

(deftest args-fit-is-ok-or-never-bad-args-with-the-text
  (let [check (checks/args-fit "needs a name" #(string? (:name (:args %))))]
    (are [args expected] (= expected (tu/kind-reason (check {:args args})))
      {:name "a"} [:ok :args]
      {:name 5} [:never :bad-args])
    (is (= "needs a name" (second (check {:args {}}))))))

(deftest the-cause-map-becomes-reason-and-data
  (is (= [:later "text" {:reason :zone :zone "keep"}] (checks/later-of "text" {:reason :zone :zone "keep"})))
  (is (= [:never "text" {:reason :claim :id 3}] (checks/never-of "text" {:reason :claim :id 3}))))

(deftest unseen-later-asks-for-a-look-until-the-body-has-looked
  (are [mem expected] (= expected (with-redefs [ctx/mem (constantly mem)]
                                    (tu/kind-reason (checks/unseen-later {} :no-tree "none" :near 3))))
    {:looked {:x 0 :y 64 :z 0}} [:later :no-tree]
    {} [:look :no-tree]))

(deftest not-moved-is-later-with-the-distance-only-when-the-body-left
  (with-redefs [submitted/away-cause (constantly nil)]
    (is (= [:ok :here] (tu/kind-reason (checks/not-moved {})))))
  (with-redefs [submitted/away-cause (constantly {:reason :moved-since-submit :distance 9})]
    (is (= [:later :moved-since-submit] (tu/kind-reason (checks/not-moved {}))))
    (is (= 9 (:distance (nth (checks/not-moved {}) 2))))))

(deftest zones-read-waits-for-a-zone-list-unless-zones-are-ignored
  (are [zones args expected] (= expected (with-redefs [known/zones (constantly zones)]
                                           (tu/kind-reason (checks/zones-read {:args args}))))
    nil {} [:later :no-zones]
    nil {:ignore-zones? true} [:ok :zones-read]
    [] {} [:ok :zones-read]
    [{:name "a"}] {} [:ok :zones-read]))

(deftest holds-is-ok
  (is (= [:ok :holds] (tu/kind-reason (checks/holds)))))
