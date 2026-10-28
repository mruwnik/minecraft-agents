(ns engine.checks-test
  "jobs.lib.checks: the statuses of the checks jobs share."
  (:require [cljs.test :refer [deftest is are]]
            [engine.ctx :as ctx]
            [engine.test-util :as tu]
            [jobs.lib.checks :as checks]
            [jobs.lib.ready :as ready]
            [jobs.lib.world :as known]))

(deftest args-fit-is-ok-or-never-bad-args-with-the-text
  (let [check (checks/args-fit "needs a name" #(string? (:name (:args %))))]
    (are [args expected] (= expected (tu/kind-reason (check {:args args})))
      {:name "a"} [:ok :args]
      {:name 5} [:never :bad-args])
    (is (= "needs a name" (second (check {:args {}}))))))

(deftest the-cause-map-becomes-reason-and-data
  (is (= [:later "text" {:reason :zone :zone "keep"}] (checks/cause-status :later {:reason :zone :zone "keep"} "text")))
  (is (= [:never "text" {:reason :claim :id 3}] (checks/cause-status :never {:reason :claim :id 3} "text"))))

(deftest a-refusal-text-names-what-refused
  (are [cause text] (= text (second (checks/cause-status :later cause)))
    {:reason :not-allowed :by :footprint :plan "hollis-field" :pos [1 2 3]} "refused by plan hollis-field"
    {:reason :not-allowed :by :zone :zone "den" :owner "Miles"} "refused by zone den"
    {:reason :not-allowed :by :no-zones} "refused: no-zones"
    {:reason :refused :zones ["den"] :claims [] :plans ["jizo-farm"]} "refused by zone den; plan jizo-farm"
    {:reason :refused :zones [] :claims [] :plans []} "refused"))

(deftest no-cause-is-ok
  (is (= [:ok :holds] (tu/kind-reason (checks/cause-status :later nil)))))

(deftest zones-read-waits-for-a-zone-list-unless-zones-are-ignored
  (are [zones args expected] (= expected (with-redefs [known/zones (constantly zones)]
                                           (tu/kind-reason (checks/zones-read {:args args}))))
    nil {} [:later :no-zones]
    nil {:ignore-zones? true} [:ok :zones-read]
    [] {} [:ok :zones-read]
    [{:name "a"}] {} [:ok :zones-read]))

(deftest refusing-takes-a-kind-or-a-fn-of-the-call
  (are [kind expected] (= expected (tu/kind-reason ((checks/refusing kind :ok-r "ok" (constantly :why)) {})))
    :never [:never :why]
    (constantly :look) [:look :why]))

(deftest a-spectator-or-creative-body-waits-and-survival-or-unreported-is-ready
  (are [mode expected] (let [c {:primitives (tu/fake {:self (cond-> {} mode (assoc :gameMode mode))})}]
                         (= [expected expected] [(first (ready/fit? c)) (first (checks/survival-mode c))]))
    "spectator" :later
    "creative" :later
    "survival" :ok
    nil :ok))
