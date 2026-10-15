(ns engine.steps-test
  "jobs.lib.steps: the todo walk and the step under way."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.steps :as steps]))

(defn decide [step _args facts]
  (cond
    (contains? (:skip facts) step) {:skip (get (:skip facts) step)}
    :else {:call {:slot step :job (symbol (str "jobs." (name step))) :args {:s step}}}))

(deftest plan-skips-steps-and-calls-the-first-wanted
  (let [r (steps/plan decide [:a :b :c] {} {:skip {:a "no need"}} {})]
    (is (= [:b :c] (:todo r)))
    (is (= {:a {:skipped "no need"}} (:report r)))
    (is (= :b (get-in r [:call :slot])))))

(deftest plan-keeps-an-existing-report-entry
  (let [r (steps/plan decide [:a :b] {} {:skip {:a "now"}} {:a {:done true}})]
    (is (= {:done true} (:a (:report r))))
    (is (= :b (get-in r [:call :slot])))))

(deftest plan-with-everything-skipped-has-no-call
  (let [r (steps/plan decide [:a :b] {} {:skip {:a "x" :b "y"}} {})]
    (is (= [] (:todo r)))
    (is (nil? (:call r)))
    (is (= #{:a :b} (set (keys (:report r)))))))

(deftest plan-of-an-empty-todo-is-empty
  (is (= {:todo [] :report {:z 1} :call nil} (steps/plan decide [] {} {} {:z 1}))))

(deftest running-call-is-the-first-step-with-saved-args
  (let [jobs {:a 'jobs.a}]
    (is (= {:slot :a :job 'jobs.a :args {:x 1}}
           (steps/running-call jobs {:todo [:a :b] :call-args {:x 1}})))
    (is (nil? (steps/running-call jobs {:todo [:a]})))))
