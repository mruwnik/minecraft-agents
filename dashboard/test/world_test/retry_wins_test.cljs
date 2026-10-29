(ns world-test.retry-wins-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.retry-wins :as r]))

(defn action [job kind t status]
  (pr-str (cond-> {:generation-id "g" :time-ms t :source :action :kind kind :context {:job-id job :action-id job}
                   :data {:name "craft"}}
            status (assoc-in [:data :status] status))))

(defn walk [job t arrived]
  (pr-str {:generation-id "g" :time-ms t :source :job :kind :result :context {:job-id job} :data {:arrived arrived}}))

(deftest the-try-after-a-failed-one-counts-won-or-failed-with-its-seconds
  (let [rows (keep r/scan-line [(action "j1/craft" :done 1000 "timeout") (action "j1/craft" :done 3000 "crafted")
                                (action "j2/craft" :done 1000 "timeout") (action "j2/craft" :done 2000 "timeout")
                                (action "j2/craft" :done 3000 "timeout")
                                (walk "j3/go-to" 0 false) (walk "j3/go-to" 4000 true) (walk "j4/go-to" 0 true)])
        {:keys [craft walk]} (r/report rows)]
    (is (= {:retries 3 :calls 2} (select-keys craft [:retries :calls])) "a won try ends the run of a call's fails")
    (is (= 1.333 (:try-s craft)) "a retry's seconds run from the failed try's end")
    (is (> (:fail craft) (:win craft)) "one call won one retry of three")
    (is (= {:win 1 :fail 0 :try-s 4 :retries 1 :calls 1} walk) "a walk that did not arrive, then did")))

(deftest a-kind-with-no-retry-is-priced-by-its-tries-on-the-uniform-prior
  (is (= {:craft {:win 1 :fail 1 :try-s 2 :retries 0 :calls 0}}
         (r/report (keep r/scan-line [(action "j1/craft" :started 1000 nil) (action "j1/craft" :done 3000 "crafted")])))))
