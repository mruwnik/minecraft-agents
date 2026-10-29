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
    (is (= {:won 1 :retries 3 :calls 2} (select-keys craft [:won :retries :calls])) "a won try ends the run of a call's fails")
    (is (= 1.333 (:try-s craft)) "a retry's seconds run from the failed try's end")
    (is (> (:fail craft) (:win craft)) "one call won one retry of three")
    (is (= {:win 1 :fail 0 :try-s 4 :won 1 :retries 1 :calls 1} walk) "a walk that did not arrive, then did")))

(defn call [job t changed]
  (pr-str {:generation-id "g" :time-ms t :source :job :kind :hostile.call :context {:job-id job} :data {:changed changed :unchanged (if changed 0 1)}}))

(deftest respond-calls-count-as-their-own-kind-and-every-kind-pools
  (let [{:keys [respond pooled]} (r/report (keep r/scan-line [(call "j5" 1000 false) (call "j5" 1500 false) (call "j5" 2500 true)
                                                              (walk "j3/go-to" 0 false) (walk "j3/go-to" 4000 true)]))]
    (is (= {:won 1 :retries 2 :calls 1} (select-keys respond [:won :retries :calls])) "a call after one that changed nothing, won or not")
    (is (= {:won 2 :retries 3 :calls 2} (select-keys pooled [:won :retries :calls])) "the walk's retry joins the respond calls'")))

(deftest a-kind-with-no-retry-is-priced-by-its-tries-on-the-uniform-prior
  (is (= {:craft {:win 1 :fail 1 :try-s 2 :won 0 :retries 0 :calls 0}}
         (r/report (keep r/scan-line [(action "j1/craft" :started 1000 nil) (action "j1/craft" :done 3000 "crafted")])))))

(deftest a-kind-with-too-few-calls-takes-the-pooled-spread-of-the-fitted-kinds
  (let [fails (fn [job n] (concat [(walk job 0 false)] (map #(walk job (* 1000 %) false) (range 1 n))))
        rows (keep r/scan-line (concat (fails "a" 4) [(walk "a" 9000 true)]
                                       (fails "b" 3) [(walk "b" 9000 true)]
                                       (fails "c" 2) [(walk "c" 9000 false)]
                                       [(action "d/craft" :done 1000 "timeout") (action "d/craft" :done 2000 "crafted")]
                                       [(action "e/craft" :done 1000 "timeout") (action "e/craft" :done 2000 "timeout")]))
        {:keys [craft walk]} (r/report rows)]
    (is (= 2 (:calls craft)) "two calls, one won and one lost, have no spread to fit")
    (is (< (abs (- (+ (:win walk) (:fail walk)) (+ (:win craft) (:fail craft)))) 0.01) "the craft prior has the walk's spread")
    (is (= 0.5 (/ (:win craft) (+ (:win craft) (:fail craft)))) "and keeps its own mean")))

(deftest a-risky-wait-refusal-then-the-stay-allowed-is-a-won-risky-stay-retry
  (let [ev (fn [kind t] (pr-str {:generation-id "g" :time-ms t :source :job :kind kind :context {:job-id "j/x"} :data {}}))
        {:keys [risky-stay]} (r/report (keep r/scan-line [(ev :near.risky-wait 0) (ev :near.risky-wait 5000) (ev :near.risky-over 10000)]))]
    (is (= {:won 1 :retries 2 :calls 1 :try-s 5} (select-keys risky-stay [:won :retries :calls :try-s])))))

(deftest a-comb-look-that-took-none-then-some-is-a-won-comb-look-retry
  (let [ev (fn [kind t] (pr-str {:generation-id "g" :time-ms t :source :job :kind kind :context {:job-id "j/x"} :data {}}))
        {:keys [comb-look]} (r/report (keep r/scan-line [(ev :apiary.comb-wait 0) (ev :apiary.comb-wait 5000) (ev :apiary.comb-over 10000)]))]
    (is (= {:won 1 :retries 2 :calls 1 :try-s 5} (select-keys comb-look [:won :retries :calls :try-s])))))
