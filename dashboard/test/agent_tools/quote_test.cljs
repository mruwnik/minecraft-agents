(ns agent-tools.quote-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.quote :as quote]))

(deftest the-request-names-the-job-and-carries-the-args
  (let [r (quote/request-for ["Probe" "--world" "test" "jobs.movement.go-to" "{:pos {:x 1 :y 64 :z 2}}"])]
    (is (= "/quote?name=jobs.movement.go-to&args=%7B%3Apos%20%7B%3Ax%201%20%3Ay%2064%20%3Az%202%7D%7D" (:path r)))
    (is (= "/quote?name=jobs.primitive.dig" (:path (quote/request-for ["Probe" "--world" "test" "jobs.primitive.dig"]))))))

(deftest bad-requests-are-refused
  (doseq [argv [["Probe" "jobs.movement.go-to"]
                ["Probe" "--world" "test"]
                ["Probe" "--world" "test" "go-to"]
                ["Probe" "--world" "test" "jobs.a" "{}" "extra"]]]
    (is (string? (:error (quote/request-for argv))) (pr-str argv))))

(deftest main-prints-the-engines-answer-and-its-exit-code
  (async done
    (let [out (atom "")
          [request-fn seen] (fake/request-fn (fn [_] {:status 200 :content-type "application/edn" :text "{:ok true :quote {:uses {:time [1 ##Inf ##Inf]}}}"}))]
      (-> (quote/main! ["Probe" "--world" "test" "jobs.movement.go-to"] {:output #(swap! out str %) :request-fn request-fn})
          (.then (fn [code]
                   (is (= 0 code))
                   (is (= "{:ok true :quote {:uses {:time [1 ##Inf ##Inf]}}}\n" @out))
                   (is (= "/quote?name=jobs.movement.go-to" (:path (first @seen))))
                   (done)))))))
