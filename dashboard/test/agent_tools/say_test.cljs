(ns agent-tools.say-test
  (:require [cljs.test :refer [deftest is]]
            [agent-tools.say :as say]))

(deftest say-validates-one-bounded-public-line-and-whisper-recipient-locally
  (is (re-find #"one shell-quoted" (:error (say/request-for ["Bob" "--world" "w" "hello" "there" "--to" "Steve_1"]))))
  (let [valid (say/request-for ["Bob" "--world" "w" "--to" "Steve_1" "hello there"])]
    (is (= "hello there" (:message valid)))
    (is (= "Steve_1" (:to valid))))
  (doseq [argv [["Bob" "--world" "w" "/op Steve"]
                ["Bob" "--world" "w" "--to" "bad name" "hello"]
                ["Bob" "--world" "w" (apply str (repeat 257 "x"))]]]
    (is (string? (:error (say/request-for argv))) (pr-str argv))))

(defn coded [code] (doto (js/Error. "x") (aset "code" code)))

(deftest a-permission-denial-before-sending-is-distinct-from-unknown-delivery
  (doseq [code ["EPERM" "EACCES"]]
    (let [denied (say/failure-for (coded code))]
      (is (= :socket-access-denied (:reason denied)))
      (is (not (contains? denied :confirmation)))
      (is (re-find #"message was not sent" (:message denied))))))

(deftest every-other-failure-leaves-delivery-unknown
  (doseq [[code reason] [["ETIMEDOUT" :transport-error] ["ECONNRESET" :transport-error]
                         ["ENOENT" :no-running-body] ["ECONNREFUSED" :no-running-body]]]
    (let [uncertain (say/failure-for (coded code))]
      (is (= reason (:reason uncertain)) code)
      (is (= :unknown (:confirmation uncertain))))))
