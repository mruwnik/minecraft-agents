(ns agent-tools.http-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.http :as http]
            ["node:events" :refer [EventEmitter]]))

;; A fake socket: the request function is injected, so no server is needed.
(defn fake-response [& {:keys [headers] :or {headers #js {"content-type" "application/edn"}}}]
  (let [res (EventEmitter.)]
    (set! (.-statusCode res) 200)
    (set! (.-headers res) headers)
    (set! (.-setEncoding res) (fn [_]))
    (set! (.-destroy res) (fn [_]))
    res))

(defn fake-request-fn
  "Returns [request-fn seen]; on end the request runs (on-end req callback). seen holds the request and options."
  [on-end]
  (let [seen (atom {})]
    [(fn [options callback]
       (let [req (EventEmitter.)]
         (set! (.-end req) (fn [payload]
                             (swap! seen assoc :payload payload)
                             (on-end req callback)))
         (set! (.-destroy req) (fn [_] (swap! seen assoc :destroyed true)))
         (swap! seen assoc :options options)
         req))
     seen]))

(defn outcome [promise]
  (-> promise
      (.then (fn [reply] {:reply reply}))
      (.catch (fn [error] {:code (.-code error) :message (.-message error)}))))

(defn settle! [done checks promise seen]
  (-> (outcome promise)
      (.then (fn [result] (checks result @seen) (done)))))

(deftest edn-response-checks-the-content-type
  (doseq [[content-type expected] [["application/edn" true]
                                   ["application/edn; charset=utf-8" true]
                                   ["Application/EDN;x" true]
                                   ["application/json" false]
                                   ["application/edn-x" false]
                                   ["text/plain; application/edn" false]
                                   ["" false]
                                   [nil false]]]
    (is (= expected (http/edn-response? content-type)) (pr-str content-type))))

(deftest unsupported-route-is-only-the-bare-not-found-edn-reply
  (let [edn "application/edn"]
    (doseq [[response expected] [[{:status 404 :content-type edn :text "{:ok false :reason :not-found}"} true]
                                 [{:status 404 :content-type edn :text "  {:ok false, :reason :not-found}\n"} true]
                                 [{:status 404 :content-type "application/json" :text "{:ok false :reason :not-found}"} false]
                                 [{:status 500 :content-type edn :text "{:ok false :reason :not-found}"} false]
                                 [{:status 404 :content-type edn :text "{:ok false :reason :other}"} false]
                                 [{:status 404 :content-type edn :text "{:ok false :reason :not-found :x 1}"} false]
                                 [{:status 404 :content-type nil :text "{:ok false :reason :not-found}"} false]]]
      (is (= expected (http/unsupported-route? response)) (pr-str response)))))

(deftest a-reply-carries-status-content-type-and-text-and-sends-the-body
  (async done
    (let [[request-fn seen] (fake-request-fn
                             (fn [req callback]
                               (let [res (fake-response)]
                                 (callback res)
                                 (.emit res "data" "{:ok ")
                                 (.emit res "data" "true}")
                                 (.emit res "end"))))]
      (settle! done
               (fn [{:keys [reply]} seen]
                 (is (= {:status 200 :content-type "application/edn" :text "{:ok true}"} reply))
                 (is (= "/chat" (.-path (:options seen))))
                 (is (= "POST" (.-method (:options seen))))
                 (is (= "/tmp/x.sock" (.-socketPath (:options seen))))
                 (is (= "application/edn" (aget (.-headers (:options seen)) "content-type")))
                 (is (= "hello" (:payload seen)))
                 (is (= 5 (aget (.-headers (:options seen)) "content-length"))))
               (http/request {:socket-path "/tmp/x.sock" :method "POST" :path "/chat" :body "hello"
                              :headers {"content-type" "application/edn"} :request-fn request-fn})
               seen))))

(deftest a-reply-without-content-type-has-an-empty-one
  (async done
    (let [[request-fn seen] (fake-request-fn
                             (fn [req callback]
                               (let [res (fake-response :headers #js {})]
                                 (callback res)
                                 (.emit res "end"))))]
      (settle! done
               (fn [{:keys [reply]} _] (is (= {:status 200 :content-type "" :text ""} reply)))
               (http/request {:socket-path "/s" :path "/x" :request-fn request-fn})
               seen))))

(deftest a-request-with-no-answer-times-out-and-is-destroyed
  (async done
    (let [[request-fn seen] (fake-request-fn (fn [_ _]))]
      (settle! done
               (fn [{:keys [code]} seen]
                 (is (= "ETIMEDOUT" code))
                 (is (true? (:destroyed seen))))
               (http/request {:socket-path "/s" :path "/x" :timeout-ms 5 :request-fn request-fn})
               seen))))

(deftest an-oversized-reply-is-refused-and-the-request-destroyed
  (async done
    (let [[request-fn seen] (fake-request-fn
                             (fn [req callback]
                               (let [res (fake-response)]
                                 (callback res)
                                 (.emit res "data" "12345"))))]
      (settle! done
               (fn [{:keys [code message]} seen]
                 (is (= "ERESPONSETOOLARGE" code))
                 (is (= "world response exceeded 4 bytes" message))
                 (is (true? (:destroyed seen))))
               (http/request {:socket-path "/s" :path "/x" :max-bytes 4 :label "world" :request-fn request-fn})
               seen))))

(deftest a-reply-within-the-cap-is-accepted
  (async done
    (let [[request-fn seen] (fake-request-fn
                             (fn [req callback]
                               (let [res (fake-response)]
                                 (callback res)
                                 (.emit res "data" "1234")
                                 (.emit res "end"))))]
      (settle! done
               (fn [{:keys [reply]} _] (is (= "1234" (:text reply))))
               (http/request {:socket-path "/s" :path "/x" :max-bytes 4 :request-fn request-fn})
               seen))))

(deftest an-aborted-reply-is-a-connection-reset
  (async done
    (let [[request-fn seen] (fake-request-fn
                             (fn [req callback]
                               (let [res (fake-response)]
                                 (callback res)
                                 (.emit res "aborted"))))]
      (settle! done
               (fn [{:keys [code]} _] (is (= "ECONNRESET" code)))
               (http/request {:socket-path "/s" :path "/x" :request-fn request-fn})
               seen))))

(deftest a-socket-error-rejects-with-its-own-code
  (async done
    (let [[request-fn seen] (fake-request-fn
                             (fn [req _] (.emit req "error" (doto (js/Error. "refused") (aset "code" "ECONNREFUSED")))))]
      (settle! done
               (fn [{:keys [code]} _] (is (= "ECONNREFUSED" code)))
               (http/request {:socket-path "/s" :path "/x" :request-fn request-fn})
               seen))))

(deftest an-abort-signal-cancels-the-request
  (async done
    (let [controller (js/AbortController.)
          [request-fn seen] (fake-request-fn (fn [_ _] (js/setTimeout #(.abort controller) 1)))]
      (settle! done
               (fn [{:keys [code]} seen]
                 (is (= "ABORT_ERR" code))
                 (is (true? (:destroyed seen))))
               (http/request {:socket-path "/s" :path "/x" :signal (.-signal controller) :request-fn request-fn})
               seen))))

(deftest an-already-aborted-signal-sends-nothing
  (async done
    (let [controller (js/AbortController.)
          [request-fn seen] (fake-request-fn (fn [_ _]))]
      (.abort controller)
      (settle! done
               (fn [{:keys [code]} seen]
                 (is (= "ABORT_ERR" code))
                 (is (not (contains? seen :payload))))
               (http/request {:socket-path "/s" :path "/x" :signal (.-signal controller) :request-fn request-fn})
               seen))))
