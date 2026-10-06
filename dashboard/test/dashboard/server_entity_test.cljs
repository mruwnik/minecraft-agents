(ns dashboard.server-entity-test
  (:require [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            ["http" :as http]
            ["os" :as os]
            ["path" :as path]
            [dashboard.server.entities :as srv-entities]))

(deftest entity-socket-enforces-total-time-byte-cap-and-capability-status
  (async done
    (let [dir (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-entities-"))
          socket (.join path dir "control.sock")
          mode (atom :ok)
          original-socket srv-entities/entity-socket
          original-time srv-entities/entity-request-ms
          original-cap srv-entities/entity-response-bytes
          body {:world "a" :name "Mock"}
          mock (http/createServer
                (fn [req res]
                  (is (= "/entities" (.-url req)))
                  (case @mode
                    :ok (.end res (pr-str {:ok true :world "a" :body "Mock" :entities []}))
                    :missing (do (.writeHead res 404) (.end res "missing"))
                    :large (.end res (apply str (repeat 2048 "x")))
                    :stall (.writeHead res 200))))]
      (set! srv-entities/entity-socket (constantly socket))
      (set! srv-entities/entity-request-ms 100)
      (set! srv-entities/entity-response-bytes 1024)
      (-> (js/Promise. (fn [resolve reject]
                        (.once mock "error" reject)
                        (.listen mock socket resolve)))
          (.then (fn [_] (srv-entities/entity-request! body)))
          (.then (fn [payload]
                   (is (= "Mock" (:body payload)))
                   (reset! mode :missing)
                   (-> (srv-entities/entity-request! body)
                       (.then (fn [_] (is false "HTTP 404 must reject")))
                       (.catch (fn [e] (is (= 404 (:status (ex-data e)))))))))
          (.then (fn [_]
                   (reset! mode :large)
                   (-> (srv-entities/entity-request! body)
                       (.then (fn [_] (is false "oversized response must reject")))
                       (.catch (fn [e] (is (boolean (re-find #"exceeds" (ex-message e)))))))))
          (.then (fn [_]
                   (reset! mode :stall)
                   (-> (srv-entities/entity-request! body)
                       (.then (fn [_] (is false "stalled response must reject")))
                       (.catch (fn [e] (is (boolean (re-find #"timed out" (ex-message e)))))))))
          (.catch (fn [e] (is (nil? e) (str e))))
          (.finally (fn []
                      (set! srv-entities/entity-socket original-socket)
                      (set! srv-entities/entity-request-ms original-time)
                      (set! srv-entities/entity-response-bytes original-cap)
                      (.close mock)
                      (.rmSync fs dir #js {:recursive true :force true})
                      (done)))))))
