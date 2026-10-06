(ns dashboard.server-http-test
  (:require [cljs.test :refer [deftest is are]]
            ["fs" :as fs]
            ["node:events" :refer [EventEmitter]]
            ["os" :as os]
            ["path" :as path]
            [dashboard.server :as server]))

(defn fake-response []
  (let [seen (atom {})]
    #js {:seen seen
         :writeHead (fn [code _] (swap! seen assoc :code code))
         :end (fn [payload] (swap! seen assoc :payload (str payload)))}))

(deftest send-file-answers-404-for-a-directory
  (let [dir (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-sendfile-"))
        res (fake-response)]
    (server/send-file! res dir)
    (.rmSync fs dir #js {:recursive true :force true})
    (is (= 404 (:code @(.-seen res))))
    (is (not (re-find (re-pattern dir) (:payload @(.-seen res)))))))

(deftest rcon-failure-reply-carries-no-detail
  (let [res (fake-response)]
    (server/send-rcon-failure! res (js/Error "connect ECONNREFUSED /home/secret/path"))
    (is (= 502 (:code @(.-seen res))))
    (is (= "{\"error\":\"RCON failed\"}" (:payload @(.-seen res))))))

(deftest read-body-gives-up-once-on-request-error
  (are [event] (let [req (EventEmitter.)
                     calls (atom [])]
                 (server/read-body req 100 #(swap! calls conj %))
                 (.emit req "data" (js/Buffer.from "ab"))
                 (.emit req event)
                 (.emit req "end")
                 (= [nil] @calls))
    "error"
    "aborted"))
