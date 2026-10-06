(ns dashboard.server-http-test
  (:require [cljs.test :refer [deftest is are async]]
            ["fs" :as fs]
            ["node:events" :refer [EventEmitter]]
            ["os" :as os]
            ["path" :as path]
            [dashboard.server :as server]
            [dashboard.server.engine-state :as srv-engine-state]
            [dashboard.server.files :as srv-files]
            [dashboard.server.pictures :as srv-pictures]
            [dashboard.server.posts :as srv-posts]
            [dashboard.server.responses :as srv-responses]))

(defn fake-response []
  (let [seen (atom {})]
    #js {:seen seen
         :writeHead (fn [code _] (swap! seen assoc :code code))
         :end (fn [payload] (swap! seen assoc :payload (str payload)))}))

(deftest send-file-answers-404-for-a-directory
  (let [dir (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-sendfile-"))
        res (fake-response)]
    (srv-responses/send-file! res dir)
    (.rmSync fs dir #js {:recursive true :force true})
    (is (= 404 (:code @(.-seen res))))
    (is (not (re-find (re-pattern dir) (:payload @(.-seen res)))))))

(deftest rcon-failure-reply-carries-no-detail
  (let [res (fake-response)]
    (srv-posts/send-rcon-failure! res (js/Error "connect ECONNREFUSED /home/secret/path"))
    (is (= 502 (:code @(.-seen res))))
    (is (= "{\"error\":\"RCON failed\"}" (:payload @(.-seen res))))))

(deftest read-body-gives-up-once-on-request-error
  (are [event] (let [req (EventEmitter.)
                     calls (atom [])]
                 (srv-responses/read-body req 100 #(swap! calls conj %))
                 (.emit req "data" (js/Buffer.from "ab"))
                 (.emit req event)
                 (.emit req "end")
                 (= [nil] @calls))
    "error"
    "aborted"))

(defn body-request [headers]
  (let [req (EventEmitter.)
        destroyed (atom 0)]
    (set! (.-headers req) (clj->js headers))
    (set! (.-socket req) #js {:destroySoon #(swap! destroyed inc)})
    [req destroyed]))

(deftest read-body-answers-nil-once-as-soon-as-the-body-passes-the-limit
  (let [[req destroyed] (body-request {})
        calls (atom [])]
    (srv-responses/read-body req 4 #(swap! calls conj %))
    (.emit req "data" (js/Buffer.from "abc"))
    (is (= [] @calls))
    (.emit req "data" (js/Buffer.from "de"))
    (is (= [nil] @calls))
    (.emit req "data" (js/Buffer.from "more"))
    (.emit req "end")
    (is (= [nil] @calls))
    (is (= 1 @destroyed) "the upload is cut off")))

(deftest read-body-refuses-a-declared-length-over-the-limit-without-reading
  (let [[req destroyed] (body-request {"content-length" "5000"})
        calls (atom [])]
    (srv-responses/read-body req 4096 #(swap! calls conj %))
    (is (= [nil] @calls))
    (is (= 1 @destroyed))))

(deftest read-body-returns-the-text-within-the-limit
  (let [[req destroyed] (body-request {"content-length" "4"})
        calls (atom [])]
    (srv-responses/read-body req 4 #(swap! calls conj %))
    (.emit req "data" (js/Buffer.from "ab"))
    (.emit req "data" (js/Buffer.from "cd"))
    (.emit req "end")
    (is (= ["abcd"] @calls))
    (is (= 0 @destroyed))))

(defn request-to [url host]
  #js {:url url :method "GET" :headers #js {:host host}})

(defn fake-res []
  (let [res (fake-response)]
    (aset res "headersSent" false)
    res))

(deftest a-malformed-request-target-is-answered-400-not-thrown
  (let [res (fake-res)]
    (server/handler (request-to "http://[" (str "127.0.0.1:" srv-files/port)) res)
    (is (= 400 (:code @(.-seen res))))))

(deftest every-route-refuses-a-foreign-host
  (are [host] (let [res (fake-res)]
                (server/handler (request-to "/api/worlds" host) res)
                (= 403 (:code @(.-seen res))))
    "evil.example:3701"
    "127.0.0.1.evil.example:3701"
    nil))

(deftest loopback-hosts-reach-the-routes
  (are [host] (let [res (fake-res)]
                (server/handler (request-to "/api/build-id" host) res)
                (= 200 (:code @(.-seen res))))
    (str "127.0.0.1:" srv-files/port)
    (str "localhost:" srv-files/port)))

(deftest thumbs-stats-failure-is-answered-500
  (let [res (fake-res)]
    (async done
      (let [original srv-pictures/thumbnailer]
        (set! srv-pictures/thumbnailer (delay (js/Promise.resolve {:stats #(throw (js/Error. "stats broke"))})))
        (-> (srv-pictures/send-thumbs-stats! res)
            (.then (fn [_]
                     (set! srv-pictures/thumbnailer original)
                     (is (= 500 (:code @(.-seen res))))
                     (done))))))))

(deftest cache-pruning-keeps-only-live-bodies
  (is (= {{:world "w" :name "A"} 1}
         (srv-engine-state/prune-cache {{:world "w" :name "A"} 1 {:world "w" :name "Gone"} 2}
                             [{:world "w" :name "A" :text "x"}]))))
