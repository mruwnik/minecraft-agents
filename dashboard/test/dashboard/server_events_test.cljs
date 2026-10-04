(ns dashboard.server-events-test
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            ["http" :as http]
            ["os" :as os]
            ["path" :as path]
            ["stream" :as stream]
            [dashboard.server :as server]))

(defn run [done f]
  (-> (js/Promise.resolve)
      (.then f)
      (.catch (fn [e] (is (nil? e) (str "async server test failed: " e))))
      (.finally done)))

(defn answer! [res value]
  (.writeHead res 200 #js {"content-type" "application/edn; charset=utf-8" "cache-control" "no-store"})
  (.end res (pr-str value)))

(defn mock-engine! [socket-path received]
  (let [srv (http/createServer
             (fn [req res]
               (let [chunks (atom [])]
                 (.on req "data" #(swap! chunks conj %))
                 (.on req "end"
                      (fn []
                        (let [url (js/URL. (.-url req) "http://engine")
                              body (.toString (js/Buffer.concat (to-array @chunks)) "utf8")]
                          (case (.-pathname url)
                            "/snapshot"
                            (answer! res {:body "Mock" :generation-id "g7" :state {:current "j1"}
                                          :outstanding {"r1" {:request-id "r1" :job-id "j1" :reason :blocked
                                                              :event {:source :job :kind :failed :attention :required :request-id "r1"}}}
                                          :cursor {:stream-id "s4" :seq 2} :position {:x 1 :y 64 :z 2}
                                          :offline false :settling false})
                            "/events"
                            (do (swap! received assoc :events-query (.-search url))
                                (answer! res {:stream-id "s4" :oldest-seq 1 :latest-seq 2 :gap? false
                                              :events [{:seq 1 :generation-id "g7" :time-ms 1000 :source :job :kind :started
                                                        :context {:job-id "j1"} :data {:name "jobs.test"} :attention :none}
                                                       {:seq 2 :generation-id "g7" :time-ms 2000 :source :job :kind :failed
                                                        :context {:job-id "j1"} :data {:reason :blocked} :attention :required :request-id "r1"}]
                                              :cursor {:stream-id "s4" :seq 2}}))
                            "/attention/resolve"
                            (do (swap! received assoc :resolve-body (reader/read-string body))
                                (answer! res {:ok true :request-id "r1" :resolved true :already-resolved false}))
                            (answer! res {:error :not-found}))))))))]
    (js/Promise.
     (fn [resolve reject]
       (.once srv "error" reject)
       (.listen srv socket-path (fn [] (resolve srv)))))))

(defn close-mock! [mock]
  (js/Promise.
   (fn [resolve reject]
     (.close mock (fn [error] (if error (reject error) (resolve true)))))))

(defn response-recorder []
  (let [result (atom nil)
        headers (atom nil)
        response #js {}
        promise (js/Promise. (fn [resolve] (aset response "end" (fn [body] (reset! result {:status @headers :body body}) (resolve @result)))))]
    (aset response "writeHead" (fn [status _] (reset! headers status)))
    {:response response :promise promise :result result}))

(defn resolve-dashboard-request! [body received]
  (let [request (stream/PassThrough.)
        response (response-recorder)]
    (set! (.-method request) "POST")
    (set! (.-headers request) #js {:host "127.0.0.1:3701" :content-type "application/edn"})
    (server/resolve-attention! request (:response response) body)
    (.end request (pr-str {:request-id "r1" :reason :handled}))
    (-> (:promise response)
        (.then (fn [{:keys [status body]}]
                 (is (= 200 status))
                 (is (= {:request-id "r1" :reason :handled} (:resolve-body @received)))
                 (is (= {:ok true :request-id "r1" :resolved true :already-resolved false}
                        (reader/read-string body))))))))

(defn check-dashboard-event-routes! [body received]
  (let [events-res (response-recorder)
        query (js/URLSearchParams. "?limit=10")]
    (-> (server/send-events! (:response events-res) body query)
        (.then (fn [_]
                 (let [{:keys [status body]} @(:result events-res)
                       response (reader/read-string body)]
                   (is (= 200 status))
                   (is (= "s4" (:stream-id response)))
                   (is (= 2 (get-in response [:cursor :seq])))
                   (is (= :required (get-in response [:events 1 :attention])))
                   (is (= "r1" (get-in response [:outstanding "r1" :request-id])))
                   (is (= "?stream-id=s4&after=0&limit=10" (:events-query @received))))))
        (.then (fn [_] (resolve-dashboard-request! body received))))))

(deftest canonical-dashboard-events-proxy-over-unix-socket
  (async done
    (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-events-test-"))
          original-state-dir server/state-dir
          body {:world "w" :name "Mock"}
          engine-dir (.join path root "worlds" "w" "agents" (:name body) "engine")
          socket-path (.join path engine-dir "events.sock")
          received (atom {})]
      (.mkdirSync fs engine-dir #js {:recursive true})
      (set! server/state-dir root)
      (run done
           (fn []
             (-> (mock-engine! socket-path received)
                 (.then (fn [mock]
                          (-> (check-dashboard-event-routes! body received)
                              (.finally #(close-mock! mock)))))
                 (.finally (fn []
                             (set! server/state-dir original-state-dir)
                             (.rmSync fs root #js {:recursive true :force true})))))))))

(deftest canonical-offline-keeps-persisted-attention-without-reading-jsonl
  (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-offline-test-"))
        name "Offline"
        engine-dir (.join path root "worlds" "w" "agents" name "engine")
        inbox {"r-durable" {:request-id "r-durable" :job-id "job-4" :reason :awaiting-signal
                            :event {:source :job :kind :failed :attention :required :request-id "r-durable"}
                            :updated-at 42}}
        state {:generation-id "g3" :list ["job-4"]
               :instances {"job-4" {:id "job-4" :spec {:op :leaf :job 'jobs.example/do-work}}}
               :current "job-4" :failed {} :attention inbox}]
    (.mkdirSync fs engine-dir #js {:recursive true})
    (.writeFileSync fs (.join path engine-dir "events.edn") "")
    (.writeFileSync fs (.join path engine-dir "events.jsonl") "{\"source\":\"job\",\"kind\":\"failed\",\"level\":\"error\",\"text\":\"stale legacy event\"}\n")
    (.writeFileSync fs (.join path engine-dir "engine.edn") (pr-str state))
    (try
      (with-redefs [server/state-dir root
                    server/live-engines (atom {})
                    server/live-errors (atom {{:world "w" :name name} "socket unavailable"})]
        (let [body (server/engine-body {:name name :username name :world "w"} 1000)]
          (is (= inbox (:outstanding body)))
          (is (false? (:up body)))
          (is (not= "stale legacy event" (get-in body [:engine :recent 0 :text])))))
      (finally (.rmSync fs root #js {:recursive true :force true})))))
