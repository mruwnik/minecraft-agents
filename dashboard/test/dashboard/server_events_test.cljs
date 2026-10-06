(ns dashboard.server-events-test
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            ["http" :as http]
            ["os" :as os]
            ["path" :as path]
            ["stream" :as stream]
            [dashboard.server.engine-state :as srv-engine-state]
            [dashboard.server.events :as srv-events]
            [dashboard.server.files :as srv-files]))

(defn run [done f]
  (-> (js/Promise.resolve)
      (.then f)
      (.catch (fn [e] (is (nil? e) (str "async server test failed: " e))))
      (.finally done)))

(defn answer! [res value]
  (.writeHead res 200 #js {"content-type" "application/edn; charset=utf-8" "cache-control" "no-store"})
  (.end res (pr-str value)))

(defn ev [n gen]
  (if (= n 2)
    {:seq 2 :generation-id gen :time-ms 2000 :source :job :kind :failed :context {:job-id "j1"}
     :data {:reason :blocked} :attention :required :request-id "r1"}
    {:seq n :generation-id gen :time-ms (* 1000 n) :source :job :kind :started
     :context {:job-id "j1"} :data {:name "jobs.test"} :attention :none}))

(defn mock-config
  "Mock engine script: stream, generation, the events it holds, and a one-shot gap reply."
  []
  (atom {:stream "s4" :gen "g7" :events [(ev 1 "g7") (ev 2 "g7")] :gap-next? false}))

(defn mock-engine!
  ([socket-path received] (mock-engine! socket-path received (mock-config)))
  ([socket-path received config]
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
                            (let [{:keys [stream gen events]} @config]
                              (answer! res {:body "Mock" :generation-id gen :state {:current "j1"}
                                            :outstanding {"r1" {:request-id "r1" :job-id "j1" :reason :blocked
                                                                :event {:source :job :kind :failed :attention :required :request-id "r1"}}}
                                            :cursor {:stream-id stream :seq (count events)} :position {:x 1 :y 64 :z 2}
                                            :offline false :settling false}))
                            "/events"
                            (let [{:keys [stream events gap-next?]} @config
                                  after (js/parseInt (or (.get (.-searchParams url) "after") "0") 10)
                                  limit (js/parseInt (or (.get (.-searchParams url) "limit") "1000") 10)
                                  latest (count events)
                                  base {:stream-id stream :oldest-seq 1 :latest-seq latest}]
                              (swap! received #(-> % (assoc :events-query (.-search url)) (update :events-queries (fnil conj []) (.-search url))))
                              (if gap-next?
                                (do (swap! config assoc :gap-next? false)
                                    (answer! res (assoc base :gap? true :events [] :cursor {:stream-id stream :seq latest})))
                                (let [page (vec (take limit (filter #(> (:seq %) after) events)))]
                                  (answer! res (assoc base :gap? false :events page
                                                          :cursor {:stream-id stream :seq (or (:seq (peek page)) after)})))))
                            "/attention/resolve"
                            (do (swap! received assoc :resolve-body (reader/read-string body))
                                (answer! res {:ok true :request-id "r1" :resolved true :already-resolved false}))
                            (answer! res {:error :not-found}))))))))]
    (js/Promise.
     (fn [resolve reject]
       (.once srv "error" reject)
       (.listen srv socket-path (fn [] (resolve srv))))))))

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
    (srv-events/resolve-attention! request (:response response) body)
    (.end request (pr-str {:request-id "r1" :reason :handled :extra "not for the engine"}))
    (-> (:promise response)
        (.then (fn [{:keys [status body]}]
                 (is (= 200 status))
                 (is (= {:request-id "r1" :reason :handled} (:resolve-body @received)))
                 (is (= {:ok true :request-id "r1" :resolved true :already-resolved false}
                        (reader/read-string body))))))))

(defn check-dashboard-event-routes! [body received]
  (let [events-res (response-recorder)
        query (js/URLSearchParams. "?limit=10")]
    (-> (srv-events/send-events! (:response events-res) body query)
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
          original-state-dir srv-files/state-dir
          body {:world "w" :name "Mock"}
          engine-dir (.join path root "worlds" "w" "agents" (:name body) "engine")
          socket-path (.join path engine-dir "events.sock")
          received (atom {})]
      (.mkdirSync fs engine-dir #js {:recursive true})
      (set! srv-files/state-dir root)
      (run done
           (fn []
             (-> (mock-engine! socket-path received)
                 (.then (fn [mock]
                          (-> (check-dashboard-event-routes! body received)
                              (.finally #(close-mock! mock)))))
                 (.finally (fn []
                             (set! srv-files/state-dir original-state-dir)
                             (.rmSync fs root #js {:recursive true :force true})))))))))

(deftest canonical-offline-keeps-persisted-attention
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
    (.writeFileSync fs (.join path engine-dir "engine.edn") (pr-str state))
    (try
      (with-redefs [srv-files/state-dir root
                    srv-engine-state/live-engines (atom {})
                    srv-engine-state/live-errors (atom {{:world "w" :name name} "socket unavailable"})]
        (let [body (srv-engine-state/engine-body {:name name :username name :world "w"} 1000)]
          (is (= inbox (:outstanding body)))
          (is (false? (:up body)))))
      (finally (.rmSync fs root #js {:recursive true :force true})))))

(deftest live-refresh-reads-only-events-after-the-cached-cursor
  (async done
    (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-refresh-test-"))
          original-state-dir srv-files/state-dir
          body {:world "w" :name "Mock"}
          engine-dir (.join path root "worlds" "w" "agents" (:name body) "engine")
          received (atom {})]
      (.mkdirSync fs engine-dir #js {:recursive true})
      (set! srv-files/state-dir root)
      (run done
           (fn []
             (-> (mock-engine! (.join path engine-dir "events.sock") received)
                 (.then (fn [mock]
                          (with-redefs [srv-engine-state/live-engines (atom {})]
                            (-> (srv-engine-state/refresh-live-engine! body)
                                (.then (fn [_] (srv-engine-state/refresh-live-engine! body)))
                                (.then (fn [_]
                                         (is (= ["?stream-id=s4&after=0&limit=1000"
                                                 "?stream-id=s4&after=2&limit=1000"]
                                                (:events-queries @received)))))
                                (.finally #(close-mock! mock))))))
                 (.finally (fn []
                             (set! srv-files/state-dir original-state-dir)
                             (.rmSync fs root #js {:recursive true :force true})))))))))

(deftest an-oversized-engine-event-response-is-refused
  (async done
    (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-events-cap-"))
          body {:world "w" :name "Mock"}
          engine-dir (.join path root "worlds" "w" "agents" (:name body) "engine")]
      (.mkdirSync fs engine-dir #js {:recursive true})
      (run done
           (fn []
             (-> (mock-engine! (.join path engine-dir "events.sock") (atom {}))
                 (.then (fn [mock]
                          (let [original-state-dir srv-files/state-dir
                                original-cap srv-engine-state/event-response-bytes]
                            (set! srv-files/state-dir root)
                            (set! srv-engine-state/event-response-bytes 10)
                            (-> (srv-engine-state/event-socket-request! body "GET" "/snapshot" nil)
                                (.then (fn [_] (is false "an over-cap response must be rejected"))
                                       (fn [e] (is (re-find #"exceeds" (ex-message e)))))
                                (.finally (fn []
                                            (set! srv-files/state-dir original-state-dir)
                                            (set! srv-engine-state/event-response-bytes original-cap)
                                            (close-mock! mock)))))))
                 (.finally #(.rmSync fs root #js {:recursive true :force true}))))))))

(defn engine-dir-of [root name] (.join path root "worlds" "w" "agents" name "engine"))

(defn with-poller
  "Run (f bodies configs received) with one mock engine per name under a temp state dir and an empty
  live cache; everything is closed and restored afterwards."
  [done names f]
  (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-poller-test-"))
        original-state-dir srv-files/state-dir
        bodies (mapv #(hash-map :world "w" :name %) names)
        configs (mapv (fn [_] (mock-config)) names)
        receiveds (mapv (fn [_] (atom {})) names)
        mocks (atom [])]
    (doseq [n names] (.mkdirSync fs (engine-dir-of root n) #js {:recursive true}))
    (set! srv-files/state-dir root)
    (run done
         (fn []
           (-> (js/Promise.all
                (clj->js (map (fn [n c r] (mock-engine! (.join path (engine-dir-of root n) "events.sock") r c))
                              names configs receiveds)))
               (.then (fn [started]
                        (reset! mocks (vec started))
                        (reset! srv-engine-state/live-engines {})
                        (reset! srv-engine-state/live-errors {})
                        (f bodies configs receiveds)))
               (.finally (fn []
                           (set! srv-files/state-dir original-state-dir)
                           (reset! srv-engine-state/live-engines {})
                           (reset! srv-engine-state/live-errors {})
                           (-> (js/Promise.all (clj->js (map close-mock! @mocks)))
                               (.finally #(.rmSync fs root #js {:recursive true :force true}))))))))))

(defn cached [body] (get @srv-engine-state/live-engines (srv-engine-state/body-key body)))

(deftest poller-keeps-the-cache-on-an-empty-second-page
  (async done
    (with-poller done ["Mock"]
      (fn [[body] _ [received]]
        (-> (srv-engine-state/refresh-live-engine! body)
            (.then (fn [_] (srv-engine-state/refresh-live-engine! body)))
            (.then (fn [_]
                     (is (= 2 (count (:events (cached body)))))
                     (is (= 2 (get-in (cached body) [:cursor :seq])))
                     (is (false? (:reset? (cached body))))
                     (is (= ["?stream-id=s4&after=0&limit=1000" "?stream-id=s4&after=2&limit=1000"]
                            (:events-queries @received))))))))))

(deftest poller-appends-only-the-new-events
  (async done
    (with-poller done ["Mock"]
      (fn [[body] [config] [received]]
        (-> (srv-engine-state/refresh-live-engine! body)
            (.then (fn [_]
                     (swap! config update :events conj (ev 3 "g7"))
                     (srv-engine-state/refresh-live-engine! body)))
            (.then (fn [_]
                     (is (= [1 2 3] (mapv :seq (:events (cached body)))))
                     (is (= 3 (get-in (cached body) [:cursor :seq])))
                     (is (= "?stream-id=s4&after=2&limit=1000" (peek (:events-queries @received)))))))))))

(deftest poller-rereads-after-a-generation-change
  (async done
    (with-poller done ["Mock"]
      (fn [[body] [config] [received]]
        (-> (srv-engine-state/refresh-live-engine! body)
            (.then (fn [_]
                     (swap! config assoc :gen "g8" :events [(ev 1 "g8") (ev 2 "g8") (ev 3 "g8")])
                     (srv-engine-state/refresh-live-engine! body)))
            (.then (fn [_]
                     (is (true? (:reset? (cached body))))
                     (is (= "g8" (:generation-id (cached body))))
                     (is (= ["g8" "g8" "g8"] (mapv :generation-id (:events (cached body)))))
                     (is (= "?stream-id=s4&after=0&limit=1000" (peek (:events-queries @received)))))))))))

(deftest poller-takes-a-fresh-tail-on-a-gap-reply
  (async done
    (with-poller done ["Mock"]
      (fn [[body] [config] [received]]
        (-> (srv-engine-state/refresh-live-engine! body)
            (.then (fn [_]
                     (swap! config assoc :gap-next? true :events (mapv #(ev % "g7") (range 1 6)))
                     (srv-engine-state/refresh-live-engine! body)))
            (.then (fn [_]
                     (is (true? (:reset? (cached body))))
                     (is (= [1 2 3 4 5] (mapv :seq (:events (cached body)))))
                     (is (= 5 (get-in (cached body) [:cursor :seq])))
                     (is (= ["?stream-id=s4&after=0&limit=1000" "?stream-id=s4&after=2&limit=1000"
                             "?stream-id=s4&after=0&limit=1000"]
                            (:events-queries @received))))))))))

(deftest poller-marks-a-body-without-a-socket-down-and-asks-nobody
  (async done
    (with-poller done ["Mock"]
      (fn [[body] _ [received]]
        (let [offline {:world "w" :name "Offline"}
              dir (engine-dir-of srv-files/state-dir "Offline")]
          (.mkdirSync fs dir #js {:recursive true})
          (.writeFileSync fs (.join path dir "events.edn") "")
          (-> (srv-engine-state/refresh-live-engines-in! [offline])
              (.then (fn [_]
                       (is (= srv-engine-state/no-socket-error (get @srv-engine-state/live-errors offline)))
                       (is (nil? (cached offline)))
                       (is (nil? (:events-queries @received)))))))))))

(deftest poller-keeps-two-bodies-apart
  (async done
    (with-poller done ["Alpha" "Beta"]
      (fn [[alpha beta] [_ config-b] _]
        (swap! config-b assoc :stream "s9" :events [(ev 1 "g7") (ev 2 "g7") (ev 3 "g7")])
        (-> (srv-engine-state/refresh-live-engines-in! [alpha beta])
            (.then (fn [_]
                     (is (= {:stream-id "s4" :seq 2} (:cursor (cached alpha))))
                     (is (= {:stream-id "s9" :seq 3} (:cursor (cached beta))))
                     (is (= 2 (count (:events (cached alpha)))))
                     (is (= 3 (count (:events (cached beta))))))))))))
