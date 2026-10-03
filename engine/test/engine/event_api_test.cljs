(ns engine.event-api-test
  (:require [cljs.test :refer [async deftest is testing]]
            [cljs.reader :as reader]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.test-util :as tu]
            ["http" :as http]
            ["path" :as path]))

(defn request [socket-path method request-path headers body]
  (js/Promise.
   (fn [resolve reject]
     (let [req (.request http #js {:socketPath socket-path
                                   :path request-path
                                   :method method
                                   :headers (clj->js headers)}
                         (fn [res]
                           (let [chunks (atom [])]
                             (.on res "data" #(swap! chunks conj %))
                             (.on res "end"
                                  #(resolve {:status (.-statusCode res)
                                             :content-type (aget (.-headers res) "content-type")
                                             :text (.toString (js/Buffer.concat (clj->js @chunks)) "utf8")})))))]
       (.on req "error" reject)
       (when body (.write req body))
       (.end req)))))

(defn edn-response [response]
  (reader/read-string (:text response)))

(deftest unix-api-serves-edn-snapshot-events-and-resolution
  (async done
    (let [dir (tu/tmp-dir)
          socket-path (path/join dir "events.sock")
          stream (events/make {:generation-id "gen-api"})
          primitives #js {:self (fn [] #js {:username "TestBody" :pos #js {:x 1 :y 2 :z 3}})
                          :isOffline (fn [] false)
                          :isSettling (fn [] true)}
          eng {:events stream
               :state (atom {:generation-id "gen-api"
                             :attention {"req-1" {:request-id "req-1" :job-id "j1"}}})
               :primitives primitives
               :now (constantly 123)}
          server (event-api/create socket-path eng)
          sid (:stream-id (events/cursor stream))]
      (events/emit! stream {:source :job :kind :blocked :attention :notice
                            :context {:job-id "j1"} :data {:reason :no-route}})
      (tu/run-async
       done
       (fn []
         (let [work (-> ((:listen server))
             (.then (fn [_]
                      (request socket-path "GET" "/snapshot" {} nil)))
             (.then (fn [response]
                      (let [snapshot (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (.startsWith (:content-type response) "application/edn"))
                        (is (= "TestBody" (:body snapshot)))
                        (is (= "gen-api" (:generation-id snapshot)))
                        (is (= {:stream-id sid :seq 1} (:cursor snapshot)))
                        (is (= false (:offline snapshot)))
                        (is (= true (:settling snapshot)))
                        (is (contains? (:outstanding snapshot) "req-1"))
                        (request socket-path "GET"
                                 (str "/events?stream-id=" (js/encodeURIComponent sid)
                                      "&after=0&limit=10") {} nil))))
             (.then (fn [response]
                      (let [page (edn-response response)
                            [event] (:events page)]
                        (is (= 200 (:status response)))
                        (is (.startsWith (:content-type response) "application/edn"))
                        (is (false? (:gap? page)))
                        (is (= 1 (:seq event)))
                        (is (= :notice (:attention event)))
                        (is (= :blocked (:kind event)))
                        (request socket-path "POST" "/attention/resolve"
                                 {"Content-Type" "application/json"}
                                 "{\"request-id\":\"req-1\"}"))))
             (.then (fn [response]
                      (is (= 415 (:status response)))
                      (is (= :content-type-must-be-application-edn
                             (:reason (edn-response response))))
                      (request socket-path "GET" "/events?after=bad" {} nil)))
             (.then (fn [response]
                      (is (= 400 (:status response)))
                      (request socket-path "POST" "/attention/resolve"
                               {"Content-Type" "application/edn"}
                               "{:request-id \"req-1\" :reason :job-retried}")))
             (.then (fn [response]
                      (is (= 400 (:status response)))
                      (request socket-path "POST" "/attention/resolve"
                               {"Content-Type" "application/edn"}
                               "{:request-id \"req-1\" :reason :handled}")))
             (.then (fn [response]
                      (let [body (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (true? (:resolved body)))
                        (is (not (contains? (:attention @(:state eng)) "req-1")))
                        (let [tail (events/read-after stream
                                                      {:stream-id sid :after 1 :limit 10})
                              event (first (:events tail))]
                          (is (= :resolved (:kind event)))
                          (is (= "req-1" (:request-id event)))
                          (is (= :handled (get-in event [:data :reason]))))))))]
           (.finally work (fn [] ((:close server))))))))))
