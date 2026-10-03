(ns engine.event-api-test
  (:require [cljs.test :refer [async deftest is testing]]
            [cljs.reader :as reader]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
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

(deftest resumed-job-is-not-reported-as-merely-queued
  (is (= :resuming (event-api/instance-status {:resume "j1" :list ["j1"]} "j1")))
  (is (= :queued (event-api/instance-status {:list ["j1"]} "j1"))))

(deftest unix-api-serves-edn-snapshot-events-and-resolution
  (async done
    (let [dir (tu/tmp-dir)
          socket-path (path/join dir "events.sock")
          stream (events/make {:generation-id "gen-api"})
          primitives #js {:self (fn [] #js {:username "TestBody" :pos #js {:x 1 :y 2 :z 3}
                                             :health 18 :food 15})
                          :isOffline (fn [] false)
                          :isSettling (fn [] true)}
          eng {:events stream
               :state (atom {:generation-id "gen-api"
                             :list ["j1"] :current "j1"
                             :instances {"j1" {:id "j1" :round 2 :hold? true
                                               :spec {:op :leaf :job 'jobs.movement.look-around
                                                      :args {:every-ms 2500}}}}
                             :attention {"req-1" {:request-id "req-1" :job-id "j1"
                                                  :reason :round-failed :updated-at 123
                                                  :event {:kind :failed :message "needs attention"}}}})
               :primitives primitives
               :jobs registry/jobs
               :triggers triggers/all
               :running (atom {:id "j1" :round 2 :reflex :health-low})
               :manual (atom nil)
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
                        (request socket-path "GET" "/status?limit=2" {} nil))))
             (.then (fn [response]
                      (let [status (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (.startsWith (:content-type response) "application/edn"))
                        (is (= :settling (:mode status)))
                        (is (= {:x 1 :y 2 :z 3} (:position status)))
                        (is (= 18 (:health status)))
                        (is (= 15 (:food status)))
                        (is (= "j1" (get-in status [:current :id])))
                        (is (= :running (get-in status [:current :status])))
                        (is (= :health-low (get-in status [:current :reflex])))
                        (is (= "jobs.movement.look-around" (get-in status [:current :name])))
                        (is (= {:total 0 :items [] :more? false} (:failed status)))
                        (is (= "req-1" (get-in status [:outstanding :items 0 :request-id])))
                        (request socket-path "GET" "/job?id=j1" {} nil))))
             (.then (fn [response]
                      (let [job (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (= {:every-ms 2500} (:args job)))
                        (is (= true (:hold? job)))
                        (is (= :running (:status job)))
                        (is (= true (:current? job)))
                        (request socket-path "GET"
                                 "/catalog?kind=job&name=jobs.movement.look-around" {} nil))))
             (.then (fn [response]
                      (let [job (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (= :job (:kind job)))
                        (is (= 2000 (get-in job [:args :every-ms :default])))
                        (is (string? (:doc job)))
                        (request socket-path "GET" "/catalog?kind=trigger&name=health-low" {} nil))))
             (.then (fn [response]
                      (let [trigger (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (= :trigger (:kind trigger)))
                        (is (= :health-low (:name trigger)))
                        (is (= :cooldown (:persistence trigger)))
                        (request socket-path "GET"
                                 "/catalog?kind=jobs&prefix=jobs.movement.&limit=1&offset=0" {} nil))))
             (.then (fn [response]
                      (let [listing (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (= ["jobs.movement.follow"] (:items listing)))
                        (is (= 1 (:next-offset listing)))
                        (request socket-path "GET" "/catalog?kind=triggers&prefix=health" {} nil))))
             (.then (fn [response]
                      (let [listing (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (= ["health-low"] (:items listing)))
                        (is (nil? (:next-offset listing)))
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
