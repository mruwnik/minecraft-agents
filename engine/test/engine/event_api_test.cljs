(ns engine.event-api-test
  (:require [cljs.test :refer [async deftest is testing]]
            [cljs.reader :as reader]
            [engine.chat :as chat]
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

(deftest inventory-view-is-read-only-bounded-and-omits-empty-equipment-slots
  (let [stacks (mapv (fn [slot] {:name (str "item-" slot) :count 2 :slot slot}) (range 50))
        eng {:primitives #js {:self (fn [] #js {:inventory (clj->js stacks)
                                                :equipment (clj->js {:head {:name "iron_helmet" :count 1 :durability 140}
                                                                     :offHand nil
                                                                     :mainHand {:name "iron_sword" :count 1}})})
                              :isOffline (fn [] false)}}
        view (event-api/inventory-view eng)]
    (is (= true (:ok view)))
    (is (= 46 (count (:inventory view))))
    (is (= {:name "item-45" :count 2 :slot 45} (last (:inventory view))))
    (is (true? (:more? view)))
    (is (= {:head {:name "iron_helmet" :count 1 :durability 140}
            :mainHand {:name "iron_sword" :count 1}}
           (:equipment view)))))

(deftest inventory-view-reports-offline-without-reading-primitives
  (let [eng {:primitives #js {:isOffline (fn [] true)}}]
    (is (= {:ok false :reason :offline} (event-api/inventory-view eng)))))

(deftest unix-api-serves-edn-snapshot-events-and-resolution
  (async done
    (let [dir (tu/tmp-dir)
          socket-path (path/join dir "events.sock")
          stream (events/make {:generation-id "gen-api"})
          primitives #js {:self (fn [] #js {:username "TestBody" :pos #js {:x 1 :y 2 :z 3}
                                             :health 18 :food 15
                                             :inventory #js [#js {:name "bread" :count 5 :slot 9}]
                                             :equipment #js {:head #js {:name "iron_helmet" :count 1 :durability 140}
                                                             :feet nil :offHand nil :mainHand nil}})
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
                        (request socket-path "GET" "/inventory" {} nil))))
             (.then (fn [response]
                      (let [inventory (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (true? (:ok inventory)))
                        (is (= [{:name "bread" :count 5 :slot 9}] (:inventory inventory)))
                        (is (= {:head {:name "iron_helmet" :count 1 :durability 140}}
                               (:equipment inventory)))
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

(deftest direct-chat-is-edn-bounded-and-does-not-require-an-owner-token
  (async done
    (let [dir (tu/tmp-dir)
          socket-path (path/join dir "chat.sock")
          sent (atom [])
          primitives #js {:self (fn [] #js {:username "ActualBody"})
                          :chatDirect (fn [args]
                                        (swap! sent conj (js->clj args :keywordize-keys true))
                                        (js/Promise.resolve #js {:status "sent" :parts 1}))}
          eng {:primitives primitives :said (atom []) :now js/Date.now :chat-limits chat/limits}
          server (event-api/create socket-path eng)]
      (tu/run-async
       done
       (fn []
         (let [work (-> ((:listen server))
                        (.then (fn [_]
                                 (request socket-path "POST" "/chat" {"Content-Type" "application/edn"}
                                          "{:message \"hello\" :to \"Player_1\" :from \"spoofed\"}")))
                        (.then (fn [response]
                                 (let [body (edn-response response)]
                                   (is (= 200 (:status response)))
                                   (is (= "ActualBody" (:body body)))
                                   (is (= "sent" (get-in body [:result :status])))
                                   (is (= [{:message "hello" :to "Player_1"}] @sent))
                                   (request socket-path "POST" "/chat" {"Content-Type" "application/edn"}
                                            "{:message \"/op Player_1\"}"))))
                        (.then (fn [response]
                                 (is (= 200 (:status response)))
                                 (is (= "command" (get-in (edn-response response) [:result :reason])))
                                 (is (= 1 (count @sent)))
                                 (request socket-path "POST" "/chat" {"Content-Type" "application/edn"}
                                          "{:message \"hello\" :to \"bad name\"}")))
                        (.then (fn [response]
                                 (is (= 400 (:status response)))
                                 (is (= :bad-request (:reason (edn-response response)))))))]
           (.finally work (fn [] ((:close server))))))))))
