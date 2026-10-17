(ns engine.event-api-test
  (:require [cljs.test :refer [are async deftest is testing]]
            [cljs.reader :as reader]
            [engine.build :as build]
            [engine.chat :as chat]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.job-api :as job-api]
            [engine.args :as a]
            [engine.registry :as registry]
            [engine.settings :as settings]
            ["fs" :as fs]
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

(deftest the-running-round-is-running-in-every-view
  (is (= :running (event-api/instance-status {:list ["j1"] :current nil} "j1" "j1")) "the round in flight, state :current not yet set")
  (is (= :queued (event-api/instance-status {:list ["j1"]} "j1" "j2")))
  (is (= :failed (event-api/instance-status {:failed {"j1" {}}} "j1" "j1"))))

(defn status-engine
  "A minimal engine for event-api/status: the primitives and state given."
  [primitives state running]
  {:events (events/make {:generation-id "g"})
   :store (atom {:now js/Date.now :data {}})
   :state (atom (merge {:generation-id "g" :list [] :instances {}} state))
   :primitives primitives
   :jobs registry/jobs
   :triggers triggers/all
   :results (atom {})
   :running (atom running)
   :manual (atom nil)
   :now (constantly 123)})

(deftest status-names-the-engine-build-and-its-start
  (let [view (event-api/status (status-engine #js {:isOffline (fn [] false) :isSettling (fn [] false) :self (fn [] #js {})} {} nil) nil {:build build/status-extra})]
    (is (= #{:commit :started-at} (set (keys (:build view)))))
    (is (number? (:started-at (:build view))))))

(deftest read-commit-reads-the-build-id-beside-the-bundle
  (let [dir (fs/mkdtempSync (path/join (.tmpdir (js/require "os")) "build-id-"))]
    (is (nil? (build/read-commit dir)))
    (fs/writeFileSync (path/join dir "build-id.edn") "{:commit \"abc1234\"}")
    (is (= "abc1234" (build/read-commit dir)))
    (fs/rmSync dir #js {:recursive true})))

(deftest status-of-an-offline-body-shows-its-last-known-position-health-and-food
  (let [view (event-api/status
              (status-engine #js {:isOffline (fn [] true) :isSettling (fn [] false)
                                  :self (fn [] #js {:status "offline"})
                                  :lastKnown (fn [] #js {:username "Probe" :pos #js {:x 1.5 :y 64 :z -2.5 :yaw 3} :health 11 :food 7})}
                             {} nil)
              nil)]
    (is (= :offline (:mode view)))
    (is (= true (:last-known view)))
    (is (= {:x 1.5 :y 64 :z -2.5} (:position view)))
    (is (= 11 (:health view)))
    (is (= 7 (:food view)))))

(deftest status-of-a-body-whose-connection-dropped-says-offline-by-connection
  (let [view (event-api/status
              (status-engine #js {:isOffline (fn [] true) :isSettling (fn [] false)
                                  :self (fn [] #js {:status "offline"}) :lastKnown (fn [] nil)}
                             {} nil)
              nil)]
    (is (= :offline (:mode view)))
    (is (= {:by :connection :why :connection-lost} (:offline view)))))

(deftest status-of-an-offline-body-with-nothing-read_has_no_last-known
  (let [view (event-api/status
              (status-engine #js {:isOffline (fn [] true) :isSettling (fn [] false)
                                  :self (fn [] #js {:status "offline"}) :lastKnown (fn [] nil)}
                             {} nil)
              nil)]
    (is (nil? (:last-known view)))
    (is (nil? (:position view)))
    (is (nil? (:health view)))))

(deftest status-adds-what-its-extras-report-from-the-memory-view
  (let [eng (status-engine #js {:isOffline (fn [] false) :isSettling (fn [] false)
                                :self (fn [] #js {:username "Probe"})}
                           {} nil)
        view (event-api/status eng nil {:marker (fn [mem-view] {:now (:now mem-view)})})]
    (is (number? (:now (:marker view))) "an extra gets the memory view, its result is a status key")
    (is (not (contains? (event-api/status eng nil) :died)) "the engine itself reports no death")))

(deftest a-job-just-started-reads-the-same-in-status-job-detail-and-the-list
  (let [inst {"j1" {:id "j1" :round 0 :spec {:op :leaf :job 'jobs.movement.look-around :args {}}}}
        eng (status-engine #js {:isOffline (fn [] false) :isSettling (fn [] false)
                                :self (fn [] #js {:username "Probe" :pos #js {:x 0 :y 64 :z 0} :health 20 :food 20})}
                           {:list ["j1"] :current nil :instances inst}
                           {:id "j1" :round 1})
        status (event-api/status eng nil)]
    (is (= :running (get-in status [:current :status])))
    (is (= :running (get-in status [:jobs :items 0 :status])))
    (is (= :running (:status (event-api/job-detail eng "j1" 4))))
    (is (= :running (get-in (job-api/list-jobs eng 0 10) [:items 0 :status])))))

(deftest inventory-view-is-read-only-bounded-and-shows-empty-armour-slots-and-omits-other-empty-ones
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
            :torso :empty :legs :empty :feet :empty
            :mainHand {:name "iron_sword" :count 1}}
           (:equipment view)))))

(deftest inventory-view-of-an-offline-body-is-its-last-known-one
  (let [eng {:primitives #js {:isOffline (fn [] true)
                              :lastKnown (fn [] #js {:inventory #js [#js {:name "bread" :count 5 :slot 9}]
                                                     :equipment #js {:mainHand #js {:name "iron_sword" :count 1}}})}}
        view (event-api/inventory-view eng)]
    (is (= true (:ok view)))
    (is (= true (:last-known view)))
    (is (= {:by :connection :why :connection-lost} (:offline view)))
    (is (= [{:name "bread" :count 5 :slot 9}] (:inventory view)))
    (is (= "iron_sword" (get-in view [:equipment :mainHand :name])))))

(deftest inventory-view-reports-offline-without-reading-primitives
  (let [eng {:primitives #js {:isOffline (fn [] true)}}]
    (is (= {:ok false :reason :offline :offline {:by :connection :why :connection-lost}} (event-api/inventory-view eng)))))

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
                          :seenBlocks (fn [_] #js [#js {:name "chest" :pos #js {:x 1 :y 2 :z 3} :distance 1.5 :age-ms 0}])
                          :isOffline (fn [] false)
                          :isSettling (fn [] true)}
          flushes (atom 0)
          eng {:events stream
               :flush-seen (fn [] (swap! flushes inc) (js/Promise.resolve nil))
               :store (atom {:now js/Date.now :data {}})
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
               :running (atom {:id "j1" :round 2 :reflex :hungry})
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
                        (is (= :hungry (get-in status [:current :reflex])))
                        (is (= "jobs.movement.look-around" (get-in status [:current :name])))
                        (is (= {:total 0 :items [] :more? false} (:failed status)))
                        (is (= "req-1" (get-in status [:outstanding :items 0 :request-id])))
                        (request socket-path "GET" "/inventory" {} nil))))
             (.then (fn [response]
                      (let [inventory (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (true? (:ok inventory)))
                        (is (= [{:name "bread" :count 5 :slot 9}] (:inventory inventory)))
                        (is (= {:head {:name "iron_helmet" :count 1 :durability 140} :torso :empty :legs :empty :feet :empty}
                               (:equipment inventory)))
                        (request socket-path "GET" "/seen?names=chest&radius=10&max=5" {} nil))))
             (.then (fn [response]
                      (is (= 200 (:status response)))
                      (is (= [{:name "chest" :pos [1 2 3] :distance 1.5 :age-ms 0}] (:blocks (edn-response response))))
                      (request socket-path "GET" "/seen?radius=abc" {} nil)))
             (.then (fn [response]
                      (is (= 400 (:status response)))
                      (request socket-path "GET" "/seen?names=Bad!" {} nil)))
             (.then (fn [response]
                      (is (= 400 (:status response)))
                      (request socket-path "POST" "/seen/flush" {} nil)))
             (.then (fn [response]
                      (is (= 200 (:status response)))
                      (is (true? (:ok (edn-response response))))
                      (is (= 1 @flushes))
                      (request socket-path "GET" "/job?id=j1" {} nil)))
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
                        (request socket-path "GET" "/catalog?kind=trigger&name=hungry" {} nil))))
             (.then (fn [response]
                      (let [trigger (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (= :trigger (:kind trigger)))
                        (is (= :hungry (:name trigger)))
                        (is (= :cooldown (:persistence trigger)))
                        (request socket-path "GET"
                                 "/catalog?kind=jobs&prefix=jobs.movement.&limit=1&offset=0" {} nil))))
             (.then (fn [response]
                      (let [listing (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (= 1 (count (:items listing))))
                        (is (.startsWith (first (:items listing)) "jobs.movement."))
                        (is (= 1 (:next-offset listing)))
                        (request socket-path "GET" "/catalog?kind=triggers&prefix=hung" {} nil))))
             (.then (fn [response]
                      (let [listing (edn-response response)]
                        (is (= 200 (:status response)))
                        (is (= ["hungry"] (:items listing)))
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

(deftest an-engine-error-in-a-post-is-a-500-not-a-bad-request
  (async done
    (let [socket-path (path/join (tu/tmp-dir) "events.sock")
          server (event-api/create socket-path {:events (events/make {:generation-id "g"})})]
      (tu/run-async
       done
       (fn []
         (-> ((:listen server))
             (.then (fn [_] (request socket-path "POST" "/jobs" {"Content-Type" "application/edn"} "{:op :cancel-all}")))
             (.then (fn [response]
                      (is (= 500 (:status response)))
                      (is (= :engine-error (:reason (edn-response response))))))
             (.finally (fn [] ((:close server))))))))))

(deftest an-engine-error-in-a-post-is-logged-with-message-and-stack
  (async done
    (let [socket-path (path/join (tu/tmp-dir) "events.sock")
          stream (events/make {:generation-id "g"})
          server (event-api/create socket-path {:events stream})]
      (tu/run-async
       done
       (fn []
         (-> ((:listen server))
             (.then (fn [_] (request socket-path "POST" "/jobs" {"Content-Type" "application/edn"} "{:op :cancel-all}")))
             (.then (fn [response]
                      (let [logged (->> (:events (events/read-after stream {:stream-id (:stream-id @stream) :after 0}))
                                        (filter #(= :engine-error (:kind %))))]
                        (is (= 500 (:status response)))
                        (is (= 1 (count logged)))
                        (is (= :error (:level (first logged))))
                        (is (string? (:message (first logged))))
                        (is (string? (get-in (first logged) [:data :stack]))))))
             (.finally (fn [] ((:close server))))))))))

(deftest inventory-view-keeps-a-short-list-of-enchants-on-worn-gear
  (let [enchants (mapv (fn [i] {:name (str "enchant-" i) :level i}) (range 1 15))
        eng {:primitives #js {:self (fn [] #js {:inventory #js []
                                                :equipment (clj->js {:torso {:name "iron_chestplate" :count 1
                                                                             :enchants (conj enchants {:name 5 :level 1})}})})
                              :isOffline (fn [] false)}}
        shown (get-in (event-api/inventory-view eng) [:equipment :torso :enchants])]
    (is (= {:name "enchant-1" :level 1} (first shown)))
    (is (= 8 (count shown)) "capped")))

(deftest settings-ops-read-reload-set-and-unset
  (async done
    (let [dir (tu/tmp-dir)
          socket-path (path/join dir "events.sock")
          body-file (path/join dir "body.edn")
          server (event-api/create socket-path {:events (events/make {:generation-id "g"})
                                                :primitives #js {}})
          post (fn [p body] (request socket-path "POST" p {"Content-Type" "application/edn"} body))]
      (tu/run-async
       done
       (fn []
         (settings/with-settings {}
           (fn []
             (settings/load! {:specs (a/declare-settings! {:jobs.demo/wait-ms {:default 5000 :doc "w" :spec (a/int-in 0 nil)}})
                              :body-file body-file :body "Bob" :emit (fn [_])})
             (-> ((:listen server))
                 (.then (fn [_] (request socket-path "GET" "/settings" {} nil)))
                 (.then (fn [response]
                          (let [v (edn-response response)]
                            (is (= 200 (:status response)))
                            (is (= {:value 5000 :layer :code :doc "w"} (get-in v [:settings :jobs.demo/wait-ms])))
                            (is (= 20 (:game-rate v)))
                            (is (= 50 (:physics-ms v))))
                          (post "/settings/set" "{:key :jobs.demo/wait-ms :value 9}")))
                 (.then (fn [response]
                          (is (= 200 (:status response)))
                          (is (= {:ok true} (select-keys (edn-response response) [:ok])))
                          (is (= 9 (settings/get {:jobs.demo/wait-ms {:default 5000}} :jobs.demo/wait-ms)))
                          (post "/settings/set" "{:key :jobs.demo/wait-ms :value -1}")))
                 (.then (fn [response]
                          (is (= 409 (:status response)))
                          (is (= false (:ok (edn-response response))))
                          (fs/writeFileSync body-file "{:jobs.demo/wait-ms 12}")
                          (post "/settings/reload" "{}")))
                 (.then (fn [response]
                          (is (= 200 (:status response)))
                          (is (= 12 (settings/get {:jobs.demo/wait-ms {:default 5000}} :jobs.demo/wait-ms)))
                          (post "/settings/unset" "{:key :jobs.demo/wait-ms}")))
                 (.then (fn [response]
                          (is (= 200 (:status response)))
                          (is (= 5000 (settings/get {:jobs.demo/wait-ms {:default 5000}} :jobs.demo/wait-ms)))
                          (post "/settings/set" "{:key \"nope\" :value 1}")))
                 (.then (fn [response] (is (= 400 (:status response)))))
                 (.finally (fn [] ((:close server))))))))))))

(deftest a-finished-job-reads-its-recorded-result-from-job-detail
  (let [eng (status-engine #js {:isOffline (fn [] false) :isSettling (fn [] false)
                                :self (fn [] #js {:username "Probe"})}
                           {} nil)
        _ (reset! (:results eng) {"j5" {:name "mine" :status :stopped :result {:reason :no-ore} :ended-at 7 :n 1}})]
    (is (= {:ok true :id "j5" :name "mine" :status :stopped :result {:reason :no-ore} :ended-at 7 :finished? true}
           (select-keys (event-api/job-detail eng "j5" 4) [:ok :id :name :status :result :ended-at :finished?])))
    (is (nil? (event-api/job-detail eng "j6" 4)))
    (is (= {"j5" {:name "mine" :status :stopped :result {:reason :no-ore} :ended-at 7}}
           (:results (event-api/results eng ["j5" "j6"])))
        "the listing answers for the asked ids only, without the order counter")))

(deftest results-leave-out-ids-that-are-running-now
  (let [eng (status-engine #js {:isOffline (fn [] false) :isSettling (fn [] false)
                                :self (fn [] #js {:username "Probe"})}
                           {} nil)]
    (reset! (:results eng) {"j1" {:name "old" :status :completed :result {} :ended-at 7 :n 1}})
    (swap! (:state eng) assoc-in [:instances "j1"] {:spec '(walk)})
    (is (= {} (:results (event-api/results eng ["j1"]))) "a stale record never answers for a live job id")))

(defn seen-engine [rows calls]
  {:primitives #js {:isOffline (fn [] false)
                    :seenBlocks (fn [q] (swap! calls conj (js->clj q :keywordize-keys true))
                                  (clj->js rows))}})

(deftest seen-view-lists-remembered-blocks-nearest-first-with-name-pos-and-distance
  (let [calls (atom [])
        rows [{:name "chest" :pos {:x 1 :y 64 :z 2} :distance 3.04 :age-ms 5}
              {:name "iron_ore" :pos {:x 4 :y 60 :z 2} :distance 7.26 :age-ms 9}]
        view (event-api/seen-view (seen-engine rows calls) {:names ["chest" "*_ore"] :radius 20 :max 10})]
    (is (= {:ok true :radius 20
            :blocks [{:name "chest" :pos [1 64 2] :distance 3.0 :age-ms 5}
                     {:name "iron_ore" :pos [4 60 2] :distance 7.3 :age-ms 9}]}
           view))
    (is (= 20 (:radius (first @calls))))
    (is (= 11 (:max (first @calls))))))

(deftest seen-view-matches-star-suffix-names-and-bounds-radius-and-count
  (let [calls (atom [])
        _ (event-api/seen-view (seen-engine [] calls) {:names ["*_bed" "chest"] :radius 500 :max 500})
        q (first @calls)]
    (is (= 64 (:radius q)))
    (is (nil? (:names q)))
    (is (fn? (:match q)))
    (is (<= (:max q) 65))))

(deftest seen-view-reports-more-when-the-list-is-cut
  (let [rows (mapv (fn [i] {:name "chest" :pos {:x i :y 1 :z 1} :distance (double i) :age-ms 0}) (range 4))
        view (event-api/seen-view (seen-engine rows (atom [])) {:names ["chest"] :radius 8 :max 3})]
    (is (= 3 (count (:blocks view))))
    (is (true? (:more? view)))))

(deftest seen-view-of-an-offline-body-reports-offline-without-reading-memory
  (let [calls (atom [])
        eng (let [e (seen-engine [] calls)] (aset (:primitives e) "isOffline" (fn [] true)) e)]
    (is (= {:ok false :reason :offline :offline {:by :connection :why :connection-lost}}
           (event-api/seen-view eng {:names ["chest"] :radius 8 :max 3})))
    (is (empty? @calls))))

(deftest seen-matcher-takes-exact-names-and-star-suffixes
  (let [m (event-api/seen-matcher ["chest" "*_bed" "*ore"])]
    (is (true? (m "chest")))
    (is (true? (m "white_bed")))
    (is (true? (m "iron_ore")))
    (is (true? (m "deepslate_iron_ore")))
    (is (false? (m "trapped_chest")))
    (is (false? (m "bedrock")))
    (is (false? (m "bed_rock")))
    (is (nil? (event-api/seen-matcher nil)))))

(deftest inventory-view-lists-the-live-reserve-entries
  (let [store (atom {:now (constantly 1000) :data {}})
        eng {:store store :primitives #js {:isOffline (fn [] false) :self (fn [] #js {:inventory #js []})}}
        kept {:id "k" :item "diamond" :count 3 :strength 1 :by "agent" :why "beacon" :until nil}]
    (swap! store assoc :data {:entries {:reserve [{:t 0 :data kept}
                                                   {:t 0 :data {:id "old" :item "bread" :strength 0.5 :until 500}}
                                                   {:t 0 :data {:id "dead" :item "bread" :strength 0.5 :by "gone-1"}}]}})
    (is (= [kept] (:reserved (event-api/inventory-view eng))))
    (swap! store assoc :data {})
    (is (not (contains? (event-api/inventory-view eng) :reserved)))))

(deftest chests-view-lists-remembered-chest-stock-nearest-first-with-age
  (let [store (atom {:now (constantly 100000) :data {}})
        eng {:store store
             :primitives #js {:isOffline (fn [] false)
                              :self (fn [] #js {:pos #js {:x 0 :y 64 :z 0}})}}]
    (swap! store assoc :data {:entries {:fetch/stock [{:t 40000 :data {:pos [30 64 0] :items {"bread" 3}}}
                                                      {:t 90000 :data {:pos [5 64 0] :items {"shears" 1 "dirt" 2}}}
                                                      {:t 95000 :data {:pos [5 64 0] :items {"shears" 1}}}]}})
    (is (= {:ok true
            :chests [{:pos [5 64 0] :distance 5.0 :age-ms 5000 :items {"shears" 1}}
                     {:pos [30 64 0] :distance 30.0 :age-ms 60000 :items {"bread" 3}}]}
           (event-api/chests-view eng {:max 10})) "the latest entry per chest wins")
    (is (= [[5 64 0]] (mapv :pos (:chests (event-api/chests-view eng {:max 1})))))
    (is (true? (:more? (event-api/chests-view eng {:max 1}))))))

(def wide-args
  (into {} (for [i (range 40)] [(keyword (str "arg" i)) {:default i :doc "d"}])))

(deftest catalog-entry-shows-every-declared-arg
  (let [eng {:jobs {'jobs.demo.wide {:doc "wide" :args wide-args}}}
        args (:args (event-api/catalog-entry eng "job" "jobs.demo.wide"))]
    (is (= (set (keys wide-args)) (set (keys args))))
    (is (not (contains? args :truncated)))))

(deftest event-payload-values-stay-bounded
  (let [bounded (event-api/bounded-value wide-args 0 (volatile! 64))]
    (is (true? (:truncated bounded)))))

(deftest scalar-fields-of-a-result-survive-a-bulky-neighbour
  (let [result {:animals (vec (repeat 20 {:kind "cow" :id 1 :pos [1 2 3]}))
                :leaks [{:pos {:x 1 :y 2 :z 3} :why :gap}]
                :closed? false :reason :leak :cells 15}
        bounded (event-api/bounded-value result 0 (volatile! 32))]
    (is (= {:closed? false :reason :leak :cells 15} (select-keys bounded [:closed? :reason :cells])))))

(deftest catalog-entry-shows-target-ways-and-defaults
  (let [eng {:jobs {'jobs.demo.go {:doc "go" :args {} :target [:pos] :ways [:walk :stair] :defaults {:spend {:job-time 30 :food (fn [_] 2)} :least 1}}
                    'jobs.demo.old {:doc "old" :args {}}}}
        entry (event-api/catalog-entry eng "job" "jobs.demo.go")]
    (is (= [:pos] (:target entry)))
    (is (= [:walk :stair] (:ways entry)))
    (is (= {:spend {:job-time 30 :food :from-args} :least 1} (:defaults entry)) "a default set by a fn of the args reads :from-args")
    (is (not-any? #(contains? (event-api/catalog-entry eng "job" "jobs.demo.old") %) [:target :ways :defaults]))))

(def quoted-job
  {:args {:pos {:doc "where" :default nil}} :target [:pos]
   :quote-of (fn [_] {:uses {:job-time [1 2 3]}})
   :round (fn [c] (ctx/end c :done nil))})

(deftest quote-view-asks-a-job-for-its-quote
  (async done
    (let [eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now (constantly 1000) :jobs {'jobs.demo.q quoted-job} :triggers {}
                            :events (events/make {:body "Fake"})})]
      (-> (js/Promise.all #js [(event-api/quote-view eng "jobs.demo.q" "{:pos {:x 1 :y 64 :z 2}}")
                               (event-api/quote-view eng "jobs.demo.q" nil)
                               (event-api/quote-view eng "jobs.demo.nope" nil)
                               (event-api/quote-view eng "jobs.demo.q" "{:pos")])
          (.then (fn [[ok no-args unknown bad]]
                   (is (= {:ok true :job 'jobs.demo.q :quote {:uses {:job-time [1 2 3]}}} ok))
                   (is (true? (:ok no-args)))
                   (is (= :unknown-job (:reason unknown)))
                   (is (= :bad-args (:reason bad)))
                   (done)))))))

(deftest catalog-list-filters-jobs-by-category
  (let [eng {:jobs {'jobs.a.goal {:category :goal} 'jobs.a.step {:category :phase :part-of '[jobs.a.goal]}}}]
    (are [category items] (= items (:items (event-api/catalog-list eng "jobs" "" 0 10 category)))
      nil ["jobs.a.goal" "jobs.a.step"]
      "phase" ["jobs.a.step"])
    (is (= [:phase '[jobs.a.goal]] ((juxt :category :part-of) (event-api/catalog-entry eng "job" "jobs.a.step"))))))
