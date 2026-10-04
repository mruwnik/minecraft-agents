(ns agent-tools.jobs-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.jobs :as jobs]
            [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn request [& args] (jobs/request-for (vec args)))

(deftest native-expressions-preserve-lists-symbols-and-keyword-arguments
  (let [r (request "--world" "w" "Bob" "submit" "(jobs.movement.go-to {:pos {:x -1 :y 64 :z 2}})" "--request-id" "move-home")]
    (is (= "move-home" (get-in r [:request :request-id])))
    (is (re-find #"\(jobs.movement.go-to \{:pos \{:x -1" (data/write-edn (:request r)))))
  (is (= :interrupt (get-in (request "--world" "w" "Bob" "interrupt" "(repeat (jobs.movement.look-around))") [:request :op]))))

(deftest list-show-cancel-and-retry-use-fixed-endpoints-and-validate-arguments
  (is (= "/jobs?limit=8&offset=0" (:path (request "--world" "w" "Bob"))))
  (is (= "/jobs?limit=3&offset=4" (:path (request "--world" "w" "Bob" "list" "--limit" "3" "--offset" "4"))))
  (is (= "/job?id=j4" (:path (request "--world" "w" "Bob" "show" "j4"))))
  (is (= "j4" (get-in (request "--world" "w" "Bob" "cancel" "j4") [:request :id])))
  (is (= :retry (get-in (request "--world" "w" "Bob" "retry" "j4") [:request :op])))
  (doseq [argv [["--world" "w" "../Bob"]
                ["--world" "w" "Bob" "cancel" "invalid"]
                ["--world" "w" "Bob" "list" "--request-id" "bad"]
                ["--world" "w" "Bob" "list" "--limit" "99"]]]
    (is (string? (:error (apply request argv))) (pr-str argv))))

(deftest attention-resolution-targets-its-dedicated-endpoint-and-validates-its-reason
  (let [handled (request "Bob" "--world" "w" "resolve" "notice:12" "--reason" "handled")]
    (is (= "/attention/resolve" (:path handled)))
    (is (true? (:resolve handled)))
    (is (= {:request-id "notice:12" :reason :handled} (:request handled))))
  (doseq [argv [["Bob" "--world" "w" "resolve" "notice:12"]
                ["Bob" "--world" "w" "resolve" "notice:12" "--reason" "cancel"]]]
    (is (string? (:error (apply request argv))) (pr-str argv))))

(deftest a-body-is-addressed-in-its-world
  (is (re-find #"missing --world <world>" (:error (request "Bob"))))
  (is (re-find #"world" (:error (request "Bob" "--world" "../x"))))
  (is (= "/s/worlds/w/agents/Bob/engine/events.sock" (:socketPath (request "Bob" "--world" "w" "--state" "/s")))))

(deftest malformed-multiple-or-oversized-native-job-forms-fail-locally
  (doseq [source ["{:job :foo}" "(jobs.one) (jobs.two)" "()" (apply str (repeat 13000 "x"))]]
    (is (thrown? js/Error (jobs/spec-for source)) source))
  (is (string? (:error (request "--world" "w" "Bob" "submit" "{:job \"custom\"}")))))

(deftest job-mutations-have-a-total-deadline-even-when-no-response-arrives
  (async done
    (-> (jobs/post! "/unused" {:op :cancel :id "j1"} {:timeout-ms 5 :request-fn (fn [_ _] (fake/pending-request))})
        (.then (fn [_] (is false "resolved")))
        (.catch (fn [error] (is (= "ETIMEDOUT" (.-code error)))))
        (.then done))))

(deftest cancel-all-has-no-job-argument-and-hold-and-front-are-submission-options
  (let [cancel (request "Bob" "--world" "w" "cancel-all" "--request-id" "all")
        held (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-day)" "--hold")
        front (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-day)" "--front")
        plain (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-day)")]
    (is (= :cancel-all (get-in cancel [:request :op])))
    (is (= "all" (get-in cancel [:request :request-id])))
    (is (not (contains? (:request cancel) :id)))
    (is (not (contains? (:request cancel) :spec)))
    (is (true? (:mutating cancel)))
    (is (true? (get-in held [:request :hold?])))
    (is (re-find #":hold\? true" (data/write-edn (:request held))))
    (is (true? (get-in front [:request :front?])))
    (is (re-find #":front\? true" (data/write-edn (:request front))))
    (is (not (contains? (:request plain) :hold?)))
    (is (not (contains? (:request plain) :front?))))
  (doseq [args [["cancel-all" "j1"] ["cancel-all" "--hold"] ["list" "--hold"] ["show" "j1" "--hold"] ["retry" "j1" "--hold"]]]
    (is (string? (:error (apply request "Bob" "--world" "w" args))) (pr-str args)))
  (doseq [args [["list" "--front"] ["show" "j1" "--front"] ["interrupt" "(jobs.time.wait-for-day)" "--front"]
                ["cancel-all" "--front"] ["cancel" "j1" "--front"] ["retry" "j1" "--front"]]]
    (is (re-find #"--front requires submit" (:error (apply request "Bob" "--world" "w" args))) (pr-str args))))

(defn state-dir [] (.mkdtempSync fs (.join path (.tmpdir os) "jobs-cli-")))

(defn run-main! [state argv handler]
  (let [[request-fn seen] (fake/request-fn handler)
        lines (atom [])]
    (-> (jobs/main! (into ["Bob" "--world" "w" "--state" state] argv)
                    {:request-fn request-fn :output #(swap! lines conj %)})
        (.then (fn [code] {:code code :out (apply str @lines) :seen @seen})))))

(defn engine [{:keys [method path]}]
  {:text (cond (= "/snapshot" path) "{:generation-id \"generation\"}"
               (= "GET" method) "{:ok true}"
               :else "{:ok true}")})

(deftest submit-options-and-cancel-all-go-through-the-generation-aware-api
  (let [state (state-dir)]
    (async done
      (-> (run-main! state ["submit" "(seq (jobs.time.wait-for-day) (jobs.movement.go-to {:pos {:x -1 :y 64 :z 2}}))"
                            "--hold" "--front" "--request-id" "held"] engine)
          (.then (fn [first-run]
                   (is (= 0 (:code first-run)))
                   (is (= {:ok true} (data/read-edn (:out first-run))))
                   (let [posts (filterv #(= "POST" (:method %)) (:seen first-run))
                         sent (data/read-edn (:body (first posts)))]
                     (is (= 1 (count posts)))
                     (is (= "/jobs" (:path (first posts))))
                     (is (true? (:hold? sent)))
                     (is (true? (:front? sent)))
                     (is (= "generation" (:generation-id sent)))
                     (is (= 'seq (first (:spec sent))))
                     (is (= {:pos {:x -1 :y 64 :z 2}} (second (nth (:spec sent) 2)))))
                   (run-main! state ["cancel-all" "--request-id" "clear"] engine)))
          (.then (fn [second-run]
                   (let [sent (data/read-edn (:body (first (filterv #(= "POST" (:method %)) (:seen second-run)))))
                         cached (.readFileSync fs (.join path state "worlds" "w" "agents" "Bob" ".commands" "jobs" "clear.edn") "utf8")]
                     (is (= :cancel-all (:op sent)))
                     (is (not (contains? sent :id)))
                     (is (= "generation" (:generation-id sent)))
                     (is (= {:generation-id "generation"} (data/read-edn cached))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-retry-with-the-same-request-id-reuses-the-recorded-generation
  (let [state (state-dir)
        engine-at (fn [generation] (fn [{:keys [path]}] {:text (if (= "/snapshot" path) (str "{:generation-id \"" generation "\"}") "{:ok true}")}))]
    (async done
      (-> (run-main! state ["cancel" "j1" "--request-id" "again"] (engine-at "old"))
          (.then (fn [_] (run-main! state ["cancel" "j1" "--request-id" "again"] (engine-at "new"))))
          (.then (fn [{:keys [seen]}]
                   (is (= "old" (:generation-id (data/read-edn (:body (last seen))))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest resolve-sends-one-direct-request-without-a-snapshot
  (let [state (state-dir)]
    (async done
      (-> (run-main! state ["resolve" "req-1" "--reason" "condition-recovered"]
                     (fn [_] {:text "{:ok true :resolved true}"}))
          (.then (fn [{:keys [code out seen]}]
                   (is (= 0 code))
                   (is (true? (:resolved (data/read-edn out))))
                   (is (= 1 (count seen)))
                   (is (= "/attention/resolve" (:path (first seen))))
                   (is (= {:request-id "req-1" :reason :condition-recovered} (data/read-edn (:body (first seen)))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest show-prints-the-bounded-job-detail-and-a-missing-route-asks-for-a-restart
  (let [state (state-dir)]
    (async done
      (-> (run-main! state ["show" "j3"]
                     (fn [_] {:text "{:id \"j3\" :name \"go\" :status :running :round 2 :spec (jobs.x) :noise 1 :attention {:total 0}}"}))
          (.then (fn [{:keys [code out]}]
                   (is (= 0 code))
                   (is (= {:id "j3" :name "go" :status :running :round 2 :spec '(jobs.x)} (data/read-edn out)))))
          (.then (fn [_] (run-main! state ["list"] (fn [_] {:status 404 :text "{:ok false :reason :not-found}"}))))
          (.then (fn [{:keys [code out]}]
                   (is (= 2 code))
                   (is (= {:ok false :reason :jobs-unavailable :action :restart-with-current-build} (data/read-edn out)))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest an-uncertain-mutation-reports-its-request-id
  (let [state (state-dir)]
    (async done
      (-> (run-main! state ["cancel" "j1" "--request-id" "r9"]
                     (fn [{:keys [path]}] (if (= "/snapshot" path)
                                              {:text "{:generation-id \"g\"}"}
                                              {:status 500 :text "{:ok false :reason :request-uncertain}"})))
          (.then (fn [{:keys [code out]}]
                   (is (= 1 code))
                   (is (= "r9" (:request-id (data/read-edn out))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))
