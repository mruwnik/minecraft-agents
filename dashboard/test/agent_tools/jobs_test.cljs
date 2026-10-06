(ns agent-tools.jobs-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.jobs :as jobs]
            [agent-tools.world-data :as data]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn request [& args] (jobs/request-for (vec args)))

(deftest native-expressions-preserve-lists-symbols-and-keyword-arguments
  (let [r (request "--world" "w" "Bob" "submit" "(jobs.movement.go-to {:pos {:x -1 :y 64 :z 2}})" "--request-id" "move-home")]
    (is (= "move-home" (get-in r [:request :request-id])))
    (is (re-find #"\(jobs.movement.go-to \{:pos \{:x -1" (data/write-edn (:request r)))))
  (is (= :interrupt (get-in (request "--world" "w" "Bob" "submit" "(repeat (jobs.movement.look-around))" "--now") [:request :op]))))

(deftest submit-appends-by-default-and-now-cuts-in-front-of-the-current-job
  (let [plain (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-day)")
        now (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-day)" "--now")]
    (is (= :submit (get-in plain [:request :op])))
    (is (= {:op :submit} (select-keys (:request plain) [:op :front? :hold?])) "a plain submit appends")
    (is (= :interrupt (get-in now [:request :op])))
    (is (= '(jobs.time.wait-for-day) (get-in now [:request :spec]))))
  (doseq [[pattern args] [[#"unknown operation" ["interrupt" "(jobs.time.wait-for-day)"]]
                          [#"--now and --front" ["submit" "(jobs.time.wait-for-day)" "--now" "--front"]]
                          [#"--now requires submit" ["list" "--now"]]
                          [#"--now requires submit" ["cancel" "j1" "--now"]]]]
    (is (re-find pattern (str (:error (apply request "Bob" "--world" "w" args)))) (pr-str args))))

(deftest wait-and-its-timeout-belong-to-submit
  (is (= {:timeout "5m"} (select-keys (:wait (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-day)" "--wait" "--timeout" "5m")) [:timeout])))
  (is (some? (:wait (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-day)" "--now" "--wait"))))
  (is (nil? (:wait (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-day)"))))
  (doseq [[pattern args] [[#"--wait requires submit" ["cancel" "j1" "--wait"]]
                          [#"--timeout requires --wait" ["submit" "(jobs.time.wait-for-day)" "--timeout" "5s"]]
                          [#"--timeout must be" ["submit" "(jobs.time.wait-for-day)" "--wait" "--timeout" "forever"]]]]
    (is (re-find pattern (str (:error (apply request "Bob" "--world" "w" args)))) (pr-str args))))

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

(deftest a-malformed-job-spec-error-shows-an-example
  (doseq [source ["{:job :foo}" "()" "(jobs.one) (jobs.two)"]]
    (is (re-find #"e\.g\. \(jobs\.movement\.go-to \{" (.-message (try (jobs/spec-for source) (catch :default e e)))) source)))

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
  (doseq [args [["list" "--front"] ["show" "j1" "--front"]
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
                     (fn [_] {:text "{:id \"j3\" :name \"go\" :status :queued :round 2 :spec (jobs.x) :noise 1 :waiting {:reason :no-tree :radius 2} :attention {:total 0}}"}))
          (.then (fn [{:keys [code out]}]
                   (is (= 0 code))
                   (is (= {:id "j3" :name "go" :status :queued :round 2 :spec '(jobs.x) :waiting {:reason :no-tree :radius 2}} (data/read-edn out)))))
          (.then (fn [_] (run-main! state ["list"] (fn [_] {:status 404 :text "{:ok false :reason :not-found}"}))))
          (.then (fn [{:keys [code out]}]
                   (is (= 2 code))
                   (is (= {:ok false :reason :jobs-unavailable :action :restart-with-current-build} (data/read-edn out)))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest show-of-a-job-the-scheduler-no-longer-holds-answers-from-the-event-history
  (let [state (state-dir)
        ev (fn [n kind] {:seq n :generation-id "g" :time-ms n :source :job :kind kind :context {:job-id "j7"}})
        handler (fn [{:keys [path]}]
                  (cond
                    (str/starts-with? path "/job?") {:status 404 :text "{:ok false :reason :job-not-found}"}
                    (= "/snapshot" path) {:text (data/write-edn {:generation-id "g" :cursor {:stream-id "s" :seq 3}})}
                    :else {:text (data/write-edn {:stream-id "s" :latest-seq 3 :gap? false
                                                  :events [(ev 1 :queued) (ev 2 :round_started) (ev 3 :completed)]})}))]
    (async done
      (-> (run-main! state ["show" "j7"] handler)
          (.then (fn [{:keys [code out]}]
                   (is (= 0 code) out)
                   (is (= :completed (:status (data/read-edn out))) out)
                   (is (true? (:finished? (data/read-edn out))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest cancel-or-retry-of-a-finished-job-says-it-is-gone-and-a-held-submit-says-it-runs
  (let [state (state-dir)
        snap "{:generation-id \"generation\"}"
        gone (fn [{:keys [path]}] (if (= "/snapshot" path) {:text snap} {:status 404 :text "{:ok false :reason :job-not-found}"}))]
    (async done
      (-> (run-main! state ["cancel" "j5"] gone)
          (.then (fn [{:keys [code out]}]
                   (is (= 1 code))
                   (is (= :job-not-found (:reason (data/read-edn out))))
                   (is (string? (:hint (data/read-edn out))) out)))
          (.then (fn [_] (run-main! state ["retry" "j5"] gone)))
          (.then (fn [{:keys [out]}] (is (string? (:hint (data/read-edn out))) out)))
          (.then (fn [_] (run-main! state ["submit" "(jobs.x {})" "--hold"]
                                    (fn [{:keys [path]}] {:text (if (= "/snapshot" path) snap "{:ok true :job {:id \"j5\" :status :queued :hold? true}}")}))))
          (.then (fn [{:keys [code out]}]
                   (is (= 0 code))
                   (is (string? (:hint (data/read-edn out))) out)))
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

;; submit --wait

(defn event-of [n source kind context & [extra]]
  (merge {:seq n :generation-id "g" :time-ms n :source source :kind kind :context context} extra))

(defn waiting-engine
  "A fake engine whose event stream is the atom events; POST /jobs answers the job j7."
  [events]
  (fn [{:keys [method path]}]
    (let [cursor {:stream-id "s" :seq (count @events)}]
      {:text (data/write-edn
               (cond
                 (= "POST" method) {:ok true :job {:id "j7" :status :queued}}
                 (= "/snapshot" path) {:generation-id "g" :body "Bob" :outstanding {} :cursor cursor
                                       :state {:instances {"j7" {}}}}
                 (= "/status" path) {:mode :scheduled}
                 :else (let [after (js/Number (.get (.-searchParams (js/URL. (str "http://x" path))) "after"))]
                         {:gap? false :stream-id "s" :latest-seq (count @events) :cursor cursor
                          :events (filterv #(> (:seq %) after) @events)})))})))

(deftest submit-wait-returns-the-job-and-everything-until-it-ends
  (let [state (state-dir)
        events (atom [(event-of 1 :system :started {})])]
    (js/setTimeout #(swap! events into [(event-of 2 :job :queued {:job-id "j7" :chain ["j7"]})
                                        (event-of 3 :reflex :fired {:job-id "j8" :reflex-id :hostile-near}
                                                  {:message "hostile-near → jobs.survival.respond-to-hostile"})
                                        (event-of 4 :body :picked-up {} {:data {:item "oak_log" :count 3}})
                                        (event-of 5 :job :arrived {:job-id "j7" :chain ["j7"]} {:data {:pos [1 64 2]}})
                                        (event-of 6 :job :completed {:job-id "j7" :chain ["j7"]})])
                   60)
    (async done
      (-> (run-main! state ["submit" "(jobs.movement.go-to {:pos {:x 1 :y 64 :z 2}})" "--wait" "--timeout" "3s"]
                     (waiting-engine events))
          (.then (fn [{:keys [code out]}]
                   (let [result (data/read-edn out)
                         wait (:wait result)]
                     (is (= 0 code))
                     (is (= {:id "j7" :status :queued} (:job result)))
                     (is (= [:job-finished "j7" :completed] [(:wake wait) (:job wait) (:result wait)]))
                     (is (= [{:event :arrived :data {:pos [1 64 2]}}] (:events wait)) "the job's own events")
                     (is (= {:reflexes 1 :picked-up 1} (get-in wait [:summary :counts])) "and what else happened meanwhile")
                     (is (= {:event :fired :reflex :hostile-near :message "hostile-near → jobs.survival.respond-to-hostile"}
                            (first (get-in wait [:summary :items]))))
                     (is (not (contains? result :follow)) "nothing left to follow"))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-wait-that-ends-before-the-job-says-how-to-go-on-waiting
  (let [state (state-dir)
        events (atom [(event-of 1 :system :started {})])]
    (async done
      (-> (run-main! state ["submit" "(jobs.time.wait-for-day)" "--wait" "--timeout" "100ms"] (waiting-engine events))
          (.then (fn [{:keys [code out]}]
                   (let [result (data/read-edn out)]
                     (is (= 0 code))
                     (is (= :timeout (get-in result [:wait :wake])))
                     (is (= "./bin/observe --wait --watch j7" (:follow result))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-waited-submit-under-manual-control-says-who-holds-the-body
  (let [state (state-dir)
        events (atom [(event-of 1 :system :started {})])
        held {:who "Wren" :why "probing" :since 5}
        engine (fn [req] (if (= "/status" (:path req))
                           {:text (str "{:mode :manual :manual " (pr-str held) "}")}
                           ((waiting-engine events) req)))]
    (async done
      (-> (run-main! state ["submit" "(jobs.time.wait-for-day)" "--wait" "--timeout" "100ms"] engine)
          (.then (fn [{:keys [out]}]
                   (let [result (data/read-edn out)]
                     (is (re-find #"manual control held by Wren" (str (:hint result))) out)
                     (is (= :timeout (get-in result [:wait :wake]))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-refused-submit-does-not-wait
  (let [state (state-dir)]
    (async done
      (-> (run-main! state ["submit" "(jobs.time.wait-for-day)" "--wait"]
                     (fn [{:keys [method path]}]
                       {:status (if (= "POST" method) 400 200)
                        :text (if (= "/snapshot" path) "{:generation-id \"g\"}" "{:ok false :reason :bad-spec}")}))
          (.then (fn [{:keys [code out seen]}]
                   (is (= 1 code))
                   (is (= {:ok false :reason :bad-spec} (data/read-edn out)))
                   (is (= ["GET" "POST"] (mapv :method seen)) "no event reads")))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(defn manual-engine [manual]
  (fn [{:keys [path]}]
    {:text (cond (= "/snapshot" path) "{:generation-id \"generation\"}"
                 (= "/status" path) (str "{:mode :manual :manual " (pr-str manual) "}")
                 (str/starts-with? path "/jobs?") "{:total 1 :items [{:id \"j1\" :name \"go\" :status :queued}]}"
                 :else "{:ok true :job {:id \"j2\" :status :queued}}")}))

(deftest queued-jobs-under-manual-control-say-who-holds-the-body
  (let [state (state-dir)
        held {:who "Wren" :why "probing" :since 5}]
    (async done
      (-> (run-main! state ["list"] (manual-engine held))
          (.then (fn [{:keys [out]}]
                   (let [hint (:hint (data/read-edn out))]
                     (is (re-find #"manual control" hint) out)
                     (is (re-find #"Wren" hint) out)
                     (is (re-find #"drive.mjs.*release" hint) out))))
          (.then (fn [_] (run-main! state ["submit" "(jobs.x {})"] (manual-engine held))))
          (.then (fn [{:keys [out]}] (is (re-find #"Wren" (str (:hint (data/read-edn out)))) out)))
          (.then (fn [_] (run-main! state ["list"] (fn [{:keys [path]}]
                                                     {:text (if (= "/status" path)
                                                              "{:mode :scheduled :manual nil}"
                                                              "{:total 1 :items [{:id \"j1\" :status :queued}]}")}))))
          (.then (fn [{:keys [out]}] (is (nil? (:hint (data/read-edn out))) out)))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest usage-says-jobs-wait-under-manual-control
  (is (re-find #"manual control" jobs/usage)))

(deftest failure-for-keeps-the-transport-reason
  (doseq [[code reason] [["ENOENT" :no-running-body] ["ETIMEDOUT" :timeout] ["ERESPONSETOOLARGE" :response-too-large]
                         ["EACCES" :socket-access-denied] ["EPIPE" :transport-error]]]
    (is (= reason (:reason (jobs/failure-for {} (let [e (js/Error. "x")] (aset e "code" code) e)))) code)))
