(ns agent-tools.jobs-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.jobs :as jobs]
            [agent-tools.world-data :as data]
            [dashboard.jobs-registry :as reg]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(deftest usage-for-builds-help-with-custom-prefix
  "usage-for should build the usage string with the given prefix, not including body/world args"
  (let [raw (jobs/usage-for "jobs.mjs")
        bin (jobs/usage-for "./bin/jobs")]
    (is (str/starts-with? raw "usage: jobs.mjs <body> --world <world>"))
    (is (str/starts-with? bin "usage: ./bin/jobs <body> --world <world>"))
    (is (str/includes? raw "observe.mjs <body>"))
    (is (str/includes? bin "observe.mjs <body>")) "usage-for doesn't cut other tool names"))

(defn request [& args] (jobs/request-for (vec args)))

(deftest native-expressions-preserve-lists-symbols-and-keyword-arguments
  (let [r (request "--world" "w" "Bob" "submit" "(jobs.movement.go-to {:pos {:x -1 :y 64 :z 2}})" "--request-id" "move-home")]
    (is (= "move-home" (get-in r [:request :request-id])))
    (is (re-find #"\(jobs.movement.go-to \{:pos \{:x -1" (data/write-edn (:request r)))))
  (is (= :interrupt (get-in (request "--world" "w" "Bob" "submit" "(repeat (jobs.movement.look-around))" "--now") [:request :op]))))

(deftest submit-appends-by-default-and-now-cuts-in-front-of-the-current-job
  (let [plain (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)")
        now (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)" "--now")]
    (is (= :submit (get-in plain [:request :op])))
    (is (= {:op :submit} (select-keys (:request plain) [:op :next? :exclusive?])) "a plain submit appends")
    (is (= :interrupt (get-in now [:request :op])))
    (is (= '(jobs.time.wait-for-light) (get-in now [:request :spec]))))
  (doseq [[pattern args] [[#"unknown operation" ["interrupt" "(jobs.time.wait-for-light)"]]
                          [#"--now and --next" ["submit" "(jobs.time.wait-for-light)" "--now" "--next"]]
                          [#"--now requires submit" ["list" "--now"]]
                          [#"--now requires submit" ["cancel" "j1" "--now"]]]]
    (is (re-find pattern (str (:error (apply request "Bob" "--world" "w" args)))) (pr-str args))))

(deftest wait-and-its-timeout-belong-to-submit
  (is (= {:timeout "5m"} (select-keys (:wait (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)" "--wait" "--timeout" "5m")) [:timeout])))
  (is (some? (:wait (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)" "--now" "--wait"))))
  (is (nil? (:wait (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)"))))
  (doseq [[pattern args] [[#"--wait requires submit" ["cancel" "j1" "--wait"]]
                          [#"--timeout requires --wait" ["submit" "(jobs.time.wait-for-light)" "--timeout" "5s"]]
                          [#"--timeout must be" ["submit" "(jobs.time.wait-for-light)" "--wait" "--timeout" "forever"]]]]
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

(deftest survive-death-is-a-submission-option
  (let [kept (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)" "--survive-death")
        plain (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)")]
    (is (true? (get-in kept [:request :survive-death?])))
    (is (not (contains? (:request plain) :survive-death?)))
    (is (string? (:error (request "Bob" "--world" "w" "list" "--survive-death"))))))

(deftest a-standing-repeat-job-survives-death-by-default
  (let [kept (request "Bob" "--world" "w" "submit" "(repeat (jobs.farm.tend {}))")
        one-shot (request "Bob" "--world" "w" "submit" "(seq (jobs.time.wait-for-light))")]
    (is (true? (get-in kept [:request :survive-death?])))
    (is (not (contains? (:request one-shot) :survive-death?)))))

(deftest cancel-all-has-no-job-argument-and-front-and-exclusive-are-submission-options
  (let [cancel (request "Bob" "--world" "w" "cancel-all" "--request-id" "all")
        exclusive (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)" "--exclusive")
        front (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)" "--next")
        plain (request "Bob" "--world" "w" "submit" "(jobs.time.wait-for-light)")]
    (is (= :cancel-all (get-in cancel [:request :op])))
    (is (= "all" (get-in cancel [:request :request-id])))
    (is (not (contains? (:request cancel) :id)))
    (is (not (contains? (:request cancel) :spec)))
    (is (true? (:mutating cancel)))
    (is (= :exclusive (get-in exclusive [:request :op])))
    (is (true? (get-in front [:request :next?])))
    (is (re-find #":next\? true" (data/write-edn (:request front))))
    (is (not (contains? (:request plain) :next?))))
  (doseq [args [["cancel-all" "j1"] ["cancel-all" "--exclusive"] ["list" "--exclusive"] ["show" "j1" "--exclusive"] ["retry" "j1" "--exclusive"]
                ["submit" "(jobs.x {})" "--exclusive" "--next"] ["submit" "(jobs.x {})" "--exclusive" "--now"] ["submit" "(jobs.x {})" "--exclusive" "--survive-death"]]]
    (is (string? (:error (apply request "Bob" "--world" "w" args))) (pr-str args)))
  (doseq [args [["list" "--next"] ["show" "j1" "--next"]
                ["cancel-all" "--next"] ["cancel" "j1" "--next"] ["retry" "j1" "--next"]]]
    (is (re-find #"--next requires submit" (:error (apply request "Bob" "--world" "w" args))) (pr-str args))))

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
      (-> (run-main! state ["submit" "(seq (jobs.time.wait-for-light) (jobs.movement.go-to {:pos {:x -1 :y 64 :z 2}}))"
                            "--survive-death" "--next" "--request-id" "held"] engine)
          (.then (fn [first-run]
                   (is (= 0 (:code first-run)))
                   (is (= {:ok true} (data/read-edn (:out first-run))))
                   (let [posts (filterv #(= "POST" (:method %)) (:seen first-run))
                         sent (data/read-edn (:body (first posts)))]
                     (is (= 1 (count posts)))
                     (is (= "/jobs" (:path (first posts))))
                     (is (true? (:survive-death? sent)))
                     (is (true? (:next? sent)))
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
          (.then (fn [_] (run-main! state ["show" "j4"]
                                    (fn [_] {:text "{:id \"j4\" :name \"get-kit\" :status :stopped :finished? true :ended-at 5 :result {:reason :no-source :why \"no source\"}}"}))))
          (.then (fn [{:keys [out]}]
                   (is (= {:id "j4" :name "get-kit" :status :stopped :finished? true :ended-at 5 :result {:reason :no-source :why "no source"}}
                          (data/read-edn out)))))
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
  (merge {:seq n :generation-id "g" :time-ms (js/Date.now) :source source :kind kind :context context} extra))

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
                 (= "/job?id=j7" path) {:id "j7" :name "jobs.blocks.place" :status :queued
                                        :waiting {:reason :unreachable :why :exhausted}}
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
                     (is (= {:id "j7"} (:job result)) "no submit-time :queued next to the wake")
                     (is (= [:job-finished "j7" :completed] [(:wake wait) (:job wait) (:result wait)]))
                     (is (= [{:event :arrived :data {:pos [1 64 2]}}] (:events wait)) "the job's own events")
                     (is (= {:reflexes 1 :picked-up 1} (get-in wait [:summary :counts])) "and what else happened meanwhile")
                     (is (= {:event :fired :reflex :hostile-near :message "hostile-near → jobs.survival.respond-to-hostile"}
                            (first (get-in wait [:summary :items]))))
                     (is (not (contains? result :follow)) "nothing left to follow"))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(def decline-text "plant declined: every target is refused by plan jizo-crop-field")

(deftest submit-wait-prints-the-decline-text-of-a-job-that-ends-or-waits-declined
  (let [ended {:job-id "j7" :chain ["j7"]}
        ends (fn [kind data] (event-of 2 :job kind ended {:message decline-text :attention :notice :data data}))
        waits (event-of 2 :job :farm.plant.declined ended {:message decline-text :attention :notice :data {:reason :refused :plans ["jizo-crop-field"]}})]
    (async done
      (-> (js/Promise.all
            (clj->js
              (for [event [(ends :stopped {:status :stopped :kind :nothing-possible :reason :refused})
                           (ends :stopped {:status :stopped :reason :yielded})
                           waits]]
                (let [state (state-dir)
                      events (atom [(event-of 1 :system :started {})])]
                  (js/setTimeout #(swap! events conj event) 60)
                  (-> (run-main! state ["submit" "(jobs.farm.plant)" "--wait" "--timeout" "3s"] (waiting-engine events))
                      (.then (fn [{:keys [out]}]
                               (.rmSync fs state #js {:recursive true :force true})
                               out)))))))
          (.then (fn [outs]
                   (doseq [out outs]
                     (is (str/includes? out decline-text) out))))
          (.then (fn [_] (done)))))))

(deftest a-wait-that-ends-before-the-job-says-how-to-go-on-waiting
  (let [state (state-dir)
        events (atom [(event-of 1 :system :started {})])]
    (async done
      (-> (run-main! state ["submit" "(jobs.time.wait-for-light)" "--wait" "--timeout" "100ms"] (waiting-engine events))
          (.then (fn [{:keys [code out]}]
                   (let [result (data/read-edn out)]
                     (is (= 1 code) "a script can tell the job did not end")
                     (is (= :timeout (get-in result [:wait :wake])))
                     (is (= "./bin/observe --wait --watch j7" (:follow result)))
                     (is (= {:reason :unreachable :why :exhausted} (get-in result [:now :waiting])) "says what holds the job"))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-wait-woken-by-body-idle-says-the-job-is-not-lost
  (let [wake {:wake :idle :jobs [{:job "j7" :name "jobs.blocks.dig"}
                                 {:job "j8" :name "jobs.survival.respond-to-hostile" :holding {:reason "hiding"}}]}
        note (:note (jobs/wait-result {:ok true} "j7" wake))]
    (is (str/includes? note "j7 is still queued, running or cut by a reflex, not lost"))
    (is (str/includes? note "jobs.survival.respond-to-hostile"))
    (is (str/includes? note "./bin/jobs show j7"))))

(deftest a-wait-woken-by-a-notice-says-what-the-job-is-doing-now
  (let [wake {:wake :notice}
        note-of #(:note (jobs/wait-result {:ok true} "j7" wake %))]
    (is (str/includes? (note-of {:status :running}) "j7 is running, not lost"))
    (is (not (str/includes? (note-of {:status :running}) "queued")))
    (is (str/includes? (note-of {:status :queued}) "j7 is queued"))))

(deftest a-refused-submit-does-not-wait
  (let [state (state-dir)]
    (async done
      (-> (run-main! state ["submit" "(jobs.time.wait-for-light)" "--wait"]
                     (fn [{:keys [method path]}]
                       {:status (if (= "POST" method) 400 200)
                        :text (if (= "/snapshot" path) "{:generation-id \"g\"}" "{:ok false :reason :bad-spec}")}))
          (.then (fn [{:keys [code out seen]}]
                   (is (= 1 code))
                   (is (= {:ok false :reason :bad-spec} (data/read-edn out)))
                   (is (= ["GET" "GET" "POST"] (mapv :method seen)) "the job list, the snapshot, the post; no event reads")))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(defn reflex-engine [current]
  (fn [{:keys [path]}]
    {:text (cond (= "/snapshot" path) "{:generation-id \"generation\"}"
                 (= "/status" path) (str "{:mode :scheduled :current " (pr-str current) "}")
                 :else "{:ok true :job {:id \"j2\" :status :queued}}")}))

(deftest a-now-submit-queued-behind-a-reflex-says-which-one-and-until-when
  (let [state (state-dir)
        night {:id "j9" :name "jobs.survival.night" :status :running :reflex :night}]
    (async done
      (-> (run-main! state ["submit" "(jobs.x {})" "--now"] (reflex-engine night))
          (.then (fn [{:keys [out]}]
                   (let [hint (str (:hint (data/read-edn out)))]
                     (is (re-find #"queued behind reflex :night" hint) out)
                     (is (re-find #"j9" hint) out)
                     (is (re-find #"dawn" hint) out))))
          (.then (fn [_] (run-main! state ["submit" "(jobs.x {})" "--now"] (reflex-engine {:id "j9" :name "jobs.survival.hungry" :status :running :reflex :hungry}))))
          (.then (fn [{:keys [out]}]
                   (let [hint (str (:hint (data/read-edn out)))]
                     (is (re-find #"queued behind reflex :hungry" hint) out)
                     (is (not (re-find #"dawn" hint)) out))))
          (.then (fn [_] (run-main! state ["submit" "(jobs.x {})" "--now"] (reflex-engine {:id "j3" :name "jobs.x" :status :running}))))
          (.then (fn [{:keys [out]}] (is (not (re-find #"reflex" (str (:hint (data/read-edn out))))) out)))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest usage-shows-the-submit-form-where-to-find-args-and-now
  (is (re-find #"\(jobs\.movement\.go-to \{:pos" jobs/usage))
  (is (re-find #"observe\.mjs.*catalog job" jobs/usage))
  (is (re-find #"queues behind a running reflex" jobs/usage))
  (is (re-find #"--exclusive" jobs/usage)))

(deftest submit-usage-and-argument-errors-show-the-list-form
  (is (str/includes? (jobs/usage-for "./bin/jobs") "submit '(jobs."))
  (is (not (str/includes? jobs/usage "<EDN-spec>")))
  (is (re-find #"\(jobs\.[a-z.-]+ \{" (str (jobs/request-for ["B" "--world" "w" "submit" "jobs.a.b" "{}"])))))

(deftest failure-for-keeps-the-transport-reason
  (doseq [[code reason] [["ENOENT" :no-running-body] ["ETIMEDOUT" :timeout] ["ERESPONSETOOLARGE" :response-too-large]
                         ["EACCES" :socket-access-denied] ["EPIPE" :transport-error]]]
    (is (= reason (:reason (jobs/failure-for {} (let [e (js/Error. "x")] (aset e "code" code) e)))) code)))

(deftest a-mutation-to-a-body-that-is-not-running-says-nothing-was-queued
  (let [e (let [e (js/Error. "x")] (aset e "code" "ECONNREFUSED") e)
        v (jobs/failure-for {:mutating true :request {:request-id "abc-1"}} e)]
    (is (= :not-sent (:confirmation v)))
    (is (not (re-find #"request-id" (:message v))))
    (is (re-find #"nothing was queued" (:message v)))))

(deftest an-uncertain-mutation-names-the-command-to-repeat
  (let [e (let [e (js/Error. "x")] (aset e "code" "ECONNRESET") e)
        m (:message (jobs/failure-for {:mutating true :request {:request-id "abc-1"}} e))]
    (is (re-find #"--request-id abc-1" m) m)
    (is (re-find #"same command" m) m)))

(deftest an-uncertain-timeout-says-the-engine-did-not-answer
  (let [e (let [e (js/Error. "x")] (aset e "code" "ETIMEDOUT") e)
        m (:message (jobs/failure-for {:mutating true :request {:request-id "abc-1"}} e))]
    (is (re-find #"did not answer in time" m) m)
    (is (re-find #"--request-id abc-1" m) m)))

(defn listing-engine [items]
  (fn [{:keys [method path]}]
    {:text (cond (= "/snapshot" path) "{:generation-id \"generation\"}"
                 (and (= "GET" method) (str/starts-with? path "/jobs?")) (str "{:total " (count items) " :items " (pr-str items) "}")
                 :else "{:ok true :job {:id \"j9\" :status :queued}}")}))

(def mine-spec "(jobs.gather.mine {:target \"iron_ore\"})")

(defn posted? [{:keys [seen]}] (boolean (some #(= "POST" (:method %)) seen)))

(deftest a-submit-clashing-with-a-live-job-of-the-same-name-is-held-back
  (let [state (state-dir)]
    (async done
      (-> (js/Promise.resolve nil)
          (.then (fn [_] (run-main! state ["submit" mine-spec] (listing-engine [{:id "j1442" :name "jobs.gather.mine" :status :running}]))))
          (.then (fn [{:keys [code out] :as result}]
                   (let [answer (data/read-edn out)]
                     (is (= 1 code) out)
                     (is (false? (posted? result)) "nothing was submitted")
                     (is (= :job-clash (:reason answer)) out)
                     (is (= [{:id "j1442" :name "jobs.gather.mine" :status :running}] (:clashes answer)) out)
                     (is (str/includes? (:hint answer) "./bin/observe --wait --watch j1442") out)
                     (is (str/includes? (:hint answer) "./bin/jobs cancel j1442") out)
                     (is (re-find #"--alongside" (str (:hint answer))) out)
                     (is (re-find #"cancel j1442" (str (:hint answer))) out))))
          (.then (fn [_] (run-main! state ["submit" mine-spec] (listing-engine [{:id "j7" :name "jobs.gather.mine" :status :queued}]))))
          (.then (fn [{:keys [out]}] (is (= :job-clash (:reason (data/read-edn out))) "a queued twin clashes too")))
          (.then (fn [_] (run-main! state ["submit" mine-spec "--now" "--wait"] (listing-engine [{:id "j7" :name "jobs.gather.mine" :status :resuming}]))))
          (.then (fn [{:keys [out]}] (is (= :job-clash (:reason (data/read-edn out))) "--now and --wait do not skip the guard")))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-single-cell-dig-queues-behind-another-dig
  (let [state (state-dir)]
    (async done
      (-> (js/Promise.resolve nil)
          (.then (fn [_] (run-main! state ["submit" "(jobs.blocks.dig {:pos [1 64 2]})"] (listing-engine [{:id "j5" :name "jobs.blocks.dig" :status :queued}]))))
          (.then (fn [result] (is (posted? result) "a second one-cell dig is not held back")))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-memory-job-queues-behind-another-of-its-name
  (let [state (state-dir)]
    (async done
      (-> (js/Promise.resolve nil)
          (.then (fn [_] (run-main! state ["submit" "(jobs.memory.forget-place {:name \"bed\"})"] (listing-engine [{:id "j5" :name "jobs.memory.forget-place" :status :queued}]))))
          (.then (fn [result] (is (posted? result) "forgetting another place is not held back")))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-submit-clashing-with-another-digging-job-is-held-back-and-names-it
  (let [state (state-dir)]
    (async done
      (-> (js/Promise.resolve nil)
          (.then (fn [_] (run-main! state ["submit" "(jobs.access.stair {:dir :down})"] (listing-engine [{:id "j5" :name "jobs.strip.corridor" :label "corridor 12" :status :running}]))))
          (.then (fn [{:keys [out] :as result}]
                   (let [answer (data/read-edn out)]
                     (is (false? (posted? result)))
                     (is (= :job-clash (:reason answer)) out)
                     (is (= [{:id "j5" :name "jobs.strip.corridor" :label "corridor 12" :status :running}] (:clashes answer)) out)
                     (is (re-find #"jobs.strip.corridor \(corridor 12\)" (str (:hint answer))) out)
                     (is (re-find #"jobs.access.stair" (str (:hint answer))) out))))
          (.then (fn [_] (run-main! state ["submit" "(jobs.gather.chop {})"] (listing-engine [{:id "j5" :name "jobs.access.tunnel" :status :running}]))))
          (.then (fn [result] (is (posted? result) "a non-digging job does not clash with a dig")))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(defn flaky-listing-engine
  "listing-engine whose first `failures` job-list reads fail (a busy engine)."
  [failures items]
  (let [left (atom failures)
        ok (listing-engine items)]
    (fn [call]
      (if (and (= "GET" (:method call)) (str/starts-with? (:path call) "/jobs?") (pos? @left))
        (do (swap! left dec) {:error "ETIMEDOUT"})
        (ok call)))))

(defn flaky-post-engine
  "engine whose first `failures` job POSTs time out (a busy engine; the job is queued each time)."
  [failures]
  (let [left (atom failures)]
    (fn [{:keys [method] :as call}]
      (if (and (= "POST" method) (pos? @left))
        (do (swap! left dec) {:error "ETIMEDOUT"})
        (engine call)))))

(deftest a-submit-that-times-out-is-sent-again-with-the-same-request-id
  (let [state (state-dir)
        spec "(jobs.time.wait-for-light)"]
    (async done
      (-> (run-main! state ["submit" spec] (flaky-post-engine 2))
          (.then (fn [{:keys [code seen]}]
                   (let [posts (filterv #(= "POST" (:method %)) seen)]
                     (is (= 0 code) "the third try answers")
                     (is (= 3 (count posts)))
                     (is (= 1 (count (set (map :body posts)))) "every try carries the one request ID"))))
          (.then (fn [_] (run-main! state ["submit" spec] (flaky-post-engine 9))))
          (.then (fn [{:keys [code out]}]
                   (is (= 2 code))
                   (is (= :unknown (:confirmation (data/read-edn out))) out)))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-busy-engine-does-not-let-a-twin-submit-through-the-guard
  (let [state (state-dir)
        twin [{:id "j1" :name "jobs.gather.mine" :status :running}]]
    (async done
      (-> (js/Promise.resolve nil)
          (.then (fn [_] (run-main! state ["submit" mine-spec] (flaky-listing-engine 2 twin))))
          (.then (fn [{:keys [out] :as result}]
                   (is (= :job-clash (:reason (data/read-edn out))) "the list read is retried")
                   (is (false? (posted? result)))))
          (.then (fn [_] (run-main! state ["submit" mine-spec] (flaky-listing-engine 9 twin))))
          (.then (fn [{:keys [out code] :as result}]
                   (is (= :clash-check-failed (:reason (data/read-edn out))) out)
                   (is (= 1 code))
                   (is (false? (posted? result)) "an unreadable list queues nothing")))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest alongside-or-a-different-or-standing-job-goes-through
  (let [state (state-dir)
        twin {:id "j1" :name "jobs.gather.mine" :status :running}]
    (async done
      (-> (js/Promise.resolve nil)
          (.then (fn [_] (run-main! state ["submit" mine-spec "--alongside"] (listing-engine [twin]))))
          (.then (fn [result] (is (posted? result) "--alongside overrides")))
          (.then (fn [_] (run-main! state ["submit" "(jobs.gather.chop {})"] (listing-engine [twin]))))
          (.then (fn [result] (is (posted? result) "another job name")))
          (.then (fn [_] (run-main! state ["submit" mine-spec] (listing-engine [(assoc twin :survive-death? true :name "(repeat jobs.gather.mine)")]))))
          (.then (fn [result] (is (posted? result) "a standing repeat of the name is not a twin")))
          (.then (fn [_] (run-main! state ["submit" mine-spec] (listing-engine [(assoc twin :status :failed)]))))
          (.then (fn [result] (is (posted? result) "a failed job is not live")))
          (.then (fn [_] (run-main! state ["submit" "(repeat (jobs.gather.mine {}))"] (listing-engine [twin]))))
          (.then (fn [result] (is (posted? result) "a standing submit is never held")))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest --alongside-belongs-to-submit-and-usage-names-the-guard
  (is (re-find #"--alongside requires submit" (str (:error (request "Bob" "--world" "w" "list" "--alongside")))))
  (is (= "jobs.gather.mine" (:clash-name (request "Bob" "--world" "w" "submit" mine-spec))))
  (is (nil? (:clash-name (request "Bob" "--world" "w" "submit" mine-spec "--alongside"))))
  (is (re-find #"--alongside" jobs/usage))
  (is (re-find #"same job name" jobs/usage)))

(deftest stamp-from-adds-the-submit-position-only-to-jobs-that-take-it-and-did-not-say-otherwise
  (let [pos {:x 1.5 :y 64 :z -3 :yaw 2}]
    (doseq [[spec takes? want] [['(jobs.access.stair {:dir :up}) true '(jobs.access.stair {:dir :up :submitted-at {:x 1.5 :y 64 :z -3}})]
                                ['(jobs.access.stair) true '(jobs.access.stair {:submitted-at {:x 1.5 :y 64 :z -3}})]
                                ['(jobs.access.stair {:dir :up}) false '(jobs.access.stair {:dir :up})]
                                ['(jobs.access.stair {:here true}) true '(jobs.access.stair {:here true})]
                                ['(jobs.access.stair {:submitted-at {:x 0 :y 0 :z 0}}) true '(jobs.access.stair {:submitted-at {:x 0 :y 0 :z 0}})]
                                ['(repeat (jobs.farm.tend)) true '(repeat (jobs.farm.tend))]]]
      (is (= want (jobs/stamp-from spec pos takes?)) (pr-str spec takes?)))))

(deftest the-jobs-that-take-from-are-named-not-read-from-the-truncated-catalog
  (doseq [spec ['(jobs.access.stair {:dir :down}) '(jobs.access.bridge) '(jobs.access.pillar) '(jobs.gather.mine {:block "coal_ore"})]]
    (is (true? (jobs/takes-from? spec)) (pr-str spec)))
  (doseq [spec ['(jobs.movement.go-to {:pos [1 2 3]}) '(repeat (jobs.access.stair)) '(jobs.access.tunnel {})]]
    (is (false? (jobs/takes-from? spec)) (pr-str spec))))

(deftest a-stair-submit-posts-from-the-snapshot-position
  (let [state (state-dir)]
    (async done
      (-> (run-main! state ["submit" "(jobs.access.stair {:dir :down})"]
                     (fn [{:keys [path]}] {:text (if (= "/snapshot" path)
                                                   "{:generation-id \"g\" :position {:x -528.3 :y 64 :z 350.5}}"
                                                   "{:ok true :job {:id \"j5\" :status :queued}}")}))
          (.then (fn [{:keys [seen]}]
                   (is (= '(jobs.access.stair {:dir :down :submitted-at {:x -528.3 :y 64 :z 350.5}})
                          (:spec (data/read-edn (:body (last (filterv #(= "POST" (:method %)) seen)))))) (pr-str seen))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(defn vitals-engine
  "waiting-engine whose /status health falls from 20 to 14 between its first and later answers."
  [events exclusive]
  (let [calls (atom 0)]
    (fn [req]
      (if (= "/status" (:path req))
        (let [n (swap! calls inc)]
          {:text (pr-str (cond-> {:mode :scheduled :health (if (< n 3) 20 14) :food 18 :position {:x 0 :y 64 :z 0}}
                           exclusive (assoc :exclusive {:job "j1" :name "wait"})))})
        ((waiting-engine events) req)))))

(deftest a-waited-submit-reports-health-lost-and-the-reflexes-off-reminder
  (let [state (state-dir)
        events (atom [(event-of 1 :system :started {})])]
    (async done
      (-> (run-main! state ["submit" "(jobs.time.wait-for-light)" "--wait" "--timeout" "100ms"] (vitals-engine events true))
          (.then (fn [{:keys [out]}]
                   (let [v (:vitals (data/read-edn out))]
                     (is (= "lost 6 health during the wait (20 -> 14)" (:hurt v)) out)
                     (is (= 14 (:health v)))
                     (is (re-find #"reflexes are off" (str (:reflexes-off v)))))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-waited-submit-woken-by-a-hit-says-so
  (let [state (state-dir)
        events (atom [(event-of 1 :system :started {})])]
    (js/setTimeout #(swap! events conj (event-of 2 :body :hurt {} {:data {:health 14 :causes ["zombie"]}})) 150)
    (async done
      (-> (run-main! state ["submit" "(jobs.time.wait-for-light)" "--wait" "--timeout" "5s"] (vitals-engine events false))
          (.then (fn [{:keys [out]}]
                   (is (= :danger (get-in (data/read-edn out) [:wait :wake])) out)))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))

(deftest a-queued-job-behind-a-log-out-says-when-the-body-is-back
  (let [back-at (+ (js/Date.now) 60000)
        hint (:hint (jobs/with-hint {} {:job {:status :queued}} {:offline {:by :night :back-at back-at}}))]
    (is (re-find #"back about \d\d:\d\d, in \d+ s" hint) hint)))
