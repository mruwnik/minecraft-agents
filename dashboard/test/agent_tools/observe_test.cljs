(ns agent-tools.observe-test
  (:require [cljs.test :refer [deftest is are async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.job-results :as job-results]
            [agent-tools.observe :as observe]
            [agent-tools.observe.lock :as observe-lock]
            [agent-tools.observe.request :as observe-request]
            [agent-tools.observe.status :as observe-status]
            [agent-tools.world-data :as data]
            [engine.expr :as expr]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn request [& args] (observe-request/request-for (vec args)))

(defn finish!
  "Report an unexpected rejection as a failure, then end the async test."
  [done promise]
  (-> promise
      (.catch (fn [error] (is false (str "rejected: " error))))
      (.then (fn [_] (done)))))

(defn rejects
  "A promise of the error a promise rejects with (or of ::resolved)."
  [promise]
  (-> promise (.then (fn [_] ::resolved)) (.catch identity)))

(defn series
  "Run promise-returning thunks one after the other: a promise of the vector of their results."
  [thunks]
  (reduce (fn [acc thunk] (.then acc (fn [done] (.then (thunk) (fn [value] (conj done value))))))
          (js/Promise.resolve []) thunks))

;; Requests

(def socket-of #(.join path % "worlds" "w" "agents" "ProbeBody" "engine" "events.sock"))

(def unsupported-cases
  [[{:status 404 :content-type "application/edn; charset=utf-8" :text "{:ok false, :reason :not-found}"} true]
   [{:status 404 :content-type "application/edn" :text "{:ok false, :reason :job-not-found}"} false]
   [{:status 404 :content-type "application/edn" :text "{:extra {:reason :not-found}}"} false]
   [{:status 404 :content-type "text/plain" :text "not found"} false]])

(deftest observe-distinguishes-an-older-engine-missing-the-projection-routes
  (doseq [[response expected] unsupported-cases]
    (is (= expected (observe-request/unsupported-route? response)) (pr-str response))))

(deftest legacy-observe-guidance-preserves-a-nondefault-state-root
  (let [notice (observe-request/legacy-notice (request "--world" "w" "ProbeBody" "--state" "/tmp/custom state"))]
    (is (re-find #":reason :observe-unavailable" notice))
    (is (re-find #":action :restart-with-current-build" notice))
    (is (re-find #":fallback \{:op :status :raw true :world \"w\" :state \"/tmp/custom state\"\}" notice)))
  (is (nil? (re-find #":fallback" (observe-request/legacy-notice (request "--world" "w" "ProbeBody" "inventory"))))))

(deftest observe-defaults-to-bounded-status-and-targets-the-engine-event-socket
  (is (= {:agent "ProbeBody" :world "w" :state (.resolve path "/tmp/state")
          :socket-path (socket-of "/tmp/state") :path "/status"}
         (request "--world" "w" "ProbeBody" "--state" "/tmp/state"))))

(deftest observe-without-a-valid-world-is-an-error-naming-the-flag
  (is (re-find #"missing --world <world>" (:error (request "ProbeBody"))))
  (is (re-find #"world" (:error (request "ProbeBody" "--world" "a/b")))))

(deftest observe-supports-detail-and-on-demand-capability-lookups
  (doseq [[argv expected]
          [[["status" "--limit" "4"] "/status?limit=4"]
           [["status" "--raw"] "/snapshot"]
           [["job" "j12"] "/job?id=j12"]
           [["catalog" "job" "jobs.movement.go-to"] "/catalog?kind=job&name=jobs.movement.go-to"]
           [["catalog" "trigger" "health-low"] "/catalog?kind=trigger&name=health-low"]
           [["catalog" "jobs" "jobs.movement." "--limit" "5" "--offset" "10"]
            "/catalog?kind=jobs&prefix=jobs.movement.&limit=5&offset=10"]
           [["catalog" "triggers"] "/catalog?kind=triggers&prefix=&limit=20&offset=0"]]]
    (is (= expected (:path (apply request "--world" "w" "ProbeBody" argv))) (pr-str argv))))

(deftest inventory-and-equipment-are-read-only-observe-modes-with-explicit-raw-or-slot-detail
  (is (= {:agent "ProbeBody" :world "w" :state (.resolve path "/tmp/state")
          :inventory-mode :inventory :slots false :raw false
          :socket-path (socket-of "/tmp/state") :path "/inventory"}
         (request "--world" "w" "ProbeBody" "inventory" "--state" "/tmp/state")))
  (is (true? (:slots (request "--world" "w" "ProbeBody" "inventory" "--slots"))))
  (is (= :equipment (:inventory-mode (request "--world" "w" "ProbeBody" "equipment" "--raw"))))
  (is (true? (:raw (request "--world" "w" "ProbeBody" "equipment" "--raw"))))
  (is (nil? (:inventory-mode (request "--world" "w" "ProbeBody" "status"))))
  (doseq [[argv pattern] [[["equipment" "--slots"] #"only valid for inventory"]
                          [["inventory" "bread"] #"takes no positional arguments"]
                          [["inventory" "--raw" "--slots"] #"redundant with --raw"]
                          [["inventory" "--limit" "2"] #"not valid for inventory"]]]
    (is (re-find pattern (:error (apply request "--world" "w" "ProbeBody" argv))) (pr-str argv))))

(deftest observe-rejects-malformed-names-and-limits-before-connecting
  (doseq [[argv pattern] [[["bad/name"] #"body name"]
                          [["ProbeBody" "status" "--limit" "100"] #"1 to 32"]
                          [["ProbeBody" "status" "--offset" "1"] #"only valid for catalog lists"]
                          [["ProbeBody" "job" "j12" "--offset" "1"] #"only valid for status and catalog lists"]
                          [["ProbeBody" "catalog" "job" "jobs.x.y" "--limit" "2"] #"does not accept"]
                          [["ProbeBody" "catalog" "job" "(eval foo)"] #"exact jobs namespace"]
                          [["ProbeBody" "job" "j12" "extra"] #"one job ID"]
                          [["ProbeBody" "frobnicate"] #"unknown operation"]]]
    (is (re-find pattern (:error (apply request "--world" "w" argv))) (pr-str argv))))

;; The CLI over a fake socket

(defn state-dir [] (.mkdtempSync fs (.join path (.tmpdir os) "observe-cli-")))

(defn run-main! [argv handler]
  (let [state (state-dir)
        [request-fn seen] (fake/request-fn handler)
        lines (atom [])]
    (-> (observe/main! (into ["ProbeBody" "--world" "w" "--state" state] argv)
                       {:request-fn request-fn :output #(swap! lines conj %)})
        (.then (fn [code]
                 (.rmSync fs state #js {:recursive true :force true})
                 {:code code :out (apply str @lines) :seen @seen})))))

(def inventory-reply
  {:text "{:ok true :inventory [{:name \"bread\" :count 5 :slot 9} {:name \"iron_pickaxe\" :count 1 :slot 37}] :equipment {:head {:name \"iron_helmet\" :count 1 :durability 140}}}"})

(def head {:head {:name "iron_helmet" :count 1 :durability 140}})

(deftest inventory-and-equipment-emit-aggregate-raw-and-optional-slot-views-from-the-read-socket
  (async done
    (finish! done
             (-> (series [#(run-main! ["inventory"] (constantly inventory-reply))
                          #(run-main! ["inventory" "--slots"] (constantly inventory-reply))
                          #(run-main! ["equipment"] (constantly inventory-reply))
                          #(run-main! ["equipment" "--raw"] (constantly inventory-reply))
                          #(run-main! ["inventory" "--raw"] (constantly inventory-reply))])
                 (.then (fn [[summary slots equipment raw-equipment raw]]
                          (is (= 0 (:code summary)))
                          (is (= {:total-items 6 :kinds 2 :counts {"bread" 5 "iron_pickaxe" 1} :equipment head}
                                 (data/read-edn (:out summary))))
                          (is (= [{:name "bread" :count 5 :slot 9} {:name "iron_pickaxe" :count 1 :slot 37}]
                                 (:slots (data/read-edn (:out slots)))))
                          (is (= {:equipment head} (data/read-edn (:out equipment))))
                          (is (= {:equipment head} (data/read-edn (:out raw-equipment))))
                          (is (= {:name "bread" :count 5 :slot 9} (first (:inventory (data/read-edn (:out raw))))))
                          (is (= head (:equipment (data/read-edn (:out raw)))))
                          (is (= ["/inventory"] (map :path (:seen summary))))))))))

(def outcome-cases
  [["default status is compact" ["status"] {:text "{:mode :scheduled :current nil :health 20 :food 18}"} 0
    "{:mode :scheduled :idle true :health 20 :food 18}\n"]
   ["a verbose status is passed through" ["status" "--verbose"] {:text "{:mode :scheduled}"} 0 "{:mode :scheduled}\n"]
   ["a non-success status is passed through with exit 1" ["job" "j1"] {:status 400 :text "{:ok false :reason :bad-id}"} 1
    "{:ok false :reason :bad-id}\n"]
   ["a refused inventory exits 1" ["inventory"] {:text "{:ok false :reason :nope}"} 1 "{:ok false :reason :nope}\n"]
   ["an older engine gets the restart notice" ["status"] {:status 404 :text "{:ok false :reason :not-found}"} 2 nil]
   ["a non-EDN answer is a bad response" ["status"] {:content-type "text/plain" :text "hi"} 1
    "{:ok false :reason :bad-response :detail :unexpected-content-type}\n"]
   ["a refused connection is no running body" ["status"] {:error "ECONNREFUSED"} 2 "{:ok false :reason :no-running-body :body \"ProbeBody\"}\n"]
   ["a missing socket is no running body" ["status"] {:error "ENOENT"} 2 "{:ok false :reason :no-running-body :body \"ProbeBody\"}\n"]
   ["a denied socket" ["status"] {:error "EACCES"} 2 "{:ok false :reason :socket-access-denied :body \"ProbeBody\"}\n"]
   ["any other failure is a transport error" ["status"] {:error "EPIPE"} 2 "{:ok false :reason :transport-error :body \"ProbeBody\"}\n"]])

(deftest main-reports-each-outcome-with-its-exit-code-and-reason
  (async done
    (finish! done
             (-> (series (map (fn [[_ argv reply]] #(run-main! argv (constantly reply))) outcome-cases))
                 (.then (fn [results]
                          (doseq [[[label _ _ code out] result] (map vector outcome-cases results)]
                            (is (= code (:code result)) label)
                            (is (= (or out (:out result)) (:out result)) label))
                          (is (re-find #":reason :observe-unavailable" (:out (nth results 4))))))))))

(deftest observe-socket-reads-use-the-observe-label-and-a-finite-deadline
  (async done
    (finish! done
             (-> (rejects (observe/get! "/unused" "/status" {:timeout-ms 5 :request-fn (fn [_ _] (fake/pending-request))}))
                 (.then (fn [error]
                          (is (= "ETIMEDOUT" (.-code error)))
                          (is (= "observe request exceeded 5 ms" (.-message error)))))))))

(deftest a-bad-request-prints-the-usage-and-exits-2
  (async done
    (finish! done
             (-> (observe/main! ["ProbeBody"] {:output (fn [_])})
                 (.then (fn [code] (is (= 2 code))))))))

;; Compact status

(def compact-cases
  [["omits metadata and empty collections, rounds the position, keeps keywords"
    "{:body \"Probe\" :generation-id \"uuid\" :cursor {} :mode :scheduled :current nil :position {:x 1.254 :y 70 :z -9.666} :health 20 :food 20 :jobs {:total 0 :items []} :failed {:total 0} :outstanding {:total 0}}"
    "{:mode :scheduled :idle true :pos [1.3 70 -9.7] :health 20 :food 20}"]
   ["lists queued jobs apart from the current one"
    "{:mode :scheduled :current {:id \"j1\" :name \"gather\" :status :running} :jobs {:total 3 :more? true :items [{:id \"j1\" :name \"gather\" :status :running} {:id \"j2\" :name \"smelt\" :status :queued}]}}"
    "{:mode :scheduled :current {:id \"j1\" :name \"gather\" :status :running} :jobs {:total 2 :items [{:id \"j2\" :name \"smelt\" :status :queued}] :more? true}}"]
   ["reports failures and attention requests briefly"
    "{:mode :scheduled :current nil :failed {:total 1 :items [{:id \"j3\" :error \"boom\"}]} :outstanding {:total 1 :items [{:request-id \"r1\" :job-id \"j3\" :reason :blocked :message \"help\"}]}}"
    "{:mode :scheduled :idle true :failed {:total 1 :items [{:id \"j3\" :error \"boom\"}]} :attention {:total 1 :items [{:id \"r1\" :job \"j3\" :reason :blocked :message \"help\"}]}}"]
   ["shows a recent death with where, cause, pile and time left"
    "{:mode :scheduled :current nil :position {:x 20.5 :y 66 :z 2.5} :died {:pos {:x 50.5 :y 40 :z 3.46} :cause \"skeleton\" :ago-ms 62000 :despawns-in-ms 238000}}"
    "{:mode :scheduled :idle true :pos [20.5 66 2.5] :died {:at [50.5 40 3.5] :cause \"skeleton\" :ago-s 62 :pile-at [50.5 40 3.5] :despawns-in-s 238}}"]
   ["a collected pile drops the pile and the despawn timer"
    "{:mode :scheduled :current nil :died {:pos {:x 1 :y 2 :z 3} :ago-ms 1000 :despawns-in-ms 299000 :recovered :collected}}"
    "{:mode :scheduled :idle true :died {:at [1 2 3] :ago-s 1 :recovered \"collected\"}}"]
   ["a death without a known cause omits it"
    "{:mode :scheduled :current nil :died {:pos {:x 1 :y 2 :z 3} :ago-ms 1000 :despawns-in-ms 299000}}"
    "{:mode :scheduled :idle true :died {:at [1 2 3] :ago-s 1 :pile-at [1 2 3] :despawns-in-s 299}}"]
   ["a queued job shows why it waits"
    "{:mode :scheduled :current nil :jobs {:total 1 :items [{:id \"j2\" :name \"smelt\" :status :queued :waiting {:reason :cooking :ready-at 5}}]}}"
    "{:mode :scheduled :idle true :jobs {:total 1 :items [{:id \"j2\" :name \"smelt\" :status :queued :waiting {:reason :cooking :ready-at 5}}]}}"]
   ["an error answer passes through"
    "{:ok false :reason :nope}"
    "{:ok false :reason :nope}"]])

(deftest compact-status-is-bounded-and-keeps-edn-keywords
  (doseq [[label text expected] compact-cases]
    (is (= expected (data/write-edn (observe-status/compact-status (data/read-edn text)))) label)))

;; Wake classification

(def defaults {:chatter "addressed" :watch [] :danger false :disconnect false})

(defn event [source kind & [data message]]
  (cond-> {:source source :kind kind :data (or data {})} message (assoc :message message)))

(def chat (event :body :chat {:from "Alex"} "Hello Probe!"))

(def classify-cases
  [["addressed chatter wakes" chat defaults :chat]
   ["a longer name does not match" (assoc chat :message "ProbeExtra") defaults nil]
   ["chatter all wakes on banter" (assoc chat :message "banter") (assoc defaults :chatter "all") :chat]
   ["a sender filter applies" chat (assoc defaults :from "Other") nil]
   ["a whisper wakes" (event :body :whisper {:from "Alex"} "hi") defaults :chat]
   ["chatter none suppresses whispers" (event :body :whisper {} "hi") (assoc defaults :chatter "none") nil]
   ["hurt is opt-in" (event :body :hurt) defaults nil]
   ["disconnection is opt-in" (event :body :disconnected) defaults nil]
   ["danger wakes when asked" (event :body :hurt) (assoc defaults :danger true) :danger]
   ["disconnection wakes when asked" (event :body :disconnected) (assoc defaults :disconnect true) :disconnected]
   ["exhausted reconnection always wakes" (event :body :reconnect-failed) defaults :reconnect-failed]
   ["an unwatched job stays quiet" (assoc (event :job :completed) :context {:job-id "j1"}) defaults nil]
   ["a watched job wakes" (assoc (event :job :completed) :context {:job-id "j1"}) (assoc defaults :watch ["j1"]) :job-finished]
   ["a watched job wakes when stopped" (assoc (event :job :stopped) :context {:job-id "j1"}) (assoc defaults :watch ["j1"]) :job-finished]
   ["a watched job wakes when cancelled" (assoc (event :job :cancelled) :context {:job-id "j1"}) (assoc defaults :watch ["j1"]) :job-finished]])

(deftest classify-wakes-only-for-what-was-asked
  (doseq [[label e opts expected] classify-cases]
    (is (= expected (:wake (observe-status/classify e opts "Probe"))) label)))

;; Attention

(def blocked {:job-id "j1" :reason :blocked :updated-at 123
              :event {:kind :blocked :message "No food" :data {:pos {:x 1}}}})

(deftest attention-deduplication-ignores-timestamps-and-position-and-reports-semantic-changes
  (let [first-pass (observe-status/attention-changes {"r" blocked} {})]
    (is (= 1 (count (:changed first-pass))))
    (is (= 0 (count (:changed (observe-status/attention-changes
                               {"r" (-> blocked (assoc :updated-at 456) (assoc-in [:event :data :pos] {:x 2}))}
                               (:seen first-pass))))))
    (is (= 1 (count (:changed (observe-status/attention-changes
                               {"r" (assoc-in blocked [:event :message] "No tools")} (:seen first-pass))))))
    (is (= {} (:seen (observe-status/attention-changes {} (:seen first-pass)))))
    (is (= [{:id "r" :job "j1" :reason :blocked :message "No food"}] (:changed first-pass)))))

(defn pass-sizes
  "How many attention requests each successive pass delivers, until a pass delivers none."
  [requests]
  (->> (iterate #(observe-status/attention-changes requests (:seen %)) {:seen {}})
       rest
       (map (comp count :changed))
       (take-while pos?)
       vec))

(deftest attention-is-delivered-four-at-a-time-until-all-are-seen
  (are [n expected] (= expected (pass-sizes (into {} (map (fn [i] [(str "r" i) {:reason :blocked :event {:message "help"}}])) (range n))))
    10 [4 4 2]
    129 (conj (vec (repeat 32 4)) 1)))

(deftest keyword-keyed-requests-are-reported-by-their-name
  (is (= "r1" (:id (first (:changed (observe-status/attention-changes {:r1 blocked} {})))))))

(deftest too-many-attention-requests-fail-with-a-code
  (let [many (into {} (map (fn [i] [(str "r" i) blocked])) (range 4097))]
    (is (= "EATTENTIONLIMIT" (try (observe-status/attention-changes many {}) nil (catch :default e (.-code e)))))))

;; The signature digests are stored in observer checkpoint files: they must not change.
(def digest-cases
  [[blocked "9542dc93064cfc366b307a0cca64e500308add1cc6df7fa550fc85c1ab7c8a7c"]
   [(data/read-edn "{:job-id \"j7\" :reason :stuck :event {:kind :stuck :message \"caf\\u00e9 \\\"quoted\\\"\\n line\" :data {:time-ms 5 :pos {:x 1.5} :item \"wheat\" :count 3 :tags [:a :b] :ratio 0.25 :zz nil :flag true}}}")
    "a2b482cc7c04eb641969e21870bf94ec89abe08e60f523c4f7c60f29ab1060ec"]
   [{:reason :x} "73aa11d87b89e036ee79ec1ea9b84fc67dbb279d112303b434a96cb1ca580526"]])

(deftest attention-signatures-keep-the-digests-saved-checkpoints-hold
  (doseq [[r digest] digest-cases]
    (is (= {"q" digest} (:seen (observe-status/attention-changes {"q" r} {}))))))

;; Summaries

(deftest summaries-are-bounded-and-discard-routine-ticks
  (let [summary (atom {:counts {} :items [] :more false})]
    (dotimes [_ 1000] (swap! summary observe-status/collect (event :body :physics-tick)))
    (is (= {} (:counts @summary)))
    (dotimes [_ 1000] (swap! summary observe-status/collect (event :body :picked-up {:item "wheat" :count 1})))
    (is (= 4 (count (:items @summary))))
    (is (= 1000 (get-in @summary [:counts :picked-up])))
    (is (true? (:more @summary)))
    (is (= {:event :picked-up :item "wheat" :count 1} (first (:items @summary))))))

(deftest make-room-tosses-are-summarised-with-item-and-count
  (let [summary (observe-status/collect {:counts {} :items [] :more false}
                                 (event :job :make-room.tossed {:item "coal" :count 4}))]
    (is (= 1 (get-in summary [:counts :tossed])))
    (is (= {:event :make-room.tossed :item "coal" :count 4} (first (:items summary))))))

(defn plain-job-event [kind id data] {:source :job :kind kind :context {:job-id id} :data data})

(deftest a-death-names-the-jobs-it-cancelled
  (let [events [(plain-job-event :queued "j1" {:name "jobs.explore.search"})
                (plain-job-event :cancelled "j1" {:by :death})
                (plain-job-event :cancelled "j2" {:by :death})
                (plain-job-event :cancelled "j3" {:by :agent})
                (event :body :died {})]
        tracker (reduce observe-status/track-deaths {} events)
        jobs (observe-status/death-jobs tracker (last events))
        summary (observe-status/collect {:counts {} :items [] :more false} (last events) jobs)]
    (is (= [{:id "j1" :name "jobs.explore.search"} {:id "j2"}] jobs))
    (is (nil? (observe-status/death-jobs tracker (first events))))
    (is (= jobs (:cancelled-jobs (first (:items summary)))))
    (is (= jobs (:cancelled-jobs (observe-status/with-death-jobs {:wake :danger} jobs))))
    (is (= {:wake :danger} (observe-status/with-death-jobs {:wake :danger} nil)))))

;; The wait loop

(defn fixture
  ([] (fixture "30ms"))
  ([timeout]
   (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "observe-unit-"))
         req (request "--world" "w" "Probe" "--state" dir "--wait" "--timeout" timeout "--poll-ms" "50")
         world (atom {:generation "g" :outstanding {} :events [] :gap false})
         queries (atom [])
         get! (fn [_socket endpoint _options]
                (swap! queries conj endpoint)
                (let [{:keys [generation outstanding events gap]} @world
                      cursor {:stream-id "s" :seq (count events)}
                      value (cond
                              (= "/snapshot" endpoint) (cond-> {:body "Probe" :generation-id generation :outstanding outstanding :cursor cursor}
                                                         (contains? @world :instances) (assoc :state {:instances (:instances @world)}))
                              (= "/status" endpoint) {:mode :scheduled :current nil}
                              :else (let [after (js/Number (.get (.-searchParams (js/URL. (str "http://x" endpoint))) "after"))]
                                      {:gap? gap :stream-id "s" :latest-seq (count events) :cursor cursor
                                       :events (cond->> (filterv #(> (:seq %) after) events)
                                                 (:page-limit @world) (take (:page-limit @world))
                                                 true vec)}))]
                  (js/Promise.resolve {:status 200 :content-type "application/edn" :text (data/write-edn value)})))]
     {:dir dir :req req :world world :get! get! :queries queries
      :file (.join path dir "worlds" "w" "observers" "Probe" "agent.edn")
      :cleanup #(.rmSync fs dir #js {:recursive true :force true})
      :wait! (fn [& [signal deliver]] (observe/wait-observe req get! signal (or deliver (fn [_] (js/Promise.resolve nil)))))})))

(defn with-fixture
  "Run (body fixture), a promise; clean up and end the async test whatever happens."
  [fixture-args body]
  (async done
    (let [f (apply fixture fixture-args)]
      (-> (js/Promise.resolve nil)
          (.then #(body f))
          (.catch (fn [error] (is false (str "rejected: " error))))
          (.then (fn [_] ((:cleanup f)) (done)))))))

(defn saved [f] (data/read-edn (.readFileSync fs (:file f) "utf8")))

(defn push! [f & events] (swap! (:world f) update :events into events))

(deftest wait-persists-between-invocations-reports-quiet-changes-and-does-not-miss-between-call-chat
  (with-fixture []
    (fn [f]
      (-> ((:wait! f))
          (.then (fn [result]
                   (is (= {:wake :timeout :changed false} result))
                   (push! f (assoc (event :body :picked-up {:item "wheat" :count 2}) :seq 1))
                   ((:wait! f))))
          (.then (fn [quiet]
                   (is (= :timeout (:wake quiet)))
                   (is (= 1 (get-in quiet [:summary :counts :picked-up])))
                   (push! f (assoc (event :body :chat {:from "Alex"} "Probe come home") :seq 2))
                   ((:wait! f))))
          (.then (fn [woken]
                   (is (= :chat (:wake woken)))
                   (is (= 2 (get-in (saved f) [:cursor :seq])))
                   ((:wait! f))))
          (.then (fn [quiet] (is (false? (:changed quiet)))))))))

(deftest a-reconnect-failure-is-not-woken-for-when-the-body-is-back-on-a-later-page
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :page-limit 1)
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (assoc (event :body :reconnect-failed {:attempt 1}) :seq 1)
                          (assoc (event :body :online) :seq 2))
                   ((:wait! f))))
          (.then (fn [result] (is (= :timeout (:wake result)) "the :online on the next page stales the failure")))))))

(deftest a-reconnect-failure-still-wakes-when-nothing-recovers-it
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :page-limit 1)
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (assoc (event :body :reconnect-failed {:attempt 1}) :seq 1)
                          (assoc (event :body :picked-up {:item "wheat" :count 1}) :seq 2))
                   ((:wait! f))))
          (.then (fn [result] (is (= :reconnect-failed (:wake result)))))))))

(deftest outstanding-requests-remain-unresolved-wake-once-and-wake-again-on-change
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc-in [:outstanding :r] {:job-id "j1" :reason :blocked :event {:message "help"}})
      (-> ((:wait! f))
          (.then (fn [first-wake]
                   (is (= :attention (:wake first-wake)))
                   ((:wait! f))))
          (.then (fn [quiet]
                   (is (= :timeout (:wake quiet)))
                   (is (= 1 (count (:outstanding @(:world f)))))
                   (swap! (:world f) assoc-in [:outstanding :r :event :message] "different help")
                   ((:wait! f))))
          (.then (fn [again] (is (= :attention (:wake again)))))))))

(deftest a-checkpoint-written-by-the-previous-tool-still-silences-an-unchanged-request
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc-in [:outstanding :r] blocked)
      (.mkdirSync fs (.dirname path (:file f)) #js {:recursive true})
      (.writeFileSync fs (:file f)
                      (str "{:cursor {:stream-id \"s\" :seq 0} :generation \"g\" :seen {:r \"9542dc93064cfc366b307a0cca64e500308add1cc6df7fa550fc85c1ab7c8a7c\"} :pending []}\n"))
      (-> ((:wait! f))
          (.then (fn [result] (is (= {:wake :timeout :changed false} result))))))))

(deftest checkpoints-keep-their-layout
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc-in [:outstanding :r] blocked)
      (-> ((:wait! f))
          (.then (fn [_]
                   (is (= "{:cursor {:stream-id \"s\" :seq 0} :generation \"g\" :seen {:r \"9542dc93064cfc366b307a0cca64e500308add1cc6df7fa550fc85c1ab7c8a7c\"} :pending [] :lookup true}\n"
                          (.readFileSync fs (:file f) "utf8")))))))))

(deftest cancellation-and-failed-delivery-do-not-checkpoint-concurrent-observer-rejected-restart-and-gap-explicit
  (with-fixture ["1s"]
    (fn [f]
      (let [controller (js/AbortController.)
            pending (rejects ((:wait! f) (.-signal controller)))]
        (-> (rejects ((:wait! f)))
            (.then (fn [busy]
                     (is (= "EOBSERVERBUSY" (.-code busy)))
                     (.abort controller)
                     pending))
            (.then (fn [cancelled]
                     (is (= "ABORT_ERR" (.-code cancelled)))
                     (is (= 0 (get-in (saved f) [:cursor :seq])))
                     (observe/wait-observe (assoc-in (:req f) [:wait-options :timeout-ms] 20) (:get! f) nil
                                           (fn [_] (js/Promise.reject (js/Error. "stdout failed"))))))
            (.then (fn [_] (is false "resolved")))
            (.catch (fn [error] (is (re-find #"stdout failed" (.-message error)))))
            (.then (fn [_]
                     (is (= 0 (get-in (saved f) [:cursor :seq])))
                     ((:wait! f))))
            (.then (fn [_]
                     (swap! (:world f) assoc :generation "g2")
                     ((:wait! f))))
            (.then (fn [restarted]
                     (is (= :engine-restarted (:reason restarted)))
                     (swap! (:world f) assoc :gap true)
                     ((:wait! f))))
            (.then (fn [gap] (is (= :event-gap (:reason gap))))))))))

(deftest wait-validates-policies-durations-and-observer-names
  (is (= 60000 (get-in (request "--world" "w" "Probe" "--wait") [:wait-options :timeout-ms])))
  (is (= 1500 (get-in (request "--world" "w" "Probe" "--wait" "--timeout" "1.5s") [:wait-options :timeout-ms])))
  (doseq [argv [["--wait" "--observer" "../bad"]
                ["--wait" "--chatter" "classified"]
                ["--wait" "--watch" "j1,bad"]
                ["--wait" "--timeout" "infinity"]
                ["--wait" "--timeout" "5ms"]
                ["--wait" "--poll-ms" "10"]
                ["--wait" "--from" "bad/name"]
                ["--wait" "--raw"]
                ["--wait" "--verbose"]
                ["--timeout" "1s"]
                ["--danger"]
                ["--watch" "j1"]]]
    (is (string? (:error (apply request "--world" "w" "Probe" argv))) (pr-str argv)))
  (is (string? (:error (request "--world" "w" "Probe" "inventory" "--wait")))))

(deftest watchers-accept-repeated-and-comma-separated-options
  (let [r (request "--world" "w" "Probe" "--wait" "--watch" "j1,j2" "--watch" "j3")]
    (is (= ["j1" "j2" "j3"] (get-in r [:wait-options :watch])))))

(deftest the-first-cancelled-wait-preserves-the-baseline-so-events-before-retry-are-not-missed
  (with-fixture ["1s"]
    (fn [f]
      (let [controller (js/AbortController.)
            pending (rejects ((:wait! f) (.-signal controller)))]
        (-> (js/Promise. (fn [resolve _] (js/setTimeout resolve 5)))
            (.then (fn [_] (.abort controller) pending))
            (.then (fn [cancelled]
                     (is (some? (.-code cancelled)))
                     (push! f (assoc (event :body :chat {:from "Alex"} "Probe hello") :seq 1))
                     ((:wait! f))))
            (.then (fn [woken] (is (= :chat (:wake woken))))))))))

(deftest a-deadline-during-a-read-returns-a-quiet-summary-and-an-engine-start-notification-does-not-repeat
  (with-fixture ["20ms"]
    (fn [f]
      (let [stalled (fn [socket endpoint options]
                      (if (str/starts-with? endpoint "/events")
                        (-> (js/Promise. (fn [resolve _] (js/setTimeout resolve (+ 5 (:timeout-ms options))))) ; outlasts the deadline the read was given
                            (.then (fn [_] (throw (doto (js/Error. "deadline") (aset "code" "ETIMEDOUT"))))))
                        ((:get! f) socket endpoint options)))]
        (-> (observe/wait-observe (:req f) stalled nil (fn [_] (js/Promise.resolve nil)))
            (.then (fn [result]
                     (is (= :timeout (:wake result)))
                     (push! f (assoc (event :system :restored) :seq 1))
                     ((:wait! f))))
            (.then (fn [restarted]
                     (is (= :engine-restarted (:reason restarted)))
                     ((:wait! f))))
            (.then (fn [quiet] (is (= :timeout (:wake quiet))))))))))

(deftest resolved-attention-does-not-wake-again
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc-in [:outstanding :r] {:reason :blocked :event {:message "help"}})
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (assoc (event :attention :resolved) :request-id "r" :seq 1))
                   ((:wait! f))))
          (.then (fn [result] (is (= :timeout (:wake result)))))))))

(deftest a-second-observer-name-keeps-its-own-checkpoint
  (with-fixture []
    (fn [f]
      (let [other (assoc-in (:req f) [:wait-options :observer] "builder")]
        (-> ((:wait! f))
            (.then (fn [_] (observe/wait-observe other (:get! f) nil (fn [_] (js/Promise.resolve nil)))))
            (.then (fn [_]
                     (is (.existsSync fs (.join path (.dirname path (:file f)) "builder.edn")))
                     (is (.existsSync fs (:file f))))))))))

;; Job outcomes from the retained history

(defn job-event [n id kind & [data]]
  {:seq n :generation-id "g" :source :job :kind kind :context {:job-id id :chain [id]} :data (or data {})})

(def search-found [{:what "stone" :pos [5 70 3]}])

(def search-job
  [(job-event 1 "j4" :queued)
   (job-event 2 "j4" :search.done {:found search-found :coverage {:scans 1}})
   (job-event 3 "j4" :completed)])

(deftest result-and-fresh-watched-completion-expose-search-coordinates-reading-history-once-each
  (with-fixture ["1s"]
    (fn [f]
      (apply push! f search-job)
      (let [req (assoc-in (:req f) [:wait-options :watch] ["j4"])]
        (-> (job-results/read! (:get! f) "/unused" "j4" {})
            (.then (fn [result]
                     (is (= :completed (:status result)))
                     (is (= search-found (get-in result [:events 0 :data :found])))
                     (observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil)))))
            (.then (fn [wake]
                     (is (= :job-finished (:wake wake)))
                     (is (= :complete (:history wake)))
                     (is (= search-found (get-in wake [:events 0 :data :found])))
                     (is (= 4 (count @(:queries f))) "result, then the fresh watcher, each read snapshot and history once"))))))))

(deftest a-watched-job-finishing-during-the-wait-carries-its-outcome
  (with-fixture ["1s"]
    (fn [f]
      (let [req (assoc-in (:req f) [:wait-options :watch] ["j4"])
            wait! #(observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil)))]
        (-> (js/Promise.resolve nil)
            (.then (fn [_] (js/setTimeout #(apply push! f search-job) 20) (wait!)))
            (.then (fn [wake]
                     (is (= :job-finished (:wake wake)))
                     (is (= search-found (get-in wake [:events 0 :data :found]))))))))))

(deftest a-fresh-watch-for-a-job-outside-the-retained-window-reports-history-unavailable
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :instances {})
      (apply push! f (map #(job-event % "j8" :memory_written) (range 1 6)))
      (let [req (assoc-in (:req f) [:wait-options :watch] ["j4"])]
        (-> (observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil)))
            (.then (fn [wake]
                     (is (= :history-unavailable (:reason wake)))
                     (is (= ["j4"] (:jobs wake))))))))))

(deftest a-cursor-from-before-a-restart-resets-once-without-old-jobs-and-the-watched-job-is-still-found
  (with-fixture ["1s"]
    (fn [f]
      (let [req (assoc-in (:req f) [:wait-options :watch] ["j2"])
            wait! #(observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil)))]
        (-> (wait!)
            (.then (fn [_]
                     (push! f (job-event 1 "j1" :completed)
                            (assoc (event :system :restored) :seq 2 :generation-id "g")
                            (job-event 3 "j2" :queued)
                            (job-event 4 "j2" :completed))
                     (wait!)))
            (.then (fn [restarted]
                     (is (= :reset (:wake restarted)))
                     (is (= :engine-restarted (:reason restarted)))
                     (is (not (contains? restarted :summary)) "old jobs from before the restart are not summarised")
                     (wait!)))
            (.then (fn [watched]
                     (is (= :job-finished (:wake watched)))
                     (is (= "j2" (:job watched)))
                     (wait!)))
            (.then (fn [quiet] (is (= :timeout (:wake quiet))))))))))

(deftest a-fresh-watch-for-a-live-job-without-events-keeps-waiting
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :instances {"j4" {}})
      (let [req (assoc-in (:req f) [:wait-options :watch] ["j4"])]
        (-> (observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil)))
            (.then (fn [wake] (is (= :timeout (:wake wake))))))))))

(deftest observe-result-validates-ids-before-connecting
  (is (= "j4" (:result-id (request "--world" "w" "Probe" "result" "j4"))))
  (doseq [argv [["result" "not-a-job"] ["result" "j4" "--limit" "9"] ["result"] ["result" "j4" "j5"]]]
    (is (string? (:error (apply request "--world" "w" "Probe" argv))) (pr-str argv))))

(defn job-history-handler [{:keys [path]}]
  (cond
    (str/starts-with? path "/job?") {:status 404 :text "{:ok false :reason :job-not-found}"}
    (= "/snapshot" path) {:text (data/write-edn {:body "ProbeBody" :generation-id "g" :cursor {:stream-id "s" :seq 3}})}
    :else {:text (data/write-edn {:stream-id "s" :latest-seq 3 :gap? false :events search-job})}))

(deftest a-completed-job-and-an-explicit-result-print-the-same-outcome
  (async done
    (finish! done
             (-> (series [#(run-main! ["job" "j4"] job-history-handler) #(run-main! ["result" "j4"] job-history-handler)])
                 (.then (fn [[job result]]
                          (is (= 0 (:code job)) (:out job))
                          (is (= (:out job) (:out result)))
                          (is (= search-found (get-in (data/read-edn (:out result)) [:events 0 :data :found])))))))))

(deftest compact-status-keeps-why-and-return-of-an-offline-body
  (is (= {:by :shelter :job "j563" :why :logged-out-for-night :back-at 1020000 :back-in-s 0}
         (:offline (observe-status/compact-status {:mode :offline :offline {:by :shelter :job "j563" :why :logged-out-for-night :back-at 1020000}} 1020000))))
  (is (not (contains? (observe-status/compact-status {:mode :scheduled}) :offline))))

(deftest compact-status-says-how-long-until-a-planned-return-and-marks-last-known-readings
  (let [s {:mode :offline :offline {:by :shelter :why :logged-out-for-night :back-at 1020000}
           :position {:x 1.04 :y 64 :z 2} :health 18 :food 15 :last-known true}
        out (observe-status/compact-status s 960000)]
    (is (= 60 (get-in out [:offline :back-in-s])))
    (is (= [1 64 2] (:pos out)))
    (is (= 18 (:health out)))
    (is (true? (:last-known out))))
  (is (not (contains? (:offline (observe-status/compact-status {:mode :offline :offline {:by :connection :why :connection-lost}} 5)) :back-in-s))))

(deftest a-reconnect-failure-wakes-once-per-outage-and-never-after-the-body-is-back
  (let [failed (fn [attempt] (event :body :reconnect-failed {:attempt attempt :reason "ECONNREFUSED"}))
        online (event :body :online)]
    (is (false? (observe-status/stale-reconnect? (failed 1) [])))
    (is (true? (observe-status/stale-reconnect? (failed 2) [])) "later tries of one outage stay in the summary")
    (is (true? (observe-status/stale-reconnect? (failed 1) [(failed 2) online])) "the body came back later in the backlog")
    (is (false? (observe-status/stale-reconnect? (event :body :reconnect-failed) [])))))

(defn with-lock-dir [f]
  (let [dir (.mkdtempSync fs (.join path (os/tmpdir) "observe-lock-"))]
    (try (f dir)
         (finally (.rmSync fs dir #js {:recursive true :force true})))))

(deftest acquire-reclaims-a-lock-whose-owner-died-before-writing-its-pid
  (with-lock-dir
    (fn [dir]
      (let [lock (.join path dir "obs.lock")]
        (.mkdirSync fs lock)
        (.utimesSync fs lock 1 1)
        (let [release (observe-lock/acquire! dir "obs")]
          (is (= (str (.-pid js/process)) (.readFileSync fs (.join path lock "pid") "utf8")))
          (is (= ["obs.lock"] (vec (.readdirSync fs dir))) "the stale lock is renamed away and removed")
          (release))))))

(deftest acquire-refuses-a-fresh-lock-with-no-pid-yet
  (with-lock-dir
    (fn [dir]
      (.mkdirSync fs (.join path dir "obs.lock"))
      (is (= "EOBSERVERBUSY" (try (observe-lock/acquire! dir "obs") nil (catch :default e (.-code e))))))))

(defn kill-failing [code] (fn [_] (throw (doto (js/Error. "kill") (aset "code" code)))))

(deftest a-process-owned-by-another-user-counts-as-alive
  (is (true? (observe-lock/process-alive? 1 (kill-failing "EPERM"))))
  (is (false? (observe-lock/process-alive? 1 (kill-failing "ESRCH"))))
  (is (true? (observe-lock/process-alive? 1 (fn [_] nil))))
  (is (= "EINVAL" (try (observe-lock/process-alive? 1 (kill-failing "EINVAL")) (catch :default e (.-code e))))))

(deftest reclaim-puts-back-a-live-lock-that-replaced-the-stale-one
  (with-lock-dir
    (fn [dir]
      (let [lock (.join path dir "obs.lock")]
        (.mkdirSync fs lock)
        (.writeFileSync fs (.join path lock "pid") (str (.-pid js/process)))
        (is (= "EEXIST" (try (observe-lock/reclaim-stale-lock! lock) nil (catch :default e (.-code e)))))
        (is (= (str (.-pid js/process)) (.readFileSync fs (.join path lock "pid") "utf8")) "the live lock is back, untouched")
        (is (= ["obs.lock"] (vec (.readdirSync fs dir))) "no aside left behind")))))

(defn code-thrown [f] (try (f) nil (catch :default e (.-code e))))

(deftest reclaim-never-leaves-a-live-lock-path-absent-and-a-third-observer-is-refused
  (with-lock-dir
    (fn [dir]
      (let [lock (.join path dir "obs.lock")
            seen (atom [])]
        (.mkdirSync fs lock)
        (.writeFileSync fs (.join path lock "pid") (str (.-pid js/process)))
        (is (= "EEXIST" (code-thrown
                          #(observe-lock/reclaim-stale-lock!
                             lock (fn [step]
                                    (swap! seen conj [step (.existsSync fs lock) (code-thrown (fn [] (observe-lock/acquire! dir "obs")))]))))))
        (is (seq @seen) "the interleaving ran")
        (is (every? (fn [[_ present? third]] (and present? (= "EOBSERVERBUSY" third))) @seen))
        (is (= (str (.-pid js/process)) (.readFileSync fs (.join path lock "pid") "utf8")))
        (is (= ["obs.lock"] (vec (.readdirSync fs dir))) "no aside or mutex left behind")))))

(deftest reclaim-of-a-dead-owners-lock-leaves-one-holder-when-a-third-observer-takes-the-gap
  (with-lock-dir
    (fn [dir]
      (let [lock (.join path dir "obs.lock")
            fired (atom false)]
        (.mkdirSync fs lock)
        (.writeFileSync fs (.join path lock "pid") "99999999")
        (is (= "EEXIST" (code-thrown
                          #(observe-lock/reclaim-stale-lock!
                             lock (fn [step]
                                    (when (= :lock-removed step)
                                      (reset! fired true)
                                      (.mkdirSync fs lock)
                                      (.writeFileSync fs (.join path lock "pid") (str (.-pid js/process))))))))
            "the reclaimer loses to the third observer")
        (is @fired)
        (is (= (str (.-pid js/process)) (.readFileSync fs (.join path lock "pid") "utf8")) "the third observer's lock stands")
        (is (= ["obs.lock"] (vec (.readdirSync fs dir))))))))

(deftest reclaimers-take-turns-and-a-crashed-reclaimers-mutex-expires
  (with-lock-dir
    (fn [dir]
      (let [lock (.join path dir "obs.lock")
            mutex (str lock ".reclaim")]
        (.mkdirSync fs lock)
        (.writeFileSync fs (.join path lock "pid") "99999999")
        (.mkdirSync fs mutex)
        (is (= "EEXIST" (code-thrown #(observe-lock/reclaim-stale-lock! lock))) "another reclaimer is working")
        (is (.existsSync fs lock) "the lock was not touched")
        (.utimesSync fs mutex 1 1)
        (is (nil? (code-thrown #(observe-lock/reclaim-stale-lock! lock))) "a long-dead reclaimer's mutex is cleared")
        (is (= ["obs.lock"] (vec (.readdirSync fs dir))))))))

;; Submit waits are ephemeral: no observer lock, no checkpoint, only attention raised after they start

(defn wait-for! [f opts]
  (observe/wait-for! (select-keys (:req f) [:agent :world :state :socket-path]) (merge {:timeout "1s"} opts) (:get! f)))

(defn observer-files [f]
  (let [dir (.dirname path (:file f))]
    (if (.existsSync fs dir) (vec (.readdirSync fs dir)) [])))

(deftest a-submit-wait-does-not-contend-with-an-observer-wait
  (with-fixture ["1s"]
    (fn [f]
      (let [release (observe-lock/acquire! (.dirname path (:file f)) "agent")]
        (js/setTimeout #(apply push! f search-job) 20)
        (-> (wait-for! f {:watch ["j4"]})
            (.then (fn [wake]
                     (is (= :job-finished (:wake wake)))
                     (is (not= :observer-busy (:reason wake)))))
            (.finally release))))))

(deftest two-submit-waits-at-once-both-finish
  (with-fixture ["1s"]
    (fn [f]
      (js/setTimeout #(apply push! f search-job) 20)
      (-> (js/Promise.all #js [(wait-for! f {:watch ["j4"]}) (wait-for! f {:watch ["j4"]})])
          (.then (fn [wakes] (is (= [:job-finished :job-finished] (mapv :wake wakes)))))))))

(deftest a-submit-wait-ignores-attention-outstanding-at-its-start-but-wakes-on-new
  (with-fixture ["1s"]
    (fn [f]
      (swap! (:world f) assoc-in [:outstanding :r] blocked)
      (js/setTimeout (fn []
                       (swap! (:world f) assoc-in [:outstanding :r2] blocked)
                       (push! f (assoc (event :attention :raised) :seq 1 :attention :required)))
                     60)
      (-> (wait-for! f {:watch ["j9"]})
          (.then (fn [wake]
                   (is (= :attention (:wake wake)))
                   (is (= ["r2"] (mapv :id (:requests wake))) "only the request raised after the start wakes")))))))

(deftest a-submit-wait-leaves-no-files-after-success-timeout-or-abort
  (with-fixture ["1s"]
    (fn [f]
      (js/setTimeout #(apply push! f search-job) 20)
      (-> (wait-for! f {:watch ["j4"]})
          (.then (fn [_] (wait-for! f {:timeout "50ms" :watch ["j9"]})))
          (.then (fn [_] (is (= [] (observer-files f)))))))))

;; Deaths through the wait loop

(defn died-item [result] (->> (get-in result [:summary :items]) (filter #(= :died (:event %))) first))

(defn plain-job-event-at [kind id seq data] (assoc (plain-job-event kind id data) :seq seq))

(deftest a-death-names-a-job-queued-before-the-call-and-a-second-death-does-not-repeat-the-first
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :instances {"j1" {:spec {:op :leaf :job 'jobs.explore.search}}})
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (plain-job-event-at :cancelled "j1" 1 {:by :death}) (assoc (event :body :died {}) :seq 2))
                   ((:wait! f))))
          (.then (fn [first-death]
                   (is (= [{:id "j1" :name "jobs.explore.search"}] (:cancelled-jobs (died-item first-death))))
                   (push! f (assoc (event :body :died {}) :seq 3))
                   ((:wait! f))))
          (.then (fn [second-death]
                   (is (= :died (:event (died-item second-death))))
                   (is (not (contains? (died-item second-death) :cancelled-jobs)))))))))

(deftest cancels-from-an-earlier-call-are-kept-for-the-death-in-a-later-one
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :instances {"j1" {:spec {:op :leaf :job 'jobs.explore.search}}})
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (plain-job-event-at :cancelled "j1" 1 {:by :death}))
                   ((:wait! f))))
          (.then (fn [_]
                   (swap! (:world f) assoc :instances {})
                   (push! f (assoc (event :body :died {}) :seq 2))
                   ((:wait! f))))
          (.then (fn [death]
                   (is (= [{:id "j1" :name "jobs.explore.search"}] (:cancelled-jobs (died-item death))))))))))

(deftest the-cancelled-list-keeps-only-the-latest-cancels
  (let [events (for [i (range 80)] (plain-job-event :cancelled (str "j" i) {:by :death}))
        tracker (reduce observe-status/track-deaths {} events)]
    (is (= 50 (count (:cancelled tracker))))
    (is (= {:id "j79"} (last (:cancelled tracker))))))

(deftest an-event-gap-drops-the-cancels-seen-before-it
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :instances {"j1" {:spec {:op :leaf :job 'jobs.explore.search}}})
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (plain-job-event-at :cancelled "j1" 1 {:by :death}))
                   ((:wait! f))))
          (.then (fn [_]
                   (swap! (:world f) assoc :gap true)
                   ((:wait! f))))
          (.then (fn [gap]
                   (is (= :event-gap (:reason gap)))
                   (swap! (:world f) assoc :gap false :events [])
                   (push! f (assoc (event :body :died {}) :seq 5))
                   ((:wait! f))))
          (.then (fn [death]
                   (is (not (contains? (died-item death) :cancelled-jobs)))))))))

(deftest spec-label-prints-as-the-engine-expr-label
  (let [specs [{:op :leaf :job 'jobs.a.b}
               {:op :repeat :child {:op :leaf :job 'jobs.a.b}}
               {:op :seq :children [{:op :leaf :job 'jobs.a.b} {:op :any :children [{:op :leaf :job 'jobs.c.d}]}]}]]
    (doseq [spec specs]
      (is (= (expr/label spec) (observe-status/spec-label spec))))))
