(ns agent-tools.observe-test
  (:require [cljs.test :refer [deftest is are async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.job-results :as job-results]
            [agent-tools.observe :as observe]
            [agent-tools.observe.events :as observe-events]
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

(deftest usage-for-builds-help-with-custom-prefix
  "usage-for should build the usage string with the given prefix"
  (let [raw (observe-request/usage-for "observe.mjs")
        bin (observe-request/usage-for "./bin/observe")]
    (is (str/starts-with? raw "usage: observe.mjs <agent> --world <world>"))
    (is (str/starts-with? bin "usage: ./bin/observe <agent> --world <world>"))))

(deftest a-truncated-catalog-page-says-how-to-get-the-rest
  (let [page "{:ok true :kind :jobs :items [\"a\"] :total 45 :next-offset 20}\n"]
    (is (str/includes? (observe/with-catalog-more "/catalog?kind=jobs&prefix=&limit=20&offset=0" page) "25 more, use --offset 20"))
    (is (= "{:ok true :items [] :total 1 :next-offset nil}\n"
           (observe/with-catalog-more "/catalog?kind=jobs" "{:ok true :items [] :total 1 :next-offset nil}\n")))
    (is (= "x\n" (observe/with-catalog-more "/status" "x\n")))))

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
           [["catalog" "jobs.movement.go-to"] "/catalog?kind=job&name=jobs.movement.go-to"]
           [["catalog" "trigger" "health-low"] "/catalog?kind=trigger&name=health-low"]
           [["catalog" "jobs" "jobs.movement." "--limit" "5" "--offset" "10"]
            "/catalog?kind=jobs&prefix=jobs.movement.&limit=5&offset=10"]
           [["catalog" "triggers"] "/catalog?kind=triggers&prefix=&limit=20&offset=0"]
           [["catalog" "jobs" "--tag" "mine"] "/catalog?kind=jobs&prefix=&limit=20&offset=0&tag=mine"]]]
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

(deftest seen-is-a-read-only-observe-query-defaulting-to-the-useful-blocks
  (let [default (request "--world" "w" "ProbeBody" "seen")
        query (second (str/split (:path default) #"\?"))
        params (js/URLSearchParams. query)]
    (is (str/starts-with? (:path default) "/seen?"))
    (is (= "32" (.get params "radius")))
    (is (= "16" (.get params "max")))
    (doseq [n ["crafting_table" "furnace" "chest" "barrel" "*_bed" "*_door" "*_ore" "wheat" "carrots" "potatoes" "beetroots" "nether_wart" "melon" "pumpkin"]]
      (is (some #{n} (str/split (.get params "names") #",")) n)))
  (is (= "/seen?names=chest%2Ciron_ore&radius=20&max=5"
         (:path (request "--world" "w" "ProbeBody" "seen" "--names" "chest,iron_ore" "--radius" "20" "--limit" "5"))))
  (doseq [[argv pattern] [[["seen" "x"] #"takes no positional"]
                          [["seen" "--radius" "65"] #"--radius"]
                          [["seen" "--names" "Bad!"] #"--names"]
                          [["status" "--radius" "5"] #"only valid for seen"]]]
    (is (re-find pattern (:error (apply request "--world" "w" "ProbeBody" argv))) (pr-str argv))))

(deftest chests-is-a-read-only-observe-query
  (is (= "/chests?max=16" (:path (request "--world" "w" "ProbeBody" "chests"))))
  (is (= "/chests?max=5" (:path (request "--world" "w" "ProbeBody" "chests" "--limit" "5"))))
  (doseq [[argv pattern] [[["chests" "x"] #"takes no positional"]
                          [["chests" "--radius" "5"] #"only valid for seen"]]]
    (is (re-find pattern (:error (apply request "--world" "w" "ProbeBody" argv))) (pr-str argv))))

(deftest light-is-a-read-only-observe-query-of-a-cell-or-the-feet
  (doseq [[argv path] [[["light"] "/light"]
                       [["light" "--radius" "2"] "/light?radius=2"]
                       [["light" "--radius" "0" "--" "1" "64" "-3"] "/light?x=1&y=64&z=-3&radius=0"]]]
    (is (= path (:path (apply request "--world" "w" "ProbeBody" argv))) (pr-str argv)))
  (doseq [[argv pattern] [[["light" "1" "2"] #"no cell"]
                          [["light" "1" "a" "3"] #"integers"]
                          [["light" "--radius" "9"] #"--radius"]
                          [["light" "--names" "x"] #"--names"]]]
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
   ["the engine build is kept"
    "{:mode :scheduled :build {:commit \"abc1234\" :started-at 5}}"
    "{:mode :scheduled :idle true :build {:commit \"abc1234\" :started-at 5}}"]
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

(defn notice [job kind message]
  (assoc (event :job kind {} message) :attention :notice :context {:job-id job}))

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
   ["a hit above the threshold stays quiet" (event :body :hurt {:health 17}) (assoc defaults :danger true :danger-below 10) nil]
   ["a hit at the threshold wakes" (event :body :hurt {:health 10}) (assoc defaults :danger true :danger-below 10) :danger]
   ["disconnection wakes when asked" (event :body :disconnected) (assoc defaults :disconnect true) :disconnected]
   ["a death always wakes" (event :body :died {:cause "skeleton"}) defaults :died]
   ["a death wakes under danger as a death" (event :body :died {:cause "skeleton"}) (assoc defaults :danger true) :died]
   ["exhausted reconnection always wakes" (event :body :reconnect-failed) defaults :reconnect-failed]
   ["an unwatched job stays quiet" (assoc (event :job :completed) :context {:job-id "j1"}) defaults nil]
   ["a watched job wakes" (assoc (event :job :completed) :context {:job-id "j1"}) (assoc defaults :watch ["j1"]) :job-finished]
   ["a watched job wakes when stopped" (assoc (event :job :stopped) :context {:job-id "j1"}) (assoc defaults :watch ["j1"]) :job-finished]
   ["a watched job wakes when cancelled" (assoc (event :job :cancelled) :context {:job-id "j1"}) (assoc defaults :watch ["j1"]) :job-finished]
   ["an exploration start wakes" (notice "j2" :obtain.exploring "looking for oak_log") defaults :notice]
   ["any notice wakes" (notice "j2" :shelter.exposed "exposed: no roof") defaults :notice]
   ["a notice from a body source wakes" (assoc (event :body :job.notify {} "hi") :attention :notice) defaults :notice]
   ["a watch limits notices to the watched job" (notice "j2" :shelter.exposed "exposed") (assoc defaults :watch ["j5"]) nil]
   ["a notice of a watched job's child wakes" (notice "j5/place" :child_ended "x") (assoc defaults :watch ["j5"]) :notice]
   ["a body notice wakes under a watch" (assoc (event :body :job.notify {} "hi") :attention :notice) (assoc defaults :watch ["j5"]) :notice]
   ["a flight that moved the body wakes under a watch" (notice "j9" :hostile.fled "fled 18 blocks") (assoc defaults :watch ["j5"]) :notice]
   ["quiet notices suppress it" (notice "j2" :shelter.exposed "exposed") (assoc defaults :quiet-notices true) nil]
   ["an unwatched job end noticed stays quiet" (notice "j2" :completed nil) defaults nil]
   ["an unwatched give-up noticed stays quiet" (notice "j2" :stopped "gave up walking") defaults nil]
   ["a watched job end still reports as finished" (notice "j2" :completed nil) (assoc defaults :watch ["j2"] :quiet-notices true) :job-finished]
   ["an event without attention stays quiet" (assoc (event :job :retreat {} "x") :context {:job-id "j2"}) defaults nil]
   ["an idle body always wakes" (event :body :idle {:jobs []} "body idle 10 s") defaults :idle]])

(deftest a-notice-wake-names-kind-job-and-clipped-message
  (is (= {:wake :notice :event :hostile.chased :job "j4" :message (apply str (repeat 240 "x"))}
         (observe-status/classify (notice "j4" :hostile.chased (apply str (repeat 400 "x"))) defaults "Probe"))))

(deftest a-repeating-notice-wakes-once-per-kind-and-job
  (let [a (notice "j1" :retreat "back")
        b (notice "j2" :retreat "back")
        c (notice "j1" :dig "dig")
        noticed (reduce observe-status/note-notice [] [a])]
    (is (observe-status/repeat-notice? noticed a))
    (is (not (observe-status/repeat-notice? noticed b)))
    (is (not (observe-status/repeat-notice? noticed c)))
    (is (not (observe-status/repeat-notice? noticed (assoc (event :job :retreat) :context {:job-id "j1"}))))))

(deftest a-job-end-starts-a-new-stretch-for-its-notices
  (let [a (notice "j1" :retreat "back")
        noticed (-> [] (observe-status/note-notice a) (observe-status/note-notice (notice "j2" :retreat "x")))
        after (observe-status/note-notice noticed (assoc (event :job :completed) :context {:job-id "j1"}))]
    (is (not (observe-status/repeat-notice? after a)))
    (is (observe-status/repeat-notice? after (notice "j2" :retreat "x")))))

(deftest the-noticed-list-is-bounded
  (let [noticed (reduce #(observe-status/note-notice %1 (notice (str "j" %2) :dig "d")) [] (range 1000))]
    (is (<= (count noticed) 256))
    (is (observe-status/repeat-notice? noticed (notice "j999" :dig "d")))))

(deftest a-death-wake-passes-on-the-surviving-jobs
  (is (= ["j14"] (:survived (observe-status/classify (event :body :died {:cause "skeleton" :survived ["j14"]}) defaults "Probe"))))
  (is (not (contains? (observe-status/classify (event :body :died {:cause "skeleton"}) defaults "Probe") :survived))))

(deftest a-death-wake-names-cause-and-place
  (is (= {:wake :died :cause "skeleton" :at [1 2 3]}
         (observe-status/classify (event :body :died {:cause "skeleton" :pos {:x 1 :y 2 :z 3}}) defaults "Probe"))))

(deftest an-idle-wake-names-the-jobs
  (let [jobs [{:job "j1" :name "jobs.items.get-tool" :waiting {:reason :no-source}}]]
    (is (= {:wake :idle :message "body idle 10 s: j1 ..." :jobs jobs}
           (observe-status/classify (event :body :idle {:jobs jobs} "body idle 10 s: j1 ...") defaults "Probe")))))

(deftest an-idle-wake-under-watch-keeps-only-the-watched-jobs
  (let [jobs [{:job "j1" :name "jobs.items.craft" :waiting {:reason :reflex-holds :reflex :night :job "j9"}}
              {:job "j2" :name "(repeat jobs.farm.maintain)" :waiting {:reason :exposed :text "dark at 1 2 3 until light"}}]
        message "body idle 10 s with no useful act: j1 jobs.items.craft waiting: reflex-holds {:reflex :night, :job \"j9\"}; j2 (repeat jobs.farm.maintain) waiting: dark at 1 2 3 until light"
        wake (observe-status/classify (event :body :idle {:jobs jobs} message) (assoc defaults :watch ["j1"]) "Probe")]
    (is (= [(first jobs)] (:jobs wake)))
    (is (= "body idle 10 s with no useful act: j1 jobs.items.craft waiting: reflex-holds {:reflex :night, :job \"j9\"}" (:message wake)))))

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

(defn plain-job-event [kind id data] {:source :job :kind kind :context {:job-id id} :data data})

(deftest a-job-end-or-notice-takes-a-routine-items-place-in-a-full-summary
  (let [routine [(event :body :picked-up {:item "dirt" :count 1}) (event :body :picked-up {:item "dirt" :count 1})
                 (assoc (event :reflex :fired) :context {:job-id "j142" :reflex-id :hostile-near})
                 (assoc (event :reflex :fired) :context {:job-id "j143" :reflex-id :hostile-near})
                 (event :body :picked-up {:item "dirt" :count 1})]
        ended (assoc (plain-job-event :completed "j141" {:status :completed}) :attention :notice)
        fled (assoc (notice "j148/flee" :hostile.fled "fled 30 blocks") :level :warn)
        summary (reduce observe-status/collect {:counts {} :items [] :more false} (concat routine [ended fled]))]
    (is (= 4 (count (:items summary))))
    (is (true? (:more summary)))
    (is (= [:fired :fired :completed :hostile.fled] (mapv :event (:items summary)))
        "the oldest routine items make room, the rest keep their order")
    (is (= "j141" (:job (nth (:items summary) 2))))
    (is (= {:picked-up 3 :reflexes 2 :completed 1 :notices 1} (:counts summary)))))

(deftest make-room-tosses-are-summarised-with-item-and-count
  (let [summary (observe-status/collect {:counts {} :items [] :more false}
                                 (event :job :make-room.tossed {:item "coal" :count 4}))]
    (is (= 1 (get-in summary [:counts :tossed])))
    (is (= {:event :make-room.tossed :item "coal" :count 4} (first (:items summary))))))

(deftest an-event-older-than-the-cutoff-is-only-counted-in-the-summary
  (let [old (assoc (notice "j132/dig-in" :dig-in.unsealed "NOT sealed in") :time-ms 1000)
        fresh (assoc (notice "j140/dig-in" :dig-in.unsealed "NOT sealed in") :time-ms 9000)
        summary (-> {:counts {} :items [] :more false}
                    (observe-status/collect old nil 5000)
                    (observe-status/collect fresh nil 5000))]
    (is (= {:notices 2} (:counts summary)))
    (is (= ["j140/dig-in"] (mapv :job (:items summary))))))


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

(def real-hurt {:source :body :kind :hurt :time-ms 1791455125080 :data {:amount 3 :health 0 :causes ["skeleton"]}})
(def real-died
  {:source :body :kind :died :time-ms 1791455125099
   :data {:dimension "overworld" :pos {:x -1547.8 :y 62.3 :z -1499.2}
          :inventory [{:name "birch_log" :count 2 :slot 9} {:name "stick" :count 2 :slot 10}
                      {:name "oak_sapling" :count 2 :slot 37} {:name "leaf_litter" :count 8 :slot 38}
                      {:name "dirt" :count 16 :slot 39} {:name "birch_planks" :count 1 :slot 40}]}})
(def real-decision
  {:source :job :kind :stopped
   :data {:decision :skip :reason :not-worth :value 75.35 :cost :infinite}})

(deftest a-death-takes-its-cause-from-the-last-hurt
  (let [tracker (reduce observe-status/track-deaths {} [real-hurt real-died])
        details (observe-status/death-details tracker real-died nil)]
    (is (= "skeleton" (:cause details)))
    (is (nil? (observe-status/death-details tracker real-hurt nil)))))

(deftest a-hurt-long-before-the-death-is-not-its-cause
  (let [old (assoc real-hurt :time-ms (- (:time-ms real-died) 20000))
        tracker (reduce observe-status/track-deaths {} [old real-died])]
    (is (nil? (:cause (observe-status/death-details tracker real-died nil))))))

(deftest a-death-lists-the-top-lost-items-by-count
  (let [details (observe-status/death-details {} real-died nil)]
    (is (= ["dirt x16" "leaf_litter x8" "birch_log x2"] (:lost details)))
    (is (= 3 (:lost-more details)))))

(deftest a-death-carries-the-recover-decision-when-it-arrived
  (is (= {:decision "skip" :reason "not-worth" :value 75}
         (:recover (observe-status/death-details {} real-died [real-decision]))))
  (is (nil? (:recover (observe-status/death-details {} real-died nil)))))

(deftest the-death-wake-merges-the-details
  (let [tracker (reduce observe-status/track-deaths {} [real-hurt real-died])
        wake (observe-status/with-death-details (observe-status/classify real-died defaults "Probe")
               (observe-status/death-details tracker real-died [real-decision]))]
    (is (= "skeleton" (:cause wake)))
    (is (= :died (:wake wake)))
    (is (= "skip" (get-in wake [:recover :decision])))))

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
                                                         (or (contains? @world :instances) (contains? @world :recent-results))
                                                         (assoc :state (select-keys @world [:instances])))
                              (.startsWith endpoint "/results?ids=")
                              {:results (select-keys (:recent-results @world)
                                                     (str/split (second (str/split endpoint #"=")) #","))}
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

(deftest waits-wake-on-hurt-unless-told-not-to
  (is (true? (get-in (request "--world" "w" "Probe" "--wait") [:wait-options :danger])))
  (is (false? (get-in (request "--world" "w" "Probe" "--wait" "--no-danger") [:wait-options :danger])))
  (is (string? (:error (request "--world" "w" "Probe" "--no-danger"))))
  (is (= 12 (get-in (request "--world" "w" "Probe" "--wait" "--danger-below" "12") [:wait-options :danger-below])))
  (is (string? (:error (request "--world" "w" "Probe" "--wait" "--danger-below" "0")))))

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
                     (is (= 6 (count @(:queries f))) "result, then the fresh watcher, each read snapshot, recorded results and history once"))))))))

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

(deftest a-fresh-watch-for-a-job-finished-before-the-last-1000-events-reads-its-recorded-result
  (with-fixture ["1s"]
    (fn [f]
      (swap! (:world f) assoc :instances {})
      (apply push! f search-job)
      (apply push! f (map #(job-event % "j8" :memory_written) (range 4 1204)))
      (let [req (assoc-in (:req f) [:wait-options :watch] ["j4"])]
        (-> (observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil)))
            (.then (fn [wake]
                     (is (= :job-finished (:wake wake)))
                     (is (= "j4" (:job wake)))
                     (is (= search-found (get-in wake [:events 0 :data :found]))))))))))

(def recorded-search
  {"j4" {:name "search" :status :completed :ended-at 5 :result {:status :completed}}})

(deftest a-job-that-ended-long-ago-is-reported-from-the-engines-recorded-results
  (with-fixture ["1s"]
    (fn [f]
      (swap! (:world f) assoc :instances {} :recent-results recorded-search)
      (apply push! f (map #(job-event % "j8" :memory_written) (range 1 10001)))
      (let [req (assoc-in (:req f) [:wait-options :watch] ["j4"])]
        (-> (job-results/read! (:get! f) "/unused" "j4" {})
            (.then (fn [result]
                     (is (= :completed (:status result)))
                     (is (true? (:finished? result)))
                     (observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil)))))
            (.then (fn [wake]
                     (is (= :job-finished (:wake wake)))
                     (is (= "j4" (:job wake)))
                     (is (= :completed (:result wake))))))))))

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

(deftest compact-status-shows-air-and-in-water-only-when-in-water
  (let [out (observe-status/compact-status {:mode :scheduled :air 12 :in-water true})]
    (is (= 12 (:air out)))
    (is (true? (:in-water out))))
  (is (not (contains? (observe-status/compact-status {:mode :scheduled :air 20 :in-water false}) :in-water))))

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

(deftest a-log-out-and-the-return-are-summarised-with-who-and-for-how-long
  (let [out (event :action :logged-out {:by :jobs.survival.night :job "j71/log-out" :why :logged-out-for-sleeping-player :back-at 1020000})
        summary (observe-status/collect {:counts {} :items [] :more false} out)
        item (first (:items summary))]
    (is (= {:logged-out 1} (:counts summary)))
    (is (= :logged-out (:event item)))
    (is (= "jobs.survival.night" (:by item)))
    (is (= "logged-out-for-sleeping-player" (:why item)))))

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

(deftest acquire-reclaims-a-lock-whose-pid-was-reused
  (with-lock-dir
    (fn [dir]
      (let [lock (.join path dir "obs.lock")]
        (.mkdirSync fs lock)
        (.writeFileSync fs (.join path lock "pid") (str (.-pid js/process)))
        (.writeFileSync fs (.join path lock "start") "1")
        (let [release (observe-lock/acquire! dir "obs")]
          (is (= (observe-lock/process-start-time (.-pid js/process)) (.readFileSync fs (.join path lock "start") "utf8")))
          (release))))))

(deftest acquire-refuses-a-lock-held-by-the-live-process-that-recorded-its-start
  (with-lock-dir
    (fn [dir]
      (let [release (observe-lock/acquire! dir "obs")]
        (is (= "EOBSERVERBUSY" (try (observe-lock/acquire! dir "obs") nil (catch :default e (.-code e)))))
        (release)))))

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

(def parked
  {:job-id "j101" :reason :round-failed :updated-at 1
   :event {:kind :failed :message "Job failed and is parked until retried or cancelled: boom"
           :data {:error "boom" :reason :round-failed}}})

(defn parked-event [seq]
  {:seq seq :generation-id "g" :source :job :kind :failed :attention :required
   :context {:job-id "j101" :chain ["j101"]} :data {:error "boom" :reason :round-failed}
   :message "Job failed and is parked until retried or cancelled: boom"})

(deftest a-watched-job-that-fails-with-attention-wakes-as-finished-with-its-reason
  (with-fixture ["1s"]
    (fn [f]
      (js/setTimeout (fn []
                       (swap! (:world f) assoc-in [:outstanding :r101] parked)
                       (push! f (parked-event 1)))
                     60)
      (-> (wait-for! f {:watch ["j101"]})
          (.then (fn [wake]
                   (is (= {:wake :job-finished :job "j101" :result :failed :message "boom"}
                          (select-keys wake [:wake :job :result :message])))))))))

(deftest a-failed-watched-job-does-not-wake-again-as-attention-on-the-next-wait
  (with-fixture ["1s"]
    (fn [f]
      (swap! (:world f) assoc-in [:outstanding :r101] parked)
      (push! f (parked-event 1))
      (-> ((:wait! f))
          (.then (fn [_] ((:wait! f))))
          (.then (fn [again] (is (not= :attention (:wake again)))))))))

(deftest an-idle-notice-from-before-the-watch-started-does-not-wake-it
  (with-fixture ["200ms"]
    (fn [f]
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (assoc (event :body :idle {:jobs []} "body idle 10 s") :seq 1 :time-ms 1000))
                   (let [req (assoc-in (:req f) [:wait-options :watch] ["j9"])]
                     (observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil))))))
          (.then (fn [wake] (is (not= :idle (:wake wake)))))))))

(deftest a-death-from-before-the-watch-started-does-not-wake-it
  (with-fixture ["200ms"]
    (fn [f]
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (assoc (event :body :died {:cause "zombie"}) :seq 1 :time-ms 1000))
                   (let [req (assoc-in (:req f) [:wait-options :watch] ["j9"])]
                     (observe/wait-observe req (:get! f) nil (fn [_] (js/Promise.resolve nil))))))
          (.then (fn [wake] (is (not= :died (:wake wake)))))))))

(defn wake-over-backlog
  "The wake of a plain wait over events stamped before it began."
  [check & evs]
  (with-fixture ["200ms"]
    (fn [f]
      (-> ((:wait! f))
          (.then (fn [_]
                   (apply push! f (map-indexed (fn [i e] (assoc e :seq (inc i) :time-ms (* 1000 (inc i)))) evs))
                   ((:wait! f))))
          (.then (fn [wake] (is (check (:wake wake)) (pr-str wake))))))))

(deftest a-death-the-body-has-since-respawned-from-does-not-wake-a-plain-wait
  (wake-over-backlog #(not= :died %) (event :body :died {:cause "zombie"}) (event :body :respawned {})))

(deftest the-lethal-hurt-of-a-death-the-body-has-since-respawned-from-does-not-wake-a-plain-wait
  (wake-over-backlog #(not= :danger %) (event :body :hurt {:health 0}) (event :body :died {:cause "generic_kill"}) (event :body :respawned {})))

(deftest a-hit-from-before-the-wait-does-not-wake-it-though-the-body-never-died
  (wake-over-backlog #(not= :danger %) (event :body :hurt {:health 6.5})))

(deftest a-death-from-before-the-wait-with-no-respawn-still-wakes
  (wake-over-backlog #(= :died %) (event :body :died {:cause "zombie"})))

;; Deaths through the wait loop

(defn died-item [result] (when (= :died (:wake result)) result))

(defn plain-job-event-at [kind id seq data] (assoc (plain-job-event kind id data) :seq seq))

(def picked (event :body :picked-up {:item "wheat" :count 1}))

(defn ended-beyond-the-page-wakes
  "The wake of a wait over a page of one event, for `first-event` followed by filler and the end of j1."
  [first-event]
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :page-limit 1)
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (assoc first-event :seq 1) (assoc picked :seq 2) (assoc picked :seq 3)
                          (plain-job-event-at :cancelled "j1" 4 {}))
                   ((:wait! f))))
          (.then (fn [result] (is (= :timeout (:wake result)) (pr-str result))))))))

(deftest a-notice-of-a-job-that-ended-beyond-the-page-does-not-wake
  (ended-beyond-the-page-wakes (notice "j1" :harvest.declined "refused")))

(deftest an-idle-from-before-a-job-ended-beyond-the-page-does-not-wake
  (ended-beyond-the-page-wakes (event :body :idle)))

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
                   (is (= :died (:wake (died-item second-death))))
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

;; settings

(deftest settings-requests-read-reload-set-and-unset
  (let [r #(select-keys (apply request "--world" "w" "ProbeBody" %&) [:path :method :body :settings-mode])]
    (is (= {:path "/settings" :settings-mode {:changed false :raw false}} (r "settings")))
    (is (= {:path "/settings" :settings-mode {:changed true :raw false}} (r "settings" "--changed")))
    (is (= {:path "/settings" :settings-mode {:changed false :raw true}} (r "settings" "--raw")))
    (is (= {:path "/settings/reload" :method "POST" :body "{}"} (r "settings" "reload")))
    (is (= {:path "/settings/set" :method "POST" :body "{:key :jobs.items.smelt/wait-ms, :value 5000}"}
           (r "settings" "set" ":jobs.items.smelt/wait-ms" "5000")))
    (is (= {:path "/settings/unset" :method "POST" :body "{:key :engine.game/follow-tick-rate}"}
           (r "settings" "unset" ":engine.game/follow-tick-rate")))))

(deftest settings-requests-refuse-bad-arguments
  (doseq [[argv pattern] [[["settings" "set" ":a/b"] #"needs a key and an EDN value"]
                          [["settings" "set" "nokey" "1"] #"keyword such as"]
                          [["settings" "set" ":a/b" "{:x"] #"not valid EDN"]
                          [["settings" "set" ":a/b" "5 6"] #"exactly one"]
                          [["settings" "set" ":a/b" "5 :value 7"] #"exactly one"]
                          [["settings" "unset"] #"needs a key"]
                          [["settings" "reload" "x"] #"takes no"]
                          [["settings" "frob"] #"settings needs"]]]
    (is (re-find pattern (:error (apply request "--world" "w" "ProbeBody" argv))) (pr-str argv))))

(def settings-reply
  {:text "{:game-rate 40 :rate-source :packet :frozen? false :physics-ms 50 :settings {:a/b {:value 1 :layer :code :doc \"d\"} :a/c {:value 2 :layer :body :doc \"e\"}}}"})

(deftest settings-prints-the-rates-and-the-keys-without-docs-and-only-changed-ones-on-request
  (async done
    (finish! done
             (-> (series [#(run-main! ["settings"] (constantly settings-reply))
                          #(run-main! ["settings" "--changed"] (constantly settings-reply))
                          #(run-main! ["settings" "--raw"] (constantly settings-reply))
                          #(run-main! ["settings" "set" ":a/b" "3"] (constantly {:status 409 :text "{:ok false :reason \"bad\"}"}))])
                 (.then (fn [[all changed raw refused]]
                          (is (= {:game-rate 40 :rate-source :packet :frozen? false :physics-ms 50
                                  :settings {:a/b {:value 1 :layer :code} :a/c {:value 2 :layer :body}}}
                                 (data/read-edn (:out all))))
                          (is (= {:a/c {:value 2 :layer :body}} (:settings (data/read-edn (:out changed)))))
                          (is (= 40 (:game-rate (data/read-edn (:out changed)))))
                          (is (= "d" (get-in (data/read-edn (:out raw)) [:settings :a/b :doc])))
                          (is (= 1 (:code refused)))
                          (is (= "POST" (:method (first (:seen refused)))))
                          (is (= "{:key :a/b, :value 3}" (:body (first (:seen refused)))))))))))

(deftest a-body-idle-in-the-backlog-is-stale-once-the-jobs-or-the-body-moved-on
  (let [idle (event :body :idle {:jobs [{:job "j73"}]} "body idle 10 s")
        job (fn [kind] (assoc (event :job kind) :context {:job-id "j74"}))
        started (fn [name] (event :action :started {:name name}))]
    (is (false? (observe-status/stale-idle? idle [])))
    (is (false? (observe-status/stale-idle? idle [(event :body :chat)])))
    (is (false? (observe-status/stale-idle? idle [(started "look")])) "an idle-ok job's look keeps it true")
    (is (true? (observe-status/stale-idle? idle [(job :cancelled)])) "the listed jobs changed")
    (is (true? (observe-status/stale-idle? idle [(job :queued)])))
    (is (true? (observe-status/stale-idle? idle [(started "look") (started "moveTo")])) "the body walked")))

(defn idle-wake-check
  "Waits over a backlog body.idle (stamped an hour back) naming job j1, when the engine has recorded `recent-results`; passes the wake to check."
  [recent-results check]
  (with-fixture []
    (fn [f]
      (swap! (:world f) assoc :recent-results recent-results)
      (-> ((:wait! f))
          (.then (fn [_]
                   (push! f (assoc (event :body :idle {:jobs [{:job "j1"}]} "body idle 10 s: j1") :seq 1 :time-ms 1000))
                   ((:wait! f))))
          (.then check)))))

(deftest a-body-idle-naming-an-ended-job-does-not-wake
  (idle-wake-check {"j1" {:status :cancelled}} (fn [wake] (is (= :timeout (:wake wake)) (pr-str wake)))))

(deftest an-old-body-idle-naming-only-live-jobs-still-wakes
  (idle-wake-check {} (fn [wake] (is (= :idle (:wake wake)) (pr-str wake)))))

(deftest a-read-the-busy-body-misses-does-not-end-the-wait-before-its-deadline
  (with-fixture ["300ms"]
    (fn [f]
      (let [missed (atom #{})
            get! (fn [socket endpoint options]
                   (if (contains? @missed endpoint)
                     ((:get! f) socket endpoint options)
                     (do (swap! missed conj endpoint)
                         (js/Promise.reject (doto (js/Error. "slow") (aset "code" "ETIMEDOUT"))))))]
        (-> (observe/wait-observe (:req f) get! nil (fn [_] (js/Promise.resolve nil)))
            (.then (fn [result] (is (= :timeout (:wake result)) (pr-str result)))))))))

(deftest a-notice-of-an-ended-job-is-stale
  (let [a (notice "j1" :build.gave-up "x")]
    (is (observe-status/stale-notice? a [(notice "j1" :build.wrong "y") (assoc (event :job :stopped) :context {:job-id "j1"})]))
    (is (not (observe-status/stale-notice? a [(assoc (event :job :stopped) :context {:job-id "j2"})])))
    (is (not (observe-status/stale-notice? a [])))))

;; Plain status lists what happened since the observer's last look

(deftest missed-summary-sums-damage-and-lists-reflex-ends-and-fight-flee-decisions
  (let [hurt (fn [t amount health causes] (assoc (event :body :hurt {:amount amount :health health :causes causes}) :time-ms t))
        ended (assoc (event :reflex :ended {:outcome :done} "night → shelter: done") :context {:job-id "j8" :reflex-id :night} :time-ms 9000)
        decided (fn [d] (assoc (event :job :hostile.decided {:decision d :mob "zombie"} "fight: zombie") :context {:job-id "j9"} :time-ms 8000))
        events [(hurt 1000 2.5 8.5 ["zombie"]) (decided :fight) (hurt 2000 3 5.5 ["zombie"]) ended (decided :wait)]
        summary (observe-status/missed-summary events 11000)]
    (is (= 3 (:count summary)))
    (is (= [:hurt :hostile.decided :ended] (mapv :event (:items summary))))
    (is (= {:event :hurt :damage 5.5 :lowest-health 5.5 :cause "zombie" :ago-s 9} (first (:items summary))))
    (is (= "night" (:reflex (nth (:items summary) 2))))))


(deftest missed-summary-keeps-the-damage-when-later-events-crowd-the-list
  (let [hurt (assoc (event :body :hurt {:amount 12 :health 1 :causes ["creeper"]}) :time-ms 1000)
        ended (fn [i] (assoc (event :job :completed {} "done") :context {:job-id (str "j" i)} :time-ms (+ 2000 i)))
        summary (observe-status/missed-summary (into [hurt] (map ended (range 6))) 11000)]
    (is (= 7 (:count summary)))
    (is (= :hurt (:event (first (:items summary)))) "the damage and its cause stay in the list")
    (is (= ["j2" "j3" "j4" "j5"] (mapv :job (rest (:items summary)))) "after the latest four")))

(deftest missed-summary-keeps-job-ends-flights-and-deaths
  (let [ended (assoc (event :job :completed {} "done") :context {:job-id "j141"} :time-ms 1000)
        fled (assoc (event :job :hostile.fled {} "fled 92 blocks") :attention :notice :context {:job-id "j9"} :time-ms 5000)
        events [(event :body :chat) ended (event :action :started) fled (event :body :died {:cause "fall"})]
        summary (observe-status/missed-summary events 11000)]
    (is (= 3 (:count summary)))
    (is (= [:completed :hostile.fled :died] (mapv :event (:items summary))))
    (is (= {:event :completed :job "j141" :message "done" :ago-s 10} (first (:items summary))))
    (is (nil? (observe-status/missed-summary [(event :body :chat)] 11000)))))

(deftest missed-summary-folds-a-repeated-notice-into-one-item-with-its-count
  (let [again (fn [i] (assoc (notice "j5" :obtain.no-source (str "no source " i)) :time-ms (* 1000 i)))
        other (assoc (notice "j6" :go-home.too-far "far") :time-ms 9000)
        summary (observe-status/missed-summary (conj (mapv again (range 1 6)) other) 11000)]
    (is (= 2 (:count summary)))
    (is (= [{:event :obtain.no-source :job "j5" :count 5 :message "no source 5" :ago-s 6}
            {:event :go-home.too-far :job "j6" :message "far" :ago-s 2}]
           (:items summary)))))

(deftest missed-summary-lists-a-notice-the-agent-must-decide-on
  (let [warn (assoc (notice "j5" :go-home.too-far "home is 409 blocks away") :time-ms 5000)
        summary (observe-status/missed-summary [(event :body :chat) warn (event :job :tick)] 11000)]
    (is (= [{:event :go-home.too-far :job "j5" :message "home is 409 blocks away" :ago-s 6}] (:items summary)))))

(deftest missed-summary-always-shows-a-death-however-much-came-after
  (let [done (fn [i] (assoc (event :job :completed {} "done") :context {:job-id (str "j" i)} :time-ms (* 1000 i)))
        events (-> [(assoc (event :body :died {:cause "zombie"}) :time-ms 500) (assoc (event :body :respawned {}) :time-ms 600)]
                   (into (map done (range 1 11))))
        summary (observe-status/missed-summary events 20000 3)]
    (is (= 12 (:count summary)))
    (is (= [:died :respawned :completed :completed :completed :completed] (mapv :event (:items summary)))
        "the death and respawn, then the latest max-missed others")
    (is (= ["j7" "j8" "j9" "j10"] (keep :job (:items summary))) "the latest pages win")
    (is (= 3 (:skipped summary)) "events never read are counted")
    (is (not (contains? (observe-status/missed-summary events 20000) :skipped)))))

(deftest missed-reads-every-page-after-the-cursor
  (async done
    (let [state (state-dir)
          file (.join path state "worlds" "w" "observers" "ProbeBody" "agent.edn")
          total 30000
          at (fn [n] (cond (= n 5000) {:seq n :source :body :kind :died :data {:cause "zombie"}}
                           (> n 29990) {:seq n :source :job :kind :completed :context {:job-id (str "j" n)} :data {}}
                           :else {:seq n :source :body :kind :chat :data {}}))
          handler (fn [{:keys [path]}]
                    (if (str/starts-with? path "/events")
                      (let [after (js/parseInt (second (re-find #"after=(\d+)" path)))
                            limit (js/parseInt (second (re-find #"limit=(\d+)" path)))
                            evs (mapv at (range (inc after) (inc (min total (+ after limit)))))]
                        {:text (data/write-edn {:stream-id "s" :latest-seq total :cursor {:stream-id "s" :seq (or (:seq (last evs)) after)} :events evs})})
                      {:text "{:mode :scheduled :current nil :health 20 :food 18}"}))
          [request-fn _] (fake/request-fn handler)
          lines (atom [])]
      (.mkdirSync fs (.dirname path file) #js {:recursive true})
      (.writeFileSync fs file "{:cursor {:stream-id \"s\" :seq 0} :generation \"g\" :seen {}}\n")
      (finish! done
               (-> (observe/main! ["ProbeBody" "--world" "w" "--state" state] {:request-fn request-fn :output #(swap! lines conj %)})
                   (.then (fn [code]
                            (.rmSync fs state #js {:recursive true :force true})
                            (let [out (data/read-edn (apply str @lines))]
                              (is (= 0 code))
                              (is (= [:died :completed :completed :completed :completed] (mapv :event (get-in out [:missed :items])))
                                  "a death 25,000 events back is still shown with the latest four")))))))))

(deftest plain-status-lists-the-events-since-the-checkpoint
  (async done
    (let [state (state-dir)
          file (.join path state "worlds" "w" "observers" "ProbeBody" "agent.edn")
          lines (atom [])
          events [{:seq 1 :source :job :kind :completed :context {:job-id "j141"} :data {} :message "done"}
                  {:seq 2 :source :job :kind :hostile.fled :attention :notice :context {:job-id "j9"} :data {} :message "fled"}]
          handler (fn [{:keys [path]}]
                    (if (str/starts-with? path "/events")
                      {:text (data/write-edn {:stream-id "s" :latest-seq 2 :cursor {:stream-id "s" :seq 2} :events events})}
                      {:text "{:mode :scheduled :current nil :health 20 :food 18}"}))
          [request-fn _] (fake/request-fn handler)]
      (.mkdirSync fs (.dirname path file) #js {:recursive true})
      (.writeFileSync fs file "{:cursor {:stream-id \"s\" :seq 0} :generation \"g\" :seen {}}\n")
      (finish! done
               (-> (observe/main! ["ProbeBody" "--world" "w" "--state" state] {:request-fn request-fn :output #(swap! lines conj %)})
                   (.then (fn [code]
                            (.rmSync fs state #js {:recursive true :force true})
                            (let [out (data/read-edn (apply str @lines))]
                              (is (= 0 code))
                              (is (= [:completed :hostile.fled] (mapv :event (get-in out [:missed :items]))))
                              (is (= 2 (get-in out [:missed :count])))))))))))

;; observe events: a bounded, paged look at past events

(defn timed [n source kind & [{:keys [job level message data]}]]
  (cond-> {:seq n :generation-id "g" :time-ms (* n 1000) :source source :kind kind :level (or level :info) :data (or data {})}
    job (assoc :context {:job-id job :chain [job]})
    message (assoc :message message)))

(def past-events
  (vec (concat [(timed 1 :job :queued {:job "j1"})
                (timed 2 :job :memory_written {:job "j1" :level :debug})
                (timed 3 :reflex :fired {:message "night -> jobs.survival.night"})
                (timed 4 :job :dig-in.done {:job "j1" :message "dug in" :data {:cells 4}})
                (timed 5 :body :hurt {:message "zombie" :data {:damage 3}})
                (timed 6 :body :died {:message "died" :data {:cause "zombie"}})
                (timed 7 :job :queued {:job "j2"})
                (timed 8 :job :go-to.unreachable {:job "j2" :message "gave up"})]
               (map #(timed % :job :explore.leg {:job "j2" :message (str "leg " %)}) (range 9 40)))))

(def long-why (apply str "no source: " (repeat 60 "gathering was tried and ended :no-torches; ")))

(defn events-handler [{:keys [path]}]
  (let [after (js/Number (second (re-find #"after=(\d+)" (or path ""))))
        limit (js/Number (second (re-find #"limit=(\d+)" (or path ""))))]
    (cond
      (= "/snapshot" path) {:text (data/write-edn {:body "ProbeBody" :generation-id "g" :cursor {:stream-id "s" :seq 39}})}
      (str/starts-with? (or path "") "/results?ids=") {:text (data/write-edn {:results {"j9" {:status :stopped :result {:why long-why} :text long-why}}})}
      :else {:text (data/write-edn {:stream-id "s" :latest-seq 39 :gap? false
                                    :events (vec (take limit (filter #(> (:seq %) after) past-events)))})})))

(defn events-out [& argv]
  (-> (run-main! (into ["events"] argv) events-handler)
      (.then (fn [r] (assoc r :view (data/read-edn (:out r)))))))

(deftest observe-events-validates-its-options-before-connecting
  (is (nil? (:error (request "--world" "w" "Probe" "events" "--kinds" "died,hurt" "--job" "j2" "--limit" "10" "--until" "death" "--since" "5m"))))
  (doseq [argv [["events" "--limit" "100"] ["events" "--kinds" "Bad Kind"] ["events" "--job" "x"] ["events" "--since" "soon"]
                ["events" "--before" "-1"] ["events" "extra"] ["status" "--kinds" "died"]]]
    (is (string? (:error (apply request "--world" "w" "Probe" argv))) (pr-str argv))))

(deftest observe-events-filters-by-kind-and-job-and-leaves-out-debug-noise
  (async done
    (finish! done
             (-> (series [#(events-out "--kinds" "died,hurt") #(events-out "--job" "j1") #(events-out "--job" "j1" "--level" "debug")])
                 (.then (fn [[kinds job debug]]
                          (is (= [:hurt :died] (mapv :kind (:events (:view kinds)))))
                          (is (= [:queued :dig-in.done] (mapv :kind (:events (:view job)))) "j1 without the debug memory write")
                          (is (= [:queued :memory_written :dig-in.done] (mapv :kind (:events (:view debug)))))
                          (is (= {:cells 4} (:data (last (:events (:view job))))))))))))

(deftest observe-events-for-a-job-finds-its-recorded-stop-whole-when-the-event-has-aged-out
  (async done
    (finish! done
             (-> (events-out "--job" "j9" "--kinds" "stopped")
                 (.then (fn [{:keys [view]}]
                          (is (= [:stopped] (mapv :kind (:events view))))
                          (is (= long-why (:message (first (:events view)))))
                          (is (= long-why (get-in view [:events 0 :data :why])))))))))

(deftest observe-events-until-death-shows-what-led-up-to-it-and-pages-back
  (async done
    (finish! done
             (-> (series [#(events-out "--until" "death" "--limit" "3") #(events-out "--until" "death" "--limit" "3" "--before" "4")])
                 (.then (fn [[newest older]]
                          (is (= [:dig-in.done :hurt :died] (mapv :kind (:events (:view newest)))) "nothing after the death")
                          (is (= 4 (:next-before (:view newest))))
                          (is (= [:queued :fired] (mapv :kind (:events (:view older)))))
                          (is (not (contains? (:view older) :next-before)) "nothing older is left")))))))

(deftest the-events-window-says-how-many-older-events-were-not-searched
  (let [events [{:seq 51} {:seq 52}]
        history {:snapshot {:cursor {:seq 100}} :truncated? true}]
    (is (= 50 (:older-not-searched (observe-events/window history events))))
    (is (re-find #"50 older" (:note (observe-events/window history events))))
    (is (nil? (:note (observe-events/window {:snapshot {:cursor {:seq 2}}} [{:seq 1} {:seq 2}]))))))

(deftest a-kind-without-its-prefix-matches-the-dotted-kinds-ending-in-it
  (let [match (observe-events/matcher {:kinds #{"broke"}})]
    (is (match {:seq 1 :kind :tool.broke :level :warn}))
    (is (not (match {:seq 2 :kind :tool.low :level :warn})))
    (is (not (match {:seq 3 :kind :unbroke :level :warn})))))

(deftest kept-events-reads-every-log-file-for-the-kinds-asked
  (async done
    (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "kept-"))
          line (fn [seq kind] (str "{:seq " seq ", :source :body, :kind :" kind "}\n"))]
      (.writeFileSync fs (.join path dir "events.edn.1") (str (line 1 "hurt") (line 2 "planned") (line 3 "died")))
      (.writeFileSync fs (.join path dir "events.edn") (str (line 4 "planned") (line 5 "hurt")))
      (.writeFileSync fs (.join path dir "events.edn.debug") (line 9 "hurt"))
      (finish! done
               (-> (observe-events/kept-events! (.join path dir "events.sock") #{"hurt"})
                   (.then (fn [events]
                            (is (= [1 3 5] (mapv :seq events)) "hurts and deaths from the old file too, oldest first, no debug chain")
                            (.rmSync fs dir #js {:recursive true :force true})))))))
  )

(deftest observe-events-without-filters-is-the-newest-page-and-is-bounded
  (async done
    (finish! done
             (-> (events-out)
                 (.then (fn [{:keys [view]}]
                          (is (= (range 20 40) (map :seq (:events view))) "20 newest, oldest first")
                          (is (= 20 (:next-before view)))))))))

(deftest a-timed-out-read-is-tried-once-more
  (async done
    (let [calls (atom 0)
          timeout (fn [] (js/Promise.reject (observe-status/coded "ETIMEDOUT" "slow")))
          attempt (fn [] (if (= 1 (swap! calls inc)) (timeout) (js/Promise.resolve :ok)))]
      (finish! done
        (-> (observe/retry-timeout attempt)
            (.then (fn [v] (is (= :ok v)) (is (= 2 @calls)))
                   )
            (.then (fn [_] (reset! calls 0) (observe/retry-timeout (fn [] (swap! calls inc) (timeout)))))
            (.then (fn [_] (is false "second timeout must reject"))
                   (fn [e] (is (= "ETIMEDOUT" (observe-status/code-of e))) (is (= 2 @calls)))))))))
