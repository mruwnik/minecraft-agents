(ns agent-tools.drive-test
  (:require [cljs.test :refer [deftest is are async]]
            [agent-tools.drive :as drive]
            [agent-tools.fake-socket :as fake]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn coded [code] (doto (js/Error. "x") (aset "code" code)))

(defn post [body] {:method "POST" :path "/drive" :body body})

(deftest request-for-builds-the-fixed-drive-requests
  (doseq [[argv expected]
          [[["Bob" "take" "--why" "poking"] (post {:op "take" :who "claude" :why "poking"})]
           [["Bob" "take"] (post {:op "take" :who "claude" :why ""})]
           [["Bob" "take" "--idle-s" "120" "--why" "x"] (post {:op "take" :who "claude" :why "x" :idleS 120})]
           [["Bob" "hold" "forward" "500" "--who" "view"] (post {:op "set" :who "view" :controls {:forward true} :ms 500})]
           [["Bob" "hold" "forward,sprint" "1000"] (post {:op "set" :who "claude" :controls {:forward true :sprint true} :ms 1000})]
           [["Bob" "look" "180" "-10"] (post {:op "set" :who "claude" :look {:yaw 180 :pitch -10}})]
           [["Bob" "turn" "15"] (post {:op "set" :who "claude" :look {:dyaw 15 :dpitch 0}})]
           [["Bob" "turn" "-15" "5"] (post {:op "set" :who "claude" :look {:dyaw -15 :dpitch 5}})]
           [["Bob" "jump"] (post {:op "set" :who "claude" :controls {:jump true} :ms 300})]
           [["Bob" "stop"] (post {:op "stop" :who "claude"})]
           [["Bob" "ping"] (post {:op "ping" :who "claude"})]
           [["Bob" "release"] (post {:op "release" :who "claude"})]
           [["Bob" "release" "--force"] (post {:op "release" :who "claude" :force true})]
           [["Bob" "state"] {:method "GET" :path "/drive" :body nil}]]]
    (let [r (drive/request-for (into argv ["--world" "w"]))]
      (is (nil? (:error r)) (pr-str argv))
      (is (= "Bob" (:agent r)))
      (is (= expected (select-keys r [:method :path :body])) (pr-str argv)))))

(deftest request-for-rejects-bad-usage
  (doseq [argv [[] ["Bob"] ["Bob" "dance"] ["Bob" "hold"] ["Bob" "hold" "forward"] ["Bob" "hold" "forward" "abc"]
                ["Bob" "hold" "forward" "1.5"] ["Bob" "hold" "fly" "100"] ["Bob" "look" "10"] ["Bob" "look" "a" "b"]
                ["Bob" "turn"] ["Bob" "turn" "x"] ["Bob" "take" "--bogus"]]]
    (is (string? (:error (drive/request-for (into argv ["--world" "w"])))) (pr-str argv))))

(deftest request-for-addresses-a-body-in-its-world
  (is (= "w2" (:world (drive/request-for ["Bob" "state" "--world" "w2"]))))
  (is (re-find #"missing --world <world>" (:error (drive/request-for ["Bob" "state"]))))
  (is (re-find #"world" (:error (drive/request-for ["Bob" "state" "--world" "../x"]))))
  (is (= "/x/y" (:state (drive/request-for ["Bob" "state" "--world" "w" "--state" "/x/y"]))))
  (is (= {:worldsDir (.join path map-tool/default-state-dir "worlds")}
         (:state (drive/request-for ["Bob" "state" "--world" "w"])))))

(deftest request-for-names-a-bad-idle-seconds
  (doseq [argv [["Bob" "take" "--idle-s" "abc"] ["Bob" "take" "--idle-s" ""]]]
    (is (re-find #"idle-s" (:error (drive/request-for (into argv ["--world" "w"])))) (pr-str argv))))

(deftest socket-path-for-builds-the-control-socket-path
  (is (= "/s/worlds/w/agents/Bob/engine/control.sock" (drive/socket-path-for {:state "/s" :world "w" :agent "Bob"}))))

(deftest exit-code-is-zero-only-for-an-ok-2xx-reply
  (doseq [[reply code] [[{:status 200 :json {:ok true}} 0]
                        [{:status 200 :json {:ok false :reason "offline"}} 1]
                        [{:status 400 :json {:ok false}} 1]
                        [{:status 404 :json {}} 1]
                        [{:status 200 :json {}} 1]
                        [{:status 200 :json nil} 1]]]
    (is (= code (drive/exit-code-for reply)) (pr-str reply))))

(deftest failure-text-distinguishes-the-error-codes
  (are [code expected] (= expected (drive/failure-text (coded code) "Bob" "/no/such/socket"))
    "ETIMEDOUT" "drive request to Bob timed out after 3 s"
    "ERESPONSETOOLARGE" "drive response from Bob exceeded 64 KB"
    "ECONNRESET" "connection to Bob was reset"
    "EWHATEVER" "drive request to Bob failed (EWHATEVER)"
    "ENOENT" "no running body Bob (no control socket at /no/such/socket)"
    "ECONNREFUSED" "no running body Bob (no control socket at /no/such/socket)"))

(deftest request-options-bound-time-and-reply-size
  (let [options (drive/request-options "/s" {:method "GET" :path "/drive" :body nil})]
    (is (= 3000 (:timeout-ms options)))
    (is (= 65536 (:max-bytes options)))))

(defn run-main! [argv handler]
  (let [[request-fn seen] (fake/request-fn handler)
        lines (atom [])
        state (.mkdtempSync fs (.join path (.tmpdir os) "drive-cli-"))]
    (-> (drive/main! (into argv ["--world" "w" "--state" state]) {:request-fn request-fn :output #(swap! lines conj %)})
        (.then (fn [code] (.rmSync fs state #js {:recursive true :force true}) {:code code :out (apply str @lines) :seen @seen})))))

(deftest stop-cancels-the-running-slot-job-then-stops
  (let [stops (atom 0)]
    (async done
      (-> (run-main! ["Bob" "stop" "--who" "Wren"]
                     (fn [{:keys [path]}]
                       (case path
                         "/drive" (if (= 1 (swap! stops inc))
                                    {:status 409 :content-type "application/json" :text "{\"ok\":false,\"reason\":\"job-running\",\"job\":\"j3\"}"}
                                    {:content-type "application/json" :text "{\"ok\":true}"})
                         "/snapshot" {:text "{:generation-id \"g\"}"}
                         {:text "{:ok true}"})))
          (.then (fn [{:keys [code seen]}]
                   (is (= 0 code))
                   (is (= ["/drive" "/snapshot" "/jobs" "/drive"] (mapv :path seen)))
                   (is (= {:op :cancel :id "j3" :by "Wren"} (select-keys (data/read-edn (:body (nth seen 2))) [:op :id :by])))
                   (done)))))))

(deftest a-stop-that-is-not-refused-sends-no-cancel
  (async done
    (-> (run-main! ["Bob" "stop"] (fn [_] {:content-type "application/json" :text "{\"ok\":true}"}))
        (.then (fn [{:keys [code seen]}]
                 (is (= 0 code))
                 (is (= ["/drive"] (mapv :path seen)))
                 (done))))))
