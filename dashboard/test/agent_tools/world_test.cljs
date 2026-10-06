(ns agent-tools.world-test
  (:require [cljs.test :refer [deftest is are async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.world :as world]
            [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn fails-with [pattern argv]
  (let [error (:error (world/request-for argv))]
    (and (string? error) (boolean (re-find pattern error)))))

(deftest builds-a-bounded-move-to-submission-with-a-request-id
  (let [request (world/request-for ["--world" "w" "Probe" "submit" "move-to" "1" "64" "-2" "--timeout-s" "4" "--who" "claude"])]
    (is (nil? (:error request)))
    (is (= :submit (get-in request [:body :op])))
    (is (= :move-to (get-in request [:body :action])))
    (is (re-matches #"[0-9a-f-]{36}" (get-in request [:body :request-id])))
    (is (= {:pos {:x 1 :y 64 :z -2} :timeoutS 4} (get-in request [:body :args])))))

(deftest a-submission-can-be-retried-with-the-same-request-id
  (is (= "retry-1" (get-in (world/request-for ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--request-id" "retry-1"])
                           [:body :request-id]))))

(deftest rejects-options-that-do-not-fit-the-command
  (doseq [[pattern argv] [[#"body name" ["--world" "w" "../etc" "inventory"]]
                          [#"only valid for submit" ["--world" "w" "Probe" "status" "op-1" "--request-id" "ignored"]]
                          [#"not valid for dig" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--range" "-1"]]
                          [#"only valid with submit" ["--world" "w" "Probe" "inventory" "--item" "bread"]]
                          [#"--request-id must be" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--request-id" "bad id"]]
                          [#"--who must be" ["--world" "w" "Probe" "inventory" "--who" ""]]]]
    (is (fails-with pattern argv) (pr-str argv))))

(deftest maps-status-cancel-inventory-and-interactions-to-fixed-operations
  (doseq [[argv op] [[["status" "op-1"] :status] [["cancel" "op-1"] :cancel] [["inventory"] :inventory]]]
    (is (= op (get-in (world/request-for (into ["--world" "w" "Probe"] argv)) [:body :op])) (pr-str argv)))
  (let [place (world/request-for ["--world" "w" "Probe" "submit" "place" "1" "64" "2" "oak_planks"])]
    (is (= :place (get-in place [:body :action])))
    (is (= {:pos {:x 1 :y 64 :z 2} :item "oak_planks"} (get-in place [:body :args]))))
  (is (= 17 (get-in (world/request-for ["--world" "w" "Probe" "submit" "interact" "17"]) [:body :args :id])))
  (is (= {:action :wear :args {:item "iron_helmet"}}
         (select-keys (:body (world/request-for ["--world" "w" "Probe" "submit" "wear" "iron_helmet"])) [:action :args])))
  (is (= {} (get-in (world/request-for ["--world" "w" "Probe" "submit" "wear"]) [:body :args])))
  (is (= {:pos {:x 1 :y 64 :z 2} :item "bread" :face "up"}
         (get-in (world/request-for ["--world" "w" "Probe" "submit" "use-on" "1" "64" "2" "--item" "bread" "--face" "up"])
                 [:body :args]))))

(deftest rejects-malformed-commands
  (doseq [[pattern argv] [[#"unknown action delete-file" ["--world" "w" "Probe" "submit" "delete-file"]]
                          [#"unknown action undefined" ["--world" "w" "Probe" "submit"]]
                          [#"needs x y z" ["--world" "w" "Probe" "submit" "dig" "1" "64"]]
                          [#"needs x y z item" ["--world" "w" "Probe" "submit" "place" "1" "64" "2"]]
                          [#"x must be a finite number" ["--world" "w" "Probe" "submit" "dig" "x" "64" "2"]]
                          [#"x must be a finite number" ["--world" "w" "Probe" "submit" "dig" "" "64" "2"]]
                          [#"--range must be a finite number" ["--world" "w" "Probe" "submit" "move-to" "1" "64" "2" "--range" " "]]
                          [#"request-id" ["--world" "w" "Probe" "status"]]
                          [#"unknown command" ["--world" "w" "Probe" "dance"]]]]
    (is (fails-with pattern argv) (pr-str argv))))

(deftest failure-text-does-not-claim-a-reset-body-is-absent
  (are [code expected] (= expected (world/failure-text (doto (js/Error. "x") (aset "code" code)) "/no/such/socket"))
    "ECONNRESET" "connection was reset"
    "EWHATEVER" "request failed (EWHATEVER)"
    "ENOENT" "no running body (no socket at /no/such/socket)"))

(deftest a-handler-failure-after-a-sent-submit-is-reported-as-unconfirmed
  (let [argv ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--request-id" "dig-9"]
        [bad-edn] (fake/request-fn (fn [_] {:text "{:ok"}))
        [good-edn] (fake/request-fn (fn [_] {:text "{:ok true}"}))
        lines (atom [])]
    (async done
      (-> (world/main! argv {:request-fn bad-edn :output #(swap! lines conj %)})
          (.then (fn [code]
                   (is (= 1 code))
                   (is (re-find #"world-unavailable" (apply str @lines)))))
          (.then (fn [_] (world/main! argv {:request-fn good-edn :output (fn [_] (throw (js/Error. "EPIPE")))})))
          (.then (fn [code] (is (= 2 code))))
          (.then (fn [_] (done)))))))

(deftest a-body-is-addressed-in-its-world
  (is (fails-with #"missing --world <world>" ["Probe" "inventory"]))
  (is (fails-with #"world" ["Probe" "inventory" "--world" "../x"]))
  (let [r (world/request-for ["Probe" "inventory" "--world" "w" "--state" "/s"])]
    (is (= ["w" "Probe" "/s"] [(:world r) (:agent r) (:state r)]))))

(deftest the-body-is-written-as-edn-in-the-engine-s-key-order
  (is (= "{:op :submit :who \"claude\" :request-id \"r1\" :action :move-to :args {:pos {:x 1 :y 64 :z -2} :timeoutS 4}}"
         (world/body-edn (:body (world/request-for ["--world" "w" "Probe" "submit" "move-to" "1" "64" "-2" "--timeout-s" "4" "--request-id" "r1"]))))))

;; submit --wait

(deftest wait-and-its-timeout-belong-to-submit
  (is (= {:timeout "2m"} (:wait (world/request-for ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--wait" "--timeout" "2m"]))))
  (is (nil? (:wait (world/request-for ["--world" "w" "Probe" "submit" "dig" "1" "64" "2"]))))
  (doseq [[pattern argv] [[#"--wait requires submit" ["--world" "w" "Probe" "status" "op-1" "--wait"]]
                          [#"--timeout requires --wait" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--timeout" "5s"]]
                          [#"--timeout must be" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--wait" "--timeout" "x"]]]]
    (is (fails-with pattern argv) (pr-str argv))))

(defn action-event [n kind data]
  {:seq n :generation-id "g" :time-ms n :source :action :kind kind :context {:action-id "dig-1"} :data data})

(deftest submit-wait-returns-the-operation-and-the-action-result
  (let [state (.mkdtempSync fs (.join path (.tmpdir os) "world-cli-"))
        events (atom [])
        engine (fn [{:keys [method path]}]
                 (let [cursor {:stream-id "s" :seq (count @events)}]
                   {:text (data/write-edn
                            (cond
                              (= "POST" method) {:ok true :operation {:request-id "dig-1" :action :dig :status :queued}
                                                 :behind "use-1" :position 1}
                              (= "/snapshot" path) {:generation-id "g" :body "Probe" :outstanding {} :cursor cursor}
                              :else (let [after (js/Number (.get (.-searchParams (js/URL. (str "http://x" path))) "after"))]
                                      {:gap? false :stream-id "s" :latest-seq (count @events) :cursor cursor
                                       :events (filterv #(> (:seq %) after) @events)})))}))
        [request-fn seen] (fake/request-fn engine)
        lines (atom [])]
    (js/setTimeout #(swap! events into [(action-event 1 :started {:name "dig"})
                                        (action-event 2 :done {:name "dig" :status "dug" :result {:status "dug" :block "stone"}})])
                   60)
    (async done
      (-> (world/main! ["--world" "w" "Probe" "--state" state "submit" "dig" "1" "64" "2" "--request-id" "dig-1" "--wait" "--timeout" "3s"]
                       {:request-fn request-fn :output #(swap! lines conj %)})
          (.then (fn [code]
                   (let [result (data/read-edn (apply str @lines))]
                     (is (= 0 code))
                     (is (= [:queued "use-1"] [(get-in result [:operation :status]) (:behind result)]))
                     (is (= {:wake :action-finished :action "dig-1" :result {:status "dug" :block "stone"}} (:wait result)))
                     (is (re-find #"control\.sock$" (:socket-path (first @seen))) "submitted on the control socket")
                     (is (re-find #"events\.sock$" (:socket-path (last @seen))) "waited on the events socket"))))
          (.then (fn [_] (.rmSync fs state #js {:recursive true :force true}) (done)))))))
