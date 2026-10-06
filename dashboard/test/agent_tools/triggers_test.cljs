(ns agent-tools.triggers-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.fake-socket :as fake]
            [agent-tools.triggers :as triggers]
            [agent-tools.world-data :as data]))

(defn request [& args] (triggers/request-for (into ["Probe" "--world" "test"] args)))

(deftest ad-hoc-and-predefined-upserts-preserve-native-edn-list-and-map-values
  (let [a (request "add" "near-home" "--when" "(and (< (inventory \"bread\") 8) (< (distance-to (place :home)) 16))"
                   "--job" "(jobs.movement.look-around)" "--for" "10m")
        text (data/write-edn (:request a))
        b (request "put" "health-watch" "--trigger" "health-low" "--args" "{:health 12}" "--backoff" "{:base-ms 1000}")
        b-text (data/write-edn (:request b))]
    (is (= :put (get-in a [:request :op])))
    (is (= 600 (get-in a [:request :ttl-s])))
    (is (re-find #":when \(and \(< \(inventory \"bread\"\) 8\)" text))
    (is (re-find #":job \(jobs.movement.look-around\)" text))
    (is (re-find #":args \{:health 12\}" b-text))
    (is (re-find #":backoff \{:base-ms 1000\}" b-text))))

(deftest mute-unmute-order-and-reset-map-onto-the-register-api
  (doseq [[argv path expected] [[["mute" "hungry" "--for" "30s"] [:request :ttl-s] 30]
                                [["unmute" "hungry"] [:request :property] :mute]
                                [["move" "hungry" "--before" "health-low" "--for" "2m"] [:request :above] :health-low]
                                [["move" "hungry" "--after" "health-low"] [:request :below] :health-low]
                                [["reset" "hungry" "--property" "position"] [:request :op] :clear]
                                [["remove" "custom"] [:request :op] :remove]]]
    (is (= expected (get-in (apply request argv) path)) (pr-str argv)))
  (doseq [argv [["move" "hungry" "--before" "hungry"]
                ["move" "hungry" "--before" "x" "--after" "y"]
                ["reset" "hungry"]]]
    (is (string? (:error (apply request argv))) (pr-str argv))))

(deftest validation-requires-world-name-one-form-compatible-flags-and-bounded-durations
  (is (string? (:error (triggers/request-for ["Probe" "list"]))))
  (is (string? (:error (triggers/request-for ["../body" "--world" "test" "list"]))))
  (doseq [argv [["add" "x" "--when" "true"]
                ["put" "x" "--when" "true" "--trigger" "hungry" "--job" "(jobs.movement.look-around)"]
                ["remove" "x" "--for" "30s"]
                ["list" "--limit" "99"]]]
    (is (string? (:error (apply request argv))) (pr-str argv)))
  (doseq [text ["(one) (two)" "1) (2" (apply str (repeat 13000 "x"))]]
    (is (thrown? js/Error (triggers/one-form text)) text))
  (is (string? (:error (apply request ["add" "x" "--when" "true" "--job" "(jobs.movement.look-around)" "--cooldown-s" ""]))))
  (is (= 'x (triggers/one-form "x")))
  (doseq [text ["0s" "forever"]]
    (is (thrown? js/Error (triggers/duration text)) text))
  (is (= 0.25 (triggers/duration "250ms"))))

(def listing
  (data/read-edn "{:ok true :generation-id \"uuid\" :total 2 :order [:first :second] :items [{:id :first :trigger :condition :job (jobs.movement.look-around) :when (and (< (inventory \"bread\") 8) (< (distance-to (place :home)) 16)) :builtin? false} {:id :second :trigger :hungry :builtin? true :muted {:until 123} :job (jobs.survival.eat)}]}"))

(deftest list-is-paged-and-compact-and-detail-exposes-bounded-conditions
  (let [paged (triggers/compact (request "list" "--limit" "1") listing)
        item (first (:items paged))]
    (is (= 1 (count (:items paged))))
    (is (= 1 (:next-offset paged)))
    (is (= "jobs.movement.look-around" (:job item)))
    (is (= 'and (first (:when item))))
    (is (= '< (first (second (:when item)))))
    (is (re-find #":when \(and \(< \(inventory \"bread\"\) 8\)" (data/write-edn paged)))
    (is (not (contains? paged :generation-id)))
    (is (= 1 (:priority item))))
  (is (= [1 2] (mapv :priority (:items (triggers/compact (request "list") (assoc listing :order [:second :first]))))))
  (is (not (contains? (second (:items (triggers/compact (request "list") (assoc listing :order [:first])))) :priority)))
  (let [shown (triggers/compact (request "show" "first") (assoc listing :explain {:terms (vec (repeat 100 {:form true :value true}))}))
        timed (triggers/compact (request "show" "first")
                                (assoc listing :explain {:terms [{:form (list 'held-for 5) :value false :remaining-ms 2500}]}))]
    (is (= :first (:id shown)))
    (is (= 16 (count (:explain shown))))
    (is (= 2500 (get-in timed [:explain 0 :remaining-ms]))))
  (is (= :trigger-not-found (:reason (triggers/compact (request "show" "unknown") listing))))
  (is (= {:ok true :id :first :op :mute :muted true}
         (triggers/compact (request "mute" "first")
                           {:ok true :id :first :op :mute :trigger {:muted {:until 123} :job (vec (repeat 100 "large"))}}))))

(deftest list-is-in-priority-order-with-unranked-last
  (let [ids #(mapv :id (:items (triggers/compact (request "list") %)))]
    (is (= [:second :first] (ids (assoc listing :order [:second :first]))))
    (is (= [:first :second] (ids (assoc listing :order [:first]))))
    (is (= [:second :first] (ids (assoc listing :order [:second]))))
    (is (= [:second] (ids (assoc listing :order [:second] :items [{:id :second}])))))
  (is (= :first (:id (first (:items (triggers/compact (request "list" "--limit" "1") (assoc listing :order [:first :second]))))))))

(deftest structured-compiler-errors-remain-data-but-bounded
  (let [r (triggers/compact (request "add" "x" "--when" "(unknown)" "--job" "(jobs.movement.look-around)")
                            {:ok false :reason :bad-condition :at [:when] :message (apply str (repeat 2000 "x"))
                             :condition {:reason :unknown-function :allowed (vec (repeat 100 "func"))}})]
    (is (= :bad-condition (:reason r)))
    (is (= 240 (count (:message r))))
    (is (= 16 (count (get-in r [:condition :allowed]))))))

(deftest mutation-transport-has-a-finite-total-deadline
  (async done
    (-> (triggers/post! "/unused" {:op :mute} {:timeout-ms 5 :request-fn (fn [_ _] (fake/pending-request))})
        (.then (fn [_] (is false "resolved")))
        (.catch (fn [error] (is (= "ETIMEDOUT" (.-code error)))))
        (.then done))))

(defn run-main! [argv handler]
  (let [[request-fn seen] (fake/request-fn handler)
        lines (atom [])]
    (-> (triggers/main! (into ["Probe" "--world" "test"] argv)
                        {:request-fn request-fn :output #(swap! lines conj %)})
        (.then (fn [code] {:code code :out (apply str @lines) :seen @seen})))))

(deftest a-mutation-reads-the-generation-then-posts-it
  (async done
    (-> (run-main! ["mute" "first"]
                   (fn [{:keys [method]}]
                     {:text (if (= "GET" method)
                              "{:ok true :generation-id \"g1\"}"
                              "{:ok true :id :first :op :mute :trigger {:muted {:until 5}}}")}))
        (.then (fn [{:keys [code out seen]}]
                 (is (= 0 code))
                 (is (= ["GET" "POST"] (mapv :method seen)))
                 (is (= {:op :mute :id :first :by "agent" :generation-id "g1"} (data/read-edn (:body (second seen)))))
                 (is (= {:ok true :id :first :op :mute :muted true} (data/read-edn out)))))
        (.then done))))

(deftest an-engine-without-the-route-says-restart-with-the-current-build
  (async done
    (-> (run-main! ["list"] (fn [_] {:status 404 :text "{:ok false :reason :not-found}"}))
        (.then (fn [{:keys [code out]}]
                 (is (= 2 code))
                 (is (= {:ok false :reason :triggers-unavailable :action :restart-with-current-build} (data/read-edn out)))))
        (.then done))))

(deftest a-transport-failure-after-sending-leaves-confirmation-unknown
  (async done
    (-> (run-main! ["mute" "first"]
                   (fn [{:keys [method]}]
                     (if (= "GET" method) {:text "{:ok true :generation-id \"g1\"}"} {:error "ECONNREFUSED"})))
        (.then (fn [{:keys [code out]}]
                 (is (= 2 code))
                 (is (= {:ok false :reason :no-running-body :confirmation :unknown} (select-keys (data/read-edn out) [:ok :reason :confirmation])))))
        (.then done))))

(deftest a-transport-failure-before-sending-claims-nothing-about-confirmation
  (async done
    (-> (run-main! ["list"] (fn [_] {:error "ECONNREFUSED"}))
        (.then (fn [{:keys [code out]}]
                 (is (= 2 code))
                 (is (= {:ok false :reason :no-running-body} (data/read-edn out)))))
        (.then done))))

(deftest a-predefined-trigger-without-job-leaves-the-job-to-the-trigger-default
  (let [r (request "add" "health-watch" "--trigger" "health-low")]
    (is (= {:op :put :id :health-watch :trigger :health-low :by "agent"} (:request r)))))

(deftest upgrade-is-a-mutation-without-an-id
  (is (= {:op :upgrade :by "agent"} (:request (request "upgrade"))))
  (is (true? (:mutating (request "upgrade"))))
  (is (string? (:error (request "upgrade" "--for" "1m")))))

(deftest upgrade-and-decline-take-optional-ids
  (is (= {:op :upgrade :by "agent" :ids [:night :wedged]} (:request (request "upgrade" "night" ":wedged"))))
  (is (= {:op :decline :by "agent" :ids [:wedged]} (:request (request "decline" "wedged"))))
  (is (= {:op :decline :by "agent"} (:request (request "decline"))))
  (is (string? (:error (request "decline" "Bad Id")))))

(deftest upgrade-and-decline-replies-show-what-is-left-offered
  (is (= {:ok true :op :decline :declined [:wedged] :offered [:night]}
         (into {} (triggers/compact {:command :decline} {:ok true :declined [:wedged] :offered [:night]})))))

(deftest upgrade-and-decline-report-ignored-ids-with-a-reason
  (let [d (into {} (triggers/compact (request "decline" "nonsense" "wedged") {:ok true :declined [:wedged] :offered []}))
        u (into {} (triggers/compact (request "upgrade" "night") {:ok true :added [] :offered []}))
        none (into {} (triggers/compact (request "decline") {:ok true :declined [] :offered []}))]
    (is (= [:nonsense] (mapv :id (:ignored d))))
    (is (string? (:reason (first (:ignored d)))))
    (is (= [:night] (mapv :id (:ignored u))))
    (is (not (contains? none :ignored)))))

(deftest bad-id-message-says-ids-come-from-the-offer
  (is (re-find #"offer" (:error (request "upgrade" "Night")))))

(deftest failure-for-keeps-the-transport-reason
  (doseq [[code reason] [["ENOENT" :no-running-body] ["ETIMEDOUT" :timeout] ["ERESPONSETOOLARGE" :response-too-large]
                         ["EACCES" :socket-access-denied] ["EPIPE" :transport-error]]]
    (is (= reason (:reason (triggers/failure-for (let [e (js/Error. "x")] (aset e "code" code) e) false))) code)))
