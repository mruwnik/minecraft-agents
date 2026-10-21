(ns agent-tools.progress-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.progress :as progress]
            [agent-tools.world-data :as data]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

;; Requests

(defn request [& args] (progress/request-for (vec args)))

(deftest want-lists-items-with-optional-counts
  (is (= [["iron_ingot" 24] ["iron_pickaxe" 1]] (progress/parse-want "iron_ingot:24,iron_pickaxe")))
  (is (thrown? js/Error (progress/parse-want "Iron Ingot")))
  (is (thrown? js/Error (progress/parse-want "iron_ingot:0"))))

(deftest the-request-binds-the-body-and-reads-wait-and-stall-durations
  (let [r (request "Probe" "--world" "w" "--state" "/tmp/s" "--goal" "j14" "--want" "iron_ingot:3" "--wait" "60m" "--stall" "5m")]
    (is (= "Probe" (get-in r [:base :agent])))
    (is (str/ends-with? (get-in r [:base :socket-path]) "/worlds/w/agents/Probe/engine/events.sock"))
    (is (= "j14" (:goal r)))
    (is (= [["iron_ingot" 3]] (:want r)))
    (is (= 3600000 (:wait-ms r)))
    (is (= 300000 (:stall-ms r))))
  (is (= 600000 (:stall-ms (request "Probe" "--world" "w" "--wait" "1m"))) "stall defaults to 10 minutes")
  (is (nil? (:wait-ms (request "Probe" "--world" "w"))) "no --wait: the one line")
  (is (:error (request "Probe" "--world" "w" "--wait" "soon")))
  (is (:error (request "Probe" "--world" "w" "--goal" "fourteen")))
  (is (:error (request "Probe" "--world" "w" "--stall" "5m")) "--stall needs --wait"))

;; The line

(def busy-status
  {:mode :scheduled :health 18 :food 15 :pos [10 64 -3]
   :current {:id "j14" :name "(jobs.items.obtain {:item \"iron_pickaxe\"})" :status :running}
   :jobs {:total 2 :items [{:id "j15" :name "(jobs.gather.mine)" :status :queued :waiting {:reason :no-source :item "iron_ore"}}
                           {:id "j16" :name "(jobs.items.smelt)" :status :queued :waiting {:reason :cooking}}]}
   :attention {:total 1 :items [{:id "r1" :job "j15" :reason :fruitless :message "every act failed"}]}})

(def inventory {:counts {"iron_ingot" 3 "cobblestone" 40} :equipment {:head {:name "iron_helmet"}}})

(deftest needs-you-lists-only-what-the-autopilot-cannot-clear
  (let [needs (progress/needs-you busy-status)]
    (is (some #(str/includes? % "no-source") needs) "a job waiting for a source the agent must find")
    (is (some #(str/includes? % "fruitless") needs) "an attention request")
    (is (not-any? #(str/includes? % "cooking") needs) "cooking finishes by itself")))

(deftest needs-you-lists-muted-triggers
  (is (some #(= "muted hostile,eat" %) (progress/needs-you {:muted-triggers [:hostile "eat"] :current {:id "j1"}})))
  (is (empty? (progress/needs-you {:current {:id "j1"}}))))

(deftest a-death-wake-says-whether-the-goal-job-survived
  (doseq [[survived goal text] [[["j14"] "j14" "goal j14 kept"]
                                [[] "j14" "goal j14 lost, re-submit"]
                                [["j2"] "j14" "goal j14 lost, re-submit"]
                                [["j2" "j3"] nil "kept j2,j3"]]]
    (is (str/includes? (:text (progress/wake-verdict {:wake :died :cause "zombie" :survived survived} goal)) text))))

(deftest needs-you-names-idle-death-offline-and-manual
  (is (some #(str/includes? % "no jobs") (progress/needs-you {:idle true :health 20})))
  (is (some #(str/includes? % "died") (progress/needs-you {:died {:ago-s 30 :cause "zombie"} :current {:id "j1"}})))
  (is (empty? (progress/needs-you {:died {:ago-s 4000 :cause "zombie"} :current {:id "j1"}})) "an old death is not news")
  (is (some #(str/includes? % "offline") (progress/needs-you {:offline {:reason "kicked"} :current {:id "j1"}}))))

(deftest needs-you-names-a-low-hp
  (is (some #(str/includes? % "hp 1.8") (progress/needs-you {:health 1.8 :current {:id "j1"}})))
  (is (empty? (progress/needs-you {:health 12 :current {:id "j1"}}))))

(deftest want-counts-include-worn-items
  (is (= [["iron_ingot" 3 24] ["iron_helmet" 1 1] ["iron_boots" 0 1]]
         (progress/want-counts inventory [["iron_ingot" 24] ["iron_helmet" 1] ["iron_boots" 1]]))))

(deftest the-line-is-one-short-line-with-goal-counts-health-and-needs
  (let [line (progress/progress-line {:status busy-status :goal "j14" :want-counts [["iron_ingot" 3 24] ["iron_helmet" 1 1]]})]
    (is (not (str/includes? line "\n")))
    (is (<= (count line) 300) line)
    (is (str/includes? line "hp 18 food 15"))
    (is (str/includes? line "goal j14"))
    (is (str/includes? line "running"))
    (is (str/includes? line "iron_ingot 3/24"))
    (is (str/includes? line "needs-you"))
    (is (str/includes? line "no-source"))))

(deftest a-goal-that-ended-shows-its-end
  (let [line (progress/progress-line {:status {:health 20 :food 20 :idle true} :goal "j14"
                                      :goal-job {:id "j14" :finished? true :status :completed}})]
    (is (str/includes? line "goal j14 completed") line)))

;; Wake verdicts: nil goes on waiting

(def night-idle {:wake :idle :jobs [{:job "j14" :name "obtain" :waiting {:reason :night-sheltered}}
                                    {:job "r3" :name "night" :holding {:reason :night}}]})

(deftest routine-wakes-go-on-waiting
  (doseq [wake [{:wake :timeout}
                {:wake :reset :reason :engine-restarted}
                {:wake :notice :event :hostile.fled :job "r2/flee"}
                {:wake :job-finished :job "j15" :result :completed}
                {:wake :job-finished :job "j15" :result :cancelled}
                night-idle
                {:wake :idle :jobs [{:job "j1" :name "noop" :idle-ok true}
                                    {:job "j8" :name "pen" :holding {:reason :searching}}
                                    {:job "j9" :name "pen"}]}
                {:wake :idle :jobs [{:job "j9" :name "noop" :idle-ok true}
                                    {:job "j14" :name "smelt" :waiting :cooking}]}
                {:wake :idle :jobs [{:job "j1" :name "noop" :idle-ok true}
                                    {:job "j30" :name "go-to" :waiting {:reason :exposed :lowered-by #{:light}}}]}]]
    (is (nil? (progress/wake-verdict wake "j14")) (pr-str wake))))

(deftest needs-you-wakes-end-the-wait
  (doseq [[wake kind] [[{:wake :died :cause "zombie"} :died]
                       [{:wake :attention :requests [{:id "r1" :reason :fruitless}]} :needs-you]
                       [{:wake :chat :from "Ann" :message "Probe stop"} :chat]
                       [{:wake :reconnect-failed :reason "kicked"} :needs-you]
                       [{:wake :job-finished :job "j14" :result :completed} :goal-ended]
                       [{:wake :job-finished :job "j14" :result :stopped :message "no iron"} :goal-ended]
                       [{:wake :job-finished :job "j15" :result :failed :message "boom"} :gave-up]
                       [{:wake :job-finished :job "j15" :result :stopped :message "unreachable"} :gave-up]
                       [{:wake :idle :jobs [{:job "j14" :name "obtain" :waiting {:reason :no-source}}]} :needs-you]
                       [{:wake :idle :jobs []} :needs-you]
                       [{:wake :idle :jobs [{:job "j8" :name "pen"} {:job "j9" :name "pen"}]} :needs-you]
                       [{:wake :idle :jobs [{:job "j8" :name "pen" :holding {:reason :searching}}
                                            {:job "j9" :name "pen" :waiting {:reason :no-source}}]} :needs-you]
                       [{:wake :idle :jobs [{:job "j1" :name "noop" :idle-ok true}]} :needs-you]
                       [{:wake :idle :jobs [{:job "j14" :name "obtain" :failed "boom"}]} :needs-you]]]
    (is (= kind (:wake (progress/wake-verdict wake "j14"))) (pr-str wake))))

;; The wait loop against a fake engine

(defn ev [source kind & [data message]]
  (cond-> {:source source :kind kind :data (or data {})} message (assoc :message message)))

(defn job-ev [id kind & [data]] (assoc (ev :job kind data) :context {:job-id id}))

(defn fixture [status]
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "progress-unit-"))
        world (atom {:events [] :status status :inventory {:ok true :inventory [] :equipment {}} :jobs {}})
        get! (fn [_socket endpoint _options]
               (let [{:keys [events]} @world
                     cursor {:stream-id "s" :seq (count events)}
                     value (cond
                             (= "/snapshot" endpoint) {:body "Probe" :generation-id "g" :outstanding {} :cursor cursor}
                             (str/starts-with? endpoint "/status") (:status @world)
                             (= "/triggers" endpoint) (:triggers @world {:items []})
                             (= "/inventory" endpoint) (:inventory @world)
                             (str/starts-with? endpoint "/job?id=") (get (:jobs @world) (subs endpoint 8) {:ok false :reason :job-not-found})
                             (str/starts-with? endpoint "/results") {:results {}}
                             :else (let [after (js/Number (.get (.-searchParams (js/URL. (str "http://x" endpoint))) "after"))]
                                     {:gap? false :stream-id "s" :latest-seq (count events) :cursor cursor
                                      :events (filterv #(> (:seq %) after) events)}))]
                 (js/Promise.resolve {:status (if (= :job-not-found (:reason value)) 404 200)
                                      :content-type "application/edn" :text (data/write-edn value)})))
        req (assoc (request "Probe" "--world" "w" "--state" dir "--goal" "j14" "--wait" "10s")
                   :slice-ms 50)]
    {:world world :get! get! :req req
     :push! (fn [& evs] (swap! world update :events
                               (fn [old] (into old (map-indexed (fn [i e] (assoc e :seq (+ 1 i (count old)) :generation-id "g")) evs)))))
     :cleanup #(.rmSync fs dir #js {:recursive true :force true})}))

(def working-status
  {:mode :scheduled :health 20 :food 20 :position {:x 1 :y 64 :z 1}
   :current {:id "j14" :name "obtain" :status :running}
   :jobs {:total 1 :items [{:id "j15" :name "mine" :status :queued}]}})

(defn with-fixture [status opts body]
  (async done
    (let [f (fixture status)
          f (update f :req merge opts)]
      (-> (js/Promise.resolve nil)
          (.then #(body f))
          (.catch (fn [error] (is false (str "rejected: " error))))
          (.then (fn [_] ((:cleanup f)) (done)))))))

(defn wait! [f] (progress/wait-progress! (:req f) (:get! f) nil))

(deftest routine-events-do-not-wake-the-wait
  (with-fixture working-status {:wait-ms 400 :stall-ms 3600000}
    (fn [f]
      (js/setTimeout #((:push! f)
                        (assoc (ev :reflex :fired {} "hostile near") :context {:reflex-id :hostile})
                        (job-ev "r7" :queued {:name "flee"})
                        (job-ev "r7" :completed)
                        (job-ev "j15" :queued {:name "mine"})
                        (job-ev "j15" :completed)
                        (assoc (job-ev "r7/flee" :hostile.fled {} "fled 20 blocks") :attention :notice)
                        (assoc (ev :body :idle {:jobs [{:job "r3" :name "night" :holding {:reason :night}}]}) :level :info))
                     60)
      (-> (wait! f)
          (.then (fn [result]
                   (is (= :timeout (:wake result)) (pr-str (dissoc result :view)))))))))

(deftest a-death-wakes-with-a-few-short-lines
  (with-fixture working-status {:wait-ms 2000}
    (fn [f]
      (js/setTimeout #((:push! f) (job-ev "r7" :completed) (ev :body :died {:cause "zombie" :pos {:x 1 :y 64 :z 1}})) 60)
      (-> (wait! f)
          (.then (fn [result]
                   (is (= :died (:wake result)))
                   (let [text (progress/wake-text result)
                         lines (str/split-lines text)]
                     (is (<= 2 (count lines) 5) text)
                     (is (str/starts-with? (first lines) "WAKE died"))
                     (is (str/includes? (second lines) "hp 20 food 20")))))))))

(deftest the-line-lists-the-muted-triggers-the-engine-reports
  (with-fixture working-status {:wait-ms 100}
    (fn [f]
      (swap! (:world f) assoc :triggers {:items [{:id :hostile :muted {:until 5}} {:id :eat}]})
      (-> (wait! f)
          (.then (fn [result] (is (str/includes? (:line result) "muted hostile") (:line result))))))))

(deftest a-goal-job-waiting-for-a-source-wakes-on-body-idle
  (with-fixture working-status {:wait-ms 2000}
    (fn [f]
      (js/setTimeout #((:push! f) (ev :body :idle {:jobs [{:job "j14" :name "obtain" :waiting {:reason :no-source}}]})) 60)
      (-> (wait! f)
          (.then (fn [result]
                   (is (= :needs-you (:wake result)))
                   (is (str/includes? (:text result) "no-source") (:text result))))))))

(deftest the-goal-ending-wakes-the-wait
  (with-fixture working-status {:wait-ms 2000}
    (fn [f]
      (js/setTimeout (fn []
                       (swap! (:world f) assoc-in [:jobs "j14"] {:ok true :id "j14" :finished? true :status :completed})
                       ((:push! f) (job-ev "j14" :completed)))
                     60)
      (-> (wait! f)
          (.then (fn [result] (is (= :goal-ended (:wake result)) (pr-str (dissoc result :view)))))))))

(deftest a-goal-already-ended-wakes-at-once
  (with-fixture (dissoc working-status :current) {:wait-ms 2000}
    (fn [f]
      (swap! (:world f) assoc-in [:jobs "j14"] {:ok true :id "j14" :finished? true :status :stopped})
      (-> (wait! f)
          (.then (fn [result] (is (= :goal-ended (:wake result)))))))))

(deftest no-progress-for-the-stall-time-wakes-the-wait
  (with-fixture working-status {:wait-ms 2000 :stall-ms 150}
    (fn [f]
      (-> (wait! f)
          (.then (fn [result] (is (= :stall (:wake result)) (pr-str (dissoc result :view)))))))))

(deftest progress-holds-off-the-stall
  (with-fixture working-status {:wait-ms 500 :stall-ms 250}
    (fn [f]
      (let [timer (js/setInterval #(swap! (:world f) update-in [:status :position :x] + 10) 40)]
        (-> (wait! f)
            (.then (fn [result] (is (= :timeout (:wake result)))))
            (.finally #(js/clearInterval timer)))))))

(deftest reflex-churn-is-not-progress
  (with-fixture (assoc working-status :current {:id "j90" :name "flee" :status :running :reflex true}
                       :jobs {:total 1 :items [{:id "j14" :name "obtain" :status :running}]})
    {:wait-ms 2000 :stall-ms 150}
    (fn [f]
      (let [n (atom 90)
            timer (js/setInterval
                   #(swap! (:world f) (fn [w] (-> w (assoc-in [:status :current :id] (str "j" (swap! n inc)))
                                                  (update-in [:status :position :x] + 10))))
                   40)]
        (-> (wait! f)
            (.then (fn [result] (is (= :stall (:wake result)) (pr-str (dissoc result :view)))))
            (.finally #(js/clearInterval timer)))))))

;; The goal and want are kept for later calls

(deftest goal-and-want-are-saved-and-reused
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "progress-plan-"))]
    (try
      (let [first-call (request "Probe" "--world" "w" "--state" dir "--goal" "j14" "--want" "iron_ingot:24")
            _ (progress/remember-plan! first-call)
            later (progress/remember-plan! (request "Probe" "--world" "w" "--state" dir))
            cleared (progress/remember-plan! (request "Probe" "--world" "w" "--state" dir "--goal" "none"))]
        (is (= "j14" (:goal later)))
        (is (= [["iron_ingot" 24]] (:want later)))
        (is (nil? (:goal cleared)))
        (is (= [["iron_ingot" 24]] (:want cleared)) "clearing the goal keeps the want list"))
      (finally (.rmSync fs dir #js {:recursive true :force true})))))

(deftest a-hit-wakes-progress-and-the-reply-says-so-first
  (is (= {:wake :needs-you :text "hit: health 14"} (progress/wake-verdict {:wake :danger :health 14} nil)))
  (is (false? (:danger (request "Probe" "--world" "w" "--wait" "1m" "--no-danger"))))
  (is (true? (:danger (request "Probe" "--world" "w" "--wait" "1m"))))
  (let [text (progress/wake-text {:wake :needs-you :text "hit: health 14" :vitals {:hurt "lost 6 health during the wait (20 -> 14)" :health 14 :food 18}})]
    (is (str/starts-with? text "HURT: lost 6 health"))
    (is (str/includes? text "NOW health 14 food 18"))))

(defn hurt-wake [danger & [{:keys [below health] :or {health 14}}]]
  (with-fixture working-status {:wait-ms 400 :stall-ms 3600000 :danger danger :danger-below (or below 21)}
    (fn [f]
      (js/setTimeout #((:push! f) (ev :body :hurt {:health health})) 60)
      (-> (wait! f)
          (.then (fn [result] (is (= (if (and danger (<= health (or below 21))) {:wake :needs-you :text (str "hit: health " health)} {:wake :timeout :text "no news"}) (select-keys result [:wake :text])) (pr-str (dissoc result :view)))))))))

(deftest a-hit-wakes-the-wait
  (hurt-wake true))

(deftest no-danger-lets-a-hit-pass
  (hurt-wake false))

(deftest a-small-hit-passes-under-the-health-threshold
  (hurt-wake true {:below 10 :health 17}))

(deftest a-hit-below-the-threshold-wakes
  (hurt-wake true {:below 10 :health 9}))

(deftest the-hit-threshold-defaults-to-ten-and-is-settable
  (is (= 10 (:danger-below (request "Probe" "--world" "w" "--wait" "1m"))))
  (is (= 15 (:danger-below (request "Probe" "--world" "w" "--wait" "1m" "--danger-below" "15"))))
  (is (string? (:error (request "Probe" "--world" "w" "--wait" "1m" "--danger-below" "40")))))
