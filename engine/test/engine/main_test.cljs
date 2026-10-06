(ns engine.main-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.bodies :as bodies]
            [engine.core :as core]
            [engine.events :as events]
            [engine.main :as main]
            [engine.registry :as registry]
            [engine.trigger-api :as api]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(deftest parse-args-reads-flags
  (is (= {:agent "Claude" :world "claude" :scenario "s.edn" :fresh? true :upgrade? false :state-dir nil :drive-idle-s 15 :events-max-bytes nil}
         (main/parse-args ["--agent" "Claude" "--world" "claude" "--scenario" "s.edn" "--fresh"])))
  (is (= {:agent nil :world nil :scenario nil :fresh? false :upgrade? false :state-dir "/x" :drive-idle-s 15 :events-max-bytes nil}
         (main/parse-args ["--state-dir" "/x"])))
  (is (= 5 (:drive-idle-s (main/parse-args ["--drive-idle-s" "5"]))))
  (is (= "4096" (:events-max-bytes (main/parse-args ["--events-max-bytes" "4096"]))))
  (is (= "" (:events-max-bytes (main/parse-args ["--events-max-bytes"]))))
  (is (= 67108864 (main/event-cap nil nil)))
  (is (= 4096 (main/event-cap "4096" nil)))
  (is (nil? (main/event-cap "1e6" nil)))
  (is (nil? (main/event-cap "1023" nil)))
  (is (nil? (main/event-cap nil 1.5))))

(declare agent-state-dir)

(deftest load-agent-reads-config-and-world
  (let [dir (agent-state-dir)]
    (is (= {:username "Bob" :host "h" :port 7 :world "w" :events-max-bytes nil :view-distance nil
            :engine-dir (path/join dir "worlds" "w" "agents" "Bob" "engine")}
           (main/load-agent dir "w" "Bob")))
    (is (= :no-config (:error (main/load-agent dir "w" "Nobody"))))
    (is (= :no-config (:error (main/load-agent dir "other" "Bob"))))))

(deftest view-distance-comes-from-the-config
  (let [dir (agent-state-dir)
        cfg-file (path/join (bodies/body-dir dir "w" "Bob") "config.json")]
    (fs/writeFileSync cfg-file "{\"username\":\"Bob\",\"viewDistance\":4}")
    (is (= 4 (:view-distance (main/load-agent dir "w" "Bob"))))
    (is (= 4 (.-viewDistance (main/connect-options (main/load-agent dir "w" "Bob")))))
    (is (not (contains? (js->clj (main/connect-options {:host "h" :port 7 :username "Bob"})) "viewDistance")))))

(deftest the-world-comes-from-the-flag-not-the-config
  (let [dir (agent-state-dir)]
    (fs/writeFileSync (path/join (bodies/body-dir dir "w" "Bob") "config.json") "{\"username\":\"Bob\",\"world\":\"elsewhere\"}")
    (is (= "w" (:world (main/load-agent dir "w" "Bob"))))))

(defn agent-state-dir []
  (let [dir (tu/tmp-dir)
        body (bodies/body-dir dir "w" "Bob")]
    (fs/mkdirSync body #js {:recursive true})
    (fs/writeFileSync (path/join body "config.json") "{\"username\":\"Bob\"}")
    (fs/writeFileSync (path/join dir "worlds" "w" "world.json") "{\"host\":\"h\",\"port\":7}")
    dir))

(deftest run-refuses-without-an-agent-or-primitives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (re-find #"usage" (:error (await (main/run {})))))
        (is (re-find #"missing --world <world>" (:error (await (main/run {:agent "Bob" :state-dir (agent-state-dir)})))))
        (is (re-find #"no config" (:error (await (main/run {:agent "Nobody" :world "w" :state-dir (tu/tmp-dir)})))))
        (is (re-find #"js/primitives.mjs.*refusing"
                     (:error (await (main/run {:agent "Bob" :world "w" :state-dir (agent-state-dir)
                                               :engine-root (tu/tmp-dir)})))))))))

(deftest shutdown-exits-after-stop-finishes-or-the-time-limit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [log (atom [])
              slow-stop (fn [] (js/Promise. (fn [resolve _] (js/setTimeout (fn [] (swap! log conj :stopped) (resolve)) 40))))
              hung-stop (fn [] (js/Promise. (fn [_ _])))
              failing-stop (fn [] (js/Promise.reject (js/Error. "boom")))]
          (await ((main/shutdown-handler slow-stop #(swap! log conj :exit) 1000)))
          (await ((main/shutdown-handler hung-stop #(swap! log conj :exit-hung) 60)))
          (await ((main/shutdown-handler failing-stop #(swap! log conj :exit-failed) 1000)))
          (is (= [:stopped :exit :exit-hung :exit-failed] @log)))))))

(deftest a-second-signal-does-not-stop-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [log (atom [])
              stop (fn [] (js/Promise. (fn [resolve _] (js/setTimeout (fn [] (swap! log conj :stopped) (resolve)) 40))))
              handler (main/shutdown-handler stop #(swap! log conj :exit) 1000)]
          (handler)
          (await (handler))
          (await (js/Promise. (fn [resolve] (js/setTimeout resolve 80))))
          (is (= [:stopped :exit] @log) "one stop and one exit, however many signals"))))))

(defn scenario-file [text]
  (let [f (path/join (tu/tmp-dir) "s.edn")]
    (fs/writeFileSync f text)
    f))

(def stale-scenario "{:register [{:trigger :hungry} {:trigger :night-unsafe}]}")

(defn save-engine! [dir]
  (let [engine (path/join (bodies/body-dir dir "w" "Bob") "engine")]
    (fs/mkdirSync engine #js {:recursive true})
    (fs/writeFileSync (path/join engine "engine.edn") "{}")))

(deftest a-first-start-exits-on-an-unknown-trigger
  (let [dir (agent-state-dir)
        r (main/preflight {:agent "Bob" :world "w" :state-dir dir :scenario (scenario-file stale-scenario)})]
    (is (re-find #"unknown trigger :night-unsafe" (:error r)))))

(deftest a-restart-leaves-an-unknown-trigger-out-and-reports-it
  (let [dir (agent-state-dir)
        _ (save-engine! dir)
        r (main/preflight {:agent "Bob" :world "w" :state-dir dir :scenario (scenario-file stale-scenario)})]
    (is (nil? (:error r)))
    (is (= [:hungry] (mapv :trigger (:register (:plan r)))))
    (is (= [:night-unsafe] (mapv :id (:stale r))))))

(deftest a-restart-still-exits-on-other-scenario-problems
  (let [dir (agent-state-dir)
        _ (save-engine! dir)
        r (main/preflight {:agent "Bob" :world "w" :state-dir dir :scenario (scenario-file "{:register [{:trigger :hungry :cooldown-s -1}]}")})]
    (is (re-find #"scenario problems.*; fix or remove them in .*s\.edn" (:error r)))))

(deftest fresh-treats-a-restart-as-a-first-start
  (let [dir (agent-state-dir)
        _ (save-engine! dir)
        r (main/preflight {:agent "Bob" :world "w" :state-dir dir :fresh? true :scenario (scenario-file stale-scenario)})]
    (is (re-find #"unknown trigger" (:error r)))))

(deftest parse-args-reads-upgrade
  (is (true? (:upgrade? (main/parse-args ["--upgrade"])))))

;; ---------------------------------------------------------------- boot-scenario! (what start does with the scenario)

(defn body-engine
  "A body engine on dir (a saved one when dir is reused), with the events it emitted."
  ([] (body-engine (tu/tmp-dir)))
  ([dir]
   (let [[seen sink] (tu/legacy-capture-sink)
         eng (core/create {:primitives (tu/fake {}) :jobs registry/jobs :triggers (main/body-triggers) :dir dir
                           :events (events/make {:body "Fake" :sinks [sink]})})]
     {:eng eng :dir dir :seen seen})))

(def two '{:register [{:trigger :hungry} {:trigger :burning}]})
(def one '{:register [{:trigger :hungry}]})
(defn ids [eng] (mapv :id (:register (core/state eng))))
(defn attention-reasons [eng] (set (map :reason (vals (:attention (core/state eng))))))

(deftest a-first-start-loads-the-scenario-and-offers-nothing
  (let [{:keys [eng]} (body-engine)]
    (main/boot-scenario! eng {:plan two :stale [{:id :gone :message "m"}] :restoring? false :upgrade? true})
    (is (= [:hungry :burning] (ids eng)))
    (is (empty? (:attention (core/state eng))))))

(deftest a-restore-with-a-stale-entry-warns-and-asks-and-does-not-exit
  (let [{:keys [eng seen]} (body-engine)]
    (main/boot-scenario! eng {:plan one :stale [{:id :gone :message "unknown trigger"}] :restoring? true :upgrade? false})
    (is (re-find #"scenario names :gone, which is not a known trigger \(renamed or misspelt\); skipped" (str (some #(when (= :dropped (:kind %)) (:text %)) @seen))))
    (is (contains? (attention-reasons eng) :reflex-dropped))))

(deftest only-a-restore-resumes-the-scenario
  (let [{:keys [eng dir]} (body-engine)]
    (main/boot-scenario! eng {:plan one :restoring? false :upgrade? false})
    (core/shutdown! eng)
    (let [{again :eng} (body-engine dir)]
      (main/boot-scenario! again {:plan two :restoring? true :upgrade? false})
      (is (= [:hungry] (ids again)) "the saved register is kept")
      (is (= #{:new-default-triggers} (attention-reasons again))))
    (let [{fresh :eng} (body-engine)]
      (main/boot-scenario! fresh {:plan two :restoring? false :upgrade? false})
      (is (empty? (:attention (core/state fresh))) "a first start offers nothing"))))

(deftest the-upgrade-flag-reaches-the-resume
  (let [{:keys [eng dir]} (body-engine)]
    (main/boot-scenario! eng {:plan one :restoring? false :upgrade? false})
    (core/shutdown! eng)
    (let [{again :eng} (body-engine dir)]
      (main/boot-scenario! again {:plan two :restoring? true :upgrade? true})
      (is (= [:hungry :burning] (ids again)))
      (is (empty? (:attention (core/state again)))))))

(deftest a-restart-without-a-scenario-closes-an-old-offer
  (let [{:keys [eng dir]} (body-engine)]
    (main/boot-scenario! eng {:plan one :restoring? false :upgrade? false})
    (core/shutdown! eng)
    (let [{second-run :eng} (body-engine dir)]
      (main/boot-scenario! second-run {:plan two :restoring? true :upgrade? false})
      (is (some? (:new-defaults (core/state second-run))))
      (core/shutdown! second-run)
      (let [{third :eng} (body-engine dir)]
        (main/boot-scenario! third {:plan nil :stale nil :restoring? true :upgrade? false})
        (is (nil? (:new-defaults (core/state third))))
        (is (nil? (:scenario-order (core/state third))))
        (is (empty? (:attention (core/state third))))
        (is (= {:ok true :op :upgrade :added [] :offered []} (api/upgrade! third)) "nothing stale is added")
        (is (= [:hungry] (ids third)))))))
