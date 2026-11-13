(ns engine.main-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.bodies :as bodies]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake.raw-world :as fake-raw]
            [engine.hooks :as hooks]
            [engine.main :as main]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.trigger-api :as api]
            [engine.test-util :as tu]
            [jobs.lib.look :as look]
            ["fs" :as fs]
            ["path" :as path]))

(deftest parse-args-reads-flags
  (is (= {:agent "Claude" :world "test" :scenario "s.edn" :fresh? true :upgrade? false :state-dir nil}
         (main/parse-args ["--agent" "Claude" "--world" "test" "--scenario" "s.edn" "--fresh"])))
  (is (= {:agent nil :world nil :scenario nil :fresh? false :upgrade? false :state-dir "/x"}
         (main/parse-args ["--state-dir" "/x"]))))

(declare agent-state-dir)

(deftest load-agent-reads-config-and-world
  (let [dir (agent-state-dir)]
    (is (= {:username "Bob" :host "h" :port 7 :world "w" :view-distance nil
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

(deftest tick-rate-physics-follows-the-setting-on-by-default
  (is (true? (.-followTickRate (main/connect-options {:host "h" :port 7 :username "Bob"}))))
  (with-redefs [settings/state (atom {:values {:engine.game/follow-tick-rate false}})]
    (is (false? (.-followTickRate (main/connect-options {:host "h" :port 7 :username "Bob"}))))))

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

(deftest a-stop-waits-for-a-running-danger-reflex-and-not-for-a-comfort-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (atom [])
              fake (fn [reflex level]
                     {:running (atom {:reflex reflex})
                      :state (atom {:register [{:id reflex :trigger reflex}]})
                      :triggers {reflex {:level level}}
                      :events (events/make {:body "Fake" :sinks [#(swap! raw conj %)] :now #(js/Date.now)})})
              danger (fake :suffocating :danger)
              comfort (fake :hungry :comfort)
              ended (atom false)]
          (js/setTimeout #(do (reset! ended true) (reset! (:running danger) nil)) 300)
          (await (main/wait-out-danger! comfort))
          (is (false? @ended) "a comfort reflex is not waited for")
          (await (main/wait-out-danger! danger))
          (is (true? @ended) "the stop waited until the danger reflex ended")
          (is (= [:stop-waiting] (mapv :kind @raw))))))))

(deftest a-warm-hook-that-throws-or-rejects-emits-warm-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (atom [])
              eng {:events (events/make {:body "Fake" :sinks [#(swap! raw conj %)] :now #(js/Date.now)})}]
          (main/run-warm-hook! eng #(throw (js/Error. "sync")))
          (main/run-warm-hook! eng #(js/Promise.reject (js/Error. "async")))
          (await (js/Promise. #(js/setTimeout % 20)))
          (is (= [:warm-failed :warm-failed] (mapv :kind @raw)))
          (is (= ["warming the tables failed: sync" "warming the tables failed: async"] (mapv :message @raw))))))))

(deftest a-way-seen-in-a-mining-look-is-read-in-the-same-pass
  ;; the body at [0 64 0] faces south (+z) at a stone wall from z 5: the way from [0 64 4] south is rock
  (let [raw-events (atom [])
        p (tu/blind (tu/fake {:blocks (tu/box -8 58 5 8 72 8 "stone")}))
        eng-ref (atom nil)
        per (perception/create (fake-raw/create p) {:on-pass (main/pass-hook eng-ref (:perception/pass hooks/all) 60000)})
        store (atom {:data {} :now (constantly 0) :world-time (constantly 0)})
        eng {:store store :primitives (perception/wrap p per)
             :events (events/make {:body "Fake" :sinks [#(swap! raw-events conj %)] :now #(js/Date.now)})}
        _ (mem/write! store :way-pending {:way [0 64 4 2]} {:cap 64 :ttl :forever})
        _ (reset! eng-ref eng)
        _ (look/see! (core/hook-ctx eng))]
    (is (= [{:way [0 64 4 2] :on false}] (mapv #(select-keys (:data %) [:way :on]) (filter #(= :way-on.seen (:kind %)) @raw-events))))
    (is (= [] (mem/entries (mem/view store) :way-pending)) "read once, then forgotten")))

(deftest a-pass-hook-that-throws-emits-pass-hook-failed-at-most-once-per-every-ms
  (let [raw (atom [])
        eng {:events (events/make {:body "Fake" :sinks [#(swap! raw conj %)] :now #(js/Date.now)})}
        run (main/pass-hook (atom eng) (fn [_] (throw (js/Error. "boom"))) 60000)]
    (run)
    (run)
    (is (= [:pass-hook-failed] (mapv :kind @raw)) "the second throw within every-ms is not emitted again, and none is thrown")
    (is (= ["the perception pass hook failed: boom"] (mapv :message @raw)))))

(deftest a-pass-hook-whose-promise-rejects-emits-pass-hook-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (atom [])
              eng {:events (events/make {:body "Fake" :sinks [#(swap! raw conj %)] :now #(js/Date.now)})}
              run (main/pass-hook (atom eng) (fn [_] (js/Promise.reject (js/Error. "late"))) 60000)]
          (run)
          (await (js/Promise. (fn [resolve _] (js/setTimeout resolve 0))))
          (is (= ["the perception pass hook failed: late"] (mapv :message @raw))))))))

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

(deftest fresh-wipes-body-memory-and-keeps-event-logs
  (let [dir (tu/tmp-dir)
        names ["engine.edn" "results.edn" "memory.edn" "seen.bin" "events.edn"]]
    (doseq [n names] (fs/writeFileSync (path/join dir n) "x"))
    (main/wipe-body-state! dir)
    (is (= ["events.edn"] (filter #(fs/existsSync (path/join dir %)) names)))))
