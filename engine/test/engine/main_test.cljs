(ns engine.main-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.bodies :as bodies]
            [engine.main :as main]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(deftest parse-args-reads-flags
  (is (= {:agent "Claude" :world "claude" :scenario "s.edn" :fresh? true :state-dir nil :drive-idle-s 15 :events-max-bytes nil}
         (main/parse-args ["--agent" "Claude" "--world" "claude" "--scenario" "s.edn" "--fresh"])))
  (is (= {:agent nil :world nil :scenario nil :fresh? false :state-dir "/x" :drive-idle-s 15 :events-max-bytes nil}
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
    (is (= {:username "Bob" :host "h" :port 7 :world "w" :events-max-bytes nil
            :engine-dir (path/join dir "worlds" "w" "agents" "Bob" "engine")}
           (main/load-agent dir "w" "Bob")))
    (is (= :no-config (:error (main/load-agent dir "w" "Nobody"))))
    (is (= :no-config (:error (main/load-agent dir "other" "Bob"))))))

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
