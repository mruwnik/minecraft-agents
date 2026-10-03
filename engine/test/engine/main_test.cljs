(ns engine.main-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.main :as main]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(deftest parse-args-reads-flags
  (is (= {:agent "Claude" :scenario "s.edn" :fresh? true :state-dir nil}
         (main/parse-args ["--agent" "Claude" "--scenario" "s.edn" "--fresh"])))
  (is (= {:agent nil :scenario nil :fresh? false :state-dir "/x"}
         (main/parse-args ["--state-dir" "/x"]))))

(declare agent-state-dir)

(deftest load-agent-reads-config-and-world
  (let [dir (agent-state-dir)]
    (is (= {:username "Bob" :host "h" :port 7 :engine-dir (path/join dir "agents" "Bob" "engine")}
           (main/load-agent dir "Bob")))
    (is (= :no-config (:error (main/load-agent dir "Nobody"))))))

(defn agent-state-dir []
  (let [dir (tu/tmp-dir)]
    (fs/mkdirSync (path/join dir "agents" "Bob") #js {:recursive true})
    (fs/mkdirSync (path/join dir "worlds" "w") #js {:recursive true})
    (fs/writeFileSync (path/join dir "agents" "Bob" "config.json") "{\"username\":\"Bob\",\"world\":\"w\"}")
    (fs/writeFileSync (path/join dir "worlds" "w" "world.json") "{\"host\":\"h\",\"port\":7}")
    dir))

(deftest run-refuses-without-an-agent-or-primitives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (re-find #"usage" (:error (await (main/run {})))))
        (is (re-find #"no config" (:error (await (main/run {:agent "Nobody" :state-dir (tu/tmp-dir)})))))
        (is (re-find #"js/primitives.mjs.*refusing"
                     (:error (await (main/run {:agent "Bob" :state-dir (agent-state-dir)
                                               :engine-root (tu/tmp-dir)})))))))))
