(ns engine.migrate-cli-test
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [engine.memory :as mem]
            [engine.migrate-cli :as cli]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["net" :as net]
            ["path" :as path]))

(deftest parse-args-reads-flags-and-names
  (is (= {:dry-run true :state-dir "/s" :names ["A" "B"]} (cli/parse-args ["--dry-run" "--state-dir" "/s" "A" "B"])))
  (is (= {:dry-run false :state-dir nil :names []} (cli/parse-args [])))
  (is (some? (:error (cli/parse-args ["--bogus"]))))
  (is (some? (:error (cli/parse-args ["--state-dir"])))))

(deftest summary-line-names-the-plan
  (let [line (cli/summary-line
              {:name "Ann" :beds 1 :chests 1
               :beds-dropped [{:x 1 :y 2 :z 3}] :chests-dropped []
               :memory {:entries {:bed [{:data {:pos {:x 9 :y 8 :z 7}}}]}}
               :pose {:pos {:x 1.5 :y 2 :z 3}}
               :skipped [{:file "events.jsonl" :error "bad"}]})]
    (doseq [s ["Ann" "engine/memory.edn" "view/pose.json" "bed 9 8 7" "dropped 1" "pose 1.5 2 3" "events.jsonl"]]
      (is (str/includes? line s) s))))

(deftest summary-line-with-nothing
  (let [line (cli/summary-line {:name "Bo" :memory {:entries {} :policies {}} :beds 0 :chests 0 :beds-dropped [] :chests-dropped [] :skipped []})]
    (is (str/includes? line "no pose"))
    (is (str/includes? line "writes engine/memory.edn"))
    (is (not (str/includes? line "pose.json")))))

;; ------------------------------------------------------------------ write mode, over temp roots

(defn write-file! [file text]
  (fs/mkdirSync (path/dirname file) #js {:recursive true})
  (fs/writeFileSync file text))

(defn make-body!
  "state/agents/<name> with a config (port), an event with a position and, when bed is given, the world's places."
  [root name port]
  (write-file! (path/join root "agents" name "config.json")
               (js/JSON.stringify (clj->js {:username name :apiPort port :world "w"})))
  (write-file! (path/join root "agents" name "events.jsonl")
               (str (js/JSON.stringify (clj->js {:seq 1 :t "2026-09-27T17:00:00.000Z" :type "x" :pos {:x 1 :y 2 :z 3}})) "\n")))

(defn make-root!
  "A temp state dir with bodies Ann and Bob (nothing listens on port 1) and a places.json with Ann's bed."
  []
  (let [root (tu/tmp-dir)]
    (make-body! root "Ann" 1)
    (make-body! root "Bob" 1)
    (write-file! (path/join root "worlds" "w" "places.json")
                 (js/JSON.stringify (clj->js [{:name "b" :kind "bed" :x 1 :y 64 :z 2 :by "Ann" :note ""}])))
    root))

(defn tree
  "{relative-path mtimeMs} of every file under root."
  [root]
  (into {} (for [f (fs/readdirSync root #js {:recursive true})
                 :let [full (path/join root f)]
                 :when (.isFile (fs/statSync full))]
             [f (.-mtimeMs (fs/statSync full))])))

(defn written [before after] (sort (keys (apply dissoc after (keys before)))))

(defn run-all [root names] (cli/migrate-all root names false))

(deftest write-mode-writes-memory-and-pose-and-nothing-else
  (async done
    (let [root (make-root!) before (tree root)]
      (tu/run-async done
        (fn [] (.then (run-all root ["Ann"])
                      (fn [_]
                        (is (= ["agents/Ann/engine/memory.edn" "agents/Ann/view/pose.json"] (written before (tree root))))
                        (is (= before (select-keys (tree root) (keys before))) "nothing existing changed")
                        (is (= {:x 1 :y 64 :z 2}
                               (mem/place (mem/view (mem/open (path/join root "agents" "Ann" "engine") {:now js/Date.now})) :bed)))
                        (is (= {:v 1 :world "w" :status "offline" :pos {:x 1 :y 2 :z 3}}
                               (dissoc (tu/read-json (path/join root "agents" "Ann" "view" "pose.json")) :t)))))))))) 

(deftest a-folder-with-engine-is-left-untouched
  (async done
    (let [root (make-root!)]
      (write-file! (path/join root "agents" "Ann" "engine" "engine.edn") "{}")
      (let [before (tree root)]
        (tu/run-async done
          (fn [] (.then (run-all root ["Ann"])
                        (fn [lines]
                          (is (= before (tree root)))
                          (is (str/includes? (str/join "\n" lines) "left alone"))))))))))

(deftest an-existing-pose-is-kept-and-reported-the-memory-is-still-written
  (async done
    (let [root (make-root!)
          pose-file (path/join root "agents" "Ann" "view" "pose.json")]
      (write-file! pose-file "{\"mine\":true}")
      (tu/run-async done
        (fn [] (.then (run-all root ["Ann"])
                      (fn [lines]
                        (is (= "{\"mine\":true}" (fs/readFileSync pose-file "utf8")))
                        (is (fs/existsSync (path/join root "agents" "Ann" "engine" "memory.edn")))
                        (is (str/includes? (str/join "\n" lines) "kept existing")))))))))

(defn refuses-and-others-convert
  "Run Ann (made connected by setup!) and Bob; Ann writes nothing and says why, Bob converts."
  [done root why cleanup!]
  (tu/run-async done
    (fn [] (.then (run-all root ["Ann" "Bob"])
                  (fn [lines]
                    (cleanup!)
                    (let [text (str/join "\n" lines)]
                      (is (str/includes? text (str "Ann: refused, " why)))
                      (is (not (fs/existsSync (path/join root "agents" "Ann" "engine" "memory.edn"))))
                      (is (not (fs/existsSync (path/join root "agents" "Ann" "view"))))
                      (is (fs/existsSync (path/join root "agents" "Bob" "engine" "memory.edn")))
                      (is (fs/existsSync (path/join root "agents" "Bob" "view" "pose.json")))))))))

(deftest a-control-socket-refuses
  (async done
    (let [root (make-root!)]
      (write-file! (path/join root "agents" "Ann" "engine" "control.sock") "")
      (refuses-and-others-convert done root "engine/control.sock exists" identity))))

(deftest a-live-body-pid-refuses
  (async done
    (let [root (make-root!)]
      (write-file! (path/join root "agents" "Ann" "body.pid") (str js/process.pid))
      (refuses-and-others-convert done root "body.pid names a live process" identity))))

(deftest a-listener-on-the-api-port-refuses
  (async done
    (let [root (make-root!)
          server (net/createServer (fn [sock] (.destroy sock)))]
      (.listen server 0 "127.0.0.1"
               (fn []
                 (let [port (.-port (.address server))]
                   (make-body! root "Ann" port)
                   (refuses-and-others-convert done root (str "something listens on 127.0.0.1:" port)
                                               #(.close server))))))))

(deftest a-second-run-changes-nothing
  (async done
    (let [root (make-root!)]
      (tu/run-async done
        (fn [] (.then (run-all root ["Ann" "Bob"])
                      (fn [_]
                        (let [after-first (tree root)]
                          (.then (run-all root ["Ann" "Bob"])
                                 (fn [_] (is (= after-first (tree root)))))))))))))
