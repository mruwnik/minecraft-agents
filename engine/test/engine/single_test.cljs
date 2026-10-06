(ns engine.single-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.bodies :as bodies]
            [engine.main :as main]
            [engine.single :as single]
            [engine.test-util :as tu]
            [clojure.string :as str]
            ["child_process" :as cp]
            ["fs" :as fs]
            ["net" :as net]
            ["path" :as path]))

(def who {:pid 4242 :world "w" :body "Bob"})

(defn body-state-dir
  "A state dir holding the folder of body Bob in world w, with saved files."
  []
  (let [dir (tu/tmp-dir)
        body (bodies/body-dir dir "w" "Bob")
        eng (path/join body "engine")]
    (fs/mkdirSync eng #js {:recursive true})
    (fs/writeFileSync (path/join body "config.json") "{\"username\":\"Bob\"}")
    (fs/writeFileSync (path/join dir "worlds" "w" "world.json") "{\"host\":\"h\",\"port\":7}")
    (fs/writeFileSync (path/join eng "engine.edn") "{:saved 1}")
    (fs/writeFileSync (path/join eng "memory.edn") "{:memory 1}")
    (fs/writeFileSync (path/join eng "events.jsonl") "{\"seq\":1}\n")
    dir))

(defn files-of
  "{relative file: [mtime-ms content]} of everything under dir."
  [dir]
  (into {}
        (for [f (js->clj (fs/readdirSync dir #js {:recursive true}))
              :let [full (path/join dir f)]
              :when (.isFile (fs/statSync full))]
          [f [(.-mtimeMs (fs/statSync full)) (fs/readFileSync full "utf8")]])))

(defn stale-socket!
  "Leave a real stale unix socket file at sock: a process binds it and is killed with SIGKILL."
  [sock]
  (cp/spawnSync "node" #js ["-e" (str "require('net').createServer().listen(" (pr-str sock)
                                      ", () => process.kill(process.pid, 'SIGKILL'))")])
  (is (fs/existsSync sock)))

(deftest the-first-claim-holds-and-the-second-sees-the-pid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")
              first-claim (await (single/claim! sock who))
              second-claim (await (single/claim! sock {:pid 1 :world "w" :body "Bob"}))]
          (is (fn? (:held first-claim)))
          (is (= {:running who} second-claim))
          (await ((:held first-claim)))
          (is (not (fs/existsSync sock))))))))

(deftest a-released-claim-can-be-taken-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")]
          (await ((:held (await (single/claim! sock who)))))
          (is (fn? (:held (await (single/claim! sock who))))))))))

(deftest a-stale-socket-from-a-crash-is-taken-over
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")]
          (stale-socket! sock)
          (let [c (await (single/claim! sock who))]
            (is (fn? (:held c)))
            (is (= {:running who} (await (single/claim! sock who))))
            (await ((:held c)))))))))

(deftest a-plain-file-in-the-socket-place-is-stale
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")]
          (fs/writeFileSync sock "")
          (is (fn? (:held (await (single/claim! sock who))))))))))

(deftest a-listener-that-does-not-answer-still-counts-as-running
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")
              server ((.-createServer (js/require "net")) (fn [_]))]
          (await (js/Promise. (fn [res] (.listen server sock res))))
          (is (= {:running {}} (await (single/claim! sock who))))
          (.close server))))))

(deftest a-second-start-is-refused-and_touches-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [fresh? [false true]]
          (let [dir (body-state-dir)
                eng (path/join (bodies/body-dir dir "w" "Bob") "engine")
                holder (await (single/claim! (single/socket-path eng) who))
                before (files-of (bodies/body-dir dir "w" "Bob"))
                result (await (main/run {:agent "Bob" :world "w" :state-dir dir :fresh? fresh?}))]
            (is (= 3 (:exit-code result)) (str "fresh? " fresh?))
            (is (re-find #"body Bob in world w is already running \(pid 4242" (:error result)))
            (is (not (:stop result)))
            (is (= before (files-of (bodies/body-dir dir "w" "Bob"))) (str "fresh? " fresh?))
            (await ((:held holder)))))))))

(deftest two-starts-over-one-stale-socket-never-both-hold-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")]
          (stale-socket! sock)
          (let [results (await (js/Promise.all #js [(single/claim! sock who) (single/claim! sock who)]))
                held (filterv :held results)]
            (is (= 1 (count held)) "exactly one start wins; the other sees it running")
            (doseq [c held] (await ((:held c))))))))))

(deftest three-waiters-in-one-process-enter-one-at-a-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")
              inside (atom 0)
              max-inside (atom 0)
              entered (atom 0)
              enter (fn ^:async enter []
                      (swap! inside inc)
                      (swap! max-inside max @inside)
                      (swap! entered inc)
                      (await (js/Promise. (fn [r] (js/setTimeout r 30))))
                      (swap! inside dec))]
          (await (js/Promise.all (clj->js (repeatedly 3 #(single/with-replace-lock sock enter)))))
          (is (= 3 @entered))
          (is (= 1 @max-inside)))))))

(deftest every-connection-gets-an-error-listener-so-an-early-reset-cannot-crash-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")
              server (await (single/listen! sock who))
              listeners (js/Promise. (fn [resolve]
                                       (.on server "connection" (fn [conn] (resolve (.listenerCount conn "error"))))))
              client (.createConnection net #js {:path sock})]
          (.on client "error" (fn []))
          (is (pos? (await listeners)))
          (.destroy client)
          (await (js/Promise. (fn [resolve] (.close server resolve)))))))))

;; Cross-process lock tests: each child re-runs this test bundle with --test=engine.single-test/lock-child and its
;; part in SINGLE_LOCK_CHILD. Inside the lock a holder keeps a marker directory; finding it made already means two
;; processes were inside at once.

(def child-env "SINGLE_LOCK_CHILD")

(defn sleep [ms] (js/Promise. (fn [r] (js/setTimeout r ms))))

(defn ^:async inside-alone?
  "Hold the lock's inside for ms behind marker; false when another holder's marker was already there."
  [marker ms]
  (if (try (fs/mkdirSync marker) true (catch :default e (if (= "EEXIST" (.-code e)) false (throw e))))
    (do (await (sleep ms)) (fs/rmdirSync marker) true)
    false))

(defn stdin-closed [] (js/Promise. (fn [r] (.on js/process.stdin "end" r) (.resume js/process.stdin))))

(defn ^:async run-child
  "One child's part: :loop takes the lock iterations times (dying inside on the crash-at'th), :hold keeps it until
  stdin closes, :die dies inside it."
  [{:keys [mode sock marker iterations crash-at hold-ms]}]
  (case mode
    "loop" (do (loop [i 0]
                 (when (< i iterations)
                   (await (single/with-replace-lock
                            sock (fn ^:async inside []
                                   (when-not (await (inside-alone? marker hold-ms)) (println "LOCK:overlap"))
                                   (when (= i crash-at) (.kill js/process js/process.pid "SIGKILL")))))
                   (recur (inc i))))
               (println "LOCK:done"))
    "hold" (await (single/with-replace-lock
                    sock (fn ^:async inside []
                           (fs/mkdirSync marker)
                           (println "LOCK:in")
                           (await (stdin-closed))
                           (fs/rmdirSync marker))))
    "die" (await (single/with-replace-lock
                   sock (fn [] (println "LOCK:in") (.kill js/process js/process.pid "SIGKILL"))))))

(deftest lock-child
  ;; Not a test: the body of the child processes below; does nothing without SINGLE_LOCK_CHILD.
  (when-let [cfg (some-> (aget js/process.env child-env) js/JSON.parse (js->clj :keywordize-keys true))]
    (async done (tu/run-async done (fn ^:async t [] (await (run-child cfg)))))))

(defn child-command [] [(.-execPath js/process) (aget js/process.argv 1) "--test=engine.single-test/lock-child"])

(defn start-child
  "Start a child with cfg. {:proc :out (atom of stdout) :exit (promise of [code signal])}. With zombie? the child runs
  under a shell that execs a long sleep, which never reaps it: once dead it stays a zombie until that sleep dies."
  ([cfg] (start-child cfg false))
  ([cfg zombie?]
   (let [env (js/Object.assign #js {} js/process.env (js-obj child-env (js/JSON.stringify (clj->js cfg))))
         [node & args] (child-command)
         proc (if zombie?
                (cp/spawn "sh" (clj->js (into ["-c" "\"$0\" \"$@\" & exec sleep 600" node] args)) #js {:env env})
                (cp/spawn node (clj->js args) #js {:env env}))
         out (atom "")]
     (.on (.-stdout proc) "data" (fn [d] (swap! out str d)))
     {:proc proc :out out
      :exit (js/Promise. (fn [r] (.on proc "exit" (fn [code signal] (r [code signal])))))})))

(defn ^:async seen
  "Wait until child printed line (or exited)."
  [{:keys [out exit]} line]
  (let [exited (atom false)]
    (.then exit #(reset! exited true))
    (loop []
      (when-not (or (str/includes? @out line) @exited)
        (await (sleep 10))
        (recur)))
    (str/includes? @out line)))

(deftest three-processes-dying-inside-the-lock-never-share-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              sock (path/join dir "body.sock")
              marker (path/join (tu/tmp-dir) "inside")
              iterations 15]
          (doseq [round (range 4)]
            (let [kids (mapv #(start-child {:mode "loop" :sock sock :marker marker :iterations iterations
                                            :hold-ms 2 :crash-at (if (zero? %) (rand-int iterations) -1)})
                             (range 3))
                  exits (await (js/Promise.all (clj->js (map :exit kids))))]
              (is (= [[nil "SIGKILL"] [0 nil] [0 nil]] (js->clj exits)) (str "round " round))
              (doseq [k kids] (is (not (str/includes? @(:out k) "LOCK:overlap")) (str "round " round)))
              (doseq [k (rest kids)] (is (str/includes? @(:out k) "LOCK:done")))))
          (await (single/with-replace-lock sock (fn [] nil)))
          (is (= [] (js->clj (fs/readdirSync dir))) "nothing is left beside the socket"))))))

(deftest a-holder-that-died-but-is-not-yet-reaped-frees-the-lock
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")
              zombie (start-child {:mode "die" :sock sock} true)]
          (is (await (seen zombie "LOCK:in")))
          (let [attempt (.then (single/with-replace-lock sock (fn [] nil)) (fn [_] :entered))
                result (await (js/Promise.race #js [attempt (.then (sleep 20000) (fn [_] :still-waiting))]))]
            (is (= :entered result) "the zombie's parent is still alive, so the zombie still exists")
            (.kill (:proc zombie) "SIGKILL")
            (await attempt)))))))

(deftest a-live-holder-in-another-process-keeps-the-lock-however-long-it-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sock (path/join (tu/tmp-dir) "body.sock")
              marker (path/join (tu/tmp-dir) "inside")
              holder (start-child {:mode "hold" :sock sock :marker marker})]
          (is (await (seen holder "LOCK:in")))
          (let [shared (atom nil)
                attempt (single/with-replace-lock sock (fn [] (reset! shared (fs/existsSync marker))))]
            (await (sleep 300))
            (.end (.-stdin (:proc holder)))
            (await attempt)
            (is (= false @shared) "entered only after the holder left")
            (is (= [0 nil] (js->clj (await (:exit holder)))))))))))

(deftest a-symlinked-route-to-the-body-takes-the-same-lock
  (let [dir (tu/tmp-dir)
        link (path/join (tu/tmp-dir) "worlds")]
    (fs/symlinkSync dir link)
    (is (= (single/lock-name (path/join dir "body.sock")) (single/lock-name (path/join link "body.sock"))))
    (is (not= (single/lock-name (path/join dir "body.sock")) (single/lock-name (path/join (tu/tmp-dir) "body.sock"))))))
