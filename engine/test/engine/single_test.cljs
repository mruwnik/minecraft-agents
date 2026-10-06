(ns engine.single-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.bodies :as bodies]
            [engine.main :as main]
            [engine.single :as single]
            [engine.test-util :as tu]
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

(deftest reclaiming-a-lock-judged-stale-earlier-leaves-a-fresh-one-alone
  (let [lock (str (path/join (tu/tmp-dir) "body.sock") ".lock")]
    (fs/mkdirSync lock)
    (single/reclaim-stale! lock)
    (is (fs/existsSync lock) "a waiter that saw the old lock must not remove the lock made since")))

(deftest a-stale-lock-is-reclaimed
  (let [lock (str (path/join (tu/tmp-dir) "body.sock") ".lock")
        old (- (/ (js/Date.now) 1000) 60)]
    (fs/mkdirSync lock)
    (fs/utimesSync lock old old)
    (single/reclaim-stale! lock)
    (is (not (fs/existsSync lock)))))

(deftest a-lock-held-longer-than-the-stale-age-is-kept-by-the-holder
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lock (str (path/join (tu/tmp-dir) "body.sock") ".lock")]
          (let [sock (str (subs lock 0 (- (count lock) 5)))
                old (- (/ (js/Date.now) 1000) 60)]
            (await (single/with-replace-lock
                     sock
                     (fn ^:async hold []
                       (fs/utimesSync lock old old)
                       (await (js/Promise. (fn [r] (js/setTimeout r (+ 50 (/ single/lock-stale-ms 5))))))
                       (is (not (single/lock-stale? lock)) "the heartbeat refreshed the lock"))))))))))

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
