(ns world-test.runner-test
  (:require [cljs.test :refer [deftest is async]]
            ["child_process" :as cp]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.reader]
            [world-test.fixture :as f]
            [world-test.runner :as r]))

(deftest the-body-launch-argv-caps-the-v8-heap
  (let [argv (vec (r/body-argv {:body "B" :world "w"} "/tmp/s.edn"))
        script (.indexOf argv "out/body.cjs")]
    (is (= ["--max-old-space-size=128" "--max-semi-space-size=8"] (subvec argv 0 script)))
    (is (= ["--agent" "B" "--world" "w" "--scenario" "/tmp/s.edn" "--fresh"]
           (vec (drop (inc script) argv))))))

(deftest build-plot-retries-an-unloaded-plot-then-fails-as-a-setup-error
  (let [origin [20000 150 20000]
        c {:plot {:height 4 :floor "stone"} :blocks [[:fill [0 0 0] [1 1 1] "water"]]}
        unloaded "That position is not loaded"
        run (fn [bad-rounds]
              (let [sent (atom []) left (atom bad-rounds)]
                (.then (r/build-plot!
                        {:send (fn [cmds] (swap! sent into cmds)
                                 (js/Promise.resolve
                                  (mapv (fn [cmd] (if (and (re-find #"^fill .* water" cmd) (pos? @left)) (do (swap! left dec) unloaded) "ok")) cmds)))
                         :sleep (fn [_] (js/Promise.resolve nil))}
                        f/default-grid origin c)
                       (fn [res] {:res res :sent @sent}))))]
    (async done
      (-> (js/Promise.resolve)
          (.then #(run 0))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res))
                   (is (= 1 (count (filter #(re-find #"water" %) sent))))))
          (.then #(run 2))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res) "loaded on the third try")
                   (is (= 3 (count (filter #(re-find #"water" %) sent))))))
          (.then #(run 9))
          (.then (fn [{:keys [res]}]
                   (is (re-find #"plot not built.*not loaded" res))
                   (done)))))))

(deftest ensure-at-start-retries-the-tp-then-fails-with-a-message
  (let [origin [20000 150 20000]
        c {:body {:at [16.5 0 16.5]}}
        at "x has the following entity data: [20016.5d, 150.0d, 20016.5d]"
        spawn "x has the following entity data: [9.5d, 68.0d, 0.5d]"
        run (fn [replies]
              (let [sent (atom []) left (atom replies)]
                (.then (r/ensure-at-start!
                        {:send (fn [cmds] (swap! sent into cmds)
                                 (js/Promise.resolve (mapv (fn [_] (let [x (first @left)] (swap! left rest) x)) (filter #(re-find #"^data get" %) cmds))))
                         :sleep (fn [_] (js/Promise.resolve nil))}
                        origin "B" c)
                       (fn [res] {:res res :sent @sent}))))]
    (async done
      (-> (js/Promise.resolve)
          (.then #(run [at]))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res))
                   (is (= 0 (count (filter #(re-find #"^tp " %) sent))) "already there: no extra tp")))
          (.then #(run [spawn at]))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res))
                   (is (= 1 (count (filter #(re-find #"^tp " %) sent))) "one retry tp")))
          (.then #(run [spawn spawn spawn spawn spawn]))
          (.then (fn [{:keys [res]}]
                   (is (re-find #"not at its start" res))
                   (done)))))))

(deftest a-register-case-counts-events-from-before-the-register-was-put
  (let [pre {:offset 10 :from-ms 1000} post {:offset 90 :from-ms 9000}]
    (is (= pre (r/watch-window true pre post)) "a trigger firing inside the settle is counted")
    (is (= post (r/watch-window false pre post)) "no register: the settle still hides leftover events")))

(deftest stop-on-fail-is-an-option-and-ends-the-batch-after-a-non-pass
  (is (true? (:stop-on-fail (r/parse-args #js ["--stop-on-fail"]))))
  (is (nil? (:stop-on-fail (r/parse-args #js []))))
  (is (r/stop-batch? {:stop-on-fail true} [{:status :pass} {:status :fail}]))
  (is (r/stop-batch? {:stop-on-fail true} [{:status :error}]))
  (is (not (r/stop-batch? {:stop-on-fail true} [{:status :pass} {:status :skipped}])))
  (is (not (r/stop-batch? {} [{:status :fail}]))))

(deftest the-event-reader-follows-a-log-rotation
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "wt-rot-"))
        file (path/join dir "events.edn")
        seg (str file ".1")
        ev (fn [& ks] (apply str (map #(str "{:k " % "}\n") ks)))
        ks (fn [evs] (mapv :k evs))]
    (fs/writeFileSync file (ev 1 2))
    (let [cur (r/log-cursor file)]
      (fs/appendFileSync file (ev 3))
      (is (= [3] (ks (r/read-events-from file cur))) "no rotation: the tail")
      (fs/renameSync file seg)
      (fs/writeFileSync file (ev 4 5 6 7 8 9 10 11))
      (is (= [3 4 5 6 7 8 9 10 11] (ks (r/read-events-from file cur))) "rotated, new file longer than the offset: tail of .1 then the new file")
      (fs/renameSync seg (str file ".2"))
      (fs/renameSync file seg)
      (fs/writeFileSync file (ev 12))
      (is (= [3 4 5 6 7 8 9 10 11 12] (ks (r/read-events-from file cur))) "rotated twice")
      (let [rotate! (fn [n]
                      (fs/rmSync (str file ".3") #js {:force true})
                      (fs/renameSync (str file ".2") (str file ".3"))
                      (fs/renameSync seg (str file ".2"))
                      (fs/renameSync file seg)
                      (fs/writeFileSync file (ev n)))]
        (rotate! 13)
        (rotate! 14)
        (is (= [4 5 6 7 8 9 10 11 12 13 14] (ks (r/read-events-from file cur)))
            "the segment active at the cursor was rotated out: every kept segment oldest first, then the active file")))
    (fs/rmSync dir #js {:recursive true :force true})))

(deftest an-await-step-counts-events-from-the-watch-window
  (let [opts {:world "wt-await-769ce017" :body "Probe"}
        file (r/events-file opts)
        pre {:offset {:ino nil :pos 0} :from-ms 500}
        post {:offset {:ino nil :pos 0} :from-ms 2000}
        run (fn [window]
              (r/run-steps! opts [0 0 0] {:act [[:await {:kind :slept} 1]]} (:offset window) (:from-ms window)))
        cleanup #(fs/rmSync (path/join (r/repo-path "worlds" "wt-await-769ce017")) #js {:recursive true :force true})]
    (fs/mkdirSync (path/dirname file) #js {:recursive true})
    (fs/writeFileSync file "{:kind :slept :time-ms 1000}\n")
    (async done
      (-> (js/Promise.resolve)
          (.then #(run (r/watch-window true pre post)))
          (.then (fn [ids] (is (= #{} ids) "a register case sees an event from before the settle ended")))
          (.then #(.then (run (r/watch-window false pre post))
                         (fn [_] (is false "no register: the event before the settle is hidden"))
                         (fn [e] (is (re-find #"timed out" (.-message e))))))
          (.finally (fn [] (cleanup) (done)))))))

(deftest the-exit-code-is-zero-only-when-every-case-ran-and-passed
  (is (= 0 (r/exit-code [{:status :pass} {:status :pass}])))
  (is (= 1 (r/exit-code [{:status :pass} {:status :fail}])))
  (is (= 1 (r/exit-code [{:status :error}])))
  (is (= 1 (r/exit-code [{:status :skipped}])))
  (is (= 1 (r/exit-code [])) "no result is not a pass"))

(deftest results-are-written-to-the-file-as-they-come
  (let [file (path/join (os/tmpdir) (str "wt-results-" (.-pid js/process) ".edn"))]
    (r/write-results! {:results file} [{:status :pass :id "a"}])
    (is (= "[{:status :pass, :id \"a\"}]" (fs/readFileSync file "utf8")))
    (r/write-results! {:results file} [{:status :pass :id "a"} {:status :fail :id "b"}])
    (is (= 2 (count (cljs.reader/read-string (fs/readFileSync file "utf8")))))
    (r/write-results! {} [{:status :pass}])
    (fs/unlinkSync file)))

(defn- contend-env
  "The contender child's settings (set by the parent test): plot index, scratch dir, rounds."
  []
  (let [e (.-env js/process)]
    (when-let [plot (.-WT_CONTEND_PLOT e)]
      {:plot (js/parseInt plot 10) :dir (.-WT_CONTEND_DIR e)})))

(defn- take-plot!
  "Real acquire-plot! on a one-plot range, retried while another live runner holds it."
  [plot deadline]
  (or (try (r/acquire-plot! plot (inc plot)) (catch :default _ nil))
      (do (when (> (.now js/Date) deadline) (throw (js/Error. "contender gave up")))
          (r/pause-sync 1)
          (recur plot deadline))))

(deftest lease-contender-child
  "Not a test on its own: the process the next test spawns (it runs only when WT_CONTEND_PLOT is set). Takes the real
  plot lease 6 times in turn; after the critical section it releases (real release-plot!) or leaves the lease as a
  crashed runner would, holding a dead PID or empty. Two inside the critical section at once is logged."
  (when-let [{:keys [plot dir]} (contend-env)]
    (doseq [n (range 6)]
      (take-plot! plot (+ (.now js/Date) 60000))
      (let [cs (path/join dir "cs")]
        (try (fs/mkdirSync cs) (catch :default _ (fs/appendFileSync (path/join dir "bad") "x")))
        (r/pause-sync 2)
        (fs/rmSync cs #js {:recursive true :force true}))
      (case (mod n 3)
        0 (r/release-plot! plot)
        1 (fs/writeFileSync (r/lease-file plot) "99999999")
        2 (fs/writeFileSync (r/lease-file plot) "")))
    (is true)))

(deftest racing-reclaimers-of-a-dead-holder-never-share-the-lock
  (async done
    (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "wt-reclaim-"))
          plot (+ 100000 (.-pid js/process))
          bundle (aget js/process.argv 1)
          env (js/Object.assign #js {} js/process.env #js {"WT_CONTEND_PLOT" (str plot) "WT_CONTEND_DIR" dir})
          run (fn [] (js/Promise. (fn [res] (.on (cp/spawn (.-execPath js/process)
                                                           #js [bundle "--test=world-test.runner-test/lease-contender-child"]
                                                           #js {:env env :stdio "ignore"})
                                                 "close" res))))]
      (fs/mkdirSync r/lease-dir #js {:recursive true})
      (fs/writeFileSync (r/lease-file plot) "99999999")
      (-> (js/Promise.all #js [(run) (run) (run) (run)])
          (.then (fn [codes]
                   (is (every? zero? (array-seq codes)) "every contender finished all its rounds")
                   (is (not (fs/existsSync (path/join dir "bad"))) "never two holders at once")))
          (.finally (fn []
                      (fs/rmSync (r/lease-file plot) #js {:force true})
                      (fs/rmSync (str (r/lease-file plot) ".guard") #js {:force true})
                      (fs/rmSync dir #js {:recursive true :force true})
                      (done)))))))

(deftest finishing-a-run-removes-its-own-temp-dir-only
  (async done
    (let [body (str "WtLeak" (.-pid js/process))
          dir (r/run-dir {:body body})
          other (fs/mkdtempSync (path/join (os/tmpdir) "wt-other-"))]
      (fs/mkdirSync dir #js {:recursive true})
      (fs/writeFileSync (path/join dir "body.log") "log")
      (fs/writeFileSync (path/join dir "scenario.edn") "{}")
      (-> (r/finish-run! {:body body} [{:status :pass} {:status :skipped}])
          (.then (fn []
                   (is (not (fs/existsSync dir)))
                   (is (fs/existsSync other))))
          (.finally (fn [] (fs/rmSync other #js {:recursive true :force true}) (done)))))))

(deftest a-failed-or-broken-run-keeps-its-temp-dir-for-the-log
  (async done
    (let [body (str "WtKeep" (.-pid js/process))
          dir (r/run-dir {:body body})
          keeps? (fn [results]
                   (fs/mkdirSync dir #js {:recursive true})
                   (fs/writeFileSync (path/join dir "body.log") "log")
                   (.then (r/finish-run! {:body body} results)
                          (fn [] (let [kept (fs/existsSync (path/join dir "body.log"))]
                                   (fs/rmSync dir #js {:recursive true :force true})
                                   kept))))]
      (-> (keeps? [{:status :pass} {:status :fail}])
          (.then (fn [failed] (is failed) (keeps? [{:status :error}])))
          (.then (fn [errored] (is errored) (keeps? nil)))
          (.then (fn [broken] (is broken)))
          (.finally (fn [] (fs/rmSync dir #js {:recursive true :force true}) (done)))))))

(deftest a-new-run-starts-the-body-log-empty
  (let [body (str "WtLog" (.-pid js/process))
        dir (r/run-dir {:body body})
        log (path/join dir "body.log")]
    (fs/mkdirSync dir #js {:recursive true})
    (fs/writeFileSync log "the previous run")
    (r/reset-body-log! {:body body})
    (let [left (fs/existsSync log)]
      (fs/rmSync dir #js {:recursive true :force true})
      (is (not left) "the kept log of an earlier run does not mix with this one"))))

(deftest create-file-exclusive-never-shows-an-empty-file-and-leaves-an-existing-one
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "wt-create-"))
        file (path/join dir "lease")]
    (is (true? (r/create-file-exclusive! file "123")))
    (is (= "123" (fs/readFileSync file "utf8")))
    (is (false? (r/create-file-exclusive! file "456")))
    (is (= "123" (fs/readFileSync file "utf8")) "the holder's file is untouched")
    (is (= ["lease"] (vec (fs/readdirSync dir))) "no temp file is left behind")
    (fs/writeFileSync file "")
    (is (false? (r/create-file-exclusive! file "789")) "an empty leftover still blocks")
    (is (= "" (fs/readFileSync file "utf8")))
    (fs/rmSync dir #js {:recursive true :force true})))

(deftest time-disturbed-when-the-clock-jumped-beyond-the-elapsed-ticks
  (let [c {:act [[:wait-s 5]]}]
    (is (false? (r/time-disturbed? c 1000 1600 30000)) "30 s = 600 ticks")
    (is (false? (r/time-disturbed? c 1000 1500 30000)) "a lagging server stays inside the tolerance")
    (is (true? (r/time-disturbed? c 1000 14000 30000)) "set to night")
    (is (true? (r/time-disturbed? c 14000 1000 30000)) "set back")
    (is (false? (r/time-disturbed? c 23900 500 30000)) "wraps at 24000")
    (is (false? (r/time-disturbed? c nil 500 30000)) "no reading")
    (is (false? (r/time-disturbed? c 1000 1000 300000)) "a stopped daylight cycle is no jump")
    (is (false? (r/time-disturbed? c 1000 4240 180000)) "18 TPS over 3 min is no jump")
    (is (false? (r/time-disturbed? c 1000 1000 30000)) "no movement")
    (is (true? (r/time-disturbed? c 1000 6000 30000)) "forward jump")
    (is (true? (r/time-disturbed? c 6000 1000 300000)) "back jump after a stopped cycle")
    (is (false? (r/time-disturbed? {:act [[:time-set 14000]]} 1000 14000 30000)) "the case set it itself")))

(deftest a-failed-case-in-a-disturbed-world-is-inconclusive
  (is (= :inconclusive (:status (r/mark-time-disturbed {:status :fail :why "x"} true))))
  (is (= :pass (:status (r/mark-time-disturbed {:status :pass} true))))
  (is (= :error (:status (r/mark-time-disturbed {:status :error} true))))
  (is (= :fail (:status (r/mark-time-disturbed {:status :fail} false)))))

(deftest allow-time-needs-a-time-log-so-every-time-set-is-logged
  (is (thrown-with-msg? js/Error #"--time-log" (r/parse-args #js ["--allow-time"])))
  (is (true? (:allow-time (r/parse-args #js ["--allow-time" "--time-log" "f.log"]))))
  (is (nil? (:allow-time (r/parse-args #js [])))))

(deftest check-reports-one-result-per-case-and-names-broken-files
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "wt-check-"))
        good (str "{:name \"ok\" :plot {:height 4} :body {:at [1 1 1]} :time :any\n"
                  " :expect [{:event :x :within-s 5}]}")
        no-check "{:name \"empty\" :plot {:height 4} :body {:at [1 1 1]} :time :any}"
        spit! (fn [n s] (fs/writeFileSync (path/join dir n) s))]
    (spit! "a-good.edn" good)
    (spit! "b-empty.edn" no-check)
    (spit! "c-broken.edn" "{:name \"x\" :plot {")
    (let [res (r/check-fixtures [dir])
          by-id (into {} (map (juxt :id identity)) res)]
      (fs/rmSync dir #js {:recursive true :force true})
      (is (= 3 (count res)))
      (is (= :pass (:status (by-id "a-good/ok"))))
      (is (= :fail (:status (by-id "b-empty/empty"))))
      (is (re-find #"checks nothing" (:why (by-id "b-empty/empty"))))
      (is (= :fail (:status (by-id "c-broken"))) "a file that does not parse is one failed result naming it")
      (is (= 1 (r/check-exit-code res)))
      (is (= 0 (r/check-exit-code [{:status :pass}]))))))

(deftest check-is-an-option
  (is (true? (:check (r/parse-args #js ["--check"])))))

(deftest a-time-independent-case-locks-as-day-when-the-run-may-set-the-time
  (is (= [:day :any :night :night-x :day]
         [(r/lock-phase {:allow-time true} {:time :any})
          (r/lock-phase {} {:time :any})
          (r/lock-phase {:allow-time true} {:time :night})
          (r/lock-phase {} {:time :night-exclusive})
          (r/lock-phase {} {:time :any :act [[:time-set 1000]]})])))

(defn share-env [files read-phase told]
  {:ops {:guard (fn [f] (f))
         :entries (fn [] @files)
         :put! (fn [pid e] (swap! files assoc pid e))
         :remove! (fn [pid] (swap! files dissoc pid))
         :alive? (constantly true)}
   :read-phase read-phase
   :sleep (fn [_] (js/Promise.resolve nil))
   :log (fn [& parts] (swap! told conj (apply str parts)))})

(deftest the-any-lock-joins-the-phase-read-from-the-world-when-nobody-holds
  (async done
    (let [files (atom {}) told (atom [])]
      (.then (r/acquire-shared! (share-env files #(js/Promise.resolve :night) told) :any "x")
             (fn [res]
               (is (= {:first? true} res))
               (is (= :night (:phase (get @files (.-pid js/process)))))
               (done))))))

(deftest the-day-lock-waits-out-a-night-holder-and-says-so
  (async done
    (let [files (atom {1 {:phase :night :state :hold :seq 1}})
          told (atom [])
          env (assoc (share-env files #(js/Promise.resolve :day) told)
                     :sleep (fn [_] (swap! files dissoc 1) (js/Promise.resolve nil)))]
      (.then (r/acquire-shared! (assoc-in env [:ops :alive?] (constantly true)) :day "x")
             (fn [res]
               (is (= {:first? true} res))
               (is (= ["waiting for time lock (day) held by 1 (x)"] @told))
               (done))))))

(deftest the-previous-body-stops-before-the-time-lock-wait-and-the-new-one-starts-under-the-lock
  (let [calls (atom [])
        io (fn [first?] {:stop! (fn [] (swap! calls conj :stop) (js/Promise.resolve nil))
                         :acquire! (fn [] (swap! calls conj :acquire) (js/Promise.resolve {:first? first?}))
                         :first-set! (fn [] (swap! calls conj :set) (js/Promise.resolve true))
                         :start! (fn [] (swap! calls conj :start) (js/Promise.resolve nil))})]
    (async done
      (-> (r/hold-phase! (io true))
          (.then (fn [ok] (is (true? ok)) (is (= [:stop :acquire :set :start] @calls)) (reset! calls [])
                   (r/hold-phase! (io false))))
          (.then (fn [ok] (is (true? ok)) (is (= [:stop :acquire :start] @calls)) (reset! calls [])
                   (r/hold-phase! (assoc (io true) :acquire! #(js/Promise.reject (js/Error. "no rcon"))))))
          (.then (fn [_] (is false "hold-phase! must reject when acquire! rejects")))
          (.catch (fn [e] (is (= "no rcon" (.-message e))) (is (= [:stop] @calls))))
          (.then done)))))

(defn fake-watch-io
  "A fake clock: now advances by 500 per sleep, plus the jumps (a map sleep-number -> extra ms)."
  [jumps events]
  (let [t (atom 1000000) n (atom 0)]
    {:now (fn [] @t)
     :sleep (fn [_] (swap! n inc) (swap! t + 500 (get jumps @n 0)) (js/Promise.resolve nil))
     :events (fn [] (events @t))}))

(deftest watch-ends-stalled-when-the-clock-jumps-between-polls
  (async done
    (let [c {:limit-s 100 :expect [{:event {:kind :never} :within-s 90}]}
          clock (r/stall-clock 1000000)]
      (.then (r/watch-with! (fake-watch-io {2 (* 2 r/stall-gap-ms)} (constantly [])) c 1000000 #{} clock)
             (fn [res]
               (is (= [:stalled] (mapv :status res)))
               (done))))))

(deftest watch-ends-stalled-when-the-gap-came-before-its-first-poll
  (async done
    (let [c {:limit-s 100 :expect [{:event {:kind :never} :within-s 90}]}
          clock (r/stall-clock 1000000)]
      (r/tick! clock (+ 1000000 (* 2 r/stall-gap-ms)))
      (.then (r/watch-with! (fake-watch-io {} (constantly [])) c 1000000 #{} clock)
             (fn [res]
               (is (= [:stalled] (mapv :status res)))
               (done))))))

(deftest watch-without-a-gap-runs-to-the-limit-as-a-fail
  (async done
    (let [c {:limit-s 3 :expect [{:event {:kind :never} :within-s 1}]}
          clock (r/stall-clock 1000000)]
      (.then (r/watch-with! (fake-watch-io {} (constantly [])) c 1000000 #{} clock)
             (fn [res]
               (is (= [:fail] (mapv :status res)))
               (done))))))

(deftest a-poll-gap-far-over-the-poll-interval-is-a-stall
  (is (false? (r/stalled? 1000 1500)))
  (is (false? (r/stalled? 1000 (+ 1000 r/stall-gap-ms))))
  (is (true? (r/stalled? 1000 (+ 1001 r/stall-gap-ms)))))

(deftest a-stalled-run-turns-undecided-expectations-and-a-failure-inconclusive
  (let [expects [{:status :pass} {:status :pending}]
        marked (r/stall-results expects 6400000)]
    (is (= [:pass :stalled] (mapv :status marked)))
    (is (re-find #"6400" (:evidence (second marked))))
    (is (= :inconclusive (:status (r/mark-stalled {:status :fail :expects marked}))))
    (is (re-find #"stalled" (:why (r/mark-stalled {:status :fail :expects marked}))))
    (is (= :fail (:status (r/mark-stalled {:status :fail :expects [{:status :fail}]}))))
    (is (= :fail (:status (r/mark-stalled {:status :fail :expects [{:status :fail} {:status :stalled}]}))))
    (is (= :pass (:status (r/mark-stalled {:status :pass :expects [{:status :pass}]}))))))
