(ns world-test.runner-test
  (:require [cljs.test :refer [deftest is are async]]
            ["child_process" :as cp]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.reader]
            [world-test.fixture :as f]
            [world-test.runner :as r]))

(deftest the-body-launch-argv-caps-the-v8-heap
  (let [argv (vec (r/body-argv {:body "B" :world "w"} "/tmp/s.edn"))
        script (.indexOf argv "out/body-release.cjs")]
    (is (= ["--max-old-space-size=128" "--max-semi-space-size=8"] (subvec argv 0 script)))
    (is (= ["--agent" "B" "--world" "w" "--scenario" "/tmp/s.edn" "--fresh"]
           (vec (drop (inc script) argv))))))

(deftest a-restoring-launch-keeps-engine-edn-by-leaving-out-fresh
  (is (not-any? #{"--fresh"} (r/body-argv {:body "B" :world "w"} "/tmp/s.edn" true))))

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

(deftest the-happy-paths-of-build-and-start-check-sleep-never
  (let [sleeps (atom 0)
        io (fn [send] {:send send :sleep (fn [_] (swap! sleeps inc) (js/Promise.resolve nil))})
        at "x has the following entity data: [20016.5d, 150.0d, 20016.5d]"]
    (async done
      (-> (r/build-plot! (io (fn [cmds] (js/Promise.resolve (mapv (constantly "ok") cmds))))
                         f/default-grid [20000 150 20000]
                         {:plot {:height 4 :floor "stone"} :blocks [[:fill [0 0 0] [1 1 1] "water"]]})
          (.then (fn [res] (is (nil? res))))
          (.then #(r/ensure-at-start! (io (fn [cmds] (js/Promise.resolve (mapv (constantly at) cmds))))
                                      [20000 150 20000] "B" {:body {:at [16.5 0 16.5]}}))
          (.then (fn [res]
                   (is (nil? res))
                   (is (zero? @sleeps) "no fixed sleep when the world is already right")
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
                   (is (= 1 (count (filter #(re-find #"^tp " %) sent))) "one retry tp")
                   (is (= "gamemode survival B" (last sent)) "survival only once the body stands at its start")))
          (.then #(run (concat (repeat 12 spawn) [at])))
          (.then (fn [{:keys [res]}]
                   (is (nil? res) "entity data that lags after login is polled for about 5 s")))
          (.then #(run (repeat 30 spawn)))
          (.then (fn [{:keys [res sent]}]
                   (is (re-find #"not at its start" res))
                   (is (not-any? #(re-find #"^gamemode" %) sent) "a body not at its start stays a spectator")
                   (done)))))))

(deftest park-repeats-until-the-server-has-the-body-as-a-spectator
  (let [none "No player was found"
        set-mode "Set B's game mode to Spectator Mode"
        run (fn [replies]
              (let [sent (atom []) left (atom replies)]
                (.then (r/park! {:send (fn [cmds] (swap! sent into cmds)
                                         (js/Promise.resolve (mapv (fn [_] (let [x (first @left)] (swap! left rest) x)) cmds)))
                                 :sleep (fn [_] (js/Promise.resolve nil))}
                                "B")
                       (fn [res] {:res res :sent @sent}))))]
    (async done
      (-> (js/Promise.resolve)
          (.then #(run [set-mode]))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res))
                   (is (= ["gamemode spectator B"] sent))))
          (.then #(run [""]))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res) "a body already a spectator gets no reply")
                   (is (= 1 (count sent)))))
          (.then #(run [none none set-mode]))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res) "a body still logging in is parked once the server finds it")
                   (is (= 3 (count sent)))))
          (.then #(run (repeat 40 none)))
          (.then (fn [{:keys [res]}]
                   (is (re-find #"could not be parked" res))
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

(deftest an-await-of-job-ignores-events-of-other-jobs
  (let [opts {:world "wt-await-of-job-52bfb309" :body "Probe"}
        file (r/events-file opts)
        window {:offset {:ino nil :pos 0} :from-ms 500}
        run (fn [pattern]
              (r/run-steps! opts [0 0 0] {:act [[:await pattern 1]]} (:offset window) (:from-ms window)))
        cleanup #(fs/rmSync (path/join (r/repo-path "worlds" "wt-await-of-job-52bfb309")) #js {:recursive true :force true})]
    (fs/mkdirSync (path/dirname file) #js {:recursive true})
    (fs/writeFileSync file "{:kind :slept :time-ms 1000 :context {:job-id 7}}\n")
    (async done
      (-> (js/Promise.resolve)
          (.then #(run {:kind :slept}))
          (.then (fn [ids] (is (= #{} ids) "without :of-job any job's event satisfies it")))
          (.then #(.then (run {:kind :slept :of-job true})
                         (fn [_] (is false ":of-job: another job's event must not satisfy it"))
                         (fn [e] (is (re-find #"timed out" (.-message e))))))
          (.finally (fn [] (cleanup) (done)))))))

(deftest a-reload-await-after-a-tool-step-ignores-reloads-from-before-it
  (is (= 5000 (r/await-since {:kind :world.reloaded} 1000 5000)))
  (is (= 1000 (r/await-since {:kind :world.reloaded} 1000 nil)) "no tool step before it")
  (is (= 1000 (r/await-since {:kind :slept} 1000 5000)) "other events keep the case window")
  (is (= 9000 (r/await-since {:kind :world.reloaded} 9000 5000))))

(def boot-error {:status :error :why "the body did not start within 90 s (see out)"})

(deftest boot-dead-needs-the-last-n-results-all-boot-failures
  (is (r/boot-dead? [{:status :pass} boot-error boot-error]))
  (is (not (r/boot-dead? [boot-error {:status :pass} boot-error])))
  (is (not (r/boot-dead? [boot-error])) "one is not a pattern")
  (is (not (r/boot-dead? [{:status :error :why "other"} {:status :error :why "other"}]))))

(deftest a-boot-dead-batch-stops-and-exits-6
  (is (r/stop-batch? {} [boot-error boot-error]))
  (is (not (r/stop-batch? {} [boot-error])))
  (is (= r/boot-exit (r/exit-code [{:status :pass} boot-error boot-error])))
  (is (= 6 r/boot-exit)))

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
    (is (false? (r/time-disturbed? c 1000 1600 30000 false)) "30 s = 600 ticks")
    (is (false? (r/time-disturbed? c 1000 1500 30000 false)) "a lagging server stays inside the tolerance")
    (is (true? (r/time-disturbed? c 1000 14000 30000 false)) "set to night")
    (is (true? (r/time-disturbed? c 14000 1000 30000 false)) "set back")
    (is (false? (r/time-disturbed? c 23900 500 30000 false)) "wraps at 24000")
    (is (false? (r/time-disturbed? c nil 500 30000 false)) "no reading")
    (is (false? (r/time-disturbed? c 1000 1000 300000 false)) "a stopped daylight cycle is no jump")
    (is (false? (r/time-disturbed? c 1000 4240 180000 false)) "18 TPS over 3 min is no jump")
    (is (false? (r/time-disturbed? c 1000 1000 30000 false)) "no movement")
    (is (false? (r/time-disturbed? c 12500 14300 30000 false 60)) "30 s at 60 TPS = 1800 ticks")
    (is (true? (r/time-disturbed? c 12500 14300 30000 false)) "the same advance at 20 TPS is a jump")
    (is (false? (r/time-disturbed? c 1000 6000 30000 false)) "a jump that keeps the day phase is no disturbance")
    (is (false? (r/time-disturbed? c 10950 1100 30000 false)) "the runner's phase refresh (time set 1000 at ~11000)")
    (is (true? (r/time-disturbed? c 10950 13500 30000 false)) "a day case sees the night")
    (is (true? (r/time-disturbed? {:time :night} 14000 1000 30000 false)) "a night case sees the day")
    (is (false? (r/time-disturbed? {:time :night} 14000 18000 30000 false)) "a night case stays in the night")
    (is (true? (r/time-disturbed? c 6000 14000 300000 false)) "jump into the night after a stopped cycle")
    (is (false? (r/time-disturbed? {:act [[:time-set 14000]]} 1000 14000 30000 false)) "the case set it itself")
    (is (false? (r/time-disturbed? c 14000 1000 30000 true)) "the body woke from sleep, which skipped the night")
    (is (true? (r/time-disturbed? c 1000 14000 30000 false)) "no wake, still a jump")
    (is (true? (r/woke? [{:source :job :kind :completed} {:source :body :kind :woke}])))
    (is (false? (r/woke? [{:source :job :kind :completed}])))))

(deftest a-failed-case-in-a-disturbed-world-is-inconclusive
  (is (= :inconclusive (:status (r/mark-time-disturbed {:status :fail :why "x"} true))))
  (is (= :pass (:status (r/mark-time-disturbed {:status :pass} true))))
  (is (= :error (:status (r/mark-time-disturbed {:status :error} true))))
  (is (= :fail (:status (r/mark-time-disturbed {:status :fail} false)))))

(deftest allow-time-needs-a-time-log-so-every-time-set-is-logged
  (is (thrown-with-msg? js/Error #"--time-log" (r/parse-args #js ["--allow-time"])))
  (is (true? (:allow-time (r/parse-args #js ["--allow-time" "--time-log" "f.log"]))))
  (is (nil? (:allow-time (r/parse-args #js [])))))

(defn check-in-repo
  "r/check-fixtures with the repository root (the tests run in dashboard/) as the source tree the event kinds come from."
  [paths]
  (let [old (.. js/process -env -WORLD_TEST_REPO)]
    (set! (.. js/process -env -WORLD_TEST_REPO) (path/resolve ".."))
    (try (r/check-fixtures paths)
         (finally (if old (set! (.. js/process -env -WORLD_TEST_REPO) old) (js-delete (.-env js/process) "WORLD_TEST_REPO"))))))

(deftest check-reports-one-result-per-case-and-names-broken-files
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "wt-check-"))
        good (str "{:name \"ok\" :plot {:height 4} :body {:at [1 1 1]} :time :any\n"
                  " :expect [{:event :x :within-s 5}]}")
        no-check "{:name \"empty\" :plot {:height 4} :body {:at [1 1 1]} :time :any}"
        spit! (fn [n s] (fs/writeFileSync (path/join dir n) s))]
    (spit! "a-good.edn" good)
    (spit! "b-empty.edn" no-check)
    (spit! "c-broken.edn" "{:name \"x\" :plot {")
    (let [res (check-in-repo [dir])
          by-id (into {} (map (juxt :id identity)) res)]
      (fs/rmSync dir #js {:recursive true :force true})
      (is (= 3 (count res)))
      (is (= :pass (:status (by-id "a-good/ok"))))
      (is (= :fail (:status (by-id "b-empty/empty"))))
      (is (re-find #"checks nothing" (:why (by-id "b-empty/empty"))))
      (is (= :fail (:status (by-id "c-broken"))) "a file that does not parse is one failed result naming it")
      (is (= 1 (r/check-exit-code res)))
      (is (= 0 (r/check-exit-code [{:status :pass}]))))))

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

(deftest hold-phase-takes-a-bare-first-set-result
  (async done
    (-> (r/hold-phase! {:stop! #(js/Promise.resolve nil) :acquire! #(js/Promise.resolve {:first? true})
                        :first-set! (fn [] true) :start! #(js/Promise.resolve nil)})
        (.then (fn [ok] (is (true? ok))))
        (.catch (fn [e] (is false (str "rejected: " (.-message e)))))
        (.then done))))

(deftest phase-time-fix-says-when-the-phase-window-was-left
  (is (= [nil nil 1000 1000 nil nil 14000 14000 nil nil nil 1000]
         (let [fix #(r/phase-time-fix %1 %2 r/default-day-limit)]
           [(fix :day 1000) (fix :day 8999) (fix :day 9000) (fix :day 18000)
            (fix :day 23500)
            (fix :night 14000) (fix :night 12999) (fix :night 22500)
            (fix :night-x 20000) (fix :any 18000)
            (r/phase-time-fix :day 10999 11000) (r/phase-time-fix :day 11000 11000)]))
      "day resets from the limit (default 9000), well before dusk"))

(deftest a-joiner-re-sets-a-drifted-phase-time-and-leaves-an-in-window-one
  (async done
    (let [sets (atom [])
          env (fn [ticks] {:day-limit r/default-day-limit :read-ticks #(js/Promise.resolve ticks)
                           :set-ticks! (fn [t why] (swap! sets conj [t why]) (js/Promise.resolve true))})]
      (-> (r/refresh-phase-time! (env 11500) :day "c1")
          (.then (fn [ok] (is (true? ok)) (is (= [1000] (map first @sets))) (reset! sets [])
                   (r/refresh-phase-time! (env 8000) :day "c1")))
          (.then (fn [_] (is (empty? @sets))
                   (r/refresh-phase-time! (env 9000) :night "c2")))
          (.then (fn [_] (is (= [14000] (map first @sets))) (reset! sets [])
                   (r/refresh-phase-time! (env 9000) :any "c3")))
          (.then (fn [_] (is (empty? @sets)) (done)))))))

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

(deftest watch-fails-a-case-whose-body-left-its-plot
  (async done
    (let [c {:limit-s 100 :expect [{:event {:kind :never} :within-s 90}]}
          calls (atom 0)
          io (assoc (fake-watch-io {} (constantly []))
                    :left-plot (fn [] (js/Promise.resolve (when (> (swap! calls inc) 2) "body left its plot: at x"))))]
      (.then (r/watch-with! io c 1000000 #{} (r/stall-clock 1000000))
             (fn [res]
               (is (= [:fail] (mapv :status res)))
               (is (re-find #"body left its plot" (:evidence (first res))))
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

(deftest valued-and-boolean-flags-parse
  (are [args k v] (= v (k (r/parse-args args)))
    #js ["--changed-since-pass"] :changed-since-pass true
    #js ["--check"] :check true
    #js ["--refresh-build"] :refresh-build true
    #js ["--stop-stale-bodies" "ProbePool"] :stop-stale-bodies "ProbePool"
    #js ["--night-batch" "n1"] :night-batch "n1"
    #js ["--list" "--night-classes"] :night-classes true
    #js ["--night-barrier" "3"] :night-barrier 3
    #js ["--night-batch-end" "n1"] :night-batch-end "n1"
    #js ["--match-id" "a/c1"] :match-ids #{"a/c1"}
    #js ["--match-id" "a/c1" "--match-id" "b/c2"] :match-ids #{"a/c1" "b/c2"}
    #js [] :night-batch nil
    #js [] :night-barrier nil
    #js [] :changed-since-pass nil))

(deftest phase-is-an-option
  (is (= "night" (:phase (r/parse-args #js ["--phase" "night"]))))
  (is (nil? (:phase (r/parse-args #js []))))
  (is (thrown-with-msg? js/Error #"--phase" (r/parse-args #js ["--phase" "dusk"]))))

(deftest select-phase-filters-by-the-time-class-of-each-case
  (let [cases [{:id "a/day" :time :day} {:id "a/any" :time :any} {:id "a/none"}
               {:id "a/night" :time :night} {:id "a/nx" :time :night-exclusive}
               {:id "a/set" :time :any :act [[:time-set 14000]]}
               {:id "a/any-set" :time :any :act [[:time-set 1000]]}]
        ids (fn [phase] (mapv :id (r/select-phase cases phase)))]
    (is (= ["a/day" "a/any" "a/none" "a/any-set"] (ids "day")))
    (is (= ["a/night" "a/set"] (ids "night")))
    (is (= ["a/nx"] (ids "night-exclusive")))
    (is (= (count cases) (count (r/select-phase cases nil))))))

(deftest retry-failed-is-an-option
  (is (= 2 (:retry-failed (r/parse-args #js ["--retry-failed" "2"]))))
  (is (nil? (:retry-failed (r/parse-args #js []))))
  (is (thrown-with-msg? js/Error #"--retry-failed" (r/parse-args #js ["--retry-failed" "x"]))))

(deftest a-pass-on-retry-is-flaky-and-keeps-the-first-failure
  (let [first-run {:id "a" :run 1 :status :fail :expects [{:status :fail :evidence "none seen"}]}
        flaky (r/settle-retries first-run [{:id "a" :run 1 :status :fail} {:id "a" :run 1 :status :pass}])]
    (is (= :flaky (:status flaky)))
    (is (= 2 (:retry flaky)))
    (is (= [{:status :fail :evidence "none seen"}] (:expects (:first-failure flaky))))))

(deftest a-case-failing-every-retry-stays-failed-with-the-first-evidence
  (let [first-run {:id "a" :run 1 :status :fail :why "x"}
        r1 (r/settle-retries first-run [{:id "a" :run 1 :status :fail :why "y"}])]
    (is (= :fail (:status r1)))
    (is (= "x" (:why r1)))
    (is (= 1 (:retries r1)))
    (is (= "y" (:why (:last-retry r1))) "the last retry's evidence is kept")))

(deftest only-failed-results-are-retried-and-flaky-is-not-a-failure
  (is (= 0 (r/exit-code [{:status :pass} {:status :flaky}])))
  (is (= 1 (r/exit-code [{:status :flaky} {:status :fail}])))
  (is (= [1] (map :n (r/retry-targets [{:n 0 :status :pass} {:n 1 :status :fail} {:n 2 :status :error}
                                       {:n 3 :status :inconclusive} {:n 4 :status :flaky}])))))

(deftest retry-failed!-reruns-failures-up-to-n-times-and-stops-for-a-passing-case
  (async done
    (let [calls (atom [])
          rerun! (fn [targets]
                   (swap! calls conj (mapv :id targets))
                   (js/Promise.resolve
                    (mapv (fn [t] (assoc t :status (if (and (= "a" (:id t)) (= 2 (count @calls))) :pass :fail))) targets)))
          results [{:id "a" :run 1 :status :fail} {:id "b" :run 1 :status :pass} {:id "c" :run 1 :status :fail}]]
      (-> (r/retry-failed! 3 results rerun!)
          (.then (fn [out]
                   (is (= [["a" "c"] ["a" "c"] ["c"]] @calls))
                   (is (= [:flaky :pass :fail] (mapv :status out)))
                   (is (= 3 (:retries (nth out 2))))
                   (done)))))))

(deftest retry-failed!-with-zero-retries-reruns-nothing
  (async done
    (-> (r/retry-failed! 0 [{:id "a" :run 1 :status :fail}] (fn [_] (throw (js/Error. "no"))))
        (.then (fn [out] (is (= [:fail] (mapv :status out))) (done))))))

(deftest a-passing-retry-is-reported-flaky-and-a-failing-one-as-it-is
  (is (= :flaky (:status (r/retry-view {:id "a" :run 1 :status :pass}))))
  (is (re-find #"retry" (:why (r/retry-view {:id "a" :run 1 :status :pass}))))
  (is (= {:id "a" :run 1 :status :fail :why "x"} (r/retry-view {:id "a" :run 1 :status :fail :why "x"}))))

(deftest retry-failed-with-stop-on-fail-is-an-error
  (is (thrown-with-msg? js/Error #"--stop-on-fail" (r/parse-args #js ["--retry-failed" "1" "--stop-on-fail"])))
  (is (thrown-with-msg? js/Error #"--retry-failed" (r/parse-args #js ["--stop-on-fail" "--retry-failed" "1"]))))

(deftest retry-groups-follow-each-cases-register-keeping-run-numbers
  (let [by-id {"a" {:id "a" :register :r1} "b" {:id "b" :register :r2} "c" {:id "c" :register :r1}}
        groups (r/retry-groups by-id [{:id "a" :run 1} {:id "b" :run 2} {:id "c" :run 1} {:id "a" :run 2}])]
    (is (= [[:r1 [[(by-id "a") 1] [(by-id "c") 1] [(by-id "a") 2]]] [:r2 [[(by-id "b") 2]]]]
           (mapv (fn [[reg pairs]] [reg (vec pairs)]) groups)))))

(deftest retried-results-map-back-to-the-failed-order
  (is (= [{:id "a" :run 1 :status :pass} {:id "b" :run 1 :status :fail}]
         (r/align-retries [{:id "a" :run 1 :status :fail} {:id "b" :run 1 :status :fail}]
                          {["a" 1] {:id "a" :run 1 :status :pass}})))
  (is (= [{:id "a" :run 1 :status :fail}] (r/align-retries [{:id "a" :run 1 :status :fail}] {}))))

(declare log-reload!)

(deftest a-case-seeds-its-zones-and-places-and-drops-only-its-own
  (let [repo (fs/mkdtempSync (path/join (os/tmpdir) "wt-shared-"))
        old (.. js/process -env -WORLD_TEST_REPO)
        world (path/join repo "worlds" "w")
        zones (path/join world "zones.edn")
        places (path/join world "places.json")
        opts {:body "ProbeX" :world "w" :shared-settle-ms 3000}
        c {:zones [{:name "box" :min [1 0 1] :max [2 3 2] :owner "Ann" :allow #{:dig}}]
           :places [{:name "home-$tag" :pos [4 0 5] :note "n"}]}
        names #(map :name (cljs.reader/read-string (fs/readFileSync zones "utf8")))
        markers #(map (fn [m] (.-name m)) (js/JSON.parse (fs/readFileSync places "utf8")))]
    (set! (.. js/process -env -WORLD_TEST_REPO) repo)
    (fs/mkdirSync world #js {:recursive true})
    (fs/writeFileSync zones "[{:name \"keep\" :min [0 0 0] :max [1 1 1] :owner \"Z\"}\n {:name \"wt-other-box\" :min [0 0 0] :max [1 1 1] :owner \"Z\"}]\n")
    (fs/writeFileSync places "[\n {\n  \"name\": \"mine\",\n  \"x\": 1,\n  \"y\": 2,\n  \"z\": 3\n }\n]\n")
    (js/setTimeout #(log-reload! opts ["/w/zones.edn" "/w/places.json"]) 150)
    (async done
      (-> (r/seed-shared! opts [20000 150 20000] c)
          (.then (fn []
                   (is (= ["keep" "wt-other-box" "wt-probex-box"] (names)))
                   (is (= {:min [20001 150 20001] :max [20002 153 20002] :owner "Ann"}
                          (select-keys (last (cljs.reader/read-string (fs/readFileSync zones "utf8"))) [:min :max :owner])))
                   (is (= ["mine" "home-wt-probex"] (markers)))
                   (let [m (aget (js/JSON.parse (fs/readFileSync places "utf8")) 1)]
                     (is (= [20004 150 20005 "wt-probex"] [(.-x m) (.-y m) (.-z m) (.-by m)])))
                   (r/drop-shared! opts)))
          (.then (fn []
                   (is (= ["keep" "wt-other-box"] (names)))
                   (is (= ["mine"] (markers)))
                   (is (not (fs/existsSync (str zones ".lock"))))
                   (is (not (fs/existsSync (str places ".lock"))))))
          (.catch (fn [e] (is false (.-message e))))
          (.finally (fn []
                      (if old (set! (.. js/process -env -WORLD_TEST_REPO) old) (js-delete (.-env js/process) "WORLD_TEST_REPO"))
                      (fs/rmSync repo #js {:recursive true :force true})
                      (done)))))))

(deftest cleanup-runs-every-step-even-after-a-failure
  (let [ran (atom [])
        step (fn [k fail?] #(do (swap! ran conj k) (if fail? (js/Promise.reject (js/Error. "x")) (js/Promise.resolve))))]
    (async done
      (-> (r/run-cleanup! [(step :cancel true) (step :rcon false) (step :drop false)])
          (.then (fn [] (is (= [:cancel :rcon :drop] @ran))))
          (.catch (fn [e] (is false (.-message e))))
          (.finally done)))))

(defn with-body-dir
  "Calls (f body-dir-path) with WORLD_TEST_REPO pointing at a temp repo, then removes it and restores the variable."
  [f]
  (let [repo (fs/mkdtempSync (path/join (os/tmpdir) "wt-until-"))
        old (.. js/process -env -WORLD_TEST_REPO)
        opts {:body "B" :world "w"}]
    (set! (.. js/process -env -WORLD_TEST_REPO) repo)
    (fs/mkdirSync (r/body-dir opts) #js {:recursive true})
    (-> (f opts (r/body-dir opts))
        (.finally (fn []
                    (if old (set! (.. js/process -env -WORLD_TEST_REPO) old) (js-delete (.-env js/process) "WORLD_TEST_REPO"))
                    (fs/rmSync repo #js {:recursive true :force true}))))))

(deftest until-step-passes-on-a-later-poll
  (async done
    (-> (with-body-dir
          (fn [opts dir]
            (js/setTimeout #(fs/writeFileSync (path/join dir "ready.edn") "{:ok true}") 700)
            (.then (r/until-step! opts [:file "ready.edn" {:ok true}] 10 nil nil) (constantly :passed))))
        (.then (fn [v] (is (= :passed v))))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest until-step-times-out-with-the-last-evidence
  (async done
    (-> (with-body-dir
          (fn [opts _] (.then (r/until-step! opts [:file "never.edn" {:ok true}] 1 nil nil) (constantly :passed))))
        (.then (fn [v] (is false (str "should have thrown, got " v))))
        (.catch (fn [e]
                  (is (re-find #":until \[:file \"never.edn\"\] not met after 1 s" (.-message e)))
                  (is (re-find #"never.edn does not exist" (.-message e)))))
        (.finally done))))

(deftest until-rcon-polls-the-reply-until-it-matches
  (async done
    (let [replies (atom ["food 20" "food 12" "food 5"])
          sent (atom [])
          io {:send (fn [cmds] (swap! sent into cmds) (js/Promise.resolve [(let [r (first @replies)] (swap! replies #(if (next %) (vec (rest %)) %)) r)]))
              :sleep (fn [_] (js/Promise.resolve nil))}]
      (-> (r/until-rcon-step! io "q B" "food [0-9]$" 10)
          (.then (fn [_] (is (= ["q B" "q B" "q B"] @sent))))
          (.catch (fn [e] (is false (.-message e))))
          (.finally done)))))

(deftest until-pos-polls-until-the-body-is-inside-the-plot-relative-bounds
  (async done
    (let [replies (atom ["B: [20003.5d, 150.0d, 20016.5d]" "B: [20022.5d, 150.0d, 20016.5d]"])
          sent (atom [])
          io {:send (fn [cmds] (swap! sent into cmds) (js/Promise.resolve [(let [r (first @replies)] (swap! replies #(if (next %) (vec (rest %)) %)) r)]))
              :sleep (fn [_] (js/Promise.resolve nil))}]
      (-> (r/until-pos-step! io "data get entity B Pos" [20000 150 20000] {:x-min 20} 10)
          (.then (fn [_] (is (= 2 (count @sent)))))
          (.catch (fn [e] (is false (.-message e))))
          (.finally done)))))

(deftest until-rcon-times-out-with-the-last-reply
  (async done
    (let [now (atom 0)
          io {:send (fn [_] (js/Promise.resolve ["food 20"]))
              :sleep (fn [ms] (swap! now + ms) (js/Promise.resolve nil))
              :now (fn [] @now)}]
      (-> (r/until-rcon-step! io "q" "food 5" 2)
          (.then (fn [_] (is false "should have thrown")))
          (.catch (fn [e] (is (re-find #":until-rcon \"q\" \"food 5\" not met after 2 s: last reply food 20" (.-message e)))))
          (.finally done)))))

(defn log-reload!
  "Appends a world.reloaded event for files to the body's event log, as the engine writes it."
  [opts files]
  (let [file (r/events-file opts)]
    (fs/mkdirSync (path/dirname file) #js {:recursive true})
    (fs/appendFileSync file (str (pr-str {:source :system :kind :world.reloaded :level :debug :time-ms (js/Date.now)
                                          :data {:files files}}) "\n"))))

(deftest await-reload-resolves-when-every-written-file-was-re-read-after-the-write
  (async done
    (-> (with-body-dir
          (fn [opts _]
            (let [since (js/Date.now)
                  cursor (r/log-cursor (r/events-file opts))]
              (js/setTimeout #(log-reload! opts ["/w/zones.edn"]) 300)
              (js/setTimeout #(log-reload! opts ["/w/places.json"]) 900)
              (r/await-reload! opts cursor since ["zones.edn" "places.json"] 10000))))
        (.then (fn [v] (is (true? v))))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest await-reload-matches-the-exact-file-name
  (async done
    (-> (with-body-dir
          (fn [opts _]
            (log-reload! opts ["/w/plans/xtest-b-z.edn" "/w/plans/test-c-z.edn"])
            (r/await-reload! opts {:ino nil :pos 0} 0 ["test-b-z.edn"] 600)))
        (.then (fn [v] (is (false? v) "another file ending in the name is no reload of it")))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest await-reload-gives-up-at-its-bound-and-ignores-older-events
  (async done
    (-> (with-body-dir
          (fn [opts _]
            (log-reload! opts ["/w/zones.edn"])
            (let [cursor {:ino nil :pos 0}]
              (r/await-reload! opts cursor (+ (js/Date.now) 5) ["zones.edn"] 600))))
        (.then (fn [v] (is (false? v))))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest tick-rate-of-reads-the-tick-query-reply
  (is (= 20 (r/tick-rate-of "The game is running normallyTarget tick rate: 20.0 per second.\nAverage time per tick: 2.6ms")))
  (is (= 60 (r/tick-rate-of "The game is running normallyTarget tick rate: 60.0 per second.")))
  (is (= 20 (r/tick-rate-of "")) "no reading means the vanilla rate")
  (is (= 20 (r/tick-rate-of nil))))

(deftest seed-shared-fails-loudly-when-the-body-never-re-reads
  (async done
    (-> (with-body-dir
          (fn [opts _]
            (r/seed-shared! (assoc opts :shared-settle-ms 300) [0 0 0] {:places [{:name "p-$tag" :pos [1 0 1]}]})))
        (.then (fn [v] (is false (str "should have thrown, got " v))))
        (.catch (fn [e] (is (re-find #"world.reloaded" (.-message e)) (.-message e))))
        (.finally done))))

(deftest seed-shared-ignores-a-reload-logged-while-it-waited-for-the-lock
  (async done
    (-> (with-body-dir
          (fn [opts _]
            (let [places (r/world-file opts "places.json")
                  lock (str places ".lock")]
              (fs/mkdirSync (path/dirname places) #js {:recursive true})
              (fs/writeFileSync lock (str (.-pid js/process) "\nother"))
              (js/setTimeout #(log-reload! opts ["/w/zones.edn" "/w/places.json"]) 100)
              (js/setTimeout #(fs/unlinkSync lock) 400)
              (r/seed-shared! (assoc opts :shared-settle-ms 700) [0 0 0] {:places [{:name "p-$tag" :pos [1 0 1]}]}))))
        (.then (fn [v] (is false (str "should have thrown, got " v))))
        (.catch (fn [e] (is (re-find #"world.reloaded" (.-message e)) (.-message e))))
        (.finally done))))

(deftest an-rcon-step-logs-its-reply
  (let [sent (atom []) logged (atom [])]
    (async done
      (-> (r/rcon-step! {:send (fn [cmds] (swap! sent into cmds) (js/Promise.resolve ["Bob has the data {SpawnX: 3}"]))
                         :log (fn [s] (swap! logged conj s))}
                        "data get entity Bob SpawnX")
          (.then (fn [_]
                   (is (= ["data get entity Bob SpawnX"] @sent))
                   (is (= ["rcon: data get entity Bob SpawnX -> Bob has the data {SpawnX: 3}"] @logged))))
          (.catch (fn [e] (is false (.-message e))))
          (.finally done)))))

(deftest check-fails-an-unknown-act-op-and-a-repeated-case-name
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "wt-check-"))
        case-str (fn [n op] (str "{:name \"" n "\" :plot {:height 4} :body {:at [1 1 1]} :time :any :act [[" op " 1]]"
                                 " :expect [{:event :x :within-s 5}]}"))
        spit! (fn [n s] (fs/writeFileSync (path/join dir n) s))]
    (spit! "a-op.edn" (case-str "bad" ":no-such-op"))
    (spit! "b-dup.edn" (str "{:cases [" (case-str "same" ":wait-s") " " (case-str "same" ":wait-s") "]}"))
    (let [res (check-in-repo [dir])]
      (fs/rmSync dir #js {:recursive true :force true})
      (is (= [:fail] (distinct (map :status (filter #(= "a-op/bad" (:id %)) res)))))
      (is (some #(and (= "b-dup/same" (:id %)) (= :fail (:status %)) (re-find #"repeated" (:why %))) res)
          "a case name used twice in one file fails"))))

(deftest check-fails-a-job-event-kind-that-no-source-emits
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "wt-check-"))
        case-str (fn [n kind] (str "{:name \"" n "\" :plot {:height 4} :body {:at [1 1 1]} :time :any"
                                   " :expect [{:event {:source :job :kind " kind "} :within-s 5}]}"))]
    (fs/writeFileSync (path/join dir "a-kind.edn") (str "{:cases [" (case-str "known" ":stopped") " " (case-str "stale" ":no.such.kind.anywhere") "]}"))
    (let [res (check-in-repo [dir])
          by-id (into {} (map (juxt :id identity)) res)]
      (fs/rmSync dir #js {:recursive true :force true})
      (is (= :pass (:status (by-id "a-kind/known"))))
      (is (= :fail (:status (by-id "a-kind/stale"))))
      (is (re-find #"no.such.kind.anywhere" (:why (by-id "a-kind/stale")))))))

(deftest every-real-world-fixture-parses-and-validates
  (let [res (check-in-repo ["../engine/fixtures/world"])
        bad (remove #(= :pass (:status %)) res)]
    (is (seq res))
    (is (empty? bad) (str (count bad) " fixture cases fail: " (pr-str (map (juxt :id :why) (take 5 bad)))))))

(deftest clear-stale-plans-drops-only-this-bodys-leftover-test-plans
  (async done
    (-> (with-body-dir
    (fn [opts _]
      (let [plans (r/plans-dir opts)
            names ["test-b-deep.edn" "test-b-pen.edn" "test-bb-x.edn" "test-c-deep.edn" "real.edn" "late-wt-b.edn" "late-wt-bb.edn"]]
        (fs/mkdirSync plans #js {:recursive true})
        (doseq [n names] (fs/writeFileSync (path/join plans n) "{}"))
        (r/clear-stale-plans! opts)
        (is (= ["late-wt-bb.edn" "real.edn" "test-bb-x.edn" "test-c-deep.edn"] (vec (sort (fs/readdirSync plans)))) "also a plan a case added by the CLI under the body's shared tag")
        (r/clear-stale-plans! {:body "B" :world "nope"})
        (js/Promise.resolve nil))))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest clear-body-notes-drops-only-this-bodys-notes-file-test
  (async done
    (-> (with-body-dir
    (fn [opts _]
      (let [dir (path/join (r/world-file opts "notes"))]
        (fs/mkdirSync dir #js {:recursive true})
        (doseq [n ["B.edn" "BB.edn"]] (fs/writeFileSync (path/join dir n) "{}"))
        (r/clear-body-notes! opts)
        (is (= ["BB.edn"] (vec (fs/readdirSync dir))))
        (r/clear-body-notes! opts)
        (js/Promise.resolve nil))))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest write-plans-settled-waits-for-the-body-to-re-read-each-plan-file
  (async done
    (-> (with-body-dir
          (fn [opts _]
            (let [plans (r/plans-dir opts)
                  c {:plans [{:id "z" :parts []} {:id "y" :parts []}]}
                  logged (atom nil)]
              (fs/mkdirSync plans #js {:recursive true})
              (js/setTimeout #(log-reload! opts [(path/join plans "test-b-z.edn")]) 300)
              (js/setTimeout (fn [] (reset! logged (js/Date.now)) (log-reload! opts [(path/join plans "test-b-y.edn")])) 900)
              (.then (r/write-plans-settled! opts c)
                     (fn [files] {:files files :at (js/Date.now) :logged @logged})))))
        (.then (fn [{:keys [files at logged]}]
                 (is (= ["test-b-z.edn" "test-b-y.edn"] (map path/basename files)))
                 (is (some? logged) "resolved only after the last plan was re-read")
                 (is (>= at logged))))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest write-plans-settled-rewrites-once-when-the-first-write-was-absorbed-silently
  (async done
    (-> (with-body-dir
          (fn [opts _]
            (let [plans (r/plans-dir opts)
                  file (path/join plans "test-b-z.edn")
                  mtime (atom nil)
                  watch (atom nil)]
              (fs/mkdirSync plans #js {:recursive true})
              ;; a body that swallowed the first write at its start logs only for a later change of the file
              (reset! watch (js/setInterval
                             (fn []
                               (when (fs/existsSync file)
                                 (let [m (.-mtimeMs (fs/statSync file))]
                                   (if (nil? @mtime)
                                     (reset! mtime m)
                                     (when (not= m @mtime) (log-reload! opts [file]) (js/clearInterval @watch))))))
                             20))
              (-> (r/write-plans-settled! (assoc opts :shared-settle-ms 1000) {:plans [{:id "z" :parts []}]})
                  (.finally #(js/clearInterval @watch))))))
        (.then (fn [files] (is (= ["test-b-z.edn"] (map path/basename files)))))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest write-plans-settled-fails-loudly-when-the-body-never-re-reads
  (async done
    (-> (with-body-dir
          (fn [opts _]
            (fs/mkdirSync (r/plans-dir opts) #js {:recursive true})
            (r/write-plans-settled! (assoc opts :shared-settle-ms 300) {:plans [{:id "z" :parts []}]})))
        (.then (fn [v] (is false (str "should have thrown, got " v))))
        (.catch (fn [e] (is (re-find #"world.reloaded for the plans" (.-message e)) (.-message e))))
        (.finally done))))

(deftest write-plans-settled-has-nothing-to-wait-for-without-plans
  (async done
    (-> (with-body-dir (fn [opts _] (r/write-plans-settled! opts {})))
        (.then (fn [v] (is (= [] v))))
        (.catch (fn [e] (is false (.-message e))))
        (.finally done))))

(deftest an-http-tool-answer-of-settling-is-retried-then-passed-on
  (async done
    (let [answers (atom ["{:ok false :reason \"settling\"}" "{:ok false :reason \"settling\"}" "{:ok true}"])
          slept (atom 0)
          run (fn [replies limit]
                (reset! answers replies)
                (r/exec-settled {:exec (fn [_] (let [a (first @answers)]
                                                 (swap! answers #(if (next %) (vec (rest %)) %))
                                                 (js/Promise.resolve {:code 0 :out a})))
                                 :sleep (fn [ms] (swap! slept + ms) (js/Promise.resolve nil))
                                 :limit-ms limit :every-ms 500}
                                ["x"]))]
      (-> (run @answers 10000)
          (.then (fn [res]
                   (is (= "{:ok true}" (:out res)) "retried until the body settled")
                   (is (= 1000 @slept))
                   (run ["{:ok false :reason \"settling\"}"] 1500)))
          (.then (fn [res]
                   (is (= "{:ok false :reason \"settling\"}" (:out res)) "after the limit the reply is passed on unchanged")
                   (done)))))))

(deftest a-submit-timeout-is-retried-with-the-same-request-id-until-the-body-answers
  (async done
    (let [calls (atom [])
          answers (atom ["{:ok false :reason :timeout :request-id \"r1\"}" "{:ok false :reason :timeout :request-id \"r1\"}"
                         "{:ok true :duplicate true :job {:id \"j7\"}}"])
          opts {:body "B" :world "w"
                :exec (fn [argv] (swap! calls conj argv)
                        (let [a (first @answers)]
                          (swap! answers #(if (next %) (vec (rest %)) %))
                          (js/Promise.resolve {:code 1 :out a})))
                :sleep (fn [_] (js/Promise.resolve nil))}]
      (-> (r/submit-job! opts {:job :x} [])
          (.then (fn [id]
                   (is (= "j7" id))
                   (is (= 3 (count @calls)))
                   (is (= 1 (count (set (map #(second (drop-while (complement #{"--request-id"}) %)) @calls)))) "one request id for every attempt")
                   (reset! answers ["{:ok false :reason :timeout}"])
                   (reset! calls [])
                   (r/submit-job! (assoc opts :submit-retries 2) {:job :x} [])))
          (.then (fn [_] (is false "gave up silently")))
          (.catch (fn [e]
                    (is (re-find #"submit refused" (.-message e)))
                    (is (= 3 (count @calls)) "bounded retries")
                    (done)))))))

(deftest the-case-goal-names-file-case-and-run-or-retry-only-above-the-first
  (is (= "test pen-check › closed" (r/case-goal {:id "pen-check/closed"} 1 0)))
  (is (= "test pen-check › closed (run 2)" (r/case-goal {:id "pen-check/closed"} 2 0)))
  (is (= "test pen-check › closed (retry 1)" (r/case-goal {:id "pen-check/closed"} 1 1)))
  (is (= "test pen-check › closed (run 2, retry 1)" (r/case-goal {:id "pen-check/closed"} 2 1)))
  (is (= "test solo" (r/case-goal {:id "solo"} 1 0))))

(deftest the-case-wait-names-what-the-expectations-wait-for
  (is (= "waiting: job.completed, no reflex.fired"
         (r/case-wait {:expect [{:event {:source :job :kind :completed} :within-s 30}
                                {:no-event {:source :reflex :kind :fired} :for-s 25}]})))
  (is (= "waiting: count job.started" (r/case-wait {:expect [{:count-event {:source :job :kind :started} :at-least 2}]})))
  (is (nil? (r/case-wait {:id "a/c"}))))

(deftest a-case-run-sets-the-body-goal-and-clears-it-after-an-error-too
  (async done
    (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "runner-goal-"))
          file (path/join dir "goal.edn")
          seen (atom [])]
      (-> (r/with-goal! dir "world-test a/b (run 1, retry 0)"
                        #(do (swap! seen conj (cljs.reader/read-string (fs/readFileSync file "utf8")))
                             (js/Promise.reject (js/Error. "setup failed"))))
          (.catch (fn [e] (swap! seen conj (.-message e))))
          (.then (fn []
                   (is (= "world-test a/b (run 1, retry 0)" (:text (first @seen))))
                   (is (= "world-test" (:by (first @seen))))
                   (is (= "setup failed" (second @seen)))
                   (is (not (fs/existsSync file)))
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(defn job-ev [kind id & {:as more}]
  (merge {:time-ms 1000100 :source :job :kind kind :context {:job-id id :chain [id]}} more))

(def never-expect [{:event {:source :job :kind :never} :within-s 90}])

(defn watch-case [c events]
  (r/watch-with! (fake-watch-io {} (constantly events)) c 1000000 #{"j1"} (r/stall-clock 1000000)))

(deftest a-job-case-ends-early-once-no-job-is-active
  (async done
    (.then (watch-case {:limit-s 100 :expect never-expect} [(job-ev :queued "j1") (job-ev :failed "j1" :message "parked: no tree")])
           (fn [res]
             (is (= [:fail] (mapv :status res)))
             (is (re-find #"no job active: failed parked: no tree" (:evidence (first res))))
             (is (< 80 (:ended-early-s (first res)) 100))
             (done)))))

(deftest the-early-end-names-the-jobs-near-miss
  (async done
    (.then (watch-case {:limit-s 100 :expect [{:event {:source :job :kind :found} :within-s 90}]}
                       [(job-ev :queued "j1") (job-ev :done "j1" :data {:reason :no-start}) (job-ev :completed "j1")])
           (fn [res]
             (is (re-find #"no job active: completed" (:evidence (first res))))
             (is (re-find #"no-start" (:evidence (first res))))
             (done)))))

(deftest a-job-case-keeps-waiting-while-a-job-is-active
  (async done
    (.then (watch-case {:limit-s 5 :expect [{:event {:kind :never} :within-s 2}]} [(job-ev :queued "j1")])
           (fn [res]
             (is (= [:fail] (mapv :status res)))
             (is (= "no matching event within 2 s" (:evidence (first res))) "judged by its own deadline, not ended early")
             (is (nil? (:ended-early-s (first res))))
             (done)))))

(deftest early-end-is-off-for-wait-full-register-and-no-event-cases
  (async done
    (let [ended [(job-ev :completed "j1")]
          keeps (fn [c] (.then (watch-case (merge {:limit-s 5} c) ended) (fn [res] (nil? (:ended-early-s (first res))))))]
      (-> (js/Promise.all #js [(keeps {:wait-full true :expect never-expect})
                               (keeps {:register [{:id "t"}] :expect never-expect})
                               (keeps {:expect [{:no-event {:kind :bad} :for-s 4}]})])
          (.then (fn [oks] (is (every? true? oks)) (done)))))))

(deftest early-end-waits-for-an-event-that-is-not-about-the-cases-jobs
  (async done
    (let [ended [(job-ev :completed "j1")]
          trigger {:event {:source :trigger :kind :fired} :within-s 10}
          keeps (fn [c evs] (.then (watch-case (merge {:limit-s 20} c) evs) (fn [res] (nil? (:ended-early-s (first res))))))]
      (-> (js/Promise.all #js [(keeps {:expect [trigger]} ended)
                               (keeps {:expect [{:event {:kind :never} :within-s 10}]} ended)
                               (keeps {:expect [{:event {:kind :never} :of-job true :within-s 10}]} ended)])
          (.then (fn [oks] (is (= [true true false] (vec oks))) (done)))))))

(deftest idle-grace-s-sets-how-long-a-job-case-waits-after-its-jobs-ended
  (async done
    (let [evs [(job-ev :completed "j1") (job-ev :done-later "j1" :time-ms 1010100)]
          io #(fake-watch-io {} (fn [t] (filterv (fn [e] (<= (:time-ms e) t)) evs)))
          c {:limit-s 30 :expect [{:event {:source :job :kind :done-later} :within-s 20}]}
          watch (fn [c] (r/watch-with! (io) c 1000000 #{"j1"} (r/stall-clock 1000000)))]
      (-> (js/Promise.all #js [(watch (assoc c :idle-grace-s 15)) (watch c)])
          (.then (fn [[long-grace default]]
                   (is (= [:pass] (mapv :status long-grace)) "the event 10 s after the end is waited for")
                   (is (= [:fail] (mapv :status default)) "the default grace is 3 s")
                   (done)))))))

(deftest early-end-waits-while-a-job-of-a-trigger-is-active
  (async done
    (.then (watch-case {:limit-s 20 :expect never-expect}
                       [(job-ev :completed "j1") (job-ev :queued "t1")])
           (fn [res] (is (nil? (:ended-early-s (first res)))) (done)))))

(deftest clear-goals-removes-the-goal-of-a-run-still-in-flight
  (async done
    (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "runner-goal-"))
          file (path/join dir "goal.edn")
          release (atom nil)
          in-flight (r/with-goal! dir "world-test a/b (run 1, retry 0)" #(js/Promise. (fn [resolve] (reset! release resolve))))]
      (is (fs/existsSync file))
      (r/clear-goals!)
      (is (not (fs/existsSync file)) "the exit and signal handlers call this")
      (js/setTimeout #(@release nil) 10)
      (.then in-flight (fn [] (fs/rmSync dir #js {:recursive true}) (done))))))

(deftest goal-cleanup-listens-for-exit-sigint-and-sigterm-once
  (let [counts #(mapv (fn [s] (.listenerCount js/process s)) ["exit" "SIGINT" "SIGTERM"])
        before (counts)]
    (r/install-goal-cleanup!)
    (r/install-goal-cleanup!)
    (is (= (mapv inc before) (counts)))))

(deftest weather-is-read-from-the-two-predicate-replies
  (is (= :thunder (r/weather-of ["Test passed" "Test passed"])))
  (is (= :rain (r/weather-of ["Test failed" "Test passed"])))
  (is (= :clear (r/weather-of ["Test failed" "Test failed"])))
  (is (nil? (r/weather-of ["Unknown command" "Test failed"]))))

(deftest read-weather-sends-the-queries-and-survives-an-rcon-failure
  (async done
    (let [sent (atom nil)]
      (-> (r/read-weather! (fn [cmds] (reset! sent cmds) (js/Promise.resolve ["Test failed" "Test passed"])))
          (.then (fn [w]
                   (is (= :rain w))
                   (is (= r/weather-commands @sent))
                   (r/read-weather! (fn [_] (js/Promise.reject (js/Error. "down"))))))
          (.then (fn [w] (is (nil? w)) (done)))))))

(deftest only-an-exclusive-night-case-locks-with-the-batch
  (is (= ["n1" nil nil]
         [(r/lock-batch {:night-batch "n1"} {:time :night-exclusive})
          (r/lock-batch {:night-batch "n1"} {:time :night})
          (r/lock-batch {} {:time :night-exclusive})])))

(deftest a-day-case-setting-dusk-starts-at-day-in-its-batch
  (is (= "n1" (r/lock-batch {:night-batch "n1"} {:time :day :act [[:time-set 12000]]})))
  (is (= [14000 1000] [(r/batch-start-ticks {:time :night-exclusive}) (r/batch-start-ticks {:time :day :act [[:time-set 12000]]})]))
  (is (false? (r/starts-night? {:time :day :act [[:time-set 12000]]})))
  (is (true? (r/starts-night? {:time :night-exclusive})))
  (is (true? (r/starts-night? {:time :any :act [[:time-set 14000]]}))))

(deftest a-batch-joiner-never-re-sets-the-time
  (is (= [false true false false]
         [(r/joiner-refresh? {:allow-time true :night-batch "n1"} :night-x {:act []})
          (r/joiner-refresh? {:allow-time true} :night-x {:act []})
          (r/joiner-refresh? {:allow-time true} :night {:act [[:time-set 1000]]})
          (r/joiner-refresh? {} :night {:act []})])))

(deftest the-night-class-line-names-the-class
  (is (= "a/b  natural" (r/night-class-line {:id "a/b" :time :night-exclusive :act []})))
  (is (= "a/c  barrier:1000" (r/night-class-line {:id "a/c" :time :night-exclusive :act [[:time-set 1000]]})))
  (is (= "a/d  solo" (r/night-class-line {:id "a/d" :time :night-exclusive :act [[:time-set 1000] [:time-set 14000]]})))
  (is (= "a/e  barrier:day-11500" (r/night-class-line {:id "a/e" :time :day :act [[:time-set 11500]]}))))

(deftest set-weather-sends-and-logs-only-when-allowed
  (async done
    (let [sent (atom [])
          file (str (os/tmpdir) "/weather-test-fixer-weather-control-" (js/Date.now) ".log")
          send (fn [cmds] (swap! sent into cmds) (js/Promise.resolve ["Set the weather"]))]
      (-> (r/apply-weather! send {:body "B" :card "c1"} :rain "case x")
          (.then (fn [ok] (is (false? ok)) (is (empty? @sent))
                   (r/apply-weather! send {:allow-time true :time-log file :body "B" :card "c1"} :rain "case x")))
          (.then (fn [ok]
                   (is (true? ok))
                   (is (= ["weather rain"] @sent))
                   (is (re-find #"weather rain by world-test B for card c1 \(case x\)" (fs/readFileSync file "utf8")))
                   (fs/unlinkSync file)
                   (done)))))))

(deftest weather-wanted-is-the-cases-or-clear
  (is (= [:rain :clear :clear]
         [(r/weather-wanted {:weather :rain}) (r/weather-wanted {}) (r/weather-wanted {:weather :clear})])))

(deftest parse-gamerule-reads-the-value-of-the-server-reply
  (is (= [100 0 nil]
         [(r/parse-gamerule "Gamerule players_sleeping_percentage is currently set to: 100")
          (r/parse-gamerule "Gamerule players_sleeping_percentage is currently set to: 0")
          (r/parse-gamerule "Unknown gamerule")])))

(defn- tmp-dir [] (fs/mkdtempSync (path/join (os/tmpdir) "fixer-pass-c-test-")))

(defn- fake-send
  "A send fn answering the players_sleeping_percentage query with old; records every command."
  [sent old]
  (fn [cmds]
    (swap! sent into cmds)
    (js/Promise.resolve (mapv #(if (= % "gamerule players_sleeping_percentage")
                                 (str "Gamerule players_sleeping_percentage is currently set to: " old)
                                 "ok") cmds))))

(deftest a-non-skip-night-batch-stops-the-early-skip-and-the-end-restores-the-old-value
  (async done
    (let [sent (atom [])
          dir (tmp-dir)
          log (path/join dir "time.log")
          opts {:allow-time true :time-log log :body "B" :card "c1" :sleep-state-dir dir}]
      (-> (r/start-sleep-rule! (fake-send sent 50) opts {:time :night-exclusive :act []} "n1" dir "why")
          (.then (fn [old]
                   (is (= 50 old))
                   (is (= ["gamerule players_sleeping_percentage" "gamerule players_sleeping_percentage 101"] @sent))
                   (is (re-find #"gamerule players_sleeping_percentage 101 by world-test B for card c1 \(why\)" (fs/readFileSync log "utf8")))
                   (is (= 50 (:old (r/read-sleep-state opts))) "recorded before the end")
                   (reset! sent [])
                   (r/end-night-batch! (fake-send sent 101) opts "n1" dir)))
          (.then (fn [_]
                   (is (= ["gamerule players_sleeping_percentage 50"] @sent))
                   (is (re-find #"gamerule players_sleeping_percentage 50 by world-test B" (fs/readFileSync log "utf8")))
                   (is (false? (r/night-batch-set? "n1" dir)) "the marker is gone")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-skip-batch-or-a-run-without-allow-time-keeps-the-server-rule
  (async done
    (let [sent (atom [])
          dir (tmp-dir)]
      (-> (r/start-sleep-rule! (fake-send sent 100) {:allow-time true :sleep-state-dir dir} {:time :night-exclusive :sleep-skip true :act []} "n1" dir "why")
          (.then (fn [old] (is (nil? old))
                   (r/start-sleep-rule! (fake-send sent 100) {:sleep-state-dir dir} {:time :night-exclusive :act []} "n2" dir "why")))
          (.then (fn [old]
                   (is (nil? old))
                   (is (empty? @sent))
                   (r/end-night-batch! (fake-send sent 100) {:allow-time true :sleep-state-dir dir} "n1" dir)))
          (.then (fn [_]
                   (is (empty? @sent) "nothing recorded, nothing restored")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

;; Pass C restore hardening (card 1fff343a)
(def night-rc {:time :night-exclusive :act []})

(defn- start! [sent old opts batch dir]
  (r/start-sleep-rule! (fake-send sent old) (assoc opts :allow-time true :sleep-state-dir dir) night-rc batch dir "why"))

(deftest an-unparsable-read-sets-nothing
  (async done
    (let [sent (atom []) dir (tmp-dir)]
      (-> (start! sent "garbage" {} "n1" dir)
          (.then (fn [old]
                   (is (nil? old))
                   (is (nil? (r/read-sleep-state {:sleep-state-dir dir})) "no old value recorded")
                   (is (not-any? #(re-find #" 101$" %) @sent) "the rule was never set")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-read-of-101-counts-as-the-vanilla-100
  (async done
    (let [sent (atom []) dir (tmp-dir)]
      (-> (start! sent 101 {} "n1" dir)
          (.then (fn [old]
                   (is (= 100 old))
                   (is (= 100 (:old (r/read-sleep-state {:sleep-state-dir dir}))))
                   (reset! sent [])
                   (fs/writeFileSync (path/join dir "world-test-sleep-rule.edn") (pr-str {:old 101 :batch "n1" :pid 2147483000}))
                   (r/restore-sleep-rule! (fake-send sent 101) {:sleep-state-dir dir} "crashed" {:only-dead? true})))
          (.then (fn [_]
                   (is (= ["gamerule players_sleeping_percentage 100"] @sent) "a recorded 101 restores 100")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest the-markers-live-in-one-machine-wide-dir-outside-any-checkout
  (let [home (path/join (os/homedir) ".local" "state" "minecraft-claude")]
    (is (= (path/join home "world-test-sleep-rule.edn") (r/sleep-state-file {})))
    (is (= (path/join home "night-batches") r/night-batch-marker-dir))))

(deftest two-starters-of-one-batch-set-once-and-record-the-real-value
  (async done
    (let [sent (atom []) dir (tmp-dir)]
      (-> (js/Promise.all #js [(start! sent 40 {} "n1" dir) (start! sent 101 {} "n1" dir)])
          (.then (fn [olds]
                   (is (= [40 nil] (vec olds)))
                   (is (= 1 (count (filter #(= "gamerule players_sleeping_percentage 101" %) @sent))))
                   (is (= 40 (:old (r/read-sleep-state {:sleep-state-dir dir}))))
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-leftover-state-restores-and-is-removed
  (async done
    (let [sent (atom []) dir (tmp-dir) opts {:sleep-state-dir dir}
          log (path/join dir "t.log")]
      (-> (start! sent 25 {:time-log log} "n1" dir)
          (.then (fn [_]
                   (reset! sent [])
                   (r/restore-sleep-rule! (fake-send sent 101) {:sleep-state-dir dir :body "B"} "crashed" {:only-dead? true})))
          (.then (fn [_]
                   (is (empty? @sent) "the recorder (our parent) is alive: a running batch is left alone")
                   (r/restore-sleep-rule! (fake-send sent 101) {:sleep-state-dir dir :body "B"} "crashed")))
          (.then (fn [_]
                   (is (= ["gamerule players_sleeping_percentage 25"] @sent))
                   (is (nil? (r/read-sleep-state opts)))
                   (is (re-find #"gamerule players_sleeping_percentage 25 by world-test B" (fs/readFileSync log "utf8")) "logged to the recorded time log")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest the-state-records-the-pool-pid-and-its-start-time-and-a-live-pool-is-not-restored-over
  (async done
    (let [sent (atom []) dir (tmp-dir) opts {:sleep-state-dir dir :owner-pid js/process.pid}]
      (-> (start! sent 25 opts "n1" dir)
          (.then (fn [_]
                   (let [st (r/read-sleep-state opts)]
                     (is (= js/process.pid (:pid st)) "the pool's pid, not the runner's parent")
                     (is (= (r/proc-start js/process.pid) (:start st)) "with its start time")
                     (is (some? (:start st))))
                   (reset! sent [])
                   (r/restore-sleep-rule! (fake-send sent 101) {:sleep-state-dir dir} "crashed" {:only-dead? true})))
          (.then (fn [_]
                   (is (empty? @sent) "the pool is alive: a dead-only restore leaves the batch")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-reused-pid-with-another-start-time-counts-as-dead
  (async done
    (let [sent (atom []) dir (tmp-dir)]
      (fs/writeFileSync (path/join dir "world-test-sleep-rule.edn") (pr-str {:old 60 :batch "n1" :pid js/process.pid :start "1"}))
      (-> (r/restore-sleep-rule! (fake-send sent 101) {:sleep-state-dir dir} "crashed" {:only-dead? true})
          (.then (fn [_]
                   (is (= ["gamerule players_sleeping_percentage 60"] @sent))
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-batch-end-or-pool-exit-of-another-run-leaves-the-state
  (async done
    (let [sent (atom []) dir (tmp-dir) opts {:sleep-state-dir dir :allow-time true :owner-pid js/process.pid}]
      (-> (start! sent 25 opts "n1" dir)
          (.then (fn [_]
                   (reset! sent [])
                   (r/end-night-batch! (fake-send sent 101) opts "n2" dir)))
          (.then (fn [_]
                   (r/restore-sleep-rule! (fake-send sent 101) opts "exit" {:pool 1})))
          (.then (fn [_]
                   (is (empty? @sent) "another batch / another pool restores nothing")
                   (is (= 25 (:old (r/read-sleep-state opts))))
                   (r/restore-sleep-rule! (fake-send sent 101) opts "exit" {:pool js/process.pid})))
          (.then (fn [_]
                   (is (= ["gamerule players_sleeping_percentage 25"] @sent) "the recording pool's own exit restores")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-state-of-a-dead-run-restores-under-only-dead
  (async done
    (let [sent (atom []) dir (tmp-dir)]
      (fs/writeFileSync (path/join dir "world-test-sleep-rule.edn") (pr-str {:old 60 :batch "n1" :pid 2147483000}))
      (-> (r/restore-sleep-rule! (fake-send sent 101) {:sleep-state-dir dir} "crashed" {:only-dead? true})
          (.then (fn [_]
                   (is (= ["gamerule players_sleeping_percentage 60"] @sent))
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest after-the-barrier-timeout-the-time-is-set-once-and-the-timeout-is-noted
  (async done
    (let [dir (tmp-dir) sets (atom []) notes (atom [])
          at (fn [pid] (r/barrier-wait! {:dir dir :batch "n1" :n 3 :pid pid :sleep r/sleep :timeout-ms 300 :note #(swap! notes conj %)}
                                        (fn [] (swap! sets conj pid) (r/sleep 100) (js/Promise.resolve true))))]
      (-> (js/Promise.all #js [(at 1) (at 2)])
          (.then (fn [_]
                   (is (= 1 (count @sets)) "one body set the time, not both")
                   (is (seq @notes) "the timeout is noted")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest the-barrier-opens-when-every-body-arrived-or-left
  (is (= [false true true]
         [(r/barrier-open? {1 :arrived 2 :arrived} 3)
          (r/barrier-open? {1 :arrived 2 :arrived 3 :arrived} 3)
          (r/barrier-open? {1 :arrived 2 :gone 3 :arrived} 3)])))

(deftest one-body-at-the-barrier-sets-the-time-once-for-all
  (async done
    (let [dir (tmp-dir)
          sets (atom [])
          at (fn [pid] (r/barrier-wait! {:dir dir :batch "n1" :n 2 :pid pid :sleep r/sleep :timeout-ms 5000}
                                        (fn [] (swap! sets conj pid) (js/Promise.resolve true))))]
      (-> (js/Promise.all #js [(at 1) (at 2)])
          (.then (fn [_]
                   (is (= 1 (count @sets)) "exactly one body set the time")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-body-that-never-arrives-does-not-hold-the-others-past-the-timeout
  (async done
    (let [dir (tmp-dir)
          sets (atom 0)]
      (-> (r/barrier-wait! {:dir dir :batch "n1" :n 2 :pid 1 :sleep r/sleep :timeout-ms -1}
                           (fn [] (swap! sets inc) (js/Promise.resolve true)))
          (.then (fn [_]
                   (is (= 1 @sets))
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-body-that-leaves-counts-for-the-barrier
  (async done
    (let [dir (tmp-dir)]
      (r/barrier-leave! dir "n1" 2)
      (-> (r/barrier-wait! {:dir dir :batch "n1" :n 2 :pid 1 :sleep r/sleep :timeout-ms 5000}
                           (fn [] (js/Promise.resolve true)))
          (.then (fn [ok] (is (true? ok)) (fs/rmSync dir #js {:recursive true}) (done)))))))

(deftest a-failed-night-batch-case-that-ran-into-dawn-is-inconclusive
  (let [c {:time :night-exclusive :act []}
        b {:night-batch "n1"}]
    (is (= [true false false false false]
           [(r/night-overran? b c 100)
            (r/night-overran? b c 15000)
            (r/night-overran? {} c 100)
            (r/night-overran? b {:time :night :act []} 100)
            (r/night-overran? b {:time :night-exclusive :act [[:time-set 1000]]} 100)]))
    (is (= [:inconclusive :pass]
           [(:status (r/mark-night-overran {:status :fail} true)) (:status (r/mark-night-overran {:status :pass} true))]))
    (is (= {:overran true :first-why "boom"}
           (select-keys (r/mark-night-overran {:status :fail :why "boom"} true) [:overran :first-why]))
        "the pool reruns an :overran result; the first failure reason is kept")))

(deftest trace-options-parse-and-only-failed-results-get-a-trace
  (let [o (r/parse-args #js ["--trace-dir" "/d/cases" "--trace-attempt" "2"])]
    (is (= ["/d/cases" 2] ((juxt :trace-dir :trace-attempt) o))))
  (is (= {:status :pass} (r/write-trace! {:trace-dir "/nonexistent-x/cases" :body "B"} {:status :pass} {:ino nil :pos 0})))
  (is (= {:status :fail} (r/write-trace! {:body "B"} {:status :fail} {:ino nil :pos 0}))))

(deftest a-failed-result-gets-its-trace-file
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "trace-run-"))
        out (r/write-trace! {:trace-dir dir :body "NoSuchBodyX" :world "claude" :trace-attempt 1 :retry 1}
                            {:id "f/c" :run 2 :status :fail} {:ino nil :pos 0})]
    (is (= (path/join dir "f__c__r2-a2.edn") (:trace out)))
    (is (fs/existsSync (:trace out)))
    (fs/rmSync dir #js {:recursive true :force true})))

(deftest a-stray-101-with-no-recorded-batch-goes-back-to-100-and-is-logged
  (async done
    (let [sent (atom []) dir (tmp-dir) log (path/join dir "time.log")
          opts {:allow-time true :sleep-state-dir dir :time-log log :body "B"}]
      (-> (r/reset-stray-sleep-rule! (fake-send sent 101) opts "pool start")
          (.then (fn [_]
                   (is (= ["gamerule players_sleeping_percentage" "gamerule players_sleeping_percentage 100"] @sent))
                   (is (re-find #"gamerule players_sleeping_percentage 100 by world-test B .*\(pool start\)" (fs/readFileSync log "utf8")))
                   (reset! sent [])
                   (r/reset-stray-sleep-rule! (fake-send sent 60) opts "pool start")))
          (.then (fn [_]
                   (is (= ["gamerule players_sleeping_percentage"] @sent) "any other value is left alone")
                   (reset! sent [])
                   (fs/writeFileSync (path/join dir "world-test-sleep-rule.edn") (pr-str {:old 60 :batch "n1" :pid js/process.pid}))
                   (r/reset-stray-sleep-rule! (fake-send sent 101) opts "case")))
          (.then (fn [_]
                   (is (empty? @sent) "a recorded batch owns its 101")
                   (r/reset-stray-sleep-rule! (fake-send sent 101) (dissoc opts :allow-time) "case")))
          (.then (fn [_]
                   (is (empty? @sent) "without --allow-time the rule is not touched")
                   (fs/rmSync dir #js {:recursive true})
                   (done)))))))

(deftest a-register-put-that-times-out-is-asked-again
  (let [calls (atom 0)
        run (fn [] (js/Promise.resolve (if (< (swap! calls inc) 3) {:code 1 :out "{:ok false :reason :timeout}"} {:code 0 :out "{:ok true}"})))]
    (async done
      (-> (r/put-until-answered! run 4 1)
          (.then (fn [answer] (is (= [0 3] [(:code answer) @calls]))))
          (.then (fn [_] (reset! calls 0) (r/put-until-answered! (fn [] (swap! calls inc) (js/Promise.resolve {:code 2 :out "{:ok false :reason :bad-args}"})) 4 1)))
          (.then (fn [answer] (is (= [2 1] [(:code answer) @calls]) "a refusal is not asked again")))
          (.then done)))))
