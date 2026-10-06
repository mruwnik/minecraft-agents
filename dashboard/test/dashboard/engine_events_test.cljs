(ns dashboard.engine-events-test
  (:require [cljs.test :refer [deftest is testing are]]
            [dashboard.engine-events :as ee]))

(def t0 1000000000000)

(defn ev
  ([seq] (ev seq {}))
  ([seq extra]
   (merge {:seq seq :t (+ t0 (* seq 1000)) :body "B" :source "job" :kind "round_started" :level "info"
           :job "j1" :chain ["j1"] :pos {:x seq :y 64 :z (- seq)}}
          extra)))

(defn fold [events] (ee/fold-engine ee/empty-engine events))
(defn view [events now] (ee/engine-view (fold events) now))

(def enc (let [e (js/TextEncoder.)] (fn [s] (.encode e s))))
(def decode (let [d (js/TextDecoder.)] (fn [b] (.decode d b))))

(deftest thresholds
  (is (= 30000 ee/engine-up-ms))
  (is (= 600000 ee/warn-window-ms))
  (is (= 10 ee/recent-max)))

(deftest chunks-fold-the-same-as-one-batch
  (let [events [(ev 1 {:name "(repeat look)"}) (ev 2 {:kind "failed" :error "boom" :level "warn"}) (ev 3 {:name "dig"})]]
    (is (= (ee/fold-engine (ee/fold-engine ee/empty-engine (subvec events 0 1)) (subvec events 1))
           (ee/fold-engine ee/empty-engine events)))
    (is (= ee/empty-engine (ee/fold-engine ee/empty-engine [])))))

(deftest view-with-no-events-is-down
  (let [v (ee/engine-view ee/empty-engine t0)]
    (is (= false (:up v)))
    (is (nil? (:age-ms v)))
    (is (re-find #"no events" (:error v)))
    (is (= [] (:recent v)))))

(deftest liveness-by-age-of-last-event
  (let [events [(ev 1)] last-t (+ t0 1000)]
    (doseq [[age up] [[0 true] [(dec ee/engine-up-ms) true] [ee/engine-up-ms false] [300000 false]]]
      (testing (str "age " age)
        (is (= up (:up (view events (+ last-t age)))))))
    (is (= 3000 (:age-ms (view events (+ last-t 3000)))))
    (is (= (+ t0 1000) (:at (view events (+ last-t 3000)))))))

(deftest error-text-when-down
  (is (= "last event 98s ago" (:error (view [(ev 1)] (+ t0 99000)))))
  (is (nil? (:error (view [(ev 1)] (+ t0 2000))))))

(deftest pos-is-latest-event-pos
  (is (= {:x 3 :y 64 :z -3} (:pos (view [(ev 1) (ev 2) (ev 3)] (+ t0 4000))))))

(deftest current-job
  (doseq [[title events job]
          [["named by a top-level job event" [(ev 1 {:name "(repeat look)"})] {:id "j1" :name "(repeat look)"}]
           ["action events do not name the job"
            [(ev 1 {:name "(repeat look)"})
             (ev 2 {:source "action" :kind "started" :level "debug" :name "moveTo" :chain ["j1" "j1/c0"] :job "j1/c0"})]
            {:id "j1" :name "(repeat look)"}]
           ["child job events do not rename the top-level job"
            [(ev 1 {:name "outer"}) (ev 2 {:name "inner" :chain ["j1" "j1/c0"] :job "j1/c0"})]
            {:id "j1" :name "outer"}]
           ["a queued job is not current" [(ev 1 {:kind "queued" :name "pace"})] nil]
           ["stopped clears it" [(ev 1 {:name "a"}) (ev 2 {:kind "stopped" :name "a"})] nil]
           ["completed clears it" [(ev 1 {:name "a"}) (ev 2 {:kind "completed" :name "a"})] nil]
           ["failed clears it" [(ev 1 {:name "a"}) (ev 2 {:kind "failed" :name "a" :error "x" :level "warn"})] nil]
           ["cancelled clears it" [(ev 1 {:name "a"}) (ev 2 {:kind "cancelled" :name "a"})] nil]
           ["another job ending leaves it"
            [(ev 1 {:name "a"}) (ev 2 {:kind "completed" :name "b" :job "j2" :chain ["j2"]})]
            {:id "j1" :name "a"}]
           ["a child ending leaves it"
            [(ev 1 {:name "a"}) (ev 2 {:kind "completed" :name "c" :job "j1/c0" :chain ["j1" "j1/c0"]})]
            {:id "j1" :name "a"}]
           ["system.started clears it"
            [(ev 1 {:name "a"}) (ev 2 {:source "system" :kind "started" :job nil :chain nil})]
            nil]
           ["a hold shows on the job"
            [(ev 1 {:name "a"}) (ev 2 {:kind "holding" :name "a" :reason "waiting-for-boat" :since (+ t0 2000)})]
            {:id "j1" :name "a" :holding {:reason "waiting-for-boat" :since (+ t0 2000)}}]
           ["the next round ends the hold"
            [(ev 1 {:name "a"}) (ev 2 {:kind "holding" :name "a" :reason "x" :since t0}) (ev 3 {:name "a"})]
            {:id "j1" :name "a"}]
           ["a child's hold is not the job's"
            [(ev 1 {:name "a"}) (ev 2 {:kind "holding" :name "c" :reason "x" :since t0 :chain ["j1" "j1/c0"] :job "j1/c0"})]
            {:id "j1" :name "a"}]
           ["a new job replaces it"
            [(ev 1 {:name "a"}) (ev 2 {:name "b" :job "j2" :chain ["j2"]})]
            {:id "j2" :name "b"}]]]
    (testing title
      (is (= job (:job (view events (+ t0 9000))))))))

(defn reflex-ev [seq kind id extra]
  (ev seq (merge {:source "reflex" :kind kind :reflex id :job "j9" :chain nil} extra)))

(deftest active-reflex
  (doseq [[title events id]
          [["fired sets it" [(reflex-ev 1 "fired" "hungry" {})] "hungry"]
           ["ended clears the same reflex"
            [(reflex-ev 1 "fired" "hungry" {}) (reflex-ev 2 "ended" "hungry" {:how "cleared"})] nil]
           ["ended of another reflex leaves it"
            [(reflex-ev 1 "fired" "hungry" {}) (reflex-ev 2 "ended" "stuck" {:how "dropped"})] "hungry"]
           ["system.started clears it"
            [(reflex-ev 1 "fired" "hungry" {}) (ev 2 {:source "system" :kind "started" :job nil :chain nil})] nil]
           ["changed does not set it" [(reflex-ev 1 "changed" "hungry" {})] nil]]]
    (testing title
      (is (= id (:reflex (view events (+ t0 9000))))))))

(deftest recent-filters-and-shapes
  (let [events [(ev 1 {:level "debug" :source "action" :kind "started" :name "moveTo"})
                (ev 2 {:kind "yielded"})
                (ev 3 {:kind "round_started"})
                (ev 4 {:source "body" :kind "chat" :text "hello" :job nil})
                (ev 5 {:kind "failed" :level "warn" :error "no path" :name "dig"})
                (ev 6 {:source "job" :kind "dig_in_failed" :level "warn" :name "shelter"})
                (ev 7 {:source "body" :kind "hurt" :job nil})]]
    (is (= [{:t (+ t0 1000) :source "action" :kind "started" :attention "none" :text "action.started moveTo"}
            {:t (+ t0 4000) :source "body" :kind "chat" :attention "none" :text "hello"}
            {:t (+ t0 5000) :source "job" :kind "failed" :attention "none" :text "no path"}
            {:t (+ t0 7000) :source "body" :kind "hurt" :attention "none" :text "body.hurt"}]
           (:recent (view events (+ t0 8000)))))))

(deftest recent-keeps-newest-10
  (let [events (mapv #(ev (inc %) {:source "body" :kind "chat" :text (str "m" (inc %)) :job nil}) (range 25))]
    (is (= (mapv #(str "m" (+ 16 %)) (range 10))
           (mapv :text (:recent (view events (+ t0 30000))))))))

(deftest warn-and-error-counts-cover-ten-minutes
  (let [at (fn [seq level t] (ev seq {:level level :kind "x" :t t}))
        events [(at 1 "warn" t0) (at 2 "error" (+ t0 1000)) (at 3 "warn" (+ t0 700000))
                (at 4 "warn" (+ t0 710000)) (at 5 "error" (+ t0 720000)) (at 6 "info" (+ t0 730000))]
        v (view events (+ t0 735000))]
    (is (= 2 (:warn10m v)))
    (is (= 1 (:error10m v)))))

(deftest warnings-age-out-with-now
  (let [state (fold [(ev 1 {:level "warn" :kind "x"})])
        t (+ t0 1000)]
    (is (= 1 (:warn10m (ee/engine-view state (+ t ee/warn-window-ms -1)))))
    (is (= 0 (:warn10m (ee/engine-view state (+ t ee/warn-window-ms 1)))))))

(deftest fold-prunes-stored-warnings
  (is (= [] (:warns (fold [(ev 1 {:level "warn" :kind "x"}) (ev 2000 {:level "info"})])))))

(deftest complete-lines-cases
  (doseq [[title carry chunk text rest-text]
          [["whole lines" "" "a\nb\n" "a\nb\n" ""]
           ["partial tail kept" "" "a\nb" "a\n" "b"]
           ["carry completes" "b" "c\nd" "bc\n" "d"]
           ["no newline at all" "ab" "c" "" "abc"]
           ["multibyte" "" "xé" "" "xé"]]]
    (testing title
      (let [r (ee/complete-lines (enc carry) (enc chunk))]
        (is (= text (decode (:complete r))))
        (is (= rest-text (decode (:rest r))))))))

(deftest multibyte-cut-between-reads-is-whole-after-join
  (let [bytes (enc "{\"t\":\"é\"}\n")
        first-read (ee/complete-lines (js/Uint8Array. 0) (.subarray bytes 0 7))
        second-read (ee/complete-lines (:rest first-read) (.subarray bytes 7))]
    (is (= "{\"t\":\"é\"}\n" (decode (:complete second-read))))))

(deftest drop-torn-head-removes-through-first-newline
  (is (= "{\"a\":1}\n" (decode (ee/drop-torn-head (enc "rn\"}\n{\"a\":1}\n")))))
  (is (= "" (decode (ee/drop-torn-head (enc "no newline"))))))

(deftest parse-event-lines-skips-garbage
  (is (= [{:seq 1} {:seq 2}] (ee/parse-event-lines "{\"seq\":1}\n\nnot json\n{\"seq\":2}\n")))
  (is (= [] (ee/parse-event-lines "42\n[1]\n\"s\"\n")))
  (is (= [{:seq 1 :chain ["j1"] :pos {:x 1}}] (ee/parse-event-lines "{\"seq\":1,\"chain\":[\"j1\"],\"pos\":{\"x\":1}}\n"))))

(deftest decode-bytes-roundtrip
  (is (= "héllo" (ee/decode-bytes (enc "héllo")))))

(deftest parse-engine-agents-tolerates-bad-config
  (is (= [{:name "Broken" :username "Broken" :world "claude"}
          {:name "NoConfig" :username "NoConfig" :world "claude"}
          {:name "NoPort" :username "NoPort" :world "claude"}
          {:name "ProbeWater" :username "PW" :world "claude"}]
         (ee/parse-engine-agents
          [{:name "ProbeWater" :world "claude" :text "{\"username\":\"PW\",\"apiPort\":3804}"}
           {:name "NoPort" :world "claude" :text "{}"}
           {:name "NoConfig" :world "claude" :text ""}
           {:name "Broken" :world "claude" :text "{"}]))))

(deftest parse-engine-agents-takes-the-world-from-the-folder-not-the-config
  (is (= [{:name "Bob" :username "Bob" :world "a"}
          {:name "Bob" :username "Bob" :world "b"}]
         (ee/parse-engine-agents
          [{:name "Bob" :world "b" :text "{\"world\":\"elsewhere\"}"}
           {:name "Bob" :world "a" :text "{}"}]))))

(deftest engine-body-shape
  (let [agent {:name "P" :username "P" :world "claude"}
        up (ee/engine-body agent (view [(ev 1 {:name "a"})] (+ t0 2000)))
        down (ee/engine-body agent (view [(ev 1)] (+ t0 99000)))
        none (ee/engine-body agent (ee/engine-view ee/empty-engine t0))]
    (is (= {:name "P" :username "P" :world "claude" :up true :error nil :at (+ t0 1000) :state {:pos {:x 1 :y 64 :z -1}}}
           (dissoc up :engine)))
    (is (= "a" (get-in up [:engine :job :name])))
    (is (= [false "last event 98s ago"] [(:up down) (:error down)]))
    (is (= [false nil nil] [(:up none) (:state none) (:at none)]))))

(deftest unsupported-body-is-down
  (is (= {:name "Old" :username "O" :world "claude" :up false :error "not an engine body (unsupported)" :at nil :state nil :engine nil}
         (ee/unsupported-body {:name "Old" :username "O" :world "claude"}))))

;; ---------------------------------------------------------------- signals (what the trouble rules read)
(defn signals [events] (:signals (view events (+ t0 99000))))

(deftest signals-hurt-and-died
  (doseq [[title events path expected]
          [["hurt keeps the time of the last hurt, even at debug level"
            [(ev 1 {:source "body" :kind "hurt" :level "debug" :health 15}) (ev 2 {:source "body" :kind "hurt" :level "debug"})]
            [:hurt-t] (+ t0 2000)]
           ["died keeps its time" [(ev 1 {:source "body" :kind "died" :level "error"})] [:died-t] (+ t0 1000)]
           ["a hurt from another source is not a hurt" [(ev 1 {:source "job" :kind "hurt"})] [:hurt-t] nil]]]
    (testing title
      (is (= expected (get-in (signals events) path))))))

(deftest signals-backoff
  (doseq [[title events expected]
          [["a job backoff starts" [(ev 1 {:kind "backoff" :level "warn" :name "go-to"})] {"go-to" (+ t0 1000)}]
           ["a reflex backoff is keyed by the reflex" [(reflex-ev 1 "backoff" "hungry" {:level "warn"})] {"hungry" (+ t0 1000)}]
           ["recovered ends it"
            [(ev 1 {:kind "backoff" :level "warn" :name "go-to"}) (ev 2 {:kind "recovered" :name "go-to"})] {}]
           ["recovered of another leaves it"
            [(ev 1 {:kind "backoff" :level "warn" :name "go-to"}) (ev 2 {:kind "recovered" :name "dig"})] {"go-to" (+ t0 1000)}]
           ["a restart clears it"
            [(ev 1 {:kind "backoff" :level "warn" :name "go-to"}) (ev 2 {:source "system" :kind "started" :job nil :chain nil})] {}]]]
    (testing title
      (is (= expected (into {} (:backoffs (signals events))))))))

(deftest signals-stuck
  (doseq [[title events expected]
          [["the stuck reflex firing" [(reflex-ev 1 "fired" "stuck" {})] {:stuck-open? true :stuck-t (+ t0 1000)}]
           ["ended closes it" [(reflex-ev 1 "fired" "stuck" {}) (reflex-ev 2 "ended" "stuck" {})] {:stuck-open? false :stuck-t (+ t0 1000)}]
           ["unstick giving up" [(ev 1 {:kind "unstick.failed" :level "warn"})] {:stuck-t (+ t0 1000)}]
           ["another reflex is not stuck" [(reflex-ev 1 "fired" "hungry" {})] {}]]]
    (testing title
      (is (= expected (select-keys (signals events) [:stuck-open? :stuck-t]))))))

(deftest signals-takeover
  (doseq [[title events expected]
          [["started" [(ev 1 {:source "system" :kind "takeover_started"})] true]
           ["started then ended" [(ev 1 {:source "system" :kind "takeover_started"}) (ev 2 {:source "system" :kind "takeover_ended"})] false]
           ["ended then started again"
            [(ev 1 {:source "system" :kind "takeover_started"}) (ev 2 {:source "system" :kind "takeover_ended"}) (ev 3 {:source "system" :kind "takeover_started"})] true]
           ["never" [(ev 1)] false]]]
    (testing title
      (is (= expected (boolean (:takeover? (signals events))))))))

(deftest signals-takeover-who-and-since
  (doseq [[title events expected]
          [["started" [(ev 1 {:source "system" :kind "takeover_started" :who "operator"})] {:takeover? true :takeover-who "operator" :takeover-t (+ t0 1000)}]
           ["ended clears who and since" [(ev 1 {:source "system" :kind "takeover_started" :who "operator"}) (ev 2 {:source "system" :kind "takeover_ended"})] {:takeover? false}]
           ["a new holder replaces" [(ev 1 {:source "system" :kind "takeover_started" :who "a"}) (ev 3 {:source "system" :kind "takeover_started" :who "b"})]
            {:takeover? true :takeover-who "b" :takeover-t (+ t0 3000)}]]]
    (testing title
      (is (= expected (select-keys (signals events) [:takeover? :takeover-who :takeover-t]))))))

(deftest log-entries
  (doseq [[title e worthy?]
          [["a job completing" (ev 1 {:kind "completed"}) true]
           ["a job stopping" (ev 1 {:kind "stopped"}) true]
           ["a heartbeat" (ev 1 {:kind "round_started"}) false]
           ["a yield" (ev 1 {:kind "yielded"}) false]
           ["memory saves" (ev 1 {:source "memory" :kind "saved" :level "debug"}) false]
           ["memory written note" (ev 1 {:kind "memory_written" :level "debug"}) false]
           ["view stats" (ev 1 {:source "body" :kind "view.stats"}) false]
           ["an action start (debug)" (ev 1 {:source "action" :kind "started" :level "debug"}) true]
           ["other debug noise" (ev 1 {:source "path" :kind "x" :level "debug"}) false]
           ["a warning level does not determine attention" (ev 1 {:source "path" :kind "x" :level "warn"}) false]]]
    (is (= worthy? (ee/log-worthy? e)) title)))

(deftest log-entry-shape
  (is (= {:generation-id "legacy" :time-ms 1 :seq 2 :source :job :kind :completed :attention :none
          :context {} :message nil :data {:name "n" :args {:a 1} :pos {:x 1}}}
         (ee/log-entry {:t 1 :seq 2 :level "info" :source "job" :kind "completed" :name "n" :args {:a 1} :inventory [1 2 3] :pos {:x 1}}))))

(deftest log-tail
  (let [events (mapv #(ev % (if (even? %) {:kind "yielded"} {:kind "completed"})) (range 1 11))]
    (is (= [7 9] (mapv :seq (ee/log-tail events 2))))
    (is (= [1 3 5 7 9] (mapv :seq (ee/log-tail events 99))))))

(defn jsonl [events] (apply str (map #(str (js/JSON.stringify (clj->js %)) "\n") events)))

(deftest fold-text-equals-fold-of-parsed-events
  (let [noise {:inventory [{:name "dirt" :count 3}] :args {:to "x"}}
        events [(ev 1 {:name "(repeat look)"})
                (ev 2 (merge noise {:source "body" :kind "hurt" :level "debug"}))
                (ev 3 {:kind "failed" :error "boom" :level "warn"})
                (ev 4 {:source "reflex" :kind "fired" :reflex "stuck" :level "info" :pos nil})
                (ev 5 {:source "job" :kind "backoff" :name "dig" :level "debug"})
                (ev 6 {:source "job" :kind "failed" :error {:code 3} :level "error"})]
        text (str (jsonl events) "garbage\n\n")]
    (is (= (fold events) (ee/fold-text ee/empty-engine text)))
    (is (= (fold events) (ee/fold-text (ee/fold-text ee/empty-engine (jsonl (take 2 events))) (jsonl (drop 2 events)))))))

(deftest split-chunk-cases
  (doseq [[title state chunk text rest-text skipping?]
          [["whole lines" {} "a\nb\n" "a\nb\n" "" false]
           ["torn head dropped" {:skipping? true} "rn\"}\nb\nc" "b\n" "c" false]
           ["torn head spans chunks" {:skipping? true} "no newline" "" "" true]
           ["carry joins" {:rest (enc "b")} "c\nd" "bc\n" "d" false]]]
    (testing title
      (let [r (ee/split-chunk (update state :rest #(or % (js/Uint8Array. 0))) (enc chunk))]
        (is (= text (decode (:complete r))))
        (is (= rest-text (decode (:rest r))))
        (is (= skipping? (:skipping? r)))))))

(deftest parse-event-lines-with-line-filter
  (is (= [{:seq 2 :kind "chat"}]
         (ee/parse-event-lines "{\"seq\":1,\"kind\":\"x\"}\n{\"seq\":2,\"kind\":\"chat\"}\n" #(.includes % "chat")))))

;; ---------------------------------------------------------------- offline as soon as the last lifecycle event says so
(def spawned (fn [n] (ev n {:source "body" :kind "spawned"})))
(def online-ev (fn [n] (ev n {:source "body" :kind "online"})))
(def stopping (fn [n] (ev n {:source "system" :kind "stopping"})))
(def disconnected (fn [n] (ev n {:source "body" :kind "disconnected" :reason "disconnect.quitting"})))
(def kicked (fn [n] (ev n {:source "body" :kind "kicked" :level "error"})))
(def reconnect-failed (fn [n] (ev n {:source "body" :kind "reconnect-failed" :level "error"})))
(def started (fn [n] (ev n {:source "system" :kind "started" :job nil :chain nil})))

(deftest offline-at-once-after-a-stop-or-crash
  (doseq [[title events up]
          [["running" [(spawned 1) (ev 2) (ev 3)] true]
           ["SIGTERM: stopping then disconnected, 3 s old" [(spawned 1) (ev 2) (stopping 3) (disconnected 4)] false]
           ["stopping alone" [(spawned 1) (stopping 2)] false]
           ["a crash: disconnected only" [(spawned 1) (ev 2) (disconnected 3)] false]
           ["kicked" [(spawned 1) (kicked 2)] false]
           ["reconnected: spawned after the disconnect" [(spawned 1) (disconnected 2) (spawned 3)] true]
           ["reconnected: online after the disconnect" [(spawned 1) (disconnected 2) (online-ev 3)] true]
           ["reconnect failed" [(spawned 1) (disconnected 2) (reconnect-failed 3)] false]
           ["engine restarted after a stop" [(spawned 1) (stopping 2) (disconnected 3) (started 4)] true]
           ["events after a disconnect do not revive it" [(spawned 1) (disconnected 2) (ev 3)] false]]]
    (testing title
      (let [v (view events (+ t0 (* 1000 (inc (count events)))))]
        (is (= up (:up v)))))))

(deftest offline-error-names-the-lifecycle-event
  (is (= "disconnected" (:error (view [(spawned 1) (disconnected 2)] (+ t0 3000)))))
  (is (nil? (:error (view [(spawned 1) (disconnected 2) (spawned 3)] (+ t0 4000))))))

(deftest signals-online-time
  (are [events expected] (= expected (:online-t (signals events)))
    [(spawned 1) (ev 2)] (+ t0 1000)
    [(spawned 1) (disconnected 2) (online-ev 3)] (+ t0 3000)
    [(spawned 1) (started 2)] (+ t0 2000)
    [(ev 1)] nil))

(deftest pose-offline-counts-only-when-newer-than-the-last-connect
  (let [up-view (view [(spawned 1) (ev 2)] (+ t0 3000))
        online-t (+ t0 1000)]
    (are [pose-view expected] (= expected (:up (ee/with-view-status up-view pose-view)))
      {:status "offline" :poseMtimeMs (+ online-t 500)} false
      {:status "offline" :poseMtimeMs (- online-t 500)} true
      {:status "online" :poseMtimeMs (+ online-t 500)} true
      nil true)
    (is (= "view offline" (:error (ee/with-view-status up-view {:status "offline" :poseMtimeMs (+ online-t 500)}))))
    (is (= false (:up (ee/with-view-status (view [(ev 1)] (+ t0 99000)) {:status "online" :poseMtimeMs t0}))))))

(def base-view {:up true :signals {:hp 20} :pos {:x 0}})
(def base-ctx {:position {:x 1} :cursor 7 :generation-id "g1" :outstanding {:a 1}
               :scheduler-summary {:jobs ["a"]} :offline? false :snap-present? true :settling? false})

(deftest body-view-merges-scheduler-summary
  (let [v (ee/body-view base-view base-ctx)]
    (is (some? v))
    (is (= {:up true :signals {:hp 20} :pos {:x 1} :cursor 7 :generation-id "g1"
            :outstanding {:a 1} :jobs ["a"]}
           v))))

(deftest body-view-offline-error
  (are [snap? err] (= {:up false :error err}
                      (select-keys (ee/body-view base-view (assoc base-ctx :offline? true :snap-present? snap?))
                                   [:up :error]))
    true "disconnected"
    false "event service unavailable"))

(deftest body-view-settling
  (are [settling? expected] (= expected
                               (:settling (ee/body-view base-view (assoc base-ctx :settling? settling?))))
    true true
    false nil)
  (is (not (contains? (ee/body-view base-view base-ctx) :settling))))

(defn error-with-code [message code]
  (doto (js/Error. message) (aset "code" code)))

(deftest socket-failure-text-names-the-cause-without-the-path
  (are [e expected] (= expected (ee/socket-failure-text e))
    (error-with-code "connect ENOENT /home/x/state/worlds/w/agents/B/engine/events.sock" "ENOENT")
    "engine event service unavailable: ENOENT"
    (error-with-code "connect ECONNREFUSED /a/b.sock" "ECONNREFUSED")
    "engine event service unavailable: ECONNREFUSED"
    (js/Error. "engine event API timed out") "engine event service unavailable: engine event API timed out"
    (js/Error. "engine event API HTTP 500") "engine event service unavailable: engine event API HTTP 500"))

;; BaseMiner 2026-10-05: a non-fresh restart wrote system.stopping then system.restored and nothing else (events.edn seq 7567-7568)
(def restart-events
  [{:seq 7567 :time-ms 1791151607738 :source :system :kind :stopping :data {:pos {:x 12.4 :y 63 :z 11.5}}}
   {:seq 7568 :time-ms 1791151610849 :source :system :kind :restored
    :data {:list [] :register [:stuck :died] :pos {:x 12.4 :y 63 :z 11.5}} :run-id "c4a9f57c"}])

(deftest restored-after-stopping-is-up
  (is (:up (view restart-events (+ 1791151610849 1000))))
  (is (not (:offline? (:signals (view restart-events (+ 1791151610849 1000))))) "offline signal cleared"))

(deftest stopping-alone-is-offline
  (is (= "disconnected" (:error (view (take 1 restart-events) (+ 1791151607738 1000))))))

(deftest restored-resets-job-and-reflex
  (let [events [(ev 1 {:kind "started" :name "mine"})
                (reflex-ev 2 "fired" "hungry" {})
                (ev 3 {:source "system" :kind "stopping" :job nil :chain nil})
                (ev 4 {:source "system" :kind "restored" :job nil :chain nil})]
        v (view events (+ t0 5000))]
    (is (nil? (:job v)))
    (is (nil? (:reflex v)))))

(deftest a-legacy-line-without-a-kind-folds-and-is-kept
  (let [text "{\"source\":\"job\",\"t\":1000}\n{\"source\":\"job\",\"kind\":\"round_started\",\"t\":2000}\n"
        state (ee/fold-text ee/empty-engine text)]
    (is (= 2000 (get-in state [:last :t])))
    (is (false? (boolean (ee/offline-event? {:source "job"}))))
    (is (boolean? (ee/log-worthy? {:source "job"})))
    (is (map? (ee/log-entry {:source "job" :t 1})))))
