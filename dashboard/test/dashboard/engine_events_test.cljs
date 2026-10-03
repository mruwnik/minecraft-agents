(ns dashboard.engine-events-test
  (:require [cljs.test :refer [deftest is testing]]
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
    (is (= [{:t (+ t0 4000) :level "info" :source "body" :kind "chat" :text "hello"}
            {:t (+ t0 5000) :level "warn" :source "job" :kind "failed" :text "no path"}
            {:t (+ t0 6000) :level "warn" :source "job" :kind "dig_in_failed" :text "job.dig_in_failed shelter"}
            {:t (+ t0 7000) :level "info" :source "body" :kind "hurt" :text "body.hurt"}]
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
  (is (= [{:name "Broken" :username "Broken" :world nil}
          {:name "NoConfig" :username "NoConfig" :world nil}
          {:name "NoPort" :username "NoPort" :world "claude"}
          {:name "ProbeWater" :username "PW" :world "claude"}]
         (ee/parse-engine-agents
          [{:name "ProbeWater" :text "{\"username\":\"PW\",\"world\":\"claude\",\"apiPort\":3804}"}
           {:name "NoPort" :text "{\"world\":\"claude\"}"}
           {:name "NoConfig" :text ""}
           {:name "Broken" :text "{"}]))))

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

(deftest log-entries
  (doseq [[title e worthy?]
          [["a job completing" (ev 1 {:kind "completed"}) true]
           ["a heartbeat" (ev 1 {:kind "round_started"}) false]
           ["a yield" (ev 1 {:kind "yielded"}) false]
           ["memory saves" (ev 1 {:source "memory" :kind "saved" :level "debug"}) false]
           ["memory written note" (ev 1 {:kind "memory_written" :level "debug"}) false]
           ["view stats" (ev 1 {:source "body" :kind "view.stats"}) false]
           ["an action start (debug)" (ev 1 {:source "action" :kind "started" :level "debug"}) true]
           ["other debug noise" (ev 1 {:source "path" :kind "x" :level "debug"}) false]
           ["a warning" (ev 1 {:source "path" :kind "x" :level "warn"}) true]]]
    (is (= worthy? (ee/log-worthy? e)) title)))

(deftest log-entry-shape
  (is (= {:t 1 :seq 2 :level "info" :source "job" :kind "completed" :name "n" :text nil :error nil :args {:a 1} :reflex nil :ms nil}
         (ee/log-entry {:t 1 :seq 2 :level "info" :source "job" :kind "completed" :name "n" :args {:a 1} :inventory [1 2 3] :pos {:x 1}}))))

(deftest log-tail
  (let [events (mapv #(ev % (if (even? %) {:kind "yielded"} {:kind "completed"})) (range 1 11))]
    (is (= [7 9] (mapv :seq (ee/log-tail events 2))))
    (is (= [1 3 5 7 9] (mapv :seq (ee/log-tail events 99))))))
