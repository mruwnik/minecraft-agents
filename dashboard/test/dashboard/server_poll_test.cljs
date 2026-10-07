(ns dashboard.server-poll-test
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [dashboard.engine-edn :as engine-edn]
            [dashboard.goal :as goal]
            [dashboard.server.engine-state :as srv-engine-state]
            [dashboard.server.entities :as srv-entities]
            [dashboard.server.files :as srv-files]
            [dashboard.server.responses :as srv-responses]
            [dashboard.server.snapshot :as srv-snapshot]))

(def state-text "{:generation-id \"g\" :list [\"job-1\"] :instances {\"job-1\" {:id \"job-1\" :spec {:op :leaf :job jobs.a/b}}} :current \"job-1\" :failed {}}")
(def other-text "{:generation-id \"g\" :list [] :instances {} :current nil :failed {:x 1 :y 2}}")

(defn make-body! [root world name]
  (let [dir (.join path root "worlds" world "agents" name)
        engine (.join path dir "engine")]
    (.mkdirSync fs engine #js {:recursive true})
    (.writeFileSync fs (.join path dir "config.json") (str "{\"username\":\"" name "\"}"))
    (.writeFileSync fs (.join path engine "events.edn") "")
    (.writeFileSync fs (.join path engine "engine.edn") state-text)
    engine))

(defn touch! [file seconds-ahead]
  (let [t (+ (/ (js/Date.now) 1000) seconds-ahead)]
    (.utimesSync fs file t t)))

(defn with-root [n f]
  (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-poll-test-"))
        engines (mapv #(make-body! root "w" (str "B" %)) (range n))]
    (try
      (with-redefs [srv-files/state-dir root
                    srv-engine-state/live-engines (atom {})
                    srv-engine-state/live-errors (atom {})
                    srv-engine-state/edn-cache (atom {})
                    srv-engine-state/body-cache (atom {})
                    srv-engine-state/view-cache (atom {})
                    srv-engine-state/goal-cache (atom {})]
        (f root engines))
      (finally (.rmSync fs root #js {:recursive true :force true})))))

(defn counting [counter orig] (fn [& args] (swap! counter inc) (apply orig args)))

(deftest unchanged-offline-bodies-are-built-and-parsed-once
  (with-root 3
    (fn [_ _]
      (let [parses (atom 0) entries (srv-engine-state/agent-entries)]
        (with-redefs [engine-edn/read-edn (counting parses engine-edn/read-edn)]
          (let [first-poll (srv-engine-state/bodies-in 1000 entries)
                after-first @parses
                second-poll (srv-engine-state/bodies-in 2000 entries)]
            (is (= 3 after-first))
            (is (= after-first @parses) "second poll parses nothing")
            (is (= first-poll second-poll))))))))

(deftest changed-engine-edn-is-picked-up-for-that-body-only
  (with-root 3
    (fn [_ engines]
      (let [parses (atom 0) entries (srv-engine-state/agent-entries)]
        (with-redefs [engine-edn/read-edn (counting parses engine-edn/read-edn)]
          (let [before (srv-engine-state/bodies-in 1000 entries)
                file (.join path (second engines) "engine.edn")]
            (.writeFileSync fs file other-text)
            (touch! file 10)
            (let [after (srv-engine-state/bodies-in 2000 entries)]
              (is (= 4 @parses) "one body parsed again")
              (is (not= (nth before 1) (nth after 1)))
              (is (= (nth before 0) (nth after 0)))
              (is (= (nth before 2) (nth after 2))))))))))

(deftest changed-pose-and-new-minute-rebuild
  (with-root 1
    (fn [root engines]
      (let [entries (srv-engine-state/agent-entries)
            builds (atom 0)]
        (with-redefs [srv-engine-state/build-engine-body (counting builds srv-engine-state/build-engine-body)]
          (srv-engine-state/bodies-in 1000 entries)
          (srv-engine-state/bodies-in 2000 entries)
          (is (= 1 @builds))
          (srv-engine-state/bodies-in 70000 entries)
          (is (= 2 @builds) "a new minute")
          (.mkdirSync fs (.join path root "worlds" "w" "agents" "B0" "view") #js {:recursive true})
          (.writeFileSync fs (.join path root "worlds" "w" "agents" "B0" "view" "hud.json") "{}")
          (srv-engine-state/bodies-in 70500 entries)
          (is (= 3 @builds) "a new view file"))))))

(deftest a-body-carries-its-goal-and-a-new-goal-shows-on-the-next-poll
  (with-root 2
    (fn [root _]
      (let [entries (srv-engine-state/agent-entries)
            dir (.join path root "worlds" "w" "agents" "B0")]
        (is (every? nil? (map :goal (srv-engine-state/bodies-in 1000 entries))))
        (goal/write-goal! dir "fence the pen" "B0" 500)
        (is (= [{:text "fence the pen" :by "B0" :since 500} nil] (mapv :goal (srv-engine-state/bodies-in 2000 entries))))
        (goal/write-goal! dir "world-test a/b (run 1, retry 0)" "world-test" 600)
        (touch! (goal/goal-file dir) 10)
        (is (= "world-test a/b (run 1, retry 0)" (:text (:goal (first (srv-engine-state/bodies-in 3000 entries))))))
        (goal/clear-goal! dir)
        (is (nil? (:goal (first (srv-engine-state/bodies-in 4000 entries)))))))))

(deftest one-state-request-lists-the-bodies-once
  (async done
    (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-poll-test-"))
          _ (doseq [n (range 3)] (make-body! root "w" (str "B" n)))
          config-reads (atom 0) answer (atom nil)
          read-text srv-files/read-text
          res #js {:writeHead (fn [& _]) :end (fn [body] (reset! answer body))}]
      (with-redefs [srv-files/state-dir root
                    srv-engine-state/live-engines (atom {}) srv-engine-state/live-errors (atom {})
                    srv-engine-state/edn-cache (atom {}) srv-engine-state/body-cache (atom {}) srv-engine-state/view-cache (atom {})
                    srv-files/read-text (fn [file]
                                       (when (.endsWith file "config.json") (swap! config-reads inc))
                                       (read-text file))]
        (srv-snapshot/send-state! res "w")
        (js/setTimeout
         (fn []
           (is (some? @answer))
           (is (= 3 @config-reads) "each config.json read once")
           (.rmSync fs root #js {:recursive true :force true})
           (done))
         400)))))

(defn with-root-async
  "Like with-root; f returns a promise, and the root and the redefs stay until it settles."
  [n f done]
  (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-poll-test-"))
        engines (mapv #(make-body! root "w" (str "B" %)) (range n))
        live-engines (atom {}) live-errors (atom {})
        restore (juxt (constantly srv-files/state-dir) (constantly srv-engine-state/live-engines) (constantly srv-engine-state/live-errors)
                      (constantly srv-engine-state/edn-cache) (constantly srv-engine-state/body-cache) (constantly srv-engine-state/view-cache))
        [old-state old-live old-errors old-edn old-body old-view] (restore)]
    (set! srv-files/state-dir root)
    (set! srv-engine-state/live-engines live-engines)
    (set! srv-engine-state/live-errors live-errors)
    (set! srv-engine-state/edn-cache (atom {}))
    (set! srv-engine-state/body-cache (atom {}))
    (set! srv-engine-state/view-cache (atom {}))
    (-> (js/Promise.resolve) (.then #(f root engines))
        (.catch (fn [e] (is (nil? e) (str "async test failed: " e))))
        (.finally (fn []
                    (set! srv-files/state-dir old-state) (set! srv-engine-state/live-engines old-live) (set! srv-engine-state/live-errors old-errors)
                    (set! srv-engine-state/edn-cache old-edn) (set! srv-engine-state/body-cache old-body) (set! srv-engine-state/view-cache old-view)
                    (.rmSync fs root #js {:recursive true :force true})
                    (done))))))

(deftest bodies-without-an-events-socket-get-no-socket-request
  (async done
    (with-root-async 3
      (fn [_ engines]
        (let [requests (atom []) entries (srv-engine-state/agent-entries)]
          (.writeFileSync fs (.join path (second engines) "events.sock") "")
          (with-redefs [srv-engine-state/event-socket-request! (fn [body & _] (swap! requests conj (:name body)) (js/Promise.reject (js/Error. "down")))]
            (-> (srv-engine-state/refresh-live-engines-in! entries)
                (.then (fn [_]
                         (is (= ["B1"] @requests) "only the body with a socket file is asked")
                         (is (= #{"B0" "B1" "B2"} (set (map :name (keys @srv-engine-state/live-errors))))
                             "offline bodies are marked down without a request")
                         (is (every? false? (map :up (srv-engine-state/bodies-in 1000 entries))))))))))
      done)))

(deftest a-body-coming-online-is-asked-on-the-next-poll
  (async done
    (with-root-async 1
      (fn [_ engines]
        (let [requests (atom 0) entries (srv-engine-state/agent-entries) original srv-engine-state/event-socket-request!]
          (set! srv-engine-state/event-socket-request! (fn [& _] (swap! requests inc) (js/Promise.reject (js/Error. "down"))))
          (-> (srv-engine-state/refresh-live-engines-in! entries)
              (.then (fn [_]
                       (is (= 0 @requests))
                       (.writeFileSync fs (.join path (first engines) "events.sock") "")
                       (srv-engine-state/refresh-live-engines-in! entries)))
              (.then (fn [_] (is (= 1 @requests))))
              (.finally #(set! srv-engine-state/event-socket-request! original)))))
      done)))

(deftest bodies-without-a-control-socket-get-no-entity-request
  (with-root 2
    (fn [_ engines]
      (let [requests (atom []) entries (srv-engine-state/agent-entries)]
        (.writeFileSync fs (.join path (first engines) "control.sock") "")
        (with-redefs [srv-entities/entity-cache (atom {}) srv-entities/entity-in-flight (atom #{})
                      srv-entities/entity-request! (fn [body] (swap! requests conj (:name body)) (js/Promise.reject (js/Error. "down")))]
          (srv-entities/refresh-entities-in! "w" entries)
          (is (= ["B0"] @requests))
          (is (= :unavailable (get-in @srv-entities/entity-cache [{:world "w" :name "B1"} :status])))
          (is (< (count (get-in @srv-entities/entity-cache [{:world "w" :name "B1"} :error])) 60)))))))

(deftest one-poll-settles-every-body-without-a-control-socket
  (with-root 12
    (fn [_ engines]
      (let [requests (atom []) entries (srv-engine-state/agent-entries)]
        (.writeFileSync fs (.join path (nth engines 11) "control.sock") "")
        (with-redefs [srv-entities/entity-cache (atom {}) srv-entities/entity-in-flight (atom #{})
                      srv-entities/entity-request! (fn [body] (swap! requests conj (:name body)) (js/Promise.reject (js/Error. "down")))]
          (srv-entities/refresh-entities-in! "w" entries)
          (is (= ["B11"] @requests) "a body with a socket is asked even behind many without one")
          (is (= 12 (count @srv-entities/entity-cache)))
          (is (= 11 (count (filter #(= :unavailable (:status %)) (vals @srv-entities/entity-cache))))
              "none stays :loading after the first poll"))))))

(deftest world-entry-does-not-repeat-the-bodies
  (let [entry (srv-snapshot/world-entry [] {:name "w"})]
    (is (not (contains? entry :bodies)))))

(deftest state-text-matches-pr-str-and-prints-unchanged-bodies-once
  (let [prints (atom 0)
        body-a {:name "A" :engine {:jobs [{:id "j1"}]}}
        body-b {:name "B" :engine {:jobs []}}
        snap (fn [bodies] {:at 1 :bodies bodies :worlds [{:name "w" :places [{:name "p"}]}] :selected "w"})]
    (with-redefs [srv-responses/print-memo (atom {})
                  srv-responses/pr-body (counting prints pr-str)]
      (is (= (pr-str (snap [body-a body-b])) (srv-responses/state-text (snap [body-a body-b]))))
      (is (= 2 @prints))
      (is (= (pr-str (snap [body-a body-b])) (srv-responses/state-text (snap [body-a body-b]))))
      (is (= 2 @prints) "the same bodies are not printed again")
      (let [changed (assoc-in body-b [:engine :jobs] [{:id "j2"}])]
        (is (= (pr-str (snap [body-a changed])) (srv-responses/state-text (snap [body-a changed]))))
        (is (= 3 @prints) "only the changed body is printed again")))))

(deftest state-text-prints-the-static-world-part-and-equal-bodies-once
  (let [prints (atom 0)
        snap (fn [stamp] {:at 1 :bodies [{:name "A" :engine {:jobs [{:id "j1"}]}}]
                          :worlds [{:name "w" :places (mapv (fn [i] {:name (str "p" i)}) (range 5))
                                    :entities [{:observed-at stamp}] :entity-truncated? false}]
                          :selected "w"})
        same? (fn [snapshot] (= snapshot (reader/read-string (srv-responses/state-text snapshot))))]
    (with-redefs [srv-responses/print-memo (atom {})
                  srv-responses/pr-body (counting prints pr-str)
                  srv-responses/pr-part (counting prints pr-str)]
      (is (same? (snap 1)))
      (let [first-prints @prints]
        (is (same? (snap 2)))
        (is (= first-prints @prints) "freshly built equal bodies and static world parts are not printed again"))
      (let [before @prints
            changed (assoc-in (snap 3) [:worlds 0 :places 0 :name] "x")]
        (is (same? changed))
        (is (= (inc before) @prints) "only the changed static part is printed again")))))
