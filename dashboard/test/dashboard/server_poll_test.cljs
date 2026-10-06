(ns dashboard.server-poll-test
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [dashboard.engine-edn :as engine-edn]
            [dashboard.server :as server]))

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
      (with-redefs [server/state-dir root
                    server/live-engines (atom {})
                    server/live-errors (atom {})
                    server/edn-cache (atom {})
                    server/body-cache (atom {})
                    server/view-cache (atom {})]
        (f root engines))
      (finally (.rmSync fs root #js {:recursive true :force true})))))

(defn counting [counter orig] (fn [& args] (swap! counter inc) (apply orig args)))

(deftest unchanged-offline-bodies-are-built-and-parsed-once
  (with-root 3
    (fn [_ _]
      (let [parses (atom 0) entries (server/agent-entries)]
        (with-redefs [engine-edn/read-edn (counting parses engine-edn/read-edn)]
          (let [first-poll (server/bodies-in 1000 entries)
                after-first @parses
                second-poll (server/bodies-in 2000 entries)]
            (is (= 3 after-first))
            (is (= after-first @parses) "second poll parses nothing")
            (is (= first-poll second-poll))))))))

(deftest changed-engine-edn-is-picked-up-for-that-body-only
  (with-root 3
    (fn [_ engines]
      (let [parses (atom 0) entries (server/agent-entries)]
        (with-redefs [engine-edn/read-edn (counting parses engine-edn/read-edn)]
          (let [before (server/bodies-in 1000 entries)
                file (.join path (second engines) "engine.edn")]
            (.writeFileSync fs file other-text)
            (touch! file 10)
            (let [after (server/bodies-in 2000 entries)]
              (is (= 4 @parses) "one body parsed again")
              (is (not= (nth before 1) (nth after 1)))
              (is (= (nth before 0) (nth after 0)))
              (is (= (nth before 2) (nth after 2))))))))))

(deftest changed-pose-and-new-minute-rebuild
  (with-root 1
    (fn [root engines]
      (let [entries (server/agent-entries)
            builds (atom 0)]
        (with-redefs [server/build-engine-body (counting builds server/build-engine-body)]
          (server/bodies-in 1000 entries)
          (server/bodies-in 2000 entries)
          (is (= 1 @builds))
          (server/bodies-in 70000 entries)
          (is (= 2 @builds) "a new minute")
          (.mkdirSync fs (.join path root "worlds" "w" "agents" "B0" "view") #js {:recursive true})
          (.writeFileSync fs (.join path root "worlds" "w" "agents" "B0" "view" "hud.json") "{}")
          (server/bodies-in 70500 entries)
          (is (= 3 @builds) "a new view file"))))))

(deftest one-state-request-lists-the-bodies-once
  (async done
    (let [root (.mkdtempSync fs (.join path (os/tmpdir) "dashboard-poll-test-"))
          _ (doseq [n (range 3)] (make-body! root "w" (str "B" n)))
          config-reads (atom 0) answer (atom nil)
          read-text server/read-text
          res #js {:writeHead (fn [& _]) :end (fn [body] (reset! answer body))}]
      (with-redefs [server/state-dir root
                    server/live-engines (atom {}) server/live-errors (atom {})
                    server/edn-cache (atom {}) server/body-cache (atom {}) server/view-cache (atom {})
                    server/read-text (fn [file]
                                       (when (.endsWith file "config.json") (swap! config-reads inc))
                                       (read-text file))]
        (server/send-state! res "w")
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
        restore (juxt (constantly server/state-dir) (constantly server/live-engines) (constantly server/live-errors)
                      (constantly server/edn-cache) (constantly server/body-cache) (constantly server/view-cache))
        [old-state old-live old-errors old-edn old-body old-view] (restore)]
    (set! server/state-dir root)
    (set! server/live-engines live-engines)
    (set! server/live-errors live-errors)
    (set! server/edn-cache (atom {}))
    (set! server/body-cache (atom {}))
    (set! server/view-cache (atom {}))
    (-> (js/Promise.resolve) (.then #(f root engines))
        (.catch (fn [e] (is (nil? e) (str "async test failed: " e))))
        (.finally (fn []
                    (set! server/state-dir old-state) (set! server/live-engines old-live) (set! server/live-errors old-errors)
                    (set! server/edn-cache old-edn) (set! server/body-cache old-body) (set! server/view-cache old-view)
                    (.rmSync fs root #js {:recursive true :force true})
                    (done))))))

(deftest bodies-without-an-events-socket-get-no-socket-request
  (async done
    (with-root-async 3
      (fn [_ engines]
        (let [requests (atom []) entries (server/agent-entries)]
          (.writeFileSync fs (.join path (second engines) "events.sock") "")
          (with-redefs [server/event-socket-request! (fn [body & _] (swap! requests conj (:name body)) (js/Promise.reject (js/Error. "down")))]
            (-> (server/refresh-live-engines-in! entries)
                (.then (fn [_]
                         (is (= ["B1"] @requests) "only the body with a socket file is asked")
                         (is (= #{"B0" "B1" "B2"} (set (map :name (keys @server/live-errors))))
                             "offline bodies are marked down without a request")
                         (is (every? false? (map :up (server/bodies-in 1000 entries))))))))))
      done)))

(deftest a-body-coming-online-is-asked-on-the-next-poll
  (async done
    (with-root-async 1
      (fn [_ engines]
        (let [requests (atom 0) entries (server/agent-entries) original server/event-socket-request!]
          (set! server/event-socket-request! (fn [& _] (swap! requests inc) (js/Promise.reject (js/Error. "down"))))
          (-> (server/refresh-live-engines-in! entries)
              (.then (fn [_]
                       (is (= 0 @requests))
                       (.writeFileSync fs (.join path (first engines) "events.sock") "")
                       (server/refresh-live-engines-in! entries)))
              (.then (fn [_] (is (= 1 @requests))))
              (.finally #(set! server/event-socket-request! original)))))
      done)))

(deftest bodies-without-a-control-socket-get-no-entity-request
  (with-root 2
    (fn [_ engines]
      (let [requests (atom []) entries (server/agent-entries)]
        (.writeFileSync fs (.join path (first engines) "control.sock") "")
        (with-redefs [server/entity-cache (atom {}) server/entity-in-flight (atom #{})
                      server/entity-request! (fn [body] (swap! requests conj (:name body)) (js/Promise.reject (js/Error. "down")))]
          (server/refresh-entities-in! "w" entries)
          (is (= ["B0"] @requests))
          (is (= :unavailable (get-in @server/entity-cache [{:world "w" :name "B1"} :status])))
          (is (< (count (get-in @server/entity-cache [{:world "w" :name "B1"} :error])) 60)))))))

(deftest world-entry-does-not-repeat-the-bodies
  (let [entry (server/world-entry [] {:name "w"})]
    (is (not (contains? entry :bodies)))))

(deftest state-text-matches-pr-str-and-prints-unchanged-bodies-once
  (let [prints (atom 0)
        body-a {:name "A" :engine {:jobs [{:id "j1"}]}}
        body-b {:name "B" :engine {:jobs []}}
        snap (fn [bodies] {:at 1 :bodies bodies :worlds [{:name "w" :places [{:name "p"}]}] :selected "w"})]
    (with-redefs [server/body-texts (js/WeakMap.)
                  server/pr-body (counting prints pr-str)]
      (is (= (pr-str (snap [body-a body-b])) (server/state-text (snap [body-a body-b]))))
      (is (= 2 @prints))
      (is (= (pr-str (snap [body-a body-b])) (server/state-text (snap [body-a body-b]))))
      (is (= 2 @prints) "the same bodies are not printed again")
      (let [changed (assoc-in body-b [:engine :jobs] [{:id "j2"}])]
        (is (= (pr-str (snap [body-a changed])) (server/state-text (snap [body-a changed]))))
        (is (= 3 @prints) "only the changed body is printed again")))))
