(ns agent-tools.job-results-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.job-results :as results]))

(defn event [n id kind data]
  {:seq n :generation-id "g" :source :job :kind kind
   :context {:job-id id :chain [id]} :data data})

(deftest retained-search-observations-survive-completion-and-exclude-unrelated-events
  (let [events [(event 1 "j4" :queued {:spec "search"})
                (assoc (event 2 "j4/c0" :search.done {:found [{:what "stone" :pos [5 70 3]}] :coverage {:scans 1}})
                       :context {:job-id "j4/c0" :chain ["j4" "j4/c0"]})
                (event 3 "j4" :memory_written {:memory :origin})
                (event 4 "j4" :completed {:status :completed})
                (event 5 "j5" :search.done {:found [{:what "diamond"}]})
                (assoc (event 6 "j4" :search.done {:found [{:what "wrong-generation"}]}) :generation-id "old")]
        result (results/project "j4" "g" events false)]
    (is (= :completed (:status result)))
    (is (= :complete (:history result)))
    (is (= [{:event :search.done :job "j4/c0" :data {:found [{:what "stone" :pos [5 70 3]}] :coverage {:scans 1}}}]
           (:events result)))))

(deftest the-terminal-events-data-is-the-jobs-result
  (let [events [(event 1 "j4" :queued {}) (event 2 "j4" :completed {:status :completed :scope :seen :blocks [{:name "iron_ore"}]})]]
    (is (= {:status :completed :scope :seen :blocks [{:name "iron_ore"}]} (:result (results/project "j4" "g" events false))))
    (is (not (contains? (results/project "j4" "g" [(event 1 "j4" :queued {})] false) :result)))))

(deftest history-and-output-loss-are-explicit
  (is (= :unknown-job (:reason (results/project "j999" "g" [] false))))
  (is (= :job-history-unavailable (:reason (results/project "j999" "g" [] true))))
  (is (= :job-history-unavailable (:reason (results/project "j3" "g" [] true 10))))
  (is (= :unknown-job (:reason (results/project "j10" "g" [] true 10))))
  (is (= :unknown-job (:reason (results/project "j12/c0" "g" [] true 10))))
  (is (= :unknown-job (:reason (results/project "bogus" "g" [] true 10))))
  (is (= :partial (:history (results/project "j4" "g" [(event 2 "j4" :completed {})] false))))
  (let [events (into [(event 1 "j4" :queued {})]
                     (map #(event % "j4" :search.done {:found (vec (repeat 80 {:what (apply str (repeat 1000 "s")) :pos [1 2 3]}))})
                          (range 2 20)))
        result (results/project "j4" "g" events true)]
    (is (= :partial (:history result)))
    (is (:events-truncated? result))
    (is (<= (count (:events result)) results/event-limit))
    (is (< (js/Buffer.byteLength (pr-str result)) 65536))))

(deftest a-job-without-a-terminal-event-is-reported-unfinished
  (let [result (results/project "j4" "g" [(event 1 "j4" :queued {:spec "mine"})] false)]
    (is (= :unfinished (:status result)))
    (is (false? (:finished? result)))
    (is (= :queued (:state result))))
  (let [result (results/project "j4" "g" [(event 1 "j4" :queued {}) (event 2 "j4" :round_started {})] false)]
    (is (= :running (:state result))))
  (is (true? (:finished? (results/project "j4" "g" [(event 1 "j4" :queued {}) (event 2 "j4" :completed {})] false)))))

(defn fake-get
  "A get! over a log of n filler events, with j1's events at seq 1-2 and a terminal event for j1 at seq 3."
  [n]
  (let [log (into [(event 1 "j1" :queued {}) (event 2 "j1" :round_started {}) (event 3 "j1" :completed {})]
                  (map #(event % "filler" :memory_written {})) (range 4 (inc n)))
        edn (fn [v] (js/Promise.resolve {:status 200 :content-type "application/edn" :text (pr-str v)}))]
    (fn [_ path _]
      (if (= "/snapshot" path)
        (edn {:generation-id "g" :cursor {:stream-id "s" :seq n}})
        (let [after (js/parseInt (second (re-find #"after=(\d+)" path)))
              size (js/parseInt (second (re-find #"limit=(\d+)" path)))]
          (edn {:gap? false :events (vec (take size (drop after log)))}))))))

(deftest a-job-older-than-the-default-window-is-still-found
  (async done
    (.then (results/read! (fake-get 3000) "sock" "j1" {})
           (fn [result] (is (= :completed (:status result))) (done)))))

(deftest a-job-beyond-every-window-is-unavailable-not-unknown
  (async done
    (.then (results/read! (fake-get 20000) "sock" "j1" {})
           (fn [result] (is (= :job-history-unavailable (:reason result))) (done)))))

(deftest every-terminal-event-ends-the-job
  (doseq [kind [:completed :failed :cancelled :stopped]]
    (let [result (results/project "j5" "g" [(event 1 "j5" :queued {}) (event 2 "j5" :round_started {}) (event 3 "j5" kind {})] false)]
      (is (= kind (:status result)) (str kind))
      (is (true? (:finished? result)) (str kind))
      (is (nil? (:state result)) (str kind)))))

(deftest a-recent-job-is-found-in-few-requests
  (let [n 8000
        log (into (mapv #(event % "filler" :memory_written {}) (range 1 (- n 2)))
                  [(event (- n 2) "j9" :queued {}) (event (- n 1) "j9" :round_started {}) (event n "j9" :completed {})])
        calls (atom 0)
        edn (fn [v] (js/Promise.resolve {:status 200 :content-type "application/edn" :text (pr-str v)}))
        get! (fn [_ path _]
               (swap! calls inc)
               (if (= "/snapshot" path)
                 (edn {:generation-id "g" :cursor {:stream-id "s" :seq n}})
                 (let [after (js/parseInt (second (re-find #"after=(\d+)" path)))
                       size (js/parseInt (second (re-find #"limit=(\d+)" path)))]
                   (edn {:gap? false :events (vec (take size (drop after log)))}))))]
    (async done
      (.then (results/read! get! "sock" "j9" {})
             (fn [result]
               (is (= :completed (:status result)))
               (is (<= @calls 6) (str @calls " requests"))
               (done))))))

(deftest a-stopped-job-reports-its-ending
  (let [r (results/project "j4" "g" [(event 1 "j4" :queued {})
                                     (event 2 "j4" :stopped {:kind :nothing-possible :cause {:ways {:a :x}} :reason :ways})]
                           false)]
    (is (= :stopped (:status r)))
    (is (= {:kind :nothing-possible :cause {:ways {:a :x}} :reason :ways} (:ending r)))))

(defn exclusive-stop
  "The events of an exclusive run j4 that stopped with data (and the text its stopped event carried)."
  [data text]
  [(assoc (event 1 "j4" :queued {}) :exclusive true)
   (assoc (event 2 "j4" :stopped data) :text text :report {:yields {:items {:bread 1}}})])

(deftest an-exclusive-run-that-did-not-finish-carries-a-note-per-ending
  (let [note (fn [data text] (:note (results/project "j4" "g" (exclusive-stop data text) false)))]
    (is (re-find #"Yielded after its one exclusive round.*Got done: bread 1" (note {:status :stopped :reason :yielded} "stopped: yielded")))
    (is (re-find #"Declined to run \(no tool\); nothing ran" (note {:status :stopped :reason :declined} "no tool")))
    (is (re-find #"Waited after its one exclusive round \(waiting: a table\)" (note {:status :stopped :reason :need} "waiting: a table")))
    (is (nil? (note {:status :stopped :reason :cut} "cut")))
    (is (nil? (:note (results/project "j4" "g" [(event 1 "j4" :queued {}) (event 2 "j4" :stopped {:reason :ways})] false))))))

(defn unknown-id-get
  "A get! over a log of n filler events whose snapshot says ids below j5 were issued; counts its requests in calls."
  [n calls]
  (let [log (mapv #(event % "filler" :memory_written {}) (range 1 (inc n)))
        edn (fn [v] (js/Promise.resolve {:status 200 :content-type "application/edn" :text (pr-str v)}))]
    (fn [_ path _]
      (swap! calls inc)
      (case path
        "/snapshot" (edn {:generation-id "g" :cursor {:stream-id "s" :seq n} :state {:next-id 5}})
        (let [after (js/parseInt (second (re-find #"after=(\d+)" path)))
              size (js/parseInt (second (re-find #"limit=(\d+)" path)))]
          (edn {:gap? false :events (vec (take size (drop after log)))}))))))

(deftest an-id-the-engine-never-issued-is-unknown-without-scanning-the-wide-window
  (let [calls (atom 0)]
    (async done
      (.then (results/read! (unknown-id-get 8000 calls) "sock" "j999" {})
             (fn [result]
               (is (= :unknown-job (:reason result)))
               (is (<= @calls 6) (str @calls " requests"))
               (done))))))

(deftest the-wide-window-is-read-in-full-when-every-page-is-slow
  (let [real-now (.-now js/Date)
        clock (atom 0)
        inner (unknown-id-get 8000 (atom 0))
        get! (fn [socket path opts] (swap! clock + 400) (inner socket path opts))]
    (set! (.-now js/Date) (fn [] @clock))
    (async done
      (-> (results/history! get! "sock" {:limit results/result-limit})
          (.then (fn [history] (count (:events history))) (fn [error] (.-code error)))
          (.then (fn [outcome] (is (= results/result-limit outcome))))
          (.finally (fn [] (set! (.-now js/Date) real-now) (done)))))))

(deftest a-recorded-end-with-clipped-result-says-so
  (let [[e] (results/with-recorded [] "g" {"j1" {:status :stopped :result {:reason :x} :result-clipped true}} ["j1"])]
    (is (= {:reason :x :result-clipped true} (:data e)))))
