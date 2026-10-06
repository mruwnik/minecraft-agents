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
    (is (= [{:event :search.done :data {:found [{:what "stone" :pos [5 70 3]}] :coverage {:scans 1}}}]
           (:events result)))))

(deftest history-and-output-loss-are-explicit
  (is (= :unknown-job (:reason (results/project "j999" "g" [] false))))
  (is (= :job-history-unavailable (:reason (results/project "j999" "g" [] true))))
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
