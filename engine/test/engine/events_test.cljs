(ns engine.events-test
  (:require [cljs.test :refer [deftest is]]
            [clojure.string :as str]
            [engine.events :as events]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(defn lines [file]
  (->> (str/split-lines (fs/readFileSync file "utf8"))
       (remove str/blank?)
       (mapv #(js->clj (js/JSON.parse %) :keywordize-keys true))))

(deftest emit-fills-the-envelope
  (let [[seen sink] (tu/capture-sink)
        ev (events/make {:body "Fake" :sinks [sink] :now (constantly 5)
                         :pos-fn (constantly {:x 1.7 :y 64 :z -0.2})})]
    (is (= 1 (events/emit! ev {:source :job :kind :queued :level :info :job "j1" :name "go-to"})))
    (is (= 2 (events/emit! ev {:source :job :kind :cut :level :info :cause 1})))
    (is (= [{:seq 1 :t 5 :body "Fake" :source :job :kind :queued :level :info
             :job "j1" :pos {:x 1 :y 64 :z -1} :name "go-to"}]
           (take 1 @seen)))
    (is (= 1 (:cause (second @seen))))))

(deftest file-sink-writes-json-lines-and-seq-continues-across-restarts
  (let [file (path/join (tu/tmp-dir) "events.jsonl")
        first-run (events/make {:body "Fake" :file file})]
    (events/emit! first-run {:source :system :kind :started :level :info :text "hi"})
    (events/emit! first-run {:source :system :kind :started :level :info})
    (let [second-run (events/make {:body "Fake" :file file})]
      (is (= 3 (events/emit! second-run {:source :system :kind :restored :level :info})))
      (is (= [[1 "system" "started" "hi"] [2 "system" "started" nil] [3 "system" "restored" nil]]
             (mapv (juxt :seq :source :kind :text) (lines file)))))))

(deftest min-level-filters-stdout-only
  (let [[seen sink] (tu/capture-sink)
        ev (events/make {:body "Fake" :sinks [sink]})]
    (events/emit! ev {:source :action :kind :started :level :debug})
    (is (= 1 (count @seen)))
    (is (true? (events/at-least? :warn :error)))
    (is (false? (events/at-least? :info :debug)))))
