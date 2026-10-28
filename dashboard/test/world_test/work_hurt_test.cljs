(ns world-test.work-hurt-test
  (:require [cljs.test :refer [deftest is are]]
            [clojure.string :as str]
            [world-test.work-hurt :as w]))

(defn line
  ([t kind] (line t kind nil {}))
  ([t kind job data] (pr-str (cond-> {:time-ms t :generation-id "g" :source (if (= :hurt kind) :body :job) :kind kind :data data}
                               job (assoc :context {:job-id job})))))

(def queued-mine (line 0 :queued "j1" {:name "jobs.gather.mine"}))

(defn counts [lines] (w/report [(w/tally (keep w/scan-line lines))]))

(deftest a-slot-is-named-by-its-root-or-the-obtain-that-gathers-with-it-and-walks-and-needs-are-not-work
  (let [names {"j1" "jobs.gather.mine" "j2" "jobs.items.obtain" "j2/gather-raw_iron" "jobs.gather.mine"}]
    (are [path want] (= want (w/category path names))
      "j1/dig" [:long "j1"]
      "j1/dig/stair" [:work "j1/dig/stair"]
      "j1/walk" nil
      "j1/dig/collect/search" nil
      "j2/gather-raw_iron/dig" [:long "j2/gather-raw_iron"]
      "j2/need/dig" nil
      "j3/dig" nil))
  (are [path want] (= want (w/outermost path {"j1" "jobs.gather.mine"}))
    "j1/dig/stair" [:long "j1"]
    "j1/walk" [:long "j1"]
    "j4/stair/dig" [:work "j4/stair"]))

(deftest a-hit-while-the-work-runs-counts-and-one-in-a-walk-a-reflex-a-hold-or-a-long-gap-does-not-and-the-walk-is-in-the-jobs-stay
  (are [lines want] (= want (select-keys (:long (counts lines)) [:s :hp :hits :lethal :stays]))
    [queued-mine (line 1000 :planned "j1/dig" {}) (line 6000 :hurt nil {:amount 4 :health 16}) (line 11000 :planned "j1/dig" {})]
    {:s 10 :hp 4 :hits 1 :lethal 0 :stays 1}
    [queued-mine (line 1000 :planned "j1/dig" {}) (line 6000 :hurt nil {:amount 3 :health 0}) (line 11000 :planned "j1/dig" {})]
    {:s 10 :hp 3 :hits 1 :lethal 1 :stays 1}
    [queued-mine (line 1000 :planned "j1/walk" {}) (line 6000 :hurt nil {:amount 4 :health 16}) (line 11000 :planned "j1/dig" {})]
    {:s 0 :hp 0 :hits 0 :lethal 0 :stays 1})
  (are [lines] (nil? (:long (counts lines)))
    [queued-mine (line 1000 :planned "j1/dig" {}) (line 61000 :planned "j1/dig" {})]
    [queued-mine (line 1000 :holding "j1" {}) (line 6000 :hurt nil {:amount 4 :health 16}) (line 11000 :planned "j1/dig" {})]
    [queued-mine (str/replace (line 1000 :planned "j1/dig" {}) ":job-id \"j1/dig\"" ":job-id \"j1/dig\", :reflex-id :hostile-near")
     (line 6000 :hurt nil {:amount 4 :health 16}) (line 11000 :planned "j1/dig" {})]))

(deftest a-hit-counts-in-the-tail-of-each-size-it-reaches
  (is (= [2 1 1 0] (take 4 (:at-least (:work (counts [(line 0 :queued "j1" {:name "jobs.farm.tend"}) (line 1000 :planned "j1" {})
                                                       (line 2000 :hurt nil {:amount 1 :health 19}) (line 3000 :hurt nil {:amount 3 :health 16})
                                                       (line 4000 :planned "j1" {})])))))))
