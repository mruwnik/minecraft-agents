(ns plan.drop-status-test
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest are is]]
            [plan.drop-status :as drop-status]))

(def part {:id "a" :cells [[0 64 0]] :want "stone"})

(deftest retired-plans-are-deleted-and-the-rest-lose-the-key
  (let [plans {"live" {:id "live" :status :active :parts [part]}
               "idea" {:id "idea" :status :proposed :parts [part]}
               "done" {:id "done" :status :completed :parts [part]}
               "old" {:id "old" :status :retired :parts [part]}
               "bare" {:id "bare" :parts [part]}}]
    (is (= {:plans {"live" {:id "live" :parts [part]}
                    "idea" {:id "idea" :parts [part]}
                    "done" {:id "done" :parts [part]}
                    "bare" {:id "bare" :parts [part]}}
            :deleted ["old"]
            :stripped ["done" "idea" "live"]}
           (drop-status/migrate plans)))))

(deftest nothing-to-do-for-plans-without-status
  (is (= {:plans {"p" {:id "p" :parts []}} :deleted [] :stripped []}
         (drop-status/migrate {"p" {:id "p" :parts []}}))))

(deftest the-text-loses-only-the-status-pair
  (are [text expected] (= expected (drop-status/strip-status-text text))
    "{:id \"p\" :status :active\n :parts []}" "{:id \"p\"\n :parts []}"
    "{:id \"p\", :status :proposed, :note \"n\"}" "{:id \"p\", :note \"n\"}"
    "{:id \"p\" :parts [] :status :completed}" "{:id \"p\" :parts []}"
    "{:id \"p\" :parts []}" "{:id \"p\" :parts []}"
    "{:status :active :id \"p\"}" "{:id \"p\"}"
    "{:id \"p\" :metadata {:status :nested}}" "{:id \"p\" :metadata {:status :nested}}"
    ";; keep :status in comments?\n{:id \"p\" :status :active :note \":status :active\"}"
    ";; keep :status in comments?\n{:id \"p\" :note \":status :active\"}"))

(deftest the-stripped-text-reads-as-the-plan-without-the-key
  (let [text "{:id \"p\" :status :active\n :note \"x\" :parts [{:id \"a\" :cells [[0 64 0]] :want \"stone\"}]}"]
    (is (= (dissoc (reader/read-string text) :status)
           (reader/read-string (drop-status/strip-status-text text))))))
