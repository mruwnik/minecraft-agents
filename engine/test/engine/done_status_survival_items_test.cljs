(ns engine.done-status-survival-items-test
  "done? of the items and survival jobs is :ok when the whole goal holds, read from the world (docs/contract-conditions-design.md 2.2)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]))

(defn done-of
  "done? of job sym in a world spec, args over its defaults, memory m."
  [sym world args m]
  (let [c {:primitives (tu/seeing-all (tu/fake world)) :args (second (expr/leaf registry/jobs sym args)) :root "j" :slots []
           :id "j" :engine {:warned (atom #{}) :world (world/of-data {} {} [])} :emit (fn [& _]) :sym sym
           :view (fn [] {:now 1000 :data {:entries {(mem/job-kind "j") [{:t 1000 :data m}]}}})}]
    ((:done? (get registry/jobs sym)) c)))

(defn stack [n] [{:name "stick" :count n}])

(deftest craft-is-done-when-the-count-is-gained-and-the-table-is-up
  (are [inventory args m want] (= want (tu/kind-reason (done-of 'jobs.items.craft {:inventory inventory :floor [-3 -3 3 3] :blocks {"1,64,0" "crafting_table"}} (merge {:item "stick" :count 2} args) m)))
    (stack 2) {} {:start 0} [:ok :crafted]
    (stack 4) {} {:start 2} [:ok :crafted]
    (stack 1) {} {:start 0} [:later :crafting]
    (stack 1) {} {:start 1} [:later :crafting]
    (stack 1) {} {} [:later :crafting]
    (stack 0) {:count 0} {} [:ok :crafted]
    (stack 2) {} {:start 0 :table-placed {:x 1 :y 64 :z 0}} [:ok :crafted]
    (stack 2) {:keep-table true} {:start 0 :table-placed {:x 1 :y 64 :z 0}} [:ok :crafted]
    (stack 2) {} {:start 0 :table-placed {:x 1 :y 64 :z 0} :picked-up true} [:ok :crafted]
    (stack 2) {} {:start 0 :table-placed {:x 5 :y 64 :z 0}} [:ok :crafted]))

(deftest craft-already-carried-says-so-not-crafted-0
  (let [text (fn [m] (nth (done-of 'jobs.items.craft {:inventory (stack 2)} {:item "stick" :count 2} m) 1))]
    (is (= "already carried 2 stick; pass :count N for more" (text {:start 2})))
    (is (= "crafted 2 stick" (text {:start 0 :crafted 2})))))

(def load3 {:item "raw_iron" :count 3 :base 0})

(deftest smelt-is-done-when-the-load-is-taken
  (are [m want] (= want (tu/kind-reason (done-of 'jobs.items.smelt {:inventory [{:name "raw_iron" :count 3}]} {:item "raw_iron" :count 3} m)))
    {} [:later :smelting]
    {:owed load3 :got 2} [:later :smelting]
    {:owed load3 :got 3} [:ok :smelted]
    {:owed {:item "raw_iron" :count 5 :base 2} :got 5} [:ok :smelted]
    {:items ["a" "b"] :batch {"a" {:smelted 2 :wanted 2}}} [:later :smelting]
    {:items ["a" "b"] :batch {"a" {:smelted 2 :wanted 2} "b" {:smelted 0 :wanted 2 :reason "no-fuel"}}} [:later :smelting]
    {:items ["a" "b"] :batch {"a" {:smelted 2 :wanted 2} "b" {:smelted 1 :wanted 1}}} [:ok :smelted]))

(deftest deposit-is-done-when-nothing-spare-is-left-and-the-chest-is-offered
  (are [inventory args m want]
       (= want (tu/kind-reason (done-of 'jobs.storage.deposit {:inventory inventory :floor [-3 -3 3 3] :blocks {"3,64,0" "chest"}} (merge {:chest [3 64 0]} args) m)))
    [{:name "dirt" :count 5}] {} {} [:later :depositing]
    [] {} {} [:ok :deposited]
    [{:name "dirt" :count 5}] {:keep {"dirt" 5}} {} [:ok :deposited]
    [] {} {:deposited 2 :moved 5} [:later :offering]
    [] {} {:deposited 2 :moved 5 :offered true} [:ok :deposited]))

