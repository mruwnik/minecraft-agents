(ns engine.bring-test
  "jobs.animals.bring against the fake world: it picks lead-to or lure-to and passes the ending up."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.expr :as expr]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.animals.bring :as bring]))

(defn cow [id x]
  {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}})

(def goal {:x 30 :y 64 :z 0})
(def lead [{:name "lead" :count 1}])
(def wheat [{:name "wheat" :count 4}])

(defn ctx-of [inventory args]
  {:primitives (tu/seeing-all (tu/fake {:inventory inventory}))
   :args (second (expr/leaf registry/jobs 'jobs.animals.bring (merge {:mob "cow" :pos goal} args)))})

(deftest the-way-follows-the-args-and-what-is-carried
  (are [inventory args method] (= method (bring/method-of (ctx-of inventory args)))
    lead {} :lead
    wheat {} :lure
    [] {} :lure
    [] {:mob "zombie"} :lead
    wheat {:fence {:x 31 :y 64 :z 0}} :lead
    lead {:uuid "u1"} :lure
    lead {:box {:min {:x 28 :y 64 :z -1} :max {:x 32 :y 64 :z 1}}} :lure
    lead {:method :lure} :lure
    wheat {:method :lead} :lead))

(deftest a-way-that-cannot-take-the-args-never-starts
  (are [args expected] (= expected (tu/kind-reason (bring/can-start? (ctx-of [] args))))
    {:mob nil} [:never :no-mob]
    {:pos nil} [:never :no-destination]
    {:method :lure :fence {:x 31 :y 64 :z 0}} [:never :bad-way]
    {:method :lead :uuid "u1"} [:never :bad-way]
    {:method :lure :pos nil} [:never :no-destination]
    {} [:ok :order]))

(def box {:min {:x 28 :y 64 :z -1} :max {:x 32 :y 64 :z 1}})

(deftest a-lure-into-a-box-that-leaves-no-animal-in-it-is-short
  (are [args data want] (= want (bring/short? (ctx-of wheat args) data))
    {:box box} {:end :led :inside 0} true
    {:box box} {:end :led :inside 1} false
    {:box box} {:end :led} false
    {} {:end :led :inside 0} false
    {:box box} {:end :tie :inside 0} false))

(defn ^:async bring! [world args]
  (let [{:keys [eng clock] :as s} (h/setup (merge {:floor tu/walk-floor} world) 50)]
    (core/submit! eng (list 'jobs.animals.bring (merge {:mob "cow" :pos goal} args)) {})
    (await (tu/run-ticks eng clock 14 700))
    s))

(defn ended [{:keys [seen]}]
  (first (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %))) @seen)))

(deftest leads-a-cow-and-lets-it-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (ended (await (bring! {:inventory lead :entities [(cow 1 3)]} {})))]
          (is (= :completed (:kind e)))
          (is (= [:lead :free "u1"] ((juxt :method :end :animal) e))))))))

(deftest a-badly-hurt-body-still-leads-a-cow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (ended (await (bring! {:inventory lead :self {:health 2} :entities [(cow 1 3)]} {})))]
          (is (= :completed (:kind e))))))))

(deftest lures-a-cow-with-wheat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (ended (await (bring! {:inventory wheat :entities [(cow 1 3)]} {})))]
          (is (= :completed (:kind e)))
          (is (= [:lure :led "u1"] ((juxt :method :end :animal) e))))))))

(deftest a-failed-way-ends-with-its-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (ended (await (bring! {:inventory wheat :entities []} {})))]
          (is (= :stopped (:kind e)))
          (is (= [:lure :no-animal] ((juxt :method :reason) e))))))))

(deftest with-no-animal-in-sight-it-searches-before-giving-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (bring! {:inventory wheat :entities []} {}))]
          (is (some #(and (= :child_started (:kind %)) (= "j1/explore" (:job %))) @(:seen s))))))))

(deftest an-animal-the-search-found-beyond-the-radius-is-then-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (bring! {:inventory wheat :entities [(cow 1 20)]} {})) e (ended s)]
          (is (= [:completed :lure "u1"] ((juxt :kind :method :animal) e))))))))
