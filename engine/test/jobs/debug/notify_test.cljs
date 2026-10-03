(ns jobs.debug.notify-test
  "jobs.debug.notify against the fake world: an info event with a sensing
  snapshot, a :notify memory entry, and a trigger that fires it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn notify-events [seen]
  (filterv #(= :job.notify (:kind %)) @seen))

(defn notes [eng] (mapv :data (mem/entries (mem/view (:store eng)) :notify)))

(deftest check-is-always-true
  (is (true? ((:check (get registry/jobs 'jobs.debug.notify)) {}))))

(deftest args-default-text-and-chat
  (let [args (:args (get registry/jobs 'jobs.debug.notify))]
    (is (= "notify" (get-in args [:text :default])))
    (is (false? (get-in args [:chat? :default])))))

(deftest round-emits-an-info-event-and-a-notify-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:health 7 :food 12 :oxygen 9 :onFire true
                                                :pos {:x 1.4 :y 64 :z -2.6}}
                                         :time 6000})]
          (core/submit! eng '(jobs.debug.notify {:text "burning fired"}) {})
          (await (core/tick! eng))
          (let [[e :as evs] (notify-events seen)
                text (:text e)]
            (is (= 1 (count evs)))
            (is (= :info (:level e)))
            (is (= :job (:source e)))
            (is (re-find #"^burning fired" text))
            (is (re-find #"health 7" text))
            (is (re-find #"food 12" text))
            (is (re-find #"oxygen 9" text))
            (is (re-find #"on-fire true" text))
            (is (re-find #"pos 1 64 -3" text))
            (is (re-find #"time 6000" text)))
          (is (= [{:text "burning fired"}] (notes eng)))
          (is (empty? (:list (core/state eng))) "returns :done"))))))

(deftest hostiles-are-counted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:entities [{:name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}
                                                    {:name "cow" :kind "passive" :pos {:x 2 :y 64 :z 0}}]})]
          (core/submit! eng '(jobs.debug.notify) {})
          (await (core/tick! eng))
          (is (re-find #"hostiles 1" (:text (first (notify-events seen))))))))))

(deftest a-registered-trigger-fires-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:onFire true}})]
          (core/register-reflex! eng {:id :notify-burning :trigger :burning :job '(jobs.debug.notify {:text "burning fired"})})
          (await (core/tick! eng))
          (is (= 1 (count (notify-events seen)))))))))
