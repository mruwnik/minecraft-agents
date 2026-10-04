(ns jobs.debug.notify-test
  "jobs.debug.notify against the fake world: a canonical event with a sensing
  snapshot, a :notify memory entry, and a trigger that fires it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.fake :as fake]
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

(deftest round-emits-a-canonical-event-and-a-notify-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:health 7 :food 12 :oxygen 9 :onFire true
                                                :pos {:x 1.4 :y 64 :z -2.6}}
                                         :time 6000})]
          (core/submit! eng '(jobs.debug.notify {:text "burning fired"}) {})
          (await (core/tick! eng))
          (let [[e :as evs] (notify-events seen)
                text (:message e)]
            (is (= 1 (count evs)))
            (is (not (contains? e :level)))
            (is (= :job (:source e)))
            (is (= :notice (:attention e)) "explicit notification is surfaced as a notice")
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
          (is (re-find #"hostiles 1" (:message (first (notify-events seen))))))))))

(deftest a-registered-trigger-fires-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:onFire true}})]
          (core/register-reflex! eng {:id :notify-burning :trigger :burning :job '(jobs.debug.notify {:text "burning fired"})})
          (await (core/tick! eng))
          (is (= 1 (count (notify-events seen)))))))))

(defn chat-lines [p] (mapv :message (:chat @(fake/state p))))

(defn ^:async notify-chat
  "Run notify with args on a fake world, with prep called on the primitives first; [p seen eng]."
  [args prep]
  (let [{:keys [eng p seen]} (setup {:entities [{:kind "player" :username "Steve" :name "Steve" :pos {:x 2 :y 64 :z 0}}]})]
    (prep p)
    (core/submit! eng (list 'jobs.debug.notify args) {})
    (await (core/tick! eng))
    [p seen eng]))

(deftest chat-true-sends-the-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[p _ eng] (await (notify-chat {:text "hello" :chat? true} identity))]
          (is (= ["hello"] (chat-lines p)))
          (is (empty? (:list (core/state eng)))))))))

(deftest chat-false-sends-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[p] (await (notify-chat {:text "hello" :chat? false} identity))]
          (is (empty? (chat-lines p))))))))

(deftest a-blocked-chat-still-finishes-done-and-is-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ seen eng] (await (notify-chat {:text "hello" :chat? true}
                                               #(.override (.-world %) "chat" (fn ^:async f [_ _ _] #js {:status "blocked" :reason "rate"}))))
              failed (filterv #(= :notify.chat-failed (:kind %)) @seen)]
          (is (empty? (:list (core/state eng))) "done")
          (is (= 1 (count failed)))
          (is (not (contains? (first failed) :level)))
          (is (re-find #"blocked" (:message (first failed))))
          (is (re-find #"rate" (:message (first failed)))))))))

(deftest a-command-text-is-refused-by-the-fake-and-not-recorded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[p seen eng] (await (notify-chat {:text "/op me" :chat? true} identity))
              failed (filterv #(= :notify.chat-failed (:kind %)) @seen)]
          (is (empty? (chat-lines p)))
          (is (empty? (:list (core/state eng))))
          (is (re-find #"cannot" (:message (first failed))))
          (is (re-find #"command" (:message (first failed)))))))))

(deftest chat-with-to-whispers-the-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[p] (await (notify-chat {:text "psst" :chat? true :to "Steve"} identity))
              lines (:chat @(fake/state p))]
          (is (= 1 (count lines)))
          (is (= "psst" (:message (first lines))))
          (is (= "Steve" (:to (first lines)))))))))

(deftest to-defaults-to-nil
  (is (nil? (get-in (:args (get registry/jobs 'jobs.debug.notify)) [:to :default]))))
