(ns engine.moved-test
  "The :moved memory entry written by act, and the fake-world helpers other tests share."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def t0 1000000)

(defn setup
  ([world] (setup world (tu/tmp-dir) t0))
  ([world dir start]
   (let [clock (atom start)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/fake world)
         eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir
                           :now #(deref clock) :backoff false
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
      {:eng eng :p p :seen seen :clock clock :dir dir})))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [p name]
  (mapv #(js->clj (.-args %) :keywordize-keys true) (calls p name)))

(def at5 {:x 5 :y 64 :z 0})

(def deep-pit
  "A 3-deep 1x1 pit: feet at y 64, walls at y 64, 65, 66, ground under the feet."
  (into {"5,63,0" "stone"} (for [[x z] [[4 0] [6 0] [5 1] [5 -1]] y [64 65 66]] [(str x "," y "," z) "stone"])))

(defn moved [eng] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))

;; ---------------------------------------------------------------- act

(deftest act-records-a-moved-entry-for-move-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:floor [-2 -2 8 2]})]
          (core/submit! eng '(jobs.movement.pace {:a {:x 5 :y 64 :z 0} :b {:x 5 :y 64 :z 0} :laps 1}) {})
          (await (core/tick! eng))
          (is (= [{:from {:x 0 :y 64 :z 0} :to {:x 4 :y 64 :z 0} :status "arrived" :target at5}]
                 (moved eng)) "the walker arrives within range of the target")
          (is (= {:cap 20 :ttl 600000} (mem/policy (mem/view (:store eng)) :moved))))))))

(deftest act-records-a-moved-entry-even-when-the-body-does-not-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:unreachable ["5,64,0"]})]
          (core/submit! eng '(jobs.movement.pace {:a {:x 5 :y 64 :z 0} :b {:x 5 :y 64 :z 0} :laps 1}) {})
          (await (core/tick! eng))
          (is (= (repeat 3 {:from {:x 0 :y 64 :z 0} :to {:x 0 :y 64 :z 0} :status "blocked" :target at5 :no-path true})
                 (moved eng)) "go-to walks three times before it gives up"))))))

(deftest act-writes-no-moved-entry-for-other-primitives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:time 1000})]
          (core/submit! eng '(jobs.movement.look-around) {})
          (await (core/tick! eng))
          (is (= [] (moved eng))))))))
