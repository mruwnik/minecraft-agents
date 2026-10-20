(ns engine.chase-test
  "jobs.movement.chase against the fake world: one go-to leg aimed at where the thing is now."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.movement.chase :as chase]))

(def job 'jobs.movement.chase)

(defn cow [x] {:id 7 :name "cow" :kind "mob" :pos {:x x :y 64 :z 0}})

(defn ^:async run-chase
  "Chase over a fake world with a go-to stand-in that records its args in calls and ends as (ending c) says; the chase's
  ending."
  [world args calls ending]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        out (atom :not-done)
        stub (assoc (get registry/jobs 'jobs.movement.go-to)
                    :round (fn ^:async stub-round [c] (swap! calls conj (assoc (:args c) :job-time (get-in (ctx/frozen-call c) [:spend :job-time]))) (ending c p)))
        eng (core/create {:primitives p :jobs (merge registry/jobs {'jobs.movement.go-to stub 'recording-parent (tu/recording-parent job args out identity)})
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 10) (= :not-done @out))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    @out))

(def leg-over #(ctx/end % :limit-reached {:reason :unreachable :why :search-spent :line :job-time} {:arrived false}))

(defn nearer [c p]
  (fake/swap-self! p assoc :pos [8 64 0])
  (leg-over c))

(defn arrive [c p]
  (fake/swap-self! p assoc :pos [11 64 0])
  (ctx/end c :done nil {:arrived true}))

(deftest the-leg-is-aimed-at-where-the-entity-stands-now-and-bounded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              r (await (run-chase {:entities [(cow 12)]} {:pos {:x 3 :y 64 :z 0} :entity 7 :range 2 :leg-s 4 :walk {:doors :shut}} calls
                                  (fn [c _] (leg-over c))))]
          (is (= [{:pos {:x 12 :y 64 :z 0} :range 2 :doors :shut :job-time 4}]
                 (map #(select-keys % [:pos :range :doors :job-time]) @calls)))
          (is (= {:kind :limit-reached :cause {:reason :leg :line :job-time} :data {:arrived false :leg true :nearer false}}
                 (select-keys r [:kind :cause :data]))))))))

(deftest a-leg-that-got-more-than-a-block-nearer-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-chase {:entities [(cow 12)]} {:pos {:x 12 :y 64 :z 0} :range 1 :leg-s 4} (atom []) nearer))]
          (is (= {:arrived false :leg true :nearer true} (:data r))))))))

(deftest an-arrival-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-chase {:entities [(cow 12)]} {:pos {:x 12 :y 64 :z 0} :range 1 :leg-s 4} (atom []) arrive))]
          (is (= {:kind :done :data {:arrived true}} (select-keys r [:kind :data]))))))))

(deftest go-tos-give-up-is-passed-up-with-its-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-chase {} {:pos {:x 12 :y 64 :z 0} :range 1 :leg-s 4} (atom [])
                                  #(ctx/end %1 :nothing-possible {:reason :unreachable :why :goal-enclosed} {:arrived false :why :goal-enclosed})))]
          (is (= {:kind :nothing-possible :cause {:reason :unreachable :why :goal-enclosed} :data {:arrived false :why :goal-enclosed}}
                 (select-keys r [:kind :cause :data])))
          (is (= :nothing-possible (:kind (:because r)))))))))

(deftest a-player-is-found-by-name-and-an-unseen-entity-falls-back-to-pos
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])]
          (await (run-chase {:entities [{:id 5 :name "Steve" :kind "player" :pos {:x 9 :y 64 :z 0}}]} {:player "Steve" :pos {:x 1 :y 64 :z 0} :leg-s 2} calls (fn [c _] (leg-over c))))
          (await (run-chase {} {:entity 99 :pos {:x 21 :y 64 :z 0} :leg-s 2} calls (fn [c _] (leg-over c))))
          (is (= [{:x 9 :y 64 :z 0} {:x 21 :y 64 :z 0}] (map :pos @calls))))))))

(deftest it-needs-a-leg-and-a-thing
  (are [args cause] (= cause (tu/kind-reason (chase/can-start? {:args args})))
    {:pos [1 64 1] :leg-s 3} [:ok :args]
    {:entity 4 :leg-s 3} [:ok :args]
    {:player "Steve" :leg-s 3} [:ok :args]
    {:pos [1 64 1]} [:never :bad-args]
    {:leg-s 3} [:never :bad-args]))
