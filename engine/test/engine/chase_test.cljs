(ns engine.chase-test
  "jobs.movement.chase and run-away against the fake world: one go-to leg aimed at where the thing is now."
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
  "A job (chase unless given) over a fake world with a go-to stand-in that records its args in calls and ends as (ending c p) says; the job's
  ending."
  ([world args calls ending] (run-chase world job args calls ending))
  ([world job args calls ending]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        out (atom :not-done)
        stub (assoc (get registry/jobs 'jobs.movement.go-to)
                    :round (fn ^:async stub-round [c] (swap! calls conj (merge (:args c) (select-keys (ctx/frozen-call c) [:ways :floor :settings :checks]) {:job-time (get-in (ctx/frozen-call c) [:spend :job-time])})) (ending c p)))
        eng (core/create {:primitives p :jobs (merge registry/jobs {'jobs.movement.go-to stub 'recording-parent (tu/recording-parent job args out identity)})
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 10) (= :not-done @out))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    @out)))

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

(deftest the-go-to-under-a-chase-leg-keeps-its-own-checks-but-one-under-a-flight-is-unchecked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [chase-calls (atom [])
              flee-calls (atom [])]
          (await (run-chase {} {:pos {:x 12 :y 64 :z 0} :leg-s 3} chase-calls (fn [c _] (leg-over c))))
          (await (run-chase {} 'jobs.movement.run-away {:pos {:x 12 :y 64 :z 0} :leg-s 3} flee-calls (fn [c _] (leg-over c))))
          (is (nil? (:checks (first @chase-calls))) "go-to judges route risk and hostiles itself")
          (is (= '(always) (get-in (first @flee-calls) [:checks :safe?])) "an escape walks unchecked"))))))

(deftest run-away-is-a-chase-leg-with-the-flee-presets
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              r (await (run-chase {} 'jobs.movement.run-away {:pos {:x 12 :y 64 :z 0} :leg-s 3 :y-margin 9} calls
                                  (fn [c _] (leg-over c))))]
          (is (= [{:pos {:x 12 :y 64 :z 0} :ways #{:walk :clear-seal} :floor {:health 0} :job-time 3
                   :settings {:jobs.movement.go-to.tuning/look-round false :jobs.lib.walk.drive/look-over-edge false :jobs.lib.walk.world/y-margin 9}}]
                 (map #(select-keys % [:pos :ways :floor :job-time :settings]) @calls)))
          (is (= {:arrived false :leg true :nearer false} (:data r))))))))

(deftest an-entity-hidden-behind-a-wall-is-not-aimed-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])]
          (await (run-chase {:entities [(assoc (cow 12) :kind "item" :visible false)]} {:pos {:x 3 :y 64 :z 0} :entity 7 :leg-s 4} calls (fn [c _] (leg-over c))))
          (is (= [{:x 3 :y 64 :z 0}] (map :pos @calls)) "the last :pos, not the cow behind the wall"))))))

(deftest under-see-a-thing-in-range-behind-a-wall-is-not-arrived
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              wall (into {} (for [y [64 65] z [-1 0 1]] [(str "1," y "," z) "stone"]))
              r (await (run-chase {:entities [(cow 2)] :blocks wall} {:entity 7 :range 5 :leg-s 4 :walk {:see true}} calls
                                  (fn [c _] (leg-over c))))]
          (is (= 1 (count @calls)) "the walk is made: in range is not seen")
          (is (false? (:arrived (:data r)))))))))
