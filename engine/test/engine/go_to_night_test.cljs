(ns engine.go-to-night-test
  "go-to at night from a shelter (sh/walk-leaves-shelter?): a walk that would take the body out of it waits until day
  (a listed go-to declines :night-sheltered, a child call yields :continue), so a queued job does not lead the body
  into the dark and the night trigger does not dig it in again and again."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.hut-shelter-test :as hut]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.shelter-test :as st]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.child :as child]
            [jobs.lib.shelter :as sh]))

(def inside {:x 5 :y 64 :z 0})
(def outside {:x 15 :y 64 :z 0})

(defn night-hut [extra] (merge (hut/hut-world inside extra) {:time st/night}))

(defn ^:async ticks [eng n]
  (await (st/tick-n eng n)))

(defn waiting-of [eng id] (core/waiting eng id))

;; ------------------------------------------------------------------ which walks leave the shelter

;; a 1x1 pit with a roof: dirt round the feet and head cells, a roof over the head (a closed room of one cell; dug in
;; when its :shelter entry says so)
(def pit
  (merge (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str (+ 0 dx) "," y "," (+ 0 dz)) "dirt"]))
         {"0,66,0" "dirt"}))

(deftest walk-leaves-shelter-names-the-shelter-a-walk-would-leave
  (are [world shelter goal range kind] (= kind (sh/walk-leaves-shelter? (tu/fake (merge {:floor tu/walk-floor} world)) shelter goal range))
    (night-hut {}) nil outside 1 :room
    (night-hut {}) nil {:x 6 :y 64 :z -1} 0 nil
    (night-hut {}) nil {:x 4 :y 64 :z 1} 1 nil
    (night-hut {}) nil {:x 6 :z -1} 1 nil
    (night-hut {}) nil {:x 15 :z 0} 1 :room
    (merge (night-hut {}) {:time st/noon}) nil outside 1 nil
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}}} nil outside 1 nil
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit} nil outside 1 :room
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit} nil {:x 0 :y 64 :z 1} 1 nil
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit} {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0}} outside 1 :dug-in
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks (merge pit {"15,66,0" "stone"})} {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0}} outside 1 :dug-in
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,67,0" "stone"}} nil outside 1 :roofed
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,67,0" "stone" "15,67,0" "stone"}} nil outside 1 :roofed))

;; ------------------------------------------------------------------ a listed go-to

(deftest a-listed-go-to-out-of-the-hut-waits-at-night-and-walks-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (night-hut {}))
              id (core/submit! eng (list 'jobs.movement.go-to {:pos [15 64 0]}) {})]
          (await (ticks eng 6))
          (is (= inside (st/pos-of p)) "the body stays in")
          (is (= {:reason :night-sheltered :shelter :room} (select-keys (waiting-of eng id) [:reason :shelter])))
          (is (seq (:list (core/state eng))) "the go-to is still listed")
          (.setTime (.-world p) st/noon)
          (await (tu/run-until-empty eng 40))
          (is (empty? (:list (core/state eng))))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "by day it walks out"))))))

(deftest a-walk-inside-the-room-runs-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (night-hut {}))]
          (core/submit! eng '(jobs.movement.go-to {:pos [6 64 -1] :range 0}) {})
          (await (tu/run-until-empty eng 20))
          (is (empty? (:list (core/state eng))))
          (is (= {:x 6 :z -1} (select-keys (st/pos-of p) [:x :z]))))))))

(deftest at-night-go-walks-out-anyway
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (night-hut {}))]
          (core/submit! eng '(jobs.movement.go-to {:pos [15 64 0] :at-night :go}) {})
          (await (tu/run-until-empty eng 40))
          (is (empty? (:list (core/state eng))))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p)))))))))))

(deftest a-manual-go-to-walks-out-of-the-hut-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (night-hut {}))
              seen-held (:ok (takeover/take! eng {:who "claude" :why "own order"}))
              id (core/submit! eng '(jobs.movement.go-to {:pos [15 64 0]}) {:by "claude"})]
          (is seen-held)
          (await (ticks eng 40))
          (is (nil? (:reason (waiting-of eng id))) "the driver's own go-to does not wait for day")
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "it walks out at night"))))))

;; ------------------------------------------------------------------ a child go-to

(deftest a-child-go-to-out-of-the-hut-yields-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake-on-floor (night-hut {}))
              rounds (atom [])
              parent {:check (constantly true)
                      :round (fn ^:async walker [c]
                               (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos [15 64 0]}))]
                                 (swap! rounds conj r)
                                 r))}
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent) :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(walker) {})
          (await (ticks eng 4))
          (is (= inside (st/pos-of p)))
          (is (seq @rounds))
          (is (every? #{:continue} @rounds) "the child yields")
          (is (= 1 (count (st/emitted seen :go-to.night-sheltered))) "said once"))))))

(deftest a-parent-walking-with-call-walk-shows-the-night-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake-on-floor (night-hut {}))
              parent {:check (constantly true)
                      :round (fn ^:async walker [c]
                               (await (child/call-walk c :walk {:pos [15 64 0]})))}
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent) :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(walker) {})
          (await (ticks eng 4))
          (is (= inside (st/pos-of p)))
          (is (= [:night-sheltered] (distinct (map :reason (st/emitted seen :holding))))))))))

(deftest get-seeds-in-a-shelter-at-night-holds-with-the-reason-instead-of-spinning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/seeing-all (tu/fake-on-floor (-> (night-hut {})
                                                        (update :blocks merge {"10,64,0" "short_grass" "11,64,0" "short_grass"})
                                                        (assoc :drops {"short_grass" "wheat_seeds"}))))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(jobs.gather.get-seeds {:count 2}) {})
          (await (ticks eng 6))
          (is (= inside (st/pos-of p)))
          (is (= [:night-sheltered] (distinct (map :reason (st/emitted seen :holding))))))))))

(deftest a-parent-walking-with-run!-shows-the-night-wait-and-yields
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake-on-floor (night-hut {}))
              parent {:check (constantly true)
                      :round (fn ^:async walker [c]
                               (await (child/run! c :walk 'jobs.movement.go-to {:pos [15 64 0]})))}
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent) :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(walker) {})
          (await (ticks eng 4))
          (is (= inside (st/pos-of p)))
          (is (= [:night-sheltered] (distinct (map :reason (st/emitted seen :holding))))))))))

;; ------------------------------------------------------------------ the night trigger and a queued walk (card 4252e02e)

(deftest a-queued-walk-does-not-lead-the-body-out-of-its-mended-room-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (night-hut {"5,67,0" "air"}) {:inventory st/dirt-stack}))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.movement.go-to {:pos [15 64 0]}) {})
          (await (ticks eng 20))
          (is (= inside (st/pos-of p)) "the body stays in its room all night")
          (is (= [{:x 5 :y 67 :z 0}] (mapv st/arg-pos (st/calls p "place"))) "one block mends the roof, no dig-in after")
          (.setTime (.-world p) st/noon)
          (await (tu/run-until-empty eng 40))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "by day the walk runs")
          (is (= 1 (count (st/calls p "place")))))))))

;; ------------------------------------------------------------------ in-place jobs still run

(deftest a-listed-get-food-eats-carried-bread-in-the-hut-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (-> (night-hut {})
                                            (assoc-in [:self :food] 2)
                                            (assoc :inventory [{:name "bread" :count 4}])))]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (tu/run-until-empty eng 20))
          (is (= ["bread"] (distinct (map #(aget (.-args %) "item") (st/calls p "eat")))) (pr-str (map #(.-name %) (.-calls (.-world p)))))
          (is (= inside (st/pos-of p))))))))
