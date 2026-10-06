(ns engine.night-test
  "The one night handler: the :night trigger and jobs.survival.night. A bed first (seen, remembered within :bed-radius
  under any roof, or carried, also in the open), then a sleeper on the server (the action bar's sleep count, or a
  sleeping player in sight): logged out in short stints until morning, then roofed or buried: the queue runs, else
  dig in."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def night st/night)
(def noon st/noon)
(def roof {"0,66,0" "stone"})

(defn holds?
  "The :night condition in world after seed! wrote body memory; :later moves the clock on after seed!."
  [world seed! & {:keys [later] :or {later 0}}]
  (let [{:keys [eng clock]} (st/setup {})]
    (seed! eng)
    (swap! clock + later)
    (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store eng)) {}))))

(defn write! [eng kind data] (mem/write! (:store eng) kind data nil))

(defn status! [sleeping needed] (fn [eng] (write! eng :sleep-status {:sleeping sleeping :needed needed})))

;; ------------------------------------------------------------------ trigger

(deftest night-is-the-only-night-trigger
  (is (some? (get triggers/all :night)))
  (is (= '(jobs.survival.night) (:job (get triggers/all :night))))
  (is (every? nil? (map triggers/all [:night-unsafe :player-sleeping-nearby :shut-in-by-day]))))

(deftest night-holds-for-a-remembered-bed-within-the-radius-under-any-roof
  (let [world {:time night :blocks (merge roof {"20,64,0" "red_bed" "60,64,0" "red_bed"})}]
    (is (true? (holds? world #(st/know-bed! % {:x 20 :y 64 :z 0}))) "20 blocks away, roofed")
    (is (false? (holds? world #(st/know-bed! % {:x 60 :y 64 :z 0}))) "beyond 48")
    (is (false? (holds? (assoc world :states {"20,64,0" {:occupied true}}) #(st/know-bed! % {:x 20 :y 64 :z 0})))
        "an occupied bed is not one to sleep in")
    (is (false? (holds? world (fn [eng] (st/know-bed! eng {:x 20 :y 64 :z 0})
                                (write! eng :bed-unreachable {:pos {:x 20 :y 64 :z 0}}))))
        "given up on")
    (is (false? (holds? {:time night :blocks roof} (fn [_]))) "roofed, no bed, nobody asleep")))

(deftest night-holds-for-a-sleeper-the-action-bar-counts-tonight
  (let [world {:time night :blocks roof}]
    (is (true? (holds? world (status! 1 2))))
    (is (true? (holds? world (fn [eng] (write! eng :sleep-status {:skipping true})))))
    (is (false? (holds? world (fn [eng] ((status! 1 2) eng) ((status! 0 2) eng)))) "the sleeper woke")
    (is (false? (holds? world (status! 1 2) :later (* 50 (- 14000 12542) 2))) "a count from before dusk")
    (is (false? (holds? world (fn [eng] ((status! 1 2) eng)
                                (mem/write! (:store eng) :log-out {:ms 0 :status "unsupported"} nil))))
        "the server does not let it log out")
    (is (false? (holds? (assoc world :time noon) (status! 1 2))) "by day")
    (is (false? (holds? (assoc-in world [:self :isSleeping] true) (status! 1 2))) "asleep itself")))

(deftest after-a-log-out-for-a-sleeper-no-count-since-the-return-still-counts-as-asleep
  (let [world {:time night :blocks roof}
        out-and-back (fn [eng] ((status! 1 2) eng)
                       (mem/write! (:store eng) :log-out {:ms 30000 :status "ok"} nil))]
    (is (true? (holds? world (fn [eng] (out-and-back eng) (write! eng :online {})))) "no news since the return")
    (is (false? (holds? world (fn [eng] (out-and-back eng) (write! eng :online {}) ((status! 0 1) eng))))
        "a count of none since the return")))

(def sleeper {:id 5 :name "Alex" :kind "player" :sleeping true :pos {:x 4 :y 64 :z 0}})

(deftest night-holds-for-a-sleeper-in-sight-not-one-behind-a-wall
  (let [world {:time night :blocks roof :entities [sleeper]}]
    (is (true? (holds? world (fn [_]))))
    (is (false? (holds? (update world :blocks merge {"2,64,0" "stone" "2,65,0" "stone"}) (fn [_]))) "behind a wall")))

(deftest night-holds-by-day-for-a-bed-it-put-down-outside-its-zone
  (let [world {:time noon :blocks {"3,64,0" "red_bed"}}
        placed (fn [eng] (write! eng :bed-placed {:pos {:x 3 :y 64 :z 0}}))]
    (is (true? (holds? world placed)))
    (is (false? (holds? {:time noon} placed)) "the bed is gone")
    (is (false? (holds? world (fn [_]))) "a bed it did not put down")))

(deftest a-muted-night-never-fires
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/mute! eng :night nil)
          (await (st/tick-n eng 4))
          (is (= [] (st/calls p "place")))
          (is (empty? (:instances (core/state eng)))))))))

;; ------------------------------------------------------------------ job: beds

(deftest the-night-walks-to-a-remembered-bed-under-a-roof-and-sleeps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks (merge roof {"20,64,0" "red_bed"})})]
          (st/know-bed! eng {:x 20 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/run-until-empty eng 20))
          (is (= [{:x 20 :y 64 :z 0}] (mapv st/arg-pos (st/calls p "sleep")))))))))

(deftest the-night-leaves-an-occupied-remembered-bed-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks (merge roof {"20,64,0" "red_bed"})
                                         :states {"20,64,0" {:occupied true}}})
              eng (update eng :triggers assoc :always-night (assoc st/always-shelter :name :always-night :job '(jobs.survival.night)))]
          (st/know-bed! eng {:x 20 :y 64 :z 0})
          (core/register-reflex! eng {:trigger :always-night})
          (await (st/tick-n eng 4))
          (is (= [] (st/calls p "sleep")))
          (is (= [] (tu/walk-calls p)) "roofed with nothing else to do: done without a step"))))))

(deftest sleep-declines-an-occupied-remembered-bed
  (let [{:keys [eng]} (st/setup {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}})]
    (st/know-bed! eng {:x 6 :y 64 :z 0})
    (core/submit! eng '(jobs.survival.sleep) {})
    (is (nil? (core/tick! eng)))))

(deftest the-night-puts-a-carried-bed-down-in-the-open-sleeps-and-picks-it-up-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}] :skipNight false})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 8))
          (let [foot (st/arg-pos (first (st/calls p "place")))]
            (is (some? foot) "put down beside the body, no roof needed")
            (is (= [foot] (mapv st/arg-pos (st/calls p "sleep"))))
            (is (= [{:pos foot}] (st/entries eng :bed-placed)) "outside any own zone: remembered for the morning")
            (fake/swap-self! p assoc :isSleeping false)
            (.setTime (.-world p) noon)
            (await (st/run-until-empty eng 20))
            (is (= [foot] (mapv st/arg-pos (st/calls p "dig"))) "picked up by day")
            (is (= [] (st/entries eng :bed-placed)))))))))

;; ------------------------------------------------------------------ job: sleepers

(defn offline-why [p] (mapv #(.-why (.-args %)) (st/calls p "offline")))

(defn on-return!
  "Each return from offline moves the engine clock on by the time away and calls (f n), n the return's count."
  [p clock f]
  (let [n (atom 0)]
    (.override (.-world p) "offline"
               (fn ^:async g [token a impl]
                 (let [r (await (impl token a))]
                   (swap! clock + (.-ms a))
                   (.emit (.-world p) #js {:kind "online" :pos #js {:x 0 :y 64 :z 0}})
                   (f (swap! n inc))
                   r)))))

(defn sleep-count! [p sleeping needed]
  (.emit (.-world p) #js {:kind "sleep-status" :sleeping sleeping :needed needed}))

(deftest a-sleeper-on-the-server-logs-the-body-out-for-a-short-stint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (on-return! p clock (fn [_] (.setTime (.-world p) noon)))
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/run-until-empty eng 10))
          (is (= [30000] (st/ms-of-offline p)))
          (is (= ["logged-out-for-sleeping-player"] (offline-why p)))
          (is (= [] (st/calls p "place")) "not dug in")
          (is (= [] (:list (core/state eng))) "ended at morning"))))))

(deftest the-body-stays-out-while-someone-sleeps-and-the-news-is-unknown-until-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (on-return! p clock (fn [n] (case n
                                        1 (sleep-count! p 1 2)
                                        2 nil
                                        (.setTime (.-world p) noon))))
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/run-until-empty eng 30))
          (is (= 3 (count (st/calls p "offline"))) "out again after a count of one and after no news; back by day")
          (is (= [] (st/calls p "place"))))))))

(deftest once-nobody-sleeps-the-body-shelters-instead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (on-return! p clock (fn [_] (sleep-count! p 0 1)))
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (= 1 (count (st/calls p "offline"))))
          (is (pos? (count (st/calls p "place"))) "dug in"))))))

(deftest a-bed-comes-before-a-sleeper
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks {"6,64,0" "red_bed"}})]
          (st/know-bed! eng {:x 6 :y 64 :z 0})
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/run-until-empty eng 8))
          (is (= 1 (count (st/calls p "sleep"))))
          (is (= [] (st/calls p "offline"))))))))

(deftest a-log-out-stint-ends-at-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time 23000})]
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.log-out) {})
          (await (st/run-until-empty eng 3))
          (is (= [25050] (st/ms-of-offline p))))))))
