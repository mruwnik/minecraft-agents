(ns engine.night-a-test
  "The one night handler: the :night trigger and jobs.survival.night. A bed first (seen, remembered within :bed-radius
  under any roof, or carried, also in the open), then a sleeper on the server (the action bar's sleep count, or a
  sleeping player in sight): logged out in short stints until morning, then roofed or buried: the queue runs, else
  dig in."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [engine.events :as events]
            [engine.hut-shelter-test :as hut]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            
            [jobs.lib.targets :as targets]
            [jobs.survival.night :as night]
            [jobs.survival.night.sites :as sites]
            [engine.night-support :refer [night noon roof holds? write! status! closed-hut holds-after? sleeper offline-why on-return! sleep-count! log-out-child fired wide-ground hillside breached-room run-night-in home-roof lit-world know-home! night-at-home with-child-log walled-home walled-pen bed-world bed-places dug-into-home stone-room block-at room-with-doorway doorway-shut-early night-holds-now? dark-world flee-home-night stone-and-dirt-patch flee-with cave-blocks cave-night held-with-raw-beef furnace-ops held-starving cow zombie]]))

;; ------------------------------------------------------------------ trigger
(deftest night-holds-for-a-remembered-bed-within-the-radius-under-any-roof
  (let [world {:time night :floor tu/walk-floor :blocks {"0,67,0" "stone" "20,64,0" "red_bed" "60,64,0" "red_bed"}}] ; roofed, not shut in: a floor, nothing seen right over the head
    (is (true? (holds? world #(st/know-bed! % {:x 20 :y 64 :z 0}))) "20 blocks away, roofed")
    (is (false? (holds? world #(st/know-bed! % {:x 60 :y 64 :z 0}))) "beyond 48")
    (is (false? (holds? (assoc world :states {"20,64,0" {:occupied true}}) #(st/know-bed! % {:x 20 :y 64 :z 0})))
        "an occupied bed is not one to sleep in")
    (is (false? (holds? world (fn [eng] (st/know-bed! eng {:x 20 :y 64 :z 0})
                                (write! eng :bed-unreachable {:pos {:x 20 :y 64 :z 0}}))))
        "given up on")
    (is (false? (holds? {:time night :blocks roof} (fn [_]))) "roofed, no bed, nobody asleep")))

(deftest night-holds-for-the-live-spawn-bed-when-the-remembered-bed-is-far
  (let [world {:time night :floor tu/walk-floor :blocks {"0,67,0" "stone" "20,64,0" "red_bed"}}
        far {:x 500 :y 64 :z 500}
        seed (fn [spawn] (fn [eng] (st/know-bed! eng far)
                           (mem/write! (:store eng) :spawn-bed {:pos spawn} {:cap 1 :ttl :forever})))]
    (is (true? (holds? world (seed {:x 20 :y 64 :z 0}))) "the respawn bed is the one within reach")
    (is (false? (holds? (assoc-in world [:blocks "20,64,0"] "air") (seed {:x 20 :y 64 :z 0})))
        "seen not to be a bed")))

(deftest a-body-already-safe-at-night-does-not-hold
  (is (false? (holds? {:time night :blocks closed-hut} (fn [_]))) "shut in a high closed room")
  (is (true? (holds? {:time night :blocks (dissoc closed-hut "0,70,0")} (fn [_]))) "a hole in the roof: not shut in")
  (is (true? (holds? {:time night :blocks {}} (fn [_]))) "outside"))

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

(deftest a-count-sent-while-the-body-lay-in-bed-counts-the-body-itself
  (let [world {:time night :blocks roof}
        slept-and-woke (fn [status] [[:sleep-status status] [:slept {}] [:woke {}]])]
    (is (false? (holds-after? world (slept-and-woke {:skipping true}))) "the night skipped for its own sleep")
    (is (false? (holds-after? world (slept-and-woke {:sleeping 1 :needed 2}))) "it was the one asleep")
    (is (true? (holds-after? world (slept-and-woke {:sleeping 2 :needed 3}))) "someone else asleep too")
    (is (true? (holds-after? world (conj (slept-and-woke {:sleeping 1 :needed 2}) [:sleep-status {:sleeping 1 :needed 2}])))
        "a count since it woke")))

(deftest after-a-log-out-for-a-sleeper-no-count-since-the-return-still-counts-as-asleep
  (let [world {:time night :blocks roof}
        out-and-back (fn [eng] ((status! 1 2) eng)
                       (mem/write! (:store eng) :log-out {:ms 30000 :status "ok"} nil))]
    (is (true? (holds? world (fn [eng] (out-and-back eng) (write! eng :online {})))) "no news since the return")
    (is (false? (holds? world (fn [eng] (out-and-back eng) (write! eng :online {}) ((status! 0 1) eng))))
        "a count of none since the return")))

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

(deftest night-does-not-hold-for-a-placed-bed-that-is-the-spawn-bed
  (let [world {:time noon :blocks {"3,64,0" "red_bed"}}]
    (is (false? (holds? world (fn [eng]
                                (write! eng :bed-placed {:pos {:x 3 :y 64 :z 0}})
                                (write! eng :spawn-bed {:pos {:x 3 :y 64 :z 0}})))))))

(deftest night-does-not-hold-for-a-placed-bed-whose-other-half-is-the-spawn-bed
  (let [world {:time noon :blocks {"3,64,0" "red_bed"}}]
    (is (false? (holds? world (fn [eng]
                                (write! eng :bed-placed {:pos {:x 3 :y 64 :z 0}})
                                (write! eng :spawn-bed {:pos {:x 4 :y 64 :z 0}})))))))

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
(deftest a-remembered-bed-seen-gone-is-retracted-before-any-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (st/know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 3))
          (is (nil? (mem/place (mem/view (:store eng)) :bed)) "no longer a known place")
          (is (= 1 (count (st/emitted seen :bed_missing))))
          (is (empty? (filterv #(and (= :child_started (:kind %)) (= :go (:slot %))) @seen)) "never walked there"))))))

(deftest the-night-digs-in-beside-an-open-bed-instead-of-sleeping-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks {"20,64,0" "red_bed"} :inventory st/dirt-stack})]
          (st/know-bed! eng {:x 20 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 40))
          (is (empty? (st/calls p "sleep")) "an unwalled, unroofed bed is no place to lie")
          (is (not (re-find #"dug in" (str (st/ending-text (first (st/child-ended-because seen :bed-down :bed-open)))))) "said before any dig")
          (is (= 1 (count (st/child-ended-because seen :bed-down :bed-open))) "the open bed is judged once")
          (is (= 1 (count (filterv #(and (= :child_started (:kind %)) (= :sleep (:slot %))) @seen))) "one sleep call for it")
          (is (<= (Math/abs (- 20 (:x (st/pos-of p)))) 3) "the body dug in beside the bed"))))))

(deftest after-a-far-flight-the-night-takes-a-bed-beyond-the-usual-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (st/know-bed! eng {:x 100 :y 64 :z 0})
          (write! eng :displaced {:from {:x 0 :y 64 :z 0}})
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 4))
          (is (empty? (st/emitted seen :night.bed-too-far)) "a bed 100 blocks away is walked to, not given up on"))))))

(deftest a-bed-too-far-to-walk-to-is-said-once-before-the-body-digs-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time night :inventory st/dirt-stack :blocks (merge st/floor {"100,64,0" "red_bed"})})]
          (st/know-bed! eng {:x 100 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 4))
          (is (= [100] (mapv :x (map :pos (st/emitted seen :night.bed-too-far))))))))))

(deftest a-near-home-and-spawn-bed-beat-a-stale-far-bed-place-in-the-too-far-note
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time night :inventory st/dirt-stack :blocks (merge st/floor {"2,64,0" "red_bed"})})]
          (st/know-bed! eng {:x 100 :y 64 :z 0})
          (mem/write! (:store eng) :home {:pos {:x 1 :y 64 :z 0}} mem/place-policy)
          (mem/write! (:store eng) :spawn-bed {:pos {:x 2 :y 64 :z 0}} {:cap 1 :ttl :forever})
          (write! eng :sleep-failed {:pos {:x 2 :y 64 :z 0}})
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 4))
          (is (empty? (st/emitted seen :night.bed-too-far))))))))

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

(deftest sleep-says-why-it-waits
  (let [reason (fn [world know?]
                 (let [{:keys [eng]} (st/setup world)]
                   (when know? (st/know-bed! eng {:x 6 :y 64 :z 0}))
                   (let [id (core/submit! eng '(jobs.survival.sleep) {})]
                     (core/tick! eng)
                     (:reason (:waiting (job-api/summary eng id))))))]
    (is (= :no-bed (reason {:time night} false)))
    (is (= :not-night (reason {:time noon :blocks {"6,64,0" "red_bed"}} true)))))

(deftest night-job-says-why-it-waits-by-day
  (let [{:keys [eng]} (st/setup {:time noon})
        id (core/submit! eng '(jobs.survival.night) {})]
    (core/tick! eng)
    (is (= :not-night (:reason (:waiting (job-api/summary eng id)))))))

(deftest a-night-job-already-safe-is-done-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks roof})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= [] (tu/walk-calls p))))))))

(deftest the-night-says-once-that-the-respawn-bed-it-sees-gone-is-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/dirt-stack :blocks (merge st/floor {"6,64,0" "stone"})})]
          (mem/write! (:store eng) :spawn-bed {:pos {:x 6 :y 64 :z 0}} {:cap 1 :ttl :forever})
          (st/dawn-after! p 3)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 20))
          (is (= [{:pos {:x 6 :y 64 :z 0} :why :seen-gone}] (st/entries eng :spawn-lost)))
          (is (= 1 (count (st/emitted seen :spawn_lost)))))))))

(deftest a-sheltered-night-still-says-the-respawn-bed-it-sees-gone-is-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time night :blocks (merge roof {"6,64,0" "stone"})})]
          (mem/write! (:store eng) :spawn-bed {:pos {:x 6 :y 64 :z 0}} {:cap 1 :ttl :forever})
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 20))
          (is (= 1 (count (st/emitted seen :spawn_lost)))))))))

(deftest the-night-check-shows-the-spawn-state
  (let [waiting (fn [world seed!]
                  (let [{:keys [eng]} (st/setup world)
                        _ (seed! eng)
                        id (core/submit! eng '(jobs.survival.night) {})]
                    (core/tick! eng)
                    (:spawn (:waiting (job-api/summary eng id)))))
        bed {:pos {:x 6 :y 64 :z 0}}]
    (is (nil? (waiting {:time noon} (constantly nil))))
    (is (= {:bed {:x 6 :y 64 :z 0}} (waiting {:time noon :blocks {"6,64,0" "red_bed"}} #(write! % :spawn-bed bed))))
    (is (= {:lost {:x 6 :y 64 :z 0} :why :seen-gone} (waiting {:time noon :blocks {"6,64,0" "stone"}} #(write! % :spawn-bed bed))))))

(deftest the-night-puts-a-carried-bed-down-in-the-open-sleeps-and-picks-it-up-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}] :skipNight false})
              mid (atom nil)]
          (st/on-wait! p (fn [n] (case n
                                1 (reset! mid {:sleeps (mapv st/arg-pos (st/calls p "sleep")) :placed (st/entries eng :bed-placed)})
                                2 (do (fake/swap-self! p assoc :isSleeping false) (.setTime (.-world p) noon))
                                nil)))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (let [foot (st/arg-pos (first (st/calls p "place")))]
            (is (some? foot) "put down beside the body, no roof needed")
            (is (= [foot] (:sleeps @mid)))
            (is (= [{:pos foot :own-zone false}] (mapv #(select-keys % [:pos :own-zone]) (st/emitted seen :night.bed-placed)))
                "says where the bed went")
            (is (= [{:pos foot}] (:placed @mid)) "outside any own zone: remembered for the morning")
            (is (= [foot] (mapv st/arg-pos (st/calls p "dig"))) "picked up by day, in the same round")
            (is (= [] (st/entries eng :bed-placed)))
            (is (empty? (st/calls p "moveTo")) "the drop is walked to by go-to, not a direct move")
            (is (= [] (:list (core/state eng))))))))))

(deftest bed-down-puts-a-carried-bed-down-and-sleeps-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}] :skipNight false})
              r (await (tu/child-outcome eng 'jobs.survival.bed-down {} 20))
              foot (st/arg-pos (first (st/calls p "place")))]
          (is (some? foot) (pr-str r))
          (is (= [foot] (mapv st/arg-pos (st/calls p "sleep"))))
          (is (true? (:slept r)) (pr-str r)))))))

(deftest a-sleeper-on-the-server-logs-the-body-out-for-a-short-stint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (on-return! p clock (fn [_] (.setTime (.-world p) noon)))
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 10))
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
          (await (tu/run-until-empty eng 30))
          (is (= 3 (count (st/calls p "offline"))) "out again after a count of one and after no news; back by day")
          (is (= [] (st/calls p "place"))))))))

(deftest once-nobody-sleeps-the-body-shelters-instead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})]
          (on-return! p clock (fn [_] (sleep-count! p 0 1)))
          (sleep-count! p 1 2)
          (st/dawn-after! p 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (= 1 (count (st/calls p "offline"))))
          (is (pos? (count (st/calls p "place"))) "dug in"))))))

(deftest a-bed-comes-before-a-sleeper
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup hut/night-bed-world)]
          (st/know-bed! eng {:x 6 :y 64 :z 0})
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 8))
          (is (= 1 (count (st/calls p "sleep"))))
          (is (= [] (st/calls p "offline"))))))))
