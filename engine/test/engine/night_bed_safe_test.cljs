(ns engine.night-bed-safe-test
  "A body safe from the night (underground, dug in, shut in a closed room) is not sent through the dark to a remembered
  bed: sh/bed-to-use skips a bed whose walk leaves that shelter (sh/walk-leaves-shelter?), so the night trigger does not
  hold for it and the night job does not walk there."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.hut-shelter-test :as hut]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.shelter :as sh]))

(def far-bed {:x 40 :y 64 :z 0})

(def buried-light
  "Sky light 0 at the miner's feet and head cells (x 5 in the tunnel): sealed from the sky."
  {[5 64 0] [0 0] [5 65 0] [0 0]})

(defn tunnel-world
  "A miner at x 5 in the one-wide stone tunnel (x 0..20), at night, with a bed block at far-bed in the open."
  [light]
  {:floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 0}} :time st/night :offlineScale 0.0001
   :blocks (merge hut/tunnel {"40,64,0" "red_bed"}) :light light})

(defn lit-fake
  "{:raw :p}: a fake body and its perception wrap, so the raw world's sky light is read (sh/buried?). :sees-all in the
  world: the fake's own sight (every block seen) with the perception only for its light."
  [world]
  (let [raw-p (tu/fake (dissoc world :light))
        _ (swap! (fake/state raw-p) assoc :light (:light world {}))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})]
    {:raw raw-p :p (if (:sees-all world)
                     (doto (js/Object.assign #js {} raw-p) (aset "perception" per))
                     (perception/wrap raw-p per))}))

(defn setup [world]
  (let [clock (atom 1000000)
        {:keys [raw p]} (lit-fake world)
        _ (st/dawn-after! raw st/default-dawn)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock) :events (events/make {:body "Fake" :sinks [] :now #(deref clock)})})]
    (st/know-bed! eng far-bed)
    {:eng eng :p p :raw raw}))

(defn bed-to-use [world]
  (let [{:keys [eng p]} (setup world)]
    (sh/bed-to-use p (mem/view (:store eng)) 64 (constantly true))))

(defn holds? [world]
  (let [{:keys [eng p]} (setup world)]
    (boolean ((:when (get triggers/all :night)) p (mem/view (:store eng)) {}))))

(deftest a-buried-body-does-not-use-a-bed-out-in-the-open
  (is (nil? (bed-to-use (tunnel-world buried-light))))
  (is (false? (holds? (tunnel-world buried-light))) "the night trigger does not hold"))

(deftest a-buried-body-neither-places-a-carried-bed-nor-logs-out-for-a-sleeper
  (let [carrying (assoc (tunnel-world buried-light) :inventory [{:name "red_bed" :count 1}])
        {:keys [eng p]} (setup carrying)]
    (mem/write! (:store eng) :sleep-status {:sleeping 1 :needed 2} nil)
    (is (false? (sh/night-holds? p (mem/view (:store eng)) {} (constantly true))) "mining goes on")))

(deftest a-body-the-sky-reaches-logs-out-for-a-sleeper
  (let [{:keys [eng p]} (setup (tunnel-world {}))]
    (mem/write! (:store eng) :sleep-status {:sleeping 1 :needed 2} nil)
    (is (true? (sh/night-holds? p (mem/view (:store eng)) {} (constantly true))))))

(defn bed-to-use-after-days
  "bed-to-use for a body that last slept `days` in-game days ago."
  [world days]
  (let [{:keys [eng p]} (setup world)
        _ (mem/write! (:store eng) :slept {:pos {:x 0 :y 64 :z 0}} {:cap 10 :ttl (* 10 sh/ms-per-day)})
        view (assoc (mem/view (:store eng)) :now (+ 1000000 (* days sh/ms-per-day)))]
    (sh/bed-to-use p view 128 (constantly true))))

(deftest a-buried-body-overdue-for-sleep-walks-to-its-bed
  (is (= far-bed (bed-to-use-after-days (tunnel-world buried-light) 3))))

(deftest a-buried-body-not-yet-overdue-stays-in-the-tunnel
  (is (nil? (bed-to-use-after-days (tunnel-world buried-light) 2))))

(deftest a-roofed-body-the-sky-reaches-still-walks-to-its-bed
  (is (= far-bed (bed-to-use (tunnel-world {}))) "a roof alone is no shelter to keep")
  (is (true? (holds? (tunnel-world {})))))

(deftest a-body-out-in-the-open-still-walks-to-its-bed
  (let [world {:floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 0}} :time st/night :blocks {"40,64,0" "red_bed"}}]
    (is (= far-bed (bed-to-use world)))
    (is (true? (holds? world)))))

(deftest a-body-shut-in-a-hut-does-not-walk-out-to-a-far-bed
  (let [world (merge (hut/hut-world {:x 5 :y 64 :z 0} {"30,64,0" "red_bed"}) {:time st/night})]
    (is (false? (hut/holds-with world (fn [eng] (st/know-bed! eng {:x 30 :y 64 :z 0})))))))

(deftest a-buried-body-asked-for-the-night-stays-in-the-tunnel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p raw]} (setup (tunnel-world buried-light))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (= {:x 5 :y 64 :z 0} (update-vals (st/pos-of p) js/Math.floor)) "the body stays in the tunnel")
          (is (empty? (st/calls raw "sleep")))
          (is (empty? (st/calls raw "moveTo")) "no walk toward the far bed"))))))

(def hut-light
  "A closed hut: sky light 0 at the body's feet and head cells."
  {[4 64 -1] [0 0] [4 65 -1] [0 0]})

(defn hut-night-world [blocks extra]
  (merge (hut/hut-world {:x 4 :y 64 :z -1} blocks) {:time st/night :light hut-light :sees-all true} extra))

(defn blind-above
  "p that has not seen the roof: the roof (above the walls) is unknown (a body cannot look up from inside)."
  [p]
  (let [seen (.-sensedAt p)]
    (doto p (aset "sensedAt" (fn [a] (if (> (.-y a) 66) #js {:unknown true} (.call seen p a)))))))

(deftest a-body-buried-in-a-closed-hut-places-its-carried-bed
  (let [{:keys [eng p]} (setup (hut-night-world {} {:inventory [{:name "red_bed" :count 1}]}))
        _ (blind-above p)]
    (is (true? (sh/night-holds? p (mem/view (:store eng)) {} (constantly true))))))

(deftest a-body-buried-in-a-closed-hut-uses-a-bed-in-the-same-hut
  (let [{:keys [eng p]} (setup (hut-night-world {"6,64,-1" "red_bed"} {}))
        _ (blind-above p)]
    (st/know-bed! eng {:x 6 :y 64 :z -1})
    (is (= {:x 6 :y 64 :z -1} (sh/bed-to-use p (mem/view (:store eng)) 64 (constantly true))))))
