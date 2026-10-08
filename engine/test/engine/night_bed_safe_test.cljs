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
  "{:raw :p}: a fake body and its perception wrap, so the raw world's sky light is read (sh/buried?)."
  [world]
  (let [raw-p (tu/fake (dissoc world :light))
        _ (swap! (fake/state raw-p) assoc :light (:light world {}))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})]
    {:raw raw-p :p (perception/wrap raw-p per)}))

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
