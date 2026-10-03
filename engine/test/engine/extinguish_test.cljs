(ns engine.extinguish-test
  "jobs.survival.extinguish and the burning trigger against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.extinguish :as extinguish]))

(defn setup [world]
  (let [clock (atom 1000000)
        [_ sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p}))

(defn floor
  "Stone at y 63 for every x and z within n of the origin."
  [n]
  (into {} (for [x (range (- n) (inc n)) z (range (- n) (inc n))] [(str x ",63," z) "stone"])))

(defn cells [name ys xs zs]
  (into {} (for [y ys x xs z zs] [(str x "," y "," z) name])))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [c] (js->clj (.-args c) :keywordize-keys true))

(defn pos-now [p] (js->clj (.-pos (.self p)) :keywordize-keys true))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

;; ------------------------------------------------------------------- check

(deftest check-is-true-on-fire-or-in-lava-and-false-otherwise
  (doseq [[self expected] [[{:onFire true} true]
                           [{:inLava true} true]
                           [{:onFire true :inLava true} true]
                           [{:inWater true} false]
                           [{} false]]]
    (is (= expected (extinguish/check {:primitives (tu/fake {:self self})})) (pr-str self))))

(deftest burning-trigger-holds-on-fire-or-in-lava
  (doseq [[self expected] [[{:onFire true} true] [{:inLava true} true] [{} false]]]
    (is (= expected ((:when (:burning triggers/all)) (tu/fake {:self self}) nil {})) (pr-str self))))

;; ------------------------------------------------------------------- rounds

(deftest round-is-done-without-acting-when-clear
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {})]
          (is (= :done (await (extinguish/round {:primitives p :args {}}))))
          (is (= [] (vec (.-calls (.-world p))))))))))

(deftest heads-for-water-when-it-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 8) {"3,64,0" "water" "0,64,40" "water"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [{:x 3 :y 64 :z 0}] (mapv (comp :pos call-args) (calls p "moveTo"))))
          (is (not (.-onFire (.self p))))
          (await (run-until-empty eng 3))
          (is (= [] (:list (core/state eng))) "done once the fire is out")
          (is (= [{:pos {:x 0 :y 64 :z 0} :cause :fire}] (entries eng :extinguish))))))))

(deftest water-beyond-the-radius-is-ignored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 12) {"9,64,0" "water" "1,64,0" "fire"})})]
          (core/submit! eng '(jobs.survival.extinguish {:water-radius 6}) {})
          (await (core/tick! eng))
          (let [target (:pos (call-args (first (calls p "moveTo"))))]
            (is (not= {:x 9 :y 64 :z 0} target))
            (is (<= (Math/hypot (:x target) (:z target)) 6) "the walk is short")
            (is (= "stone" (.-name (.blockAt p (clj->js (update target :y dec))))) "onto a solid block")))))))

(deftest fire-blocks-are-not-a-place-to-stand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 8) {"0,63,0" "magma_block" "1,64,0" "fire"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (let [target (:pos (call-args (first (calls p "moveTo"))))
                under (.-name (.blockAt p (clj->js (update target :y dec))))]
            (is (= "stone" under))
            (is (not= {:x 1 :y 64 :z 0} target))))))))

(deftest moves-out-of-lava-preferring-higher-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inLava true}
                                      :blocks (merge (floor 6)
                                                     (cells "lava" [64] [-1 0 1] [-1 0 1])
                                                     {"2,64,0" "stone"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= {:x 2 :y 65 :z 0} (pos-now p)) "up onto the ledge, out of the pool")
          (is (not (.-inLava (.self p))))
          (is (some #{{:kind :lava :pos {:x -1 :y 64 :z -1}}} (entries eng :hazard)) "the pool is remembered as hazards")
          (is (= 9 (count (entries eng :hazard))))
          (is (= [{:pos {:x 0 :y 64 :z 0} :cause :lava}] (entries eng :extinguish)))
          (is (= {:cap 20 :ttl 3600000} (select-keys (mem/policy (mem/view (:store eng)) :extinguish) [:cap :ttl])))
          (is (= {:cap 50 :ttl 21600000} (select-keys (mem/policy (mem/view (:store eng)) :hazard) [:cap :ttl]))))))))

(deftest hazards-are-not-written-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inLava true}
                                      :blocks (merge (floor 6) (cells "lava" [64] [-1 0 1] [-1 0 1]))})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "blocked"}))
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= 9 (count (entries eng :hazard)))))))))

;; ------------------------------------------------------------------- bucket

(deftest uses-a-water-bucket-at-the-feet-when-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :inventory [{:name "water_bucket" :count 1}]
                                      :blocks (floor 6)})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [{:pos {:x 0 :y 64 :z 0} :item "water_bucket"}] (mapv call-args (calls p "place"))))
          (is (= [] (calls p "moveTo")) "no walking needed")
          (is (not (.-onFire (.self p))))
          (await (run-until-empty eng 3))
          (is (= [] (:list (core/state eng)))))))))

(deftest no-safe-cell-gives-up-after-three-rounds-with-one-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/capture-sink)
              p (tu/fake {:self {:onFire true} :blocks {"0,63,0" "stone" "1,64,0" "fire"}})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (filter #(= :extinguish_stuck (:kind %)) @seen)))))))))

(deftest does-not-pour-water-into-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inLava true}
                                      :inventory [{:name "water_bucket" :count 1}]
                                      :blocks (merge (floor 6) (cells "lava" [64] [0] [0]))})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (calls p "place")))
          (is (= 1 (count (calls p "moveTo")))))))))

(deftest burning-on-bare-stone-with-no-water-or-hazards-stands-still
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/capture-sink)
              p (tu/fake {:self {:onFire true} :blocks (floor 8)})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (calls p "moveTo")) "does not walk")
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (filter #(= :extinguish_wait (:kind %)) @seen))))
          (is (not-any? #(= :warn (:level %)) @seen)))))))

(deftest burning-next-to-a-fire-block-still-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 8) {"1,64,0" "fire"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "moveTo")))))))))
