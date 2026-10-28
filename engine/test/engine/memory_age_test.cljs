(ns engine.memory-age-test
  "jobs.lib.memory-age: the chance a remembered cell changed, its levels, and the epoch that moves only when a level does."
  (:require [cljs.test :refer [deftest is are]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.lib.memory-age :as memory-age]
            [jobs.lib.util :as u]))

(defn rig
  "A body at [0 64 0] facing south with blocks, its perception's clock at 0."
  [blocks]
  (let [clock (atom 0)
        p (tu/blind (tu/fake {:blocks blocks}))
        per (perception/create (fake-raw/create p) {:now #(deref clock) :rand (constantly 0.5)})]
    {:p p :per per :st (:st per) :clock clock :w (perception/wrap p per)}))

(deftest a-cell-is-likelier-changed-than-not-at-its-half-age
  (doseq [k [memory-age/FLUID memory-age/DOOR memory-age/FIRE]]
    (is (zero? (memory-age/changed-chance k 0)))
    (is (< (memory-age/changed-chance k (dec (memory-age/half-age-ms k))) 0.5 (memory-age/changed-chance k (inc (memory-age/half-age-ms k))))))
  (is (zero? (memory-age/changed-chance memory-age/NONE 1e9)) "stone never changes"))

(deftest each-due-age-starts-the-next-level
  ;; past ~300 years (the fluid prior's top levels) one ms moves the chance by less than a double resolves
  (doseq [k [memory-age/FLUID memory-age/DOOR memory-age/FIRE] l (range (dec memory-age/TOP))
          :let [due (memory-age/due-age-ms k l)] :when (< due 1e13)]
    (let [at #(memory-age/level (memory-age/changed-chance k %))]
      (is (= [l (inc l)] [(at (dec due)) (at due)]) (str "kind " k " level " l)))))

(deftest level-chances-double-to-one-half-then-halve-what-is-left
  (are [l q] (= q (memory-age/level-chance l))
    0 0
    1 (js/Math.pow 2 -10)
    9 0.25
    10 0.5
    11 0.75))

(deftest the-epoch-holds-while-nothing-can-age
  (let [{:keys [per st clock]} (rig {"0,65,4" "gold_block"})]
    (perception/pass! per)
    (let [k (memory-age/epoch st)]
      (swap! clock + 3600000)
      (is (= k (memory-age/epoch st)) "an hour on, nothing that changes by itself: the same key"))))

(deftest the-epoch-moves-only-when-a-doors-chance-moves-a-level-or-it-is-seen-again
  (let [{:keys [per st clock]} (rig {"0,65,4" "oak_door"})
        due #(memory-age/due-age-ms memory-age/DOOR %)]
    (perception/pass! per)
    (let [k (memory-age/epoch st)]
      (reset! clock (dec (due 0)))
      (is (= k (memory-age/epoch st)) "before its first due age: the same key")
      (reset! clock (due 0))
      (is (not= k (memory-age/epoch st)) "at it: moved"))
    (reset! clock (due 3))
    (let [k (memory-age/epoch st)]
      (reset! clock (dec (due 4)))
      (is (= k (memory-age/epoch st)) "between due ages: the same key")
      (perception/pass! per)
      (is (not= k (memory-age/epoch st)) "seen again: back to level 0, moved"))
    (let [k (memory-age/epoch st)
          t @clock]
      (reset! clock (+ t (dec (due 0))))
      (perception/pass! per)
      (is (= k (memory-age/epoch st)) "seen again before its first due age: nothing moved"))))

(deftest a-door-seen-20-s-ago-by-a-still-body-is-remembered-at-a-small-chance-then-read-unknown-once-likelier-changed
  (let [{:keys [p per st clock w]} (rig {"0,65,4" "oak_door"})]
    (perception/pass! per)
    (reset! clock 20000)
    (is (some? (perception/remembered-age st 0 65 4)) "20 s on, between passes: remembered")
    (is (zero? ((memory-age/changed-fn per) 0 65 4)) "in plain view as remembered: seen now")
    (swap! (fake/state p) assoc :yaw 180)
    (is (< 0 ((memory-age/changed-fn per) 0 65 4) 0.05) "out of view: priced at a small chance, not read unseen")
    (is (= "oak_door" (some-> (u/sensed w {:x 0 :y 65 :z 4}) .-name)) "out of view: read as the door")
    (reset! clock (inc (memory-age/half-age-ms memory-age/DOOR)))
    (is (true? (.-unknown (u/sensed w {:x 0 :y 65 :z 4}))) "past its half age: unknown")))

(deftest a-fire-in-plain-view-of-a-still-body-is-priced-as-seen-now-between-its-looks
  (let [{:keys [p per clock]} (rig {"0,64,4" "netherrack" "0,65,4" "fire"})
        still (:still-ms (:opts per))]
    (perception/pass! per)
    (reset! clock (dec still))
    (is (< (memory-age/half-age-ms memory-age/FIRE) still) "the fire is likelier out than not before the next look")
    (is (zero? ((memory-age/changed-fn per) 0 65 4)) "in view: seen now")
    (swap! (fake/state p) assoc :yaw 180)
    (is (<= 0.5 ((memory-age/changed-fn per) 0 65 4)) "turned away: its age")))
