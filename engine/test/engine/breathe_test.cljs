(ns engine.breathe-test
  "The suffocating trigger and the breathe job against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def breathe 'jobs.survival.breathe)

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn holds? [p args]
  ((:when (get triggers/all :suffocating)) p nil args))

(defn check? [p args]
  ((:check (get registry/jobs breathe)) {:primitives p :args args}))

(def defaults {:min-oxygen 12})

(def water-column
  "Feet at y 64 in water up to y 67; air from y 68."
  {"0,64,0" "water" "0,65,0" "water" "0,66,0" "water" "0,67,0" "water"})

(def walls
  "Stone around the water column, except the +x side, so only air above is open."
  (into {} (for [y (range 64 68) [x z] [[-1 0] [0 1] [0 -1]]] [(str x "," y "," z) "stone"])))

(def cases
  [["drowning: low oxygen, head under water"
    {:self {:inWater true :oxygen 5} :blocks water-column} true]
   ["swimming at the surface with low oxygen: head in air"
    {:self {:inWater true :oxygen 5} :blocks {"0,64,0" "water"}} false]
   ["in water with plenty of oxygen"
    {:self {:inWater true :oxygen 18} :blocks water-column} false]
   ["low oxygen but not in water"
    {:self {:inWater false :oxygen 5}} false]
   ["head inside stone"
    {:blocks {"0,65,0" "stone"}} true]
   ["head inside gravel"
    {:blocks {"0,65,0" "gravel"}} true]
   ["head in a torch"
    {:blocks {"0,65,0" "wall_torch"}} false]
   ["head in tall grass"
    {:blocks {"0,65,0" "tall_grass"}} false]
   ["head in air"
    {} false]])

(deftest trigger-and-check-agree-on-every-branch
  (doseq [[label world expected] cases]
    (let [p (tu/fake world)]
      (is (= expected (holds? p defaults)) (str "trigger: " label))
      (is (= expected (check? p defaults)) (str "check: " label)))))

(deftest min-oxygen-is-an-arg
  (let [p (tu/fake {:self {:inWater true :oxygen 15} :blocks water-column})]
    (is (false? (holds? p defaults)))
    (is (true? (holds? p {:min-oxygen 16})))
    (is (true? (check? p {:min-oxygen 16})))))

(deftest suffocating-is-registered-with-breathe
  (let [t (get triggers/all :suffocating)]
    (is (= '(jobs.survival.breathe) (:job t)))
    (is (= 12 (:min-oxygen (:args t))))))

(deftest drowning-round-swims-up-to-the-surface-and-finishes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p)) "feet at the top water block, head in air")
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "done once no shore is near"))))))

(defn call-names [p] (mapv #(.-name %) (array-seq (.. p -world -calls))))

(deftest drowning-in-an-open-column-swims-and-does-not-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p)))
          (is (= [] (:list (core/state eng))))
          (is (= ["swim"] (call-names p))))))))

(deftest drowning-with-a-failing-swim-gives-up-with-one-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:inWater true :oxygen 4} :swimFails true :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (filter #(= :no_air (:kind %)) @seen)))))))))

(def side-args (assoc defaults :radius 1))

(deftest drowning-round-swims-sideways-when-the-column-is-capped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:inWater true :oxygen 4}
                     :blocks (merge water-column walls
                                    {"0,68,0" "stone"
                                     "1,64,0" "water" "1,65,0" "water" "1,66,0" "water" "1,67,0" "water"})}
              {:keys [eng p]} (setup world)]
          (core/submit! eng (list breathe side-args) {})
          (await (core/tick! eng))
          (is (= {:x 1 :y 64 :z 0} (core/self-pos p)) "first round: sideways at feet height")
          (await (core/tick! eng))
          (is (= {:x 1 :y 67 :z 0} (core/self-pos p)) "second round: up the adjacent column")
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest drowning-with-no-air-in-reach-gives-up-after-bounded-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:inWater true :oxygen 4}
                                         :blocks (merge water-column walls {"0,68,0" "stone" "1,64,0" "stone"})})]
          (core/submit! eng (list breathe side-args) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (some #(= :no_air (:kind %)) @seen)))))))

(deftest enclosed-round-digs-the-head-block-then-the-one-above-and-steps-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "stone" "0,66,0" "stone"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= "air" (.-name (.blockAt p (tu/pos 0 65 0)))) "head block dug")
          (is (= "air" (.-name (.blockAt p (tu/pos 0 66 0)))) "block above dug")
          (is (= {:x 0 :y 65 :z 0} (core/self-pos p)) "stepped up into the shaft")
          (is (= [] (:list (core/state eng)))))))))

(deftest enclosed-with-a-dig-that-times-out-gives-up-without-moving
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"0,65,0" "obsidian"}})]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "timeout"}))
          (core/submit! eng (list breathe defaults) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (filter #(= :no_way_out (:kind %)) @seen))))
          (is (not (some #{"moveTo"} (call-names p)))))))))

(deftest enclosed-round-leaves-the-block-above-when-it-is-already-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "dirt"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= ["dig" "moveTo"] (mapv #(.-name %) (array-seq (.. p -world -calls)))) "no second dig"))))))

(deftest a-clear-body-declines-and-its-round-is-done-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})
              round (:round (get registry/jobs breathe))]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= :done (await (round {:primitives p :args defaults}))))
          (is (zero? (count (.. p -world -calls)))))))))

(deftest an-enclosed-round-remembers-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks {"0,65,0" "stone"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= :enclosed (:why (:data (mem/latest (mem/view (:store eng)) :breathe))))))))))

(deftest a-round-writes-one-breathe-memory-entry-with-where-and-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (let [view (mem/view (:store eng))
                entry (mem/latest view :breathe)]
            (is (= 1 (count (mem/entries view :breathe))))
            (is (= :drowning (:why (:data entry))))
            (is (= {:x 0 :y 64 :z 0} (:pos (:data entry))) "where it started")
            (is (= {:cap 20 :ttl (* 60 60 1000)} (mem/policy view :breathe)))))))))

(def pool
  "Water at y 64 and 65 around the origin column; air above."
  (into {} (for [y [64 65] x [-1 0 1] z [-1 0 1]] [(str x "," y "," z) "water"])))

(deftest surfaced-in-a-pool-it-walks-onto-a-ledge-two-blocks-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4}
                                      :blocks (merge pool {"2,64,0" "stone" "2,63,0" "stone"})})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= {:x 0 :y 65 :z 0} (core/self-pos p)) "first round surfaces")
          (is (= 1 (count (:list (core/state eng)))) "still listed: the body is in water")
          (await (core/tick! eng))
          (is (= {:x 2 :y 65 :z 0} (core/self-pos p)) "second round stands on the ledge")
          (is (not (.-inWater (.self p))))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest surfaced-with-the-rim-two-blocks-above-the-feet-it-still-finds-the-ledge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4}
                                      :blocks (merge pool {"2,64,0" "stone" "2,65,0" "stone" "2,66,0" "stone"})})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= {:x 0 :y 65 :z 0} (core/self-pos p)) "the swim surfaced at the top water block")
          (await (core/tick! eng))
          (is (= {:x 2 :y 67 :z 0} (core/self-pos p)) "stands on the ledge, feet two above the own")
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest surfaced-in-open-water-with-no-shore-declines-without-a-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (filter #(= :no_shore_near (:kind %)) @seen))))
          (is (not-any? #(= :warn (:level %)) @seen))
          (is (= ["swim"] (call-names p))))))))

(deftest enclosed-with-a-free-neighbour-steps-sideways-without-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "obsidian" "1,63,0" "stone"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= {:x 1 :y 64 :z 0} (core/self-pos p)) "stepped to the free neighbour")
          (is (= ["moveTo"] (call-names p)) "no dig")
          (is (= [] (:list (core/state eng)))))))))

(deftest enclosed-with-a-failed-side-step-digs-the-next-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "dirt" "1,63,0" "stone"}})]
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "blocked"}))
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= ["moveTo"] (call-names p)) "first round only tries the step")
          (is (= 1 (count (:list (core/state eng)))) "still listed, no failure counted")
          (await (core/tick! eng))
          (is (= ["moveTo" "dig" "moveTo"] (call-names p)) "second round digs"))))))
