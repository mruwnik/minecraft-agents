(ns engine.shelter-leave-test
  "A body sealed in its own dig-in shelter by day (no shelter job running), the hint go-to, attack and hunt give for it,
  and the shelter hold eating carried food."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.fake :as fake]))

(def shut-in-trigger :dawn)

(defn fires? [world memory]
  (boolean ((:when (get triggers/all shut-in-trigger)) (tu/fake world) memory {})))

(defn shelter-entry [state]
  {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state state})

(defn ^:async dug-in-world
  "A body dug in at night by dig-in submitted directly (no shelter job), then the time set to day; the setup map."
  [world]
  (let [s (st/setup (merge {:time st/night :inventory st/wall-stack :blocks st/floor} (dissoc world :ways)))]
    (core/submit! (:eng s) (list 'jobs.survival.shut-in {:ways (:ways world #{:room :walls}) :for :night}) {})
    (await (st/tick-n (:eng s) 25))
    (is (= [:built] (mapv :state (st/entries (:eng s) :shelter))) "dug in")
    (.setTime (.-world (:p s)) st/noon)
    s))

(deftest the-dawn-trigger-fires-by-day-in-its-recorded-shelter
  (let [{:keys [eng]} (st/setup {})
        write! (fn [e] (mem/write! (:store eng) :shelter e {:cap 10 :ttl st/day-ms}))
        _ (write! (shelter-entry :built))
        view (mem/view (:store eng))]
    (is (true? (fires? {:time st/noon :blocks {"0,66,0" "stone"}} view)) "day, roofed in its own shelter")
    (is (false? (fires? {:time st/night :blocks {"0,66,0" "stone"}} view)) "at night a body already in it is safe: the queue runs")
    (is (false? (fires? {:time st/noon} view)) "not shut in: nothing overhead")
    (is (false? (fires? {:time st/noon :blocks {"0,66,0" "stone"} :self {:pos {:x 3 :y 64 :z 0}}} view)) "elsewhere than the shelter")
    (is (false? (fires? {:time st/noon :blocks {"0,66,0" "stone"}} (mem/view (:store (:eng (st/setup {}))))))
        "no shelter entry")))

(deftest a-body-sealed-by-a-direct-dig-in-is-let-out-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (dug-in-world {}))]
          (is (= {:x 0.5 :y 64 :z 0.5} (st/pos-of p)) "sealed in the cell")
          (core/load-scenario! eng (scenario/parse (str "{:register [{:trigger " shut-in-trigger "}]}")))
          (await (st/tick-n eng 12))
          (is (not= {:x 0 :y 64 :z 0} (st/pos-of p)) "the reflex ran leave!: the body stepped out of its walls")
          (is (empty? (filter :reflex (vals (:instances (core/state eng))))) "and the reflex ended")
          (is (false? (fires? {:time st/noon :blocks {"0,66,0" "stone"}} (mem/view (:store eng))))
              "a walk back through the shelter's cell is no new dawn"))))))

(deftest leave-shelter-run-by-night-in-the-shelter-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (dug-in-world {}))
              before (count (st/emitted seen :completed))]
          (.setTime (.-world p) st/night)
          (core/submit! eng '(jobs.survival.leave-shelter) {})
          (await (st/tick-n eng 12))
          (is (= {:x 0.5 :y 64 :z 0.5} (st/pos-of p)) "still sealed in")
          (is (= before (count (st/emitted seen :completed))) "no \"out of the shelter, nothing left\" while shut in"))))))

(deftest a-trapped-body-is-tried-once-not-every-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock p]} (await (dug-in-world {:blocks st/stone-ground :inventory [{:name "dirt" :count 1}] :ways #{:room :walls :pit}}))]
          (doseq [x (range -3 4) z (range -3 4) y (range 62 68) :when (not= [0 0] [x z])]
            (fake/set-block! p [x y z] "bedrock"))
          (core/load-scenario! eng (scenario/parse (str "{:register [{:trigger " shut-in-trigger "}]}")))
          (dotimes [_ 6]
            (await (st/tick-n eng 10))
            (swap! clock + 11000))
          (is (= 1 (count (st/emitted seen :stopped))) "one failure, not one per cooldown"))))))

;; ------------------------------------------------------------------ the hint

(defn sealed-world [more]
  (merge {:blocks (merge st/floor {"0,66,0" "stone"}) :unreachable ["10,64,0"]} more))

(deftest go-to-from-inside-its-own-shelter-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup (sealed-world {}))
              _ (mem/write! (:store eng) :shelter (shelter-entry :built) {:cap 10 :ttl st/day-ms})
              _ (core/submit! eng '(jobs.movement.go-to {:pos [10 64 0]}) {})
              _ (await (st/tick-n eng 30))
              u (first (st/emitted seen :stopped))]
          (is (= {:x 0 :y 64 :z 0} (:inside-own-shelter u)))
          (is (re-find #"shelter" (:hint u))))))))

(deftest go-to-outside-a-shelter-gives-no-shelter-hint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:blocks st/floor :unreachable ["10,64,0"]})
              _ (core/submit! eng '(jobs.movement.go-to {:pos [10 64 0]}) {})
              _ (await (st/tick-n eng 30))
              u (first (st/emitted seen :stopped))]
          (is (some? u))
          (is (nil? (:inside-own-shelter u))))))))

(deftest attack-gives-up-with-the-shelter-hint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup (sealed-world {:inventory h/sword :unreachable ["10,64,0"]
                                                          :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 10 :y 64 :z 0}}]}))
              _ (mem/write! (:store eng) :shelter (shelter-entry :built) {:cap 10 :ttl st/day-ms})
              _ (core/submit! eng '(jobs.combat.attack {:targets [7]}) {})
              _ (await (st/tick-n eng 30))
              g (first (st/emitted seen :stopped))]
          (is (= {:x 0 :y 64 :z 0} (:inside-own-shelter g))))))))

;; ------------------------------------------------------------------ eating while held

(defn food-of [p] (.-food (.self p)))

(deftest an-exposed-body-with-enough-food-does-not-eat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time st/night :blocks st/floor :inventory [{:name "bread" :count 3}]
                                         :self {:food 20}})]
          (st/refuse-placing! p)
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (= [] (st/calls p "eat"))))))))

(deftest hunt-gives-up-with-the-shelter-hint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cow (fn [id x] {:id id :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}})
              {:keys [eng seen clock]} (st/setup (sealed-world {:inventory h/sword :unreachable ["10,64,0" "11,64,0" "12,64,0"]
                                                          :entities [(cow 1 10) (cow 2 11) (cow 3 12)]}))
              _ (mem/write! (:store eng) :shelter (shelter-entry :built) {:cap 10 :ttl st/day-ms})
              _ (core/submit! eng '(jobs.combat.hunt {:item "beef" :count 3 :keep 0}) {})
              _ (await (tu/run-ticks eng clock 300 700))
              g (first (st/emitted seen :stopped))]
          (is (= {:x 0 :y 64 :z 0} (:inside-own-shelter g))))))))

;; ------------------------------------------------------------------ dawn exit

(def first-light 200)
(def grace-passed 7000)
(def far-zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 12 :y 64 :z 2}})

(defn ^:async held-in-then-out
  "A dug-in body at first light with the given mobs near, the dawn trigger registered, the dawn grace over at the
  fourth hold: [whether the body was still sealed in at the third hold (nil: no hold made), whether it ended outside]."
  [entities]
  (let [{:keys [eng p]} (await (dug-in-world {:entities entities}))
        sealed {:x 0.5 :y 64 :z 0.5}
        third (atom nil)]
    (st/on-wait! p (fn [n]
                     (when (= n 3) (reset! third (= sealed (st/pos-of p))))
                     (when (= n 4) (.setTime (.-world p) grace-passed))))
    (.setTime (.-world p) first-light)
    (core/load-scenario! eng (scenario/parse (str "{:register [{:trigger " shut-in-trigger "}]}")))
    (await (st/tick-n eng 12))
    [@third (not= sealed (st/pos-of p))]))

(deftest a-dug-in-body-waits-at-first-light-for-a-known-hostile-near-until-the-grace-is-over
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [nil true] (await (held-in-then-out []))) "nobody near: out at first light, no hold")
        (is (= [true true] (await (held-in-then-out [far-zombie]))) "a zombie near: held in, out once the grace is over")))))
