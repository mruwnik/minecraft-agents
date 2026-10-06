(ns engine.shelter-leave-test
  "A body sealed in its own dig-in shelter by day (no shelter job running), the hint go-to, attack and hunt give for it,
  and the shelter hold eating carried food."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.fake :as fake]))

(def shut-in-trigger :night)

(defn fires? [world memory]
  (boolean ((:when (get triggers/all shut-in-trigger)) (tu/fake world) memory {})))

(defn shelter-entry [state]
  {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state state})

(defn ^:async dug-in-world
  "A body dug in at night by dig-in submitted directly (no shelter job), then the time set to day; the setup map."
  [world]
  (let [s (st/setup (merge {:time st/night :inventory st/dirt-stack :blocks st/floor} world))]
    (core/submit! (:eng s) '(jobs.survival.dig-in) {})
    (await (st/tick-n (:eng s) 25))
    (is (= [:built] (mapv :state (st/entries (:eng s) :shelter))) "dug in")
    (.setTime (.-world (:p s)) st/noon)
    s))

(deftest the-night-trigger-holds-a-body-in-its-recorded-shelter
  (let [{:keys [eng]} (st/setup {})
        write! (fn [e] (mem/write! (:store eng) :shelter e {:cap 10 :ttl st/day-ms}))
        _ (write! (shelter-entry :built))
        view (mem/view (:store eng))]
    (is (true? (fires? {:time st/noon :blocks {"0,66,0" "stone"}} view)) "day, roofed in its own shelter")
    (is (true? (fires? {:time st/night :blocks {"0,66,0" "stone"}} view)) "at night it holds the body in it")
    (is (false? (fires? {:time st/noon} view)) "not shut in: nothing overhead")
    (is (false? (fires? {:time st/noon :blocks {"0,66,0" "stone"} :self {:pos {:x 3 :y 64 :z 0}}} view)) "elsewhere than the shelter")
    (is (false? (fires? {:time st/noon :blocks {"0,66,0" "stone"}} (mem/view (:store (:eng (st/setup {}))))))
        "no shelter entry")))

(deftest the-night-trigger-lets-a-shut-in-body-out
  (let [t (get triggers/all shut-in-trigger)]
    (is (= '(jobs.survival.night) (:job t)))
    (is (= :cooldown (:persistence t)))))

(deftest a-body-sealed-by-a-direct-dig-in-is-let-out-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (dug-in-world {}))]
          (is (= {:x 0 :y 64 :z 0} (st/pos-of p)) "sealed in the cell")
          (core/load-scenario! eng (scenario/parse (str "{:register [{:trigger " shut-in-trigger "}]}")))
          (await (st/tick-n eng 12))
          (is (not= {:x 0 :y 64 :z 0} (st/pos-of p)) "the reflex ran leave!: the body stepped out of its walls")
          (is (empty? (filter :reflex (vals (:instances (core/state eng))))) "and the reflex ended"))))))

(deftest a-trapped-body-is-tried-once-not-every-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (await (dug-in-world {:blocks st/stone-ground :inventory [{:name "dirt" :count 1}]}))]
          (core/load-scenario! eng (scenario/parse (str "{:register [{:trigger " shut-in-trigger "}]}")))
          (dotimes [_ 6]
            (await (st/tick-n eng 10))
            (swap! clock + 11000))
          (is (= 1 (count (st/emitted seen :shelter.failed))) "one failure, not one per cooldown"))))))

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
              u (first (st/emitted seen :unreachable))]
          (is (= {:x 0 :y 64 :z 0} (:inside-own-shelter u)))
          (is (re-find #"shelter" (:hint u))))))))

(deftest go-to-outside-a-shelter-gives-no-shelter-hint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:blocks st/floor :unreachable ["10,64,0"]})
              _ (core/submit! eng '(jobs.movement.go-to {:pos [10 64 0]}) {})
              _ (await (st/tick-n eng 30))
              u (first (st/emitted seen :unreachable))]
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
              g (first (st/emitted seen :attack.gave-up))]
          (is (= {:x 0 :y 64 :z 0} (:inside-own-shelter g))))))))

;; ------------------------------------------------------------------ eating while held

(defn food-of [p] (.-food (.self p)))

(deftest a-held-body-eats-carried-food-when-food-is-low
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time st/night :blocks st/floor :inventory [{:name "bread" :count 3}]
                                         :self {:food 4}})]
          (st/refuse-placing! p)
          (let [mid (st/mid-night! p 3 #(hash-map :ate (< 4 (food-of p)) :listed (count (:list (core/state eng)))))]
            (core/submit! eng '(jobs.survival.night) {})
            (await (st/tick-n eng 12))
            (is (= {:ate true :listed 1} @mid) "exposed hold: it ate, and still holds")))))))

(deftest a-held-body-with-enough-food-does-not-eat
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
              _ (core/submit! eng '(jobs.combat.hunt {:count 3 :keep 0}) {})
              _ (dotimes [_ 300] (swap! clock + 700) (await (core/tick! eng)))
              g (first (st/emitted seen :hunt.gave-up))]
          (is (= {:x 0 :y 64 :z 0} (:inside-own-shelter g))))))))
