(ns engine.seek-contract-test
  "combat.attack and explore.search as contract jobs: they export the contract, quote validly, and end with a kind and a
  cause."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [engine.explore-search-test :as es]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.combat.attack :as attack]
            [jobs.explore.search :as search]
            [jobs.lib.world-files :as world]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [[sym args] [['jobs.combat.attack {:targets ["zombie"]}]
                      ['jobs.explore.search {:target "diamond_block"}]]]
    (is (some some? ((juxt :can-start? :can-run?) (job sym))) (str sym " has checks"))
    (is (nil? (:check (job sym))) (str sym " has no old check"))
    (is (vector? (:target (job sym))) (str sym " has a target"))
    (is (some? (:done? (job sym))) (str sym " has done?"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args args :quote-call {:spend {:job-time 60}}}))) (str sym))))

(def flight-world {:floor h/flight-floor})

(defn ^:async ending-of
  "Submit spec over a fake world and tick until the job ends: {:event :completed|:stopped :kind :reason}, or {:event :waiting :reason} for a job still waiting on a condition."
  [make-prims spec]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (make-prims)
        now (tu/act-clock clock p 20)
        eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir) :now now
                          :world (world/of-data {} {} [])
                          :events (events/make {:sinks [sink] :now now})})]
    (core/submit! eng spec {})
    (loop [i 0]
      (when (and (< i 80) (not-any? #(#{:completed :stopped} (:kind %)) @seen))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))
          w (last (filter #(= :waiting (:kind %)) @seen))]
      (core/shutdown! eng)
      (if e
        (assoc (select-keys (:data e) [:kind :reason]) :event (:kind e))
        {:event (when w :waiting) :reason (:reason (:data w))}))))

(defn zed [id x & [more]] (merge {:id id :name "zombie" :kind "hostile" :pos {:x x :y 64 :z 0}} more))

(defn ^:async fight
  [args world]
  (await (ending-of #(tu/fake-on-floor (merge flight-world world)) (list 'jobs.combat.attack args))))

(deftest a-cleared-fight-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (fight {:targets [7]} {:inventory h/sword :entities [(zed 7 2)]})) [:event])))))))

(deftest a-target-given-up-on-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :nothing-possible :reason :gave-up}
               (await (fight {:targets [7] :no-damage-hits 2}
                             {:inventory h/sword :entities [(zed 7 2 {:invulnerable true})]}))))))))

(deftest no-target-at-all-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :condition-failed :reason :absent}
               (await (fight {:targets ["skeleton"]} {:inventory h/sword :entities [(zed 7 2)]}))))))))

(deftest a-fight-past-its-time-is-limit-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :limit-reached :reason :job-time}
               (await (fight {:targets [7] :spend {:job-time 3} :no-damage-hits 100 :settings {:jobs.combat.attack/max-hits 1000}}
                             {:inventory h/sword :entities [(zed 7 2 {:invulnerable true})]}))))))))

(defn ^:async equipped
  "The item the first equip of a fight names."
  [args inventory]
  (let [p (tu/fake-on-floor (merge flight-world {:inventory inventory :entities [(zed 7 2)]}))
        clock (atom 1000000)
        now (tu/act-clock clock p 20)
        eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir) :now now :world (world/of-data {} {} [])
                          :events (events/make {:sinks [(fn [_])] :now now})})]
    (core/submit! eng (list 'jobs.combat.attack args) {})
    (dotimes [_ 3] (swap! clock + 700) (await (core/tick! eng)))
    (core/shutdown! eng)
    (some-> (first (h/calls p "equip")) .-args .-item)))

(def two-swords [{:name "iron_sword" :count 1} {:name "diamond_sword" :count 1}])

(deftest the-best-weapon-is-held-unless-the-call-lists-others
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= "diamond_sword" (await (equipped {:targets [7]} two-swords))))
        (is (= "iron_sword" (await (equipped {:targets [7] :use {:tools ["iron_sword"]}} two-swords))))
        (is (= "iron_sword" (await (equipped {:targets [7] :use {:tools ["iron_"]}} two-swords))))))))

;; ------------------------------------------------------------------ explore.search

(defn ^:async seek
  [args world & [n]]
  (let [s (es/setup world (some-> (:max-distance args) (+ 8)))
        clock (:clock s)
        [seen sink] (tu/capture-sink)
        eng (core/create {:primitives (:p s) :jobs registry/jobs :dir (tu/tmp-dir) :now #(deref clock)
                          :world (world/of-data {} {} [])
                          :events (events/make {:sinks [sink] :now #(deref clock)})})]
    (core/submit! eng (list 'jobs.explore.search args) {})
    (loop [i 0]
      (when (and (< i (or n 400)) (not-any? #(#{:completed :stopped} (:kind %)) @seen))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))]
      (core/shutdown! eng)
      (assoc (select-keys (:data e) [:kind :reason]) :event (:kind e)))))

(deftest a-found-target-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (seek {:target "diamond_block"} {:blocks {"3,64,0" "diamond_block"}})) [:event])))))))

(deftest a-search-with-nothing-within-the-distance-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :nothing-possible :reason :distance}
               (await (seek {:target "diamond_block" :max-distance 20} {}))))
        (is (= {:event :stopped :kind :nothing-possible :reason :legs}
               (await (seek {:target "diamond_block" :settings {:jobs.explore.search/max-legs 2}} {}))))))))
