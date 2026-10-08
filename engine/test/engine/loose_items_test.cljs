(ns engine.loose-items-test
  "The :loose-items trigger and its job jobs.items.pick-up-loose: seen items worth the detour are picked up; the
  ones thrown, owed to a live job, in another's zone or not worth it are left."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.loose :as loose]
            [jobs.lib.world-files :as ew]
            [triggers.items.loose-items :as li]))

(def now 1000000)

(defn item
  "An item entity of name at [x y z], seen, with extra fields."
  ([id name pos] (item id name pos {}))
  ([id name pos extra] (merge {:id id :name "item" :kind "item" :pos pos :item {:name name :count 1} :visible true} extra)))

(def log-near (item 11 "oak_log" [4.5 64 0.5]))
(def dirt-down (item 12 "dirt" [2.5 57 0.5]))

(defn world [& entities] {:floor [-10 -10 10 10] :entities (vec entities)})

(defn view
  "A memory view at now holding the entries [[kind data] ...]."
  [entries]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly now)})]
    (doseq [[kind data] entries] (mem/write! s kind data {:cap 8 :ttl 600000}))
    (mem/view s)))

(defn holds?
  ([w] (holds? w {}))
  ([w {:keys [args memory live zones] :or {args {} memory [] live #{}}}]
   (li/loose-items (tu/fake-on-floor w) (view memory) args (ew/of-data {} {} zones) live)))

(deftest a-seen-log-a-few-blocks-off-holds
  (is (true? (holds? (world log-near)))))

(deftest dirt-down-a-drop-is-not-worth-it
  (is (false? (holds? (world dirt-down))))
  (is (true? (holds? (world dirt-down) {:args {:always ["dirt"]}})) ":always takes it whatever the ratio")
  (is (false? (holds? (world log-near) {:args {:never ["oak_log"]}})) ":never leaves it"))

(deftest only-seen-items-within-the-radius-count
  (is (false? (holds? (world (assoc log-near :visible false)))) "hidden behind a wall")
  (is (false? (holds? (world log-near) {:args {:radius 3}}))))

(deftest a-nearly-full-inventory-holds-nothing
  (is (false? (holds? (assoc (world log-near) :inventory (vec (for [i (range 34)] {:name "dirt" :count 64 :slot (+ 9 i)})))))))

(deftest a-danger-near-holds-nothing
  (is (false? (holds? (update (world log-near) :entities conj {:id 7 :name "zombie" :kind "hostile" :pos [-3 64 0]})))))

(deftest thrown-items-are-left
  (is (false? (holds? (world (assoc log-near :thrown-by "Fake" :spawned-at (- now 600000))))) "its own toss, ever")
  (is (false? (holds? (world (assoc log-near :thrown-by "Ann" :spawned-at (- now 10000))))) "a player's toss, for a while")
  (is (true? (holds? (world (assoc log-near :thrown-by "Ann" :spawned-at (- now 70000))))) "a player's toss, after :thrown-s"))

(deftest another-bodys-zone-or-claim-is-left
  (let [zone (fn [owner] [{:name "yard" :owner owner :min [0 60 -3] :max [8 70 3]}])]
    (is (false? (holds? (world log-near) {:zones (zone "Miles")})))
    (is (true? (holds? (world log-near) {:zones (zone "Fake")})))
    (is (true? (holds? (world log-near) {:zones nil})) "no zone list never blocks a pick-up")))

(deftest items-a-live-job-collects-are-left
  (let [hold [:collecting {:job "j1" :anchor {:x 3 :y 64 :z 0} :radius 4}]]
    (is (false? (holds? (world log-near) {:memory [hold] :live #{"j1"}})) "inside a running collect-drops' area")
    (is (true? (holds? (world log-near) {:memory [hold] :live #{}})) "its job ended"))
  (let [fresh (assoc log-near :spawned-at (- now 5000))]
    (is (false? (holds? (world fresh) {:live #{"j1"}})) "just dropped while a job runs: that job's to collect")
    (is (true? (holds? (world fresh) {:live #{}})) "just dropped with no job running")
    (is (true? (holds? (world (assoc log-near :spawned-at (- now 200000))) {:live #{"j1"}})) "lying past :owed-s")))

(deftest items-a-run-left-are-not-tried-again
  (is (false? (holds? (world log-near) {:memory [[:loose-left {:ids [11]}]]}))))

;; ------------------------------------------------------------------ the job and the register

(defn setup [w]
  (let [clock (atom now)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (merge {:offlineScale 0.0001} w)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :backoff false :world (ew/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn carried [p n] (reduce + 0 (keep #(when (= n (:name %)) (:count %)) (:inventory @(fake/state p)))))

(deftest loose-items-is-a-default-reflex-below-the-survival-ones
  (let [order (vec triggers/order)
        at #(.indexOf order %)]
    (is (= :loose-items (peek order)))
    (doseq [id [:suffocating :burning :wedged :hostile-near :night :hungry :inventory-nearly-full :died]]
      (is (< (at id) (at :loose-items)) (str id)))))

(deftest the-reflex-picks-up-the-log-says-so-once-and-leaves-the-dirt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (world log-near dirt-down))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :loose-items}]}"))
          (await (tu/run-until-empty eng 1))
          (dotimes [_ 30] (await (core/tick! eng)))
          (is (= 1 (carried p "oak_log")))
          (is (= 0 (carried p "dirt")))
          (let [picked (filterv #(= :loose-items.picked (:kind %)) @seen)]
            (is (= 1 (count picked)))
            (is (= :info (:level (first picked))))
            (is (= 1 (:collected (first picked))))))))))

(deftest the-job-alone-declines-when-nothing-is-worth-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world dirt-down))
              r (await (tu/child-outcome eng 'jobs.items.pick-up-loose {} 20))]
          (is (= :not-done r) "declined: no result")
          (is (= 0 (carried p "dirt"))))))))

(deftest collect-drops-holds-its-area-while-it-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world log-near))
              holds (atom nil)
              held #(mapv (comp (juxt :anchor :radius) :data) (mem/entries (mem/view (:store eng)) loose/hold-kind))]
          (.override (.-world p) "collect" (fn [_ _] (reset! holds (held)) #js {:status "failed"}))
          (core/submit! eng '(jobs.forestry.collect-drops {:radius 6 :near {:x 1 :y 64 :z 0}}) {})
          (await (tu/run-until-empty eng 20))
          (is (= [[{:x 1 :y 64 :z 0} 6]] @holds) "held while it collects")
          (is (= [] (held)) "released at the end"))))))
