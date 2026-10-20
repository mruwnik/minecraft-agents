(ns engine.ore-seen-test
  "The :ore-seen trigger and jobs.gather.pick-ores: ore a mining job passes is dug by the trigger's job, never ore the job
  digs itself or ore not seen."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.world-files :as world]
            [triggers.gather.ore-seen :as ore-seen]))

(def now 1000000)

(def room (merge (tu/box -4 58 -4 4 63 4 "stone") {"2,63,0" "coal_ore"}))

(defn body [blocks]
  (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}] :blocks blocks})))

(defn view [entries]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly now)})]
    (doseq [[kind data] entries] (mem/write! s kind data {:cap 8 :ttl 600000}))
    (mem/view s)))

(def mark {:leave [] :except [] :ignore-zones? true :for-plan nil})

(defn job-mem [m] [(mem/job-kind "j1") (merge {:args {} :children {}} m)])

(defn holds? [blocks entries] (ore-seen/ore-seen (body blocks) (view entries) {} nil #{"j1"}))

(deftest it-holds-for-a-seen-ore-beside-a-marked-job-in-it-or-in-a-child
  (is (true? (holds? room [(job-mem {:ore-mark mark})])))
  (is (true? (holds? room [(job-mem {:children {:dig {:ore-mark mark}}})]))))

(deftest it-does-not-hold-without-a-mining-job
  (is (false? (holds? room [])))
  (is (false? (holds? room [(job-mem {})]))))

(deftest ore-the-job-digs-itself-or-kept-or-left-is-not-held-for
  (are [entries] (false? (holds? room entries))
    [(job-mem {:ore-mark (assoc mark :leave ["coal_ore"])})]
    [(job-mem {:ore-mark (assoc mark :except [[2 63 0]])})]
    [(job-mem {:ore-mark mark}) [:ore-left {:cells [[2 63 0]]}]]))

(deftest an-ore-with-no-air-face-or-out-of-sight-is-not-held-for
  (is (false? (holds? (assoc room "2,64,0" "stone" "1,64,0" "stone" "3,64,0" "stone" "2,64,1" "stone" "2,64,-1" "stone") [(job-mem {:ore-mark mark})])))
  (is (false? (ore-seen/ore-seen (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}] :blocks room})
                                 (view [(job-mem {:ore-mark mark})]) {} nil #{"j1"}))
      "not seen"))

(deftest marks-of-several-jobs-merge-to-what-none-of-them-lets-go
  (let [ms [{:leave ["coal_ore"] :except [[1 2 3]] :ignore-zones? true} {:leave ["iron_ore"] :except [] :ignore-zones? false}]]
    (is (= {:leave #{"coal_ore" "iron_ore"} :except #{[1 2 3]} :ignore-zones? false :for-plan nil} (ore-pick/merged ms)))
    (is (false? (:ignore-zones? (ore-pick/merged []))))))

(defn setup [blocks]
  (let [clock (atom now)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}] :blocks blocks}))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :backoff false :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))

(deftest the-job-digs-every-seen-ore-in-reach-when-nothing-is-marked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc room "-2,63,0" "iron_ore"))
              r (await (tu/child-outcome eng 'jobs.gather.pick-ores {} 60))]
          (is (= :done (:status r)) (pr-str r))
          (is (= ["air" "air"] [(block-at p [2 63 0]) (block-at p [-2 63 0])])))))))

(deftest a-job-marks-what-it-digs-itself-where-the-trigger-and-the-job-read-it
  (let [mem* (atom {})
        c {:args {:ignore-zones? true :for-plan "p1"} :update-mem (fn [f more] (apply swap! mem* f more)) :view (fn [] nil)}]
    (with-redefs [ctx/mem (fn [_] @mem*)]
      (ore-pick/mark! c {:leave #{"coal_ore"} :except [[1 2 3]]}))
    (is (= {:leave ["coal_ore"] :except [[1 2 3]] :ignore-zones? true :for-plan "p1"} (:ore-mark @mem*)))))
