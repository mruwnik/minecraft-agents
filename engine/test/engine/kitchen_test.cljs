(ns engine.kitchen-test
  "jobs.items.kitchen against the fake world: every carried raw food smelted, wheat baked, the cooked food put away."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def job 'jobs.items.kitchen)
(def furnace {:x 1 :y 64 :z 0})
(def chest {:x 10 :y 64 :z 0})
(def kitchen {:blocks {"1,64,0" "furnace" "11,64,0" "crafting_table"}})

(defn item [name n] {:name name :count n})

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (merge-with merge kitchen world)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run
  "Submit form over the world and tick n times, the furnaces cooking and the clock moving 40 s between ticks."
  [form world n]
  (let [{:keys [eng p clock] :as s} (setup world)]
    (core/submit! eng form {})
    (dotimes [_ n]
      (await (core/tick! eng))
      (.advance (.-world p) 700)
      (swap! clock + 40000))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn chest-items [{:keys [p]}] (into {} (map (juxt :name :count)) (get-in @(fake/state p) [:containers [10 64 0]])))

(def args {:furnace furnace :chest chest :keep {"bread" 1}})
(def raw-food [(item "beef" 3) (item "chicken" 2) (item "coal" 2) (item "wheat" 6)])

(deftest one-pass-smelts-every-raw-food-bakes-the-wheat-and-puts-the-cooked-food-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run (list job args) {:inventory raw-food :containers {"10,64,0" []}} 40))]
          (is (true? (finished? s)))
          (is (= {"cooked_beef" 3 "cooked_chicken" 2} (select-keys (chest-items s) ["cooked_beef" "cooked_chicken"])))
          (is (pos? (get (chest-items s) "bread" 0)))
          (is (nil? (get (inv s) "beef")))
          (is (nil? (get (inv s) "chicken")))
          (is (nil? (get (inv s) "wheat"))))))))

(defn waits [s] (distinct (map (juxt :reason :backed-off) (events-of s :waiting))))

(defn run-with [inventory]
  (run (list job (dissoc args :keep)) {:inventory inventory :containers {"10,64,0" []}} 2))

(deftest the-check-passes-for-raw-food-or-three-wheat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [inventory [[(item "beef" 1)] [(item "wheat" 3)]]]
          (let [s (await (run-with inventory))]
            (is (seq (events-of s :round_started)) (pr-str inventory))))))))

(deftest the-check-waits-for-less-than-three-wheat-or-only-cooked-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [inventory [[(item "wheat" 2) (item "cooked_beef" 4) (item "bread" 5)] [(item "potato" 4)]]]
          (let [s (await (run-with inventory))]
            (is (empty? (events-of s :round_started)) (pr-str inventory))
            (is (= [[:nothing-to-do nil]] (waits s)) (pr-str inventory))))))))

(deftest a-pass-that-leaves-raw-food-carried-rests-so-a-standing-job-does-not-starve-the-queue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run (list 'repeat (list job (assoc args :furnace {:x 30 :y 64 :z 0}))) {:inventory [(item "beef" 3) (item "coal" 1)]} 8))]
          (is (= 1 (count (events-of s :kitchen.done))) "one pass, then the kitchen rests")
          (is (some #(= ["kitchen"] (:backed-off %)) (events-of s :waiting)) "the wait says it rests")
          (swap! (:clock s) + 660000)
          (dotimes [_ 6] (await (core/tick! (:eng s))) (swap! (:clock s) + 1000))
          (is (= 2 (count (events-of s :kitchen.done))) "after the rest it tries again"))))))
