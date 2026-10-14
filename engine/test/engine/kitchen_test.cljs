(ns engine.kitchen-test
  "jobs.items.kitchen against the fake world: every carried raw food smelted, wheat baked, the cooked food put away."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.withdraw-test :as wt]
            [jobs.items.kitchen :as kitchen]))

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
  "Submit form over the world and tick n times, the furnaces cooking and the clock moving 40 s between ticks.
  stock, when given, is the chest's booked content {name n}."
  [form world n & [stock]]
  (let [{:keys [eng p clock] :as s} (setup world)]
    (when stock
      (mem/write! (:store eng) :fetch/stock {:pos [10 64 0] :items stock} {:cap 64 :ttl (* 6 60 60 1000)}))
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

(defn waits [s] (distinct (map (juxt :reason (comp :backed-off :cause)) (events-of s :waiting))))

(defn run-with [inventory & [stock]]
  (run (list job (dissoc args :keep)) {:inventory inventory :containers {"10,64,0" []}} 2 (or stock {})))

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
          (is (some #(= ["kitchen"] (get-in % [:cause :backed-off])) (events-of s :waiting)) "the wait says it rests")
          (swap! (:clock s) + 660000)
          (dotimes [_ 6] (await (core/tick! (:eng s))) (swap! (:clock s) + 1000))
          (is (= 2 (count (events-of s :kitchen.done))) "after the rest it tries again"))))))

(deftest wheat-in-the-chest-is-baked-though-none-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [stock [nil {"wheat" 6}]]
          (let [s (await (run (list job args) {:inventory [(item "coal" 1)] :containers {"10,64,0" [(item "wheat" 6)]}} 12 stock))]
            (is (true? (finished? s)) (pr-str stock))
            (is (pos? (get (chest-items s) "bread" 0)) (pr-str stock))
            (is (nil? (get (chest-items s) "wheat")) (pr-str stock))))))))

(deftest a-chest-booked-without-wheat-keeps-the-kitchen-waiting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-with [(item "potato" 4)] {"cobblestone" 20}))]
          (is (empty? (events-of s :round_started)))
          (is (= [[:nothing-to-do nil]] (waits s))))))))

(deftest a-rest-after-a-missing-table-ends-when-a-table-is-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock] :as s} (setup {:inventory [(item "wheat" 6)] :containers {"10,64,0" []}})]
          (swap! (fake/state p) fake/drop-block [11 64 0])
          (core/submit! eng (list 'repeat (list job {:chest chest})) {})
          (dotimes [_ 8] (await (core/tick! eng)) (swap! clock + 1000))
          (is (= 1 (count (events-of s :kitchen.done))) "the pass without a table ends and rests")
          (swap! (fake/state p) fake/put-block [11 64 0] "crafting_table")
          (dotimes [_ 40] (await (core/tick! eng)) (.advance (.-world p) 700) (swap! clock + 1000))
          (is (pos? (get (inv s) "bread" 0)) "baked well inside the 10 minute rest"))))))

(deftest bread-carried-within-the-keep-is-reported-as-kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-with [(item "bread" 4)]))]
          (is (= [{:smelt :no-food :bake :no-wheat :deposit :kept-carried}]
                 (distinct (map (comp :skipped :cause) (events-of s :waiting))))))))))

(defn ^:async run-beside
  "Submit the job wait-for-dusk (listed, check false by day), then form, over the world; tick 3 times."
  [form world]
  (let [{:keys [eng] :as s} (setup world)]
    (core/submit! eng '(jobs.time.wait-for-dusk) {})
    (core/submit! eng form {})
    (dotimes [_ 3] (await (core/tick! eng)))
    s))

(def far-chest {:x 200 :y 64 :z 0})

(deftest a-kitchen-far-from-its-chest-declines-while-another-job-is-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-beside (list job (assoc args :chest far-chest)) {:inventory [(item "beef" 3)] :containers {"200,64,0" []}}))]
          (is (empty? (events-of s :round_started)) "no pass starts")
          (is (some #(= :away (:reason %)) (events-of s :waiting)) "the wait says the body is away"))))))

(deftest a-kitchen-far-from-its-chest-runs-when-nothing-else-is-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run (list job (assoc args :chest far-chest)) {:inventory [(item "beef" 3)] :containers {"200,64,0" []}} 3))]
          (is (seq (events-of s :round_started))))))))

(deftest a-kitchen-near-its-chest-runs-while-another-job-is-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-beside (list job args) {:inventory [(item "beef" 3)] :containers {"10,64,0" []}}))]
          (is (seq (events-of s :round_started))))))))

(def other-far-chest {:x 300 :y 64 :z 0})
(def far-world {:inventory [(item "beef" 3)] :containers {"200,64,0" [] "300,64,0" []}})

(deftest two-standing-kitchens-far-from-their-chests-do-not-wait-for-each-other
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (setup far-world)]
          (core/submit! eng (list 'repeat (list job (assoc args :chest far-chest))) {})
          (core/submit! eng (list 'repeat (list job (assoc args :chest other-far-chest))) {})
          (dotimes [_ 6] (await (core/tick! eng)) (swap! clock + 1000))
          (is (seq (events-of s :round_started))))))))

(deftest a-pass-where-every-step-stopped-does-not-end-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory [(item "beef" 3) (item "coal" 1)] :containers {"10,64,0" []}})
              e (await (wt/ending-of eng job (assoc args :furnace {:x 30 :y 64 :z 0}) 40))]
          (is (contains? #{:condition-failed :nothing-possible} (:kind e)) (pr-str e))
          (is (contains? (:data e) :steps)))))))

(deftest kitchen-hands-its-chest-argument-to-bake-and-deposit
  (let [given [-545 64 324]
        args {:chest given :keep {} :foods ["beef"]}
        facts {:carried {"bread" 20 "wheat" 9} :chest true}]
    (are [step] (= given (get-in (kitchen/decide step args facts) [:call :args :chest]))
      :bake :deposit)))
