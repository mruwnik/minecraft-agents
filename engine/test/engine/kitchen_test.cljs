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
(def kitchen {:blocks {"1,64,0" "furnace" "10,64,0" "chest" "200,64,0" "chest" "300,64,0" "chest" "11,64,0" "crafting_table"}})

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

(def args {:furnace furnace :chest chest :reserve {} :stock {"bread" 1 "cooked_beef" 8 "cooked_chicken" 8}})
(def raw-food [(item "beef" 3) (item "chicken" 2) (item "coal" 2) (item "wheat" 6)])

(deftest one-pass-smelts-every-raw-food-bakes-the-wheat-and-puts-the-cooked-food-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run (list job args) {:inventory raw-food :containers {"10,64,0" []}} 40))]
          (is (true? (finished? s)))
          (is (= {"cooked_beef" 3 "cooked_chicken" 2} (select-keys (chest-items s) ["cooked_beef" "cooked_chicken"])))
          (is (= 1 (get (chest-items s) "bread" 0)) "only the one loaf the stock lacks")
          (is (= 3 (get (inv s) "wheat")) "the wheat the shortfall did not need stays carried")
          (is (nil? (get (inv s) "beef")))
          (is (nil? (get (inv s) "chicken"))))))))

(deftest only-the-shortfall-is-smelted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run (list job (assoc args :stock {"cooked_beef" 2})) {:inventory [(item "beef" 5) (item "coal" 2)] :containers {"10,64,0" []}} 40))]
          (is (= {"cooked_beef" 2} (chest-items s)))
          (is (= 3 (get (inv s) "beef"))))))))

(deftest the-reserve-keeps-the-wheat-the-agent-needs-as-bait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[reserve carried-wheat bread left] [[4 6 0 6] [3 6 1 3]]]
          (let [s (await (run (list job (assoc args :reserve {"wheat" reserve} :stock {"bread" 16})) {:inventory [(item "wheat" carried-wheat)] :containers {"10,64,0" []}} 12))]
            (is (= bread (get (chest-items s) "bread" 0)) (pr-str reserve))
            (is (= left (get (inv s) "wheat" 0)) (pr-str reserve))))))))

(deftest the-reserve-also-holds-back-wheat-in-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run (list job (assoc args :reserve {"wheat" 2} :stock {"bread" 16})) {:inventory [(item "coal" 1)] :containers {"10,64,0" [(item "wheat" 6)]}} 12 {"wheat" 6}))]
          (is (= {"bread" 1 "wheat" 3} (chest-items s)))
          (is (nil? (get (inv s) "wheat"))))))))

(deftest a-chest-that-holds-the-stock-needs-nothing-and-gives-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:inventory [(item "wheat" 9)] :containers {"10,64,0" [(item "bread" 20)]}}
              s (await (run (list job (assoc args :stock {"bread" 16})) world 4 {"bread" 20}))]
          (is (= {"bread" 20} (chest-items s)))
          (is (= 9 (get (inv s) "wheat")) "the bread is not taken out, the wheat not baked"))))))

(deftest done-reads-the-booked-chest-stock
  (are [chest-stock stock done] (= done (kitchen/reached? {:stock stock} chest-stock))
    {"bread" 20} {"bread" 16} true
    {"bread" 15} {"bread" 16} false
    nil {"bread" 16} false
    {"bread" 20 "cooked_beef" 1} {"bread" 16 "cooked_beef" 8} false))

(defn waits [s] (distinct (map (juxt :reason (comp :backed-off :cause)) (events-of s :waiting))))

(defn run-with [inventory & [stock]]
  (run (list job (dissoc args :stock)) {:inventory inventory :containers {"10,64,0" []}} 2 (or stock {})))

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

(deftest a-pass-that-changed-nothing-rests-under-repeat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (setup {:inventory [(item "beef" 3) (item "coal" 1)]})]
          (core/submit! eng (list 'repeat (list job (assoc args :furnace {:x 30 :y 64 :z 0}))) {})
          (dotimes [_ 12] (await (core/tick! eng)) (swap! clock + 1000))
          (is (= 1 (count (filter #(get-in % [:ending :data :steps]) (events-of s :child_ended)))) "ticks 1 s apart, inside the 30 s rest")
          (is (seq (filter #(= :resting (:reason %)) (events-of s :waiting)))))))))

(deftest no-table-seen-or-carried-leaves-the-chest-wheat-alone-with-fetch-false
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:inventory [(item "coal" 1)] :blocks {"11,64,0" "air"} :containers {"10,64,0" [(item "wheat" 9)]}}
              s (await (run (list job (assoc args :reserve {} :stock {"bread" 3} :ways #{:carried})) world 12 {"wheat" 9}))]
          (is (= {"wheat" 9} (chest-items s)))
          (is (nil? (get (inv s) "wheat")))
          (is (empty? (events-of s :round_started)))
          (is (= [[:nothing-to-do nil]] (waits s))))))))

(deftest no-table-is-crafted-from-carried-planks-and-the-bread-baked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:inventory [(item "coal" 1) (item "oak_planks" 4)] :blocks {"11,64,0" "air"} :containers {"10,64,0" [(item "wheat" 9)]}}
              s (await (run (list job (assoc args :reserve {} :stock {"bread" 3})) world 30 {"wheat" 9}))]
          (is (true? (finished? s)))
          (is (= 3 (get (chest-items s) "bread" 0))))))))

(deftest no-table-and-no-wood-ends-naming-the-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:inventory [(item "coal" 1)] :blocks {"11,64,0" "air"} :containers {"10,64,0" [(item "wheat" 9)]}}
              s (await (run (list job (assoc args :reserve {} :stock {"bread" 3})) world 30 {"wheat" 9}))]
          (is (= {"wheat" 9} (chest-items s)) "the wheat is put back")
          (is (= :no-table (:reason (first (events-of s :stopped)))))
          (is (re-find #"crafting_table" (pr-str (:because (first (events-of s :stopped))))) "the cause names the table"))))))

(deftest wheat-in-the-chest-is-baked-though-none-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [stock [nil {"wheat" 6}]]
          (let [s (await (run (list job (assoc args :stock {"bread" 16})) {:inventory [(item "coal" 1)] :containers {"10,64,0" [(item "wheat" 6)]}} 12 stock))]
            (is (true? (finished? s)) (pr-str stock))
            (is (= 2 (get (chest-items s) "bread" 0)) (pr-str stock))
            (is (nil? (get (chest-items s) "wheat")) (pr-str stock))))))))

(deftest a-chest-booked-without-wheat-keeps-the-kitchen-waiting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-with [(item "potato" 4)] {"cobblestone" 20}))]
          (is (empty? (events-of s :round_started)))
          (is (= [[:nothing-to-do nil]] (waits s))))))))

(deftest the-pass-says-what-it-stored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run (list job args) {:inventory [(item "beef" 3) (item "coal" 2)] :containers {"10,64,0" []}} 40 {}))
              e (first (events-of s :completed))]
          (is (= {"cooked_beef" 3} (chest-items s)))
          (is (= {:stored {"cooked_beef" 3}} (get-in e [:steps :deposit])))
          (is (= "kitchen done: smelted beef 3; stored cooked_beef 3" (:text e))))))))

(deftest bread-carried-by-hand-stays-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run (list job args) {:inventory [(item "bread" 20)] :containers {"10,64,0" []}} 4 {}))]
          (is (= {} (chest-items s)))
          (is (= 20 (get (inv s) "bread"))))))))

(def base {:chest {:x 1 :y 1 :z 1} :reserve {} :stock {"bread" 16}})

(deftest bake-asks-for-the-loaves-the-stock-lacks-from-the-wheat-above-the-reserve
  (are [facts args-extra call] (= call (select-keys (:call (kitchen/decide :bake (merge base args-extra) facts)) [:args]))
    {:carried {"wheat" 9} :chest true :table true :chest-stock {"bread" 14}} {} {:args {:item "bread" :count 2 :ways #{:carried :chest :craft :gather}}}
    {:carried {"wheat" 9} :chest true :table true :chest-stock {}} {:reserve {"wheat" 6}} {:args {:item "bread" :count 1 :ways #{:carried :chest :craft :gather}}}))

(deftest take-fetches-only-the-wheat-the-shortfall-needs-and-keeps-the-reserve
  (are [facts extra items] (= items (get-in (kitchen/decide :take (merge base extra) facts) [:call :args :items]))
    {:carried {"wheat" 1} :chest true :table true :chest-stock {"wheat" 20 "bread" 13}} {} {"wheat" 9}
    {:carried {"wheat" 2} :chest true :table true :chest-stock {"wheat" 20}} {:reserve {"wheat" 2}} {"wheat" 20}))

(deftest take-skips-when-the-chest-lacks-the-wheat
  (is (= :no-wheat (:skip (kitchen/decide :take base {:carried {} :chest true :table true :chest-stock {"wheat" 2}})))))

(deftest kitchen-hands-its-chest-argument-to-take-and-deposit
  (let [given [-545 64 324]
        args {:chest given :reserve {} :stock {"bread" 16} :foods ["beef"]}
        facts {:carried {"bread" 4 "wheat" 3} :made {"bread" 4} :wheat0 3 :chest true :table true :chest-stock {"wheat" 9}}]
    (are [step] (= given (get-in (kitchen/decide step args facts) [:call :args :chest]))
      :take :deposit)))

(defn ^:async run-beside
  "Submit the job wait-for-light (listed, dark at night), then form, over the world; tick 3 times."
  [form world]
  (let [{:keys [eng] :as s} (setup (assoc world :time 15000))]
    (core/submit! eng '(jobs.time.wait-for-light) {})
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

(deftest the-first-stopped-step-is-kept-whole-to-end-with-as-because
  (let [e {:kind :condition-failed :cause :no-fuel :because {:kind :impossible :cause :x}}
        m (kitchen/after-child {} :take nil {} e)]
    (is (= e (:failed (kitchen/after-child m :bake nil {} {:kind :impossible :cause :y}))))))


(deftest can-run-between-steps-follows-the-chest-the-body-has-seen
  (let [p (tu/seeing-all (tu/fake-on-floor (merge-with merge kitchen {:blocks {"20,64,0" "stone"}})))
        status (fn [a] (let [s (kitchen/can-run? {:primitives p :args a :view (fn [] {:data {} :now 0})})]
                         [(first s) (:reason (nth s 2))]))]
    (are [a expect] (= expect (status a))
      {:chest chest} [:ok :chest]
      {:chest {:x 20 :y 64 :z 0}} [:never :chest-gone]
      {} [:later :no-chest])))
