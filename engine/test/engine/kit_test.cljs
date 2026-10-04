(ns engine.kit-test
  "jobs.storage.kit against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.storage.kit :as kit]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn chest-items [p] (get-in @(fake/state p) [:containers [10 64 0]]))

(def chest {:x 10 :y 64 :z 0})
(def job 'jobs.storage.kit)
(def full-chest [{:name "bread" :count 32} {:name "stone_hoe" :count 1} {:name "stone_hoe" :count 1} {:name "wooden_hoe" :count 1}])
(def kit-args {:chest chest :tools ["hoe"] :spare 1 :food 12 :craft false})

(deftest needs-lists-what-is-still-carried-short
  (are [inventory args expected] (= expected (kit/needs inventory args))
    [] {:tools ["hoe" "pickaxe"] :spare 1 :food 12} [["hoe" 2] ["pickaxe" 2] [:food 12]]
    [{:name "iron_hoe" :count 1} {:name "stone_hoe" :count 1} {:name "bread" :count 12}] {:tools ["hoe"] :spare 1 :food 12} []
    [{:name "iron_pickaxe" :count 2}] {:tools ["axe"] :spare 0 :food 0} [["axe" 1]]
    [{:name "bread" :count 5} {:name "apple" :count 4} {:name "cooked_beef" :count 1}] {:tools [] :food 12} [[:food 2]]
    [{:name "dirt" :count 40}] {:tools ["hoe"] :spare 1 :food 0} [["hoe" 2]]
    [] {} [["hoe" 2] [:food 12]]))

(deftest plan-takes-the-best-and-reports-the-rest
  (are [needs inventory chest expected] (= expected (kit/plan needs inventory chest))
    [["hoe" 2]] []
    [{:name "wooden_hoe" :count 1} {:name "iron_hoe" :count 1} {:name "stone_hoe" :count 1}]
    {:take {"iron_hoe" 1 "stone_hoe" 1} :short {}}
    [["hoe" 1]] [{:name "iron_hoe" :count 1}]
    [{:name "iron_hoe" :count 3}]
    {:take {"iron_hoe" 2} :short {}}
    [[:food 10]] [{:name "bread" :count 2}]
    [{:name "apple" :count 20} {:name "bread" :count 6} {:name "cooked_beef" :count 3}]
    {:take {"cooked_beef" 3 "bread" 8 "apple" 1} :short {}}
    [["hoe" 2] [:food 5]] []
    [{:name "stone_hoe" :count 1} {:name "dirt" :count 9}]
    {:take {"stone_hoe" 1} :short {"hoe" 1 :food 5}}
    [["axe" 1]] []
    [{:name "iron_pickaxe" :count 1}]
    {:take {} :short {"axe" 1}}))

(deftest kit-takes-the-best-hoes-and-the-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" full-chest}})
              result (await (child-outcome eng job kit-args 12))]
          (is (= {"stone_hoe" 2 "bread" 12} (inv p)))
          (is (= [{:name "bread" :count 20} {:name "wooden_hoe" :count 1}]
                 (filterv #(pos? (:count %)) (chest-items p))))
          (is (= {:gave-up false :short {}} result)))))))

(deftest kit-with-everything-carried-moves-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "iron_hoe" :count 1} {:name "stone_hoe" :count 1} {:name "bread" :count 12}]
                                      :containers {"10,64,0" full-chest}})
              result (await (child-outcome eng job kit-args 4))]
          (is (zero? (count (calls p "transfer"))))
          (is (= {:gave-up false :short {}} result)))))))

(deftest kit-reports-an-empty-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:containers {"10,64,0" []}})
              result (await (child-outcome eng job kit-args 8))]
          (is (= {:gave-up false :short {"hoe" 2 :food 12}} result))
          (is (some #(= :kit.short (:kind %)) @seen))
          (is (= [] (:list (core/state eng)))))))))

(deftest kit-gives-up-on-an-unreachable-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:containers {"10,64,0" full-chest} :unreachable ["10,64,0"]})
              result (await (child-outcome eng job kit-args 8))]
          (is (= {:gave-up true :reason "unreachable" :short {"hoe" 2 :food 12}} result))
          (is (some #(= :kit.gave-up (:kind %)) @seen)))))))

(deftest kit-gives-up-when-withdraw-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:containers {"10,64,0" full-chest}})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] (clj->js {:status "full" :moved 0})))
          (let [result (await (child-outcome eng job kit-args 12))]
            (is (true? (:gave-up result)))
            (is (= "full" (:reason result)))
            (is (some #(= :withdraw.gave-up (:kind %)) @seen))
            (is (= [] (:list (core/state eng))))))))))

(deftest kit-needs-a-known-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" full-chest}})]
          (core/submit! eng (list job {}) {})
          (is (nil? (core/tick! eng)) "no chest known: blocked")
          (is (empty? (calls p "transfer")))
          (mem/write! (:store eng) :chest {:pos chest} mem/place-policy)
          (await (run-until-empty eng 12))
          (is (= {"stone_hoe" 2 "bread" 12} (inv p))))))))

;; ------------------------------------------------------------------ the craft branch

(def hoe-recipes
  {"stone_hoe" {:needs {"cobblestone" 2 "stick" 2} :count 1 :table true}
   "wooden_hoe" {:needs {"oak_planks" 2 "stick" 2} :count 1 :table true}
   "diamond_hoe" {:needs {"diamond" 2 "stick" 2} :count 1 :table true}})
(def table {"11,64,0" "crafting_table"})
(def hoe-args {:chest chest :tools ["hoe"] :spare 0 :food 0})

(defn ^:async craft-run
  "Run the kit on a chest with the hoe recipes and a table by default; {:result :inv :chest :p :eng}."
  [{:keys [contents carried blocks args]
    :or {blocks table args hoe-args}}]
  (let [{:keys [eng p]} (setup {:containers {"10,64,0" contents} :inventory (or carried []) :blocks blocks :recipes hoe-recipes})
        result (await (child-outcome eng job args 60))]
    {:result result :inv (inv p) :chest (into {} (comp (filter #(pos? (:count %))) (map (juxt :name :count))) (chest-items p)) :p p}))

(def stone-chest [{:name "cobblestone" :count 5} {:name "stick" :count 4} {:name "diamond" :count 3}])

(deftest kit-crafts-what-the-chest-cannot-supply
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world expected-inv expected-chest]
                [["stone hoe from cobblestone and sticks, diamonds untouched"
                  {:contents stone-chest}
                  {"stone_hoe" 1}
                  {"cobblestone" 3 "stick" 2 "diamond" 3}]
                 ["wooden hoe from logs through planks and sticks"
                  {:contents [{:name "oak_log" :count 2}]}
                  {"wooden_hoe" 1}
                  {"oak_log" 1}]
                 ["diamond hoe when only diamond is allowed"
                  {:contents stone-chest :args (assoc hoe-args :craft-tiers ["diamond"])}
                  {"diamond_hoe" 1}
                  {"cobblestone" 5 "stick" 2 "diamond" 1}]]]
          (let [{:keys [result inv chest]} (await (craft-run world))]
            (is (= {:gave-up false :short {}} result) label)
            (is (= expected-inv (select-keys inv (keys expected-inv))) label)
            (is (= expected-chest chest) label)))))))

(deftest kit-craft-reports-what-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world expected]
                [["no table"
                  {:contents stone-chest :blocks {}}
                  {:gave-up false :short {"hoe" 1} :missing {"hoe" "no-table"}}]
                 ["nothing makeable"
                  {:contents [{:name "dirt" :count 4}]}
                  {:gave-up false :short {"hoe" 1} :missing {"hoe" "cobblestone, oak_log"}}]
                 ["food short, no wheat"
                  {:contents [] :args {:chest chest :tools [] :spare 0 :food 2}}
                  {:gave-up false :short {:food 2} :missing {:food "wheat"}}]]]
          (let [{:keys [result chest p]} (await (craft-run world))]
            (is (= expected result) label)
            (is (zero? (count (calls p "transfer"))) label)))))))

(deftest kit-crafts-bread-from-chest-wheat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result inv chest]} (await (craft-run {:contents [{:name "wheat" :count 10}]
                                                            :args {:chest chest :tools [] :spare 0 :food 2}}))]
          (is (= {:gave-up false :short {}} result))
          (is (= 2 (get inv "bread")))
          (is (= 4 (get chest "wheat"))))))))

(deftest kit-craft-false-leaves-the-short-as-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (craft-run {:contents stone-chest :args (assoc hoe-args :craft false)}))]
          (is (= {:gave-up false :short {"hoe" 1}} result))
          (is (empty? (calls p "craft"))))))))

(deftest kit-craft-rederives-from-the-inventory-after-a-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result inv chest]} (await (craft-run {:contents stone-chest
                                                              :carried [{:name "stick" :count 2}]}))]
          (is (= {:gave-up false :short {}} result))
          (is (= 1 (get inv "stone_hoe")))
          (is (= 4 (get chest "stick"))))))))

;; ------------------------------------------------------------------ check, full inventory, remembered failures

(deftest kit-craft-ends-done-when-the-inventory-is-full
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [packed (concat (repeat 34 {:name "dirt" :count 64}) [{:name "cobblestone" :count 2} {:name "stick" :count 2}])
              {:keys [result p]} (await (craft-run {:contents [{:name "dirt" :count 4}] :carried packed}))]
          (is (= {:gave-up false :short {"hoe" 1} :missing {"hoe" "full"}} result))
          (is (empty? (calls p "toss"))))))))

(defn ^:async two-runs
  "Run the kit twice on one engine with a dirt-only chest, advancing the clock by gap between: [first second third crafts-after-each]."
  [gaps]
  (let [{:keys [eng p clock]} (setup {:containers {"10,64,0" [{:name "dirt" :count 4}]} :blocks table :recipes hoe-recipes})]
    (loop [gaps gaps out []]
      (if (empty? gaps)
        out
        (let [_ (swap! clock + (first gaps))
              r (await (child-outcome eng job hoe-args 60))]
          (recur (rest gaps) (conj out [r (count (calls p "craft")) (count (calls p "inspectContainer"))])))))))

(deftest kit-remembers-a-failed-craft-so-repeat-does-not-retry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [missing {:gave-up false :short {"hoe" 1} :missing {"hoe" "cobblestone, oak_log"}}
              [[r1 c1] [r2 c2] [r3 c3]] (await (two-runs [0 1000 (* 11 60 1000)]))]
          (is (= missing r1))
          (is (pos? c1))
          (is (= missing r2))
          (is (= c1 c2) "no craft call while the failure is remembered")
          (is (= missing r3))
          (is (> c3 c2) "tried again after the memory expired"))))))

(deftest kit-complete-ends-at-once-without-walking-or-inspecting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "stone_hoe" :count 2} {:name "bread" :count 12}]
                                      :containers {"10,64,0" full-chest}})
              result (await (child-outcome eng job kit-args 4))]
          (is (= {:gave-up false :short {}} result))
          (is (empty? (.-calls (.-world p))) "no walk, inspect or transfer call"))))))
