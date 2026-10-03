(ns engine.kit-test
  "jobs.storage.kit against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.storage.kit :as kit]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

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
(defn chest-items [p] (js->clj (.get (.. p -world -state -containers) "10,64,0") :keywordize-keys true))

(def chest {:x 10 :y 64 :z 0})
(def job 'jobs.storage.kit)
(def full-chest [{:name "bread" :count 32} {:name "stone_hoe" :count 1} {:name "stone_hoe" :count 1} {:name "wooden_hoe" :count 1}])
(def kit-args {:chest chest :tools ["hoe"] :spare 1 :food 12})

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
          (is (some #(and (= :kit.gave-up (:kind %)) (= :warn (:level %))) @seen)))))))

(deftest kit-gives-up-when-withdraw-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:containers {"10,64,0" full-chest}})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] (clj->js {:status "full" :moved 0})))
          (let [result (await (child-outcome eng job kit-args 12))]
            (is (true? (:gave-up result)))
            (is (= "full" (:reason result)))
            (is (some #(and (= :withdraw.gave-up (:kind %)) (= :warn (:level %))) @seen))
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
