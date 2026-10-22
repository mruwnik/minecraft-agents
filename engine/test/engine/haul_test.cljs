(ns engine.haul-test
  "jobs.storage.haul against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.deposit-test :as dt]
            [engine.withdraw-test :as wt]
            [engine.test-util :as tu :refer [child-outcome run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.storage.haul :as haul]))

(defn start
  "An engine on dir over primitives p (and world store w, default none); the main chest place is known."
  [dir p w]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                           w (assoc :world w)))]
    (mem/write! (:store eng) :chest {:pos {:x 20 :y 64 :z 0}} mem/place-policy)
    {:eng eng :p p :dir dir}))

(defn setup
  ([world] (setup world nil))
  ([world w] (start (tu/tmp-dir) (tu/fake-on-floor world) w)))

(defn stacks [prefix n] (mapv (fn [i] {:name (str prefix i) :count 5}) (range n)))
(defn containers [p] (:containers @(fake/state p)))
(defn transfers [p] (filterv #(= "transfer" (.-name %)) (.-calls (.-world p))))
(defn direction [call] (.-direction (.-args call)))

(deftest load-plan-takes-whole-stacks-up-to-the-free-slots-and-leaves-kept-names
  (let [stacks [{:name "a" :count 3} {:name "keep" :count 9} {:name "b" :count 4} {:name "a" :count 2} {:name "c" :count 1}]]
    (is (= {"a" 5 "b" 4} (haul/load-plan stacks ["keep"] 3 [])))
    (is (= {"a" 13 "b" 4} (haul/load-plan stacks ["keep"] 3 [{:name "a" :count 8}])))
    (is (= {} (haul/load-plan stacks [] 0 [])))))

(deftest haul-moves-two-chests-in-three-bag-loads
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" (stacks "a" 40) "10,64,4" (stacks "b" 40) "20,64,0" []}})
              result (await (child-outcome eng 'jobs.storage.haul {:from [[10 64 0] [10 64 4]]} 120))
              cs (containers p)]
          (is (= :done (:status result)))
          (is (= 3 (:trips result)))
          (is (= 400 (reduce + (vals (:moved result)))))
          (is (empty? (get cs [10 64 0])))
          (is (empty? (get cs [10 64 4])))
          (is (= 80 (count (get cs [20 64 0])))))))))

(deftest haul-with-a-bag-full-of-other-goods-stops-bag-full-and-keeps-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (stacks "x" 35) :containers {"10,64,0" (stacks "a" 5) "20,64,0" []}})
              result (await (wt/ending-of eng 'jobs.storage.haul {:from [[10 64 0]]} 30))]
          (is (= {:kind :condition-failed :cause :bag-full} (select-keys result [:kind :cause])))
          (is (empty? (transfers p)))
          (is (empty? (get (containers p) [20 64 0]))))))))

(deftest haul-deposits-only-what-it-took-not-other-carried-goods
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_sapling" :count 4} {:name "a0" :count 2}]
                                      :containers {"10,64,0" (stacks "a" 3) "20,64,0" []}})
              result (await (child-outcome eng 'jobs.storage.haul {:from [[10 64 0]]} 60))
              main (get (containers p) [20 64 0])]
          (is (= :done (:status result)))
          (is (= {"a0" 5 "a1" 5 "a2" 5} (:moved result)))
          (is (= 15 (reduce + (map :count main))))
          (is (= {"oak_sapling" 4 "a0" 2} (select-keys (storage/carried-totals (u/inventory p)) ["oak_sapling" "a0"]))))))))

(deftest haul-leaves-the-kept-names-in-the-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (stacks "x" 33)
                                      :containers {"10,64,0" [{:name "keepme" :count 7} {:name "a" :count 5} {:name "b" :count 5}] "20,64,0" []}})
              result (await (child-outcome eng 'jobs.storage.haul {:from [[10 64 0]] :leave ["keepme"]} 60))]
          (is (= :done (:status result)))
          (is (= {"a" 5 "b" 5} (:moved result)) "the kept stack first in the chest takes no slot of the two free")
          (is (= [{:name "keepme" :count 7}] (map #(select-keys % [:name :count]) (get (containers p) [10 64 0]))))
          (is (= ["a" "b"] (map :name (get (containers p) [20 64 0])))))))))

(def two-sources {"10,64,0" (stacks "a" 5) "10,64,4" (stacks "b" 5) "20,64,0" []})

(deftest haul-without-sources-is-a-bad-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{} {:from []} {:to [20 64 0]}]]
          (let [{:keys [eng]} (setup {:containers {"20,64,0" []}})
                result (await (wt/ending-of eng 'jobs.storage.haul args 10))]
            (is (= {:kind :impossible :cause {:reason :bad-args}} (update (select-keys result [:kind :cause]) :cause dissoc :text)) (pr-str args))))))))

(deftest haul-cut-at-a-transfer-resumes-without-doubling-the-load
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [n [1 2]]
          (let [first-run (setup {:containers two-sources})
                {:keys [eng p dir]} first-run]
            (tu/shutdown-at! first-run "transfer" n)
            (core/submit! eng (list 'jobs.storage.haul {:from [[10 64 0]]}) {})
            (await (core/tick! eng))
            (let [resumed (start dir p nil)]
              (await (run-until-empty (:eng resumed) 30))
              (is (empty? (get (containers p) [10 64 0])) (str "cut at transfer " n))
              (is (= (map :name (stacks "a" 5)) (map :name (get (containers p) [20 64 0]))) (str "cut at transfer " n))
              (is (= [5] (distinct (map :count (get (containers p) [20 64 0])))) (str "cut at transfer " n)))))))))

(deftest haul-looks-again-at-a-chest-found-empty-before-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [first-run (setup {:containers {"10,64,0" [] "10,64,4" (stacks "b" 5) "20,64,0" []}})
              {:keys [eng p dir]} first-run]
          (tu/shutdown-at! first-run "transfer" 1)
          (core/submit! eng (list 'jobs.storage.haul {:from [[10 64 0] [10 64 4]]}) {})
          (await (core/tick! eng))
          (swap! (fake/state p) assoc-in [:containers [10 64 0]] (stacks "a" 3))
          (await (run-until-empty (:eng (start dir p nil)) 30))
          (is (empty? (get (containers p) [10 64 0])))
          (is (= 8 (count (get (containers p) [20 64 0])))))))))

(deftest haul-books-what-the-take-got-when-the-chest-lost-goods-meanwhile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [first-run (setup {:inventory [{:name "a0" :count 20}]
                                :containers {"10,64,0" [{:name "a0" :count 5}] "20,64,0" []}})
              {:keys [eng p dir]} first-run]
          (tu/shutdown-at! first-run "transfer" 1)
          (core/submit! eng (list 'jobs.storage.haul {:from [[10 64 0]]}) {})
          (await (core/tick! eng))
          (swap! (fake/state p) assoc-in [:containers [10 64 0]] [{:name "a0" :count 2}])
          (await (run-until-empty (:eng (start dir p nil)) 30))
          (is (= 2 (reduce + (map :count (get (containers p) [20 64 0])))))
          (is (= 20 (get (storage/carried-totals (u/inventory p)) "a0"))))))))

(defn inspects-of [p x z]
  (count (filter #(and (= "inspectContainer" (.-name %)) (= [x z] [(.-x (.-pos (.-args %))) (.-z (.-pos (.-args %)))]))
                 (.-calls (.-world p)))))

(deftest haul-resumed-after-a-wait-does-not-look-again-at-emptied-chests
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [first-run (setup {:containers {"10,64,0" [] "10,64,4" (stacks "b" 2) "20,64,0" []}})
              {:keys [eng p dir]} first-run
              args {:from [[10 64 0] [10 64 4]]}]
          (tu/shutdown-at! first-run "transfer" 1)
          (core/submit! eng (list 'jobs.storage.haul args) {})
          (await (core/tick! eng))
          (let [id (first (:list (core/state eng)))
                before (inspects-of p 10 0)]
            (mem/write! (:store eng) (mem/job-kind id) (assoc (mem/job-mem (mem/view (:store eng)) id []) :waiting true))
            (core/save-memory! eng)
            (await (run-until-empty (:eng (start dir p nil)) 30))
            (is (= before (inspects-of p 10 0)))))))))

(deftest haul-stops-to-full-when-the-main-chest-takes-nothing-and-making-is-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers two-sources})]
          (dt/full-at p [20 64 0])
          (let [result (await (wt/ending-of eng 'jobs.storage.haul {:from [[10 64 0]] :make-chests? false} 30))]
            (is (= {:kind :nothing-possible :cause :to-full :data {:gave-up true}} (update result :data select-keys [:gave-up])))))))))

(deftest haul-with-a-source-that-will-not-open-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers two-sources})]
          (.override (.-world p) "inspectContainer" (fn ^:async f [_ _ _] #js {:status "timeout" :items #js []}))
          (let [result (await (wt/ending-of eng 'jobs.storage.haul {:from [[10 64 0]]} 30))]
            (is (= {:kind :nothing-possible :cause :timeout} (select-keys result [:kind :cause])))
            (is (seq (get (containers p) [10 64 0])))))))))

(deftest haul-stops-unreachable-when-the-source-cannot-be-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:containers two-sources :unreachable ["10,64,0"]})
              result (await (wt/ending-of eng 'jobs.storage.haul {:from [[10 64 0]]} 30))]
          (is (= {:kind :nothing-possible :cause :unreachable} (select-keys result [:kind :cause]))))))))

(deftest haul-is-refused-at-start-for-a-source-in-another-agents-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args gave-up cause] [[{} true :refused] [{:ignore-zones? true} false nil]]]
          (let [w (ew/of-data {} {} [{:name "farm" :owner "Miles" :min [10 60 -5] :max [12 70 5]}])
                {:keys [eng p]} (setup {:containers two-sources} w)
                result (await (wt/ending-of eng 'jobs.storage.haul (merge {:from [[10 64 0]]} args) 30))]
            (is (= cause (some-> result :cause :reason)) (pr-str args))
            (is (= gave-up (boolean (seq (get (containers p) [10 64 0])))) (pr-str args))))))))

(deftest haul-stops-bag-full-when-nothing-can-be-deposited-to-make-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory (vec (repeat 35 {:name "iron_pickaxe" :count 1})) :containers two-sources})
              result (await (wt/ending-of eng 'jobs.storage.haul {:from [[10 64 0]]} 30))]
          (is (= {:kind :condition-failed :cause :bag-full} (select-keys result [:kind :cause]))))))))
