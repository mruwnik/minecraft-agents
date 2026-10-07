(ns engine.withdraw-test
  "jobs.storage.withdraw against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.fake :as fake]
            [engine.test-util :as tu :refer [run-until-empty child-outcome]]
            [engine.triggers :as triggers]
            [jobs.storage.withdraw :as wd]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn chest-items [p] (get-in @(fake/state p) [:containers [10 64 0]]))
(defn know-place! [eng kind pos] (mem/write! (:store eng) kind {:pos pos} mem/place-policy))

(def chest {:x 10 :y 64 :z 0})
(def job 'jobs.storage.withdraw)

(deftest shortfall-lists-what-is-still-needed-in-order
  (let [items [{:name "bread" :count 3} {:name "dirt" :count 2} {:name "bread" :count 4}]]
    (are [wanted expected] (= expected (vec (wd/shortfall items wanted)))
      (array-map "bread" 10 "stone_hoe" 2) [["bread" 3] ["stone_hoe" 2]]
      (array-map "bread" 7) []
      (array-map "bread" 5) []
      (array-map "gravel" 1 "dirt" 5 "bread" 8) [["gravel" 1] ["dirt" 3] ["bread" 1]]
      {} [])))

(deftest withdraw-tops-up-to-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "bread" :count 4}] :containers {"10,64,0" [{:name "bread" :count 32}]}})
              result (await (child-outcome eng job {:chest chest :items {"bread" 16}} 8))]
          (is (= {"bread" 16} (inv p)))
          (is (= [{:name "bread" :count 20}] (chest-items p)))
          (is (= {:gave-up false :short {}} result)))))))

(deftest withdraw-with-enough-carried-moves-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "bread" :count 20}] :containers {"10,64,0" [{:name "bread" :count 32}]}})
              result (await (child-outcome eng job {:chest chest :items {"bread" 16}} 4))]
          (is (zero? (count (calls p "transfer"))))
          (is (= {:gave-up false :short {}} result)))))))

(deftest withdraw-reports-what-the-chest-lacks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})
              result (await (child-outcome eng job {:chest chest :items (array-map "bread" 16 "stone_hoe" 2)} 8))]
          (is (= {"bread" 5} (inv p)))
          (is (= {:gave-up false :status :stopped :short {"bread" 11 "stone_hoe" 2}} result))
          (is (some #(= :withdraw.short (:kind %)) @seen)))))))

(deftest withdraw-finds-the-chest-in-places
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})]
          (core/submit! eng (list job {:items {"bread" 3}}) {})
          (is (nil? (core/tick! eng)) "no chest known: blocked")
          (is (empty? (calls p "transfer")))
          (know-place! eng :chest chest)
          (await (run-until-empty eng 6))
          (is (= {"bread" 3} (inv p))))))))

(deftest withdraw-gives-up-on-an-unreachable-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]} :unreachable ["10,64,0"]})
              result (await (child-outcome eng job {:chest chest :items {"bread" 3}} 8))]
          (is (= {:gave-up true :status :stopped :reason "unreachable" :short {"bread" 3}} result))
          (is (some #(= :withdraw.gave-up (:kind %)) @seen))
          (is (= [] (:list (core/state eng)))))))))

(deftest withdraw-gives-up-with-the-transfer-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[forced reason] [[{:status "full" :moved 0} "full"] [{:status "ok" :moved 0} "nothing-moved"]]]
          (let [{:keys [eng p]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})]
            (.override (.-world p) "transfer" (fn ^:async f [_ _ _] (clj->js forced)))
            (is (= {:gave-up true :status :stopped :reason reason :short {"bread" 3}}
                   (await (child-outcome eng job {:chest chest :items {"bread" 3}} 8))))))))))

(deftest withdraw-gives-up-on-a-missing-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {})]
          (is (= {:gave-up true :status :stopped :reason "missing" :short {"bread" 3}}
                 (await (child-outcome eng job {:chest chest :items {"bread" 3}} 8)))))))))

(deftest withdraw-gives-up-when-the-known-chest-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})]
          (know-place! eng :chest chest)
          (is (= {:gave-up true :status :stopped :reason "missing" :short {"bread" 3}}
                 (await (child-outcome eng job {:items {"bread" 3}} 8))))
          (is (some #(= :chest_missing (:kind %)) @seen))
          (is (= [] (:list (core/state eng)))))))))

(deftest withdraw-waits-with-a-reason-when-no-chest-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})]
          (core/submit! eng (list job {:items {"bread" 3}}) {})
          (core/tick! eng)
          (is (some #(and (= :waiting (:kind %)) (= :no-chest (:reason %))) @seen)))))))


(deftest withdraw-walks-and-takes-every-name-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"20,64,0" [{:name "bread" :count 9} {:name "stone_hoe" :count 3}]}})
              result (await (child-outcome eng job {:chest {:x 20 :y 64 :z 0} :items (array-map "bread" 4 "stone_hoe" 2)} 1))]
          (is (= {"bread" 4 "stone_hoe" 2} (inv p)))
          (is (= {:gave-up false :short {}} result))
          (is (= 2 (count (calls p "transfer"))))
          (is (= 1 (count (tu/walked-to eng)))))))))

(deftest withdraw-short-of-what-the-chest-holds-is-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})
              result (await (child-outcome eng job {:chest chest :items (array-map "bread" 16)} 1))]
          (is (= {:gave-up false :status :stopped :short {"bread" 11}} result)))))))

(deftest withdraw-counts-what-moved-before-a-transfer-timed-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [] :containers {"10,64,0" [{:name "bread" :count 32}]}})]
          (.override (.-world p) "transfer"
                     (fn ^:async f [token args impl]
                       (await (impl token args))
                       #js {:status "timeout" :inventoryChange #js {"bread" 16}}))
          (let [result (await (child-outcome eng job {:chest chest :items {"bread" 16}} 8))]
            (is (= {:gave-up false :short {}} result))
            (is (= {"bread" 16} (inv p)))
            (is (= 1 (count (calls p "transfer"))))))))))
