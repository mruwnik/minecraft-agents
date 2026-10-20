(ns engine.zones-containers-test
  "Containers as job rules: withdraw, deposit, bake, get-seeds, get-food and smelt consult jobs.lib.access
  before taking from or putting into another's chest."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [run-until-empty child-outcome]]
            [engine.withdraw-test :as wt]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup [world zones]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake (merge {:floor tu/walk-floor} world)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(def chest {:x 10 :y 64 :z 0})
(def vault {:name "vault" :min [9 60 -1] :max [11 70 1] :owner "Miles"})
(def stock {"10,64,0" [{:name "bread" :count 32} {:name "wheat" :count 30}]})
(def refused-at-start {:kind :condition-failed :cause {:reason :refused :zones ["vault"] :claims [] :text "refused: zone vault"}})
(def own-claim {:id "c1" :owner "Miles" :status :active :until 99999999999 :min [9 60 -1] :max [11 70 1]})

;; ------------------------------------------------------------------ withdraw

(deftest withdraw-follows-the-zone-owner-the-allowance-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zone extra taken?] [[vault {} false]
                                     [(assoc vault :owner "Fake") {} true]
                                     [(assoc vault :owner "FAKE") {} true]
                                     [(assoc vault :allow #{:take}) {} true]
                                     [(assoc vault :allow #{:dig}) {} false]
                                     [vault {:ignore-zones? true} true]
                                     [nil {} true]]]
          (let [{:keys [eng p]} (setup {:containers stock} (some-> zone vector))
                result (await (wt/ending-of eng 'jobs.storage.withdraw (merge {:chest chest :items {"bread" 8}} extra) 12))]
            (is (= taken? (pos? (count (calls p "transfer")))) (pr-str [zone extra]))
            (is (= ({true :done false :condition-failed} taken?) (:kind result)) (pr-str [zone extra]))
            (is (= ({true "withdraw: everything wanted is carried" false nil} taken?) (get-in result [:data :text])) (pr-str [zone extra]))
            (is (= ({true nil false refused-at-start} taken?) ({true nil false result} taken?)) (pr-str [zone extra]))))))))

(deftest withdraw-from-a-foreign-chest-never-inspects-its-way-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers stock} [vault])]
          (await (child-outcome eng 'jobs.storage.withdraw {:chest chest :items {"bread" 8}} 12))
          (is (empty? (calls p "transfer")))
          (is (= {} (inv p))))))))

(deftest a-claim-of_another_refuses_withdraw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers stock} [])]
          (swap! (:state (:world eng)) assoc :area-claims {:value [own-claim]})
          (let [result (await (wt/ending-of eng 'jobs.storage.withdraw {:chest chest :items {"bread" 8}} 12))]
            (is (empty? (calls p "transfer")))
            (is (= {:kind :condition-failed :cause {:reason :refused :zones [] :claims ["c1"] :text "refused: claim c1"}} result))))))))

;; ------------------------------------------------------------------ deposit

(deftest deposit-into-a-foreign-chest-is-refused-at-start-and-into-an-own-or-allowed-one-is-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zone extra put?] [[vault {} false]
                                   [(assoc vault :owner "fake") {} true]
                                   [(assoc vault :allow #{:put}) {} true]
                                   [vault {:ignore-zones? true} true]]]
          (let [{:keys [eng p]} (setup {:containers {"10,64,0" []} :inventory [{:name "dirt" :count 5}]} [zone])
                result (await (wt/ending-of eng 'jobs.storage.deposit (merge {:chest chest} extra) 12))]
            (is (= put? (pos? (count (calls p "transfer")))) (pr-str [zone extra]))
            (is (= ({true :done false :condition-failed} put?) (:kind result)) (pr-str [zone extra]))
            (is (= ({true nil false refused-at-start} put?) ({true nil false result} put?)) (pr-str [zone extra]))))))))

;; ------------------------------------------------------------------ the parents propagate :refused

;; ------------------------------------------------------------------ get-food

(def two-chests {"20,64,0" [{:name "bread" :count 4}] "30,64,0" [{:name "apple" :count 5}]})

(defn know-source! [eng pos kind]
  (mem/write! (:store eng) :food-source {:pos pos :kind kind} {:cap 5 :ttl :forever}))

(deftest get-food-never-takes-from-a-foreign-chest-even-starving
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zone extra taken] [[{:name "pantry" :min [19 60 -1] :max [21 70 1] :owner "Miles"} {} []]
                                    [{:name "pantry" :min [19 60 -1] :max [21 70 1] :owner "Fake"} {} ["bread"]]
                                    [{:name "pantry" :min [19 60 -1] :max [21 70 1] :owner "Miles"} {:ignore-zones? true} ["bread"]]]]
          (let [{:keys [eng p]} (setup {:self {:food 0} :containers {"20,64,0" [{:name "bread" :count 4}]}} [zone])]
            (know-source! eng {:x 20 :y 64 :z 0} :chest)
            (core/submit! eng (list 'jobs.survival.get-food extra) {})
            (await (run-until-empty eng 20))
            (is (= taken (mapv #(.-item (.-args %)) (calls p "transfer"))) (pr-str [zone extra]))))))))

(deftest get-food-skips-a-foreign-chest-and-uses-the-next-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pantry {:name "pantry" :min [19 60 -1] :max [21 70 1] :owner "Miles"}
              {:keys [eng p]} (setup {:self {:food 0} :containers two-chests} [pantry])]
          (know-source! eng {:x 30 :y 64 :z 0} :chest)
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 30))
          (is (= ["apple"] (mapv #(.-item (.-args %)) (calls p "transfer")))))))))

;; ------------------------------------------------------------------ smelt

(deftest smelt-leaves-a-foreign-furnace-alone-and-works-an-own-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zone extra touched?] [[{:name "forge" :min [0 60 -1] :max [2 70 1] :owner "Miles"} {} false]
                                       [{:name "forge" :min [0 60 -1] :max [2 70 1] :owner "FAKE"} {} true]
                                       [{:name "forge" :min [0 60 -1] :max [2 70 1] :owner "Miles"} {:ignore-zones? true} true]
                                       [{:name "forge" :min [0 60 -1] :max [2 70 1] :owner "Miles" :allow #{:put}} {} false]
                                       [{:name "forge" :min [0 60 -1] :max [2 70 1] :owner "Miles" :allow #{:take}} {} false]]]
          (let [{:keys [eng p seen]} (setup {:blocks {"1,64,0" "furnace"}
                                             :inventory [{:name "raw_iron" :count 5} {:name "coal" :count 3}]} [zone])]
            (core/submit! eng (list 'jobs.items.smelt (merge {:furnace {:x 1 :y 64 :z 0} :item "raw_iron" :count 3} extra)) {})
            (await (run-until-empty eng 3))
            (is (= touched? (pos? (count (calls p "furnace")))) (pr-str [zone extra]))
            (is (= touched? (empty? (filter #(and (= :waiting (:kind %)) (= :refused (:reason %))) @seen))) (pr-str [zone extra]))))))))
