(ns engine.zones-containers-test
  "Containers as job rules: withdraw, deposit, kit, bake, get-seeds, get-food and smelt consult jobs.lib.access
  before taking from or putting into another's chest."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [run-until-empty child-outcome]]
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
(def refused {:gave-up true :reason :refused :zones ["vault"] :claims []})
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
                result (await (child-outcome eng 'jobs.storage.withdraw (merge {:chest chest :items {"bread" 8}} extra) 12))]
            (is (= taken? (pos? (count (calls p "transfer")))) (pr-str [zone extra]))
            (is (= ({true {:gave-up false :short {}} false refused} taken?) result) (pr-str [zone extra]))))))))

(deftest withdraw-from-a-foreign-chest-warns-and-never-inspects-its-way-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:containers stock} [vault])]
          (await (child-outcome eng 'jobs.storage.withdraw {:chest chest :items {"bread" 8}} 12))
          (is (empty? (calls p "transfer")))
          (is (= {} (inv p)))
          (is (= [["vault"]] (mapv :zones (events-of seen :withdraw.refused)))))))))

(deftest a-claim-of_another_refuses_withdraw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers stock} [])]
          (swap! (:state (:world eng)) assoc :area-claims {:value [own-claim]})
          (let [result (await (child-outcome eng 'jobs.storage.withdraw {:chest chest :items {"bread" 8}} 12))]
            (is (empty? (calls p "transfer")))
            (is (= {:gave-up true :reason :refused :zones [] :claims ["c1"]} result))))))))

;; ------------------------------------------------------------------ deposit

(deftest deposit-into-a-foreign-chest-is-refused-and-into-an-own-or-allowed-one-is-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zone extra put?] [[vault {} false]
                                   [(assoc vault :owner "fake") {} true]
                                   [(assoc vault :allow #{:put}) {} true]
                                   [vault {:ignore-zones? true} true]]]
          (let [{:keys [eng p]} (setup {:containers {"10,64,0" []} :inventory [{:name "dirt" :count 5}]} [zone])
                result (await (child-outcome eng 'jobs.storage.deposit (merge {:chest chest} extra) 12))]
            (is (= put? (pos? (count (calls p "transfer")))) (pr-str [zone extra]))
            (is (= ({true {:gave-up false} false refused} put?) result) (pr-str [zone extra]))))))))

;; ------------------------------------------------------------------ the parents propagate :refused

(deftest kit-bake-and-get-seeds-pass-the-refusal-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[job args pred] [['jobs.storage.kit {:chest chest :tools ["hoe"] :spare 1 :food 12 :craft false}
                                  #(= refused (select-keys % [:gave-up :reason :zones :claims]))]
                                 ['jobs.items.bake {:chest chest :keep 4}
                                  #(and (= :refused (:reason %)) (= ["vault"] (:zones %)))]]]
          (let [{:keys [eng p]} (setup {:containers stock :blocks {"11,64,0" "crafting_table"}} [vault])
                result (await (child-outcome eng job args 60))]
            (is (empty? (calls p "transfer")) (str job))
            (is (pred result) (pr-str job result))))))))

(deftest get-seeds-from-a-foreign-chest-ends-short-with-the-refusal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:containers {"10,64,0" [{:name "carrot" :count 10}]}} [vault])]
          (core/submit! eng '(jobs.gather.get-seeds {:item "carrot" :count 4 :chest {:x 10 :y 64 :z 0}}) {})
          (await (run-until-empty eng 30))
          (is (empty? (calls p "transfer")))
          (is (= [:refused] (mapv :reason (events-of seen :get-seeds.gave-up)))))))))

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
                                       [{:name "forge" :min [0 60 -1] :max [2 70 1] :owner "Miles" :allow #{:put}} {} true]
                                       [{:name "forge" :min [0 60 -1] :max [2 70 1] :owner "Miles" :allow #{:take}} {} false]]]
          (let [{:keys [eng p seen]} (setup {:blocks {"1,64,0" "furnace"}
                                             :inventory [{:name "raw_iron" :count 5} {:name "coal" :count 3}]} [zone])]
            (core/submit! eng (list 'jobs.items.smelt (merge {:furnace {:x 1 :y 64 :z 0} :item "raw_iron" :count 3} extra)) {})
            (await (run-until-empty eng 3))
            (is (= touched? (pos? (count (calls p "furnace")))) (pr-str [zone extra]))
            (is (= ({true [] false ["refused"]} touched?) (mapv :reason (events-of seen :smelt.gave-up))) (pr-str [zone extra]))))))))
