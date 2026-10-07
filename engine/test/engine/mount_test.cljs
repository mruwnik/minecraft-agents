(ns engine.mount-test
  "jobs.movement.mount against the fake world: every mount status maps to a job status."
  (:require [cljs.test :refer [deftest is async]]
            [engine.test-util :as tu]
            [jobs.movement.mount :as mount]
            [jobs.movement.vehicle :as vehicle]))

(def boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 1.5 :y 64 :z 0.5}})

(defn bare-ctx [p args]
  (let [seen (atom [])]
    {:primitives p :args args :root "j1" :slots [] :token "t"
     :view (fn [] {:data {:entries {}} :now 0})
     :remember (fn [kind data _] (swap! seen conj [:remember kind data]))
     :emit (fn [kind level fields] (swap! seen conj [kind level fields]))
     :result (fn [data] (swap! seen conj [:result data]))
     :act (fn [k args] (.call (aget p (name k)) p "t" args))
     :seen seen}))

(defn ^:async run [spec args]
  (let [p (doto (tu/fake spec) (.setOwner "t"))
        c (bare-ctx p args)
        r (await (mount/round c))]
    {:r r :p p :results (filterv #(= :result (first %)) @(:seen c)) :seen @(:seen c)}))

(defn result-of [{:keys [results]}] (second (last results)))

(deftest mounts-a-boat-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {:entities [boat]} {:id 9}))]
          (is (= :done (:r o)))
          (is (= {:status :done :id 9 :vehicle "oak_boat"} (result-of o)))
          (is (some? (vehicle/vehicle-of (.self (:p o)))))
          (is (some #(= :vehicle.mounted (first %)) (:seen o)))
          (is (some #{[:remember :vehicle-hold {:job "j1"}]} (:seen o)) "holds the vehicle"))))))

(deftest already-aboard-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {:self {:vehicle 9} :entities [boat]} {:id 9}))]
          (is (= :done (:status (result-of o))))
          (is (true? (:already (result-of o)))))))))

(deftest refusals-stop-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label spec reason]
                [["gone" {:entities []} :gone]
                 ["not-mountable" {:entities [(assoc boat :name "cow" :id 9)]} :not-mountable]
                 ["timeout" {:entities [boat] :mountFails true} :timeout]]]
          (let [o (await (run spec {:id 9}))]
            (is (= {:status :stopped :reason reason} (select-keys (result-of o) [:status :reason])) label)
            (is (nil? (vehicle/vehicle-of (.self (:p o)))) label)))))))

(deftest a-bad-id-is-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {} {:id "x"}))]
          (is (= :bad-args (:reason (result-of o)))))))))

(deftest a-name-picks-the-nearest-entity-of-that-name
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (assoc boat :id 5 :pos {:x 1.5 :y 64 :z 30.5})
              o (await (run {:entities [far boat]} {:name "oak_boat"}))]
          (is (= 9 (:id (result-of o)))))))))

(deftest a-name-with-nothing-of-that-name_is_gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {:entities [boat]} {:name "pig"}))]
          (is (= :gone (:reason (result-of o)))))))))
