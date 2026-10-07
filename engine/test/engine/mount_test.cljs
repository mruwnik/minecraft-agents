(ns engine.mount-test
  "jobs.movement.mount against the fake world: every mount status maps to a job status."
  (:require [cljs.test :refer [deftest is async]]
            [engine.test-util :as tu]
            [engine.registry :as registry]
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

(defn ^:async run [spec args & [mount-status child]]
  (let [p (doto (tu/fake spec) (.setOwner "t"))
        _ (when mount-status (set! (.-mount p) (fn [_ _] (js/Promise.resolve #js {:status mount-status}))))
        c (cond-> (bare-ctx p args) child (assoc :engine {:jobs registry/jobs} :call-child (fn [_ _ _] (js/Promise.resolve child))
                                                 :child-result (fn [_] {:reason :blocked})))
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

(deftest a-name-with-nothing-of-that-name-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {:entities [boat]} {:name "pig"}))]
          (is (= :gone (:reason (result-of o)))))))))

(deftest a-name-skips-an-occupied-nearer-vehicle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [taken (assoc boat :id 5 :name "minecart" :passengers [77])
              free (assoc boat :id 6 :name "minecart" :pos {:x 1.5 :y 64 :z 1.0})
              o (await (run {:entities [(assoc taken :pos {:x 1.5 :y 64 :z 0.5}) free]} {:name "minecart"}))]
          (is (= 6 (:id (result-of o)))))))))

(deftest an-occupied-vehicle-by-id-is-occupied
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {:entities [(assoc boat :passengers [77 78])]} {:id 9}))]
          (is (= :occupied (:reason (result-of o)))))))))

(deftest mount-statuses-map-to-reasons
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[status reason] [["hand-full" :hand-full] ["out-of-reach" :unreachable] ["weird" :failed]]]
          (let [o (await (run {:entities [boat]} {:id 9} status))]
            (is (= reason (:reason (result-of o))) status)))))))

(deftest aboard-another-vehicle-is-aboard-other
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {:self {:vehicle 5} :entities [boat (assoc boat :id 5 :name "minecart")]} {:id 9}))]
          (is (= :aboard-other (:reason (result-of o)))))))))

(deftest a-walk-that-waits-on-the-world-continues
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (assoc boat :pos {:x 1.5 :y 64 :z 30.5})
              o (await (run {:entities [far]} {:id 9} nil :continue))]
          (is (= :continue (:r o)))
          (is (empty? (:results o))))))))

(deftest a-failed-walk-twice-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (assoc boat :pos {:x 1.5 :y 64 :z 30.5})
              o (await (run {:entities [far]} {:id 9} nil :done))]
          (is (= :unreachable (:reason (result-of o)))))))))

(deftest a-heard-row-without-a-position-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [heard (-> boat (assoc :id 5) (dissoc :pos))
              o (await (run {:entities [heard boat]} {:name "oak_boat"}))
              g (await (run {:entities [heard]} {:id 5}))]
          (is (= 9 (:id (result-of o))))
          (is (= :gone (:reason (result-of g)))))))))

(defn check-with [spec args]
  (let [waiting (atom nil)
        p (tu/fake spec)
        ok (mount/check {:primitives p :args args :wait waiting})]
    {:ok ok :why @waiting}))

(deftest check-declines-when-no-vehicle-is-in-sight
  (is (= {:ok false :why :no-vehicle}
         (update (check-with {:entities []} {:id 9}) :why #(some-> % :reason)))
      "by id")
  (is (= :no-vehicle (:reason (:why (check-with {:entities [(dissoc boat :pos)]} {:name "oak_boat"})))) "heard only")
  (is (true? (:ok (check-with {:entities [boat]} {:id 9}))))
  (is (true? (:ok (check-with {:entities [boat]} {:name "oak_boat"}))))
  (is (true? (:ok (check-with {:entities []} {:id "x"}))) "bad args are the round's stop"))
