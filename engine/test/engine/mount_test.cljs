(ns engine.mount-test
  "jobs.movement.mount against the fake world: every mount status maps to a job status."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.test-util :as tu]
            [engine.registry :as registry]
            [jobs.movement.mount :as mount]
            [jobs.lib.vehicle :as vehicle]))

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

(defn ^:async run [spec args & [mount-status child child-res]]
  (let [p (doto (tu/fake spec) (.setOwner "t"))
        _ (when mount-status (set! (.-mount p) (fn [_ _] (js/Promise.resolve #js {:status mount-status}))))
        c (cond-> (bare-ctx p args) child (assoc :engine {:jobs registry/jobs}
                                                 :call (fn [_ _ _] (js/Promise.resolve
                                                                    (cond (= :continue child) :continue
                                                                          (:arrived child-res) {:kind :done :cause nil :data child-res}
                                                                          :else {:kind :nothing-possible :cause :unreachable :data (or child-res {:reason :blocked})})))))
        r (await (mount/round c))]
    {:r r :p p :seen @(:seen c)}))

(defn result-of [{:keys [r]}]
  (if (= :done (:kind r))
    (assoc (:data r) :status :done)
    (assoc (:data r) :status :stopped :reason (contract/cause-key r))))

(deftest mounts-a-boat-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {:entities [boat]} {:id 9}))]
          (is (= :done (:kind (:r o))))
          (is (= {:status :done :id 9 :vehicle "oak_boat"} (result-of o)))
          (is (some? (vehicle/vehicle-of (.self (:p o)))))
          (is (not-any? #(= :vehicle.mounted (first %)) (:seen o)) "the ending carries it")
          (is (some #{[:remember :vehicle-hold {:job "j1"}]} (:seen o)) "holds the vehicle"))))))

(deftest already-aboard-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [o (await (run {:self {:vehicle 9} :entities [boat]} {:id 9}))]
          (is (= :done (:status (result-of o))))
          (is (true? (:already (result-of o)))))))))

(deftest aboard-already-ends-before-the-start-only-once-the-root-holds-the-vehicle
  (let [p (tu/fake {:self {:vehicle 9} :entities [boat]})
        held (fn [job] (assoc (bare-ctx p {:id 9}) :view (fn [] {:data {:entries {:vehicle-hold [{:t 0 :data {:job job}}]}} :now 0})))]
    (is (= [:later :holding] (tu/kind-reason (mount/done? (bare-ctx p {:id 9})))) "no hold yet: the round holds it first")
    (is (= [:ok :aboard] (tu/kind-reason (mount/done? (held "j1")))))
    (is (= [:later :holding] (tu/kind-reason (mount/done? (held "other")))) "another job's hold is not this root's")))

(deftest refusals-stop-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label spec reason]
                [["not-mountable" {:entities [(assoc boat :name "cow" :id 9)]} :not-mountable]
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

(deftest nothing-of-that-id-or-name-in-sight-is-no-vehicle-for-can-run
  (are [spec args] (= [:later :no-vehicle] (tu/kind-reason (mount/can-run? (bare-ctx (tu/fake spec) args))))
    {:entities []} {:id 9}
    {:entities [boat]} {:name "pig"}
    {:entities [(dissoc boat :pos)]} {:id 9}))

(deftest aboard-another-vehicle-is-aboard-other-for-can-run
  (is (= [:later :aboard-other]
         (tu/kind-reason (mount/can-run? (bare-ctx (tu/fake {:self {:vehicle 5} :entities [boat (assoc boat :id 5 :name "minecart")]}) {:id 9}))))))

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

(deftest a-walk-that-waits-on-the-world-continues
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (assoc boat :pos {:x 1.5 :y 64 :z 30.5})
              o (await (run {:entities [far]} {:id 9} nil :continue))]
          (is (= :continue (:r o))))))))

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
              o (await (run {:entities [heard boat]} {:name "oak_boat"}))]
          (is (= 9 (:id (result-of o)))))))))

(defn condition-with [spec args]
  (let [c {:primitives (tu/fake spec) :args args :view (fn [] {:data {:entries {}} :now 0}) :root "j1" :slots []}]
    (let [[kind _ data] (mount/can-start? c)]
      (when-not (= :ok kind) (:reason data)))))

(deftest conditions-refuse-when-no-vehicle-is-in-sight
  (is (= :no-vehicle (condition-with {:entities []} {:id 9})) "by id")
  (is (= :no-vehicle (condition-with {:entities [(dissoc boat :pos)]} {:name "oak_boat"})) "heard only")
  (is (nil? (condition-with {:entities [boat]} {:id 9})))
  (is (nil? (condition-with {:entities [boat]} {:name "oak_boat"})))
  (is (= :bad-args (condition-with {:entities [boat]} {}))))

(deftest a-walk-that-arrives-but-stays-out-of-reach-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (assoc boat :pos {:x 1.5 :y 64 :z 30.5})
              o (await (js/Promise.race #js [(run {:entities [far]} {:id 9} nil :done {:arrived true})
                                             (js/Promise. (fn [res _] (js/setTimeout #(res {:hung true}) 200)))]))]
          (is (nil? (:hung o)) "no endless re-walk")
          (is (= :unreachable (:reason (result-of o)))))))))
