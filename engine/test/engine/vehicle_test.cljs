(ns engine.vehicle-test
  "jobs.movement.vehicle (what the body rides, the vehicle hold), the :mounted trigger and
  jobs.movement.leave-vehicle against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [jobs.movement.vehicle :as vehicle]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [triggers.movement.mounted :as mounted]
            [jobs.movement.leave-vehicle :as leave]))

(def boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 1.5 :y 64 :z 0.5}})

(defn water-around
  "Water at y 64 (and sand under it at y 63) for x and z within 3 of the boat's cell."
  []
  (into {} (for [x (range -2 5) z (range -3 4) [y name] [[63 "sand"] [64 "water"]]]
             [(str x "," y "," z) name])))

(defn aboard
  "A fake body in the boat, with extra spec merged."
  ([] (aboard {}))
  ([spec] (tu/fake (-> (merge {:self {:vehicle 9} :entities [boat]} spec)
                        (update :blocks #(merge (water-around) %))))))

(def shore
  "A dry landing south of the boat: (1, 64, 2) air over stone."
  {"1,64,2" "air" "1,63,2" "stone"})

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn view
  "A memory view at :now 10 with the given entries {kind [data ...]} (written at t 5)."
  [entries]
  {:data {:entries (into {} (map (fn [[k ds]] [k (mapv (fn [d] {:t 5 :data d}) ds)])) entries)
          :policies (into {} (map (fn [k] [k mem/job-policy])) (filter mem/job-kind? (keys entries)))}
   :now 10})

(defn bare-ctx
  "A ctx with empty job memory over primitives p for check and round, recording what it writes."
  [p args]
  (let [mem* (atom {})
        seen (atom [])]
    {:primitives p :args args :root "j1" :slots [] :token "t"
     :view (fn [] {:data {:entries {(mem/job-kind "j1") [{:t 0 :data @mem*}]}} :now 0})
     :update-mem (fn [f more] (swap! mem* #(apply f % more)))
     :emit (fn [kind level fields] (swap! seen conj [kind level fields]))
     :result (fn [data] (swap! seen conj [:result data]))
     :act (fn [k args] (.call (aget p (name k)) p "t" args))
     :seen seen}))

(defn setup-owner [p] (.setOwner p "t") p)

;; ------------------------------------------------------------------- sensing and the hold

(deftest vehicle-of-reads-self
  (is (nil? (vehicle/vehicle-of (.self (tu/fake {})))))
  (is (= {:id 9 :uuid "u-9" :name "oak_boat"} (vehicle/vehicle-of (.self (aboard)))))
  (is (nil? (vehicle/vehicle-of #js {:status "offline"})) "an offline self rides nothing"))

(deftest held-only-while-the-holding-job-lives
  (doseq [[label entries expected]
          [["no hold" {} false]
           ["hold by a live job" {(mem/job-kind "j1") [{}] vehicle/hold-kind [{:job "j1"}]} true]
           ["hold by a job that is gone" {vehicle/hold-kind [{:job "j1"}]} false]
           ["one live hold among stale ones" {(mem/job-kind "j2") [{}] vehicle/hold-kind [{:job "j1"} {:job "j2"}]} true]]]
    (is (= expected (vehicle/held? (view entries))) label)))

(deftest hold-and-release-write-the-root-job
  (let [written (atom [])
        c {:root "j7"
           :view (fn [] {:data {:entries {vehicle/hold-kind (mapv (fn [d] {:t 1 :data d}) @written)}} :now 2})
           :remember (fn [kind data _policy] (swap! written conj (assoc data :kind kind)))
           :forget (fn [kind pred] (swap! written #(filterv (fn [d] (not (and (= kind (:kind d)) (pred d)))) %)))}]
    (vehicle/hold! c)
    (vehicle/hold! c)
    (is (= [{:job "j7" :kind vehicle/hold-kind}] @written) "held once")
    (vehicle/release! c)
    (is (= [] @written))))

(deftest yaw-toward-is-minecraft-degrees
  (doseq [[to expected] [[{:x 0 :z 5} 0] [{:x -5 :z 0} 90] [{:x 0 :z -5} 180] [{:x 5 :z 0} 270]]]
    (is (< (js/Math.abs (- expected (vehicle/yaw-toward {:x 0 :z 0} to))) 1e-9) (pr-str to))))

;; ------------------------------------------------------------------- the trigger

(deftest mounted-holds-aboard-unless-a-live-job-holds-the-vehicle
  (doseq [[label p entries expected]
          [["on foot" (tu/fake {}) {} false]
           ["aboard" (aboard) {} true]
           ["aboard, held by a live job" (aboard) {(mem/job-kind "j1") [{}] vehicle/hold-kind [{:job "j1"}]} false]
           ["aboard, held by a gone job" (aboard) {vehicle/hold-kind [{:job "j1"}]} true]]]
    (is (= expected (mounted/mounted p (view entries) {})) label)))

(deftest mounted-is-registered-with-leave-vehicle
  (is (= mounted/mounted (:when (:mounted triggers/all))))
  (is (= '(jobs.movement.leave-vehicle) (:job (:mounted triggers/all))))
  (is (= :stop (:persistence (:mounted triggers/all)))))

;; ------------------------------------------------------------------- leave-vehicle

(defn ^:async run-round [p args]
  (let [c (bare-ctx (setup-owner p) args)
        r (await (leave/round c))]
    {:r r :c c :seen @(:seen c)}))

(defn dismount-args [p] (mapv #(js->clj (.-args %) :keywordize-keys true) (calls p "dismount")))

(deftest on-foot-it-is-done-without-a-dismount
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {})
              {:keys [r]} (await (run-round p {}))]
          (is (= :done r))
          (is (= [] (calls p "dismount"))))))))

(deftest it-faces-the-nearest-dry-cell-and-reports-a-dry-landing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (aboard {:blocks shore :entities [(assoc boat :dismountAt {:x 1 :y 64 :z 2})]})
              {:keys [r seen]} (await (run-round p {}))]
          (is (= :done r))
          (is (= [{:yaw 0}] (dismount-args p)) "south, toward (1, 64, 2)")
          (is (nil? (vehicle/vehicle-of (.self p))))
          (is (some #{[:result {:pos {:x 1 :y 64 :z 2} :landed :dry}]} seen))
          (is (some #(= [:vehicle.left :info {:pos {:x 1 :y 64 :z 2} :landed :dry}] %) seen)))))))

(deftest toward-wins-over-the-dry-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (aboard {:blocks shore})
              _ (await (run-round p {:toward {:x -2 :y 64 :z 0}}))]
          (is (= [{:yaw 90}] (dismount-args p)) "west"))))))

(deftest with-no-dry-cell-it-leaves-without-a-yaw-and-reports-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (aboard)
              {:keys [seen]} (await (run-round p {}))]
          (is (= [{}] (dismount-args p)))
          (is (some #{[:result {:pos {:x 2.5 :y 64 :z 0.5} :landed :in-water}]} seen)))))))

(deftest a-failed-dismount-is-retried-in-the-run-then-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (setup-owner (aboard {:dismountFails true}))
              c (bare-ctx p {:max-tries 2 :wait-ms 1})
              r (await (leave/round c))]
          (is (= :done r))
          (is (= [[:result {:status :stopped :reason :dismount-failed :text "could not get off the vehicle: timeout" :tries 2}]]
                 (filterv #(= :result (first %)) @(:seen c))))
          (is (= 2 (count (calls p "dismount"))))
          (is (= [[:vehicle.dismount_failed :warn {:tries 2 :status "timeout"}]]
                 (filterv #(= :vehicle.dismount_failed (first %)) @(:seen c)))))))))

(deftest the-default-run-keeps-trying-far-longer-than-two-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (setup-owner (aboard {:dismountFails true}))
              c (bare-ctx p {:wait-ms 1})
              _ (await (leave/round c))]
          (is (= 8 (count (calls p "dismount")))))))))

(deftest check-is-whether-the-body-rides
  (is (false? (leave/check (bare-ctx (tu/fake {}) {}))))
  (is (true? (leave/check (bare-ctx (aboard) {})))))

;; ------------------------------------------------------------------- end to end

(deftest the-registered-trigger-takes-the-body-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (aboard {:blocks shore})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [] :now #(deref clock)})})]
          (core/register-reflex! eng {:trigger :mounted})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (nil? (vehicle/vehicle-of (.self p))))
          (is (= 1 (count (calls p "dismount"))))
          (is (= [] (:list (core/state eng)))))))))
