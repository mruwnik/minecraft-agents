(ns engine.vehicle-test
  "jobs.lib.vehicle (what the body rides), the :mounted trigger and
  jobs.movement.leave-vehicle against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [jobs.lib.vehicle :as vehicle]
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
        seen (atom [])
        pauses (atom [])]
    {:primitives p :args args :root "j1" :slots [] :token "t"
     :engine {:jobs registry/jobs}
     :call (fn [_slot _def pause-args] (swap! pauses conj pause-args) (js/Promise.resolve (contract/ending :done nil nil)))
     :pauses pauses
     :hold-still (fn [_])
     :view (fn [] {:data {:entries {(mem/job-kind "j1") [{:t 0 :data @mem*}]}} :now 0})
     :update-mem (fn [f more] (swap! mem* #(apply f % more)))
     :emit (fn [kind level fields] (swap! seen conj [kind level fields]))
     :result (fn [data] (swap! seen conj [:result data]))
     :act (fn [k args] (.call (aget p (name k)) p "t" args))
     :seen seen}))

(defn setup-owner [p] (.setOwner p "t") p)

;; ------------------------------------------------------------------- sensing

(deftest vehicle-of-reads-self
  (is (nil? (vehicle/vehicle-of (.self (tu/fake {})))))
  (is (= {:id 9 :uuid "u-9" :name "oak_boat"} (vehicle/vehicle-of (.self (aboard)))))
  (is (nil? (vehicle/vehicle-of #js {:status "offline"})) "an offline self rides nothing"))

(deftest yaw-toward-is-minecraft-degrees
  (doseq [[to expected] [[{:x 0 :z 5} 0] [{:x -5 :z 0} 90] [{:x 0 :z -5} 180] [{:x 5 :z 0} 270]]]
    (is (< (js/Math.abs (- expected (vehicle/yaw-toward {:x 0 :z 0} to))) 1e-9) (pr-str to))))

;; ------------------------------------------------------------------- the trigger

(deftest mounted-holds-aboard-only-with-no-job-listed
  (doseq [[label p live expected]
          [["on foot" (tu/fake {}) #{} false]
           ["aboard" (aboard) #{} true]
           ["aboard, a job listed" (aboard) #{"j1"} false]]]
    (is (= expected (mounted/mounted p (view {}) {} nil live)) label)))

(deftest mounted-is-registered-with-leave-vehicle
  (is (= mounted/mounted (:when (:mounted triggers/all))))
  (is (= '(jobs.movement.leave-vehicle) (:job (:mounted triggers/all))))
  (is (= :cooldown (:persistence (:mounted triggers/all)))))

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
          (is (= :done (:kind r)))
          (is (= [] (calls p "dismount"))))))))

(deftest it-faces-the-nearest-dry-cell-and-reports-a-dry-landing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (aboard {:blocks shore :entities [(assoc boat :dismountAt {:x 1 :y 64 :z 2})]})
              {:keys [r seen]} (await (run-round p {}))]
          (is (= {:kind :done :cause nil :data {:pos {:x 1 :y 64 :z 2} :landed :dry}} r))
          (is (= [{:yaw 0}] (dismount-args p)) "south, toward (1, 64, 2)")
          (is (nil? (vehicle/vehicle-of (.self p))))
          (is (empty? (filter #(= :vehicle.left (first %)) seen)) "the ending carries it"))))))

(deftest a-stale-in-water-flag-does-not-make-a-dry-landing-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (aboard {:blocks shore :self {:vehicle 9 :inWater true} :entities [(assoc boat :dismountAt {:x 1 :y 64 :z 2})]})
              {:keys [r]} (await (run-round p {}))]
          (is (= :dry (:landed (:data r)))))))))

(deftest a-landing-in-the-air-over-water-is-in-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (aboard {:blocks {"1,65,0" "air"} :self {:vehicle 9 :inWater false} :entities [(assoc boat :dismountAt {:x 1.5 :y 65.1 :z 0.5})]})
              {:keys [r]} (await (run-round p {}))]
          (is (= :in-water (:landed (:data r)))))))))

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
              {:keys [r]} (await (run-round p {}))]
          (is (= [{}] (dismount-args p)))
          (is (= {:pos {:x 2.5 :y 64 :z 0.5} :landed :in-water} (:data r))))))))

(deftest a-failed-dismount-is-retried-in-the-run-then-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (setup-owner (aboard {:dismountFails true}))
              c (bare-ctx p {})
              r (await (settings/with-settings {:jobs.movement.leave-vehicle/max-tries 2} #(leave/round c)))]
          (is (= {:kind :nothing-possible :cause :dismount-failed :data {:tries 2 :status "timeout"}} r))
          (is (= 2 (count (calls p "dismount"))))
          (is (empty? (filter #(= :vehicle.dismount_failed (first %)) @(:seen c))) "the ending carries it"))))))

(deftest the-wait-between-dismount-tries-is-a-noop-pause-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (setup-owner (aboard {:dismountFails true}))
              c (bare-ctx p {})
              _ (await (settings/with-settings {:jobs.movement.leave-vehicle/max-tries 3} #(leave/round c)))]
          (is (= [{:seconds 0.1 :why :dismount-retry} {:seconds 0.1 :why :dismount-retry}] @(:pauses c))
              "a fixed wait, no doubling"))))))

(deftest the-default-run-keeps-trying-far-longer-than-two-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (setup-owner (aboard {:dismountFails true}))
              c (bare-ctx p {})
              _ (await (leave/round c))]
          (is (= 8 (count (calls p "dismount")))))))))

(deftest the-condition-is-whether-the-body-rides
  (is (= [:later :not-mounted] ((juxt first #(:reason (nth % 2))) (leave/not-mounted (bare-ctx (tu/fake {}) {})))))
  (is (= :ok (first (leave/not-mounted (bare-ctx (aboard) {}))))))

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
