(ns engine.check-waits-test
  "A check that declines says why (ctx/wait): the reason reaches core/waiting, and so jobs show and observe."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

(def ground (into {} (for [x (range -3 6) z (range -3 6)] [(str x ",63," z) "stone"])))

(defn ^:async waiting-after
  "Submit spec in a fake world (spec keys :zones, default [], nil: never read) and run one tick; the reason the job
  waits for (core/waiting), and the observe status row."
  [spec world-spec]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:blocks ground} (dissoc world-spec :zones)))
        w (world/of-data {} {} (get world-spec :zones []))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world w
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
        id (core/submit! eng spec {})]
    (swap! clock + 700)
    (await (core/tick! eng))
    {:waiting (core/waiting eng id)
     :observed (get-in (event-api/status eng nil) [:jobs :items 0 :waiting])
     :listed (seq (:list (core/state eng)))}))

(def pick [{:name "iron_pickaxe" :count 1}])
(def stone-wall (into {} (for [y [64 65 66]] [(str "1," y ",0") "stone"])))

(deftest declined-for-zones-says-no-zones
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (waiting-after '(jobs.access.tunnel {:target [3 64 0]}) {:zones nil :self {:pos {:x 0 :y 64 :z 0}}}))]
          (is (= :no-zones (:reason (:waiting r))))
          (is (= :no-zones (:reason (:observed r)))))))))

(deftest time-jobs-say-what-they-wait-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :day-not-come (:reason (:waiting (await (waiting-after '(jobs.time.wait-for-day) {:time 15000}))))))
        (is (= :dusk-not-come (:reason (:waiting (await (waiting-after '(jobs.time.wait-for-dusk) {:time 6000}))))))))))

(deftest stair-without-a-pickaxe-says-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (waiting-after '(jobs.access.stair {:dir :down :heading :east :steps 2 :fetch false})
                                      {:blocks (merge ground {"1,64,0" "stone" "1,65,0" "stone" "1,66,0" "stone"}) :self {:pos {:x 0 :y 65 :z 0}} :inventory []}))]
          (is (= {:reason :no-tool :tool "pickaxe"} (select-keys (:waiting r) [:reason :tool]))))))))

(deftest stair-with-a-pickaxe-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (waiting-after '(jobs.access.stair {:dir :down :heading :east :steps 2})
                                      {:blocks (merge ground {"1,64,0" "stone"}) :self {:pos {:x 0 :y 65 :z 0}} :inventory pick}))]
          (is (nil? (:waiting r))))))))

(deftest pillar-without-blocks-says-too-few-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (waiting-after '(jobs.access.pillar {:height 3}) {:self {:pos {:x 0 :y 64 :z 0}} :inventory []}))]
          (is (= {:reason :too-few-blocks :short 3} (select-keys (:waiting r) [:reason :short]))))))))

(deftest pillar-with-dirt-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (waiting-after '(jobs.access.pillar {:height 3}) {:self {:pos {:x 0 :y 64 :z 0}} :inventory [{:name "dirt" :count 9}]}))]
          (is (nil? (:waiting r))))))))

(deftest toggle-standing-in-the-door-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (waiting-after '(jobs.access.toggle {:pos [0 64 0] :state :closed})
                                      {:blocks (merge ground {"0,64,0" "oak_fence_gate"}) :states {"0,64,0" {:open true}} :self {:pos {:x 0.5 :y 64 :z 0.5}}}))]
          (is (= :standing-in (:reason (:waiting r)))))))))

(deftest get-seeds-says-why-it-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :no-zones (:reason (:waiting (await (waiting-after '(jobs.gather.get-seeds {:item "sugar_cane"}) {:zones nil})))))
            "no zone list")
        (is (= :no-source (:reason (:waiting (await (waiting-after '(jobs.gather.get-seeds {:item "nonsense"}) {})))))
            "no way to get the item")
        (is (= :nothing-in-range (:reason (:waiting (await (waiting-after '(jobs.gather.get-seeds {:item "sugar_cane"}) {})))))
            "no source block near")))))

(def lead-item [{:name "lead" :count 1}])

(defn ^:async reason-of [spec world]
  (:reason (:waiting (await (waiting-after spec world)))))

(deftest lead-to-says-what-it-lacks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [no-mob (await (reason-of '(jobs.animals.lead-to {:pos {:x 3 :y 64 :z 0}}) {:inventory lead-item}))
              no-dest (await (reason-of '(jobs.animals.lead-to {:mob "cow"}) {:inventory lead-item}))
              no-lead (await (waiting-after '(jobs.animals.lead-to {:mob "cow" :pos {:x 3 :y 64 :z 0}}) {:inventory []}))
              ready (await (waiting-after '(jobs.animals.lead-to {:mob "cow" :pos {:x 3 :y 64 :z 0}}) {:inventory lead-item}))]
          (is (= [:no-mob :no-destination] [no-mob no-dest]))
          (is (nil? (:waiting no-lead)) "no lead carried: the job runs and fetches one")
          (is (nil? (:waiting ready)) "mob, destination and lead given: it runs"))))))
