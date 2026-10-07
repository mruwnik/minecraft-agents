(ns engine.village-maintain-test
  "jobs.village.maintain against the fake world; its children are recording stubs, found by job name."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]))

(def job 'jobs.village.maintain)
(def children {:repair 'jobs.build.from-plan :breed 'jobs.village.breed :roll 'jobs.village.roll})

(defn villager
  [id x & [extra]]
  (merge {:id id :uuid (str "v-" id) :name "villager" :kind "passive" :pos {:x x :y 64 :z 2}} extra))

(defn stubs
  "Recording stand-ins for the child jobs: calls collects [job args]; ends maps a job to its result (default {})."
  [calls ends]
  (fn [jobs]
    (reduce (fn [m j]
              (assoc m j {:check (constantly true)
                          :round (fn [c]
                                   (swap! calls conj [j (:args c)])
                                   (let [r (get ends j {})]
                                     (when (= :boom r) (throw (js/Error. "boom")))
                                     (ctx/result! c r)
                                     :done))}))
            jobs (vals children))))

(defn ^:async scenario
  ([args w ends] (scenario args w ends 40))
  ([args w ends n]
   (let [calls (atom [])
         s (doto (h/setup w) (-> :p tu/seeing-all))
         s (update s :eng #(update % :jobs (stubs calls ends)))]
     (core/submit! (:eng s) (list job args) {})
     (dotimes [_ n]
       (swap! (:clock s) + 700)
       (await (core/tick! (:eng s))))
     (assoc s :calls calls))))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :maintain.done)))
(defn step [s k] (get-in (done-event s) [:steps k]))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn ran [s j] (filterv #(= j (first %)) @(:calls s)))
(defn world [& vs] {:self {:pos {:x 0 :y 64 :z 0}} :entities (vec vs)})

(def pair [(villager 1 3) (villager 2 5)])

(deftest nothing-to-do-declines-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world) {} 10))]
          (is (nil? (done-event s)))
          (is (empty? @(:calls s)))
          (is (= :nothing-to-do (:reason (first (events-of s :waiting))))))))))

(deftest a-plan-is-repaired-first-then-the-rest-in-order
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "village" :target 4}
                                 (apply world pair) {'jobs.build.from-plan {:placed 3}}))]
          (is (= [['jobs.build.from-plan {:plan "village"}]] (ran s 'jobs.build.from-plan)))
          (is (= 3 (:placed (step s :repair))))
          (is (= [['jobs.village.breed {:target 4 :radius 48}]] (ran s 'jobs.village.breed)))
          (is (= {:skipped :no-transport} (step s :import)))
          (is (true? (finished? s)))
          (is (= ['jobs.build.from-plan 'jobs.village.breed] (mapv first @(:calls s)))))))))

(deftest breed-is-skipped-with-its-reason
  (are [args w reason] (async done
                         (tu/run-async done
                           (fn ^:async t []
                             (let [s (await (scenario (merge {:plan "p"} args) w {}))]
                               (is (= {:skipped reason} (step s :breed)))
                               (is (empty? (ran s 'jobs.village.breed)))))))
    {} (apply world pair) :no-target
    {:target 2} (apply world pair) :at-target
    {:target 4} (world (villager 1 3) (villager 2 5 {:baby true})) :too-few-adults
    {:target 4} (world) :too-few-adults))

(deftest babies-count-toward-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p" :target 3} (world (villager 1 3) (villager 2 5) (villager 3 6 {:baby true})) {}))]
          (is (= {:skipped :at-target} (step s :breed))))))))

(deftest a-role-no-villager-holds-is-rolled-once-others-are-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [roles [{:profession "librarian" :trade "mending"} {:profession "farmer"}]
              s (await (scenario {:roles roles}
                                 (world (villager 1 3 {:profession "farmer"}) (villager 2 5 {:profession "none"}))
                                 {'jobs.village.roll {:rolled 1}}))]
          (is (= [['jobs.village.roll {:profession "librarian" :trade "mending" :radius 48}]]
                 (ran s 'jobs.village.roll)))
          (is (= {:skipped :filled} (step s :roll-1)))
          (is (= 1 (:rolled (step s :roll-0))))
          (is (true? (finished? s))))))))

(deftest a-role-with-no-villager-near-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:roles [{:profession "librarian"}] :plan "p"} (world) {}))]
          (is (= {:skipped :no-villager} (step s :roll-0)))
          (is (empty? (ran s 'jobs.village.roll))))))))

(deftest a-failing-child-is-booked-and-the-pass-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p" :target 4} (apply world pair) {'jobs.build.from-plan :boom}))]
          (is (= :failed (:skipped (step s :repair))))
          (is (= 1 (count (ran s 'jobs.village.breed))))
          (is (= 1 (count (ran s 'jobs.build.from-plan))) "run once, not retried")
          (is (= "incomplete" (:reason (done-event s))))
          (is (true? (finished? s))))))))

(deftest a-stopped-child-makes-the-pass-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p"} (world) {'jobs.build.from-plan {:status :stopped :reason :incomplete :missing [[1 2 3]]}}))]
          (is (= :stopped (:status (step s :repair))))
          (is (= "incomplete" (:reason (done-event s)))))))))

(deftest a-clean-pass-has-no-stop-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p"} (world) {}))]
          (is (nil? (:reason (done-event s))))
          (is (true? (finished? s))))))))
