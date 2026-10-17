(ns engine.village-maintain-test
  "jobs.village.maintain against the fake world; its children are recording stubs, found by job name."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]
            [jobs.village.maintain :as maintain]))

(def job 'jobs.village.maintain)
(def children {:repair 'jobs.build.from-plan :breed 'jobs.village.breed :roll 'jobs.village.roll})

(defn villager
  [id x & [extra]]
  (merge {:id id :uuid (str "v-" id) :name "villager" :kind "passive" :pos {:x x :y 64 :z 2}} extra))

(defn stubs
  "Recording stand-ins for the child jobs: calls collects [job args]; ends maps a job to its result (default {}), :boom,
  {:decline reason} (its condition fails), or a fn of the ctx and the call count that returns :continue or a result
  (ends :nothing-possible with its :reason when its :status is :stopped, or with its :kind and :cause when given; else done)."
  [calls ends]
  (fn [jobs]
    (reduce (fn [m j]
              (let [e (get ends j {})]
                (assoc m j {:can-start? (fn [_] (swap! calls conj [:check j]) (or (some-> (:decline e) (contract/later "declined")) (contract/ok :stub "stub")))
                            :round (fn [c]
                                     (swap! calls conj [j (:args c)])
                                     (let [r (if (fn? e) (e c (count (filter #(= j (first %)) @calls))) e)]
                                       (when (= :boom r) (throw (js/Error. "boom")))
                                       (cond
                                         (= :continue r) :continue
                                         (= :stopped (:status r)) (ctx/end c (:kind r :nothing-possible) (:cause r (:reason r)) (dissoc r :status :reason :kind :cause))
                                         :else (ctx/end c :done nil (dissoc r :status)))))})))
            jobs (vals children))))

(defn ^:async scenario
  ([args w ends] (scenario args w ends 40))
  ([args w ends n] (await (scenario args w ends n identity)))
  ([args w ends n prepare]
   (let [calls (atom [])
         s (doto (h/setup w) (-> :p tu/seeing-all) (-> :p prepare))
         s (update s :eng #(update % :jobs (stubs calls ends)))]
     (core/submit! (:eng s) (list job args) {})
     (await (tu/run-ticks (:eng s) (:clock s) n 700))
     (assoc s :calls calls))))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event
  "The pass's ending event (job.completed or job.stopped)."
  [{:keys [seen]}]
  (first (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %))) @seen)))
(defn step [s k] (get-in (done-event s) [:steps k]))
(defn ending
  "[kind reason] of the root's last completed or stopped event."
  [{:keys [seen]}]
  (when-let [e (last (filter #(#{:completed :stopped} (:kind %)) @seen))]
    [(:kind e) (:reason e)]))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn ran [s j] (filterv #(= j (first %)) @(:calls s)))
(defn world [& vs] {:self {:pos {:x 0 :y 64 :z 0}} :entities (vec vs)})

(def pair [(villager 1 3) (villager 2 5)])
(def librarian {:profession "librarian" :trade "mending" :pos [4 64 4]})

(deftest a-village-in-order-is-done-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world) {} 10))]
          (is (= [:completed :already] ((juxt :kind :cause) (done-event s))))
          (is (empty? @(:calls s))))))))

(deftest a-plan-is-repaired-first-then-the-rest-in-order
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "village" :target 4}
                                 (apply world pair) {'jobs.build.from-plan {:placed 3}}))]
          (is (= [['jobs.build.from-plan {:plan "village"}]] (ran s 'jobs.build.from-plan)))
          (is (= 3 (:placed (step s :repair))))
          (is (= [['jobs.village.breed {:target 4 :radius 48}]] (ran s 'jobs.village.breed)))
          (is (true? (finished? s)))
          (is (= ['jobs.build.from-plan 'jobs.village.breed] (mapv first (remove #(= :check (first %)) @(:calls s))))))))))

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
        (let [roles [librarian {:profession "farmer" :pos [6 64 4]}]
              s (await (scenario {:roles roles}
                                 (world (villager 1 3 {:profession "farmer"}) (villager 2 5 {:profession "none"}))
                                 {'jobs.village.roll {:rolled 1}}))]
          (is (= [['jobs.village.roll {:profession "librarian" :trade "mending" :pos [4 64 4] :radius 48}]]
                 (ran s 'jobs.village.roll)))
          (is (= {:skipped :filled} (step s :roll-1)))
          (is (= 1 (:rolled (step s :roll-0))))
          (is (true? (finished? s))))))))

(deftest a-role-with-no-villager-near-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:roles [librarian] :plan "p"} (world) {}))]
          (is (= {:skipped :no-villager} (step s :roll-0)))
          (is (empty? (ran s 'jobs.village.roll))))))))

(deftest a-throwing-child-fails-the-pass-instead-of-ending-it-quietly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p" :target 4} (apply world pair) {'jobs.build.from-plan :boom}))]
          (is (= ["Error: boom"] (mapv :error (vals (:failed (core/state (:eng s)))))))
          (is (empty? (ran s 'jobs.village.breed)))
          (is (nil? (done-event s))))))))

(deftest a-stopped-child-makes-the-pass-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p"} (world) {'jobs.build.from-plan {:status :stopped :reason :incomplete :missing [[1 2 3]]}}))]
          (is (some? (:stopped (step s :repair))))
          (is (= :incomplete (:reason (done-event s))))
          (is (= [:stopped :incomplete] (ending s)))
          (is (not-any? #(= :maintain.gave-up (:kind %)) @(:seen s))))))))

(deftest a-pass-where-no-step-ran-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [leave! (fn [c _] (swap! (fake/state (:primitives c)) assoc :entities []) :continue)
              s (await (scenario {:roles [librarian]} (world (villager 1 3 {:profession "none"})) {'jobs.village.roll leave!}))]
          (is (= [:stopped :nothing-to-do] (ending s))))))))

(deftest a-child-that-reaches-a-limit-ends-the-pass-with-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p" :target 4} (apply world pair)
                                 {'jobs.build.from-plan {:status :stopped :kind :limit-reached :cause {:line :job-time :reason :timeout}}}))]
          (is (empty? (ran s 'jobs.village.breed)) "no later step is called")
          (is (= [:stopped :timeout] (ending s))))))))

(deftest it-is-a-contract-job
  (is (= [:plan :roles] maintain/target))
  (is (= {:spend {:job-time ::maintain/job-time-s}} maintain/defaults))
  (is (fn? maintain/done?))
  (is (fn? maintain/quote-of)))

(deftest a-clean-pass-has-no-stop-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p"} (world) {}))]
          (is (nil? (:reason (done-event s))))
          (is (= [:completed nil] (ending s)))
          (is (true? (finished? s))))))))

(deftest a-role-without-a-workstation-cell-stops-the-pass-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:roles [{:profession "librarian"}] :plan "p"} (apply world pair) {}))]
          (is (= {:skipped :no-pos} (step s :roll-0)))
          (is (empty? (ran s 'jobs.village.roll)))
          (is (= :incomplete (:reason (done-event s))))
          (is (re-find #"roll-0 no-pos" (:text (done-event s)))))))))

(deftest a-filled-role-beside-a-role-without-a-cell-ends-stopped-incomplete
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [roles [{:profession "farmer"} {:profession "librarian"}]
              s (await (scenario {:roles roles} (world (villager 1 3 {:profession "farmer"})) {}))]
          (is (= {:skipped :filled} (step s :roll-0)))
          (is (= {:skipped :no-pos} (step s :roll-1)))
          (is (= [:stopped :incomplete] (ending s))))))))

(deftest a-role-without-a-cell-alone-still-runs-the-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:roles [{:profession "librarian"}]} (apply world pair) {}))]
          (is (= :incomplete (:reason (done-event s))))
          (is (true? (finished? s))))))))

(deftest a-roll-goes-on-only-while-a-villager-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [leave! (fn [c n]
                       (swap! (fake/state (:primitives c)) assoc :entities [])
                       :continue)
              s (await (scenario {:roles [librarian]} (world (villager 1 3 {:profession "none"})) {'jobs.village.roll leave!}))]
          (is (= 1 (count (ran s 'jobs.village.roll))))
          (is (= {:skipped :no-villager} (step s :roll-0))))))))

(deftest a-passing-child-has-its-check-run-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p"} (world) {'jobs.build.from-plan (fn [_ _] {:placed 2})}))]
          (is (= 1 (count (filter #{[:check 'jobs.build.from-plan]} @(:calls s))))))))))

(deftest a-declined-child-has-its-reason-booked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p"} (world) {'jobs.build.from-plan {:decline :no-plan-found}}))]
          (is (= {:skipped :declined :reason :no-plan-found} (step s :repair))))))))

(deftest a-child-that-waits-on-the-world-is-resumed-and-ends-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p"} (world)
                                 {'jobs.build.from-plan (fn [_ n] (if (< n 3) :continue {:placed 2}))}))]
          (is (= 3 (count (ran s 'jobs.build.from-plan))))
          (is (= 2 (:placed (step s :repair))))
          (is (true? (finished? s))))))))

(deftest a-declined-child-passes-its-reason-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p" :target 4} (apply world pair) {'jobs.build.from-plan {:decline :no-plan-found}}))]
          (is (= {:skipped :declined :reason :no-plan-found} (step s :repair)))
          (is (= 1 (count (ran s 'jobs.village.breed))) "the pass goes on")
          (is (= :incomplete (:reason (done-event s))))
          (is (re-find #"repair" (:text (done-event s)))))))))

(deftest a-preempted-step-is-decided-again-from-the-world
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [grow! (fn [c n]
                      (when (= 1 n)
                        (run! #(fake/add-entity! (:primitives c) %) [(villager 3 6) (villager 4 7)]))
                      :continue)
              s (await (scenario {:target 3} (apply world pair) {'jobs.village.breed grow!}))]
          (is (= 1 (count (ran s 'jobs.village.breed))) "target reached while it waited: not called again")
          (is (= {:skipped :at-target} (step s :breed)))
          (is (true? (finished? s))))))))

(deftest a-roll-in-progress-continues-after-its-villager-took-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hire! (fn [c n]
                      (if (= 1 n)
                        (do (swap! (fake/state (:primitives c)) update :entities
                                   (partial mapv #(assoc % :profession "librarian")))
                            :continue)
                        {:rolled 1}))
              s (await (scenario {:roles [librarian]} (world (villager 1 3 {:profession "none"})) {'jobs.village.roll hire!}))]
          (is (= 2 (count (ran s 'jobs.village.roll))))
          (is (= 1 (:rolled (step s :roll-0)))))))))

(deftest ignore-zones-goes-to-the-repair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:plan "p" :ignore-zones? true} (world) {}))]
          (is (= [['jobs.build.from-plan {:plan "p" :ignore-zones? true}]] (ran s 'jobs.build.from-plan))))))))

(defn appear-on-look
  "p whose entities are all out of sight until its first look call."
  [p]
  (let [st (fake/state p)
        all (:entities @st)]
    (swap! st assoc :entities [])
    (.override (.-world p) "look" (fn ^:async f [token a impl]
                                    (swap! st assoc :entities all)
                                    (await (impl token a))))))

(deftest a-villager-behind-the-body-is-looked-for-before-the-role-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:roles [librarian]} (world (villager 1 3 {:profession "none" :pos {:x -3 :y 64 :z 0}})) {} 40 appear-on-look))]
          (is (= 1 (count (ran s 'jobs.village.roll)))))))))

(deftest only-a-condition-failed-or-impossible-ending-without-data-is-booked-declined
  (are [e booked] (= booked (maintain/entry-of e))
    {:kind :condition-failed :cause :no-plan-found} {:skipped :declined :reason :no-plan-found}
    {:kind :impossible :cause :no-plan-found} {:skipped :declined :reason :no-plan-found}
    {:kind :nothing-possible :cause :no-spot} {:reason :no-spot :stopped :nothing-possible}))
