(ns engine.animal-maintain-test
  "jobs.animals.maintain against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.core.schedule :as schedule]
            [engine.expr :as expr]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.hostile-test :as h]
            [engine.fake :as fake]
            [engine.settings :as settings]
            [engine.look-support :as ls]
            [engine.test-util :as tu]
            [engine.build-from-plan-test :as b]
            [engine.pen-build-test :as pb]
            [jobs.animals.maintain :as maintain]
            [plan.adopt :as adopt]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.pen :as pen]
            [jobs.lib.world-files :as ew]))

(def box {:min {:x 0 :y 60 :z -5} :max {:x 8 :y 70 :z 5}})

(defn spec [args] (list 'jobs.animals.maintain (merge {:box box :mob "cow"} args)))

(def cow-drops [{:name "beef" :count 1} {:name "leather" :count 1}])

(defn animal
  ([id name x] (animal id name x 0 {}))
  ([id name x z more] (merge {:id id :uuid (str "u" id) :name name :kind "passive" :pos {:x x :y 64 :z z}} more)))

(defn cow
  ([id x] (cow id x 0))
  ([id x z] (animal id "cow" x z {:drops cow-drops})))

(defn calf [id x] (animal id "cow" x 0 {:drops cow-drops :baby true}))

(defn wheat [n] {:name "wheat" :count n})

(defn drop-at
  "An item entity lying at x."
  [id name x]
  {:id id :name "item" :kind "item" :pos {:x x :y 64 :z 0} :item {:name name :count 1}})

(def leather-on-the-ground (drop-at 90 "leather" 4))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world 50)]
    (core/submit! (:eng s) (tu/order (spec args)) {})
    (await (settings/with-settings {:jobs.animals.breed/calf-wait-s 0} #(tu/run-ticks (:eng s) (:clock s) n 700)))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event
  "The job's ending event (job.completed or job.stopped)."
  [{:keys [seen]}]
  (first (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %))) @seen)))
(defn passes
  "How many passes of the repeated maintain ended done (job.child_ended of the first child level)."
  [s]
  (count (filter #(and (= 2 (count (:chain %))) (= :done (:status %))) (events-of s :child_ended))))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self p))))
(defn interacts [{:keys [p]}] (h/calls p "interact"))
(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))

(deftest two-adults-and-wheat-below-target-are-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 12))]
          (is (= ["wheat" "wheat"] (mapv #(.-item (.-args %)) (interacts s))))
          (is (empty? (attacked s)))
          (is (= 2 (get-in (done-event s) [:steps :breed :fed])))
          (is (true? (finished? s))))))))

(deftest one-call-runs-every-step-and-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3) (calf 3 4) leather-on-the-ground]} 1))]
          (is (true? (finished? s)))
          (is (= 2 (get-in (done-event s) [:steps :breed :fed])))
          (is (= 1 (get-in (done-event s) [:steps :collect :collected]))))))))

(deftest seven-adults-above-target-four-are-culled-and-the-drops-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory h/sword :entities (mapv #(cow % (+ 1 %)) (range 1 8))} 80))]
          (is (= 3 (count (set (attacked s)))))
          (is (= {:killed 3 :remaining 4 :reason :keep} (select-keys (get-in (done-event s) [:steps :cull]) [:killed :remaining :reason])))
          (is (= 3 (get (inv s) "beef")))
          (is (= 3 (get (inv s) "leather")))
          (is (true? (finished? s))))))))

(deftest the-floor-of-two-adults-is-kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:inventory h/sword :entities (mapv #(cow % (+ 1 %)) (range 1 4))} 60))]
          (is (= {:killed 1 :remaining 2 :reason :keep} (select-keys (get-in (done-event s) [:steps :cull]) [:killed :remaining :reason])))
          (is (= 2 (:adults (done-event s)))))))))

(deftest calves-count-toward-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3) (calf 3 2) (calf 4 2) leather-on-the-ground]} 12))]
          (is (empty? (interacts s)))
          (is (= {:skipped :at-target} (get-in (done-event s) [:steps :breed]))))))))

(deftest calves-and-surplus-adults-cull-to-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory h/sword :entities (into [(calf 6 2) (calf 7 2)] (map #(cow % (+ 1 %))) (range 1 6))} 80))]
          (is (= 3 (count (set (attacked s)))))
          (is (= 2 (:adults (done-event s))))
          (is (= 2 (:babies (done-event s)))))))))

(deftest without-food-the-breed-is-skipped-and-the-job-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4 :ways #{:carried}} {:entities [(cow 1 2) (cow 2 3) leather-on-the-ground]} 12))]
          (is (empty? (interacts s)))
          (is (= {:skipped :no-food} (get-in (done-event s) [:steps :breed])))
          (is (true? (finished? s))))))))

(deftest a-pass-that-lacks-food-it-may-not-fetch-says-so-not-nothing-to-do
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4 :ways #{:carried}} {:entities [(cow 1 2) (cow 2 3)]} 12))]
          (is (= {:skipped :no-food} (get-in (done-event s) [:steps :breed])))
          (is (= :stopped (:kind (done-event s))))
          (is (= :no-food (or (:cause (done-event s)) (get-in (done-event s) [:data :cause]))))
          (is (re-find #"lacks: no-food" (str (:text (done-event s)))) (pr-str (done-event s))))))))

(defn cooling-pen
  "Three cows in the box, two remembered fed within the breeding cooldown, target 4: no step can run."
  [spec]
  (let [s (h/setup {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3) (cow 3 4)]} 50)]
    (doseq [k ["u1" "u2"]] (mem/write! (:store (:eng s)) :breed/love {:key k} {:cap 50 :ttl 330000}))
    (core/submit! (:eng s) (tu/order spec) {})
    s))

(deftest adults-remembered-fed-within-the-breeding-cooldown-are-not-fed-and-the-pass-has-nothing-to-do
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (cooling-pen (spec {:target 4}))]
          (await (settings/with-settings {:jobs.animals.breed/calf-wait-s 0} #(tu/run-ticks (:eng s) (:clock s) 12 700)))
          (is (empty? (interacts s)) "the third cow alone is not fed: it has no partner")
          (is (= {:skipped :cooldown} (get-in (done-event s) [:steps :breed])))
          (is (= :stopped (:kind (done-event s))) "a pass that skipped every step below target is no done")
          (is (= :nothing-to-do (or (:cause (done-event s)) (get-in (done-event s) [:data :cause])))))))))

(deftest a-repeated-pass-with-every-step-skipped-rests-instead-of-passing-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (cooling-pen (list 'repeat (spec {:target 4})))]
          (await (tu/run-ticks (:eng s) (:clock s) 20 700))
          (is (= 1 (count (filter #(= 2 (count (:chain %))) (events-of s :child_ended)))) "one pass, then a rest")
          (is (some #(= :resting (:reason %)) (events-of s :waiting))))))))

(deftest a-child-that-fails-is-booked-with-its-kind-and-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 50)]
          (.override (.-world (:p s)) "interact"
                     (fn [_token _args _impl] (js/Promise.resolve #js {:status "out-of-reach"})))
          (core/submit! (:eng s) (tu/order (spec {:target 4})) {})
          (await (tu/run-ticks (:eng s) (:clock s) 30 700))
          (is (= {:ended :nothing-possible :cause :unreachable :fed 0 :reason :unreachable}
                 (get-in (done-event s) [:steps :breed])))
          (is (= :stopped (:kind (done-event s))))
          (is (= :failed-steps (or (:cause (done-event s)) (get-in (done-event s) [:data :cause]))))
          (is (true? (finished? s))))))))

(defn ^:async herd-ends
  "The pass of a body with one adult in a pen of target 4, whose herd child ends condition-failed for reason, more
  merged into its cause."
  [reason & [more]]
  (let [call ctx/call]
    (set! ctx/call (fn [c slot job args]
                     (if (= :herd slot)
                       (js/Promise.resolve (contract/ending :condition-failed (merge {:reason reason :text "stub"} more) nil))
                       (call c slot job args))))
    (try (await (scenario {:target 4} {:entities [(cow 1 2)]} 12))
         (finally (set! ctx/call call)))))

(deftest a-herd-that-waits-for-light-is-waited-for-not-booked-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (herd-ends :dark))]
          (is (nil? (done-event s)) "the pass has not ended")
          (is (false? (finished? s))))))))

(deftest a-herd-that-fails-for-another-reason-ends-the-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (herd-ends :unreachable))]
          (is (= :stopped (:kind (done-event s))))
          (is (= :unreachable (get-in (done-event s) [:steps :herd :cause]))))))))

(deftest a-leaky-herd-names-the-leaks-in-the-report-and-the-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [leaks [{:pos {:x 16 :y 64 :z 3} :why :gap}]
              s (await (herd-ends :leaky {:leaks leaks}))]
          (is (= leaks (get-in (done-event s) [:steps :herd :leaks])))
          (is (re-find #"gap at \[16 64 3\]" (str (:text (done-event s)))) (pr-str (done-event s))))))))

(deftest one-adult-cannot-breed-herd-is-asked-for-another
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (calf 2 2) leather-on-the-ground]} 12))]
          (is (= {:skipped :too-few-adults} (get-in (done-event s) [:steps :breed])))
          (is (some? (get-in (done-event s) [:steps :herd :ended])) "herd ran and ended short (the test pen has no gate)")
          (is (= :stopped (:kind (done-event s))) "a failed herd that brought nobody is no done")
          (is (= (get-in (done-event s) [:steps :herd :cause]) (or (:cause (done-event s)) (get-in (done-event s) [:data :cause])))))))))

(deftest herd-is-asked-for-a-pair-and-the-strays-worth-bringing-up-to-the-target
  (are [expected target adults babies strays] (= expected (maintain/decide :herd {:mob "cow" :box box :target target} {:adults adults :babies babies :strays strays}))
    {:skip :no-strays} 4 2 0 0
    {:skip :at-target} 0 0 0 0
    {:skip :at-target} 1 1 0 3
    {:skip :at-target} 4 5 0 2
    {:skip :at-target} 8 5 3 1
    {:call {:slot :herd :job 'jobs.animals.herd :args {:mob "cow" :box box :target 2 :radius 64}}} 4 1 0 0
    {:call {:slot :herd :job 'jobs.animals.herd :args {:mob "cow" :box box :target 1 :radius 64}}} 1 0 0 0
    {:call {:slot :herd :job 'jobs.animals.herd :args {:mob "cow" :box box :target 3 :radius 64}}} 4 2 0 1
    {:call {:slot :herd :job 'jobs.animals.herd :args {:mob "cow" :box box :target 4 :radius 64}}} 4 2 0 5
    {:call {:slot :herd :job 'jobs.animals.herd :args {:mob "cow" :box box :target 3 :radius 64}}} 4 2 1 5))

(deftest escaped-adults-outside-the-pen-are-herded-back-up-to-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [call ctx/call
              asked (atom nil)]
          (set! ctx/call (fn [c slot job args]
                           (if (= :herd slot)
                             (do (reset! asked args) (js/Promise.resolve (contract/ending :condition-failed {:reason :no-gate :text "stub"} nil)))
                             (call c slot job args))))
          (try (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3) (cow 3 12 0) (cow 4 13 1) (cow 5 14 -1)]} 12))
               (finally (set! ctx/call call)))
          (is (= 4 (:target @asked)) "two penned, three strays near: herd brings the pen up to 4, not just a pair"))))))

(deftest items-inside-the-box-are-collected-and-those-outside-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:entities [(cow 1 2) (cow 2 3) (cow 3 4) (cow 4 5) (drop-at 90 "leather" 4) (drop-at 91 "beef" 7) (drop-at 92 "dirt" 12)]} 20))]
          (is (= {"leather" 1 "beef" 1} (inv s)))
          (is (= 2 (get-in (done-event s) [:steps :collect :collected])))
          (is (= #{92} (set (map :id (filter #(= "item" (:kind %)) (fake/entities (:p s))))))))))))

(defn sheep [id x] (animal id "sheep" x 0 {}))

(deftest sheep-are-sheared-with-shears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "sheep" :target 2} {:inventory [{:name "shears" :count 1}] :entities [(sheep 1 2) (sheep 2 3)]} 20))]
          (is (= 2 (get (inv s) "white_wool")))
          (is (= 2 (get-in (done-event s) [:steps :shear :shorn])))
          (is (true? (finished? s))))))))

(deftest no-shears-no-shearing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "sheep" :target 2 :ways #{:carried}} {:entities [(sheep 1 2) (sheep 2 3) leather-on-the-ground]} 20))]
          (is (= {:skipped :no-shears} (get-in (done-event s) [:steps :shear]))))))))

(deftest cows-are-not-sheared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 2} {:inventory [{:name "shears" :count 1}] :entities [(cow 1 2) (cow 2 3) leather-on-the-ground]} 20))]
          (is (= {:skipped :not-sheep} (get-in (done-event s) [:steps :shear]))))))))

(def chest {:x 10 :y 64 :z 0})

(def four-cows [(cow 1 2) (cow 2 3) (cow 3 4) (cow 4 5)])

(def carried [{:name "beef" :count 3} {:name "leather" :count 2} (wheat 5) {:name "iron_sword" :count 1}])

(defn chest-items [{:keys [p]}]
  (into {} (map (juxt :name :count)) (get-in @(fake/state p) [:containers [10 64 0]])))

(deftest produce-goes-to-the-chest-and-food-and-tools-stay
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4 :chest chest} {:inventory carried :entities four-cows :containers {"10,64,0" []}} 20))]
          (is (= {"leather" 2} (chest-items s)))
          (is (= {"beef" 3 "wheat" 5 "iron_sword" 1} (inv s)) "the beef is the body's food reserve")
          (is (= {:gave-up false} (select-keys (get-in (done-event s) [:steps :deposit]) [:gave-up])))
          (is (true? (finished? s))))))))

(deftest keep-leaves-some-produce-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4 :chest chest :carry {"leather" 1}} {:inventory carried :entities four-cows :containers {"10,64,0" []}} 20))]
          (is (= {"leather" 1} (chest-items s)))
          (is (= 1 (get (inv s) "leather"))))))))

(deftest produce-the-reserve-register-holds-stays-carried-with-no-carry-arg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory carried :entities four-cows :containers {"10,64,0" []}} 50)]
          (core/submit! (:eng s) (tu/order '(jobs.memory.reserve {:item "leather" :count 2})) {})
          (await (tu/run-ticks (:eng s) (:clock s) 5 700))
          (core/submit! (:eng s) (tu/order (spec {:target 4 :chest chest})) {})
          (await (settings/with-settings {:jobs.animals.breed/calf-wait-s 0} #(tu/run-ticks (:eng s) (:clock s) 20 700)))
          (is (= {} (chest-items s)))
          (is (= 2 (get (inv s) "leather"))))))))

(deftest without-a-chest-the-produce-stays-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory carried :entities (conj four-cows leather-on-the-ground)} 20))]
          (is (= 3 (get (inv s) "beef")))
          (is (= {:skipped :no-chest} (get-in (done-event s) [:steps :deposit])))
          (is (true? (finished? s))))))))

(defn declines?
  "True when the job is submitted, the tick after its first look does nothing and nothing is walked to, fed or attacked."
  [args entities]
  (let [{:keys [eng p] :as s} (h/setup {:inventory [(wheat 4) {:name "iron_sword" :count 1}] :entities entities})]
    (core/submit! eng (tu/order (list 'jobs.animals.maintain args)) {})
    (core/tick! eng)
    (and (nil? (core/tick! eng))
         (empty? (interacts s))
         (empty? (attacked s))
         (zero? (count (h/calls p "moveTo"))))))

(deftest the-check-declines-when-nothing-needs-doing
  (is (declines? {:box box :target 4} four-cows) "at target")
  (is (declines? {:box box :target 2} [(animal 1 "pig" 2) (animal 2 "pig" 3) (animal 3 "pig" 4)]) "other kinds only")
  (is (declines? {:box box :target 4} (conj four-cows (cow 5 20) (cow 6 30) (cow 7 40))) "cows outside the box are not counted"))

(deftest a-full-run-hands-over-its-result-as-data
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory h/sword :entities (mapv #(cow % (+ 1 %)) (range 1 6))} 60))
              r (done-event s)]
          (is (= {:mob "cow" :target 4 :adults 4 :babies 0} (select-keys r [:mob :target :adults :babies])))
          (is (= [:mend :herd :breed :cull :shear :collect :deposit] (vec (keys (:steps r)))))
          (is (= {:mend {:skipped :sound} :herd {:skipped :at-target} :breed {:skipped :at-target} :shear {:skipped :not-sheep} :deposit {:skipped :no-chest}}
                 (select-keys (:steps r) [:mend :herd :breed :shear :deposit]))))))))

(deftest the-pure-pieces-decide-from-facts
  (are [expected inventory produce reserve] (= expected (maintain/to-store inventory produce reserve))
    ["beef"] [{:name "beef" :count 2} {:name "wheat" :count 1}] ["beef" "leather"] {}
    [] [{:name "beef" :count 1}] ["beef"] {"beef" 1}
    ["beef"] [{:name "beef" :count 64} {:name "beef" :count 3}] ["beef"] {"beef" 66}
    [] [{:name "iron_sword" :count 1}] ["beef"] {})
  (are [expected facts] (= expected (:skip (maintain/decide :breed {:mob "cow" :target 4 :ways #{:carried}} facts)))
    :at-target {:adults 3 :babies 1}
    :too-few-adults {:adults 1 :babies 0}
    :cooldown {:adults 3 :ready 1 :babies 0 :food "wheat"}
    :no-food {:adults 2 :ready 2 :babies 0 :food nil}
    nil {:adults 2 :ready 2 :babies 0 :food "wheat" :reach 5})
  (is (= {:call {:slot :breed :job 'jobs.animals.breed :args {:mob "cow" :pairs 1 :radius 5 :box box :ways #{:carried :chest}}}}
         (maintain/decide :breed {:mob "cow" :target 4 :box box :ways #{:carried :chest}} {:adults 2 :ready 2 :babies 0 :food nil :reach 5}))
      "no food but ways allow a fetch: breed is called with the pen and fetches")
  (is (= {:call {:slot :shear :job 'jobs.animals.shear :args {:radius 5 :ways #{:carried :chest}}}}
         (maintain/decide :shear {:mob "sheep" :ways #{:carried :chest}} {:shears false :shearable 2 :reach 5}))
      "no shears but ways allow a fetch: shear is called and fetches")
  (is (= :no-food (:skip (maintain/decide :breed {:mob "cow" :target 4 :ways #{:carried}} {:adults 2 :ready 2 :babies 0 :food nil :reach 5})))
      "ways #{:carried} is no leave to fetch food")
  (is (= :no-shears (:skip (maintain/decide :shear {:mob "sheep" :ways #{:carried}} {:shears false :shearable 2 :reach 5})))
      "ways #{:carried} is no leave to fetch shears")
  (are [expected facts] (= expected (:skip (maintain/decide :cull {:mob "cow" :target 4} facts)))
    :within-target {:adults 4 :babies 0}
    :within-target {:adults 2 :babies 3}
    nil {:adults 5 :babies 0}
    nil {:adults 5 :babies 2}))

(deftest a-seen-leaky-pen-is-mended-first
  (are [expected args facts] (= expected (maintain/decide :mend args facts))
    {:call {:slot :mend :job 'jobs.animals.pen-check :args {:box box}}} {:box box} {:gaps [[1 64 1]]}
    {:call {:slot :mend :job 'jobs.animals.pen-check :args {:box box :ways #{:check :patch}}}} {:box box :ways #{:carried}} {:gaps [[1 64 1]]}
    {:skip :sound} {:box box} {:gaps []}
    {:skip :sound} {:plan "pen"} {:gaps []}
    {:call {:slot :mend :job 'jobs.build.pen :args {:plan "pen"}}} {:plan "pen"} {:gaps [[1 64 1]]}
    {:call {:slot :mend :job 'jobs.build.pen :args {:plan "pen" :ways #{:carried}}}} {:plan "pen" :ways #{:carried}} {:gaps [[1 64 1]]})
  (let [args {:plan "pen" :mob "cow" :target 4}
        facts {:adults 4 :babies 0 :drops [] :shearable 0 :gaps [[1 64 1]] :stored-items []}]
    (is (= {:plan "pen" :worth (maintain/mend-worth args facts)} (get-in (maintain/decide :mend args facts) [:call :args]))
        "the mend is built at the pass's worth")))

(deftest a-trigger-fired-maintain-is-priced-at-its-own-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (h/setup {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)] :time 1000} 50)
              _ (core/submit! eng (tu/order (spec {:target 4 :checks {:safe? '(always)}})) {})
              _ (await (tu/run-ticks eng clock 1 700))
              node (schedule/priced eng {:id "t1" :reflex :animals} (expr/parse (:jobs eng) (spec {:target 4 :checks {:safe? '(always)}})))]
          (is (= (cost/animal-worth "cow") (get-in node [:args :worth]))
              "a pair below a target of 4: the pass breeds one calf, worth that calf"))))))

(deftest a-fence-gap-and-carried-produce-add-to-the-pass-worth
  (let [args {:mob "cow" :target 4 :chest {:x 0 :y 64 :z 0}}
        at-target {:adults 4 :babies 0 :drops [] :shearable 0 :gaps [] :stored-items []}
        worth #(maintain/mend-worth args (merge at-target %))
        beef {:stored-items [{:name "beef" :count 8}]}]
    (is (zero? (worth {})))
    (is (< 0 (worth {:gaps [[1 64 1]]})) "a gap loses animals")
    (is (< (worth {:gaps [[1 64 1]]}) (worth {:gaps [[1 64 1] [2 64 1]]})))
    (is (= (* 2 (cost/gap-loss "cow" 2 1)) (cost/gap-loss "cow" 4 1)) "escapes grow with the herd")
    (is (< (cost/gap-loss "cow" 4 1) (* 4 (cost/animal-worth "cow"))) "never more than the herd")
    (settings/with-settings {::pen/escape-prior {:escapes 1 :animal-s 300}}
      #(do (is (< (js/Math.abs (- 0.5 (pen/escape-chance 1 300))) 1e-9) "one escape per 300 animal-seconds: half gone after 300 s at one gap")
           (is (= (pen/escape-chance 2 150) (pen/escape-chance 1 300)) "gaps and seconds trade evenly")))
    (is (< 0 (worth beef)) "produce to store is worth its items")
    (is (zero? (maintain/mend-worth (dissoc args :chest) (merge at-target beef))) "no chest, no deposit: carried produce is worth nothing here")))

(def ring-pen-plan
  (update (pb/ring-plan) :parts conj {:id "cows" :box [[3 64 3] [3 64 3]] :want "air" :herd {:mob "cow" :target 0}}))

(deftest a-fence-cell-seen-blown-away-is-built-again-by-the-first-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (b/start (pb/spec pb/kit (assoc pb/built-pen "4,64,3" "air")) {"pen" ring-pen-plan} [])
              result (await (tu/child-outcome eng 'jobs.animals.maintain {:plan "pen" :part "cows"} 600))]
          (is (not= :nothing-to-do (:reason result)) "the gap starts a run"))
        (let [{:keys [eng]} (b/start (pb/spec pb/kit pb/built-pen) {"pen" ring-pen-plan} [])
              result (await (tu/child-outcome eng 'jobs.animals.maintain {:plan "pen" :part "cows"} 600))]
          (is (= :sound (get-in result [:ending :cause :skipped :mend]))))
        (let [plan (assoc-in ring-pen-plan [:parts 2 :herd :target] 1)
              {:keys [eng p]} (b/start (assoc (pb/spec pb/kit pb/built-pen) :states {"3,64,2" {:open true :facing "north"}}
                                              :entities [(cow 1 3.5 3.5)])
                                       {"pen" plan} [])
              result (await (tu/child-outcome eng 'jobs.animals.maintain {:plan "pen" :part "cows"} 600))]
          (is (= {:closed? true :reason nil} (get-in result [:steps :mend])) (pr-str result))
          (is (= "false" (str (some-> (.blockAt p #js {:x 3 :y 64 :z 2}) .-properties .-open))) "a plan gate seen open, held by no job, is mended shut"))
        (let [plan (assoc-in ring-pen-plan [:parts 2 :herd :target] 1)
              {:keys [eng]} (b/start (assoc (pb/spec pb/kit pb/built-pen) :states {"3,64,2" {:open true :facing "north"}}
                                            :entities [(cow 1 3.5 3.5)])
                                     {"pen" plan} [])
              _ (mem/write! (:store eng) :gate-gave-up {:cell [3 64 2] :reason :unreachable})
              result (await (tu/child-outcome eng 'jobs.animals.maintain {:plan "pen" :part "cows"} 600))]
          (is (= :sound (get-in result [:ending :cause :skipped :mend])) "a gate the shut job gave up on is no gap"))))))

;; card f0d2a35a: a box pen with no plan said "mend skipped no-plan" while herd waited on the leak for good
(deftest a-box-pen-with-no-plan-is-patched-from-the-read
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ring (into {} (for [x (range -1 6) z (range -1 6) :when (or (#{-1 5} x) (#{-1 5} z))] [(str x ",64," z) "oak_fence"]))
              {:keys [eng p]} (b/start (assoc (pb/spec pb/kit (assoc ring "5,64,2" "air")) :entities [(cow 1 1.5 1.5) (cow 2 3.5 3.5)]) {} [])
              result (await (tu/child-outcome eng 'jobs.animals.maintain {:box {:min [0 64 0] :max [4 64 4]} :mob "cow" :target 2} 600))]
          (is (= :done (:status result)) (pr-str result))
          (is (= "oak_fence" (some-> (.blockAt p #js {:x 5 :y 64 :z 2}) .-name)) "the gap is fenced"))))))

;; card 964be2d5: a box pen's gate seen open was shut only when herd happened to pass it
(deftest a-box-pen-gate-seen-open-is-shut-by-the-mend
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ring (into {} (for [x (range -1 6) z (range -1 6) :when (or (#{-1 5} x) (#{-1 5} z))] [(str x ",64," z) "oak_fence"]))
              {:keys [eng p]} (b/start (assoc (pb/spec pb/kit (assoc ring "5,64,2" "oak_fence_gate"))
                                              :states {"5,64,2" {:open true :facing "east"}}
                                              :entities [(cow 1 1.5 1.5) (cow 2 3.5 3.5)])
                                       {} [])
              result (await (tu/child-outcome eng 'jobs.animals.maintain {:box {:min [0 64 0] :max [4 64 4]} :mob "cow" :target 2} 600))]
          (is (= :done (:status result)) (pr-str result))
          (is (= "false" (str (some-> (.blockAt p #js {:x 5 :y 64 :z 2}) .-properties .-open))) "the open gate is shut"))))))

;; run4 F42: every fence stood, so mend said sound while herd's read of the box found climbs over the fence from a mound
(deftest a-block-in-the-pen-an-animal-climbs-the-fence-from-is-mended-as-herd-reads-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (update-in ring-pen-plan [:parts 2] assoc :box [[3 64 3] [3 65 3]] :herd {:mob "cow" :target 2})
              {:keys [eng p]} (b/start (pb/spec pb/kit (assoc pb/built-pen "3,64,3" "dirt")) {"pen" plan} [])
              result (await (tu/child-outcome eng 'jobs.animals.maintain {:plan "pen" :part "cows"} 600))]
          (is (= {:closed? true :reason nil} (get-in result [:steps :mend])) (pr-str result))
          (is (= "air" (some-> (.blockAt p #js {:x 3 :y 64 :z 3}) .-name)) "the mound is dug"))))))

(deftest the-check-declines-when-the-only-drops-lie-outside-the-box
  (is (declines? {:box box :target 4} (conj four-cows (drop-at 90 "leather" 9)))))

(deftest a-pen-at-target-with-nothing-to-do-is-done-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 2} {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3) (calf 3 4)]} 3))]
          (is (finished? s))
          (is (empty? (interacts s)))
          (is (= {:adults 2 :babies 1} (select-keys (done-event s) [:adults :babies]))))))))

(defn track-range!
  "Entities farther than r from the body are not listed, as a server loads animals only near the player."
  [{:keys [p]} r]
  (let [orig (.-entities p)]
    (set! (.-entities p) (fn [a] (.filter (orig a) (fn [e] (<= (.-distance e) r)))))))

(deftest a-body-outside-the-box-that-lists-no-animal-walks-to-the-box-and-counts-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (h/setup {:self {:pos {:x 40 :y 64 :z 0}} :entities [(cow 1 2) (cow 2 3) (cow 3 4) (cow 4 5)]} 50)]
          (track-range! s 20)
          (core/submit! eng (tu/order (spec {:target 4})) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (true? (finished? s)))
          (is (= 4 (:adults (done-event s)))))))))

(deftest a-body-outside-the-box-that-lists-fewer-animals-than-the-target-walks-to-the-box-and-counts-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (h/setup {:self {:pos {:x 20 :y 64 :z 0}} :entities [(cow 1 2) (cow 2 3) (cow 3 4) (cow 4 8)]} 50)]
          (track-range! s 14)
          (core/submit! eng (tu/order (spec {:target 4})) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (true? (finished? s)))
          (is (= 4 (:adults (done-event s)))))))))

(deftest a-body-outside-the-box-sees-nothing-there-after-walking-to-it-and-ends-with-an-empty-census
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (h/setup {:self {:pos {:x 40 :y 64 :z 0}} :entities []} 50)]
          (core/submit! eng (tu/order (spec {:target 4})) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (true? (finished? s)))
          (is (< (.-x (.-pos (.self (:p s)))) 12) "it walked to the box to look")
          (is (= 0 (:adults (done-event s)))))))))


(deftest a-look-walk-that-gives-up-ends-the-pass-and-herds-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (h/setup {:floor [30 -3 45 3] :self {:pos {:x 40 :y 64 :z 0}} :entities []} 50)]
          (core/submit! eng (tu/order (spec {:target 4})) {})
          (await (tu/run-ticks eng clock 80 700))
          (is (true? (finished? s)))
          (is (nil? (:steps (done-event s))) "no step ran on an uncounted pen")
          (is (not= :completed (:kind (done-event s)))))))))

(def pen-plan
  {:id "pen" :metadata {:by "Fake"} :parts [{:id "cows" :box [[0 60 -5] [8 70 5]] :want "air" :herd {:mob "cow" :target 4}}]})

(defn ^:async plan-scenario
  "Submit form over a world whose plans are plans; run n ticks 700 ms apart; the setup map."
  [form plans world n]
  (let [s (h/setup world 50 (ew/of-data plans {} []))]
    (core/submit! (:eng s) (tu/order form) {})
    (await (settings/with-settings {:jobs.animals.breed/calf-wait-s 0} #(tu/run-ticks (:eng s) (:clock s) n 700)))
    s))

(deftest a-pen-part-of-a-plan-gives-the-box-the-kind-and-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (plan-scenario (list 'jobs.animals.maintain {:plan "pen" :part "cows"}) {"pen" pen-plan}
                                      {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 12))]
          (is (= ["wheat" "wheat"] (mapv #(.-item (.-args %)) (interacts s))))
          (is (= {:mob "cow" :target 4} (select-keys (done-event s) [:mob :target])))
          (is (true? (finished? s))))))))

(deftest an-adopted-pen-part-gives-the-box-the-kind-and-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (:plan (adopt/adopt {:id "pen" :box [[0 60 -5] [0 60 -5]] :body "Fake" :date "d"
                                        :block-at {[0 60 -5] {:name "oak_fence"}}
                                        :pen {:box [[0 60 -5] [8 70 5]] :mob "cow" :target 4}}))
              s (await (plan-scenario (list 'jobs.animals.maintain {:plan "pen" :part "pen"}) {"pen" (assoc plan :metadata {:by "Fake"})}
                                      {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 12))]
          (is (= {:mob "cow" :target 4} (select-keys (done-event s) [:mob :target]))))))))

(deftest a-surface-plan-puts-the-pen-box-on-the-ground-like-its-fences
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (assoc pen-plan :surface true :parts [{:id "cows" :box [[0 60 -5] [8 60 5]] :want "air" :herd {:mob "cow" :target 2}}])
              s (await (plan-scenario (list 'jobs.animals.maintain {:plan "pen" :part "cows"}) {"pen" plan}
                                      {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 12))]
          (is (= [:nothing-to-do] (distinct (map :reason (events-of s :waiting)))) "the cows on the ground are in the box that rose to it"))))))

(deftest a-missing-plan-starts-no-run-and-the-wait-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (plan-scenario (list 'jobs.animals.maintain {:plan "none" :part "cows"}) {"pen" pen-plan}
                                      {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 6))]
          (is (empty? (interacts s)))
          (is (= [:plan-trouble] (distinct (map :reason (events-of s :waiting))))))))))

(deftest a-part-that-is-no-pen-starts-no-run-and-ends-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (plan-scenario (list 'jobs.animals.maintain {:plan "pen" :part "nowhere"}) {"pen" pen-plan}
                                      {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 6))]
          (is (empty? (interacts s)))
          (is (empty? (events-of s :waiting)))
          (is (true? (finished? s))))))))

(deftest a-pass-that-leaves-a-step-to-run-keeps-no-rest-of-its-own
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (plan-scenario (list 'repeat (list 'jobs.animals.maintain {:plan "pen" :part "cows"})) {"pen" pen-plan}
                                      {:inventory [(wheat 8)] :entities [(cow 1 2) (cow 2 3) leather-on-the-ground]} 60))]
          (is (< 1 (count (filter #(= 2 (count (:chain %))) (events-of s :child_ended)))) "the next pass starts without a 10-minute rest")
          (is (empty? (filter :backed-off (events-of s :waiting)))))))))

(def far-box {:min {:x 200 :y 60 :z -5} :max {:x 208 :y 70 :z 5}})

(deftest a-pen-far-away-runs-when-the-pass-is-worth-the-least-walk-and-waits-not-worth-when-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [d (js/Math.round (/ (cost/animal-worth "cow") cost/least-block-s))
              world {:inventory [(wheat 4)] :entities [(cow 1 (+ d 2)) (cow 2 (+ d 3))] :time 1000}
              far (h/setup world 50)
              farther (h/setup world 50)]
          (core/submit! (:eng far) (tu/order (spec {:box {:min {:x d :y 60 :z -5} :max {:x (+ d 8) :y 70 :z 5}} :checks {:safe? '(always)}})) {})
          (core/submit! (:eng farther) (tu/order (spec {:box {:min {:x (* 4 d) :y 60 :z -5} :max {:x (+ (* 4 d) 8) :y 70 :z 5}} :checks {:safe? '(always)}})) {})
          (await (tu/run-ticks (:eng far) (:clock far) 3 700))
          (await (tu/run-ticks (:eng farther) (:clock farther) 3 700))
          (is (seq (events-of far :round_started)) "a walk of one cow's worth is worth the pair a pass adds")
          (is (empty? (events-of farther :round_started)))
          (is (some #(= :not-worth (:reason %)) (events-of farther :waiting))))))))

(deftest a-pen-under-repeat-far-off-and-unseen-is-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory [(wheat 4)] :entities [(cow 1 42) (cow 2 43)] :time 1000 :light {} :light-default [0 15] :floor [-5 -8 60 8]} 50)]
          (tu/seeing-within (:p s) 24)
          (let [p (:p s) entities (.-entities p)]
            (aset p "entities" (fn [q] (let [me (.-pos (.self p))]
                                         (.filter (.call entities p q) (fn [e] (<= (js/Math.hypot (- (.-x (.-pos e)) (.-x me)) (- (.-z (.-pos e)) (.-z me))) 24)))))))
          (core/submit! (:eng s) (tu/order (list 'repeat (spec {:box {:min {:x 40 :y 63 :z -3} :max {:x 46 :y 66 :z 3}} :target 2 :checks {:safe? (quote (always))}}))) {})
          (await (tu/run-ticks (:eng s) (:clock s) 40 700))
          (is (seq (tu/walk-calls (:p s))) "the body goes to the pen"))))))

(deftest animals-behind-the-body-in-the-box-are-looked-for-before-the-pass-concludes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 50)]
          (ls/entities-after-look (:p s))
          (core/submit! (:eng s) (tu/order (spec {:target 4})) {})
          (await (tu/run-ticks (:eng s) (:clock s) 20 700))
          (is (= ["wheat" "wheat"] (mapv #(.-item (.-args %)) (interacts s)))))))))

(deftest a-pass-that-ends-at-a-far-chest-reports-the-pen-it-counted-in-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (h/setup {:self {:pos {:x 20 :y 64 :z 0}} :inventory carried :entities four-cows :containers {"50,64,0" []}} 50)]
          (track-range! s 14)
          (core/submit! eng (tu/order (spec {:target 4 :chest {:x 50 :y 64 :z 0}})) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (= [{:x 50 :y 64 :z 0}] (filterv #(= 50 (:x %)) (tu/walked-to eng))) "the pass looked at the pen first, then went to the chest")
          (is (< 40 (.-x (.-pos (.self (:p s))))) "the deposit took the body out of listing range of the pen")
          (is (= {:kind :completed :adults 4 :babies 0} (select-keys (done-event s) [:kind :adults :babies]))))))))

(deftest a-body-inside-a-large-box-that-sees-only-some-of-it-walks-to-look-before-breeding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock p] :as s} (h/setup {:self {:pos {:x 68 :y 64 :z 0}} :inventory [(wheat 4)] :floor [-5 -8 80 8]
                                                    :entities [(cow 1 66) (cow 2 67) (cow 3 2) (cow 4 3)]} 50)
              entities (.-entities p)]
          (aset p "entities" (fn [q] (let [here (.-x (.-pos (.self p)))]
                                       (.filter (.call entities p q) (fn [e] (<= (js/Math.abs (- (.-x (.-pos e)) here)) 40))))))
          (core/submit! eng (tu/order (spec {:target 4 :box {:min {:x 0 :y 60 :z -5} :max {:x 70 :y 70 :z 5}}})) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (< (.-x (.-pos (.self p))) 60) "it walked to the middle of the box to count")
          (is (empty? (interacts s)) "four cows in the box are at target; the two listed from its corner are no reason to breed"))))))

(deftest a-step-decided-after-the-body-left-the-pen-counts-what-the-pass-saw-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [call ctx/call
              bred (atom nil)
              {:keys [eng clock p] :as s} (h/setup {:inventory [(wheat 4)] :entities (conj four-cows (cow 5 12))} 50)]
          (track-range! s 14)
          (set! ctx/call (fn [c slot job args]
                           (case slot
                             :herd (do (fake/swap-self! p assoc :pos [50 64 0])
                                       (js/Promise.resolve (contract/ending :done nil {:inside 4 :brought 0})))
                             :breed (do (reset! bred args) (js/Promise.resolve (contract/ending :done nil {:fed [] :reason :fed})))
                             (call c slot job args))))
          (try (core/submit! eng (tu/order (spec {:target 5})) {})
               (await (tu/run-ticks eng clock 20 700))
               (finally (set! ctx/call call)))
          (is (some? @bred) "four counted in the pen below a target of 5: breed runs though the herd left the body out of listing range")
          (is (= {:adults 4 :babies 0 :in-sight? false} (select-keys (first (events-of s :maintain.pass)) [:adults :babies :in-sight?]))))))))
