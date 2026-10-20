(ns engine.animal-maintain-test
  "jobs.animals.maintain against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.fake :as fake]
            [engine.look-support :as ls]
            [engine.test-util :as tu]
            [engine.build-from-plan-test :as b]
            [engine.pen-build-test :as pb]
            [jobs.animals.maintain :as maintain]
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
    (core/submit! (:eng s) (spec args) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
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
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
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

(deftest a-child-that-fails-is-booked-with-its-kind-and-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 50)]
          (.override (.-world (:p s)) "interact"
                     (fn [_token _args _impl] (js/Promise.resolve #js {:status "out-of-reach"})))
          (core/submit! (:eng s) (spec {:target 4}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 30 700))
          (is (= {:ended :nothing-possible :cause :unreachable :fed 0 :reason :unreachable}
                 (get-in (done-event s) [:steps :breed])))
          (is (= :stopped (:kind (done-event s))))
          (is (= :failed-steps (or (:cause (done-event s)) (get-in (done-event s) [:data :cause]))))
          (is (true? (finished? s))))))))

(deftest one-adult-cannot-breed-herd-is-asked-for-another
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (calf 2 2) leather-on-the-ground]} 12))]
          (is (= {:skipped :too-few-adults} (get-in (done-event s) [:steps :breed])))
          (is (some? (get-in (done-event s) [:steps :herd :ended])) "herd ran and ended short (the test pen has no gate)"))))))

(deftest herd-is-asked-for-a-pair-or-the-target-if-less
  (are [expected target adults] (= expected (maintain/decide :herd {:mob "cow" :box box :target target} {:adults adults}))
    {:skip :at-pair} 4 2
    {:skip :at-pair} 0 0
    {:skip :at-pair} 1 1
    {:call {:slot :herd :job 'jobs.animals.herd :args {:mob "cow" :box box :target 2}}} 4 1
    {:call {:slot :herd :job 'jobs.animals.herd :args {:mob "cow" :box box :target 1}}} 1 0))

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
          (is (= {"beef" 3 "leather" 2} (chest-items s)))
          (is (= {"wheat" 5 "iron_sword" 1} (inv s)))
          (is (= {:gave-up false} (select-keys (get-in (done-event s) [:steps :deposit]) [:gave-up])))
          (is (true? (finished? s))))))))

(deftest keep-leaves-some-produce-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4 :chest chest :carry {"beef" 1}} {:inventory carried :entities four-cows :containers {"10,64,0" []}} 20))]
          (is (= {"beef" 2 "leather" 2} (chest-items s)))
          (is (= 1 (get (inv s) "beef"))))))))

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
    (core/submit! eng (list 'jobs.animals.maintain args) {})
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
          (is (= {:mend {:skipped :no-plan} :herd {:skipped :at-pair} :breed {:skipped :at-target} :shear {:skipped :not-sheep} :deposit {:skipped :no-chest}}
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
    :no-food {:adults 2 :babies 0 :food nil}
    nil {:adults 2 :babies 0 :food "wheat" :reach 5})
  (is (= {:call {:slot :breed :job 'jobs.animals.breed :args {:mob "cow" :count 2 :radius 5 :box box :ways #{:carried :chest}}}}
         (maintain/decide :breed {:mob "cow" :target 4 :box box :ways #{:carried :chest}} {:adults 2 :babies 0 :food nil :reach 5}))
      "no food but ways allow a fetch: breed is called with the pen and fetches")
  (is (= {:call {:slot :shear :job 'jobs.animals.shear :args {:radius 5 :ways #{:carried :chest}}}}
         (maintain/decide :shear {:mob "sheep" :ways #{:carried :chest}} {:shears false :shearable 2 :reach 5}))
      "no shears but ways allow a fetch: shear is called and fetches")
  (is (= :no-food (:skip (maintain/decide :breed {:mob "cow" :target 4 :ways #{:carried}} {:adults 2 :babies 0 :food nil :reach 5})))
      "ways #{:carried} is no leave to fetch food")
  (is (= :no-shears (:skip (maintain/decide :shear {:mob "sheep" :ways #{:carried}} {:shears false :shearable 2 :reach 5})))
      "ways #{:carried} is no leave to fetch shears")
  (are [expected facts] (= expected (:skip (maintain/decide :cull {:mob "cow" :target 4} facts)))
    :within-target {:adults 4 :babies 0}
    :within-target {:adults 2 :babies 3}
    nil {:adults 5 :babies 0}
    nil {:adults 5 :babies 2}))

(deftest a-seen-leaky-pen-of-a-plan-is-mended-first
  (are [expected args facts] (= expected (maintain/decide :mend args facts))
    {:skip :no-plan} {} {:gaps [[1 64 1]]}
    {:skip :sound} {:plan "pen"} {:gaps []}
    {:call {:slot :mend :job 'jobs.build.pen :args {:plan "pen"}}} {:plan "pen"} {:gaps [[1 64 1]]}
    {:call {:slot :mend :job 'jobs.build.pen :args {:plan "pen" :ways #{:carried}}}} {:plan "pen" :ways #{:carried}} {:gaps [[1 64 1]]}))

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
          (is (= :sound (get-in result [:ending :cause :skipped :mend]))))))))

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
          (core/submit! eng (spec {:target 4}) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (true? (finished? s)))
          (is (= 4 (:adults (done-event s)))))))))

(deftest a-body-outside-the-box-that-lists-fewer-animals-than-the-target-walks-to-the-box-and-counts-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (h/setup {:self {:pos {:x 20 :y 64 :z 0}} :entities [(cow 1 2) (cow 2 3) (cow 3 4) (cow 4 8)]} 50)]
          (track-range! s 14)
          (core/submit! eng (spec {:target 4}) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (true? (finished? s)))
          (is (= 4 (:adults (done-event s)))))))))

(deftest a-body-outside-the-box-sees-nothing-there-after-walking-to-it-and-ends-with-an-empty-census
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (h/setup {:self {:pos {:x 40 :y 64 :z 0}} :entities []} 50)]
          (core/submit! eng (spec {:target 4}) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (true? (finished? s)))
          (is (< (.-x (.-pos (.self (:p s)))) 12) "it walked to the box to look")
          (is (= 0 (:adults (done-event s)))))))))


(def pen-plan
  {:id "pen" :metadata {:by "Fake"} :parts [{:id "cows" :box [[0 60 -5] [8 70 5]] :want "air" :herd {:mob "cow" :target 4}}]})

(defn ^:async plan-scenario
  "Submit form over a world whose plans are plans; run n ticks 700 ms apart; the setup map."
  [form plans world n]
  (let [s (h/setup world 50 (ew/of-data plans {} []))]
    (core/submit! (:eng s) form {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
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

(deftest a-pen-far-away-declines-while-another-job-is-listed-and-runs-when-none-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:inventory [(wheat 4)] :entities [(cow 1 202) (cow 2 203)] :time 15000}
              listed (h/setup world 50)
              alone (h/setup world 50)]
          (core/submit! (:eng listed) '(jobs.time.wait-for-light) {})
          (core/submit! (:eng listed) (spec {:box far-box}) {})
          (core/submit! (:eng alone) (spec {:box far-box}) {})
          (await (tu/run-ticks (:eng listed) (:clock listed) 3 700))
          (await (tu/run-ticks (:eng alone) (:clock alone) 3 700))
          (is (empty? (events-of listed :round_started)))
          (is (some #(= :away (:reason %)) (events-of listed :waiting)) "the wait says the body is away")
          (is (seq (events-of alone :round_started)) "alone in the queue it goes"))))))

(deftest animals-behind-the-body-in-the-box-are-looked-for-before-the-pass-concludes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 50)]
          (ls/entities-after-look (:p s))
          (core/submit! (:eng s) (spec {:target 4}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 20 700))
          (is (= ["wheat" "wheat"] (mapv #(.-item (.-args %)) (interacts s)))))))))
