(ns engine.get-food-rounds-test
  "get-food as one whole attempt (job-rounds design): one round eats, or gets food and eats, or stops with a reason;
  a cut round resumes from the world; jobs.lib.child/run!* runs a child to its end."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.hut-shelter-test :as hs]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [jobs.lib.world-files :as ew]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.get-food.report :as report]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.survival.get-food :as get-food]))

(defn setup
  "step-ms: the engine's clock moves on by that much each time it is read (time passing while a round runs); the
  world's furnaces cook by the same fake time, 50 ms a tick."
  [world & {:keys [step-ms zones] :or {step-ms 0}}]
  (let [clock (atom 1000000)
        carry (atom 0)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake (merge {:floor tu/walk-floor} world)))
        now (fn []
              (let [ticks (quot (swap! carry + step-ms) 50)]
                (swap! carry - (* ticks 50))
                (when (pos? ticks) (.advance (.-world p) ticks)))
              (swap! clock + step-ms))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :world (ew/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [p name k] (mapv #(js->clj (aget (.-args %) k) :keywordize-keys true) (calls p name)))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn know-source! [eng pos kind]
  (mem/write! (:store eng) :food-source {:pos pos :kind kind} {:cap 5 :ttl :forever}))

(defn food [p] (.-food (.self p)))

(defn ended [seen]
  (filterv #(and (= :job (:source %)) (#{:stopped :completed} (:kind %))) @seen))

(defn none-endings [seen] (filterv #(and (= :stopped (:kind %)) (= :no-food (:reason %))) @seen))

(defn ^:async one-round!
  "Submit get-food with args and run exactly one tick."
  [eng args]
  (core/submit! eng (list 'jobs.survival.get-food (merge {:carry 0} args)) {})
  (await (core/tick! eng)))

(def cow {:id 7 :name "cow" :kind "passive" :pos {:x 5 :y 64 :z 0} :health 20 :drops [{:name "beef" :count 1}]})

(def herd "The cow and two more: get-food leaves the last two adults of a kind alone."
  [cow (assoc cow :id 8 :pos {:x 8 :y 64 :z 0}) (assoc cow :id 9 :pos {:x 9 :y 64 :z 0})])

(def farm
  {:blocks {"10,64,0" "carrots" "11,64,0" "carrots" "12,64,0" "carrots" "10,63,0" "farmland"}
   :ages {"10,64,0" 7 "11,64,0" 3 "12,64,0" 7}
   :drops {:carrots "carrot"}})

;; ---------------------------------------------------------------- one round is one whole attempt

(deftest one-round-eats-all-the-carried-food-it-needs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 3} :inventory [{:name "bread" :count 5}]})]
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= 18 (food p)))
          (is (= [:completed] (mapv :kind (ended seen)))))))))

(deftest one-round-takes-food-from-a-known-chest-and-eats-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 0}
                                           :containers {"20,64,0" [{:name "cobblestone" :count 30} {:name "bread" :count 4}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= ["bread"] (call-args p "transfer" "item")))
          (is (= 20 (food p)))
          (is (= [:completed] (mapv :kind (ended seen)))))))))

(defn note-stock! [eng pos items]
  (mem/write! (:store eng) :fetch/stock {:pos pos :items items} {:cap 64 :ttl (* 6 60 60 1000)}))

(deftest a-chest-whose-stock-the-body-booked-is-a-food-source-even-far-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0} :floor [-30 -10 100 10]
                                      :containers {"90,64,0" [{:name "cobblestone" :count 30} {:name "bread" :count 40}]}})]
          (note-stock! eng [90 64 0] {"cobblestone" 30 "bread" 40})
          (await (one-round! eng {}))
          (is (= ["bread"] (call-args p "transfer" "item")) "the booked chest 90 blocks off is used")
          (is (= 20 (food p))))))))

(deftest a-booked-chest-with-no-food-is-not-a-food-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0} :floor [-30 -10 100 10] :containers {"90,64,0" [{:name "cobblestone" :count 30}]}})]
          (note-stock! eng [90 64 0] {"cobblestone" 30})
          (await (one-round! eng {}))
          (is (empty? (calls p "transfer"))))))))

(deftest one-round-hunts-an-animal-and-eats-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2} :entities herd})]
          (await (one-round! eng {:settings {:jobs.lib.combat/attack-gap-ms 0}}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= [7 7 7 7] (call-args p "attack" "id")))
          (is (= ["beef"] (call-args p "eat" "item")))
          (is (= 7 (food p))))))))

(deftest an-animal-that-only-comes-into-view-when-the-body-looks-around-is-hunted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2}})
              look (.-look p)
              added (atom false)]
          (set! (.-look p) (fn [& args]
                             (when (compare-and-set! added false true) (run! #(fake/add-entity! p %) herd))
                             (.apply look p (to-array args))))
          (await (one-round! eng {:settings {:jobs.lib.combat/attack-gap-ms 0}}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= [7 7 7 7] (call-args p "attack" "id")) "it looked around before giving up")
          (is (= 7 (food p))))))))

(deftest an-animal-in-view-during-one-glance-only-is-hunted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2}})
              look (.-look p)
              entities (.-entities p)
              looks (atom 0)
              spotted (atom false)]
          (run! #(fake/add-entity! p %) herd)
          (set! (.-look p) (fn [& args] (swap! looks inc) (.apply look p (to-array args))))
          (set! (.-entities p) (fn [& args]
                                 (when (= 3 @looks) (reset! spotted true))
                                 (if @spotted (.apply entities p (to-array args)) #js [])))
          (await (one-round! eng {:settings {:jobs.lib.combat/attack-gap-ms 0}}))
          (is (= [7 7 7 7] (call-args p "attack" "id")) "the cow seen in the middle of the look-around is hunted")
          (is (= 7 (food p))))))))

(deftest the-attack-child-keeps-the-gap-between-swings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [gap [0 600]]
          (let [{:keys [eng p clock]} (setup {:self {:food 2} :entities herd} :step-ms 5)
                swings (atom [])]
            (.override (.-world p) "attack" (fn [token args impl] (swap! swings conj @clock) (impl token args)))
            (await (one-round! eng {:settings {:jobs.lib.combat/attack-gap-ms gap}}))
            (is (= ["beef"] (call-args p "eat" "item")) (str "gap " gap ": the cow died and was eaten"))
            (is (= 4 (count @swings)) (str "gap " gap))
            (is (every? #(>= % gap) (map - (rest @swings) @swings)) (str "gap " gap ": swings spaced by the gap " @swings))))))))

(deftest a-chest-that-cannot-be-read-is-skipped-for-this-job-but-not-forgotten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0}
                                      :containers {"20,64,0" [{:name "bread" :count 4}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (.override (.-world p) "inspectContainer" (fn [_ _ _] #js {:status "busy"}))
          (await (one-round! eng {}))
          (is (= 1 (count (calls p "inspectContainer"))) "not asked again in this job")
          (is (= [{:pos {:x 20 :y 64 :z 0} :kind :chest}] (entries eng :food-source)) "still remembered"))))))

(deftest a-chest-seen-gone-is-forgotten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 0}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (await (one-round! eng {}))
          (is (= [] (entries eng :food-source))))))))

(deftest one-round-harvests-the-ripe-crops-of-a-known-farm
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc farm :self {:food 0}))]
          (know-source! eng {:x 11 :y 64 :z 0} :farm)
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= #{{:x 10 :y 64 :z 0} {:x 12 :y 64 :z 0}} (set (call-args p "dig" "pos"))) "only the ripe ones")
          (is (= 2 (count (calls p "eat")))))))))

(deftest harvest-drops-seen-but-none-taken-end-the-round-stopped-not-looping
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (assoc farm :self {:food 0}))]
          (.override (.-world p) "collect" (fn [_ _ _] #js {:status "unreachable" :reason "no-path" :gained #js []}))
          (know-source! eng {:x 11 :y 64 :z 0} :farm)
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (empty? (calls p "eat")))
          (is (= [:stopped] (mapv :kind (ended seen))) "no food came in: not completed"))))))

(deftest one-round-with-no-food-anywhere-is-stopped-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1}})]
          (await (one-round! eng {}))
          (is (= [] (:list (core/state eng))))
          (is (= [:stopped] (mapv :kind (ended seen))) "nothing found is not completed")
          (is (re-find #"no food" (str (:text (first (ended seen))))))
          (is (= 1 (count (none-endings seen))))
          (is (= 1 (count (entries eng :hungry)))))))))

(deftest a-way-that-fails-is-followed-by-the-next-in-the-same-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0} :entities herd
                                      :containers {"20,64,0" [{:name "cobblestone" :count 3}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (await (one-round! eng {:settings {:jobs.lib.combat/attack-gap-ms 0}}))
          (is (= [] (:list (core/state eng))) "one round")
          (is (= [] (entries eng :food-source)) "the empty chest is forgotten")
          (is (= ["beef"] (call-args p "eat" "item")) "then it hunted"))))))

;; ---------------------------------------------------------------- as the hungry reflex

(defn reflex-events [seen kind] (filterv #(and (= :reflex (:source %)) (= kind (:kind %))) @seen))

(deftest the-hungry-reflex-runs-one-round-and-never-continues
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world fed?] [[{:self {:food 3} :inventory [{:name "bread" :count 5}]} true]
                              [{:self {:food 3} :entities herd} true]
                              [{:self {:food 3}} false]]]
          (let [{:keys [eng p seen]} (setup world :step-ms 5)]
            (core/load-scenario! eng (scenario/parse "{:register [{:trigger :hungry}]}"))
            (await (core/tick! eng))
            (is (= 1 (count (reflex-events seen :ended))) (pr-str world))
            (is (= fed? (> (food p) 3)) (pr-str world))
            (is (= [] (reflex-events seen :continued)) "a reflex job that yields is a bug (C1 turns it into :declined)")))))))

(defn yields-twice
  "A next-way! that yields :continue twice, then is the real one."
  []
  (let [real get-food/next-way!
        n (atom 0)]
    (fn ^:async f [c] (if (< (swap! n inc) 3) :continue (await (real c))))))

(deftest a-reflex-round-waits-out-a-way-that-yields
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [get-food/next-way! (yields-twice)]
          (let [{:keys [eng p seen]} (setup {:self {:food 3} :entities herd} :step-ms 5)]
            (core/load-scenario! eng (scenario/parse "{:register [{:trigger :hungry}]}"))
            (await (core/tick! eng))
            (is (= 1 (count (reflex-events seen :ended))) "the reflex waited and went on in one tick")
            (is (< 3 (food p)))
            (is (= [] (reflex-events seen :continued)))))))))

(deftest a-manual-round-yields-continue-while-a-way-waits-and-resumes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [get-food/next-way! (yields-twice)]
          (let [{:keys [eng p seen]} (setup {:self {:food 3} :entities herd} :step-ms 5)]
            (await (one-round! eng {}))
            (is (= 1 (count (:list (core/state eng)))) "one tick: the call yielded, still queued")
            (is (= [] (ended seen)))
            (await (tu/run-until-empty eng 10))
            (is (< 3 (food p)))
            (is (= [:completed] (mapv :kind (ended seen))))))))))

(deftest a-body-that-ate-mid-run-ends-done-not-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 3}})
              real get-food/next-way!
              n (atom 0)]
          (with-redefs [get-food/next-way! (fn ^:async f [c]
                                             (if (= 1 (swap! n inc))
                                               (do (swap! (fake/state p) assoc-in [:self :food] 20) :again)
                                               (await (real c))))]
            (let [out (atom nil)
                  parent {:round (fn ^:async r [c]
                                   (let [e (await (ctx/call c :kid 'jobs.survival.get-food {:carry 0}))]
                                     (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause])) (ctx/end c :done nil)))))}
                  eng (assoc-in eng [:jobs 'rec-parent] parent)]
              (core/submit! eng '(rec-parent) {})
              (await (tu/run-until-empty eng 40))
              (is (= :done (:kind @out))))))))))

(deftest hunt-skips-babies-and-animals-in-another-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[ent zones attacks] [[cow nil true]
                                     [cow [{:name "pen" :owner "Miles" :min [4 63 -1] :max [12 66 1]}] false]
                                     [(assoc cow :baby true) nil false]]]
          (let [{:keys [eng p]} (setup {:self {:food 5} :entities (into [ent] (rest herd))} :step-ms 5 :zones zones)]
            (await (one-round! eng {}))
            (is (= attacks (boolean (seq (calls p "attack")))) (pr-str [ent zones]))))))))

(deftest food-none-names-the-animals-it-passed-over
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1} :entities [(assoc cow :baby true)]} :step-ms 5)]
          (await (one-round! eng {}))
          (let [none (first (none-endings seen))]
            (is (= [{:name "cow" :id 7 :why :baby :pos [5 64 0]}] (mapv #(select-keys % [:name :id :why :pos]) (:animals none))))
            (is (re-find #"passed over 1 animal \(cow: baby at \[" (str (:text none))))))))))

(deftest food-none-lists-what-the-scan-saw-when-no-animal-qualified
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1} :entities [(assoc cow :kind "other")]} :step-ms 5)]
          (await (one-round! eng {}))
          (let [none (first (none-endings seen))]
            (is (= [{:name "cow" :kind "other" :pos [5 64 0]}] (mapv #(select-keys % [:name :kind :pos]) (:nearby none))))
            (is (re-find #"nothing hunted; in range: cow \(other\) at \[" (str (:text none))))))))))

(deftest food-none-nearby-leaves-out-a-hostile-behind-a-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:id 9 :name "zombie" :kind "hostile" :pos {:x 4 :y 64 :z 0} :health 20 :visible false}
              {:keys [eng seen]} (setup {:self {:food 1} :entities [(assoc cow :kind "other") zombie]} :step-ms 5)]
          (await (one-round! eng {}))
          (let [none (first (none-endings seen))]
            (is (= ["cow"] (mapv :name (:nearby none))))
            (is (not (re-find #"zombie" (str (:text none)))))))))))

;; ---------------------------------------------------------------- cut and resume

(defn ^:async cut-at-transfer!
  "Run a get-food round until it is held at its transfer, cut it, call (between)."
  [eng p between]
  (.hold (.-world p) "transfer")
  (let [running (core/tick! eng)]
    (loop [i 0]
      (when (and (empty? (calls p "transfer")) (< i 400))
        (await (js/Promise. (fn [r] (js/setTimeout r 5))))
        (recur (inc i))))
    (core/cut! eng (core/holder eng) :test nil)
    (await running)
    (between)))

(deftest a-cut-round-resumes-and-finishes-from-the-world
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 0} :containers {"20,64,0" [{:name "bread" :count 4}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (cut-at-transfer! eng p (fn [])))
          (is (= 1 (count (:list (core/state eng)))) "the cut job stays listed")
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "the resumed round finished")
          (is (= 20 (food p)))
          (is (= [:completed] (mapv :kind (ended seen)))))))))

(deftest a-resumed-round-finds-the-chest-emptied-meanwhile-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 0} :containers {"20,64,0" [{:name "bread" :count 4}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (cut-at-transfer! eng p #(swap! (fake/state p) assoc-in [:containers [20 64 0]] [])))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= [:stopped] (mapv :kind (ended seen))) "no throw, a clear stop")
          (is (= [] (entries eng :food-source)))
          (is (= 0 (food p))))))))

;; ---------------------------------------------------------------- jobs.lib.child/run!*

(defn counting-child
  "A child that answers :continue n times (counted in its memory), then :done; calls counts every call."
  [n calls]
  {:round (fn ^:async counting-round [c]
            (swap! calls inc)
            (let [k (inc (:k (ctx/mem c) 0))]
              (ctx/update-mem! c assoc :k k)
              (if (> k n) (ctx/end c :done nil) :continue)))})

(defn run-parent [kid args opts out]
  {:round (fn ^:async parent-round [c]
            (let [r (await (child/run!* c :kid kid args opts))
                  kind (if (map? r) (:kind r) r)]
              (swap! out conj [kind (get-in (ctx/mem c) [:children :kid :k])])
              (if (= :done kind) (ctx/end c :done nil) :continue)))})

(defn ^:async run-parent! [kid args opts]
  (let [{:keys [eng]} (setup {})
        out (atom [])
        eng (assoc eng :jobs (assoc (:jobs eng) 'run-parent (run-parent kid args opts out)))]
    (core/submit! eng '(run-parent) {})
    (await (core/tick! eng))
    {:eng eng :out out}))

(deftest run-calls-the-child-until-it-ends-with-a-timer-between-calls
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [paces (atom 0)
              pace pace/pace!
              n (atom 0)]
          (set! pace/pace! (fn [] (swap! paces inc) (pace)))
          (let [{:keys [out]} (await (run-parent! (counting-child 3 n) {} {}))]
            (set! pace/pace! pace)
            (is (= [[:done nil]] @out) "one call of run! ran it to :done, its memory cleared")
            (is (= 4 @n))
            (is (= 3 @paces) "a timer between calls, never a microtask")))))))

(deftest run-yields-continue-at-its-call-cap-and-keeps-the-childs-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              {:keys [eng out]} (await (run-parent! (counting-child 10 n) {} {:max-calls 4}))]
          (is (= [[:continue 4]] @out))
          (await (core/tick! eng))
          (is (= [[:continue 4] [:continue 8]] @out) "the next round resumes the same child"))))))

(deftest run-clears-the-slot-when-the-args-change
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              out (atom [])
              args (atom {:a 1})
              kid (counting-child 10 n)
              {:keys [eng]} (setup {})
              eng (assoc eng :jobs (assoc (:jobs eng) 'p {:round (fn ^:async p-round [c]
                                                                   (await (child/run!* c :kid kid @args {:max-calls 2}))
                                                                   (swap! out conj (get-in (ctx/mem c) [:children :kid :k]))
                                                                   :continue)}))]
          (core/submit! eng '(p) {})
          (await (core/tick! eng))
          (reset! args {:a 2})
          (await (core/tick! eng))
          (is (= [2 2] @out) "a new target starts the child afresh"))))))

;; ---------------------------------------------------------------- nothing seen: explore, rotten flesh, say why

(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))(defn searches
  "The not-found endings of the explore search child."
  [seen]
  (filterv #(and (= :child_ended (:kind %)) (= :explore (:slot %))
                 (= :nothing-possible (:kind (or (:ending %) (get-in % [:data :ending])))))
           @seen))

(deftest nothing-seen-searches-for-food-before-giving-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1}} :step-ms 5)]
          (await (one-round! eng {}))
          (is (= 1 (count (searches seen))) "one search for animals and crops")
          (is (re-find #"explored" (str (:text (first (none-endings seen)))))))))))

(deftest explore-false-gives-up-without-searching
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1}} :step-ms 5)]
          (await (one-round! eng {:explore false}))
          (is (= [] (searches seen)))
          (is (= 1 (count (none-endings seen)))))))))

(deftest a-starving-body-searches-again-after-a-short-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 0 :health 1}} :step-ms 5)]
          (await (one-round! eng {}))
          (swap! clock + 120000)
          (await (one-round! eng {}))
          (is (= 2 (count (searches seen))) "the 10 min rest is cut short when starving"))))))

(deftest each-give-up-widens-the-next-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 4 :health 20}} :step-ms 5)
              radii (fn [] (mapv #(re-find #"within \d+ blocks" (str (:text %))) (none-endings seen)))]
          (await (one-round! eng {:explore false}))
          (swap! clock + 700000)
          (await (one-round! eng {:explore false}))
          (swap! clock + 700000)
          (await (one-round! eng {:explore false}))
          (is (= ["within 24 blocks" "within 48 blocks" "within 72 blocks"] (radii))))))))

(deftest starving-with-only-rotten-flesh-eats-it-as-a-last-resort
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 2} :inventory [{:name "rotten_flesh" :count 3}]} :step-ms 5)]
          (await (one-round! eng {:explore false}))
          (is (< 2 (food p)))
          (is (= [] (none-endings seen))))))))

(deftest a-body-above-the-hungry-line-is-done-without-waiting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 8 :health 20}})]
          (core/submit! eng '(jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (core/tick! eng))
          (is (= [] (events-of seen :waiting)))
          (is (= [] (:list (core/state eng)))))))))

(deftest hungry-at-night-does-not-explore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1} :time 15000} :step-ms 5)]
          (await (one-round! eng {}))
          (is (= [] (searches seen)))
          (is (re-find #"dark" (str (:text (first (none-endings seen)))))))))))

(deftest the-search-waits-for-light-not-for-the-hour
  (let [waits? (fn [light]
                 (let [p (tu/with-light-data (tu/fake {:time 15000 :self {:food 8}}))]
                   (swap! (fake/state p) assoc :light-default light)
                   (get-food/search-waits? {:primitives p :engine {:warned (js-obj)} :id "t" :view (fn [] {:data {} :now 0}) :args {} :now (constantly 0)})))]
    (is (false? (waits? [15 15])) "night, lit place: the search goes on")
    (is (true? (waits? [15 0])) "night, dark place: it waits")))

(deftest hungry-underground-does-not-explore-the-caves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:time 6000 :blocks {"0,66,0" "stone"} :self {:food 8 :health 10}} :step-ms 5)]
          (await (one-round! eng {}))
          (is (= [] (searches seen)) "food 8 underground: no search through the dark caves")
          (is (re-find #"underground" (str (:text (first (none-endings seen)))))))))))

(deftest a-starving-body-underground-explores-the-caves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:time 6000 :blocks {"0,66,0" "stone"} :self {:food 0 :health 20}} :step-ms 5)]
          (await (one-round! eng {}))
          (is (= 1 (count (searches seen))) "starving: the search goes on"))))))

(deftest a-starving-body-searches-at-night-when-no-danger-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 0} :time 15000} :step-ms 5)]
          (await (one-round! eng {}))
          (is (= 1 (count (searches seen))) "food 0 and nothing near: the night does not stop the search"))))))

(deftest a-starving-body-does-not-search-at-night-with-a-hostile-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 0 :pos {:x 0.5 :y 64 :z 0.5}} :time 15000
                                         :entities [{:id 5 :name "zombie" :kind "hostile" :pos {:x 4.5 :y 64 :z 0.5} :health 20}]}
                                        :step-ms 5)]
          (await (one-round! eng {}))
          (is (= [] (searches seen)) "a hostile near: no search")
          (is (re-find #"dark" (str (:text (first (none-endings seen)))))))))))

(deftest carried-bread-is-eaten-at-night-with-a-hostile-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 9 :health 6 :pos {:x 0.5 :y 64 :z 0.5}} :time 15000
                                           :inventory [{:name "bread" :count 4}]
                                           :entities [{:id 5 :name "zombie" :kind "hostile" :pos {:x 4.5 :y 64 :z 0.5} :health 20}]}
                                          :step-ms 5)]
          (await (one-round! eng {:explore false}))
          (is (< 9 (food p)) "danger never holds back eating carried food")
          (is (= [] (searches seen)))
          (is (= [] (none-endings seen))))))))

(defn stop-text
  "The job's stopped events, printed: the ending's reason and why."
  [seen]
  (pr-str (filterv #(and (= :job (:source %)) (= :stopped (:kind %))) @seen)))

(def creeper {:id 5 :name "creeper" :kind "hostile" :health 20})

(deftest a-body-on-low-health-does-not-search-at-first-light
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {:self {:food 10 :health 2} :time 23800} :step-ms 5)]
          (await (one-round! eng {}))
          (is (= [] (searches seen)) "no search")
          (is (= [] (tu/walk-calls p)))
          (is (= [] (entries eng :hungry)) "not a fruitless search: no rest written")
          (is (= 1 (count (entries eng :food-too-dangerous))))
          (is (re-find #":reason :too-dangerous" (stop-text seen))))))))

(deftest a-body-on-low-health-searches-on-a-calm-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 10 :health 2} :time 6000} :step-ms 5)]
          (await (one-round! eng {}))
          (is (= 1 (count (searches seen)))))))))

(def bread-chest {:floor [-30 -10 100 10] :containers {"60,64,0" [{:name "bread" :count 4}]}})

(deftest a-body-on-low-health-does-not-walk-past-a-known-creeper-to-a-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup (merge bread-chest {:self {:food 5 :health 2} :time 6000
                                                              :entities [(assoc creeper :pos {:x 50.5 :y 64 :z 0.5})]})
                                          :step-ms 5)]
          (know-source! eng {:x 60 :y 64 :z 0} :chest)
          (await (one-round! eng {:explore false}))
          (is (= [] (calls p "transfer")))
          (is (re-find #"passes creeper" (stop-text seen))))))))

(deftest a-body-on-low-health-looks-around-before-walking-out-and-refuses-a-creeper-it-finds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup (merge bread-chest {:self {:food 5 :health 2} :time 6000}) :step-ms 5)
              look (.-look p)
              added (atom false)]
          (set! (.-look p) (fn [& args]
                             (when (compare-and-set! added false true)
                               (fake/add-entity! p (assoc creeper :pos {:x 50.5 :y 64 :z 0.5})))
                             (.apply look p (to-array args))))
          (know-source! eng {:x 60 :y 64 :z 0} :chest)
          (await (one-round! eng {:explore false}))
          (is (= [] (calls p "transfer")))
          (is (re-find #"passes creeper" (stop-text seen))))))))

(deftest a-body-at-food-zero-does-not-rest-after-a-too-dangerous-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup (merge bread-chest {:self {:food 0 :health 2} :time 6000
                                                            :entities [(assoc creeper :pos {:x 50.5 :y 64 :z 0.5})]})
                                        :step-ms 5)]
          (know-source! eng {:x 60 :y 64 :z 0} :chest)
          (await (one-round! eng {:explore false}))
          (await (one-round! eng {:explore false}))
          (is (empty? (events-of seen :waiting)) "starving: the second call looks again instead of waiting out the rest"))))))

(deftest a-body-on-low-health-walks-to-a-source-with-no-hostile-about
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (merge bread-chest {:self {:food 5 :health 2} :time 6000}) :step-ms 5)]
          (know-source! eng {:x 60 :y 64 :z 0} :chest)
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (tu/run-until-empty eng 60))
          (is (= ["bread"] (call-args p "transfer" "item")))
          (is (> (food p) 5)))))))

(deftest hungry-at-night-in-a-roofed-hut-does-not-explore-out-of-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup (merge (hs/hut-world {:x 5.5 :y 64 :z 0.5} {}) {:self {:food 1 :pos {:x 5.5 :y 64 :z 0.5}}
                                                                                       :time 15000})
                                        :step-ms 5)]
          (await (one-round! eng {}))
          (is (= [] (searches seen)) "no search out of the hut at night"))))))

;; ---------------------------------------------------------------- cooking raw food

(def furnace-world
  {:blocks {"2,64,0" "furnace"}
   :inventory [{:name "chicken" :count 2} {:name "coal" :count 2}]})

(defn ^:async run-until-idle!
  "Tick until the list is empty or n ticks."
  [eng _p n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(deftest raw-chicken-next-to-a-furnace-is-cooked-and-eaten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc furnace-world :self {:food 2}) :step-ms 2000)]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0}) {})
          (await (run-until-idle! eng p 40))
          (is (= [] (:list (core/state eng))) "the job ended")
          (is (some #{"take"} (call-args p "furnace" "op")) "the cooked output was taken")
          (is (not-any? #{"chicken"} (map #(.-name %) (.-inventory (.self p)))) "no raw chicken left")
          (is (> (food p) 2) "and eaten"))))))

(deftest raw-chicken-is-cooked-in-the-dark-sky-at-a-furnace-the-body-can-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc furnace-world :self {:food 0} :time 15000) :step-ms 2000)]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (run-until-idle! eng p 40))
          (is (some #{"take"} (call-args p "furnace" "op")) "the clock alone is no reason to skip the cook"))))))

(deftest raw-chicken-in-a-roofed-hut-at-night-is-not-carried-to-a-furnace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pos {:x 5.5 :y 64 :z 0.5}
              w (merge (hs/hut-world pos {"10,64,0" "furnace"})
                       {:self {:food 2 :pos pos} :inventory [{:name "chicken" :count 2} {:name "coal" :count 2}] :time 15000})
              {:keys [eng p]} (setup w :step-ms 5)]
          (await (one-round! eng {:explore false}))
          (is (= [] (calls p "furnace")) "a sheltered body does not walk out to a furnace at night"))))))

(deftest raw-chicken-with-no-furnace-touches-no-furnace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2} :inventory [{:name "chicken" :count 2}]})]
          (await (one-round! eng {:explore false}))
          (is (= [] (calls p "furnace"))))))))

(deftest raw-beef-is-cooked-before-it-is-eaten-when-not-urgent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"2,64,0" "furnace"} :self {:food 5}
                                      :inventory [{:name "beef" :count 4} {:name "coal" :count 2}]}
                                     :step-ms 2000)]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0}) {})
          (await (run-until-idle! eng p 40))
          (is (some #{"take"} (call-args p "furnace" "op")) "the cooked output was taken")
          (is (< (count (filter #{"beef"} (map #(.-name %) (.-inventory (.self p))))) 4) "raw beef was cooked")
          (is (> (food p) 5)))))))

(deftest raw-beef-is-eaten-at-once-when-starvation-is-ticking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"2,64,0" "furnace"} :self {:food 0 :health 3}
                                      :inventory [{:name "beef" :count 4} {:name "coal" :count 2}]})]
          (await (one-round! eng {:explore false}))
          (is (= [] (calls p "furnace")) "no time to cook")
          (is (> (food p) 0)))))))

(deftest raw-beef-is-cooked-at-food-0-when-health-outlasts-the-cook
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"2,64,0" "furnace"} :self {:food 0 :health 20}
                                      :inventory [{:name "beef" :count 4} {:name "coal" :count 2}]}
                                     :step-ms 2000)]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (run-until-idle! eng p 40))
          (is (some #{"take"} (call-args p "furnace" "op")) "cooked first")
          (is (> (food p) 0)))))))

(deftest raw-beef-is-cooked-at-a-furnace-seen-far-off-without-a-recorded-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"40,64,0" "furnace"} :self {:food 5}
                                      :inventory [{:name "beef" :count 4} {:name "coal" :count 2}]}
                                     :step-ms 500)]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (run-until-idle! eng p 80))
          (is (some #{"take"} (call-args p "furnace" "op")) "walked to the furnace it had seen")
          (is (> (food p) 5)))))))

(deftest raw-beef-is-cooked-at-low-food-with-a-furnace-at-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"2,64,0" "furnace"} :self {:food 3}
                                      :inventory [{:name "beef" :count 2} {:name "coal" :count 2}]}
                                     :step-ms 2000)]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0}) {})
          (await (run-until-idle! eng p 40))
          (is (some #{"take"} (call-args p "furnace" "op")) "the cooked output was taken")
          (is (> (food p) 3)))))))

(deftest raw-beef-is-eaten-at-once-with-a-hostile-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:id 9 :name "zombie" :kind "hostile" :pos {:x 6 :y 64 :z 0} :health 20}
              {:keys [eng p]} (setup {:blocks {"2,64,0" "furnace"} :self {:food 5} :entities [zombie]
                                      :inventory [{:name "beef" :count 4} {:name "coal" :count 2}]})]
          (await (one-round! eng {:explore false}))
          (is (= [] (calls p "furnace")) "no cooking under threat")
          (is (> (food p) 5)))))))

(defn ^:async low-health-cook! [entities]
  (let [{:keys [eng p] :as w} (setup {:blocks {"30,64,0" "furnace"} :self {:food 5 :health 2} :time 6000 :entities entities
                                      :inventory [{:name "beef" :count 4} {:name "coal" :count 2}]}
                                     :step-ms 500)]
    (core/submit! eng (list 'jobs.survival.get-food {:carry 0 :explore false}) {})
    (await (run-until-idle! eng p 40))
    w))

(deftest raw-beef-is-cooked-on-low-health-on-a-calm-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (low-health-cook! []))]
          (is (some #{"take"} (call-args p "furnace" "op"))))))))

(deftest raw-beef-is-eaten-raw-on-low-health-with-a-creeper-by-the-furnace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (low-health-cook! [(assoc creeper :pos {:x 33.5 :y 64 :z 0.5})]))]
          (is (= [] (calls p "furnace")) "the walk would likely kill it")
          (is (> (food p) 5)))))))

(deftest raw-beef-is-cooked-at-a-remembered-furnace-beyond-the-cook-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"30,64,0" "furnace"} :self {:food 5}
                                      :inventory [{:name "beef" :count 4} {:name "coal" :count 2}]}
                                     :step-ms 500)]
          (mem/write! (:store eng) :furnace {:pos {:x 30 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (run-until-idle! eng p 80))
          (is (some #{"take"} (call-args p "furnace" "op")) "walked to the remembered furnace and took the output")
          (is (> (food p) 5)))))))

(deftest one-raw-beef-does-not-pay-for-making-a-furnace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 5}
                                      :inventory [{:name "beef" :count 1} {:name "furnace" :count 1} {:name "coal" :count 2}]})]
          (await (one-round! eng {:explore false}))
          (is (= [] (calls p "furnace")))
          (is (> (food p) 5)))))))

(deftest raw-beef-with-no-furnace-seen-has-one-made-and-cooked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 5}
                                      :inventory [{:name "beef" :count 4} {:name "furnace" :count 1} {:name "coal" :count 2}]}
                                     :step-ms 500
                                     :zones [{:name "home" :owner "Fake" :min [-20 60 -20] :max [20 70 20]}])]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0}) {})
          (await (run-until-idle! eng p 60))
          (is (= 1 (count (:furnaces @(fake/state p)))) "a furnace was put down")
          (is (some #{"take"} (call-args p "furnace" "op")))
          (is (< (count (filter #{"beef"} (map #(.-name %) (.-inventory (.self p))))) 4) "raw beef was cooked"))))))

(defn ^:async two-beef-crafts!
  "Furnaces crafted when get-food holds 2 raw beef at food 5, no furnace seen and the stone for one in the pack, with entities around."
  [entities]
  (let [{:keys [eng p]} (setup {:self {:food 5}
                                :inventory [{:name "beef" :count 2} {:name "cobblestone" :count 8} {:name "coal" :count 2}]
                                :entities entities
                                :blocks {"-2,64,3" "crafting_table"}
                                :recipes {"furnace" {:count 1 :needs {"cobblestone" 8} :table true}}}
                               :step-ms 500
                               :zones [{:name "home" :owner "Fake" :min [-20 60 -20] :max [20 70 20]}])]
    (core/submit! eng (list 'jobs.survival.get-food {:carry 0}) {})
    (await (run-until-idle! eng p 60))
    (count (calls p "craft"))))

(deftest raw-beef-from-a-hunt-is-cooked-but-a-penned-herd-is-eaten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [home-herd [(assoc cow :id 8 :pos {:x 8 :y 64 :z 0}) (assoc cow :id 9 :pos {:x 9 :y 64 :z 0})]]
          (is (pos? (await (two-beef-crafts! []))) "no cows left near, no pen: replacing the beef is dear")
          (is (= 0 (await (two-beef-crafts! home-herd))) "two cows penned in the home zone: beef is cheap to replace"))))))

(deftest a-little-raw-meat-is-cooked-when-a-furnace-and-fuel-are-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 5}
                                      :inventory [{:name "porkchop" :count 1} {:name "furnace" :count 1} {:name "oak_planks" :count 4}]}
                                     :step-ms 500
                                     :zones [{:name "home" :owner "Fake" :min [-20 60 -20] :max [20 70 20]}])]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0}) {})
          (await (run-until-idle! eng p 60))
          (is (= 1 (count (:furnaces @(fake/state p)))) "the carried furnace was put down")
          (is (some #{"take"} (call-args p "furnace" "op")) "the cooked output was taken"))))))

(deftest raw-beef-with-no-furnace-to-be-had-says-so-and-is-eaten-raw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 5} :inventory [{:name "beef" :count 4}]}
                                          :zones [{:name "home" :owner "Fake" :min [-20 60 -20] :max [20 70 20]}])]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (run-until-idle! eng p 40))
          (is (> (food p) 5) "the beef was eaten raw after"))))))

(deftest a-failed-cook-is-remembered-as-no-cook-and-the-food-is-eaten-raw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"2,64,0" "furnace"} :self {:food 5}
                                           :inventory [{:name "beef" :count 4}]})]
          (core/submit! eng (list 'jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (run-until-idle! eng p 40))
          (is (> (food p) 5) "the beef was eaten raw after"))))))

;; ---------------------------------------------------------------- :carry

(deftest carry-hunts-for-carried-food-when-already-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 20} :entities herd})]
          (await (one-round! eng {:carry 3 :settings {:jobs.lib.combat/attack-gap-ms 0}}))
          (is (= [7 7 7 7] (call-args p "attack" "id")))
          (is (empty? (call-args p "eat" "item")) "a fed body eats nothing")
          (is (= [:completed] (mapv :kind (ended seen)))))))))

(deftest eat-false-gathers-food-points-and-eats-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 2} :entities herd})]
          (await (one-round! eng {:eat false :carry 3 :settings {:jobs.lib.combat/attack-gap-ms 0}}))
          (is (= [7 7 7 7] (call-args p "attack" "id")) "the cow is hunted")
          (is (empty? (call-args p "eat" "item")) "the caller eats, not get-food")
          (is (= [:completed] (mapv :kind (ended seen)))))))))

(deftest carry-already-carried-does-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 20} :inventory [{:name "bread" :count 3}] :entities herd})]
          (await (one-round! eng {:carry 10}))
          (is (empty? (call-args p "attack" "id")))
          (is (= [:completed] (mapv :kind (ended seen))) "the goal holds: done without acting"))))))

(deftest carry-with-no-source-stops-naming-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 20}} :step-ms 5)]
          (await (one-round! eng {:carry 8 :explore false}))
          (let [stop (first (ended seen))]
            (is (= :stopped (:kind stop)))
            (is (= :no-carry (:reason stop)) (pr-str stop))))))))

(deftest a-carry-ask-searches-despite-an-earlier-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 20}} :step-ms 5)]
          (mem/write! (:store eng) :hungry {:food 3} {:cap 10 :ttl 3600000})
          (await (one-round! eng {:carry 8}))
          (is (= 1 (count (searches seen))) "searched though it gave up a minute ago")
          (is (= :no-carry (:reason (first (ended seen))))))))))

(deftest explore-is-done-once-per-job-unless-starving-and-rested
  (is (false? (get-food/explore-done? nil false 100 60000)))
  (is (true? (get-food/explore-done? {:explored "x" :explored-at 0} false 999999 60000)))
  (is (true? (get-food/explore-done? {:explored "x" :explored-at 0} true 59999 60000)))
  (is (false? (get-food/explore-done? {:explored "x" :explored-at 0} true 60000 60000))))

(deftest starving-uses-the-low-health-line-of-eating
  (let [{:keys [p]} (setup {:self {:food 15 :health 6.5}} :step-ms 5)
        c {:primitives p :args {}}]
    (is (get-food/starving? c) "health below 7 and hungry")))

(deftest the-widened-search-stays-within-what-explore-search-accepts
  (let [opts (get-food/widen-explore {:max-distance 64 :max-legs 12 :spend {:job-time 120}} get-food/widen-max)]
    (is (= 128 (:max-distance opts)))
    (is (= 36 (:max-legs opts)))))

(def pond
  (merge (into {} (for [x (range 0 9) z (range 0 9) y [62 63]] [(str x "," y "," z) "water"]))
         (into {} (for [z (range 0 9) y [62 63]] [(str "9," y "," z) "stone"]))))

(defn fishing-world [inventory]
  {:blocks pond :self {:food 3 :pos [9.5 64 4.5]} :inventory inventory :fish {:catch ["cod"]}})

(deftest with-nothing-else-a-hungry-body-with-a-rod-fishes-and-eats
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (fishing-world [{:name "fishing_rod" :count 1}]))]
          (await (one-round! eng {:explore false}))
          (is (pos? (count (calls p "fish"))) "cast")
          (is (> (food p) 3) "ate the catch")
          (is (empty? (calls p "attack"))))))))

(deftest water-not-yet-seen-with-a-rod-is-looked-for-before-giving-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (fishing-world [{:name "fishing_rod" :count 1}]))
              _ (tu/seeing-after-look p)]
          (await (one-round! eng {:explore false}))
          (is (pos? (count (calls p "fish"))) "looked around, then cast"))))))

(deftest without-a-rod-or-water-in-sight-it-does-not-fish
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (fishing-world []))]
          (await (one-round! eng {:explore false}))
          (is (empty? (calls p "fish"))))
        (let [{:keys [eng p]} (setup {:self {:food 3} :inventory [{:name "fishing_rod" :count 1}]})]
          (await (one-round! eng {:explore false}))
          (is (empty? (calls p "fish"))))))))

(deftest a-starving-body-takes-the-last-adults-only-when-the-lift-is-set
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[lift food n] [[3 2 true] [3 5 false] [-1 2 false]]]
          (await (settings/with-settings {:jobs.survival.get-food/kept-lifted-below lift}
                   (fn ^:async t2 []
                     (let [{:keys [eng p]} (setup {:self {:food food} :entities [cow (assoc cow :id 8 :pos {:x 8 :y 64 :z 0})]})]
                       (await (one-round! eng {:settings {:jobs.lib.combat/attack-gap-ms 0} :explore false :food 6}))
                       (is (= n (boolean (seq (calls p "attack")))) (str "lift " lift " food " food)))))))))))

(deftest the-hungry-reflex-cooks-raw-food-and-eats-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (assoc furnace-world :self {:food 2}) :step-ms 2000)]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :hungry}]}"))
          (await (core/tick! eng))
          (is (some #{"take"} (call-args p "furnace" "op")) "the cooked output was taken")
          (is (= [] (filterv #(= :food.no-cook (:kind %)) @seen)) "no give-up on the cooking wait")
          (is (= [] (reflex-events seen :continued))))))))

(deftest a-cook-still-under-way-on-the-next-call-is-waited-out-not-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (assoc furnace-world :self {:food 2}) :step-ms 20)]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :hungry}]}"))
          (await (core/tick! eng))
          (is (= [] (filterv #(= :food.no-cook (:kind %)) @seen)) "no give-up while the load cooks")
          (is (some #{"take"} (call-args p "furnace" "op")) "the cooked output was taken"))))))

(deftest the-explored-text-names-what-the-search-found
  (is (= "2 found (cow at [1 64 2], wheat at [3 64 4]), 0 legs"
         (report/explored-text {:found [{:what "cow" :pos [1 64 2]} {:what :wheat :pos [3 64 4]}] :coverage {:legs 0}})))
  (is (= "0 found, 3 legs, time" (report/explored-text {:coverage {:legs 3} :why :time}))))
