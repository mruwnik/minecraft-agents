(ns engine.food-test
  "The eat and get-food jobs and the hungry trigger against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.core :as core]
            [engine.events :as events]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.food :as food-cost]
            [jobs.lib.cost.precious :as precious]
            [jobs.lib.cost.value :as value]
            [jobs.lib.foods :as foods]
            [jobs.lib.meal :as meal]
            [jobs.lib.reserve :as reserve]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.survival.eat :as eat]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [engine.game :as game]
            [engine.hostile-test :as h]))

(defn setup-in [world & [files]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake (merge {:floor tu/walk-floor} world)))
        eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                          files (assoc :world files)))]
    {:eng eng :p p :seen seen :clock clock}))

(defn setup [world & [zones]]
  (setup-in world (when zones (ew/of-data {} {} zones))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [p name k] (mapv #(js->clj (aget (.-args %) k) :keywordize-keys true) (calls p name)))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn know-source! [eng pos kind]
  (mem/write! (:store eng) :food-source {:pos pos :kind kind} {:cap 5 :ttl :forever}))

(defn food [p] (.-food (.self p)))

;; ---------------------------------------------------------------- hungry

(deftest hungry-food-line-rises-one-per-missing-hp-up-to-18
  (are [food health expected] (= expected (foods/hungry? food health {}))
    5 20 true
    6 20 true
    7 20 false
    2 20 true
    7 19 true
    8 19 false
    14 12 true
    15 12 false
    17 8 true
    17 3 true
    18 3 false))

(deftest hungry-thresholds-are-args
  (is (foods/hungry? 9 20 {:food 10}))
  (is (not (foods/hungry? 13 10 {:food 2})) "2 + 10 missing hp: hungry below 12"))

(deftest hungry-trigger-reads-the-world
  (let [when-fn (:when (get triggers/all :hungry))
        holds? (fn [self] (when-fn (tu/fake {:self self}) {} {}))]
    (is (= :hungry (:name (get triggers/all :hungry))))
    (is (= '(jobs.survival.get-food {:carry 0}) (:job (get triggers/all :hungry))))
    (is (holds? {:food 4}))
    (is (holds? {:food 6 :inventory [{:name "bread" :count 7}]}) "food 6 cannot sprint")
    (is (not (holds? {:food 18})))
    (is (holds? {:food 10 :health 12}))))

(deftest hungry-holds-at-urgent-near-empty-or-hurt-with-food-at-hand
  (let [level (fn [world] ((:when (get triggers/all :hungry)) (tu/fake world) {} {}))]
    (is (true? (level {:self {:food 5}})) "hungry: its :comfort level")
    (is (= :urgent (level {:self {:food 2}})) "food 2")
    (is (= :urgent (level {:self {:food 6} :inventory [{:name "bread" :count 1}]})) "hungry with food carried: one bite, no waiting for a section")
    (is (= :urgent (level {:self {:food 15 :health 7} :inventory [{:name "bread" :count 1}]})) "hurt, food carried")
    (is (not (level {:self {:food 15 :health 3} :inventory [{:name "bread" :count 1}]})) "below the risky health: the low-health rest's")))

(deftest hungry-in-the-dark-only-holds-with-food-to-eat
  (let [level (fn [world] ((:when (get triggers/all :hungry)) (tu/with-light-data (tu/fake world)) {} {}))
        night 15000]
    (is (level {:time 6000 :self {:food 4}}) "by day the search goes on")
    (is (not (level {:time night :self {:food 4}})) "in the dark nothing is searched for: the body waits sheltered")
    (is (level {:time night :self {:food 4} :inventory [{:name "bread" :count 1}]}) "carried food is eaten at night")))

(deftest hungry-leaves-a-body-under-the-risky-health-to-the-low-health-rest
  (let [level (fn [world] ((:when (get triggers/all :hungry)) (tu/with-light-data (tu/fake world)) {} {}))]
    (are [world] (not (level world))
      {:time 23800 :self {:food 10 :health 2}}
      {:time 6000 :self {:food 10 :health 2}}
      {:time 15000 :self {:food 10 :health 20}})
    (is (level {:time 23800 :self {:food 4 :health 20}}) "a healthy body goes at first light")))

;; ------------------------------------------- the hungry reflex with nothing to eat

(defn none-events [seen] (filterv #(and (= :stopped (:kind %)) (= :no-food (:reason %))) @seen))

(defn hungry-fired [seen]
  (filterv #(and (= [:reflex :fired] [(:source %) (:kind %)]) (= :hungry (:reflex %))) @seen))

(defn ^:async tick-for! [eng clock seconds]
  (dotimes [_ seconds]
    (swap! clock + 1000)
    (await (core/tick! eng))))

;; Owner: eat "should not continuously fire with no food at hand". After get-food finds nothing (ends :no-food, which
;; says why), the trigger rests: not every tick, not every cooldown.
(deftest hungry-reflex-rests-after-finding-no-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 3}})]
          (core/register-reflex! eng {:trigger :hungry})
          (await (tick-for! eng clock 5))
          (is (= 1 (count (hungry-fired seen))))
          (is (= 1 (count (none-events seen))))
          (await (tick-for! eng clock 300))
          (is (= 1 (count (hungry-fired seen))) "five minutes on, no food at hand: not fired again"))))))

(deftest food-none-says-harmful-food-is-carried-but-skipped-on-purpose
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 8 :health 14} :inventory [{:name "chicken" :count 2} {:name "rotten_flesh" :count 5}]})]
          (core/register-reflex! eng {:trigger :hungry})
          (await (tick-for! eng clock 5))
          (let [text (:text (first (none-events seen)))]
            (is (re-find #"2 chicken" text))
            (is (re-find #"5 rotten_flesh" text))
            (is (re-find #"skipped on purpose" text))))))))

(deftest hungry-reflex-wakes-when-food-is-carried-or-learned-or-the-rest-ends
  (let [when-fn (:when (get triggers/all :hungry))
        ended (fn [job kind cause] {:job job :kind kind :cause cause :report {}})
        gave-up (ended 'jobs.survival.get-food :nothing-possible :no-food)
        view (fn [now & entries]
               {:data (reduce (fn [d [kind t data]] (mem/add-entry d kind {:t t :data data} nil)) mem/empty-data entries)
                :now now})
        holds? (fn [inventory memory] (when-fn (tu/fake {:self {:food 3} :inventory inventory}) memory {}))]
    (is (holds? [] (view 1000)) "never gave up")
    (is (not (holds? [] (view 100000 [:job-report 1000 gave-up]))) "get-food found no food 99 s ago, nothing at hand")
    (is (not (holds? [] (view 100000 [:job-report 1000 gave-up] [:job-report 2000 (ended 'jobs.survival.eat :done nil)])))
        "another job's ending since leaves the rest")
    (is (not (holds? [] (view 100000 [:job-report 1000 (ended 'jobs.survival.get-food :nothing-possible :still-hungry)])))
        "ending still hungry rests too")
    (is (holds? [] (view 100000 [:job-report 1000 (ended 'jobs.survival.get-food :nothing-possible :other)]))
        "another cause is no rest")
    (is (holds? [] (view 100000 [:job-report 1000 (ended 'jobs.items.get-kit :nothing-possible :no-food)])) "another job's no-food")
    (is (holds? [{:name "bread" :count 1}] (view 100000 [:job-report 1000 gave-up])) "food carried")
    (is (holds? [{:name "wheat" :count 3}] (view 100000 [:job-report 1000 gave-up])) "bread can be baked")
    (is (holds? [] (view 100000 [:job-report 1000 gave-up] [:food-source 2000 {:pos {:x 1 :y 64 :z 1} :kind :farm}]))
        "a source learned since")
    (is (holds? [] (view 602000 [:job-report 1000 gave-up])) "the rest is over")))

(deftest hungry-reflex-backs-off-for-minutes-after-fruitless-hunts
  (let [{:keys [first-s max-s]} (:backoff (get triggers/all :hungry))]
    (is (<= 120 first-s) "a hunt that found nothing is not repeated after a second")
    (is (<= 1800 max-s) "and the wait doubles up to half an hour")))

;; ---------------------------------------------------------------- eat

(deftest eat-picks-the-best-food-and-writes-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4}
                                      :inventory [{:name "carrot" :count 2} {:name "cooked_beef" :count 1}
                                                  {:name "rotten_flesh" :count 3}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= ["cooked_beef"] (take 1 (call-args p "equip" "item"))))
          (is (= ["cooked_beef"] (take 1 (call-args p "eat" "item"))))
          (is (= ["cooked_beef" "carrot" "carrot"] (mapv :item (entries eng :fed))))
          (is (= [] (:list (core/state eng))) "one call eats until :until or nothing edible is left"))))))

(deftest eat-leaves-the-bait-an-animal-job-reserved-unless-near-starving
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [fed (fn ^:async fed* [food reserve?]
                    (let [{:keys [eng]} (setup {:self {:food food} :inventory [{:name "carrot" :count 10}]})]
                      (when reserve? (mem/write! (:store eng) :reserve {:id "bait-carrot" :item "carrot" :count :all :strength 0.8} {:cap 100 :ttl :forever}))
                      (core/submit! eng '(jobs.survival.eat) {})
                      (await (core/tick! eng))
                      (count (entries eng :fed))))]
          (is (zero? (await (fed 5 true))) "reserved: none eaten")
          (is (pos? (await (fed 5 false))) "not reserved: eaten")
          (is (pos? (await (fed 1 true))) "starving: eaten"))))))

(deftest eat-leaves-the-seed-of-a-dug-crop-for-the-tidy-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [fed (fn ^:async fed* [food]
                    (let [{:keys [eng]} (setup {:self {:food food} :inventory [{:name "potato" :count 2}]})]
                      (mem/write! (:store eng) :tidy {:cell [1 0 1] :action :dig :was "potatoes" :any-of ["potato"] :now "air" :tries 0
                                                      :plan "p" :job "j1"} {:cap 5 :ttl 300000})
                      (core/submit! eng '(jobs.survival.eat) {})
                      (await (core/tick! eng))
                      (count (entries eng :fed))))]
          (is (= 1 (await (fed 5))) "one potato of two is kept"))))))

(deftest spare-seeds-leaves-the-owed-seeds-until-near-starving
  (are [food expected] (= expected (vec (eat/spare-seeds [{:name "potato" :count 3} {:name "carrot" :count 1}] food {"potato" 2})))
    5 [{:name "potato" :count 1} {:name "carrot" :count 1}]
    1 [{:name "potato" :count 3} {:name "carrot" :count 1}]))

(deftest reserved-bait-is-not-food-for-the-hungry-checks
  (let [self (fn [food] #js {:food food :health 20 :inventory #js [#js {:name "carrot" :count 10}]})
        spared (fn [food entry] (reserve/spared {:now 0 :data {:policies {:reserve reserve/policy} :entries {:reserve [{:t 0 :data entry}]}}} (self food)))
        bait {:id "b" :item "carrot" :count :all :strength 0.8}]
    (is (= #{"carrot"} (spared 5 bait)))
    (is (= #{} (spared 1 bait)) "near starving: eaten")
    (is (= #{} (spared 5 (assoc bait :not #{:eat}))))
    (is (not (meal/carries-food? (self 5) (spared 5 bait))))
    (is (meal/carries-food? (self 1) (spared 1 bait)))))

(deftest eat-goes-on-until-fed-or-out-of-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "4, 9, 14, 19 in one round")
          (is (= 19 (food p)))
          (is (= 3 (count (entries eng :fed)))))))))

(deftest eat-max-bites-stops-after-that-many
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.eat {:max-bites 1}) {})
          (await (core/tick! eng))
          (is (= 9 (food p)) "one bite only")
          (is (= 1 (count (calls p "eat")))))))))

(deftest eat-says-why-when-the-first-bite-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 20} :inventory [{:name "bread" :count 2}]})
              out (atom nil)
              parent (tu/recording-parent 'jobs.survival.eat {:until 21} out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'eat-parent parent))]
          (core/submit! eng '(eat-parent) {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "eat"))) "a failed bite ends the job at once")
          (is (= {:ate 0 :reason :eat-failed} (select-keys @out [:ate :reason])))
          (is (= :nothing-possible (:kind (:ending @out))) "a failed bite is no done"))))))

(deftest eat-that-runs-out-of-food-short-of-until-ends-nothing-possible-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "bread" :count 1}]})
              out (atom nil)
              parent (tu/recording-parent 'jobs.survival.eat {} out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'eat-parent parent))]
          (core/submit! eng '(eat-parent) {})
          (await (core/tick! eng))
          (is (= 9 (food p)))
          (is (= [:nothing-possible :out-of-food 1] [(:kind (:ending @out)) (:reason @out) (:ate @out)])))))))

(defn ^:async ending-of-eat
  "The ending of a child eat call with args from a recording parent."
  [world args]
  (let [{:keys [eng]} (setup world)
        out (atom nil)
        parent {:round (fn ^:async r [c]
                         (let [e (await (ctx/call c :kid 'jobs.survival.eat args))]
                           (reset! out e)
                           (if (= :continue e) :continue (ctx/end c :done nil))))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'eat-parent parent))]
    (core/submit! eng '(eat-parent) {})
    (await (core/tick! eng))
    @out))

(deftest eat-ends-impossible-for-an-item-it-refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:food 14} :inventory [{:name "rotten_flesh" :count 3} {:name "cobblestone" :count 3}]}
              bad (await (ending-of-eat world {:item "rotten_flesh"}))
              rock (await (ending-of-eat world {:item "cobblestone"}))]
          (is (= [:impossible :bad-food] [(:kind bad) (contract/cause-key bad)]))
          (is (= [:impossible :not-food] [(:kind rock) (contract/cause-key rock)])))))))

(deftest eat-starts-with-food-carried
  (let [can-start? (:can-start? (get registry/jobs 'jobs.survival.eat))
        status (fn [world] (let [[kind _ {:keys [reason]}] (can-start? {:primitives (tu/fake world) :view (fn [] {:data mem/empty-data :now 0})
                                                                         :args {:until 18 :allow-bad false :item nil}})]
                             [kind reason]))]
    (is (= [:later :no-food] (status {:self {:food 4} :inventory [{:name "cobblestone" :count 2}]})))
    (is (= :ok (first (status {:self {:food 4} :inventory [{:name "bread" :count 2}]}))))))

(deftest eat-ends-done-with-the-bites-eaten-and-quotes-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:food 4} :inventory [{:name "bread" :count 5}]}
              {:keys [eng]} (setup world)
              out (atom nil)
              parent (tu/recording-parent 'jobs.survival.eat {:max-bites 2} out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'eat-parent parent))
              job (get registry/jobs 'jobs.survival.eat)
              c {:primitives (tu/fake world) :view (fn [] {:data mem/empty-data :now 0}) :args {:until 18 :allow-bad false :item nil :max-bites nil}}
              q (await ((:quote-of job) c))]
          (core/submit! eng '(eat-parent) {})
          (await (core/tick! eng))
          (is (= {:status :done :ate 2} (select-keys @out [:status :ate])))
          (is (nil? (contract/quote-problem q)))
          (is (pos? (second (get-in q [:uses :job-time])))))))))

(deftest a-cut-eat-round-yields-without-eating
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:self {:food 4} :inventory [{:name "bread" :count 2}]})
              c {:primitives p :view (fn [] {:data mem/empty-data :now 0}) :alive? (constantly false)
                 :args {:until 18 :allow-bad false :item nil :max-bites nil}}]
          (is (= :continue (await (eat/round c))))
          (is (= [] (calls p "eat"))))))))

(deftest eat-is-done-when-fed
  (let [job (get registry/jobs 'jobs.survival.eat)
        done? (fn [food] (first ((:done? job) {:primitives (tu/fake {:self {:food food} :inventory [{:name "bread" :count 2}]})
                                                :args {:until 18}})))]
    (is (= :later (done? 10)))
    (is (= :ok (done? 18)))))

(deftest eat-is-done-when-the-last-food-is-eaten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2} :inventory [{:name "bread" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= 7 (food p))))))))

(deftest eat-declines-without-edible-food
  (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "rotten_flesh" :count 2} {:name "spider_eye" :count 1}
                                                            {:name "cobblestone" :count 9}]})]
    (core/submit! eng '(jobs.survival.eat {:get-food? false}) {})
    (is (nil? (core/tick! eng)) "the check declines")
    (is (= [] (calls p "eat")))))

(deftest eat-with-nothing-carried-fetches-through-get-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 4} :inventory []})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (some #(and (= :child_started (:kind %)) (= :fetch (:slot %))) @seen) "get-food runs as the fetch child"))))))

(deftest eat-when-fed-is-done-without-eating
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [inventory [[{:name "bread" :count 2}] []]]
          (let [{:keys [eng p]} (setup {:self {:food 18} :inventory inventory})]
            (core/submit! eng '(jobs.survival.eat) {})
            (await (core/tick! eng))
            (is (= [] (:list (core/state eng))) (pr-str inventory))
            (is (= [] (calls p "eat")))))))))

(deftest eat-takes-bad-food-only-when-allowed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "rotten_flesh" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat {:allow-bad true}) {})
          (await (core/tick! eng))
          (is (= ["rotten_flesh"] (call-args p "eat" "item"))))))))

(deftest eat-refuses-a-named-harmful-item-without-allow-bad
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 14} :inventory [{:name "rotten_flesh" :count 3}]})]
          (core/submit! eng '(jobs.survival.eat {:item "rotten_flesh"}) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "the job ends at once, it does not queue")
          (is (= [] (calls p "eat")))
          (is (= 1 (count (filter #(and (= :stopped (:kind %)) (re-find #"allow-bad" (str (:cause %)))) @seen)))
              "the stopped event names :allow-bad"))))))

(deftest eat-named-harmful-item-with-allow-bad-is-eaten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 14} :inventory [{:name "rotten_flesh" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat {:item "rotten_flesh" :allow-bad true}) {})
          (await (core/tick! eng))
          (is (= ["rotten_flesh"] (call-args p "eat" "item"))))))))

(deftest eat-until-is-an-arg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.eat {:until 8}) {})
          (await (run-until-empty eng 10))
          (is (= 9 (food p))))))))

(deftest eat-eats-foods-the-old-table-lacked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [item ["honey_bottle" "tropical_fish" "chorus_fruit" "suspicious_stew"]]
          (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name item :count 1}]})]
            (core/submit! eng (list 'jobs.survival.eat {:item item}) {})
            (await (core/tick! eng))
            (is (= [item] (call-args p "eat" "item")))))))))

(deftest eat-keeps-precious-food-for-a-named-meal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4 :health 20} :inventory [{:name "golden_apple" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat {:get-food? false}) {})
          (is (nil? (core/tick! eng)) "healthy and unnamed: not eaten")
          (is (= [] (calls p "eat"))))
        (let [{:keys [eng p]} (setup {:self {:food 4 :health 20} :inventory [{:name "golden_apple" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat {:item "golden_apple"}) {})
          (await (core/tick! eng))
          (is (= ["golden_apple"] (call-args p "eat" "item")) "named"))
        (let [{:keys [eng p]} (setup {:self {:food 4 :health 4} :inventory [{:name "golden_apple" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat {:get-food? false}) {})
          (await (core/tick! eng))
          (is (= [] (calls p "eat")) "hurt with no threat known: its hp alone are worth less than it"))
        (let [{:keys [eng p]} (setup {:self {:food 4 :health 4}
                                      :inventory [{:name "golden_apple" :count 1} {:name "bread" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= ["bread"] (take 1 (call-args p "eat" "item"))) "a common food still goes first"))))))

(deftest eat-golden-carrot-is-a-normal-best-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4 :health 20}
                                      :inventory [{:name "golden_carrot" :count 1} {:name "bread" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= ["golden_carrot"] (take 1 (call-args p "eat" "item"))) "unnamed, healthy, and best by points"))))))

(deftest foods-follow-the-version-the-body-is-connected-with
  (is (contains? (foods/table-for "26.1") "honey_bottle"))
  (is (not (contains? (foods/table-for "1.12") "honey_bottle")))
  (is (= "26.1" (game/version-of (tu/fake {}))) "no rawWorld (tests): the connect default")
  (is (= "1.12" (game/version-of #js {:rawWorld #js {:version (fn [] "1.12")}})))
  (is (= "26.1" (game/version-of #js {:rawWorld #js {:version (fn [] nil)}})) "no bot yet: the default"))

(deftest an-engine-takes-its-foods-from-its-bodys-version
  (let [p (tu/fake {})]
    (set! (.-rawWorld p) #js {:version (fn [] "1.12")})
    (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)})
    (is (not (foods/food? "honey_bottle")))
    (is (foods/food? "bread"))
    (core/create {:primitives (tu/fake {}) :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)})
    (is (foods/food? "honey_bottle") "the next engine without a version: the default")))

(deftest eat-keeps-chorus-fruit-and-stew-for-a-named-meal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [item ["chorus_fruit" "suspicious_stew"]]
          (let [{:keys [eng p]} (setup {:self {:food 4 :health 4} :inventory [{:name item :count 1}]})]
            (core/submit! eng '(jobs.survival.eat) {})
            (await (core/tick! eng))
            (is (= [] (calls p "eat")) "unnamed: not eaten")))))))

(deftest eat-raw-chicken-is-harmful-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "chicken" :count 2}]})]
          (core/submit! eng '(jobs.survival.eat {:item "chicken"}) {})
          (await (core/tick! eng))
          (is (= [] (calls p "eat")))
          (is (= [] (:list (core/state eng)))))))))

(deftest eat-a-named-non-food-is-refused-at-once-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 4} :inventory [{:name "cobblestone" :count 3}]})]
          (core/submit! eng '(jobs.survival.eat {:item "cobblestone"}) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= [] (calls p "eat")))
          (is (= 1 (count (filter #(and (= :stopped (:kind %)) (= :not-food (:reason %))) @seen)))))))))

(deftest eat-keeps-harmful-and-precious-food-last-whatever-their-points
  (is (= "dried_kelp" (eat/best-food [{:name "spider_eye"} {:name "dried_kelp"}] true nil 20 (constantly true) #{})) "harmful last")
  (is (= "dried_kelp" (eat/best-food [{:name "golden_apple"} {:name "dried_kelp"}] false nil 4 (constantly true) #{})) "precious after common"))

(deftest a-golden-apple-is-not-worth-eating-at-8-hp-fed-and-safe
  (is (not (food-cost/eat-precious? {:health 8} "golden_apple" true))
      (str "its hp alone: " (food-cost/precious-worth {:health 8} "golden_apple" true) " s against "
           (:value (value/item-value [{:name "golden_apple" :count 1}])) " s")))

(deftest eat-chooses-by-saturation-when-points-tie
  (is (= "cooked_mutton" (eat/best-food [{:name "cooked_chicken"} {:name "cooked_mutton"}] false nil 20 (constantly false) #{})))
  (is (= "cooked_mutton" (eat/best-food [{:name "honey_bottle"} {:name "cooked_mutton"}] false nil 20 (constantly false) #{}))))

(deftest golden-apples-are-rare-for-the-top-up-trigger
  (is (not (foods/top-up? 16 8 ["golden_apple"])))
  (is (foods/top-up? 16 8 ["honey_bottle"])))

;; ---------------------------------------------------------------- get-food

(deftest get-food-eats-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 3} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (run-until-empty eng 10))
          (is (= 18 (food p)) "eats until fed, as the eat job does")
          (is (= [] (tu/walk-calls p)))
          (is (= [] (calls p "attack"))))))))

(deftest get-food-when-not-hungry-is-done-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 19}})]
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= [] (tu/walk-calls p))))))))

(def farm
  {:blocks {"10,64,0" "carrots" "11,64,0" "carrots" "12,64,0" "carrots" "10,63,0" "farmland"}
   :ages {"10,64,0" 7 "11,64,0" 3 "12,64,0" 7}
   :drops {:carrots "carrot"}})

(deftest get-food-harvests-mature-crops-of-a-known-farm
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (assoc farm :self {:food 0}))]
          (know-source! eng {:x 11 :y 64 :z 0} :farm)
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (run-until-empty eng 40))
          (is (= [] (:list (core/state eng))))
          (is (= [{:x 10 :y 64 :z 0} {:x 12 :y 64 :z 0}] (call-args p "dig" "pos"))
              "only the mature ones, the young carrots are left")
          (is (= 2 (count (calls p "eat"))))
          (is (= 10 (food p)))
          (is (empty? (none-events seen))))))))

(deftest get-food-ignores-a-source-beyond-its-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0}})]
          (know-source! eng {:x 500 :y 64 :z 0} :farm)
          (core/submit! eng '(jobs.survival.get-food {:carry 0 :explore false}) {})
          (await (run-until-empty eng 10))
          (is (= [] (tu/walk-calls p))))))))

(deftest get-food-takes-food-from-a-known-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0}
                                      :containers {"20,64,0" [{:name "cobblestone" :count 30} {:name "bread" :count 4}
                                                              {:name "carrot" :count 9}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (run-until-empty eng 20))
          (is (= ["bread"] (call-args p "transfer" "item")) "the best food in the chest, not the stone")
          (is (= ["withdraw"] (call-args p "transfer" "direction")))
          (is (= 20 (food p)) "then eats it"))))))

(def cow
  {:id 7 :name "cow" :kind "passive" :pos {:x 5 :y 64 :z 0} :health 20 :drops [{:name "beef" :count 1}]})

(def three-cows
  [cow (assoc cow :id 8 :pos {:x 8 :y 64 :z 0}) (assoc cow :id 9 :pos {:x 9 :y 64 :z 0})])

(defn hunt-world [entities zones]
  (let [{:keys [eng p seen] :as w} (setup {:self {:food 4} :entities entities} zones)]
    (core/submit! eng '(jobs.survival.get-food {:carry 0 :settings {:jobs.lib.combat/attack-gap-ms 0}}) {})
    w))

(deftest get-food-keeps-the-last-two-adults-of-a-kind-for-breeding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (hunt-world (take 2 three-cows) nil)]
          (await (run-until-empty eng 40))
          (is (= [] (calls p "attack")))
          (is (some #(and (= :no-food (:reason %)) (re-find #"cow: kept for breeding" (str (:text %)))) @seen)
              "the text says the cows were kept"))))))

(def own-farm [{:name "farm" :owner "Fake" :min [0 60 -3] :max [20 70 3]}])

(defn killed-events [seen] (filterv #(= :food.killed (:kind %)) @seen))

(deftest get-food-hunts-a-lone-wild-adult-no-pair-to-keep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (hunt-world [cow] nil)]
          (await (run-until-empty eng 40))
          (is (seq (calls p "attack")) "the hunt is not told to keep a pair back from a lone adult")
          (is (= [{:adults 1 :rule :lone}] (mapv #(select-keys % [:adults :rule]) (killed-events seen)))))))))

(deftest get-food-takes-the-surplus-of-its-own-pen-when-nothing-else-is-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (hunt-world three-cows own-farm)]
          (await (run-until-empty eng 40))
          (is (= [7] (distinct (call-args p "attack" "id"))) "one cow, the two others kept for breeding")
          (is (= [:own-pen] (mapv :rule (killed-events seen)))))))))

(deftest get-food-spares-the-pen-of-another-bodys-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (shape/with-author {:id "pen" :parts [{:id "p" :box [[4 64 -1] [10 64 1]] :want "dirt"}]} "Miles")
              {:keys [eng p seen]} (setup-in {:self {:food 4} :entities three-cows} (ew/of-data {"pen" plan} {} []))]
          (core/submit! eng '(jobs.survival.get-food {:carry 0 :settings {:jobs.lib.combat/attack-gap-ms 0} :explore false}) {})
          (await (run-until-empty eng 40))
          (is (= [] (calls p "attack")))
          (is (seq (none-events seen))))))))

(deftest get-food-hunts-wild-animals-before-its-own-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wild (mapv #(assoc %1 :id %2 :pos {:x %3 :y 64 :z 0}) three-cows [17 18 19] [-4 -8 -9])
              {:keys [eng p]} (hunt-world (into three-cows wild) own-farm)]
          (await (run-until-empty eng 40))
          (is (= [17] (distinct (call-args p "attack" "id")))))))))

(deftest get-food-walks-to-its-own-pen-out-of-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [penned (mapv #(assoc cow :id %1 :pos {:x %2 :y 64 :z 0}) [7 8 9] [33 36 37])
              {:keys [eng p]} (setup {:self {:food 4} :entities penned}
                                     [{:name "far-pen" :owner "Fake" :min [30 60 -3] :max [40 70 3]}])]
          (core/submit! eng '(jobs.survival.get-food {:carry 0 :settings {:jobs.lib.combat/attack-gap-ms 0} :explore false}) {})
          (await (run-until-empty eng 60))
          (is (seq (tu/walk-calls p)) "walked to the pen it could not see")
          (is (= [7] (distinct (call-args p "attack" "id"))))
          (is (> (food p) 4)))))))

(defn hunt-world-with [spec entities zones]
  (let [{:keys [eng p seen] :as w} (setup {:self {:food 4} :entities entities} zones)]
    (core/submit! eng (list 'jobs.survival.get-food (update spec :settings merge {:jobs.lib.combat/attack-gap-ms 0})) {})
    w))

(deftest get-food-spares-listed-kinds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (hunt-world-with {:settings {:jobs.lib.animal-spare/off-limits #{"cow"}}} three-cows nil)]
          (await (run-until-empty eng 40))
          (is (= [] (calls p "attack")))
          (is (some #(and (= :no-food (:reason %)) (re-find #"cow: off-limits" (str (:text %)))) @seen)))))))

(deftest get-food-takes-penned-animals-when-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (hunt-world-with {:settings {:jobs.lib.animal-spare/spare-pens false}} three-cows own-farm)]
          (await (run-until-empty eng 40))
          (is (seq (calls p "attack"))))))))

(deftest get-food-keep-count-is-a-setting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (hunt-world-with {:settings {:jobs.lib.animal-spare/keep-adults 0}} (take 2 three-cows) nil)]
          (await (run-until-empty eng 40))
          (is (seq (calls p "attack"))))))))

(deftest get-food-says-what-it-killed-and-where
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (hunt-world-with {} three-cows nil)]
          (await (run-until-empty eng 40))
          (is (some #(and (= :food.killed (:kind %)) (re-find #"cow" (str (:text %))) (re-find #"\[5 64 0\]" (str (:text %)))) @seen)))))))

(deftest get-food-says-why-each-kill-was-allowed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (hunt-world-with {} three-cows nil)]
          (await (run-until-empty eng 40))
          (is (= [{:adults 3 :keep 2 :food 4 :rule :surplus}]
                 (mapv #(select-keys % [:adults :keep :food :rule]) (killed-events seen)))))))))

(deftest get-food-leaves-the-last-pair-alone-even-when-starving
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 0} :entities (take 2 three-cows)})]
          (core/submit! eng '(jobs.survival.get-food {:carry 0 :settings {:jobs.lib.combat/attack-gap-ms 0}}) {})
          (await (run-until-empty eng 40))
          (is (= [] (calls p "attack")))
          (is (some #(and (= :no-food (:reason %)) (re-find #"cow: kept for breeding" (str (:text %)))) @seen)))))))

(deftest get-food-names-the-starvation-lift-as-the-rule
  (async done
    (tu/run-async done
      (fn [] (settings/with-settings {:jobs.survival.get-food/kept-lifted-below 3}
        (fn ^:async t []
          (let [{:keys [eng seen]} (setup {:self {:food 2} :entities (take 2 three-cows)})]
            (core/submit! eng '(jobs.survival.get-food {:carry 0 :settings {:jobs.lib.combat/attack-gap-ms 0}}) {})
            (await (run-until-empty eng 40))
            (is (= [{:adults 2 :keep 2 :food 2 :rule :starved}]
                   (mapv #(select-keys % [:adults :keep :food :rule]) (killed-events seen)))))))))))

(def pen-ring
  (merge (tu/box 3 64 -2 12 64 -2 "oak_fence") (tu/box 3 64 2 12 64 2 "oak_fence")
         (tu/box 3 64 -1 3 64 1 "oak_fence") (tu/box 12 64 -1 12 64 1 "oak_fence")))

(deftest get-food-spares-animals-inside-a-closed-fence-that-is-no-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (let [w (setup {:self {:food 4} :entities three-cows :blocks pen-ring})]
                                     (core/submit! (:eng w) '(jobs.survival.get-food {:carry 0 :settings {:jobs.lib.combat/attack-gap-ms 0}}) {})
                                     w)]
          (await (run-until-empty eng 40))
          (is (= [] (calls p "attack")))
          (is (some #(and (= :no-food (:reason %)) (re-find #"cow: pen" (str (:text %)))) @seen)))))))

(deftest get-food-looks-at-a-fence-it-has-not-seen-before-hunting-inside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (h/setup-seeing {:self {:food 4 :pos {:x -9 :y 64 :z 0}} :yaw 270 :entities three-cows :blocks (merge (tu/box 3 63 -2 12 63 2 "stone") pen-ring)} nil 700)]
          (core/submit! eng '(jobs.survival.get-food {:carry 0 :settings {:jobs.lib.combat/attack-gap-ms 0}}) {})
          (await (run-until-empty eng 40))
          (is (= [] (calls p "attack")))
          (is (some #(and (= :no-food (:reason %)) (re-find #"cow: pen" (str (:text %)))) @seen)))))))

(deftest get-food-hunts-an-animal-when-no-source-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 2} :entities three-cows})]
          (core/submit! eng '(jobs.survival.get-food {:carry 0 :settings {:jobs.lib.combat/attack-gap-ms 0}}) {})
          (await (run-until-empty eng 40))
          (is (= [] (:list (core/state eng))))
          (is (= [7 7 7 7] (call-args p "attack" "id")) "20 health, 5 a swing")
          (is (= ["beef"] (call-args p "eat" "item")))
          (is (= 7 (food p)))
          (is (empty? (none-events seen))))))))

(deftest get-food-hunts-at-the-held-weapons-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:self {:food 2} :inventory [{:name "iron_axe" :count 1}] :entities three-cows})]
          (.override (.-world p) "wait" (fn [token a impl] (swap! clock + (.-ms a)) (impl token a)))
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (run-until-empty eng 40))
          (is (seq (h/waited-between-swings p)))
          (is (every? #(<= 1072 % 1112) (h/waited-between-swings p)) "the iron axe's 1112 ms, not a fixed gap"))))))

(deftest get-food-forages-a-berry-bush-when-there-are-no-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 1}
                                      :blocks {"6,64,0" "sweet_berry_bush"} :ages {"6,64,0" 3}
                                      :drops {:sweet_berry_bush "sweet_berries"}})]
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (run-until-empty eng 20))
          (is (= [{:x 6 :y 64 :z 0}] (call-args p "dig" "pos")))
          (is (= ["sweet_berries"] (call-args p "eat" "item"))))))))

(deftest get-food-digs-a-melon-block-for-its-slices
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 1}
                                      :blocks {"6,64,0" "melon"}
                                      :drops {:melon "melon_slice"}})]
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (run-until-empty eng 20))
          (is (= [{:x 6 :y 64 :z 0}] (call-args p "dig" "pos")))
          (is (= ["melon_slice"] (call-args p "eat" "item"))))))))

(def wheat-field
  {:self {:food 1} :inventory [{:name "wheat_seeds" :count 4}]
   :blocks {"6,64,0" "wheat" "6,63,0" "farmland"} :ages {"6,64,0" 7} :drops {:wheat "wheat"}})

(deftest get-food-cuts-and-replants-wheat-only-in-a-zone-of-its-own
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn ^:async run1 [zones]
                    (let [{:keys [eng p]} (setup wheat-field zones)]
                      (core/submit! eng '(jobs.survival.get-food {:carry 0 :explore false}) {})
                      (await (run-until-empty eng 60))
                      p))
              own (await (run own-farm))
              none (await (run nil))
              other (await (run [(assoc (first own-farm) :owner "Miles")]))]
          (is (= [{:x 6 :y 64 :z 0}] (call-args own "dig" "pos")) "the field of a zone it owns is cut")
          (is (seq (calls own "place")) "and replanted")
          (is (= [] (calls none "dig")) "wheat in no zone is left")
          (is (= [] (calls other "dig")) "and so is another's"))))))

(deftest get-food-forgets-a-source-found-empty
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1} :containers {"20,64,0" [{:name "cobblestone" :count 3}]}})
              none #(none-events seen)]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (run-until-empty eng 20))
          (is (= [] (entries eng :food-source)))
          (is (= 1 (count (none))))
          (is (re-find #"no food source is known" (:text (first (none))))))))))

(def table-near {"3,64,0" "crafting_table"})

(defn carried [p name]
  (transduce (comp (filter #(= name (.-name %))) (map #(.-count %))) + 0 (array-seq (.-inventory (.self p)))))


(defn ^:async run-food!
  "A new firing of get-food (hungry below 14, so food 12 is hungry), run until it ends."
  [eng]
  (core/submit! eng '(jobs.survival.get-food {:carry 0 :food 14}) {})
  (await (run-until-empty eng 30)))

(deftest get-food-bakes-carried-wheat-and-eats-the-bread
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 12} :inventory [{:name "wheat" :count 30}] :blocks table-near})]
          (await (run-food! eng))
          (is (= "bread" (last (call-args p "craft" "item"))))
          (is (= 2 (last (call-args p "craft" "count"))))
          (is (= 24 (carried p "wheat")))
          (is (= 0 (carried p "bread")) "eaten by the next rounds")
          (is (> (food p) 12)))))))

(deftest get-food-eats-carried-food-before-baking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 12} :inventory [{:name "wheat" :count 30} {:name "apple" :count 1}]
                                      :blocks table-near})]
          (await (run-food! eng))
          (is (= [] (calls p "craft")))
          (is (= 30 (carried p "wheat")))
          (is (= ["apple"] (call-args p "eat" "item"))))))))

(deftest get-food-does-not-bake-fewer-than-three-wheat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 12} :inventory [{:name "wheat" :count 2}] :blocks table-near})]
          (await (run-food! eng))
          (is (= [] (calls p "craft"))))))))

(deftest get-food-cannot-bake-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 12} :inventory [{:name "wheat" :count 5}]})]
          (await (run-food! eng))
          (is (= 1 (count (none-events seen))) "went on down the ladder")
          (is (re-find #"carrying 5 wheat but cannot bake \(no-table\)" (:text (first (none-events seen))))))))))

(deftest get-food-withdraws-wheat-from-a-chest-with-no-food-then-bakes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 12} :blocks {"21,64,0" "crafting_table"}
                                      :containers {"20,64,0" [{:name "cobblestone" :count 30} {:name "wheat" :count 12}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (await (run-food! eng))
          (is (= ["wheat"] (call-args p "transfer" "item")))
          (is (= [6] (call-args p "transfer" "count")))
          (is (= 2 (last (call-args p "craft" "count"))))
          (is (> (food p) 12) "baked and eaten"))))))

;; ---------------------------------------------------------------- top-up

(deftest hungry-trigger-tops-up-a-hurt-body-that-carries-food
  (let [when-fn (:when (get triggers/all :hungry))
        holds? (fn [{:keys [inventory] :as self}] (when-fn (tu/fake {:self (dissoc self :inventory) :inventory inventory}) {} {}))
        bread [{:name "bread" :count 1}]]
    (is (holds? {:food 16 :health 14 :inventory bread}))
    (is (not (holds? {:food 16 :health 14 :inventory []})) "no food carried: no top-up")
    (is (not (holds? {:food 18 :health 14 :inventory bread})) "regeneration already works")
    (is (not (holds? {:food 16 :health 20 :inventory bread})) "healthy")
    (is (holds? {:food 16 :health 14 :inventory [{:name "golden_carrot" :count 1}]}) "a common food")
    (is (not (holds? {:food 16 :health 14 :inventory [{:name "golden_apple" :count 1}]})))))

(deftest hungry-trigger-eats-to-full-below-the-health-line
  (let [when-fn (:when (get triggers/all :hungry))
        holds? (fn [{:keys [inventory] :as self} & [args]]
                 (when-fn (tu/fake {:self (dissoc self :inventory) :inventory inventory}) {} (or args {})))
        bread [{:name "bread" :count 1}]]
    (is (holds? {:food 19 :health 6 :inventory bread}) "below 7 hp: eat up to 20")
    (is (not (holds? {:food 19 :health 6 :inventory [{:name "golden_apple" :count 1}]})) "precious food is no meal a trigger counts on")
    (is (not (holds? {:food 19 :health 6 :inventory []})) "nothing to eat and food enough to heal")
    (is (not (holds? {:food 20 :health 6 :inventory bread})) "full")
    (is (not (holds? {:food 19 :health 7 :inventory bread})) "at the line")
    (is (not (holds? {:food 19 :health 6 :inventory bread} {:health 5})) "the health line is an arg")))

(deftest get-food-eats-to-full-below-the-health-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 19 :health 5} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.get-food {:carry 0}) {})
          (await (run-until-empty eng 10))
          (is (= 20 (food p)) "saturation heals fastest on a full bar")
          (is (= [] (tu/walk-calls p))))))))

(deftest get-food-names-a-short-craft-as-short
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 12} :inventory [{:name "wheat" :count 5}] :blocks table-near})]
          (.override (.-world p) "craft" (fn ^:async f [_ _ _] #js {:status "partial" :reason "no-item" :made 0 :used #js {} :have #js {} :recipes #js [#js {"wheat" 3}]}))
          (await (run-food! eng))
          (is (re-find #"cannot bake \(short\)" (:text (first (none-events seen))))))))))

(deftest the-cooking-table-names-real-foods
  (is (every? foods/food? (vals foods/cooked-of)))
  (is (= ["cooked_beef" "cooked_porkchop" "cooked_chicken" "cooked_mutton" "cooked_rabbit" "cooked_cod" "cooked_salmon" "baked_potato"]
         foods/cooked-meals))
  (is (every? (set (keys foods/cooked-of)) (keys game/meat-animal))))

(deftest eat-bad-never-eats-poison-that-leaves-the-body-fragile
  (let [inv [{:name "spider_eye"} {:name "pufferfish"} {:name "rotten_flesh"}]]
    (is (= "rotten_flesh" (eat/best-food inv true nil 6.5 (constantly false) #{})) "rotten flesh only costs hunger")
    (is (= "spider_eye" (eat/best-food (take 2 inv) true nil 14 (constantly false) #{})) "healthy enough for 4 hp of poison")
    (is (nil? (eat/best-food (take 2 inv) true nil 6.5 (constantly false) #{})))
    (is (nil? (eat/best-food (take 1 inv) true "spider_eye" 9 (constantly false) #{})) "a named one too")))

(deftest a-precious-foods-worth-counts-its-hp-only-where-the-body-lives
  (let [body {:health 2 :absorption 0}
        at {:x 0 :y 64 :z 0}
        known (cost/fight-answer {:weapon nil :mobs [{:name "zombie" :distance 2}]})
        death (answer/bare body at)
        hp (food-cost/precious-worth body "golden_apple" false)
        fed (precious/fed body "golden_apple")
        before (answer/answer-death-chance body known at death)
        after (answer/answer-death-chance fed known at death)]
    (is (< after before))
    (is (< (js/Math.abs (- (food-cost/precious-worth body "golden_apple" false known at death)
                           (+ (* (- 1 after) hp) (* (- before after) (answer/died death known)))))
           1e-6)
        "hp lived x hp-price + death averted x death price: no hp is counted twice")))

(deftest a-precious-food-counts-as-carried-food-only-when-worth-eating
  (let [self (fn [item health food] #js {:health health :food food :inventory #js [#js {:name item :count 1}]})]
    (is (not (meal/carries-food? (self "golden_apple" 5 5))) "a golden apple's hp are worth less than the apple a recipe makes")
    (is (meal/carries-food? (self "enchanted_golden_apple" 5 5)) "nothing yields an enchanted one: its worth is the hp it heals from 10, and 5 hp gain more")
    (is (not (meal/carries-food? (self "enchanted_golden_apple" 19 5))) "at 19 hp it heals less than it does from 10")))
