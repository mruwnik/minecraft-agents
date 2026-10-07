(ns engine.food-test
  "The eat and get-food jobs and the hungry trigger against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [jobs.lib.foods :as foods]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [triggers.survival.hungry :as hungry]
            [jobs.survival.eat :as eat]
            [engine.game :as game]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake (merge {:floor tu/walk-floor} world)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

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
    6 20 false
    2 20 true
    6 19 true
    7 19 false
    13 12 true
    14 12 false
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
    (is (= '(jobs.survival.get-food) (:job (get triggers/all :hungry))))
    (is (holds? {:food 4}))
    (is (not (holds? {:food 18})))
    (is (holds? {:food 10 :health 12}))))

;; ------------------------------------------- the hungry reflex with nothing to eat

(defn hungry-fired [seen]
  (filterv #(and (= [:reflex :fired] [(:source %) (:kind %)]) (= :hungry (:reflex %))) @seen))

(defn ^:async tick-for! [eng clock seconds]
  (dotimes [_ seconds]
    (swap! clock + 1000)
    (await (core/tick! eng))))

;; Owner: eat "should not continuously fire with no food at hand". After get-food finds nothing (food.none, which
;; says why), the trigger rests: not every tick, not every cooldown.
(deftest hungry-reflex-rests-after-finding-no-food-and-says-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 3}})]
          (core/register-reflex! eng {:trigger :hungry})
          (await (tick-for! eng clock 5))
          (is (= 1 (count (hungry-fired seen))))
          (let [none (filterv #(= :food.none (:kind %)) @seen)]
            (is (= 1 (count none)))
            (is (re-find #"hungry reflex rests" (:text (first none))) "the agent is told why it stops"))
          (await (tick-for! eng clock 300))
          (is (= 1 (count (hungry-fired seen))) "five minutes on, no food at hand: not fired again"))))))

(deftest food-none-says-harmful-food-is-carried-but-skipped-on-purpose
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 3} :inventory [{:name "chicken" :count 2} {:name "rotten_flesh" :count 5}]})]
          (core/register-reflex! eng {:trigger :hungry})
          (await (tick-for! eng clock 5))
          (let [text (:text (first (filterv #(= :food.none (:kind %)) @seen)))]
            (is (re-find #"2 chicken" text))
            (is (re-find #"5 rotten_flesh" text))
            (is (re-find #"skipped on purpose" text))))))))

(deftest hungry-reflex-wakes-when-food-is-carried-or-learned-or-the-rest-ends
  (let [when-fn (:when (get triggers/all :hungry))
        gave-up {:food 3}
        view (fn [now & entries]
               {:data (reduce (fn [d [kind t data]] (mem/add-entry d kind {:t t :data data} nil)) mem/empty-data entries)
                :now now})
        holds? (fn [inventory memory] (when-fn (tu/fake {:self {:food 3} :inventory inventory}) memory {}))]
    (is (holds? [] (view 1000)) "never gave up")
    (is (not (holds? [] (view 100000 [:hungry 1000 gave-up]))) "gave up 99 s ago, nothing at hand")
    (is (holds? [{:name "bread" :count 1}] (view 100000 [:hungry 1000 gave-up])) "food carried")
    (is (holds? [{:name "wheat" :count 3}] (view 100000 [:hungry 1000 gave-up])) "bread can be baked")
    (is (holds? [] (view 100000 [:hungry 1000 gave-up] [:food-source 2000 {:pos {:x 1 :y 64 :z 1} :kind :farm}]))
        "a source learned since")
    (is (holds? [] (view 602000 [:hungry 1000 gave-up])) "the rest is over")))

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
              parent {:check (constantly true)
                      :round (fn ^:async r [c]
                               (let [s (await (ctx/call-child c :kid 'jobs.survival.eat {:until 21}))]
                                 (reset! out (ctx/child-result c :kid))
                                 s))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'eat-parent parent))]
          (core/submit! eng '(eat-parent) {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "eat"))))
          (is (= {:ate 0 :reason :eat-failed} (select-keys @out [:ate :reason]))))))))

(deftest eat-is-done-when-the-last-food-is-eaten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2} :inventory [{:name "bread" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= 7 (food p))))))))

(deftest eat-declines-without-edible-food-or-when-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [world [{:self {:food 4} :inventory [{:name "rotten_flesh" :count 2} {:name "spider_eye" :count 1}
                                                    {:name "cobblestone" :count 9}]}
                       {:self {:food 18} :inventory [{:name "bread" :count 2}]}]]
          (let [{:keys [eng p]} (setup world)]
            (core/submit! eng '(jobs.survival.eat) {})
            (is (nil? (core/tick! eng)) "the check declines")
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
          (is (= 1 (count (filter #(and (= :refused (:kind %)) (re-find #"allow-bad" (str (:text %)))) @seen)))
              "one refused event names :allow-bad"))))))

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

(deftest eat-keeps-precious-food-for-a-named-meal-or-low-health
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4 :health 20} :inventory [{:name "golden_apple" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (is (nil? (core/tick! eng)) "healthy and unnamed: not eaten")
          (is (= [] (calls p "eat"))))
        (let [{:keys [eng p]} (setup {:self {:food 4 :health 20} :inventory [{:name "golden_apple" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat {:item "golden_apple"}) {})
          (await (core/tick! eng))
          (is (= ["golden_apple"] (call-args p "eat" "item")) "named"))
        (let [{:keys [eng p]} (setup {:self {:food 4 :health 4} :inventory [{:name "golden_apple" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= ["golden_apple"] (call-args p "eat" "item")) "low health"))
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
          (is (= 1 (count (filter #(and (= :refused (:kind %)) (= :not-food (:reason %))) @seen)))))))))

(deftest eat-keeps-harmful-and-precious-food-last-whatever-their-points
  (is (= "dried_kelp" (eat/best-food [{:name "spider_eye"} {:name "dried_kelp"}] true nil 20)) "harmful last")
  (is (= "dried_kelp" (eat/best-food [{:name "golden_apple"} {:name "dried_kelp"}] false nil 4)) "precious after common"))

(deftest eat-chooses-by-saturation-when-points-tie
  (is (= "cooked_mutton" (eat/best-food [{:name "cooked_chicken"} {:name "cooked_mutton"}] false nil 20)))
  (is (= "cooked_mutton" (eat/best-food [{:name "honey_bottle"} {:name "cooked_mutton"}] false nil 20))))

(deftest golden-apples-are-rare-for-the-top-up-trigger
  (is (not (hungry/top-up? 16 8 ["golden_apple"])))
  (is (hungry/top-up? 16 8 ["honey_bottle"])))

;; ---------------------------------------------------------------- get-food

(deftest get-food-eats-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 3} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 10))
          (is (= 18 (food p)) "eats until fed, as the eat job does")
          (is (= [] (tu/walk-calls p)))
          (is (= [] (calls p "attack"))))))))

(deftest get-food-declines-when-not-hungry
  (let [{:keys [eng]} (setup {:self {:food 12}})]
    (core/submit! eng '(jobs.survival.get-food) {})
    (is (nil? (core/tick! eng)))))

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
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 40))
          (is (= [] (:list (core/state eng))))
          (is (= [{:x 10 :y 64 :z 0} {:x 12 :y 64 :z 0}] (call-args p "dig" "pos"))
              "only the mature ones, the young carrots are left")
          (is (= 2 (count (calls p "eat"))))
          (is (= 10 (food p)))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

(deftest get-food-ignores-a-source-beyond-its-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0}})]
          (know-source! eng {:x 500 :y 64 :z 0} :farm)
          (core/submit! eng '(jobs.survival.get-food) {})
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
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (= ["bread"] (call-args p "transfer" "item")) "the best food in the chest, not the stone")
          (is (= ["withdraw"] (call-args p "transfer" "direction")))
          (is (= 20 (food p)) "then eats it"))))))

(def cow
  {:id 7 :name "cow" :kind "passive" :pos {:x 5 :y 64 :z 0} :health 20 :drops [{:name "beef" :count 1}]})

(deftest get-food-hunts-an-animal-when-no-source-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 2} :entities [cow]})]
          (core/submit! eng '(jobs.survival.get-food {:attack-gap-ms 0}) {})
          (await (run-until-empty eng 40))
          (is (= [] (:list (core/state eng))))
          (is (= [7 7 7 7] (call-args p "attack" "id")) "20 health, 5 a swing")
          (is (= ["beef"] (call-args p "eat" "item")))
          (is (= 7 (food p)))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

(deftest get-food-forages-a-berry-bush-when-there-are-no-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 1}
                                      :blocks {"6,64,0" "sweet_berry_bush"} :ages {"6,64,0" 3}
                                      :drops {:sweet_berry_bush "sweet_berries"}})]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (= [{:x 6 :y 64 :z 0}] (call-args p "dig" "pos")))
          (is (= ["sweet_berries"] (call-args p "eat" "item"))))))))

(deftest get-food-says-so-once-per-cooldown-when-nothing-is-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 1}})
              none #(filterv (fn [e] (= :food.none (:kind e))) @seen)
              run! (fn ^:async run! []
                     (core/submit! eng '(jobs.survival.get-food) {})
                     (await (run-until-empty eng 10)))]
          (know-source! eng {:x 300 :y 64 :z 0} :farm)
          (await (run!))
          (is (= [] (:list (core/state eng))) "done, so the list resumes work")
          (is (= 1 (count (none))))
          (is (= :warn (:level (first (none)))))
          (is (re-find #"farm" (:text (first (none)))) "names the nearest known source")
          (is (= 1 (count (entries eng :hungry))))
          (swap! clock + 60000)
          (await (run!))
          (is (= 1 (count (none))) "inside the cooldown it stays quiet")
          (swap! clock + 600000)
          (await (run!))
          (is (= 2 (count (none))) "after the cooldown it says so again"))))))

(deftest get-food-skips-wheat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 1}
                                           :blocks {"6,64,0" "wheat"} :ages {"6,64,0" 7}
                                           :drops {:wheat "wheat"}})]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (= [] (calls p "dig")))
          (is (= 1 (count (filterv #(= :food.none (:kind %)) @seen)))))))))

(deftest get-food-forgets-a-source-found-empty
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:food 1} :containers {"20,64,0" [{:name "cobblestone" :count 3}]}})
              none #(filterv (fn [e] (= :food.none (:kind e))) @seen)]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (= [] (entries eng :food-source)))
          (is (= 1 (count (none))))
          (is (re-find #"no food source is known" (:text (first (none))))))))))

(deftest get-food-harvests-in-sight-during-its-ask-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:self {:food 1}
                                                 :blocks {"6,64,0" "carrots"} :ages {"6,64,0" 7}
                                                 :drops {:carrots "carrot"}})]
          (mem/write! (:store eng) :hungry {:food 1} {:cap 10 :ttl 3600000})
          (swap! clock + 60000)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (= [{:x 6 :y 64 :z 0}] (call-args p "dig" "pos")))
          (is (= ["carrot"] (call-args p "eat" "item")))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

(deftest get-food-declines-in-its-ask-cooldown-with-nothing-in-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:self {:food 1}})]
          (mem/write! (:store eng) :hungry {:food 1} {:cap 10 :ttl 3600000})
          (know-source! eng {:x 20 :y 64 :z 0} :farm)
          (swap! clock + 60000)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (core/tick! eng))
          (is (= [] (tu/walk-calls p)) "does not walk to remembered sources")
          (is (= 1 (count (entries eng :hungry))))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

(def bread-chest {"20,64,0" [{:name "cobblestone" :count 30} {:name "bread" :count 4}]})

(defn write-hungry! [eng]
  (mem/write! (:store eng) :hungry {:food 1} {:cap 10 :ttl 3600000}))

(deftest get-food-uses-a-source-learned-after-it-gave-up-during-its-ask-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:self {:food 0} :containers bread-chest})]
          (write-hungry! eng)
          (swap! clock + 60000)
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (swap! clock + 1000)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (seq (tu/walk-calls p)) "walks to the new chest")
          (is (= ["bread"] (call-args p "transfer" "item")))
          (is (= 20 (food p)) "eats the bread")
          (is (= 1 (count (entries eng :hungry))))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

(deftest get-food-skips-a-source-known-before-it-gave-up-during-its-ask-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:self {:food 0} :containers bread-chest})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (swap! clock + 1000)
          (write-hungry! eng)
          (swap! clock + 60000)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (core/tick! eng))
          (is (= [] (tu/walk-calls p)))
          (is (= 1 (count (entries eng :hungry))))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

(deftest get-food-forgets-a-fresh-source-found-empty-during-its-ask-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 0} :containers {"20,64,0" [{:name "cobblestone" :count 3}]}})]
          (write-hungry! eng)
          (swap! clock + 60000)
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (swap! clock + 1000)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (= [] (entries eng :food-source)))
          (is (= 1 (count (entries eng :hungry))))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

;; ---------------------------------------------------------------- bread rung

(def table-near {"3,64,0" "crafting_table"})

(defn carried [p name]
  (transduce (comp (filter #(= name (.-name %))) (map #(.-count %))) + 0 (array-seq (.-inventory (.self p)))))

(defn none-events [seen] (filterv #(= :food.none (:kind %)) @seen))

(defn ^:async run-food!
  "A new firing of get-food (hungry below 14, so food 12 is hungry), run until it ends."
  [eng]
  (core/submit! eng '(jobs.survival.get-food {:food 14}) {})
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

(deftest get-food-remembers-it-cannot-bake-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 12} :inventory [{:name "wheat" :count 5}]})]
          (await (run-food! eng))
          (is (= 1 (count (entries eng :no-bake))))
          (is (= "no-table" (:reason (first (entries eng :no-bake)))))
          (is (some #(= :food.no-bake (:kind %)) @seen))
          (is (= 1 (count (none-events seen))) "went on down the ladder")
          (is (re-find #"carrying 5 wheat but cannot bake \(no-table\)" (:text (first (none-events seen))))))))))

(deftest get-food-does-not-retry-baking-until-the-memory-expires
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:self {:food 12} :inventory [{:name "wheat" :count 5}]})]
          (await (run-food! eng))
          (let [n (count (calls p "craft"))]
            (swap! clock + 60000)
            (await (run-food! eng))
            (is (= n (count (calls p "craft"))) "a second firing inside 10 minutes makes no craft call")
            (swap! clock + 600000)
            (await (run-food! eng))
            (is (> (count (calls p "craft")) n) "after 10 minutes it tries again")))))))

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
    (is (holds? {:food 19 :health 6 :inventory [{:name "golden_apple" :count 1}]}) "precious food at low health")
    (is (not (holds? {:food 19 :health 6 :inventory []})) "nothing to eat and food enough to heal")
    (is (not (holds? {:food 20 :health 6 :inventory bread})) "full")
    (is (not (holds? {:food 19 :health 7 :inventory bread})) "at the line")
    (is (not (holds? {:food 19 :health 6 :inventory bread} {:health 5})) "the health line is an arg")))

(deftest get-food-eats-to-full-below-the-health-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 19 :health 5} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 10))
          (is (= 20 (food p)) "saturation heals fastest on a full bar")
          (is (= [] (tu/walk-calls p))))))))

(deftest eat-prefers-common-food-over-golden-apple
  (is (= "bread" (eat/best-food [{:name "golden_apple"} {:name "bread"}] false nil 20))))
