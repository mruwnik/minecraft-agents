(ns engine.rest-test
  "The low-health trigger and jobs.survival.rest: a hurt body that heals by waiting rests before it goes on working."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.cornered-test :as ct]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn holds? [self] (boolean ((:when (get triggers/all :low-health)) (tu/fake {:self self}) {} {})))

(deftest low-health-holds-only-for-a-hurt-body-that-heals-by-waiting
  (are [self expected] (= expected (holds? self))
    {:health 4 :food 18} true
    {:health 4 :food 17} false
    {:health 6 :food 20} false
    {:health 0 :food 20} false
    {:health 4 :food 20 :effects [{:name "poison" :amplifier 0 :duration 400}]} false
    {:health 4 :food 20 :onFire true} false
    {:health 4 :food 20 :isSleeping true} false))

(def pocket "A body shut in rock at [0 64 0]: shut-in is done at once." (ct/rock [-3 3] [62 67] #{[0 64 0] [0 65 0]}))

(defn ^:async run!
  "A 4-hp body in the pocket with the low-health reflex registered and a listed job that records the health it starts at;
  each wait heals heal hp. {:worked-at :p :eng}."
  [self inventory heal]
  (let [clock (atom 1000000)
        worked-at (atom nil)
        p (tu/seeing-all (tu/fake {:self (merge {:health 4} self) :blocks pocket :inventory inventory}))
        _ (.override (.-world p) "wait" (fn [token a impl] (swap! (fake/state p) update-in [:self :health] #(min 20 (+ % heal))) (impl token a)))
        work (tu/job (fn [c] (reset! worked-at (.-health (.self (:primitives c)))) (ctx/end c :done nil)))
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'work work) :triggers triggers/all :dir (tu/tmp-dir)
                          :now (tu/act-clock clock p 1000)})]
    (core/load-scenario! eng (scenario/parse "{:register [{:trigger :low-health}]}"))
    (core/submit! eng '(work) {})
    (await (tu/run-until-empty eng 10))
    {:worked-at @worked-at :p p :eng eng}))

(deftest a-hurt-body-rests-to-the-rested-health-before-its-listed-job-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [worked-at p]} (await (run! {:food 19} [{:name "bread" :count 2}] 1))]
          (is (<= 12 worked-at) "the listed job started only once the body had healed")
          (is (seq (ct/calls p "eat")) "a bite keeps food at 20 so regeneration goes on")
          (is (empty? (ct/calls p "place")) "already shut in: nothing placed"))))))

(deftest a-rest-that-heals-nothing-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [worked-at]} (await (run! {:food 20} [] 0))]
          (is (= 4 worked-at) "the rest ended :not-healing and the listed job ran"))))))
