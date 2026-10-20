(ns engine.fertilize-test
  "jobs.farm.fertilize against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(defn setup [world & [shared]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (or shared (ew/of-data {} {} []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn age [p k] (get (:ages @(fake/state p)) (fake/parse-cell k)))

(defn ^:async child-outcome
  "tu/child-outcome without the ending the adapter adds, and without :status when it is :done."
  [eng job args n]
  (let [r (await (tu/child-outcome eng job args n))]
    (if (map? r) (cond-> (dissoc r :ending) (= :done (:status r)) (dissoc :status)) r)))

(def job 'jobs.farm.fertilize)
(def meal [{:name "bone_meal" :count 20}])
(def two-wheat {:inventory meal
                :blocks {"2,64,0" "wheat" "3,64,1" "wheat"}
                :ages {"2,64,0" 3 "3,64,1" 4}})

(deftest fertilize-ripens-every-unripe-crop-then-reports
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-wheat)
              r (await (child-outcome eng job {} 20))]
          (is (= 7 (age p "2,64,0")))
          (is (= 7 (age p "3,64,1")))
          (is (= (count (calls p "useOn")) (:used r)))
          (is (= 4 (:used r))))))))

(deftest fertilize-is-one-call-the-whole-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-wheat)
              r (await (child-outcome eng job {} 1))]
          (is (= 4 (:used r)))
          (is (= 7 (age p "2,64,0"))))))))

(deftest fertilize-leaves-bone-meal-an-agent-reserve-entry-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-wheat)
              _ (mem/write! (:store eng) :reserve {:id "meal" :item "bone_meal" :count :all} {:cap 100 :ttl :forever})
              _ (await (child-outcome eng job {} 20))]
          (is (empty? (calls p "useOn")))
          (is (= 3 (age p "2,64,0"))))))))

(deftest fertilize-never-targets-a-ripe-crop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory meal :blocks {"2,64,0" "wheat" "3,64,1" "beetroots"}
                                      :ages {"2,64,0" 7 "3,64,1" 3}})]
          (is (= {:used 0} (dissoc (await (child-outcome eng job {} 5)) :text)))
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-max-stops-after-that-many-uses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-wheat)]
          (is (= {:used 1} (dissoc (await (child-outcome eng job {:max 1} 10)) :text)))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest fertilize-waits-without-bone-meal-while-crops-are-unripe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}})]
          (core/submit! eng (list job {:fetch false}) {})
          (is (nil? (core/tick! eng)) "no bone meal: not yet")
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-refuses-a-crop-that-stays-unchanged-and-stops-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory meal :blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}})]
          (.override (.-world p) "useOn"
                     (fn ^:async f [_ _ _] #js {:status "unchanged" :consumed 0}))
          (is (= {:used 0 :status :stopped :reason :refused :kind :condition-failed}
                 (let [r (await (tu/child-outcome eng job {} 10))] (assoc (dissoc r :ending :text) :kind (get-in r [:ending :kind])))))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest fertilize-at-touches-only-that-crop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-wheat)
              r (await (child-outcome eng job {:at {:x 2 :y 64 :z 0}} 20))]
          (is (= 7 (age p "2,64,0")))
          (is (= 4 (age p "3,64,1")))
          (is (= 2 (:used r))))))))

(deftest fertilize-finishes-when-the-bone-meal-runs-out-after-some-was-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc two-wheat :inventory [{:name "bone_meal" :count 1}]))]
          (is (= {:used 1} (dissoc (await (child-outcome eng job {} 10)) :text)))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest fertilize-center-moves-the-radius-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory meal
                                      :blocks {"30,64,0" "wheat" "2,64,0" "wheat"}
                                      :ages {"30,64,0" 5 "2,64,0" 5}})]
          (await (child-outcome eng job {:center {:x 30 :y 64 :z 2} :radius 4} 20))
          (is (= 7 (age p "30,64,0")) "near the centre, far from the body")
          (is (= 5 (age p "2,64,0")) "near the body, outside the centre's radius"))))))

;; ------------------------------------------------------------------ zones and claims

(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(def zone-over-one {:name "farm" :min [2 60 0] :max [2 70 0]})

(deftest a-crop-follows-its-zone-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra used] [["Fake" {} 4] ["FAKE" {} 4] ["Miles" {} 2]
                                         ["Miles" {:ignore-zones? true} 4]]]
          (let [{:keys [eng seen]} (setup two-wheat (ew/of-data {} {} [(assoc zone-over-one :owner owner)]))
                r (await (child-outcome eng job extra 30))]
            (is (= used (:used r)) (pr-str [owner extra]))
            (is (empty? (kinds seen :fertilize.declined)) "a condition does not warn")))))))

(deftest no-zone-list-declines-the-fertilizing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup two-wheat (ew/of-data {} {} nil))]
          (core/submit! eng (list job {}) {})
          (await (tu/run-until-empty eng 10))
          (is (empty? (calls p "useOn")))
          (is (= 1 (count (:list (core/state eng)))) "waits, does not end")
          (is (empty? (kinds seen :fertilize.declined))))))))

(deftest fertilize-finds-an-unripe-crop-past-many-nearer-ripe-ones
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [xs (range -4 5)
              cells (for [x xs z xs :when (not= z 0)] (str x ",64," z))
              {:keys [eng p]} (setup {:inventory meal
                                      :blocks (assoc (zipmap cells (repeat "wheat")) "7,64,0" "wheat")
                                      :ages (assoc (zipmap cells (repeat 7)) "7,64,0" 3)})]
          (await (child-outcome eng job {:radius 10} 20))
          (is (= 7 (age p "7,64,0"))))))))

;; ------------------------------------------------------------------ grass

(def two-grass {:inventory meal :blocks {"2,63,0" "grass_block" "3,63,1" "grass_block"}})

(deftest fertilize-grass-uses-bone-meal-on-each-open-grass-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-grass)
              r (await (child-outcome eng job {:grass true} 20))]
          (is (= {:used 2} (dissoc r :text)))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest fertilize-grass-off-leaves-grass-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-grass)]
          (is (= {:used 0} (dissoc (await (child-outcome eng job {} 10)) :text)))
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-grass-skips-covered-grass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory meal :blocks {"2,63,0" "grass_block" "2,64,0" "stone"}})]
          (is (= {:used 0} (dissoc (await (child-outcome eng job {:grass true} 10)) :text)))
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-grass-max-stops-after-that-many-uses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-grass)]
          (is (= {:used 1} (dissoc (await (child-outcome eng job {:grass true :max 1} 10)) :text)))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest fertilize-grass-waits-without-bone-meal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (dissoc two-grass :inventory))]
          (core/submit! eng (list job {:grass true :fetch false}) {})
          (is (nil? (core/tick! eng)))
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-works-in-the-footprint-of-its-own-plan-but-not-another-bodys
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[author ripe] [["Fake" 7] ["Miles" 3]]]
          (let [plan (shape/with-author {:id "hut" :parts [{:id "p" :box [[2 64 0] [2 64 0]] :want "stone"}]} author)
                {:keys [eng p]} (setup {:inventory meal :blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}} (ew/of-data {"hut" plan} {} []))]
            (await (child-outcome eng job {} 20))
            (is (= ripe (age p "2,64,0")) author)))))))

(def sapling-world {:inventory meal :blocks {"2,64,0" "oak_sapling"}})

(deftest fertilize-at-a-sapling-uses-bone-meal-up-to-max
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup sapling-world)
              r (await (child-outcome eng job {:at [2 64 0] :max 3} 20))]
          (is (= 3 (count (calls p "useOn"))))
          (is (= 3 (:used r))))))))

(deftest fertilize-at-a-sapling-stops-when-it-grew
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup sapling-world)]
          (.override (.-world p) "useOn"
                     (fn ^:async f [token a impl]
                       (let [r (await (impl token a))]
                         (fake/set-block! p [2 64 0] "oak_log")
                         r)))
          (let [r (await (child-outcome eng job {:at [2 64 0] :max 3} 20))]
            (is (= 1 (count (calls p "useOn"))))
            (is (= 1 (:used r)))))))))
