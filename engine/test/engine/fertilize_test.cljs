(ns engine.fertilize-test
  "jobs.farm.fertilize against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup [world & [shared]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (or shared (ew/of-data {} {} []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn age [p k] (get (:ages @(fake/state p)) (fake/parse-cell k)))

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

(deftest fertilize-never-targets-a-ripe-crop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory meal :blocks {"2,64,0" "wheat" "3,64,1" "beetroots"}
                                      :ages {"2,64,0" 7 "3,64,1" 3}})]
          (is (= {:used 0} (await (child-outcome eng job {} 5))))
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-max-stops-after-that-many-uses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-wheat)]
          (is (= {:used 1} (await (child-outcome eng job {:max 1} 10))))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest fertilize-waits-without-bone-meal-while-crops-are-unripe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}})]
          (core/submit! eng (list job {}) {})
          (is (nil? (core/tick! eng)) "no bone meal: not yet")
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-refuses-a-crop-that-stays-unchanged-and-finishes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory meal :blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}})]
          (.override (.-world p) "useOn"
                     (fn ^:async f [_ _ _] #js {:status "unchanged" :consumed 0}))
          (is (= {:used 0} (await (child-outcome eng job {} 10))))
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
          (is (= {:used 1} (await (child-outcome eng job {} 10))))
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
        (doseq [[owner extra used declined] [["Fake" {} 4 []] ["FAKE" {} 4 []] ["Miles" {} 2 [{:reason :refused :zones ["farm"]}]]
                                         ["Miles" {:ignore-zones? true} 4 []]]]
          (let [{:keys [eng seen]} (setup two-wheat (ew/of-data {} {} [(assoc zone-over-one :owner owner)]))
                r (await (child-outcome eng job extra 30))]
            (is (= used (:used r)) (pr-str [owner extra]))
            (is (= declined
                   (mapv #(select-keys % [:reason :zones]) (kinds seen :fertilize.declined))) (pr-str [owner extra]))))))))

(deftest no-zone-list-declines-the-fertilizing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup two-wheat (ew/of-data {} {} nil))
              r (await (child-outcome eng job {} 10))]
          (is (= {:used 0} r))
          (is (empty? (calls p "useOn")))
          (is (= [:no-zones] (mapv :reason (kinds seen :fertilize.declined)))))))))

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
          (is (= {:used 2} r))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest fertilize-grass-off-leaves-grass-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-grass)]
          (is (= {:used 0} (await (child-outcome eng job {} 10))))
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-grass-skips-covered-grass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory meal :blocks {"2,63,0" "grass_block" "2,64,0" "stone"}})]
          (is (= {:used 0} (await (child-outcome eng job {:grass true} 10))))
          (is (empty? (calls p "useOn"))))))))

(deftest fertilize-grass-max-stops-after-that-many-uses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup two-grass)]
          (is (= {:used 1} (await (child-outcome eng job {:grass true :max 1} 10))))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest fertilize-grass-waits-without-bone-meal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (dissoc two-grass :inventory))]
          (core/submit! eng (list job {:grass true}) {})
          (is (nil? (core/tick! eng)))
          (is (empty? (calls p "useOn"))))))))
