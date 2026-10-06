(ns engine.plant-test
  "jobs.farm.plant against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.farm.plant :as plant]))

(def clock (atom 1000000))

(defn start
  "An engine over primitives made from world, over the shared world (no zones by default)."
  [world & [shared]]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (or shared (ew/of-data {} {} []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (swap! clock + 700)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent; the child's result, :not-done when it never finished."
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
(defn inv-of [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn block-at [p x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-name)))
(defn age-at [p x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-age)))
(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn inv [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))

(def job 'jobs.farm.plant)

(defn cell-key [x y z] (str x "," y "," z))

(defn farmland
  "Farmland at y 63 over the xs by zs."
  [xs zs]
  (into {} (for [x xs z zs] [(cell-key x 63 z) "farmland"])))

(defn box [x0 z0 x1 z1] {:min {:x x0 :y 63 :z z0} :max {:x x1 :y 63 :z z1}})

(def field-box (box 2 2 4 4))

(defn field-world [seeds & [extra]]
  (merge {:blocks (farmland (range 2 5) (range 2 5)) :inventory seeds :floor tu/walk-floor} extra))

(defn sown [p xs zs block]
  (every? (fn [[x z]] (and (= block (block-at p x 64 z)) (= 0 (age-at p x 64 z)))) (for [x xs z zs] [x z])))

(deftest bare-cells-are-farmland-with-air-above-inside-the-box
  (let [p (tu/fake {:blocks {"0,63,0" "farmland" "1,63,0" "farmland" "2,63,0" "farmland" "3,63,0" "farmland"
                             "4,63,0" "dirt" "5,63,0" "farmland" "1,64,0" "wheat" "2,64,0" "stone"}})
        xs (fn [cells] (mapv :x cells))
        b (fn [x0 x1] {:min {:x x0 :y 63 :z 0} :max {:x x1 :y 63 :z 0}})]
    (are [box skipped expected] (= expected (xs (plant/bare-cells p box skipped)))
      (b 0 5) [] [0 3 5]
      (b 0 3) [] [0 3]
      (b 1 2) [] []
      (b 0 5) [{:x 3 :y 63 :z 0}] [0 5]
      (b 4 4) [] []
      {:min {:x 0 :y 62 :z 0} :max {:x 5 :y 62 :z 0}} [] [])))

(deftest an-unloaded-cell-is-not-bare
  (let [p (tu/fake {:blocks {"0,63,0" "farmland" "1,63,0" "farmland"} :unloaded ["1,64,0"]})]
    (is (= [0] (mapv :x (plant/bare-cells p (box 0 0 1 0) []))))))

(deftest pick-seed-takes-the-named-seed-or-the-largest-carried-stack
  (are [seed carried expected] (= expected (plant/pick-seed seed (apply inv carried)))
    nil ["wheat_seeds" 4] "wheat_seeds"
    nil ["wheat_seeds" 4 "carrot" 9 "potato" 1] "carrot"
    nil ["dirt" 64 "beetroot_seeds" 2] "beetroot_seeds"
    "wheat_seeds" ["wheat_seeds" 1 "carrot" 9] "wheat_seeds"
    "potato" ["wheat_seeds" 1] nil
    nil ["dirt" 64] nil
    nil [] nil))

(deftest count-fail-skips-a-cell-at-the-third-fail
  (let [pos {:x 1 :y 63 :z 1}
        other {:x 2 :y 63 :z 1}
        step (fn [m p] (plant/count-fail m :fails p))]
    (are [positions skipped] (= skipped (:skipped (reduce step {} positions)))
      [pos] nil
      [pos pos] nil
      [pos pos pos] [pos]
      [pos other pos other pos] [pos]
      [pos pos other pos] [pos])))

(deftest owes-needs-a-box-and-either-a-started-run-or-bare-cells-with-a-seed
  (let [world (tu/fake (field-world (inv "wheat_seeds" 1)))
        noseed (tu/fake (field-world []))
        full (tu/fake {:blocks (merge (farmland [2] [2]) {"2,64,2" "wheat"}) :inventory (inv "wheat_seeds" 1)})
        args (fn [b] {:box b :seed nil :reach 4.2})]
    (are [p b m expected] (= expected (plant/owes? p (args b) m))
      world field-box {} true
      world nil {} false
      noseed field-box {} false
      full (box 2 2 2 2) {} false
      full (box 2 2 2 2) {:started true} true
      noseed field-box {:started true} true
      world nil {:started true} false
      world field-box {:skipped (vec (for [x (range 2 5) z (range 2 5)] {:x x :y 63 :z z}))} false)))

(deftest a-field-is-sown-with-the-carried-seeds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start (field-world (inv "wheat_seeds" 9)))
              result (await (child-outcome eng job {:box field-box} 100))]
          (is (= {:planted 9 :skipped [] :reason :done} result))
          (is (sown p (range 2 5) (range 2 5) "wheat"))
          (is (= {} (inv-of p)))
          (is (= 1 (count (events-of seen :plant.done))))
          (is (= 0 (count (events-of seen :plant.gave-up)))))))))

(deftest fewer-seeds-than-cells-plant-what-there-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start (field-world (inv "carrot" 5)))
              result (await (child-outcome eng job {:box field-box} 100))]
          (is (= {:planted 5 :skipped [] :reason :no-seed} result))
          (is (= 5 (count (filter (fn [[x z]] (= "carrots" (block-at p x 64 z))) (for [x (range 2 5) z (range 2 5)] [x z])))))
          (is (= {} (inv-of p))))))))

(deftest a-field-wider-than-reach-is-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [xs (range 2 14)
              {:keys [eng p]} (start {:blocks (farmland xs [2]) :inventory (inv "wheat_seeds" 12) :floor tu/walk-floor})
              result (await (child-outcome eng job {:box (box 2 2 13 2)} 100))]
          (is (= {:planted 12 :skipped [] :reason :done} result))
          (is (pos? (count (tu/walk-calls p))))
          (is (sown p xs [2] "wheat")))))))

(deftest nothing-is-planted-outside-the-box-or-on-a-crop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (merge (farmland (range 0 7) [2]) {"3,64,2" "carrots"})
                                      :inventory (inv "wheat_seeds" 20) :floor tu/walk-floor})
              result (await (child-outcome eng job {:box (box 2 2 4 2)} 100))]
          (is (= {:planted 2 :skipped [] :reason :done} result))
          (is (= ["air" "air" "wheat" "carrots" "wheat" "air" "air"]
                 (mapv #(block-at p % 64 2) (range 0 7))))
          (is (= {"wheat_seeds" 18} (inv-of p))))))))

(deftest a-run-with-no-bare-cell-or-no-seed-is-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (farmland [2] [2]) :inventory (inv "wheat_seeds" 1)})]
          (core/submit! eng (list job {:box (box 5 5 6 6)}) {})
          (await (run-until-empty eng 20))
          (is (= [] (calls p "place"))))
        (let [{:keys [eng p]} (start (field-world []))]
          (core/submit! eng (list job {:box field-box}) {})
          (await (run-until-empty eng 20))
          (is (= [] (calls p "place"))))))))

(deftest the-seed-arg-wins-over-a-larger-stack
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start (field-world (inv "wheat_seeds" 20 "potato" 3)))
              result (await (child-outcome eng job {:box field-box :seed "potato"} 100))]
          (is (= {:planted 3 :reason :no-seed} (select-keys result [:planted :reason])))
          (is (= {"wheat_seeds" 20} (inv-of p))))))))

(deftest the-largest-stack-is-sown-when-no-seed-is-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start (field-world (inv "wheat_seeds" 2 "carrot" 9)))
              result (await (child-outcome eng job {:box field-box} 100))]
          (is (= {:planted 9 :reason :done} (select-keys result [:planted :reason])))
          (is (= {"wheat_seeds" 2} (inv-of p))))))))

(deftest a-cell-whose-place-is-refused-three-times-is-skipped-and-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start (field-world (inv "wheat_seeds" 9)))
              refused {:x 3 :y 63 :z 3}]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (if (= [3 64 3] [(.-x (.-pos args)) (.-y (.-pos args)) (.-z (.-pos args))])
                         #js {:status "failed" :reason "refused"}
                         (await (impl token args)))))
          (let [result (await (child-outcome eng job {:box field-box} 100))]
            (is (= {:planted 8 :skipped [refused] :reason :gave-up} result))
            (is (= 3 (count (filter #(= 3 (.-x (.-pos (.-args %)))) (filter #(= 3 (.-z (.-pos (.-args %)))) (calls p "place"))))))
            (is (= 1 (count (events-of seen :plant.gave-up))))
            (is (= 1 (count (events-of seen :plant.done))))))))))

;; ------------------------------------------------------------------ zones and claims

(def zone-over-cell {:name "farm" :min [2 60 2] :max [2 70 2]})

(deftest a-cell-follows-its-zone-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra planted declined] [["Fake" {} 9 []] ["FAKE" {} 9 []]
                                                ["Miles" {} 8 [{:reason :refused :zones ["farm"]}]]
                                                ["Miles" {:ignore-zones? true} 9 []]]]
          (let [{:keys [eng p seen]} (start (field-world (inv "wheat_seeds" 9)) (ew/of-data {} {} [(assoc zone-over-cell :owner owner)]))
                result (await (child-outcome eng job (merge {:box field-box} extra) 100))]
            (is (= planted (:planted result)) (pr-str [owner extra]))
            (is (= declined (mapv #(select-keys % [:reason :zones]) (events-of seen :plant.declined))) (pr-str [owner extra]))
            (is (= (if (= 8 planted) "air" "wheat") (block-at p 2 64 2)) (pr-str [owner extra]))))))))

(deftest no-zone-list-declines-the-sowing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start (field-world (inv "wheat_seeds" 9)) (ew/of-data {} {} nil))]
          (core/submit! eng (list job {:box field-box}) {})
          (await (run-until-empty eng 20))
          (is (empty? (calls p "place")))
          (is (= [:no-zones] (mapv :reason (events-of seen :plant.declined)))))))))

(defn shared-plans
  "Plan mix wants wheat at (2,64,2) and (3,64,2); plan other claims the cells of other-cells."
  [other-cells]
  {"mix" {:id "mix" :parts [{:id "w" :cells [[2 64 2] [3 64 2]] :want {:crop "wheat"}}]}
   "other" {:id "other" :parts [{:id "o" :cells other-cells :want {:crop "wheat"}}]}})

(defn ^:async run-plan [other-cells]
  (let [{:keys [eng p seen]} (start {:blocks (farmland (range 2 4) [2]) :inventory (inv "wheat_seeds" 6) :floor tu/walk-floor}
                                    (ew/of-data (shared-plans other-cells) {} []))]
    (core/submit! eng (list job {:plan "mix"}) {})
    (await (run-until-empty eng 40))
    {:p p :seen seen}))

(deftest cells-shared-with-another-plan-are-left-bare-and-named-in-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (run-plan [[3 64 2]]))
              declined (events-of seen :plant.declined)]
          (is (= "wheat" (block-at p 2 64 2)))
          (is (= "air" (block-at p 3 64 2)))
          (is (= [[:refused ["other"]]] (mapv (juxt :reason :plans) declined))))))))

(deftest every-cell-refused-by-another-plan-warns-once-though-the-job-never-starts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (run-plan [[2 64 2] [3 64 2]]))
              declined (events-of seen :plant.declined)]
          (is (= "air" (block-at p 2 64 2)))
          (is (= [[:refused ["other"]]] (mapv (juxt :reason :plans) declined))))))))
