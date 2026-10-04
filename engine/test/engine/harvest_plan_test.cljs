(ns engine.harvest-plan-test
  "jobs.farm.harvest working a plan (:plan id, optional :part) against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.harvest-test :as h]
            [jobs.farm.harvest :as harvest]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as world]))

(defn start
  "An engine over the fake world spec with the plans {id plan} as its world data."
  [spec plans]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (tu/fake spec)
        w (world/of-data plans {})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                          :world w})]
    {:eng eng :p p :seen seen :w w}))

(defn field-plan
  "A plan holding one wheat row per z over x 2..5, parts named row-<z>."
  [zs]
  {:id "field"
   :parts (mapv (fn [z] {:id (str "row-" z) :box [[2 64 z] [5 64 z]] :want {:crop "wheat"}}) zs)})

(def mixed-field
  "Row z 2 of the plan: x 2 ripe wheat, x 3 young wheat, x 4 bare farmland, x 5 ripe carrots (the plan wants wheat);
  x 6 ripe wheat one block outside the plan. Carries 5 wheat seeds."
  {:blocks (merge (h/field "wheat" 7 [2 3 6] [2]) (h/field "carrots" 7 [5] [2]) {(h/cell-key 4 63 2) "farmland"})
   :ages (merge (h/ages 7 [2 6] [2]) (h/ages 3 [3] [2]) (h/ages 7 [5] [2]))
   :inventory [{:name "wheat_seeds" :count 5}]
   :drops h/wheat-drops})

(defn dug [p] (set (map #(let [pos (.-pos (.-args %))] [(.-x pos) (.-z pos)]) (h/calls p "dig"))))
(defn placed [p] (set (map #(let [a (.-args %)] [(.-x (.-pos a)) (.-z (.-pos a)) (.-item a)]) (h/calls p "place"))))

(deftest a-plan-cuts-its-ripe-planned-crops-and-seeds-its-bare-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start mixed-field {"field" (field-plan [2])})
              result (await (h/child-outcome eng h/job {:plan "field"} 200))]
          (is (= #{[2 2]} (dug p)))
          (is (= #{[2 2 "wheat_seeds"] [4 2 "wheat_seeds"]} (placed p)))
          (is (= {:cut 1 :replanted 2 :bare [] :lost [] :gave-up false} result))
          (is (= [0 3 0] (mapv #(h/age-at p % 64 2) [2 3 4])))
          (is (= ["carrots" 7] [(h/block-at p 5 64 2) (h/age-at p 5 64 2)]))
          (is (= ["wheat" 7] [(h/block-at p 6 64 2) (h/age-at p 6 64 2)])))))))

(deftest a-bare-planned-cell-without-its-seed-is-reported-bare
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start (merge mixed-field {:inventory [] :drops {"wheat" ["wheat"]}})
                                     {"field" (field-plan [2])})
              result (await (h/child-outcome eng h/job {:plan "field"} 200))]
          (is (= #{[2 2]} (dug p)))
          (is (= {:cut 1 :replanted 0 :bare [{:x 2 :y 64 :z 2} {:x 4 :y 64 :z 2}] :lost [] :gave-up false}
                 (update result :bare #(vec (sort-by :x %))))))))))

(deftest a-part-limits-the-field-to-that-part
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (h/field "wheat" 7 [2 3] [2 3]) :ages (h/ages 7 [2 3] [2 3]) :drops h/wheat-drops}
                                     {"field" (field-plan [2 3])})
              result (await (h/child-outcome eng h/job {:plan "field" :part "row-3"} 200))]
          (is (= #{[2 3] [3 3]} (dug p)))
          (is (= 2 (:cut result))))))))

(defn declines
  "Submit harvest with args over the mixed field and plans, tick a few times: [digs, places, harvest.declined warns]."
  [plans args]
  (let [{:keys [eng p seen]} (start mixed-field plans)]
    (core/submit! eng (list h/job args) {})
    (dotimes [_ 4] (swap! h/clock + 700) (core/tick! eng))
    [(count (h/calls p "dig")) (count (h/calls p "place")) (mapv #(select-keys % [:plan :reason]) (h/events-of seen :harvest.declined))]))

(deftest the-check-declines-a-plan-it-cannot-work-with-one-warn
  (let [cane {:id "cane" :parts [{:id "c" :cells [[2 64 2]] :want {:crop "sugar_cane"}}]}]
    (is (= [0 0 [{:plan "nope" :reason "no such plan"}]] (declines {} {:plan "nope"})))
    (is (= [0 0 [{:plan "cane" :reason "no crop cells"}]] (declines {"cane" cane} {:plan "cane"})))
    (is (= [0 0 [{:plan "field" :reason "no crop cells"}]] (declines {"field" (field-plan [2])} {:plan "field" :part "row-9"})))))

(deftest a-broken-plan-declines-naming-the-error
  (let [{:keys [eng p seen w]} (start mixed-field {})]
    (reset! (:state w) (assoc @(:state w) :plans {"field" {:error "unreadable EDN: eof"}}))
    (core/submit! eng (list h/job {:plan "field"}) {})
    (dotimes [_ 3] (swap! h/clock + 700) (core/tick! eng))
    (is (zero? (count (h/calls p "dig"))))
    (is (= [{:plan "field" :reason "the plan cannot be read: unreadable EDN: eof"}]
           (mapv #(select-keys % [:plan :reason]) (h/events-of seen :harvest.declined))))))

(deftest a-row-dropped-from-the-plan-is-left-alone-from-the-next-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p w]} (start {:blocks (h/field "wheat" 7 (range 2 6) [2 9]) :ages (h/ages 7 (range 2 6) [2 9])
                                        :drops h/wheat-drops}
                                       {"field" (field-plan [2 9])})]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (world/set-data! w {"field" (field-plan [2])} {})
                       (await (impl token args))))
          (let [result (await (h/child-outcome eng h/job {:plan "field"} 200))]
            (is (= #{2} (set (map second (dug p)))))
            (is (= 4 (:cut result)))
            (is (every? #(= 7 (h/age-at p % 64 9)) (range 2 6)))))))))

(deftest without-a-plan-the-check-does-not-ask-for-one
  (let [{:keys [eng seen]} (start mixed-field {})]
    (core/submit! eng (list h/job {}) {})
    (swap! h/clock + 700)
    (core/tick! eng)
    (is (empty? (h/events-of seen :harvest.declined)))))

(deftest a-cell-this-job-cut-stays-owed-when-its-row-leaves-the-plan
  (let [a {:x 1 :y 64 :z 0} b {:x 2 :y 64 :z 0} c {:x 3 :y 64 :z 0}]
    (is (= [{:pos a :seed "wheat_seeds" :fails 1} {:pos b :seed "wheat_seeds" :cut true}]
           (:replant (harvest/sync-debts {:replant [{:pos b :seed "wheat_seeds" :cut true} {:pos c :seed "carrot"}
                                                    {:pos a :seed "carrot" :fails 1}]}
                                         [{:pos a :seed "wheat_seeds"}]))))))

(deftest a-crop-dropped-from-the-plan-during-the-walk-to-it-is-not-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p w]} (start {:blocks (h/field "wheat" 7 (range 2 6) [9]) :ages (h/ages 7 (range 2 6) [9])
                                        :drops h/wheat-drops}
                                       {"field" (field-plan [2 9])})]
          (.override (.-world p) "moveTo"
                     (fn ^:async f [token args impl]
                       (world/set-data! w {"field" (field-plan [2])} {})
                       (await (impl token args))))
          (let [result (await (h/child-outcome eng h/job {:plan "field"} 200))]
            (is (= #{} (dug p)))
            (is (= 0 (:cut result)))))))))
