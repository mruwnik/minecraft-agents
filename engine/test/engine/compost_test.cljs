(ns engine.compost-test
  "jobs.farm.compost against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.farm.compost :as compost]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome [eng job args n]
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
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn carried [p name]
  (->> (array-seq (.-inventory (.self p))) (filter #(= name (.-name %))) (map #(.-count %)) (reduce + 0)))

(def job 'jobs.farm.compost)
(def at {:x 2 :y 63 :z 0})
(def pos-key "2,63,0")
(def comp-block {pos-key "composter"})

(defn inv [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))

(deftest feedable-cases
  (are [inventory args expected] (= expected (compost/feedable inventory args))
    (inv "wheat_seeds" 5 "oak_leaves" 3) {:items nil :keep {}} [["oak_leaves" 3]]
    (inv "bread" 2 "cookie" 1 "oak_leaves" 1 "stick" 4) {:items nil :keep {}} [["oak_leaves" 1]]
    (inv "oak_leaves" 3 "oak_leaves" 2) {:items nil :keep {}} [["oak_leaves" 5]]
    (inv "wheat_seeds" 5 "oak_leaves" 3) {:items ["wheat_seeds"] :keep {}} [["wheat_seeds" 5]]
    (inv "wheat_seeds" 5 "oak_leaves" 3) {:items ["wheat_seeds" "oak_leaves"] :keep {"wheat_seeds" 2}} [["wheat_seeds" 3] ["oak_leaves" 3]]
    (inv "oak_leaves" 3) {:items nil :keep {"oak_leaves" 3}} []
    (inv "oak_leaves" 3) {:items nil :keep {"oak_leaves" 9}} []
    (inv "stick" 3) {:items ["stick"] :keep {}} []
    (inv "oak_leaves" 3) {:items ["wheat_seeds"] :keep {}} []))

(deftest compost-feeds-asked-items-and-takes-the-bone-meal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (inv "wheat_seeds" 10) :blocks comp-block})
              result (await (child-outcome eng job {:at at :items ["wheat_seeds"]} 40))]
          (is (= 1 (:bone-meal result)))
          (is (= {"wheat_seeds" 7} (:fed result)))
          (is (= 3 (carried p "wheat_seeds")))
          (is (= 1 (count (kinds seen :compost.done)))))))))

(deftest compost-finds-the-nearest-composter-when-not-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory (inv "wheat_seeds" 10) :blocks comp-block})
              result (await (child-outcome eng job {:items ["wheat_seeds"]} 40))]
          (is (= 1 (:bone-meal result))))))))

(deftest compost-default-holds-back-seeds-and-bread
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (inv "oak_leaves" 20 "wheat_seeds" 5 "bread" 3) :blocks comp-block})
              result (await (child-outcome eng job {:at at} 40))]
          (is (= {"oak_leaves" 7} (:fed result)))
          (is (= 1 (:bone-meal result)))
          (is (= 5 (carried p "wheat_seeds")))
          (is (= 3 (carried p "bread"))))))))

(deftest compost-keep-reserves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (inv "oak_leaves" 4) :blocks comp-block})
              result (await (child-outcome eng job {:at at :keep {"oak_leaves" 2}} 40))]
          (is (= {"oak_leaves" 2} (:fed result)))
          (is (= :nothing-to-feed (:reason result)))
          (is (= 2 (carried p "oak_leaves"))))))))

(deftest compost-empties-a-full-composter-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [] :blocks comp-block :states {pos-key {:level 8}}})
              result (await (child-outcome eng job {:at at} 20))]
          (is (= 1 (:bone-meal result)))
          (is (= {} (:fed result)))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest compost-no-composter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:inventory (inv "oak_leaves" 4)})
              result (await (child-outcome eng job {} 10))]
          (is (= :no-composter (:reason result)))
          (is (= 0 (:bone-meal result)))
          (is (= 1 (count (kinds seen :compost.no-composter)))))))))

(deftest compost-block-at-is-not-a-composter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory (inv "oak_leaves" 4) :blocks {pos-key "stone"}})
              result (await (child-outcome eng job {:at at} 10))]
          (is (= :no-composter (:reason result))))))))

(deftest compost-nothing-to-feed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (inv "stick" 4) :blocks comp-block})
              result (await (child-outcome eng job {:at at} 10))]
          (is (= :nothing-to-feed (:reason result)))
          (is (= 0 (:bone-meal result)))
          (is (empty? (calls p "useOn"))))))))

(deftest compost-gives-up-after-three-refusals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (inv "oak_leaves" 4) :blocks comp-block})]
          (.override (.-world p) "useOn" (fn ^:async f [_ _ _] #js {:status "unchanged" :before #js {:name "composter"} :after #js {:name "composter"} :consumed 0}))
          (let [result (await (child-outcome eng job {:at at} 20))]
            (is (= :gave-up (:reason result)))
            (is (= 3 (count (calls p "useOn"))))
            (is (= 1 (count (kinds seen :compost.gave-up))))))))))
