(ns engine.loot-cart-test
  "jobs.strip.loot-cart against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu :refer [child-outcome]]
            [engine.triggers :as triggers]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        now (tu/act-clock clock p 300)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn cart-ids [p] (set (map :id (filter #(= "chest_minecart" (:name %)) (fake/entities p)))))

(defn cart
  [id pos & {:as more}]
  (merge {:id id :name "chest_minecart" :kind "other" :pos pos :health 5
          :items [{:name "iron_ingot" :count 3} {:name "rail" :count 16} {:name "bread" :count 2}]
          :drops [{:name "chest_minecart" :count 1}]}
         more))

(def job 'jobs.strip.loot-cart)

(deftest loots-the-cart-and-takes-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(cart 50 [2 64 0])]})
              result (await (child-outcome eng job {:id 50} 12))]
          (is (= {"iron_ingot" 3 "rail" 16 "bread" 2 "chest_minecart" 1} (inv p)))
          (is (empty? (cart-ids p)))
          (is (= {:collected 22 :items {"iron_ingot" 3 "rail" 16 "bread" 2 "chest_minecart" 1}} result)))))))

(deftest a-cart-by-near-takes-the-nearest-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(cart 50 [2 64 0]) (cart 51 [4 64 0] :items [{:name "dirt" :count 1}])]})
              _ (await (child-outcome eng job {:near [2 64 0] :radius 1} 12))]
          (is (= #{51} (cart-ids p)))
          (is (zero? (get (inv p) "dirt" 0))))))))

(deftest an-absent-cart-stops-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})
              result (await (child-outcome eng job {:id 77} 6))]
          (is (= {:status :stopped :reason :gone :why "no chest minecart in sight" :collected 0 :items {}} result))
          (is (empty? (inv p))))))))

(deftest a-full-bag-leaves-the-rest-to-the-spill
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(cart 50 [2 64 0])]})
              _ (.override (.-world p) "transfer" (fn [_ _ _] #js {:status "full" :moved 0}))
              result (await (child-outcome eng job {:id 50} 12))]
          (is (:full result))
          (is (empty? (cart-ids p)))
          (is (= {"iron_ingot" 3 "rail" 16 "bread" 2 "chest_minecart" 1} (inv p))))))))

(deftest a-cart-that-will-not-break-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory [{:name "iron_sword" :count 1}] :entities [(cart 50 [2 64 0] :invulnerable true)]})]
          (core/submit! eng (list job {:id 50}) {})
          (dotimes [_ 15] (await (core/tick! eng)))
          (is (= [:cannot-break] (mapv :reason (filter #(= :loot-cart.stopped (:kind %)) @seen))))
          (is (= #{50} (cart-ids p))))))))
