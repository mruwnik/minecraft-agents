(ns engine.loot-cart-test
  "jobs.strip.loot-cart against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.blocks-dig-test :as bd]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu :refer [child-outcome]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup
  ([world] (setup world (ew/of-data {} {} [])))
  ([world store]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        now (tu/act-clock clock p 300)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now :world store
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen :clock clock})))

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
          (is (= {:status :done :collected 22 :items {"iron_ingot" 3 "rail" 16 "bread" 2}} (select-keys result [:status :collected :items]))))))))

(deftest a-cart-by-near-takes-the-nearest-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(cart 50 [2 64 0]) (cart 51 [4 64 0] :items [{:name "dirt" :count 1}])]})
              _ (await (child-outcome eng job {:near [2 64 0] :radius 1} 12))]
          (is (= #{51} (cart-ids p)))
          (is (zero? (get (inv p) "dirt" 0))))))))

(deftest an-absent-cart-keeps-the-call-from-starting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {})
              waits (await (bd/waiting-after {:eng eng :clock clock} (list job {:id 77}) 3))]
          (is (= :gone (:reason waits)))
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

(deftest a-full-bag-that-gains-nothing-ends-bag-full-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(cart 50 [2 64 0] :drops [] :items [])]})
              _ (.override (.-world p) "inspectContainer" (fn [_ _ _] #js {:status "ok" :items #js [#js {:name "iron_ingot" :count 3}]}))
              _ (.override (.-world p) "transfer" (fn [_ _ _] #js {:status "full" :moved 0}))
              result (await (child-outcome eng job {:id 50} 12))]
          (is (= [:stopped :bag-full 0] ((juxt :status :reason :collected) result))))))))

(deftest a-bag-with-no-free-slot-leaves-the-cart-standing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(cart 50 [2 64 0])] :inventory (mapv (fn [i] {:name (str "filler_" i) :count 1}) (range 36))})
              _ (.override (.-world p) "transfer" (fn [_ _ _] #js {:status "full" :moved 0}))
              result (await (child-outcome eng job {:id 50} 12))]
          (is (= [:stopped :bag-full 0] ((juxt :status :reason :collected) result)))
          (is (= #{50} (cart-ids p)) "breaking it would spill the drops where the bag cannot take them"))))))

(deftest a-cart-that-will-not-break-ends-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "iron_sword" :count 1}] :entities [(cart 50 [2 64 0] :invulnerable true)]})
              result (await (child-outcome eng job {:id 50} 15))]
          (is (= {:kind :nothing-possible :cause :cannot-break} (select-keys (:ending result) [:kind :cause])))
          (is (= #{50} (cart-ids p))))))))

(deftest the-count-ignores-what-other-jobs-put-in-or-take-out-of-the-bag
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(cart 50 [2 64 0])] :inventory [{:name "iron_ingot" :count 10}]})
              result (await (child-outcome eng job {:id 50} 12))]
          (is (= {"iron_ingot" 13} (select-keys (inv p) ["iron_ingot"])))
          (is (= {"iron_ingot" 3 "rail" 16 "bread" 2} (:items result)) "the 10 ingots held at the start are not counted"))))))

(deftest a-cart-in-another-plans-footprint-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :metadata {:by "Other"} :parts [{:id "p" :box [[0 60 -5] [8 70 5]] :want "air"}]}
              {:keys [eng p]} (setup {:entities [(cart 50 [2.5 64 0.5])]} (ew/of-data {"hut" plan} {} []))
              result (await (child-outcome eng job {:id 50} 12))]
          (is (= [:stopped :refused ["hut"] 0] ((juxt :status :reason :plans :collected) result)))
          (is (= #{50} (cart-ids p))))))))
