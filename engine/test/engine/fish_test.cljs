(ns engine.fish-test
  "jobs.food.fish against the fake world: a pond with a bank, casts that bite after some misses."
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [jobs.food.fish :as fish]))

(def pond (into {} (for [x (range 0 9) z (range 0 9) y [62 63]] [(str x "," y "," z) "water"])))
(def bank (into {} (for [z (range 0 9) y [62 63]] [(str "9," y "," z) "stone"])))
(def rod {:name "fishing_rod" :count 1})

(defn world [inventory fish]
  {:blocks (merge pond bank) :inventory inventory :fish fish :self {:pos [9.5 64 4.5]}})

(defn ^:async run [w args]
  (let [{:keys [eng p]} (lt/setup w)
        res (await (tu/child-outcome eng 'jobs.food.fish args 120))]
    {:res res :p p}))

(defn carried [p name] (reduce + (map #(.-count %) (filter #(= name (.-name %)) (array-seq (.-inventory (.self p)))))))

(deftest fishes-until-the-count-is-caught
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run (world [rod] {:catch ["cod" "salmon"] :misses 2}) {:count 3}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 3 (:got res)))
          (is (= 3 (+ (carried p "cod") (carried p "salmon")))))))))

(deftest a-body-already-carrying-the-count-is-done-without-casting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run (world [rod {:name "cod" :count 2} {:name "salmon" :count 1}] {:catch ["cod"] :misses 0}) {:count 3}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 3 (:got res)))
          (is (= 2 (carried p "cod"))))))))

(deftest no-rod-and-no-fetch-fails-its-condition-no-rod
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (world [] {}) {:count 1 :ways #{:carried}}))]
          (is (= [:condition-failed :no-rod] [(:kind (:ending res)) (:reason res)])))))))

(deftest casts-that-never-bite-end-no-bite-with-the-count-got
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (world [rod] {:misses 100}) {:count 2 :settings {:jobs.food.fish/max-misses 4}}))]
          (is (= :stopped (:status res)))
          (is (= :no-bite (:reason res)))
          (is (= 0 (:got res))))))))

(deftest no-water-in-sight-fails-its-condition-no-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks bank :inventory [rod]} {:count 1}))]
          (is (= [:condition-failed :no-water] [(:kind (:ending res)) (:reason res)])))))))

(deftest water-gone-mid-run-looks-once-then-waits-no-water
  (let [status (fn [mem] (with-redefs [ctx/mem (constantly mem)]
                           (fish/can-run? {:primitives (tu/fake-on-floor {:blocks bank :inventory [rod]}) :args {:count 3}})))
        kind-reason (fn [mem] ((juxt first (comp :reason last)) (status mem)))]
    (is (= [:look :no-water] (kind-reason {})) "not looked yet: the round looks")
    (is (= [:later :no-water] (kind-reason {:jobs.lib.look/looked [0 64 0]})) "looked, still no water: waits")))

(deftest some-fish-then-a-stop-is-done-when-the-least-was-caught
  (let [c (fn [least] {:primitives (tu/fake {:inventory [{:name "cod" :count 2}]}) :root "j1" :slots [] :args {:count 3}
                       :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data {:base 0}}]}}})
                       :quote-call (when least {:least least})})
        ending (fn [least] (select-keys (fish/stop! (c least) :nothing-possible :no-bite "no bite") [:kind :cause]))]
    (is (= {:kind :done :cause nil} (ending nil)) "the default least is 1")
    (is (= {:kind :done :cause nil} (ending 2)))
    (is (= {:kind :nothing-possible :cause :no-bite} (ending 3)))
    (is (= {:kind :nothing-possible :cause :no-bite} (ending :all)))))

(defn ^:async budget-run [misses]
  (let [{:keys [eng p clock]} (lt/setup (world [rod] {:catch ["cod"] :misses misses}))
        casts (atom 0)
        _ (.override (.-world p) "fish" (fn [token a impl] (when (< 1 (swap! casts inc)) (swap! clock + 50000)) (impl token a)))]
    (await (tu/child-outcome eng 'jobs.food.fish {:count 50 :settings {:jobs.food.fish/max-misses 1000 :jobs.food.fish/cast-s 10} :spend {:job-time 40}} 120))))

(deftest a-budget-end-with-fish-caught-is-done-at-the-least
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [res (await (budget-run 0))]
          (is (= :done (:status res)) (pr-str res))
          (is (pos? (:got res)) (pr-str res)))))))

(deftest a-budget-end-with-no-fish-is-limit-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [res (await (budget-run 1000))]
          (is (= :stopped (:status res)) (pr-str res))
          (is (= :job-time (:reason res)) (pr-str res))
          (is (= 0 (:got res)) (pr-str res)))))))


(deftest water-not-yet-seen-is-looked-for-before-no-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup (world [rod] {:catch ["cod"] :misses 0}))
              _ (tu/seeing-after-look p)
              res (await (tu/child-outcome eng 'jobs.food.fish {:count 1} 120))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (:got res))))))))

(deftest a-walk-that-ends-short-of-the-bank-is-not-an-arrival
  (is (fish/arrived? {:kind :done :data {:arrived true}}))
  (is (not (fish/arrived? {:kind :done :data {:arrived false :leg true}})))
  (is (not (fish/arrived? {:kind :stopped :data {:arrived true}}))))

(deftest only-a-bite-that-never-came-is-a-miss
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [mem (atom {})
              spot {:land {:x 9 :y 64 :z 4} :water {:x 8 :y 63 :z 4}}
              cast (fn ^:async cast-with [status]
                     (reset! mem {})
                     (with-redefs [ctx/act (fn [& _] (js/Promise.resolve #js {:status status}))
                                   ctx/update-mem! (fn [_ f & args] (apply swap! mem f args))]
                       (await (fish/cast! nil spot))
                       @mem))]
          (is (= 1 (:misses (await (cast "timeout")))))
          (is (= 1 (:misses (await (cast "nothing")))))
          (is (nil? (:misses (await (cast "lost")))) "a catch lost in the water is no miss")
          (is (nil? (:misses (await (cast "no-rod")))))
          (is (= [{:x 8 :y 63 :z 4}] (:bad-water (await (cast "unreachable")))) "the cell is not cast at again")
          (is (nil? (:misses (await (cast "unreachable"))))))))))
