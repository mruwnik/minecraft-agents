(ns engine.obtain-obsidian-test
  "Obsidian as a gathered need of items.obtain: mined where seen (diamond pickaxe), else made by pouring water on a
  seen lava source from a side stand, mined, and the bucket scooped back; else a refusal naming what is missing."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.library-test :as lt]
            [engine.test-util :as tu]))

(def pick {:name "diamond_pickaxe" :count 1})
(def ground (into {} (for [x (range 0 9) z (range -2 3) y [62 63]] [(str x "," y "," z) "stone"])))
(def lava-lake (merge ground {"4,63,0" "lava"}))

(defn world [blocks inventory]
  {:blocks blocks :inventory inventory :drops {:obsidian "obsidian"} :self {:pos [0.5 64 0.5]}})

(defn ^:async obtain [w]
  (let [{:keys [eng p]} (lt/setup w)
        res (await (tu/child-outcome eng 'jobs.items.obtain {:item "obsidian" :count 1} 300))]
    {:res res :p p}))

(defn carried [p name] (reduce + (map #(.-count %) (filter #(= name (.-name %)) (array-seq (.-inventory (.self p)))))))

(deftest seen-obsidian-is-mined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (obtain (world (assoc ground "4,63,0" "obsidian") [pick])))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (carried p "obsidian"))))))))

(deftest water-on-a-seen-lava-source-makes-obsidian-and-the-bucket-is-scooped-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (obtain (world lava-lake [pick {:name "water_bucket" :count 1}])))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (carried p "obsidian")))
          (is (= 1 (carried p "water_bucket")) "the bucket is full again"))))))

(defn ^:async waiting [w]
  (let [{:keys [eng]} (lt/setup w)
        id (core/submit! eng '(jobs.items.obtain {:item "obsidian" :count 1 :explore false}) {})]
    (await (tu/run-until-empty eng 30))
    (core/waiting eng id)))

(deftest nothing-seen-or-no-bucket-says-what-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [none (await (waiting (world ground [pick])))
              no-bucket (:res (await (obtain (world lava-lake [pick]))))]
          (is (= :no-source (:reason none)))
          (is (re-find #"obsidian" (:why none)))
          (is (= :no-source (:reason no-bucket)))
          (is (re-find #"no-bucket" (:why no-bucket)) (pr-str no-bucket)))))))
