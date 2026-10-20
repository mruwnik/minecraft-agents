(ns engine.fetch-default-test
  "A job that starts without the item it needs gets it from a seen chest by default, and leaves the chest alone with fetch off (:fetch false, or :ways #{:carried})."
  (:require [cljs.test :refer [deftest is async]]
            [engine.library-test :as lt]
            [engine.test-util :as tu]))

(def chest "-2,64,3")
(def ground (into {} (for [x (range 0 9) z (range -2 3) y [62 63]] [(str x "," y "," z) "stone"])))

(defn carried [p name]
  (reduce + (map #(.-count %) (filter #(= name (.-name %)) (array-seq (.-inventory (.self p)))))))

(def cases
  [{:job 'jobs.forestry.plant-sapling
    :ways? true
    :args {:at {:x 3 :y 64 :z 0} :species "oak"}
    :world {:blocks {chest "chest" "3,63,0" "dirt"} :containers {chest [{:name "oak_sapling" :count 1}]}}
    :item "oak_sapling"
    :used #(= "oak_sapling" (some-> (.blockAt % #js {:x 3 :y 64 :z 0}) .-name))}
   {:job 'jobs.gather.cast-obsidian
    :args {:count 1}
    :world {:blocks (merge ground {"4,63,0" "lava" "0,63,-2" "water" chest "chest"})
            :containers {chest [{:name "bucket" :count 1}]}
            :inventory [{:name "diamond_pickaxe" :count 1}]
            :drops {:obsidian "obsidian"}}
    :item "bucket"
    :used #(pos? (carried % "obsidian"))}])

(defn ^:async run-case [{:keys [job args world ways?]} fetch]
  (let [{:keys [eng p]} (lt/setup (merge {:self {:pos [0.5 64 0.5]}} world))
        call (cond (not ways?) (assoc args :fetch fetch) fetch args :else (assoc args :ways #{:carried}))
        res (await (tu/child-outcome eng job call 200))]
    {:res res :p p}))

(deftest a-job-without-its-item-fetches-it-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [{:keys [item used] :as c} cases]
          (let [{:keys [p res]} (await (run-case c true))]
            (is (used p) (str item " was fetched and used " (pr-str res)))))))))

(deftest a-job-without-its-item-leaves-the-chest-alone-with-fetch-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [{:keys [item used] :as c} cases]
          (let [{:keys [p]} (await (run-case c false))]
            (is (not (used p)) (str item " was not fetched"))
            (is (zero? (carried p item)) "nothing came out of the chest")))))))
