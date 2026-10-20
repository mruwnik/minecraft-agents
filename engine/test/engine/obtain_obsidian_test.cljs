(ns engine.obtain-obsidian-test
  "Obsidian as a gathered need of items.obtain: mined where seen (diamond pickaxe), else made by pouring water on a
  seen lava source from a side stand, mined, and the bucket scooped back; else a refusal naming what is missing."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.library-test :as lt]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.sense-test :as sense]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

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

(deftest a-mine-child-that-dug-the-obsidian-and-still-collects-is-kept-not-replaced-by-a-pour
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup (world (assoc lava-lake "1,63,0" "obsidian") [pick {:name "water_bucket" :count 1}]))
              calls (atom 0)
              stub {:round (fn ^:async mine-round [c]
                             (if (= 1 (swap! calls inc))
                               (do (await (ctx/act c :dig (clj->js {:pos {:x 1 :y 63 :z 0}}))) :continue)
                               (ctx/end c :done nil)))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.gather.mine stub))
              res (await (tu/child-outcome eng 'jobs.gather.make-obsidian {:count 1} 100))]
          (is (= 2 @calls) "the mine child ran on after the block was dug")
          (is (= :mine-failed (:reason res)) (pr-str res))
          (is (= 1 (carried p "water_bucket")) "no water was poured"))))))

(defn ^:async refused
  "{:reason :why} of an obsidian obtain that ends before it gets anywhere."
  [w]
  (let [e (await (ending-of (lt/setup w) 'jobs.items.obtain {:item "obsidian" :count 1 :explore false} 30))]
    {:reason (contract/cause-key e) :why (get-in e [:cause :why])}))

(deftest nothing-seen-or-no-bucket-says-what-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [none (await (refused (world ground [pick])))
              no-bucket (await (refused (world lava-lake [pick])))]
          (is (= :no-source (:reason none)))
          (is (re-find #"obsidian" (:why none)))
          (is (= :no-bucket (:reason no-bucket)) "make-obsidian's start condition is the wait reason"))))))

(defn ^:async no-zone-list-outcome
  "The outcome of make-obsidian with args on a body whose zone list was never read."
  [args]
  (let [[_ sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (world (assoc ground "4,63,0" "obsidian") [pick])))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now (constantly 1000000)
                          :world (ew/of-data {} {} nil)
                          :events (events/make {:body "Fake" :sinks [sink] :now (constantly 1000000)})})]
    (await (tu/child-outcome eng 'jobs.gather.make-obsidian args 300))))

(deftest ignore-zones-reaches-the-mine-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [res (await (no-zone-list-outcome {:count 1 :ignore-zones? true}))]
          (is (= :done (:status res)) (pr-str res)))))))

(def far-ground (into {} (for [x (range 0 71) z (range -2 3) y [62 63]] [(str x "," y "," z) "stone"])))
(def breach {:dim "overworld" :at {:x 57 :y 63 :z 0} :names ["air" "lava"] :cells [[60 63 0] [62 63 0]]})

(def sealed-breach
  {:dim "overworld" :at [62 63 1] :names ["air" "lava"] :cells [[62 63 0]] :lava [[62 63 0]] :seals [[62 63 1]]})

(defn ^:async obtain-remembering
  "make-obsidian with a water bucket, on w, with a :breach-behind entry in memory."
  ([w] (obtain-remembering w breach))
  ([w entry]
  (let [{:keys [eng p]} (lt/setup w)]
    (mem/write! (:store eng) :breach-behind entry)
    {:res (await (tu/child-outcome eng 'jobs.gather.make-obsidian {:count 1 :radius 24} 600))
     :p p
     :left (mem/entries (mem/view (:store eng)) :breach-behind)})))

(deftest lava-beyond-sight-is-walked-to-from-a-breach-memory-and-made-obsidian
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p left]} (await (obtain-remembering (world (assoc far-ground "62,63,0" "lava") [pick {:name "water_bucket" :count 1}])))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (carried p "obsidian")))
          (is (empty? left) "the entry is forgotten once looked at"))))))

(deftest the-seal-over-remembered-lava-is-dug-so-the-lava-is-seen-and-made-obsidian
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p left]} (await (obtain-remembering (world (assoc far-ground "62,63,0" "lava") [pick {:name "water_bucket" :count 1}]) sealed-breach))]
          (is (= :done (:status res)) (pr-str res))
          (is (= "air" (.-name (.blockAt p #js {:x 62 :y 63 :z 1}))) "the seal the mine placed is dug")
          (is (= 1 (carried p "obsidian")))
          (is (empty? left)))))))

(deftest no-lava-seen-looks-round-before-it-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup (world ground [pick {:name "water_bucket" :count 1}]))]
          (core/submit! eng '(jobs.gather.make-obsidian {:count 1}) {})
          (await (tu/run-until-empty eng 6))
          (is (pos? (sense/looks p))))))))

(deftest remembered-lava-that-is-gone-is-forgotten-and-ends-no-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res left]} (await (obtain-remembering (world far-ground [pick {:name "water_bucket" :count 1}])))]
          (is (not= :done (:status res)) (pr-str res))
          (is (empty? left)))))))

(deftest water-is-never-poured-into-another-bodys-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake-on-floor (world lava-lake [pick {:name "water_bucket" :count 1}])))
              zone {:name "other" :min [0 60 -2] :max [8 70 2] :owner "Miles"}
              [_ sink] (tu/legacy-capture-sink)
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now (constantly 1000000)
                                :world (ew/of-data {} {} [zone])
                                :events (events/make {:body "Fake" :sinks [sink] :now (constantly 1000000)})})
              res (await (tu/child-outcome eng 'jobs.gather.make-obsidian {:count 1} 100))]
          (is (= :no-lava (:reason res)) (pr-str res))
          (is (= 1 (carried p "water_bucket")) "no water was poured"))))))

(deftest a-scoop-that-waits-is-resumed-not-dropped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup (world lava-lake [pick {:name "water_bucket" :count 1}]))
              real (get registry/jobs 'jobs.blocks.use-on)
              waited (atom false)
              wrapped (assoc real :round (fn ^:async r [c]
                                           (if (and (= "bucket" (:item (:args c))) (not @waited))
                                             (do (reset! waited true) :continue)
                                             (await ((:round real) c)))))
              eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.blocks.use-on wrapped))
              res (await (tu/child-outcome eng 'jobs.gather.make-obsidian {:count 1} 300))]
          (is @waited "the scoop child waited once")
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (carried p "water_bucket")) "the water was scooped back after the wait"))))))

(deftest the-mine-child-starts-only-after-the-body-is-out-of-the-scooped-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup (world (assoc ground "4,63,0" "obsidian") [pick]))
              in-water-at-start (atom nil)
              stub {:round (fn ^:async mine-round [c]
                             (reset! in-water-at-start (.-inWater (.self p)))
                             (ctx/end c :done nil))}
              pauses (atom 0)
              noop {:round (fn ^:async noop-round [c]
                             (when (= 2 (swap! pauses inc)) (fake/swap-self! p assoc :inWater false))
                             (ctx/end c :done nil))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.gather.mine stub 'jobs.time.noop noop))]
          (fake/swap-self! p assoc :inWater true)
          (await (tu/child-outcome eng 'jobs.gather.make-obsidian {:count 1} 100))
          (is (false? @in-water-at-start) "the mine child waited for the flow to drain"))))))
