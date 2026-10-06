(ns engine.dig-niche-test
  "jobs.survival.dig-niche's choice of site: a solid face with a shell all round, never flat ground, fluid or sand."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as ew]
            [jobs.survival.dig-niche :as niche]))

(defn blocks [name [x0 x1] [y0 y1] [z0 z1]]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(def ground (blocks "dirt" [-2 8] [60 63] [-3 3]))

(defn site
  ([extra] (site extra [{:name "iron_pickaxe" :count 1}]))
  ([extra inventory] (niche/find-site (tu/fake {:blocks (merge ground extra) :inventory inventory}) 16)))

(deftest a-hill-with-a-shell-gives-a-site-facing-it
  (is (= {:stand {:x 3 :y 64 :z 0} :dir [1 0]} (site (blocks "stone" [4 8] [64 66] [-3 3])))))

(deftest flat-ground-has-no-site
  (is (nil? (site {}))))

(deftest a-face-too-thin-or-low-for-the-shell-has-no-site
  (is (nil? (site (blocks "stone" [4 8] [64 65] [-3 3]))) "no ceiling over the niche")
  (is (nil? (site (blocks "stone" [4 8] [64 66] [0 0]))) "no side walls"))

(defn ok? [extra]
  (niche/niche-ok? (tu/fake {:blocks (merge ground (blocks "stone" [4 8] [64 66] [-3 3]) extra)
                             :inventory [{:name "iron_pickaxe" :count 1}]})
                   {:x 3 :y 64 :z 0} [1 0]))

(deftest sand-gravel-or-water-in-or-round-the-niche-rules-it-out
  (is (true? (ok? {})))
  (is (false? (ok? (blocks "sand" [4 8] [64 66] [-3 3]))) "sand falls")
  (is (false? (ok? {"5,66,0" "gravel"})) "gravel over the niche")
  (is (false? (ok? {"4,64,1" "water"})) "water beside it")
  (is (false? (ok? {"6,64,0" "water"})) "water behind it"))

(deftest stone-needs-a-pickaxe-the-body-carries
  (is (nil? (site (blocks "stone" [4 8] [64 66] [-3 3]) [])))
  (is (some? (site (blocks "dirt" [4 8] [64 66] [-3 3]) []))))

;; ------------------------------------------------------------------ the job: dig, plug, refusals

(def hill (blocks "stone" [4 8] [64 66] [-3 3]))

(defn setup
  ([zones] (setup zones {}))
  ([zones spec]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/seeing-all (tu/fake (merge {:offlineScale 0.0001 :time 14000 :blocks (merge ground hill)
                                           :drops {"stone" "cobblestone"}
                                           :inventory [{:name "iron_pickaxe" :count 1}]}
                                          spec)))
         eng (core/create {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                           :world (ew/of-data {} {} zones)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :seen seen})))

(defn ^:async run! [{:keys [eng]} args]
  (core/submit! eng (list 'jobs.survival.dig-niche args) {})
  (loop [i 0]
    (when (and (< i 300) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn failed [seen] (some #(when (= :dig_niche_failed (:kind %)) %) @seen))
(defn kinds-seen [seen] (set (map :kind @seen)))

(def hill-zone {:name "keep" :owner "Miles" :min [3 60 -3] :max [9 70 3]})

(deftest the-niche-is-dug-and-plugged-from-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [])]
          (await (run! s {}))
          (is (nil? (failed seen)))
          (is (contains? (kinds-seen seen) :dig-niche.sealed))
          (is (= ["air" "air"] (mapv #(block-at p %) [[5 64 0] [5 65 0]])) "inner cells dug")
          (is (= ["cobblestone" "cobblestone"] (mapv #(block-at p %) [[4 64 0] [4 65 0]])) "opening plugged with the dropped blocks"))))))

(deftest no-blocks-to-plug-with-fails-no-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {:drops {}})]
          (await (run! s {:blocks ["dirt"]}))
          (is (= :no-blocks (:reason (failed seen)))))))))

(deftest a-face-in-another-zone-is-skipped-and-another-face-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [hill-zone]
                                            {:blocks (merge ground hill (blocks "dirt" [-10 -3] [60 63] [-3 3]) (blocks "stone" [-8 -4] [64 66] [-3 3]))})]
          (await (run! s {}))
          (is (nil? (failed seen)))
          (is (= "stone" (block-at p [5 64 0])) "the owned wall is untouched")
          (is (= "air" (block-at p [-5 64 0])) "the niche is cut in the free hill"))))))

(deftest only-foreign-faces-fail-refused-and-dig-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [hill-zone])]
          (await (run! s {}))
          (is (= :refused (:reason (failed seen))))
          (is (= "stone" (block-at p [5 64 0]))))))))

(deftest ignore-zones-digs-in-a-foreign-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [hill-zone])]
          (await (run! s {:ignore-zones? true}))
          (is (nil? (failed seen)))
          (is (= "air" (block-at p [5 64 0]))))))))

(deftest permitted-asks-the-dug-and-door-cells
  (let [in {:zones [hill-zone] :self "fake" :now 0 :footprints {} :claims {} :block-at (constantly "stone")}]
    (is (false? (niche/permitted? in {:x 3 :y 64 :z 0} [1 0])))
    (is (true? (niche/permitted? (assoc in :ignore-zones? true) {:x 3 :y 64 :z 0} [1 0])))
    (is (true? (niche/permitted? (assoc in :zones []) {:x 3 :y 64 :z 0} [1 0])))))
