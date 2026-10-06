(ns engine.sensing-farm-test
  "The farm and food scans read what the body has seen (block memory), not the world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.farm.find-spot :as find-spot]
            [jobs.farm.harvest :as harvest]
            [jobs.farm.tend :as tend]))

(def field {:blocks {"1,64,0" "wheat" "2,64,0" "wheat"} :ages {"1,64,0" 7 "2,64,0" 3}})
(def origin {:x 0 :y 64 :z 0})

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(swap! clock + 700)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

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

(deftest ripe-crops-are-those-seen
  (let [args {:radius 10}]
    (is (= [1] (mapv :x (harvest/ripe-crops (tu/seeing-all (tu/fake field)) args origin []))))
    (is (empty? (harvest/ripe-crops (tu/blind (tu/fake field)) args origin [])) "unseen crops are not cut")))

(deftest tend-counts-only-the-unripe-crops-seen
  (let [box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 2}}]
    (is (= 1 (count (tend/unripe-in-box (tu/seeing-all (tu/fake field)) box origin 6))))
    (is (empty? (tend/unripe-in-box (tu/blind (tu/fake field)) box origin 6)))))

(deftest water-counts-for-a-spot-only-when-seen
  (let [world {:blocks (assoc (into {} (for [x (range 0 4) z (range 0 4)] [(str x ",63," z) "dirt"])) "4,63,0" "water")}
        a {:range 4 :w 2 :h 2 :depth 6 :limit 1}
        share (fn [p] (:water-share (first (find-spot/scan p a origin))))]
    (is (pos? (share (tu/seeing-all (tu/fake world)))))
    (is (zero? (or (share (tu/blind (tu/fake world))) 0)) "water not seen: no water share")))

(deftest fertilize-leaves-crops-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc field :inventory [{:name "bone_meal" :count 9}]))]
          (tu/blind p)
          (await (child-outcome eng 'jobs.farm.fertilize {} 8))
          (is (empty? (calls p "useOn"))))))))

(deftest compost-does-not-use-a-composter-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "oak_leaves" :count 4}] :blocks {"2,63,0" "composter"}})]
          (tu/blind p)
          (let [r (await (child-outcome eng 'jobs.farm.compost {} 10))]
            (is (= :no-composter (:reason r)))
            (is (empty? (calls p "useOn")))))))))

(defn ^:async get-food-digs
  "The dig calls of one get-food run at a known farm over field, with primitives p made by (view p)."
  [view]
  (let [{:keys [eng p]} (setup {:blocks {"10,64,0" "carrots" "10,63,0" "farmland"} :ages {"10,64,0" 7}
                                :drops {:carrots "carrot"} :self {:food 0}})]
    (view p)
    (mem/write! (:store eng) :food-source {:pos {:x 11 :y 64 :z 0} :kind :farm} {:cap 5 :ttl :forever})
    (core/submit! eng '(jobs.survival.get-food {}) {})
    (await (core/tick! eng))
    (calls p "dig")))

(deftest get-food-digs-the-ripe-crop-it-has-seen
  (async done
    (tu/run-async done
      (fn ^:async t [] (is (seq (await (get-food-digs tu/seeing-all))))))))

(deftest get-food-does-not-dig-crops-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t [] (is (empty? (await (get-food-digs tu/blind))))))))

(deftest fertilize-looks-around-once-then-works-what-it-sees
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc field :inventory [{:name "bone_meal" :count 9}]))]
          (tu/blind p)
          (aset p "seenBlocks" (fn [q] (if (seq (calls p "look")) (.blocks p q) #js [])))
          (aset p "seenBlockAt" (fn [pos] (let [b (.blockAt p pos)] #js {:name (.-name b) :properties (.-properties b) :pos pos :age-ms 0})))
          (await (child-outcome eng 'jobs.farm.fertilize {} 12))
          (is (seq (calls p "useOn"))))))))
