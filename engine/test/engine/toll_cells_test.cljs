(ns engine.toll-cells-test
  "jobs.lib.toll-cells: the farm and zone cells a job's walks toll, and the farm jobs' walks bending round a planted strip."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.farm.harvest :as harvest]
            [jobs.lib.world-files :as ew]
            [jobs.lib.toll-cells :as tc]
            [jobs.lib.walk :as walk]))

(defn strip-blocks
  "A wheat strip on farmland across x 19..21, z from z0 to z1."
  [z0 z1]
  (into {} (mapcat (fn [[x z]] [[(str x ",63," z) "farmland"] [(str x ",64," z) "wheat"]]))
        (for [x (range 19 22) z (range z0 (inc z1))] [x z])))

(defn zone [owner] {:name "z" :min [19 60 -6] :max [21 70 6] :owner owner :allow #{}})

(defn setup [blocks zones & [extra]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (merge {:floor [-5 -20 45 20] :blocks blocks :self {:pos [0.5 64 0.5]}} extra)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async in-round
  "Run f (a fn of the ctx, may be async) as the round of a one-round job; its value."
  [{:keys [eng]} f]
  (let [out (atom nil)
        job {:check (constantly true)
             :round (fn ^:async f-round [c] (reset! out (await (f c))) :done)}
        eng (assoc eng :jobs (assoc (:jobs eng) 'f-job job))]
    (core/submit! eng '(f-job) {})
    (await (tu/tick-until-idle! eng 20))
    @out))

(deftest farm-cells-are-the-crop-cell-and-the-cell-over-farmland
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (setup (assoc (strip-blocks 0 0) "25,63,0" "farmland") [])
              tolls (await (in-round w #(tc/walk-tolls % {:x 40 :y 64 :z 0})))
              cells (set (map (juxt :x :y :z) tolls))]
          (is (contains? cells [20 64 0]) "wheat cell")
          (is (contains? cells [25 63 0]) "the farmland's own cell")
          (is (contains? cells [25 64 0]) "the cell above bare farmland"))))))

(deftest zone-cells-are-another-bodys-zone-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [foreign (await (in-round (setup {} [(zone "Other")]) #(tc/walk-tolls % {:x 40 :y 64 :z 0})))
              own (await (in-round (setup {} [(zone "Fake")]) #(tc/walk-tolls % {:x 40 :y 64 :z 0})))]
          (is (some #(= [20 64 0] [(:x %) (:y %) (:z %)]) foreign))
          (is (nil? own)))))))

(defn ^:async walk-trail
  "harvest/walk! from the origin to [40 64 0] over blocks; the [x z] cells the body steered through."
  [blocks]
  (let [{:keys [p] :as w} (setup blocks [])
        trail (atom [])]
    (.override (.-world p) "steer"
               (fn [token args impl]
                 (let [decide (.-decide args)
                       logged (fn [pose] (swap! trail conj [(js/Math.floor (.-x pose)) (js/Math.floor (.-z pose))]) (decide pose))]
                   (impl token (walk/steer-args (.-timeoutS args) logged)))))
    (await (in-round w (fn ^:async walking [c]
                         (loop [i 0] (let [r (await (harvest/walk! c {:x 40 :y 64 :z 0} 1))]  (when (and (< i 6) (not= :there r)) (recur (inc i))))))))
    @trail))

(defn on-strip?
  "Whether the trail has a cell over the strip's rows z0..z1 (the body's corners graze the edge rows, so a detour is judged on the inner ones)."
  [trail z0 z1]
  (boolean (some (fn [[x z]] (and (<= 19 x 21) (<= z0 z z1))) trail)))

(deftest a-farm-walk-bends-round-a-planted-strip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trail (await (walk-trail (strip-blocks -6 6)))]
          (is (seq trail))
          (is (not (on-strip? trail -5 5))))))))

(deftest a-farm-walk-crosses-the-strip-when-it-is-the-only-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (mapcat (fn [z] [[(str "20,64," z) "stone"] [(str "20,65," z) "stone"] [(str "20,66," z) "stone"]]))
                         (concat (range -20 -6) (range 7 21)))
              trail (await (walk-trail (merge wall (strip-blocks -6 6))))]
          (is (seq trail))
          (is (on-strip? trail -6 6)))))))

(defn big-zone [owner] {:name "big" :min [10 60 -50] :max [70 70 50] :owner owner :allow #{}})

(deftest a-zone-over-the-cap-keeps-the-cells-nearest-the-walk-and-warns-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (setup {} [(big-zone "Other")])
              tolls (await (in-round w (fn ^:async f [c] (await (tc/walk-tolls c {:x 40 :y 64 :z 40}))
                                         (await (tc/walk-tolls c {:x 40 :y 64 :z 40})))))
              cells (set (map (juxt :x :y :z) tolls))
              warns (filter #(= "toll-cells.capped" (name (:kind %))) @(:seen w))]
          (is (<= (count tolls) tc/max-cells))
          (is (contains? cells [38 64 38]) "the goal end of the line is tolled, not just the low x")
          (is (contains? cells [12 64 12]) "the start end too")
          (is (= 1 (count warns))))))))

(defn ^:async job-trail
  "The [x z] cells the body steered through running form with the fake world's extra keys, a foreign zone across x 19..21, z -6..6."
  [form extra]
  (let [{:keys [p eng]} (setup {} [(zone "Other")] extra)
        trail (atom [])]
    (.override (.-world p) "steer"
               (fn [token a impl]
                 (let [decide (.-decide a)
                       logged (fn [pose] (swap! trail conj [(js/Math.floor (.-x pose)) (js/Math.floor (.-z pose))]) (decide pose))]
                   (impl token (walk/steer-args (.-timeoutS a) logged)))))
    (core/submit! eng form {})
    (await (tu/tick-until-idle! eng 60))
    @trail))

(defn ^:async go-to-trail
  "job-trail of a go-to to [40 64 0] with args."
  [args]
  (await (job-trail (list 'jobs.movement.go-to (merge {:pos [40 64 0] :range 1 :escalate false} args)) {})))

(defn ^:async kit-trail
  "job-trail of a kit run on a chest at 40 64 0 with args."
  [args]
  (await (job-trail (list 'jobs.storage.kit (merge {:chest {:x 40 :y 64 :z 0} :tools [] :spare 0 :food 2 :craft false} args))
                    {:containers {"40,64,0" [{:name "bread" :count 8}]}})))

(deftest kit-bends-round-another-bodys-zone-unless-ignoring-zones
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tolled (await (kit-trail {}))
              ignoring (await (kit-trail {:ignore-zones? true}))]
          (is (seq tolled))
          (is (not (on-strip? tolled -5 5)))
          (is (on-strip? ignoring -5 5)))))))

(deftest go-to-zone-tolls-bends-round-another-bodys-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plain (await (go-to-trail {}))
              tolled (await (go-to-trail {:zone-tolls true}))
              ignoring (await (go-to-trail {:zone-tolls true :ignore-zones? true}))]
          (is (on-strip? plain -5 5) "control: without :zone-tolls the straight way crosses the zone")
          (is (seq tolled))
          (is (not (on-strip? tolled -5 5)))
          (is (on-strip? ignoring -5 5) ":ignore-zones? lifts the tolls"))))))

(deftest craft-bends-round-another-bodys-zone-on-its-walk-to-the-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trail (await (job-trail '(jobs.items.craft {:item "bread" :radius 60})
                                      {:inventory [{:name "wheat" :count 3}] :blocks {"40,64,0" "crafting_table"}}))]
          (is (seq trail))
          (is (not (on-strip? trail -5 5))))))))
