(ns engine.apiary-maintain-test
  "jobs.apiary.maintain against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.test-util :as tu]))

(defn inv [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))

(defn around
  "The four horizontal neighbours of x,y,z as stone cells."
  [x y z]
  (into {} (map (fn [[dx dz]] [(str (+ x dx) "," y "," (+ z dz)) "stone"]) [[1 0] [-1 0] [0 1] [0 -1]])))

(def chest {:x 8 :y 64 :z 0})

(defn world
  "A lit campfire at 2,64,0 walled on four sides over walled ground, a hive at 2,66,0 (ripe by default); :raised? opens one side of the fire."
  [{:keys [inventory entities level raised? chest? time raining] :or {level 5}}]
  (cond-> {:inventory inventory
           :entities entities
           :time (or time 1000)
           :raining (boolean raining)
           :self {:pos {:x 0 :y 64 :z 0}}
           :blocks (cond-> (merge (around 2 64 0) (around 2 63 0) {"2,64,0" "campfire" "2,63,0" "stone" "2,66,0" "beehive"})
                     raised? (dissoc "3,64,0"))
           :states {"2,64,0" {:lit true} "2,66,0" {:honey_level level}}}
    chest? (assoc :containers {"8,64,0" []})))

(defn bee
  ([id x] (bee id x {}))
  ([id x more] (merge {:id id :uuid (str "b" id) :name "bee" :kind "passive" :pos {:x x :y 64 :z 2}} more)))

(def box {:from {:x -2 :y 60 :z -6} :to {:x 6 :y 70 :z 6}})

(defn spec [args] (list 'jobs.apiary.maintain (merge {:box box} args)))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  ([args w n] (scenario args w n identity))
  ([args w n tweak-jobs]
   (let [s (doto (h/setup w) (-> :p tu/seeing-all))
         s (update s :eng #(update % :jobs tweak-jobs))]
     (core/submit! (:eng s) (spec args) {})
     (dotimes [_ n]
       (swap! (:clock s) + 700)
       (await (core/tick! (:eng s))))
     s)))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :maintain.done)))
(defn step [s k] (get-in (done-event s) [:steps k]))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls [{:keys [p]} name] (h/calls p name))
(defn carried [{:keys [p]} name]
  (->> (array-seq (.-inventory (.self p))) (filter #(= name (.-name %))) (map #(.-count %)) (reduce + 0)))
(defn chest-items [{:keys [p]}]
  (into {} (map (juxt :name :count))
        (get-in @(fake/state p) [:containers [8 64 0]])))
(defn block-name [{:keys [p]} x y z] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn honey [{:keys [p]}] (.-honey_level (.-properties (.blockAt p #js {:x 2 :y 66 :z 0}))))

(def kit (inv "shears" 1 "white_carpet" 2))

(deftest the-fire-is-guarded-before-the-hive-is-harvested
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world {:inventory kit}) 60))
              names (mapv #(.-name %) (.-calls (.-world (:p s))))
              first-of (fn [n] (.indexOf names n))]
          (is (= "white_carpet" (block-name s 2 65 0)))
          (is (= 0 (honey s)))
          (is (< (first-of "place") (first-of "useOn")) "carpet down before the shears are used")
          (is (= 1 (:carpeted (step s :guard))))
          (is (= 1 (:harvested (step s :harvest))))
          (is (true? (finished? s))))))))

(deftest an-unsafe-fire-keeps-the-shears-off-its-hive-and-the-pass-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world {:inventory kit :raised? true}) 40))]
          (is (= 1 (:carpeted (step s :guard))))
          (is (= :no-campfire (:reason (step s :guard))))
          (is (= {:skipped :unsafe-fire} (step s :harvest)))
          (is (empty? (calls s "useOn")))
          (is (= 5 (honey s)))
          (is (true? (finished? s))))))))

(deftest nothing-that-can-be-done-declines-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world {:inventory (inv "shears" 1) :raised? true}) 10))]
          (is (nil? (done-event s)))
          (is (empty? (calls s "moveTo")))
          (is (empty? (calls s "useOn"))))))))

(deftest produce-stays-carried-without-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world {:inventory kit}) 60))]
          (is (= 3 (carried s "honeycomb")))
          (is (= {:skipped :no-chest} (step s :deposit)))
          (is (true? (finished? s))))))))

(deftest produce-goes-into-the-chest-and-the-gear-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest} (world {:inventory kit :chest? true}) 100))]
          (is (= {"honeycomb" 3} (chest-items s)))
          (is (= 1 (carried s "shears")))
          (is (= 1 (carried s "white_carpet")))
          (is (= 0 (carried s "honeycomb")))
          (is (true? (finished? s))))))))

(deftest with-a-chest-but-no-produce-nothing-is-deposited-and-the-chest-is-not-visited
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest} (world {:inventory kit :level 2 :chest? true}) 40))]
          (is (= {:skipped :nothing-to-store} (step s :deposit)))
          (is (empty? (calls s "transfer")))
          (is (empty? (calls s "inspectContainer")))
          (is (true? (finished? s))))))))

(deftest a-safe-apiary-with-nothing-ripe-declines-and-nothing-moves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [carpeted (-> (world {:inventory kit :level 2}) (update :blocks assoc "2,65,0" "white_carpet"))
              s (await (scenario {} carpeted 10))]
          (is (nil? (done-event s)))
          (is (empty? (calls s "moveTo")))
          (is (empty? (calls s "place")))
          (is (empty? (calls s "useOn"))))))))

(def flowers (inv "dandelion" 4))
(def bees [(bee 1 3) (bee 2 4)])
(def carpet-and-flowers (into (inv "white_carpet" 1) flowers))

(defn quiet-world [w] (update w :blocks assoc "2,65,0" "white_carpet"))

(deftest bees-below-target-in-daylight-with-flowers-are-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} (quiet-world (world {:inventory flowers :level 2 :entities bees})) 30))]
          (is (= ["dandelion" "dandelion"] (mapv #(.-item (.-args %)) (calls s "interact"))))
          (is (= 2 (:fed (step s :breed))))
          (is (true? (finished? s))))))))

(deftest breeding-is-skipped-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label args w reason]
                [["no target" {} {:inventory carpet-and-flowers :entities bees} :no-target]
                 ["at target" {:target 2} {:inventory carpet-and-flowers :entities bees} :at-target]
                 ["night" {:target 4} {:inventory carpet-and-flowers :entities bees :time 15000} :night]
                 ["rain" {:target 4} {:inventory carpet-and-flowers :entities bees :raining true} :raining]
                 ["no flowers" {:target 4} {:inventory (inv "white_carpet" 1) :entities bees} :no-food]
                 ["one bee" {:target 4} {:inventory carpet-and-flowers :entities [(bee 1 3)]} :too-few-adults]]]
          (let [s (await (scenario args (world (assoc w :level 2)) 12))]
            (is (empty? (calls s "interact")) label)
            (is (= {:skipped reason} (step s :breed)) label)))))))

(deftest babies-count-toward-the-bee-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (world {:inventory carpet-and-flowers :level 2 :entities (conj bees (bee 3 4 {:baby true}))})
              s (await (scenario {:target 3} w 12))]
          (is (empty? (calls s "interact")))
          (is (= {:skipped :at-target} (step s :breed))))))))

(deftest a-step-without-its-tool-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world {:inventory (inv "white_carpet" 1)}) 30))]
          (is (= {:skipped :no-tool} (step s :harvest)))
          (is (= 5 (honey s)))
          (is (true? (finished? s))))))))

(deftest a-failing-child-is-booked-and-the-pass-goes-on-and-runs-it-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [runs (atom 0)
              boom (fn [jobs] (assoc jobs 'jobs.apiary.harvest
                                     {:check (constantly true)
                                      :round (fn [_c] (swap! runs inc) (throw (js/Error. "boom")))}))
              s (await (scenario {:chest chest} (world {:inventory (into kit (inv "honey_bottle" 2)) :chest? true}) 60 boom))]
          (is (= 1 @runs))
          (is (= :failed (:skipped (step s :harvest))))
          (is (= {"honey_bottle" 2} (chest-items s)))
          (is (true? (finished? s))))))))
