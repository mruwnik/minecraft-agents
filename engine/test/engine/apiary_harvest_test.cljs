(ns engine.apiary-harvest-test
  "jobs.apiary.harvest against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as ew]
            [jobs.apiary.harvest :as harvest]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} (:zones world []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

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
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn carried [p name]
  (->> (array-seq (.-inventory (.self p))) (filter #(= name (.-name %))) (map #(.-count %)) (reduce + 0)))
(defn inv [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))

(def job 'jobs.apiary.harvest)
(def hive "2,64,0")
(def under "2,63,0")

(defn world
  "Hive at 2,64,0, ripe by default; a lit campfire directly under it unless smoke? is false."
  [{:keys [inventory level smoke? name] :or {level 5 smoke? true name "beehive"}}]
  {:inventory inventory
   :blocks (cond-> {hive name} smoke? (assoc under "campfire"))
   :states (cond-> {hive {:honey_level level}} smoke? (assoc under {:lit true}))})

(deftest shears-take-the-honey-and-the-comb-is-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (world {:inventory (inv "shears" 1)}))
              result (await (child-outcome eng job {} 40))]
          (is (= 1 (:harvested result)))
          (is (= :harvested (:reason result)))
          (is (= "shears" (:with result)))
          (is (= 3 (carried p "honeycomb")))
          (is (= 1 (count (calls p "useOn"))))
          (is (= 0 (.-honey_level (.-properties (.blockAt p #js {:x 2 :y 64 :z 0})))))
          (is (= 1 (count (kinds seen :apiary.done)))))))))

(deftest a-glass-bottle-takes-the-honey-into-a-honey-bottle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world {:inventory (inv "glass_bottle" 2) :name "bee_nest"}))
              result (await (child-outcome eng job {:with :bottle} 40))]
          (is (= 1 (:harvested result)))
          (is (= 1 (carried p "honey_bottle")))
          (is (= 1 (carried p "glass_bottle")))
          (is (= 0 (carried p "honeycomb"))))))))

(deftest either-prefers-shears-and-falls-back-to-a-bottle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [both (setup (world {:inventory (inv "shears" 1 "glass_bottle" 1)}))
              r1 (await (child-outcome (:eng both) job {} 40))
              bottle (setup (world {:inventory (inv "glass_bottle" 1)}))
              r2 (await (child-outcome (:eng bottle) job {} 40))]
          (is (= "shears" (:with r1)))
          (is (= 1 (carried (:p both) "glass_bottle")))
          (is (= "glass_bottle" (:with r2)))
          (is (= 1 (carried (:p bottle) "honey_bottle"))))))))

(deftest a-ripe-hive-without-smoke-is-declined-without-a-click
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (world {:inventory (inv "shears" 1) :smoke? false}))
              result (await (child-outcome eng job {} 40))]
          (is (= 0 (:harvested result)))
          (is (= :not-smoked (:reason result)))
          (is (= {"2,64,0" :not-smoked} (:declined result)))
          (is (empty? (calls p "useOn")))
          (is (= 1 (count (kinds seen :apiary.gave-up)))))))))

(deftest a-ripe-hive-over-an-open-fire-is-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (world {:inventory (inv "shears" 1)})
                    (update :blocks #(-> % (dissoc under) (assoc "2,62,0" "campfire")))
                    (update :states #(-> % (dissoc under) (assoc "2,62,0" {:lit true}))))
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {} 40))]
          (is (= :open-fire (:reason result)))
          (is (empty? (calls p "useOn"))))))))

(deftest an-unripe-hive-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world {:inventory (inv "shears" 1) :level 4}))
              result (await (child-outcome eng job {} 40))]
          (is (= :not-ripe (:reason result)))
          (is (= 0 (:harvested result)))
          (is (empty? (calls p "useOn"))))))))

(deftest no-tool-is-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world {:inventory (inv "stick" 1)}))
              result (await (child-outcome eng job {} 40))]
          (is (= :no-tool (:reason result)))
          (is (= 0 (:harvested result)))
          (is (empty? (calls p "useOn"))))))))

(deftest the-wanted-tool-alone-counts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (world {:inventory (inv "shears" 1)}))
              result (await (child-outcome eng job {:with :bottle} 40))]
          (is (= :no-tool (:reason result))))))))

(deftest no-hive-in-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory (inv "shears" 1)})
              result (await (child-outcome eng job {} 40))]
          (is (= :no-hive (:reason result))))))))

(deftest a-hive-the-body-cannot-walk-to-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (world {:inventory (inv "shears" 1)}) :self {:pos {:x 20 :y 64 :z 0}} :unreachable [hive])
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {:radius 30} 40))]
          (is (= :unreachable (:reason result)))
          (is (= {"2,64,0" :unreachable} (:skipped result)))
          (is (empty? (calls p "useOn"))))))))

(deftest the-count-is-bounded-by-max
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (world {:inventory (inv "shears" 5)})
                    (update :blocks assoc "2,64,3" "beehive" "2,63,3" "campfire")
                    (update :states assoc "2,64,3" {:honey_level 5} "2,63,3" {:lit true}))
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {:max 1} 60))]
          (is (= 1 (:harvested result)))
          (is (= :limit (:reason result)))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest every-ripe-smoked-hive-is-harvested
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (world {:inventory (inv "shears" 1)})
                    (update :blocks assoc "2,64,3" "beehive" "2,63,3" "campfire")
                    (update :states assoc "2,64,3" {:honey_level 5} "2,63,3" {:lit true}))
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {} 60))]
          (is (= 2 (:harvested result)))
          (is (= :harvested (:reason result)))
          (is (= 6 (carried p "honeycomb"))))))))

(deftest a-box-keeps-the-job-to-its-hives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world {:inventory (inv "shears" 1)}))
              result (await (child-outcome eng job {:box {:from {:x 10 :y 60 :z -5} :to {:x 14 :y 70 :z 5}}} 40))]
          (is (= :no-hive (:reason result)))
          (is (empty? (calls p "useOn"))))))))

;; ------------------------------------------------------------------ zones and claims

(def zone-over-hive {:name "bees" :min [2 60 0] :max [2 70 0]})

(deftest a-hive-follows-its-zone-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra harvested reason declined] [["Fake" {} 1 :harvested []] ["FAKE" {} 1 :harvested []]
                                                         ["Miles" {} 0 :no-hive [{:reason :refused :zones ["bees"]}]]
                                                         ["Miles" {:ignore-zones? true} 1 :harvested []]]]
          (let [{:keys [eng p seen]} (setup (assoc (world {:inventory (inv "shears" 1)}) :zones [(assoc zone-over-hive :owner owner)]))
                result (await (child-outcome eng job extra 40))]
            (is (= [harvested reason] [(:harvested result) (:reason result)]) (pr-str [owner extra]))
            (is (= harvested (count (calls p "useOn"))) (pr-str [owner extra]))
            (is (= declined (mapv #(select-keys % [:reason :zones]) (kinds seen :apiary.declined))) (pr-str [owner extra]))))))))

(deftest no-zone-list-declines-the-harvest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (assoc (world {:inventory (inv "shears" 1)}) :zones nil))
              result (await (child-outcome eng job {} 40))]
          (is (= :no-hive (:reason result)))
          (is (empty? (calls p "useOn")))
          (is (= [:no-zones] (mapv :reason (kinds seen :apiary.declined)))))))))
