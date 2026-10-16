(ns engine.path-test
  "jobs.build.path against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.takeover :as takeover]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [jobs.build.path :as path]))

(defn setup [world & [shared]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (or shared (ew/of-data {} {} []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn ^:async child-outcome
  "tu/child-outcome without the ending the adapter adds, and without :status when it is :done."
  [eng job args n]
  (let [r (await (tu/child-outcome eng job args n))]
    (if (map? r) (cond-> (dissoc r :ending) (= :done (:status r)) (dissoc :status)) r)))

(def job 'jobs.build.path)
(def shovel [{:name "stone_shovel" :count 1}])
(def box {:from {:x 1 :y 63 :z 1} :to {:x 2 :y 63 :z 2}})
(def four {"1,63,1" "dirt" "2,63,1" "grass_block" "1,63,2" "dirt" "2,63,2" "grass_block"})

(deftest path-shovels-a-two-by-two-box-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory shovel :blocks four})
              result (await (child-outcome eng job box 1))]
          (is (= {:pathed 4 :skipped {}} (dissoc result :text)))
          (is (= ["dirt_path"] (distinct (map #(block-at p %) (path/cells box)))))
          (is (= 4 (count (calls p "useOn")))))))))

(deftest path-leaves-a-path-alone-and-skips-what-is-not-pathable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks (assoc four "1,63,1" "dirt_path" "2,63,2" "stone")})
              result (await (child-outcome eng job box 8))]
          (is (= {:pathed 2 :skipped {{:x 2 :y 63 :z 2} :not-pathable}} (dissoc result :text)))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest path-skips-a-cell-covered-by-a-solid-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory shovel :blocks (assoc four "1,64,1" "cobblestone")})
              result (await (child-outcome eng job box 8))]
          (is (= {:pathed 3 :skipped {{:x 1 :y 63 :z 1} :covered}} (dissoc result :text))))))))

(deftest path-digs-ground-cover-then-shovels
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks (assoc four "1,64,1" "short_grass")})
              result (await (child-outcome eng job box 14))]
          (is (= {:pathed 4 :skipped {}} (dissoc result :text)))
          (is (= 1 (count (calls p "dig"))))
          (is (= "dirt_path" (block-at p {:x 1 :y 63 :z 1}))))))))

(deftest path-without-a-shovel-and-fetch-false-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks four})]
          (core/submit! eng (list job (assoc box :fetch false)) {})
          (is (nil? (core/tick! eng)) "check false: nothing runs")
          (is (= [:no-tool] (map :reason (kinds seen :waiting))))
          (is (empty? (kinds seen :fetch.started)) "fetch false: no fetch child")
          (is (empty? (calls p "useOn"))))))))

(deftest path-bad-args-end-impossible-instead-of-throwing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory shovel :blocks four})]
          (core/submit! eng (list job {:from {:x 0 :y 63 :z 0} :to {:x 30 :y 63 :z 30}}) {})
          (await (core/tick! eng))
          (is (empty? (:list (core/state eng))) "impossible: it leaves the queue")
          (is (empty? (calls p "useOn"))))))))

(deftest path-does-not-shovel-a-cell-whose-cover-is-unseen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks {"1,63,1" "dirt"} :unloaded ["1,64,1"]})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}]
          (is (= {:status :stopped :reason :unloaded :pathed 0 :skipped {{:x 1 :y 63 :z 1} :unloaded}} (dissoc (await (child-outcome eng job args 8)) :text)))
          (is (empty? (calls p "useOn"))))))))

(deftest path-skips-a-cell-the-use-refuses-after-two-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks {"1,63,1" "dirt"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}]
          (.override (.-world p) "useOn" (fn ^:async f [_ _ _] #js {:status "unchanged" :before #js {:name "dirt"} :after #js {:name "dirt"} :consumed 0}))
          (is (= {:status :stopped :reason :refused :pathed 0 :skipped {{:x 1 :y 63 :z 1} :refused}} (dissoc (await (child-outcome eng job args 8)) :text)))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest path-gives-up-on-an-unreachable-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks {"5,63,0" "dirt"} :unreachable ["5,63,0"]})
              args {:from {:x 5 :y 63 :z 0} :to {:x 5 :y 63 :z 0}}
              result (await (child-outcome eng job args 8))]
          (is (= {:status :stopped :reason :unreachable :pathed 0 :skipped {{:x 5 :y 63 :z 0} :unreachable}} (dissoc result :text)))
          (is (empty? (calls p "useOn"))))))))

(deftest path-keeps-its-count-across-a-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks four})
              world (.-world p)
              out (atom nil)
              parent (tu/recording-parent job box out #(dissoc (tu/outcome %) :status))
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (.hold world "useOn")
          (let [running (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= 1 (count (calls p "useOn"))) "the first use held")
            (takeover/take! eng {:who "claude" :why "cut"})
            (await running))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (run-until-empty eng 12))
          (is (= {:pathed 4 :skipped {}} (dissoc @out :text)))
          (is (= ["dirt_path"] (distinct (map #(block-at p %) (path/cells box))))))))))

(defn zone [min max] {:name "farm" :owner "Miles" :min min :max max})

(deftest path-declines-before-the-first-round-when-every-cell-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory shovel :blocks four} (ew/of-data {} {} [(zone [1 60 1] [2 70 2])]))]
          (core/submit! eng (list job box) {})
          (is (nil? (core/tick! eng)))
          (is (= [:refused] (map :reason (kinds seen :waiting))))
          (is (= [:refused] (map :reason (kinds seen :path.declined))))
          (is (empty? (calls p "useOn"))))))))

(deftest path-declines-without-a-zone-list-and-ignore-zones-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory shovel :blocks four} (ew/of-data {} {} nil))]
          (core/submit! eng (list job box) {})
          (is (nil? (core/tick! eng)))
          (is (= [:no-zones] (map :reason (kinds seen :waiting))))
          (is (empty? (calls p "useOn"))))
        (let [{:keys [eng]} (setup {:inventory shovel :blocks four} (ew/of-data {} {} nil))]
          (is (= {:pathed 4 :skipped {}} (dissoc (await (child-outcome eng job (assoc box :ignore-zones? true) 1)) :text))))
        (let [{:keys [eng]} (setup {:inventory shovel :blocks four} (ew/of-data {} {} [(zone [1 60 1] [2 70 2])]))]
          (is (= {:pathed 4 :skipped {}} (dissoc (await (child-outcome eng job (assoc box :ignore-zones? true) 1)) :text))))))))

(deftest path-skips-the-refused-cell-and-paths-the-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks four} (ew/of-data {} {} [(zone [2 60 2] [2 70 2])]))
              result (await (child-outcome eng job box 8))]
          (is (= {:pathed 3 :skipped {{:x 2 :y 63 :z 2} :not-permitted}} (dissoc result :text)))
          (is (= "grass_block" (block-at p {:x 2 :y 63 :z 2}))))))))

(deftest path-with-every-cell-already-a-path-is-done-with-nothing-pathed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks (zipmap (keys four) (repeat "dirt_path"))})
              result (await (child-outcome eng job box 2))]
          (is (= {:pathed 0 :skipped {} :already? true} (dissoc result :text)) "already done: ended by done? before any round")
          (is (empty? (calls p "useOn"))))))))

(deftest path-skips-cells-when-the-shovel-is-lost-mid-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks {"1,63,1" "dirt"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}]
          (.override (.-world p) "useOn" (fn ^:async f [_ _ _] #js {:status "no-item"}))
          (is (= {:status :stopped :reason :no-shovel :pathed 0 :skipped {{:x 1 :y 63 :z 1} :no-shovel}}
                 (dissoc (await (child-outcome eng job args 8)) :text))))))))

(deftest path-leaves-cells-in-the-footprint-of-its-own-plan-alone-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [author ["Fake" "Miles"]]
          (let [plan (shape/with-author {:id "hut" :parts [{:id "p" :box [[1 63 1] [2 63 2]] :want "stone"}]} author)
                {:keys [eng p]} (setup {:inventory shovel :blocks four} (ew/of-data {"hut" plan} {} []))]
            (await (child-outcome eng job box 4))
            (is (empty? (calls p "useOn")) author)))))))

(deftest shovel-wanted-asks-for-any-shovel-when-the-listed-set-is-empty
  (is (= {:kind "shovel"} (path/shovel-wanted {:args {:use {:tools []}}})))
  (is (= {:item "iron_shovel"} (path/shovel-wanted {:args {:use {:tools ["iron_shovel" "stone_shovel"]}}}))))
