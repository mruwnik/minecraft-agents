(ns engine.path-test
  "jobs.build.path against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.build.path :as path]))

(defn setup [world & [shared]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (or shared (ew/of-data {} {} []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng job args n]
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
(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

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
          (is (= {:pathed 4 :skipped {}} result))
          (is (= ["dirt_path"] (distinct (map #(block-at p %) (path/cells box)))))
          (is (= 4 (count (calls p "useOn"))))
          (is (= 1 (count (kinds seen :path.done)))))))))

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
          (is (= {:pathed 3 :skipped {{:x 1 :y 63 :z 1} :covered}} result)))))))

(deftest path-digs-ground-cover-then-shovels
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks (assoc four "1,64,1" "short_grass")})
              result (await (child-outcome eng job box 14))]
          (is (= {:pathed 4 :skipped {}} result))
          (is (= 1 (count (calls p "dig"))))
          (is (= "dirt_path" (block-at p {:x 1 :y 63 :z 1}))))))))

(deftest path-without-a-shovel-and-fetch-false-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks four})]
          (core/submit! eng (list job (assoc box :fetch false)) {})
          (is (nil? (core/tick! eng)) "check false: nothing runs")
          (is (empty? (calls p "useOn"))))))))

(deftest path-bad-args-wait-instead-of-throwing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory shovel :blocks four})]
          (core/submit! eng (list job {:from {:x 0 :y 63 :z 0} :to {:x 30 :y 63 :z 30}}) {})
          (is (nil? (core/tick! eng)))
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
              parent {:check (constantly true)
                      :round (fn ^:async r [c]
                               (let [r (await (ctx/call-child c :kid job box))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
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
          (is (= {:pathed 4 :skipped {}} @out))
          (is (= ["dirt_path"] (distinct (map #(block-at p %) (path/cells box))))))))))
