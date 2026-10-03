(ns engine.till-test
  "jobs.farm.till against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.farm.till :as till]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
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

(def job 'jobs.farm.till)
(def hoe [{:name "stone_hoe" :count 1}])
(def box {:from {:x 1 :y 63 :z 1} :to {:x 2 :y 63 :z 2}})
(def four {"1,63,1" "dirt" "2,63,1" "grass_block" "1,63,2" "dirt" "2,63,2" "grass_block"})

(deftest cells-of-a-box-any-corner-order
  (are [args expected] (= expected (set (till/cells args)))
    {:from {:x 0 :y 5 :z 0} :to {:x 1 :y 5 :z 1}} #{{:x 0 :y 5 :z 0} {:x 1 :y 5 :z 0} {:x 0 :y 5 :z 1} {:x 1 :y 5 :z 1}}
    {:from {:x 1 :y 5 :z 1} :to {:x 0 :y 5 :z 0}} #{{:x 0 :y 5 :z 0} {:x 1 :y 5 :z 0} {:x 0 :y 5 :z 1} {:x 1 :y 5 :z 1}}
    {:center {:x 3 :y 7 :z 3} :radius 0} #{{:x 3 :y 7 :z 3}}))

(deftest cells-of-a-radius-is-the-square-at-center-y
  (let [cs (till/cells {:center {:x 10 :y 63 :z -4} :radius 2})]
    (is (= 25 (count cs)))
    (is (= #{63} (set (map :y cs))))
    (is (= #{8 9 10 11 12} (set (map :x cs))))
    (is (= #{-6 -5 -4 -3 -2} (set (map :z cs))))))

(deftest cells-allows-256-and-rejects-more-or-nothing
  (is (= 225 (count (till/cells {:center {:x 0 :y 63 :z 0} :radius 7}))) "15x15")
  (is (= 256 (count (till/cells {:from {:x 0 :y 63 :z 0} :to {:x 15 :y 63 :z 15}}))))
  (are [args] (thrown? js/Error (till/cells args))
    {:from {:x 0 :y 63 :z 0} :to {:x 16 :y 63 :z 15}}
    {:center {:x 0 :y 63 :z 0} :radius 8}
    {}
    {:from {:x 0 :y 63 :z 0}}
    {:center {:x 0 :y 63 :z 0}}))

(deftest till-tills-a-two-by-two-box
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory hoe :blocks four})
              result (await (child-outcome eng job box 12))]
          (is (= {:tilled 4 :skipped {}} result))
          (is (= ["farmland"] (distinct (map #(block-at p %) (till/cells box)))))
          (is (= 4 (count (calls p "useOn"))))
          (is (= 1 (count (kinds seen :till.done)))))))))

(deftest till-radius-mode-tills-the-square
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cs (till/cells {:center {:x 0 :y 63 :z 0} :radius 1})
              {:keys [eng p]} (setup {:inventory hoe :blocks (into {} (map (fn [{:keys [x y z]}] [(str x "," y "," z) "dirt"])) cs)})
              result (await (child-outcome eng job {:center {:x 0 :y 63 :z 0} :radius 1} 20))]
          (is (= 9 (:tilled result)))
          (is (= ["farmland"] (distinct (map #(block-at p %) cs)))))))))

(deftest till-leaves-farmland-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks (zipmap (keys four) (repeat "farmland"))})
              result (await (child-outcome eng job box 6))]
          (is (= {:tilled 0 :skipped {}} result))
          (is (empty? (calls p "useOn"))))))))

(deftest till-skips-what-is-not-tillable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:inventory hoe :blocks (assoc four "1,63,1" "stone")})
              result (await (child-outcome eng job box 12))]
          (is (= {:tilled 3 :skipped {{:x 1 :y 63 :z 1} :not-tillable}} result))
          (is (= 1 (count (kinds seen :till.skipped)))))))))

(deftest till-skips-a-cell-covered-by-a-solid-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory hoe :blocks (assoc four "1,64,1" "cobblestone")})
              result (await (child-outcome eng job box 12))]
          (is (= {:tilled 3 :skipped {{:x 1 :y 63 :z 1} :covered}} result)))))))

(deftest till-digs-ground-cover-then-tills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks (assoc four "1,64,1" "short_grass")})
              result (await (child-outcome eng job box 14))]
          (is (= {:tilled 4 :skipped {}} result))
          (is (= 1 (count (calls p "dig"))))
          (is (= "farmland" (block-at p {:x 1 :y 63 :z 1}))))))))

(deftest till-without-a-hoe-the-check-is-false
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks four})]
          (core/submit! eng (list job box) {})
          (is (nil? (core/tick! eng)) "check false: nothing runs")
          (is (empty? (calls p "useOn"))))))))

(deftest till-gives-up-on-an-unreachable-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"5,63,0" "dirt"} :unreachable ["5,63,0"]})
              args {:from {:x 5 :y 63 :z 0} :to {:x 5 :y 63 :z 0}}
              result (await (child-outcome eng job args 8))]
          (is (= {:tilled 0 :skipped {{:x 5 :y 63 :z 0} :unreachable}} result))
          (is (empty? (calls p "useOn"))))))))

(deftest till-skips-a-cell-the-use-refuses-after-two-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"1,63,1" "dirt"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}]
          (.override (.-world p) "useOn" (fn ^:async f [_ _ _] #js {:status "unchanged" :before #js {:name "dirt"} :after #js {:name "dirt"} :consumed 0}))
          (is (= {:tilled 0 :skipped {{:x 1 :y 63 :z 1} :refused}} (await (child-outcome eng job args 8))))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest till-keeps-its-count-across-a-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks four})
              world (.-world p)
              out (atom nil)
              parent {:check (constantly true)
                      :round (fn ^:async r [c]
                               (let [r (await (ctx/call-child c :kid job box))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (.hold world "useOn")
          (let [running (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= 2 (count (calls p "useOn"))) "one tilled, the second held")
            (takeover/take! eng {:who "claude" :why "cut"})
            (await running))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (run-until-empty eng 12))
          (is (= {:tilled 4 :skipped {}} @out))
          (is (= ["farmland"] (distinct (map #(block-at p %) (till/cells box))))))))))


(deftest till-tills-a-cell-the-world-turns-back-and-counts-it-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"1,63,1" "dirt"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}
              first? (atom true)]
          (.override (.-world p) "useOn"
                     (fn ^:async f [token a impl]
                       (let [r (await (impl token a))]
                         (when @first?
                           (reset! first? false)
                           (.set (.. p -world -state -blocks) "1,63,1" "dirt"))
                         r)))
          (is (= {:tilled 1 :skipped {}} (await (child-outcome eng job args 10))))
          (is (= 2 (count (calls p "useOn")))))))))

(deftest till-skips-a-cell-whose-cover-will-not-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory hoe :blocks {"1,63,1" "dirt" "1,64,1" "short_grass"}})
              args {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "unreachable"}))
          (is (= {:tilled 0 :skipped {{:x 1 :y 63 :z 1} :cover-stuck}} (await (child-outcome eng job args 10))))
          (is (= 2 (count (calls p "dig")))))))))
