(ns engine.tidy-prepare-contract-test
  "jobs.farm.tidy and jobs.forestry.prepare as contract jobs: they export the contract and end with a kind and a cause
  (0 of N is never done; a partial run carries its counts)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.farm-tidy-test :as ft]
            [engine.forest-maintain-test :as fm :refer [ground]]
            [engine.forest-prepare-test :as fp]
            [engine.library-test :as lt]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(defn ^:async ending-of
  "Run job with args as a ctx/call child of a recording parent on eng, at most n ticks: the child's {:kind :cause :data}
  (nil when it does not end)."
  ([eng job args n] (ending-of eng job args n tu/run-until-empty))
  ([eng job args n run-until-empty]
  (let [out (atom nil)
        parent {:conditions []
                :round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) (ctx/end c :done nil)))))}
        eng (assoc-in eng [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out)))

(defn kc [e] (select-keys e [:kind :cause]))

(def jobs '[jobs.farm.tidy jobs.forestry.prepare jobs.forestry.maintain])

(deftest both-jobs-export-the-contract-and-quote-validly
  (let [p (tu/fake {})]
    (doseq [sym jobs
            :let [job (get registry/jobs sym)]]
      (is (vector? (:conditions job)) (str sym " has conditions"))
      (is (nil? (:check job)) (str sym " has no old check"))
      (is (vector? (:target job)) (str sym " has a target"))
      (is (fn? (:done? job)) (str sym " has done?"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args {:plan "p"}}))) (str sym)))))

;; ------------------------------------------------------------------ farm.tidy

(def tidy 'jobs.farm.tidy)

(deftest tidy-digging-strays-is-done-with-the-counts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (ft/start ft/tidy-spec {"field" ft/field-plan})
              e (await (ending-of eng tidy {:plan "field"} 200 ft/run-until-empty))]
          (is (= :done (:kind e)))
          (is (= 5 (get-in e [:data :dug]))))))))

(deftest tidy-of-a-clean-field-is-done-with-nothing-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (ft/start {:blocks (ft/put [2 64 2 "wheat"])} {"field" {:id "field" :parts [{:id "w" :cells [[2 64 2]] :want {:crop "wheat"}}]}})
              e (await (ending-of eng tidy {:plan "field"} 200 ft/run-until-empty))]
          (is (= :done (:kind e)))
          (is (= 0 (get-in e [:data :dug]))))))))

(deftest tidy-without-a-plan-is-condition-failed-plan-trouble
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (ft/start ft/tidy-spec {})
              e (await (ending-of eng tidy {:plan "nope"} 20 ft/run-until-empty))]
          (is (= {:kind :condition-failed :cause {:reason :plan-trouble :why "no such plan"}} (kc e))))))))

(deftest tidy-that-refuses-every-stray-is-condition-failed-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (ft/start (ft/hazard-world [3 64 2 "water"]) {"h" ft/hazard-plan})
              e (await (ending-of eng tidy {:plan "h" :accept #{}} 200 ft/run-until-empty))]
          (is (= {:kind :condition-failed :cause :refused} (kc e)))
          (is (= 0 (get-in e [:data :dug])))
          (is (= 1 (count (get-in e [:data :refused])))))))))

;; ------------------------------------------------------------------ forestry.prepare

(def prepare 'jobs.forestry.prepare)

(deftest prepare-readying-a-cell-is-done-with-the-counts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fm/start (fp/world {"3,64,0" "short_grass"} (fm/item "oak_sapling" 1)) {"forest" fp/one-cell})
              e (await (ending-of eng prepare {:plan "forest"} 300))]
          (is (= :done (:kind e)))
          (is (= {:cleared 1 :planted 1} (select-keys (:data e) [:cleared :planted]))))))))

(deftest prepare-with-a-plan-it-cannot-work-is-condition-failed-plan-trouble
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fm/start (fp/world {}) {})
              e (await (ending-of eng prepare {:plan "nope"} 20))]
          (is (= {:kind :condition-failed :cause {:reason :plan-trouble :why "no such plan"}} (kc e))))))))

(deftest prepare-with-nothing-it-can-improve-is-condition-failed-naming-the-worst-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (fp/oak-plan [3 64 0])
              spec {:blocks (merge (ground [[3 0]]) {"3,64,0" "birch_sapling"}) :inventory [(fm/item "oak_sapling" 1)]}
              {:keys [eng]} (fm/start spec {"forest" plan})
              e (await (ending-of eng prepare {:plan "forest"} 40))]
          (is (= [:condition-failed :cells-wrong] [(:kind e) (contract/cause-key e)])))))))

(deftest prepare-whose-headroom-stays-unseen-after-the-look-ends-condition-failed-saying-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec (fp/world {"3,64,0" "cobblestone"} (fm/item "iron_pickaxe" 1) (fm/item "oak_sapling" 1))
              p (tu/seeing-after-look (tu/fake-on-floor spec))
              {:keys [eng]} (fm/start spec {"forest" fp/one-cell} (tu/tmp-dir) p)
              hidden? #(= 65 (.-y %))
              seen-at (.-seenBlockAt p)
              sensed-at (.-sensedAt p)]
          (aset p "seenBlockAt" (fn [pos] (if (hidden? pos) #js {:unknown true :pos pos} (seen-at pos))))
          (aset p "sensedAt" (fn [pos] (if (hidden? pos) #js {:unknown true} (sensed-at pos))))
          (let [e (await (ending-of eng prepare {:plan "forest"} 40))]
            (is (= [:condition-failed :cells-unloaded] [(:kind e) (contract/cause-key e)]))
            (is (= [] (fm/digs p)))))))))

(deftest a-queued-prepare-with-unseen-headroom-looks-once-and-stays-unstarted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec (fp/world {"3,64,0" "cobblestone"} (fm/item "iron_pickaxe" 1) (fm/item "oak_sapling" 1))
              p (tu/fake-on-floor spec)
              {:keys [eng]} (fm/start spec {"forest" fp/one-cell} (tu/tmp-dir) p)
              hidden? #(= 65 (.-y %))
              seen-at (.-seenBlockAt p)
              sensed-at (.-sensedAt p)
              looks #(count (filter (fn [c] (= "look" (.-name c))) (.-calls (.-world p))))
              id (core/submit! eng '(jobs.forestry.prepare {:plan "forest"}) {})]
          (aset p "seenBlockAt" (fn [pos] (if (hidden? pos) #js {:unknown true :pos pos} (seen-at pos))))
          (aset p "sensedAt" (fn [pos] (if (hidden? pos) #js {:unknown true} (sensed-at pos))))
          (await (tu/run-until-empty eng 6))
          (let [n (looks)]
            (is (pos? n) "it looked round")
            (await (tu/run-until-empty eng 6))
            (is (= n (looks)) "once from this place"))
          (is (= [id] (:list (core/state eng))) "stays queued")
          (is (nil? (:call (:contract (mem/job-mem (mem/view (:store eng)) id [])))) "never started")
          (is (= [] (fm/digs p))))))))

;; ------------------------------------------------------------------ forestry.maintain

(def maintain 'jobs.forestry.maintain)

(deftest maintain-felling-and-planting-is-done-with-the-counts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fm/start fm/oak-world {"forest" fm/oak-cell})
              e (await (ending-of eng maintain {:plan "forest"} 300))]
          (is (= :done (:kind e)))
          (is (= {:felled 1 :planted 1} (select-keys (:data e) [:felled :planted]))))))))

(deftest maintain-with-a-plan-it-cannot-work-is-condition-failed-plan-trouble
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fm/start fm/oak-world {})
              e (await (ending-of eng maintain {:plan "nope"} 20))]
          (is (= {:kind :condition-failed :cause {:reason :plan-trouble :why "no such plan"}} (kc e))))))))

(deftest maintain-that-only-left-trees-standing-is-nothing-possible-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fm/start {:blocks (merge (ground [[3 0]]) (lt/tree 3 0 "oak" 8)) :inventory [(fm/item "oak_sapling" 1)]} {"forest" fm/oak-cell})
              e (await (ending-of eng maintain {:plan "forest" :pillar? false} 300))]
          (is (= {:kind :nothing-possible :cause :left} (kc e)))
          (is (= [{:x 3 :y 64 :z 0}] (get-in e [:data :left]))))))))
