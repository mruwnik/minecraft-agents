(ns engine.forestry-contract-test
  "jobs.forestry.harvest-wood and gather.make-obsidian as contract jobs: they export the contract and end
  with a kind and a cause (0 of N is never done); a child's failed ending is read, not taken for success."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.forest-maintain-test :as fm]
            [engine.library-test :as lt]
            [engine.obtain-obsidian-test :as ot]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(defn kc [e] {:kind (:kind e) :cause (contract/cause-key e)})

(def jobs '[jobs.forestry.harvest-wood jobs.gather.make-obsidian])

(deftest the-three-jobs-export-the-contract-and-quote-validly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [eng (core/create {:primitives (tu/fake {}) :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now (constantly 0)
                                :events (events/make {:body "Fake" :sinks [] :now (constantly 0)})})]
          (doseq [sym jobs
                  :let [job (get registry/jobs sym)]]
            (is (vector? (:target job)) (str sym " has a target"))
            (is (fn? (:done? job)) (str sym " has done?"))
            (is (nil? (contract/quote-problem (:quote (await (event-api/quote-view eng (str sym) "{:radius 10}")))))
                (str sym))))))))

;; ------------------------------------------------------------------ harvest-wood

(def harvest 'jobs.forestry.harvest-wood)
(def oak {:blocks (merge (lt/tree 3 0 "oak" 3) {"3,63,0" "grass_block"})})

(defn ^:async harvest-end
  ([spec args n] (harvest-end spec args n nil))
  ([spec args n wrap-fell]
   (let [{:keys [eng] :as s} (lt/setup spec)
         eng (cond-> eng wrap-fell (assoc-in [:jobs 'jobs.forestry.fell-tree :round] (wrap-fell (get-in eng [:jobs 'jobs.forestry.fell-tree :round]))))]
     (assoc s :end (await (ending-of {:eng eng} harvest args n))))))

(deftest a-felled-tree-is-done-with-no-data
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end]} (await (harvest-end oak {:species "oak" :radius 10} 40))]
          (is (= :done (:kind end)))
          (is (nil? (:data end))))))))

(deftest count-short-of-the-trees-is-condition-failed-no-tree-with-the-logs-got
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end]} (await (harvest-end oak {:species "oak" :radius 10 :count 6} 80))]
          (is (= {:kind :condition-failed :cause :no-tree} (kc end)))
          (is (= 3 (get-in end [:data :got]))))))))

(defn fail [kind cause] (fn [_real] (fn ^:async f [c] (ctx/end c kind cause {:columns [[3 0]] :why :walk}))))

(deftest a-fell-ending-that-is-not-done-or-no-tree-ends-the-job-with-that-kind-and-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end p]} (await (harvest-end oak {:species "oak" :radius 10} 40 (fail :nothing-possible :tree-unreachable)))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end)) "not taken for a felled tree")
          (is (= [] (lt/calls p "dig")) "nothing collected or planted after it")
          (is (= [[3 0]] (get-in end [:data :columns]))))
        (let [{:keys [end]} (await (harvest-end oak {:species "oak" :radius 10} 40 (fail :condition-failed {:reason :need :any-of ["dirt"] :count 4})))]
          (is (= {:kind :condition-failed :cause :need} (kc end))))))))

;; ------------------------------------------------------------------ make-obsidian

(def obsidian 'jobs.gather.make-obsidian)

(defn ^:async obsidian-end [w args]
  (let [s (lt/setup w)]
    (assoc s :end (await (ending-of s obsidian args 200)))))

(deftest obsidian-made-is-done-and-the-missing-bucket-or-lava-are-facts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end p]} (await (obsidian-end (ot/world ot/lava-lake [ot/pick {:name "water_bucket" :count 1}]) {:count 1}))]
          (is (= :done (:kind end)))
          (is (= {:got 1 :text "made 1 obsidian"} (:data end)))
          (is (= 1 (ot/carried p "obsidian"))))
        (let [{:keys [end]} (await (obsidian-end (ot/world ot/lava-lake [ot/pick]) {:count 1 :fetch false}))]
          (is (= {:kind :condition-failed :cause :no-bucket} (kc end))))
        (let [{:keys [end]} (await (obsidian-end (ot/world ot/lava-lake [ot/pick]) {:count 1}))]
          (is (= {:kind :condition-failed :cause :need} (kc end)) "a bucket is fetched first; none to be had passes up"))
        (let [{:keys [end]} (await (obsidian-end (ot/world ot/ground [ot/pick {:name "water_bucket" :count 1}]) {:count 1}))]
          (is (= {:kind :condition-failed :cause :no-lava} (kc end))))))))

(deftest a-scoop-that-fails-at-seen-water-is-scoop-failed-not-no-bucket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ot/world (assoc ot/lava-lake "1,63,1" "water") [ot/pick {:name "bucket" :count 1}])
              s (lt/setup w)]
          (.override (.-world (:p s)) "place" (fn [_ _ _] #js {:status "failed"}))
          (let [end (await (ending-of s obsidian {:count 1} 200))]
            (is (= {:kind :nothing-possible :cause :scoop-failed} (kc end)))))))))

(defn ^:async obsidian-with-slow-lava
  "make-obsidian on a lava lake where the poured water never turns it, the engine's clock moving 20 seconds with every
  primitive call; the 'wait' calls it made, and the end state."
  [args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (ot/world ot/lava-lake [ot/pick {:name "water_bucket" :count 1}])))
        now (tu/act-clock clock p 20000)
        eng (core/create {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})
        s {:eng eng :p p :seen seen}
        placed (atom 0)]
    (.override (.-world p) "place"
               (fn [token a impl]
                 (if (= "water_bucket" (.-item a))
                   (do (swap! placed inc) #js {:status "placed" :block "water"})
                   (impl token a))))
    (core/submit! eng (list obsidian args) {})
    (await (tu/run-until-empty eng 80))
    (assoc s :waits (count (filter #(= "wait" (.-name %)) (.-calls (.-world p)))) :placed @placed)))

(deftest the-wait-for-the-conversion-ends-with-the-engines-limit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [waits placed seen]} (await (obsidian-with-slow-lava {:count 1 :spend {:wall-time 61}}))]
          (is (= 1 placed))
          (is (= [:wall-time] (mapv :reason (filter #(= :stopped (:kind %)) @seen))) "the engine's limit ends it")
          (is (< waits 6) "the limit cuts the wait between its half-second looks"))))))

(defn ^:async obsidian-cut-after-the-pour
  "make-obsidian on a lava lake cancelled right after it poured, the body then moved far off (x 8.5, out of reach of the
  pour); scoop-fails? makes the bucket's place answer failed. {:p :end} with the job's stopped event data."
  [scoop-fails?]
  (let [{:keys [eng p seen]} (lt/setup (ot/world ot/lava-lake [ot/pick {:name "water_bucket" :count 1}]))
        id (core/submit! eng (list obsidian {:count 1}) {})]
    (.override (.-world p) "place"
               (fn ^:async f [token a impl]
                 (cond
                   (= "water_bucket" (.-item a)) (let [r (await (impl token a))]
                                                   (fake/swap-self! p assoc :pos [8.5 64 0.5])
                                                   (core/cancel! eng id)
                                                   r)
                   scoop-fails? #js {:status "failed"}
                   :else (impl token a))))
    (await (tu/run-until-empty eng 40))
    {:p p :seen seen}))

(deftest a-cut-run-walks-back-and-scoops-its-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (obsidian-cut-after-the-pour false))]
          (is (not= "water" (.-name (.blockAt p (tu/pos 4 64 0)))) "the water is scooped up")
          (is (= 1 (ot/carried p "water_bucket")) "the bucket is full again")
          (is (let [pos (.-pos (.self p))] (< (fake/dist [(.-x pos) (.-y pos) (.-z pos)] [4 64 0]) 4.5)) "from within reach"))))))

(deftest a-cut-run-whose-scoop-fails-reports-the-water-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (obsidian-cut-after-the-pour true))]
          (is (= [:water-left] (mapv :cause (filter #(= :cleanup_failed (:kind %)) @seen)))))))))
