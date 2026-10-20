(ns engine.manual-jobs-test
  "jobs.items.use (the job behind world.mjs use-on / interact) against the fake world."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.items.use :as use-on-job]
            [jobs.lib.world-files :as ew]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async outcome
  "Run job with args as the child of a recording parent in a world; {:result the child's result :p :seen}."
  [world job args]
  (let [{:keys [eng p seen]} (setup world)
        out (atom :not-done)
        parent (tu/recording-parent job args out)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng 40))
    {:result @out :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(def use-on 'jobs.items.use)
(def interact 'jobs.items.use)
(def composter {"2,63,0" "composter"})
(def far-composter {"14,63,0" "composter"})
(defn cow [id x] {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}})
(defn stacks [& names] {:inventory (mapv (fn [n] {:name n :count 3}) names)})

(deftest use-on-clicks-a-block-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p seen]} (await (outcome (merge {:blocks composter} (stacks "oak_leaves")) use-on
                                                      {:on {:x 2 :y 63 :z 0} :item "oak_leaves"}))]
          (is (true? (:used result)))
          (is (= :done (:status result)))
          (is (= 1 (count (calls p "useOn"))))
          (is (= 1 (count (ce/ends seen :used)))))))))

(deftest use-on-walks-into-reach-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (outcome (merge {:blocks far-composter} (stacks "oak_leaves")) use-on
                                                 {:on [14 63 0] :item "oak_leaves"}))]
          (is (true? (:used result)))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest use-on-stops-with-the-primitives-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result seen]} (await (outcome (merge {:blocks composter} (stacks "stick")) use-on
                                                    {:on [2 63 0] :item "oak_leaves"}))]
          (is (= :stopped (:status result)))
          (is (false? (:used result)))
          (is (= :no-item (:reason result)))
          (is (= 1 (count (ce/ends seen :used)))))))))

(deftest use-on-bad-args-are-never-from-can-start
  (are [args] (= [:never :bad-args] (tu/kind-reason (use-on-job/can-start? {:args args})))
    {:item "stick"}
    {:on [2 63 0] :face "sideways"}))

(deftest interact-uses-the-item-on-an-entity-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (outcome (merge {:entities [(cow 7 2)]} (stacks "wheat")) interact {:on 7 :item "wheat"}))]
          (is (true? (:used result)))
          (is (= :done (:status result)))
          (is (= 1 (count (calls p "interact")))))))))

(deftest interact-walks-to-a-far-entity
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (outcome (merge {:entities [(cow 7 14)]} (stacks "wheat")) interact {:on 7 :item "wheat"}))]
          (is (true? (:used result)))
          (is (= 1 (count (calls p "interact")))))))))

(deftest interact-stops-when-the-entity-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p seen]} (await (outcome {:entities [(cow 7 2)]} interact {:on 99}))]
          (is (= :stopped (:status result)))
          (is (= :gone (:reason result)))
          (is (empty? (calls p "interact")))
          (is (= "did not use the empty hand on entity 99: gone" (:text result))))))))

(deftest interact-stops-with-the-primitives-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result]} (await (outcome {:entities [(cow 7 2)]} interact {:on 7 :item "lead"}))]
          (is (= :stopped (:status result)))
          (is (= :no-item (:reason result))))))))
