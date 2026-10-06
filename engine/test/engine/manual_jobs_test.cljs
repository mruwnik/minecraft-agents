(ns engine.manual-jobs-test
  "jobs.blocks.use-on and jobs.items.interact (the jobs behind world.mjs use-on / interact) against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async outcome
  "Run job with args as the child of a recording parent in a world; {:result the child's result :p :seen}."
  [world job args]
  (let [{:keys [eng p seen]} (setup world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng 40))
    {:result @out :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(def use-on 'jobs.blocks.use-on)
(def interact 'jobs.items.interact)
(def composter {"2,63,0" "composter"})
(def far-composter {"14,63,0" "composter"})
(defn cow [id x] {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}})
(defn stacks [& names] {:inventory (mapv (fn [n] {:name n :count 3}) names)})

(deftest use-on-clicks-a-block-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p seen]} (await (outcome (merge {:blocks composter} (stacks "oak_leaves")) use-on
                                                      {:pos {:x 2 :y 63 :z 0} :item "oak_leaves"}))]
          (is (true? (:used result)))
          (is (nil? (:status result)))
          (is (= 1 (count (calls p "useOn"))))
          (is (= 1 (count (kinds seen :blocks.use-on.done)))))))))

(deftest use-on-walks-into-reach-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (outcome (merge {:blocks far-composter} (stacks "oak_leaves")) use-on
                                                 {:pos [14 63 0] :item "oak_leaves"}))]
          (is (true? (:used result)))
          (is (= 1 (count (calls p "useOn")))))))))

(deftest use-on-stops-with-the-primitives-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result seen]} (await (outcome (merge {:blocks composter} (stacks "stick")) use-on
                                                    {:pos [2 63 0] :item "oak_leaves"}))]
          (is (= :stopped (:status result)))
          (is (false? (:used result)))
          (is (= :no-item (:reason result)))
          (is (= 1 (count (kinds seen :blocks.use-on.done)))))))))

(deftest use-on-refuses-bad-args-without-a-click
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p seen]} (await (outcome {} use-on {:item "stick"}))]
          (is (= :stopped (:status result)))
          (is (= :bad-args (:reason result)))
          (is (empty? (calls p "useOn")))
          (is (= 1 (count (kinds seen :blocks.use-on.declined)))))))))

(deftest interact-uses-the-item-on-an-entity-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (outcome (merge {:entities [(cow 7 2)]} (stacks "wheat")) interact {:id 7 :item "wheat"}))]
          (is (true? (:used result)))
          (is (nil? (:status result)))
          (is (= 1 (count (calls p "interact")))))))))

(deftest interact-walks-to-a-far-entity
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (outcome (merge {:entities [(cow 7 14)]} (stacks "wheat")) interact {:id 7 :item "wheat"}))]
          (is (true? (:used result)))
          (is (= 1 (count (calls p "interact")))))))))

(deftest interact-stops-when-the-entity-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p seen]} (await (outcome {:entities [(cow 7 2)]} interact {:id 99}))]
          (is (= :stopped (:status result)))
          (is (= :gone (:reason result)))
          (is (empty? (calls p "interact")))
          (is (= 1 (count (kinds seen :items.interact.done)))))))))

(deftest interact-stops-with-the-primitives-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result]} (await (outcome {:entities [(cow 7 2)]} interact {:id 7 :item "lead"}))]
          (is (= :stopped (:status result)))
          (is (= :no-item (:reason result))))))))
