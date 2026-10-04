(ns engine.path-near-test
  "engine.path.near/walk-near!, the jobs' one-walk helper, over the path planner and the executor against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.path.near :as near]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]))

(def start {:x 0 :y 64 :z 0})

(def flat (floor -2 -3 40 3))

(defn walker
  "A job whose one round is one walk-near! with args, the return kept in out."
  [out args]
  {:check (constantly true)
   :round (fn ^:async walk-round [c]
            (reset! out (await (apply near/walk-near! c args)))
            :done)})

(defn ^:async walk!
  "Run one walk-near! (args after c) over world; {:eng :p :out}."
  ([world args] (walk! world args identity))
  ([world args prep]
   (let [clock (atom 1000000)
         [_ sink] (tu/legacy-capture-sink)
         p (prep (tu/fake (merge {:self {:pos start}} world)))
         out (atom :not-done)
         eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker (walker out args)) :triggers triggers/all
                           :dir (tu/tmp-dir) :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     (core/submit! eng '(walker) {})
     (loop [i 0]
       (when (and (< i 10) (seq (:list (core/state eng))))
         (await (core/tick! eng))
         (recur (inc i))))
     {:eng eng :p p :out out})))

(defn at [p] (let [pos (.-pos (.self p))] [(.-x pos) (.-y pos) (.-z pos)]))
(defn moved [eng] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))
(defn open? [p [x y z]] (:open (js->clj (.-properties (.blockAt p #js {:x x :y y :z z})) :keywordize-keys true)))

(deftest already-within-range-is-there-without-a-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (walk! {:blocks flat} [{:x 2 :y 64 :z 0} 3]))]
          (is (= :there @out))
          (is (= [] (tu/walk-calls p)))
          (is (= [] (moved eng))))))))

(deftest walks-on-the-floor-to-the-range-and-writes-a-moved-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[range cell] [[0 [10 64 0]] [3 [7 64 0]]]]
          (let [{:keys [out p eng]} (await (walk! {:blocks flat} [{:x 10 :y 64 :z 0} range]))]
            (is (= :there @out) (str "range " range))
            (is (= cell (mapv js/Math.floor (at p))) (str "range " range))
            (is (= [{:from start :to (zipmap [:x :y :z] (at p)) :status "arrived" :target {:x 10 :y 64 :z 0}}]
                   (moved eng)))))))))

(deftest a-fractional-target-is-its-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (walk! {:blocks flat} [{:x 10.7 :y 64.2 :z 0.5} 0]))]
          (is (= :there @out))
          (is (= [10 64 0] (mapv js/Math.floor (at p)))))))))

(deftest an-enclosed-goal-is-blocked-and-the-moved-entry-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walls (merge (box 9 64 -1 11 65 -1 "stone") (box 9 64 1 11 65 1 "stone")
                           (box 9 64 0 9 65 0 "stone") (box 11 64 0 11 65 0 "stone"))
              {:keys [out p eng]} (await (walk! {:blocks (merge flat walls)} [{:x 10 :y 64 :z 0} 0]))]
          (is (= :blocked @out))
          (is (= [0 64 0] (at p)))
          (is (= ["blocked"] (mapv :status (moved eng)))))))))

(deftest without-path-sensing-it-is-blocked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (walk! {:blocks flat} [{:x 10 :y 64 :z 0} 0] #(doto % (set! -pathWorld nil))))]
          (is (= :blocked @out))
          (is (= [0 64 0] (at p))))))))

(def gate-world
  {:blocks (merge flat (assoc (box 5 64 -6 5 64 6 "oak_fence") "5,64,0" "oak_fence_gate"))
   :states {"5,64,0" {:open false :facing "east"}}})

(deftest through-a-shut-gate-by-default-it-opens-passes-and-shuts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (walk! gate-world [{:x 10 :y 64 :z 0} 0]))]
          (is (= :there @out))
          (is (= [10 64 0] (at p)))
          (is (false? (open? p [5 64 0]))))))))

(deftest with-doors-never-a-shut-gate-is-a-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (walk! gate-world [{:x 10 :y 64 :z 0} 0 {:doors :never}]))]
          (is (= :blocked @out))
          (is (= [0 64 0] (at p)))
          (is (false? (open? p [5 64 0]))))))))
