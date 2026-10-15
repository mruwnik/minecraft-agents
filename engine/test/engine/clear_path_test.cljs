(ns engine.clear-path-test
  "jobs.access.clear-path against the fake world: how a dig child's wait or a refilled cell ends the door."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(def job 'jobs.access.clear-path)

(def wall
  "A stone wall at x 1..2 (y 64..65, z -1..1) on a stone floor, the body at the origin facing east."
  {:self {:pos {:x 0 :y 64 :z 0}}
   :blocks (merge (box -2 63 -2 8 63 2 "stone") (box 1 64 -1 2 65 1 "stone"))})

(defn ^:async run-door
  "Run clear-path over world (stub: a replacement round for jobs.blocks.dig, or nil) as a child, :east :max-thick 3
  unless args says otherwise; its result."
  ([world stub] (run-door world stub {:heading :east :max-thick 3}))
  ([world stub args] (run-door world stub args nil))
  ([world stub args rounds] (run-door world stub args rounds tu/outcome))
  ([world stub args rounds f]
   (let [clock (atom 1000000)
         [_ sink] (tu/legacy-capture-sink)
         p (tu/fake (dissoc world :plans))
         out (atom :not-done)
         parent (tu/recording-parent job args out f)
         jobs (cond-> (assoc registry/jobs 'recording-parent parent)
                stub (assoc 'jobs.blocks.dig (assoc (get registry/jobs 'jobs.blocks.dig) :round stub))
                rounds (as-> js (reduce-kv (fn [m k round] (assoc m k (assoc (get m k) :round round))) js rounds)))
         eng (core/create {:primitives p :jobs jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                           :world (ew/of-data (:plans world {}) {} [])
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     (core/submit! eng '(recording-parent) {})
     (loop [i 0]
       (when (and (< i 60) (seq (:list (core/state eng))))
         (swap! clock + 700)
         (await (core/tick! eng))
         (recur (inc i))))
     @out)))

(deftest a-dig-child-that-waits-ends-the-door-dig-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-door wall nil))]
          (is (= [:stopped :dig-waits :no-tool] [(:status r) (:reason r) (:reason (:wait r))]) (pr-str r))
          (is (= [1 65 0] (:cell r)) "the top layer first"))))))

(deftest by-hand-digs-the-door-with-no-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-door wall nil {:heading :east :max-thick 3 :by-hand true}))]
          (is (= [:done [3 64 0]] [(:status r) (:through r)]) (pr-str r))
          (is (= 4 (count (:dug r))) "two blocks high, two thick, bare-handed"))))))

(deftest a-cell-that-is-dug-but-never-clears-ends-refills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [digs (atom 0)
              stub (fn ^:async refilling-dig [c]
                     (swap! digs inc)
                     (ctx/end c :done nil {:dug true :reason :dug}))
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) stub))]
          (is (= [:stopped :refills] [(:status r) (:reason r)]) (pr-str r))
          (is (= 8 @digs) "a cell is dug max-cell-digs times"))))))

(deftest a-dig-child-that-yields-continue-counts-as-one-dig-of-the-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom 0)
              stub (fn ^:async slow-dig [c]
                     (if (zero? (mod (swap! calls inc) 4))
                       (ctx/end c :done nil {:dug true :reason :dug})
                       :continue))
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) stub))]
          (is (= [:refills 8] [(:reason r) (count (:dug r))]) "a cell is dug max-cell-digs times, not max-cell-digs calls"))))))

(deftest a-dig-that-fails-keeps-its-primitive-status-in-dig-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stub (fn ^:async failing-dig [c]
                     (ctx/end c :nothing-possible :failed {:dug false :primitive "timeout"}))
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) stub))]
          (is (= [:stopped :dig-failed] [(:status r) (:reason r)]) (pr-str r))
          (is (= {:reason :failed :primitive "timeout"} (:dig r)) (pr-str r)))))))

(deftest a-wall-with-a-floor-beyond-is-dug-and-walked-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) nil))]
          (is (= [:done [3 64 0]] [(:status r) (:through r)]) (pr-str r))
          (is (= 4 (count (:dug r))) "two blocks high, two thick")
          (is (= [3 64 0] (:at r))))))))

(deftest the-step-through-is-a-go-to-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom nil)
              go-to (fn ^:async refusing-go-to [c]
                      (reset! asked (:args c))
                      (ctx/end c :nothing-possible :unreachable {:arrived false}))
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) nil
                                 {:heading :east :max-thick 3} {'jobs.movement.go-to go-to} identity))]
          (is (= [:nothing-possible :step-failed] [(:kind r) (contract/cause-key r)]) (pr-str r))
          (is (= [[3 64 0] 0 #{:walk :clear-seal}] ((juxt :pos :range :ways) @asked)) (pr-str @asked)))))))

(deftest a-missing-heading-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (run-door wall nil {:max-thick 3} nil identity))]
          (is (= [:impossible :bad-args] [(:kind e) (contract/cause-key e)]) (pr-str e)))))))

(deftest the-job-exports-the-contract-and-quotes-validly
  (let [def (get registry/jobs job)
        q ((:quote-of def) {:primitives (tu/fake wall) :args {:heading :east :max-thick 3}})]
    (is (= [:heading] (:target def)))
    (is (seq (:conditions def)))
    (is (nil? (:check def)))
    (is (nil? (contract/quote-problem q)) (pr-str q))))

(deftest a-door-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pick {:inventory [{:name "wooden_pickaxe" :count 1}]}
              end (fn ^:async end* [world stub args]
                    (let [e (await (run-door world stub args nil identity))]
                      [(:kind e) (contract/cause-key e) (:kind (:because e))]))
              refilling (fn ^:async refilling [c] (ctx/end c :done nil {:dug true :reason :dug}))
              failing (fn ^:async failing [c] (ctx/end c :nothing-possible :failed {:dug false :primitive "timeout"}))
              east {:heading :east :max-thick 3}]
          (is (= [:done :done nil] (await (end (merge wall pick) nil east))))
          (is (= [:condition-failed :dig-waits :condition-failed] (await (end wall nil east))) "the dig child's wait is its because")
          (is (= [:nothing-possible :refills nil] (await (end (merge wall pick) refilling east))))
          (is (= [:nothing-possible :dig-failed :nothing-possible] (await (end (merge wall pick) failing east))))
          (is (= [:condition-failed :no-door nil] (await (end (dissoc wall :blocks) nil east))))
          (is (= [:condition-failed :no-door nil] (await (end wall nil {:heading :east :max-thick 1}))) "a wall thicker than :max-thick")
          (is (= [:condition-failed :footprint nil]
                 (await (end (assoc (merge wall pick) :plans {"hut" (shape/with-author {:id "hut" :parts [{:id "w" :cells [[1 64 0]] :want "stone"}]} "Fake")})
                             nil east)))))))))

(deftest a-wall-of-the-bodys-own-plan-is-not-dug-unless-told-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (shape/with-author {:id "hut" :parts [{:id "w" :cells [[1 64 0] [1 65 0] [2 64 0] [2 65 0]] :want "stone"}]} "Fake")
              world (assoc wall :plans {"hut" plan})
              r (await (run-door world nil {:heading :east :max-thick 3 :by-hand true}))
              r2 (await (run-door world nil {:heading :east :max-thick 3 :spare-own-builds false :by-hand true}))]
          (is (= [:stopped :footprint "hut"] [(:status r) (:reason r) (:plan r)]) (pr-str r))
          (is (= :done (:status r2)) (pr-str r2)))))))
