(ns engine.plug-in-test
  "jobs.survival.plug-in: seals the body in where it stands, by walls, a pit or a side pocket, and ends done once enclosed."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.cornered-test :as ct]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.ledger :as ledger]))

(def cobble [{:name "cobblestone" :count 20}])
(def pick [{:name "stone_pickaxe" :count 1}])

(defn ^:async plug!
  "Run plug-in with args over world through a recording parent: {:out the ending :p :eng}."
  [world args]
  (let [clock (atom 1000000)
        out (atom :not-done)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        now (tu/act-clock clock p 1000)
        caller {:round (fn ^:async caller-round [c]
                         (let [r (await (ctx/call c :plug 'jobs.survival.plug-in args))]
                           (if (= :continue r) :continue (do (reset! out r) (ctx/end c :done nil)))))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'caller caller) :triggers triggers/all :dir (tu/tmp-dir)
                          :now now :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (core/submit! eng '(caller) {})
    (loop [i 0]
      (when (and (= :not-done @out) (< i 40))
        (await (core/tick! eng))
        (recur (inc i))))
    {:out @out :p p :eng eng}))

(defn cells-of [p name]
  (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)]) (ct/calls p name)))

(defn purposes [eng] (set (map :purpose (ledger/open-entries (mem/view (:store eng))))))

(deftest walls-seal-the-open-sides-and-roof-and-end-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (plug! {:blocks ct/dead-end :inventory cobble :entities [(ct/skeleton 4 64 0)]} {}))]
          (is (= [:done :walls] [(:kind out) (:way (:data out))]))
          (is (= [[1 64 0] [1 65 0]] (cells-of p "place")) "the open side towards the skeleton, feet then head")
          (is (= #{:retreat-plug} (purposes eng))))))))

(deftest the-callers-purpose-labels-the-ledger-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (plug! {:blocks ct/dead-end :inventory cobble} {:purpose :shelter}))]
          (is (= #{:shelter} (purposes eng))))))))

(deftest a-pit-digs-down-and-plugs-the-hole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/deep-dead-end :inventory pick :entities [(ct/zombie 1 64 0)]} {:ways #{:pit}}))]
          (is (= [:done :pit] [(:kind out) (:way (:data out))]))
          (is (= [[0 63 0] [0 62 0]] (cells-of p "dig"))))))))

(deftest a-pocket-is-dug-and-its-way-in-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/dead-end :inventory pick :entities [(ct/skeleton 4 64 0)]} {:ways #{:pocket}}))]
          (is (= [:done :pocket] [(:kind out) (:way (:data out))]))
          (is (= #{[-1 64 0] [-1 65 0]} (set (cells-of p "dig"))))
          (is (= #{[0 64 0] [0 65 0]} (set (cells-of p "place")))))))))

(deftest a-body-already-enclosed-is-done-before-anything-is-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks (ct/rock [-3 9] [62 67] #{[0 64 0] [0 65 0]}) :inventory cobble} {}))]
          (is (= :done (:kind out)))
          (is (empty? (ct/calls p "place"))))))))

(deftest nothing-to-seal-with-or-dig-with-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/dead-end} {}))]
          (is (= :nothing-possible (:kind out)))
          (is (empty? (ct/calls p "place"))))))))

(deftest a-way-left-out-of-ways-is-not-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/dead-end :inventory cobble} {:ways #{:pit}}))]
          (is (= :nothing-possible (:kind out)))
          (is (empty? (ct/calls p "place"))))))))
