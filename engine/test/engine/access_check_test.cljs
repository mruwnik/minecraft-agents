(ns engine.access-check-test
  "jobs.debug.access-check against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def job 'jobs.debug.access-check)

(def blocks
  {"5,64,5" "stone" "6,64,5" "stone" "7,64,5" "stone" "8,64,5" "stone" "7,65,5" "lava"
   "0,63,1" "stone" "0,62,1" "air"})

(defn ^:async settle
  "Tick until the job is gone."
  [args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {:blocks blocks :unloaded ["90,64,90"] :self {:pos {:x 0 :y 64 :z 1}}})
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 100) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    {:out out :seen seen :p p}))

(defn verdict-of [out cell]
  (first (filter #(= cell (:cell %)) (:verdicts @out))))

(deftest reports-a-dig-and-a-place-verdict-per-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (settle {:cells [[5 64 5] [6 64 5] [7 64 5] [0 63 1] [90 64 90]]}))]
          (is (= {:dig {:ok true} :place {:ok false :reason :not-replaceable :block "stone"}}
                 (select-keys (verdict-of out [5 64 5]) [:dig :place])))
          (is (= :fluid-adjacent (get-in (verdict-of out [7 64 5]) [:dig :reason])))
          (is (= :under-feet (get-in (verdict-of out [0 63 1]) [:dig :reason])))
          (is (= :not-loaded (get-in (verdict-of out [90 64 90]) [:dig :reason])))
          (is (= :not-loaded (get-in (verdict-of out [90 64 90]) [:place :reason])))
          (is (= 1 (count (filter #(= :access-check.result (:kind %)) @seen)))))))))

(deftest a-box-is-expanded-to-its-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (settle {:from [5 64 5] :to [8 64 5]}))]
          (is (= [[5 64 5] [6 64 5] [7 64 5] [8 64 5]] (mapv :cell (:verdicts @out)))))))))

(deftest zones-footprints-and-ledger-come-from-the-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "farm" :min [5 60 5] :max [5 70 5]}
              {:keys [out]} (await (settle {:cells [[5 64 5] [6 64 5] [0 63 1]]
                                            :zones [zone] :footprints [[6 64 5]] :ledger [[0 63 1]]}))]
          (is (= {:ok false :reason :zone :zone "farm"} (select-keys (:dig (verdict-of out [5 64 5])) [:ok :reason :zone])))
          (is (= :footprint (get-in (verdict-of out [6 64 5]) [:dig :reason])))
          (is (= {:ok true} (:dig (verdict-of out [0 63 1])))))))))

(deftest zones-nil-refuses-every-cell-with-no-zones
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (settle {:cells [[5 64 5] [6 64 5]] :zones nil}))]
          (is (= [:no-zones :no-zones] (map #(get-in % [:dig :reason]) (:verdicts @out))))
          (is (= [:not-replaceable :not-replaceable] (map #(get-in % [:place :reason]) (:verdicts @out)))))))))

(deftest nothing-is-dug-or-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (settle {:cells [[5 64 5] [6 64 5]]}))
              acts (filter #(and (= :action (:source %)) (not= "wait" (:name %))) @seen)]
          (is (= "stone" (.-name (.blockAt p #js {:x 5 :y 64 :z 5}))))
          (is (empty? (filter #(re-find #"dig|place|jumpPlace" (str (:name %) (:action %))) acts))))))))

(deftest missing-cells-is-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (settle {}))]
          (is (= {:status :bad-args} (select-keys @out [:status]))))))))
