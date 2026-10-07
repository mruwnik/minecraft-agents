(ns engine.near-go-test
  "jobs.lib.near/go-near!: a walk to something visible as a go-to child, so a body shut in escalates."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-escalate-test :as e :refer [in-pit pit stone-pit]]
            [engine.test-util :refer [floor]]
            [jobs.lib.near :as near]))

(defn probe
  "A job that calls go-near! toward pos each round and keeps its outcome in out."
  [out pos range & [opts]]
  {:check (constantly true)
   :round (fn ^:async probe-round [c]
            (let [w (await (near/go-near! c pos range (merge {:zone-tolls true} opts)))]
              (if (= :partial w)
                :continue
                (do (reset! out w) :done))))})

(defn ^:async run-probe! [world pos range & [opts]]
  (let [{:keys [eng] :as s} (e/setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'near-probe (probe out pos range opts)))]
    (core/submit! eng '(near-probe) {})
    (await (e/tick-out! eng 1000))
    (assoc s :out out)))

(deftest a-body-in-a-pit-reaches-its-target-by-escalating
  (async done
    ((fn ^:async t []
       (let [{:keys [out p]} (await (run-probe! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]}) {:x 10 :y 64 :z 0} 2))]
         (is (= :there @out))
         (is (<= (js/Math.abs (- 10 (first (e/feet p)))) 2))
         (done))))))

(deftest a-target-already-in-range-is-there-without-a-child
  (async done
    ((fn ^:async t []
       (let [{:keys [out p]} (await (run-probe! (merge in-pit {:blocks pit}) {:x 1 :y 61 :z 0} 3))]
         (is (= :there @out))
         (is (= [0 61 0] (e/feet p)))
         (done))))))

(deftest a-target-no-way-leads-to-is-blocked
  (async done
    ((fn ^:async t []
       (let [{:keys [out]} (await (run-probe! (merge in-pit {:blocks stone-pit}) {:x 10 :y 64 :z 0} 2))]
         (is (= :blocked @out) "no blocks and no pickaxe: nothing to escalate with")
         (done))))))

(deftest a-leg-that-got-nearer-is-partial
  (async done
    ((fn ^:async t []
       (let [{:keys [eng p]} (e/setup {:blocks (floor -2 -3 60 3)})
             firsts (atom [])
             job {:check (constantly true)
                  :round (fn ^:async leg-round [c]
                           (swap! firsts conj (await (near/go-near! c {:x 35 :y 64 :z 0} 0 {:leg-s 1 :escalate false})))
                           :done)}
             eng (assoc eng :jobs (assoc (:jobs eng) 'leg-probe job))]
         (core/submit! eng '(leg-probe) {})
         (await (e/tick-out! eng 1000))
         (is (= [:partial] @firsts) "a leg is partial, not blocked")
         (is (< 1 (first (e/feet p)) 35) "walked a leg toward the target")
         (done))))))

(deftest go-near-takes-no-drop-it-cannot-climb-back
  (async done
    ((fn ^:async t []
       (let [cliff (merge (floor -2 -3 10 3) (floor 60 11 -3 47 3))
             {:keys [out p]} (await (run-probe! {:blocks cliff} {:x 120 :y 61 :z 0} 1 {:escalate false}))]
         (is (= :blocked @out))
         (is (= 64 (js/Math.floor (second (e/feet p)))) "still on the plateau")
         (done))))))
