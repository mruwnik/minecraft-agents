(ns engine.near-go-test
  "jobs.lib.near/go-near!: a walk to something visible as a go-to child, so a body shut in escalates."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.go-to-escalate-support :as e :refer [in-pit pit stone-pit]]
            [engine.test-util :refer [floor]]
            [jobs.lib.near :as near]))

(defn probe
  "A job that calls go-near! toward pos each round and keeps its outcome in out and its last-walk in walk."
  [out walk pos range & [opts]]
  {:round (fn ^:async probe-round [c]
            (let [w (await (near/go-near! c pos range opts))]
              (if (= :partial w)
                :continue
                (do (reset! out w) (reset! walk (near/last-walk c)) (ctx/end c :done nil)))))})

(defn ^:async run-probe! [world pos range & [opts]]
  (let [{:keys [eng] :as s} (e/setup world)
        out (atom :not-done)
        walk (atom :unset)
        eng (assoc eng :jobs (assoc (:jobs eng) 'near-probe (probe out walk pos range opts)))]
    (core/submit! eng '(near-probe) {})
    (await (e/tick-out! eng 1000))
    (assoc s :out out :walk walk)))

(deftest a-body-in-a-pit-reaches-its-target-by-escalating
  (async done
    ((fn ^:async t []
       (let [{:keys [out walk p]} (await (run-probe! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]}) {:x 10 :y 64 :z 0} 2))]
         (is (= :there @out))
         (is (= :done (:kind @walk)))
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
       (let [{:keys [out walk]} (await (run-probe! (merge in-pit {:blocks stone-pit}) {:x 10 :y 64 :z 0} 2 {:ways #{:walk :clear-seal}}))]
         (is (= :blocked @out) "escalation off: nothing leads to the target")
         (is (not= :done (:kind @walk)) "the walk's verdict is kept for the caller")
         (done))))))

(deftest a-target-no-way-leads-to-is-reached-by-hand-without-a-pickaxe
  (async done
    ((fn ^:async t []
       (let [{:keys [out]} (await (run-probe! (merge in-pit {:blocks stone-pit}) {:x 10 :y 64 :z 0} 2))]
         (is (= :there @out) "no blocks and no pickaxe: the stair is dug by hand")
         (done))))))

(defn leg-cut-go-to
  "A go-to that moves the body to x, then ends as the engine ends a call whose :spend :job-time ran out."
  [x]
  (assoc (get registry/jobs 'jobs.movement.go-to)
         :round (fn ^:async cut-round [c]
                  (fake/swap-self! (:primitives c) assoc :pos [x 64 0])
                  (ctx/end c :limit-reached {:reason :unreachable :why :search-spent :line :job-time} {:arrived false}))))

(defn ^:async leg-probe [x]
  (let [{:keys [eng p]} (e/setup {:blocks (floor -2 -3 60 3)})
        firsts (atom [])
        job {:round (fn ^:async leg-round [c]
                      (swap! firsts conj (await (near/go-near! c {:x 35 :y 64 :z 0} 0 {:leg-s 1 :ways #{:walk :clear-seal}})))
                      (ctx/end c :done nil))}
        eng (update eng :jobs assoc 'leg-probe job 'jobs.movement.go-to (leg-cut-go-to x))]
    (core/submit! eng '(leg-probe) {})
    (await (e/tick-out! eng 1000))
    @firsts))

(deftest a-leg-that-got-nearer-is-partial
  (async done
    ((fn ^:async t []
       (is (= [:partial] (await (leg-probe 20))) "a leg is partial, not blocked")
       (done)))))

(deftest a-leg-that-got-no-nearer-is-blocked
  (async done
    ((fn ^:async t []
       (is (= [:blocked] (await (leg-probe 0.5))))
       (done)))))

(deftest go-near-takes-no-drop-it-cannot-climb-back
  (async done
    ((fn ^:async t []
       (let [cliff (merge (floor -2 -3 10 3) (floor 60 11 -3 47 3))
             {:keys [out p]} (await (run-probe! {:blocks cliff} {:x 120 :y 61 :z 0} 1 {:ways #{:walk :clear-seal}}))]
         (is (= :blocked @out))
         (is (= 64 (js/Math.floor (second (e/feet p)))) "still on the plateau")
         (done))))))

(deftest go-near-forwards-the-y-margin-to-the-walk
  (async done
    ((fn ^:async t []
       (let [{:keys [eng]} (e/setup {:blocks (floor -2 -3 60 3)})
             seen (atom [])
             real (get-in eng [:jobs 'jobs.movement.go-to])
             spy (assoc real :round (fn [c] (swap! seen conj (get-in (:args c) [:settings :jobs.lib.walk.world/y-margin])) ((:round real) c)))
             job {:round (fn ^:async margin-round [c]
                           (await (near/go-near! c {:x 10 :y 64 :z 0} 0 {:y-margin 7 :ways #{:walk}}))
                           (ctx/end c :done nil))}
             eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.movement.go-to spy 'margin-probe job))]
         (core/submit! eng '(margin-probe) {})
         (await (e/tick-out! eng 1000))
         (is (= [7] (distinct @seen)))
         (done))))))
