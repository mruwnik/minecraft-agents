(ns engine.plan-trouble-round-test
  "A plan that goes bad between the engine's can-run? and the round is plan trouble for the next check to report
  (a wait, or impossible): the round does not end the call."
  (:require [cljs.test :refer [deftest is async]]
            [engine.build-from-plan-test :as bfp]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.core.ending :as ending]
            [engine.hostile-test :as h]
            [engine.pen-build-test :as pb]
            [engine.pen-check-test :as pc]
            [engine.pen-gate-test :as pg]
            [engine.registry :as registry]
            [engine.rail-line-test :as rl]
            [engine.tend-test :as tt]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as ew]))

(defn ^:async alive-after-plan-loss?
  "Submit form on eng; the first can-run? of the job sym drops the plans of the world store w after it answers. True
  when the job is still on the list after n ticks at a fixed clock (so the checks still read the plan as it was)."
  [eng w sym form n]
  (let [raw (get registry/jobs sym)
        orig (:can-run? raw)
        dropped (atom false)
        wrapped (fn [c]
                  (let [r (if orig (orig c) (contract/ok :test "no can-run?"))]
                    (when-not @dropped
                      (reset! dropped true)
                      (ew/set-data! w {} {}))
                    r))
        eng (assoc-in eng [:jobs sym] (ending/wrap (assoc raw :can-run? wrapped)))]
    (core/submit! eng form {})
    (dotimes [_ n] (await (core/tick! eng)))
    (boolean (seq (:list (core/state eng))))))

(deftest from-plan-round-leaves-plan-trouble-to-the-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng w]} (bfp/start {:inventory bfp/kit} {"pen" (bfp/pen-plan)})]
          (is (true? (await (alive-after-plan-loss? eng w 'jobs.build.from-plan '(jobs.build.from-plan {:plan "pen"}) 3)))))))))

(deftest build-pen-round-leaves-plan-trouble-to-the-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng w]} (bfp/start (pb/spec pb/kit) {"pen" (pb/ring-plan)} [])]
          (is (true? (await (alive-after-plan-loss? eng w 'jobs.build.pen '(jobs.build.pen {:plan "pen"}) 3)))))))))

(deftest rail-line-round-leaves-plan-trouble-to-the-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng w]} (bfp/start (rl/spec "stone" 63 (rl/kit {})) {"line" (rl/line-plan {})} [])]
          (is (true? (await (alive-after-plan-loss? eng w 'jobs.build.rail-line '(jobs.build.rail-line {:plan "line"}) 3)))))))))

(deftest rail-round-leaves-plan-trouble-to-the-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng w]} (bfp/start (rl/spec "stone" 63 (rl/kit {})) {"line" (rl/line-plan {})} [])]
          (is (true? (await (alive-after-plan-loss? eng w 'jobs.build.rail '(jobs.build.rail {:plan "line"}) 3)))))))))

(deftest pen-check-round-leaves-plan-trouble-to-the-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng w]} (bfp/start (pb/spec [] pb/built-pen) {"pen" (pb/ring-plan)} [])]
          (is (true? (await (alive-after-plan-loss? eng w 'jobs.animals.pen-check '(jobs.animals.pen-check {:plan "pen"}) 3)))))))))

(deftest shut-gate-round-leaves-plan-trouble-to-the-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plans {"pen-a" pg/pen-a}
              {:keys [eng]} (pg/start (pg/pen-world true 2 -7) plans)
              w (:world eng)]
          (is (true? (await (alive-after-plan-loss? eng w 'jobs.animals.shut-gate '(jobs.animals.shut-gate {:plan "pen-a"}) 3)))))))))

(deftest animals-tend-round-leaves-plan-trouble-to-the-checks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {"pen" tt/pen-plan} {} [])
              {:keys [eng]} (h/setup {:inventory [(tt/wheat 4)] :entities [(tt/cow 1 2) (tt/cow 2 3)]} 0 w)]
          (is (true? (await (alive-after-plan-loss? eng w 'jobs.animals.tend '(jobs.animals.tend {:plan "pen" :part "cows"}) 3)))))))))

(def stone-plan {:id "pen" :parts [{:id "s" :cells [[2 64 2]] :want "stone"}]})

(defn ^:async alive?
  "Whether the job sym with args is still on the list after 12 ticks: it waits, not ended."
  [[eng] sym args]
  (core/submit! eng (list sym args) {})
  (dotimes [_ 12] (await (core/tick! eng)))
  (boolean (seq (:list (core/state eng)))))

(defn starts
  "Per job: [sym args start-fn] over plans, the kit and pen worlds."
  [plans]
  (let [tend-world (fn [] (let [w (ew/of-data plans {} [])]
                            [(:eng (h/setup {:inventory [(tt/wheat 4)] :entities [(tt/cow 1 2)]} 0 w))]))]
    (merge
     {'jobs.build.from-plan [(fn [] [(:eng (bfp/start {:inventory bfp/kit} plans))]) {:plan "pen" :part "nope"}]
      'jobs.build.pen [(fn [] [(:eng (bfp/start (pb/spec pb/kit) plans []))]) {:plan "pen"}]
      'jobs.build.rail-line [(fn [] [(:eng (bfp/start (rl/spec "stone" 63 (rl/kit {})) plans []))]) {:plan "line"}]
      'jobs.build.rail [(fn [] [(:eng (bfp/start (rl/spec "stone" 63 (rl/kit {})) plans []))]) {:plan "line"}]
      'jobs.animals.pen-check [(fn [] [(:eng (bfp/start (pb/spec [] pb/built-pen) plans []))]) {:plan "pen"}]
      'jobs.animals.tend [tend-world {:plan "pen" :part "nowhere"}]}
     (into {} (map (fn [sym] [sym [(fn [] [(:eng (bfp/start {} plans))]) {:plan "pen"}]]))
           '[jobs.farm.harvest jobs.farm.plant jobs.farm.tend jobs.farm.tidy jobs.forestry.prepare jobs.forestry.maintain]))))

(deftest a-missing-plan-waits-in-every-plan-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[sym [start args]] (starts {})]
          (is (true? (await (alive? (start) sym args))) (str sym)))))))

(deftest a-readable-plan-that-cannot-be-built-ends-in-every-plan-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[sym [start args]] (starts {"pen" stone-plan "line" {:id "line" :parts [{:id "x" :cells [] :want "stone"}]}})]
          (is (false? (await (alive? (start) sym args))) (str sym)))))))
