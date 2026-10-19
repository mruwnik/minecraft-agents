(ns engine.go-to-landing-test
  "The walk setting jobs.lib.cost.planner/landing (block name -> share of a fall's damage), seen through go-to's quote (the fake
  world does not walk deep drops): a hay landing is planned where a stone one is not, the setting's entries win, bad ones are refused."
  (:require [cljs.test :refer [deftest is async]]
            [engine.args :as a]
            [engine.go-to-quote-test :as pt]
            [engine.test-util :as tu :refer [floor box]]))

;; a plateau (feet 64) ending at x 10, an 11-block drop to a floor with feet 53: a stone one beyond x 13, `block` at x 11..13
(defn cliff [block] (merge (floor -2 -3 10 3) (floor 52 14 -3 47 3) (box 11 52 -3 13 52 3 block)))

(def args {:pos [40 53 0]})

(defn ^:async found? [block landing]
  (let [{:keys [out]} (await (pt/preview (cliff block) (cond-> args landing (assoc :settings {:jobs.lib.cost.planner/landing landing}))))]
    (:found out)))

(deftest a-hay-landing-is-planned-where-a-stone-one-is-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (await (found? "stone" nil))) "an 11-block fall onto stone (8 hp) is over the default budget (7 hp)")
        (is (true? (await (found? "hay_block" nil))))))))

(deftest the-landing-setting-overrides-the-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (await (found? "hay_block" {"hay_block" 1}))) "hay priced as a full fall")
        (is (true? (await (found? "stone" {"stone" 0}))) "stone priced as a soft landing")))))

(deftest a-bad-landing-setting-is-refused
  (doseq [bad [{"no_such_block" 0.2} {"hay_block" "soft"} [1]]]
    (is (string? (a/setting-problem :jobs.lib.cost.planner/landing bad)) (pr-str bad))))
