(ns engine.go-to-landing-test
  "go-to's :landing (block name -> share of a fall's damage), seen through path-preview (the fake world does not walk deep drops):
  a hay landing is planned where a stone one is not, a caller's entries win, bad ones are refused."
  (:require [cljs.test :refer [deftest is async]]
            [engine.path-preview-test :as pt]
            [engine.test-util :as tu :refer [floor box]]))

;; a plateau (feet 64) ending at x 10, an 11-block drop to a floor with feet 53: a stone one beyond x 13, `block` at x 11..13
(defn cliff [block] (merge (floor -2 -3 10 3) (floor 52 14 -3 47 3) (box 11 52 -3 13 52 3 block)))

(def args {:pos [40 53 0]})

(defn ^:async found? [block extra]
  (let [{:keys [out]} (await (pt/preview (cliff block) (merge args extra)))]
    (:found out)))

(deftest a-hay-landing-is-planned-where-a-stone-one-is-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (await (found? "stone" nil))) "an 11-block fall onto stone (8 hp) is over the default budget (7 hp)")
        (is (true? (await (found? "hay_block" nil))))))))

(deftest a-callers-landing-overrides-the-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (await (found? "hay_block" {:landing {"hay_block" 1}}))) "hay priced as a full fall")
        (is (true? (await (found? "stone" {:landing {"stone" 0}}))) "stone priced as a soft landing")))))

(deftest a-bad-landing-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [bad [{"no_such_block" 0.2} {"hay_block" "soft"} [1]]]
          (let [{:keys [out]} (await (pt/preview (cliff "stone") (assoc args :landing bad)))]
            (is (= :bad-landing (:reason out)) (pr-str bad))))))))
