(ns engine.lib-quote-test
  "jobs.lib.quote: a job's quote with its risk, children folded, and its risk in seconds."
  (:require [cljs.test :refer [deftest is async]]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.quote :as quote]))

(defn near? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest then-adds-time-and-hurt-and-combines-deaths-by-either
  (let [a {:uses {:job-time [1 2 3] :health [0 1 2] :items {"torch" [1 1 1]}} :death {:chance 0.1 :death 100}}
        b {:uses {:job-time [4 5 6] :items {"torch" [2 2 2] "coal" [1 1 1]}} :death {:chance 0.2 :death 400} :estimate true}
        q (quote/then a b)]
    (is (= [5 7 9] (get-in q [:uses :job-time])))
    (is (= [0 1 2] (get-in q [:uses :health])) "a child with no hurt adds none")
    (is (= {"torch" [3 3 3] "coal" [1 1 1]} (get-in q [:uses :items])))
    (is (near? 0.28 (get-in q [:death :chance])) "1 - 0.9 x 0.8, not the sum 0.3")
    (is (= (cost/either (:death a) (:death b)) (:death q)))
    (is (:estimate q))
    (is (nil? (:death (quote/then {:uses {:job-time [1 1 1]}}))) "no child risks a death")))

(deftest risk-s-prices-the-summed-hurt-at-the-health-so-children-are-folded-in-hp
  (let [child {:uses {:health [0 5 5]}}
        both (quote/then child child)]
    (is (= (health/hp-cost 20 10) (quote/hp-s 20 both)))
    (is (> (quote/hp-s 20 both) (* 2 (quote/hp-s 20 child)))
        "the second child's hp are lost from a lower health: summing seconds priced at the same start undercounts")
    (is (> (quote/hp-s 8 child) (quote/hp-s 20 child)) "the same hurt costs more to a hurt body")
    (is (= (+ (quote/hp-s 20 child) (* 0.01 900))
           (quote/hp-s 20 (assoc child :death {:chance 0.01 :death 900})))
        "a death adds its chance times its price")))

(def go-to-stub
  "A registry with a go-to that declares the args standard passes."
  {'jobs.movement.go-to {:args {:pos {:default nil} :back {:default false} :stay-s {:default nil}}
                         :round (fn [_]) :can-start? (constantly nil)}})

(deftest standard-is-the-walk-there-and-back-then-the-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom nil)
              walk {:uses {:job-time [10 12 20] :health [0 1 1]} :death {:chance 0.01 :death 600}}
              c {:engine {:jobs go-to-stub} :quote (fn [_ args _ _] (reset! asked args) (js/Promise.resolve walk))}
              work (quote/work :work)
              q (await ((quote/standard {:place (constantly {:x 5 :y 64 :z 5}) :category :work}) c))]
          (is (= {:pos {:x 5 :y 64 :z 5} :back true :stay-s (risk/work-s :work)} @asked))
          (is (= (quote/then walk work) q))
          (is (= (+ 1 (risk/work-hp :work)) (get-in q [:uses :health 1])))
          (reset! asked nil)
          (let [q (await ((quote/standard {:place (constantly nil) :category :long}) c))]
            (is (nil? @asked) "no place: no walk")
            (is (= (quote/work :long) q))))))))
