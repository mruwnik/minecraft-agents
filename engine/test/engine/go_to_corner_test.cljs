(ns engine.go-to-corner-test
  "go-to plans with the body's abilities: at food 6 or less (no sprint) a corner jump past a high block is refused."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as g]
            [jobs.movement.go-to.result :as end]
            [engine.test-util :as tu :refer [box]]))

;; start (0 64 0) on a one-cell floor; a two-high column at (1 64..65 0), no floor at (0 63 1); landing floor (1 64 1) with air over it:
;; the only way to the landing is a diagonal jump out of a corner as high as the landing.
(def corner-world
  {:blocks (merge (box 0 63 0 0 63 0 "stone") (box 1 64 0 1 65 0 "stone") (box 1 64 1 1 64 1 "stone"))})

(deftest go-to-at-food-6-refuses-a-high-corner-jump
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (g/go! (assoc-in corner-world [:self :food] 6) {:pos [1 65 1] :ways #{:walk :clear-seal}}))]
          (is (= :corner-jump (:kind @out)) (pr-str @out))
          (is (re-find #"shut in" (:text @out)) "a sealed body is told it is shut in, not only that it lacks a sprint")
          (is (= [0 64 0] (g/at p)) "the body did not move"))))))

(deftest go-to-at-food-20-plans-the-high-corner-jump-it-can-sprint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (g/go! (assoc-in corner-world [:self :food] 20) {:pos [1 65 1]}))]
          ;; the fake's point body cannot slide a corner, so the walks stick on the planned jump (not refused for the
          ;; sprint) and the escalation makes a way
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [:stuck] (distinct (map :why (g/events-of s :go-to.escalated))))))))))

(deftest gap-kinds-have-plain-stopped-words
  (doseq [[kind part] [[:gap-sprint "takes a sprint"] [:gap-width "gap"]]]
    (let [text (end/give-up-words [0 64 0] {:why :abilities :kind kind})]
      (is (re-find (re-pattern part) text) text)
      (is (not (re-find #"move the body cannot make" text)) text))))

(deftest a-damage-refused-shut-in-says-the-ways-out-cost-hp
  (let [words #(end/give-up-words [0 64 0] {:why :start-enclosed :damage-refused %})]
    (is (re-find #"cost hp" (words true)))
    (is (re-find #"nothing it can walk" (words nil)))))
