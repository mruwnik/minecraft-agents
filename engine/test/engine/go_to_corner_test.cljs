(ns engine.go-to-corner-test
  "go-to plans with the body's abilities: at food 6 or less (no sprint) a corner jump past a high block is refused."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as g]
            [jobs.movement.go-to :as go]
            [engine.test-util :as tu :refer [box]]))

;; start (0 64 0) on a one-cell floor; a two-high column at (1 64..65 0), no floor at (0 63 1); landing floor (1 64 1) with air over it:
;; the only way to the landing is a diagonal jump out of a corner as high as the landing.
(def corner-world
  {:blocks (merge (box 0 63 0 0 63 0 "stone") (box 1 64 0 1 65 0 "stone") (box 1 64 1 1 64 1 "stone"))})

(deftest go-to-at-food-6-refuses-a-high-corner-jump
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (g/go! (assoc-in corner-world [:self :food] 6) {:pos [1 65 1]}))]
          (is (= :corner-jump (:kind @out)) (pr-str @out))
          (is (= [0 64 0] (g/at p)) "the body did not move"))))))

(deftest go-to-at-food-20-plans-the-high-corner-jump-it-can-sprint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (g/go! (assoc-in corner-world [:self :food] 20) {:pos [1 65 1]}))]
          ;; the fake's point body cannot slide a corner, so the walk ends stuck on the planned jump: what counts is the jump was planned
          (is (= {:why :stuck :kind :jump} (select-keys @out [:why :kind])) (pr-str @out)))))))

(deftest gap-kinds-have-plain-stopped-words
  (doseq [[kind part] [[:gap-sprint "takes a sprint"] [:gap-width "gap"]]]
    (let [text (go/give-up-words [0 64 0] {:why :abilities :kind kind})]
      (is (re-find (re-pattern part) text) text)
      (is (not (re-find #"move the body cannot make" text)) text))))
