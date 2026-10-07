(ns engine.go-to-bubble-test
  "jobs.movement.go-to over a bubble column in the fake world: up a lifting column (soul sand) and down a dragging one (magma)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-test :as g]
            [engine.test-util :as tu :refer [box floor]]))

(def platform
  "A stone block 6 high at x 4..8, walled round its top except the cell at z 0 beside the column at x 3."
  (merge (floor -2 -3 8 3)
         (box 4 64 -3 8 69 3 "stone")
         (box 4 70 -3 8 72 -3 "stone")
         (box 4 70 3 8 72 3 "stone")
         (box 8 70 -3 8 72 3 "stone")
         (box 4 70 -3 4 72 -1 "stone")
         (box 4 70 1 4 72 3 "stone")))

(defn column
  "A bubble column at x 3, z 0 from y 64 to 69 over base, with drag as its state at each cell."
  [base drag]
  {:blocks (merge platform {"3,63,0" base} (box 3 64 0 3 69 0 "bubble_column"))
   :states (into {} (for [y (range 64 70)] [(str "3,"  y ",0") {:drag drag}]))})

(deftest go-to-rides-a-lifting-column-up-to-the-platform
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (g/go! (column "soul_sand" false) {:pos [5 70 0]}))]
          (is (= {:arrived true} (select-keys @out [:arrived])))
          (is (= 70 (second (g/at p))) "on the platform"))))))

(deftest go-to-rides-a-dragging-column-down-from-the-platform
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (g/go! (assoc (column "magma_block" true) :self {:pos {:x 5 :y 70 :z 0}})
                                            {:pos [0 64 0]}))]
          (is (= {:arrived true} (select-keys @out [:arrived])))
          (is (= 64 (second (g/at p))) "on the floor"))))))
