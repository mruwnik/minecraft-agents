(ns engine.bed-reach-test
  "Walks to a bed in a closed hut whose only door opens over a one-block hole (the planner names the bed cut off by a drop)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.go-to-escalate-support :refer [run-job! feet events-of]]
            [engine.test-util :as tu :refer [box]]))

;; ground top y 63 (feet 64); a hut x 0..4, z 0..4 on planks at y 64 (feet 65), cobblestone walls and roof, the bed at
;; (1,65,1)-(2,65,1), a door in the south wall at (2,65..66,4) whose front cell (2,65,5) has no floor: the ground there is
;; dug down to y 62
(def hut
  {:blocks (merge (box -10 63 -10 14 63 14 "stone")
                  (box 0 64 0 4 64 4 "oak_planks")
                  (box 0 65 0 4 66 4 "cobblestone")
                  (box 1 65 1 3 66 3 "air")
                  (box 0 67 0 4 67 4 "cobblestone")
                  {"2,65,4" "oak_door" "2,66,4" "oak_door" "2,63,5" "air"
                   "1,65,1" "red_bed" "2,65,1" "red_bed"})
   :states {"1,65,1" {:part "foot" :facing "east"} "2,65,1" {:part "head" :facing "east"}
            "2,65,4" {:open false :half "lower" :facing "south"} "2,66,4" {:open false :half "upper" :facing "south"}}})

(deftest reach-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (assoc hut :self {:pos {:x 0.5 :y 64 :z 8.5}} :inventory [{:name "stone_pickaxe" :count 1} {:name "dirt" :count 8}])
                                                    {:pos [1 65 1] :range 2}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (str "result " @out " at " (feet p) " " (mapv #(or (:text %) (:message %) (pr-str %)) (events-of seen :go-to.escalated)))))))))

(deftest reach-bed-over-a-two-deep-hole-with-a-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (update hut :blocks merge (box -10 62 -10 14 62 14 "stone"))
              {:keys [out p seen]} (await (run-job! (assoc world :self {:pos {:x 0.5 :y 64 :z 8.5}} :inventory [{:name "dirt" :count 8}])
                                                    {:pos [2 65 2] :range 0}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (str "result " @out " " (mapv :step (events-of seen :go-to.escalated)))))))))
