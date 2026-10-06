(ns engine.explore-look-test
  (:require [cljs.test :refer [deftest is async]] [engine.core :as core]
            [engine.events :as events] [engine.registry :as registry]
            [engine.test-util :as tu] [jobs.explore.look :as look]))
(deftest limits-and-name-filters-are-validated-before-sensing
  (is (= ["stone"] (:block-names (look/options {:block-names "stone"}))))
  (is (:error (look/options {:radius 1000})))
  (is (:error (look/options {:max-blocks 100})))
  (is (:error (look/options {:max-entities -1})))
  (is (:error (look/options {:entity-names ["sheep" 3]})))
  (is (:error (look/options {:properties? "true"})))
  (is (:error (look/options {:at [1 nil 3]}))))
(deftest loaded-air-unloaded-cell-and-exact-properties-have-distinct-evidence
  (let [p (tu/seeing-all (tu/fake {:blocks {"1,64,0" "wheat"} :ages {"1,64,0" 7} :states {"1,64,0" {:age 7}} :unloaded ["4,64,0"]}))]
    (is (= {:name "wheat" :pos [1 64 0] :properties {:age 7} :seen? true}
           (:at (look/observe p (look/options {:at [1 64 0]})))))
    (is (= {:name "air" :pos [2 64 0] :seen? true}
           (:at (look/observe p (look/options {:at [2 64 0]})))))))
(deftest a-block-never-seen-is-unknown-not-reported-through-a-wall
  (let [p (tu/fake {:blocks {"1,64,0" "diamond_ore" "5,64,0" "stone"}
                    :entities [{:id 1 :name "Bob" :kind "player" :username "Bob" :pos {:x 6 :y 64 :z 0}}]})]
    (aset p "seenBlocks" (fn [q] (.filter (.blocks p q) (fn [b] (< (aget b "pos" "x") 3)))))
    (aset p "seenBlockAt" (fn [pos] (if (< (aget pos "x") 3)
                                      #js {:name "diamond_ore" :pos pos :age-ms 0}
                                      #js {:unknown true :pos pos})))
    (let [r (look/observe p (look/options {:max-entities 4 :at [5 64 0]}))]
      (is (= [[1 64 0]] (mapv :pos (:blocks r))) "the block in view is listed, not the one behind the wall")
      (is (= {:pos [5 64 0] :unknown true} (:at r)))
      (is (= ["Bob"] (mapv :name (:entities r))) "a player is listed even behind the wall"))))
(deftest a-queued-look-emits-useful-bounded-observations-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:blocks (tu/box 1 64 0 10 64 0 "stone")
                          :entities [{:id 1 :name "sheep" :kind "passive" :uuid "sheep-uuid" :pos {:x 2 :y 64 :z 0}}]}))
              [seen sink] (tu/capture-sink)
              eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir)
                                :events (events/make {:sinks [sink]})})]
          (core/submit! eng '(jobs.explore.look {:max-blocks 2 :radius 16}) {})
          (await (core/tick! eng))
          (let [event (first (filter #(= :look.observed (:kind %)) @seen))]
            (is (= [[1 64 0] [2 64 0]] (mapv :pos (get-in event [:data :blocks]))))
            (is (get-in event [:data :more-blocks?]))
            (is (= "sheep-uuid" (get-in event [:data :entities 0 :uuid])))
            (is (= :seen (get-in event [:data :scope]))))
          (is (empty? (:list (core/state eng))))
          (is (empty? (.-calls (.-world p)))))))))
