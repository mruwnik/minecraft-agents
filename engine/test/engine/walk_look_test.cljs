(ns engine.walk-look-test
  "jobs.movement.walk-look against the fake world: a dark tunnel whose far end a look from here cannot make out."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.reach.room :as reach-room]))

(def tunnel
  "A 1x2 tunnel x 0..12 in stone, sealed at both ends, the body at its west end."
  {:self {:pos {:x 0.5 :y 64 :z 0.5}}
   :blocks (merge (into {} (remove (fn [[k _]] (let [[x y z] (map js/Number (.split k ","))] (and (<= 0 x 12) (<= 64 y 65) (= z 0)))))
                        (tu/box -1 63 -1 13 66 1 "stone")))})

(defn dark-sides
  "unseen-sides as a dark tunnel shows them: the floor cells of the tunnel more than 4 blocks past every cell the body has
  stood in stay unseen; the far side is the first of them."
  [reached]
  (fn [_ feet]
    (swap! reached max (:x feet))
    (let [x (+ @reached 5)]
      (when (<= x 12) [[x 64 0] [x 65 0]]))))

(deftest a-walk-to-look-walks-down-a-dark-tunnel-until-its-end-shows-then-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [reached (atom 0)
              [seen sink] (tu/capture-sink)
              clock (atom 1000000)
              p (tu/seeing-all (tu/fake-on-floor tunnel))
              eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs registry/jobs
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (with-redefs [reach-room/unseen-sides (dark-sides reached)]
            (core/submit! eng '(jobs.movement.walk-look) {})
            (await (tu/tick-until-idle! eng 40)))
          (is (<= 8 @reached) "it walked on until the far end lay within sight")
          (is (= [0 64 0] (mapv js/Math.floor ((juxt #(.-x %) #(.-y %) #(.-z %)) (.-pos (.self p))))) "and came back")
          (is (seq (:cells (:data (mem/latest (mem/view (:store eng)) :walk-looked)))) "the sides it looked at are remembered")
          (is (= [:completed] (into [] (comp (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %)) (= "jobs.movement.walk-look" (:name (:data %))))) (map :kind)) @seen))))))))
