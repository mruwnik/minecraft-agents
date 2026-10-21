(ns engine.go-to-fence-sliver-test
  "jobs.movement.go-to from a fence cell's sliver against the fake world: a walk that ends beside a fence post, outside
  the herd test's pen, and the next walk from there (it plans from the sliver the body is in, not through the post)."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-doors-test :as gd]
            [engine.herd-test :as ht]
            [engine.test-util :as tu]))

(defn ^:async two-walks!
  "go-to first, then go-to second, as children of one parent round; {:outs [first-result second-result] :s setup}."
  [world first second]
  (let [{:keys [eng clock] :as s} (gd/setup world [])
        outs (atom [])
        parent {:round (fn ^:async two-walks-round [c]
                         (let [r (await (ctx/call c :a 'jobs.movement.go-to first))]
                           (if (= :continue r)
                             r
                             (do (swap! outs conj (tu/outcome r))
                                 (let [r2 (await (ctx/call c :b 'jobs.movement.go-to second))]
                                   (if (= :continue r2) r2 (do (swap! outs conj (tu/outcome r2)) (ctx/end c :done nil))))))))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'two-walks parent))]
    (core/submit! eng '(two-walks) {})
    (await (gd/tick-out! eng clock 40))
    {:outs @outs :s (assoc s :eng eng)}))

(defn walker-faults [{:keys [seen]}] (filterv #(= :walker-fault (:kind %)) @seen))

(deftest a-walk-from-the-outer-sliver-of-a-fence-cell-goes-round-the-pen
  ;; the first walk stands in the south sliver of the pen's south fence cell (13,64,6), outside the pen; the second
  ;; goes west round the pen to (4,64,3), not north through the post into the pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [hitbox [false true]]
          (let [{:keys [outs s]} (await (two-walks! (merge (ht/world {}) {:self {:pos {:x 13 :y 64 :z 9}} :bodyHitbox hitbox})
                                                    {:pos [13 64 6] :range 0 :doors :never :ways #{:walk :dig} :may-dig #{:own} :spend {:tries 1}}
                                                    {:pos [4 64 3] :range 1 :doors :never :ways #{:walk :dig} :may-dig #{:own} :spend {:tries 1}}))
                [x _ z] (gd/at (:p s))]
            (is (= 2 (count outs)) (str "hitbox " hitbox ": both walks end: " (pr-str outs)))
            (is (empty? (walker-faults s)) (str "hitbox " hitbox ": " (pr-str (walker-faults s))))
            (is (<= (js/Math.hypot (- (js/Math.floor x) 4) (- (js/Math.floor z) 3)) 1) (str "hitbox " hitbox ": at " x "," z " " (pr-str (peek outs))))))))))
