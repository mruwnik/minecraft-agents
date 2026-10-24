(ns jobs.movement.go-to.route-ways
  "A dig route's steps (jobs.lib.dig-route :steps) cut into the ways go-to runs back to back: a straight run of steps
  up a stair, a run straight up a pillar, a run across or down a dig route, and walks between."
  (:require [jobs.lib.escape :as escape]))

(defn step-kind
  "What way a route step from feet cell m is run by: [:pillar], [:stair heading] (a step up that digs or lays its
  floor, when stairs?), [:dig] (digging) or [:walk]."
  [stairs? m {:keys [to move dig]}]
  (cond
    (= :pillar move) [:pillar]
    (and stairs? (> (to 1) (m 1)) (or (= :laid move) (seq dig))) [:stair (escape/heading-names [(- (to 0) (m 0)) (- (to 2) (m 2))])]
    (seq dig) [:dig]
    :else [:walk]))

(defn segments
  "The ways, in order, of a route's steps (jobs.lib.dig-route :steps) from feet cell from, each with :from, the feet it
  starts at: {:step :pillar :height :roof} (:roof its cells dug overhead), {:step :stair :heading :steps},
  {:step :dig-route :cells} then a walk on to its end, {:step :approach :pos}; walks in a row are one, and none ends
  the route (go-to's own walk takes it on to the goal). Without stairs? a step up is dug as a dig route's."
  ([block-at unseen? from steps] (segments block-at unseen? from steps true))
  ([block-at unseen? from steps stairs?]
  (let [runs (partition-by first (map (fn [m st] [(step-kind stairs? m st) m st]) (cons from (map :to steps)) steps))
        way (fn [run]
              (let [[[kind start]] run
                    run-steps (map #(nth % 2) run)
                    end (:to (last run-steps))
                    digs (mapcat :dig run-steps)]
                (case (first kind)
                  :pillar [{:step :pillar :height (count run) :roof (count digs) :from start}]
                  :stair [{:step :stair :heading (second kind) :steps (count run) :from start}]
                  :dig [{:step :dig-route :cells (mapv (fn [c] {:cell c :block (block-at c) :guess (unseen? c)}) digs) :from start}
                        {:step :approach :pos end :from start}]
                  :walk [{:step :approach :pos end :from start}])))]
    (as-> (reduce (fn [ways w]
                    (if (= :approach (:step w) (:step (peek ways)))
                      (assoc-in ways [(dec (count ways)) :pos] (:pos w))
                      (conj ways w)))
                  [] (mapcat way runs)) ways
      (cond-> ways (= :approach (:step (peek ways))) pop)))))

(defn next-way
  "The climb's next way after escalation e ended done with the body at feet, its :then the rest; nil when there is
  none or it does not start at feet (a walk starts anywhere)."
  [e feet]
  (when-let [nxt (first (:then e))]
    (when (or (= :approach (:step nxt)) (= (:from nxt) feet))
      (assoc nxt :then (vec (rest (:then e)))))))
