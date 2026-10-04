(ns plan.conflicts
  "Where two active plans want different things of the same cell. Plain data, no IO, no host interop (.cljc, so the
  engine's code could use it as the dashboard does). Input is what plan.shape/expand returned per plan id; the cells are
  indexed once, so the cost is linear in the cells plus the pairs on each shared cell, never plan against plan.
  Two wants agree when they are equal (a bare block name is the block with no state) or when one :any shares a choice
  with the other; anything else is a conflict. Cells two plans cover with agreeing wants are an overlap, counted as :same
  on a pair that also conflicts and not reported otherwise: jobs working both plans do not undo each other there."
  (:require [clojure.set :as set]
            [plan.shape :as shape]))

(defn normal [want] (if (string? want) {:block want} want))

(defn choices [want]
  (if (and (vector? want) (= :any (first want)))
    (set (map normal (rest want)))
    #{(normal want)}))

(defn agree? [a b] (boolean (seq (set/intersection (choices a) (choices b)))))

(defn index
  "{pos [[plan-id want] ...]} over every plan's cells, each cell's entries in plan id order."
  [expansions]
  (let [by-cell (reduce-kv (fn [acc id {:keys [cells]}]
                             (reduce (fn [acc {:keys [pos want]}] (update acc pos (fnil conj []) [id want])) acc cells))
                           {} expansions)]
    (update-vals by-cell #(vec (sort-by first %)))))

(defn pairs-of [entries]
  (for [i (range (count entries)) j (range (inc i) (count entries))]
    [(nth entries i) (nth entries j)]))

(defn box-of [cells]
  (let [axis (fn [i f] (apply f (map #(nth % i) cells)))]
    {:min [(axis 0 min) (axis 1 min) (axis 2 min)] :max [(axis 0 max) (axis 1 max) (axis 2 max)]}))

(defn tally
  "{[a b] {:cells [pos ..] :same n}} for every pair of plans sharing a cell."
  [by-cell]
  (reduce-kv (fn [acc pos entries]
               (reduce (fn [acc [[a wa] [b wb]]]
                         (update acc [a b] (fn [t]
                                             (if (agree? wa wb)
                                               (update (or t {:cells [] :same 0}) :same inc)
                                               (update (or t {:cells [] :same 0}) :cells conj pos)))))
                       acc (pairs-of entries)))
             {} (into {} (filter #(> (count (val %)) 1)) by-cell)))

(defn conflicts
  "{plan-id expansion} -> [{:plans [a b] :count n :same m :box {:min :max} :cells [[x y z] ..]}], one per pair of plans
  that want different things of at least one cell, the pair with most cells first. Say which plans count by what you pass."
  [expansions]
  (->> (tally (index expansions))
       (keep (fn [[plans {:keys [cells same]}]]
               (when (seq cells)
                 (let [sorted (vec (sort cells))]
                   {:plans plans :count (count sorted) :same same :box (box-of sorted) :cells sorted}))))
       (sort-by (juxt (comp - :count) :plans))
       vec))

(defn active-conflicts
  "{id plan} and {name blueprint}: the conflicts between the :active plans. Proposed and retired plans conflict with nothing."
  [plans blueprints]
  (conflicts (into {} (for [[id plan] plans :when (= :active (:status plan))] [id (shape/expand plan blueprints)]))))

(defn per-plan
  "Conflicts as each plan sees them: {plan-id [{:with other :count n :box ..}]}, the worst first."
  [conflicts]
  (reduce (fn [acc {[a b] :plans :keys [count box]}]
            (-> acc
                (update a (fnil conj []) {:with b :count count :box box})
                (update b (fnil conj []) {:with a :count count :box box})))
          {} conflicts))
