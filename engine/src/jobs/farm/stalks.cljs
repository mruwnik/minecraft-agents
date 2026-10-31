(ns jobs.farm.stalks
  "Sugar cane and bamboo parts of a plan: the stalk a want names, and the stalk blocks above the base that farm.maintain cuts."
  (:require [jobs.lib.plan-field :as pf]
            [jobs.lib.util :as u]))

(def names #{"sugar_cane" "bamboo"})

(defn stalk-of
  "The stalk a plan want names (as the block, {:crop}, or an :any naming it), else nil."
  [want]
  (cond
    (string? want) (names want)
    (vector? want) (some stalk-of (rest want))
    (map? want) (names (or (:crop want) (:block want)))))

(defn base-cells
  "{pos stalk} of the cells of a plan answer (of part, when given) that want a stalk."
  [answer part]
  (into {} (keep (fn [{:keys [pos want]}]
                   (when-let [s (stalk-of want)] [(u/cell-pos pos) s])))
        (pf/part-cells answer part)))

(defn cut-cells
  "[[x y z]] of the stalk blocks just above the base cells {pos stalk} that stand on a base of the stalk: cutting there
  leaves the base to grow again."
  [p bases]
  (vec (keep (fn [[{:keys [x y z] :as pos} stalk]]
               (let [above (update pos :y inc)]
                 (when (and (= stalk (u/seen-name p pos)) (= stalk (u/seen-name p above)))
                   [x (inc y) z])))
             bases)))
