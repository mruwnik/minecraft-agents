(ns jobs.lib.xz
  "A horizontal position arg: the one place that reads [x y z], [x z], {:x :y :z} and {:x :z} alike."
  (:require [engine.args :as a]))

(defn xz
  "{:x :z} for a position given as [x y z], [x z], {:x :y :z} or {:x :z} of numbers, else nil."
  [v]
  (cond
    (and (map? v) (every? #(number? (get v %)) [:x :z])) (select-keys v [:x :z])
    (and (sequential? v) (= 2 (count v)) (every? number? v)) (zipmap [:x :z] v)
    :else (some-> (a/cell v) (select-keys [:x :z]))))

(def position? (a/pred "[x z], [x y z], {:x :z} or {:x :y :z} of numbers" #(some? (xz %))))
