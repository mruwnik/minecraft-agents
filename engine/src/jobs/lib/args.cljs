(ns jobs.lib.args
  "Checks for the job args the spec :type cannot describe (lists, maps): a job's check declines :bad-args with the why."
  (:require [engine.ctx :as ctx]))

(defn names?
  "nil, or a collection (not a map) of strings and keywords."
  [v]
  (or (nil? v) (and (coll? v) (not (map? v)) (every? #(or (string? %) (keyword? %)) v))))

(defn targets?
  "nil, a bare id or name, or a collection of them."
  [v]
  (let [one? #(or (number? %) (string? %))]
    (or (nil? v) (one? v) (and (coll? v) (not (map? v)) (every? one? v)))))

(defn counts?
  "nil, or a map of item names to numbers."
  [v]
  (or (nil? v) (and (map? v) (every? (fn [[k n]] (and (string? k) (number? n))) v))))

(defn box?
  "nil, or {:min :max} each a map with numbers in :x :y :z."
  [v]
  (let [pos? (fn [p] (and (map? p) (every? #(number? (get p %)) [:x :y :z])))]
    (or (nil? v) (and (map? v) (pos? (:min v)) (pos? (:max v))))))

(def kinds
  {:names [names? "a collection of names"]
   :targets [targets? "an id, a name or a collection of them"]
   :counts [counts? "a map of item name to number"]
   :box [box? "a {:min :max} box of {:x :y :z}"]})

(defn problem
  "The first arg of args whose value does not fit its kind in want ({arg kind}, kinds), as a sentence, or nil."
  [args want]
  (some (fn [[k kind]]
          (let [[ok? human] (kinds kind)]
            (when-not (ok? (get args k)) (str (name k) " must be " human ", got " (pr-str (get args k))))))
        want))

(defn guard
  "The check run-check of job context c, unless an arg of want ({arg kind}, see kinds) is badly shaped: then declines :bad-args with a :why."
  [c want run-check]
  (if-let [why (problem (:args c) want)]
    (ctx/wait c {:reason :bad-args :why why})
    (run-check c)))
