(ns jobs.strip.site-plan
  "The base plan's chests and furnaces and the ore loads for them, for the strip jobs."
  (:require [clojure.string :as str]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(defn spot [plan name]
  (some (fn [[k v]] (when (str/ends-with? k (str "/" name)) v)) (:spots plan)))

(defn plan-of [c id] (known/plan c id))
(defn chests [c id] (let [plan (plan-of c id)] (vec (keep #(spot plan %) ["chest-1" "chest-3"]))))
(def ores ["raw_iron" "raw_gold" "raw_copper"])
(defn furnaces
  "[[slot furnace-cell] ..] of the plan's furnaces."
  [c id] (let [plan (plan-of c id)] (vec (keep-indexed (fn [i k] (when-let [f (spot plan k)] [(keyword (str "smelt-" i)) f])) ["furnace-1" "furnace-2"]))))
(defn loads
  "[[slot args] ..] smelting the carried ore {item n} in the free furnaces fs [[slot cell] ..]: one kind split over them, several one kind each, most first."
  [fs carried ignore-zones?]
  (let [kinds (sort-by (comp - val) (filter (comp pos? val) (select-keys carried ores)))
        k (count fs)
        one (fn [[slot f] item n] [slot {:furnace f :item item :count (min 64 n) :ways #{:carried :chest} :ignore-zones? ignore-zones?}])]
    (cond (or (zero? k) (empty? kinds)) []
          (= 1 (count kinds)) (let [[item n] (first kinds)]
                                (vec (keep-indexed (fn [i fs1] (let [share (+ (quot n k) (if (< i (rem n k)) 1 0))] (when (pos? share) (one fs1 item share)))) fs)))
          :else (mapv (fn [fs1 [item n]] (one fs1 item n)) fs kinds))))

(defn shears? [c] (boolean (some #(= "shears" (:name %)) (u/inventory (:primitives c)))))
