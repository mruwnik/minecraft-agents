(ns jobs.strip.site-plan
  "The base plan's chests and furnaces, the ore loads for them and the smelt state, for jobs.strip.site."
  (:require [clojure.string :as str]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(defn smelt-saved
  "The site state after smelt-run! result r: cooking replaced by what still cooks plus what was loaded, the output counted."
  [s r kept]
  (cond-> (-> s
              (assoc :cooking (merge kept (:cooking r)))
              (update :smelted #(merge-with + (or % {}) (:smelted r)))
              (update :smelt-failed (fnil into []) (:failed r)))
    (some pos? (vals (:smelted r))) (assoc :bank true)))

(defn spot [plan name]
  (some (fn [[k v]] (when (str/ends-with? k (str "/" name)) v)) (:spots plan)))

(defn plan-of [c s] (known/plan c (:base s)))
(defn chests [c s] (let [plan (plan-of c s)] (vec (keep #(spot plan %) ["chest-1" "chest-3"]))))
(def ores ["raw_iron" "raw_gold" "raw_copper"])
(defn furnaces
  "[[slot furnace-cell] ..] of the plan's furnaces."
  [c s] (let [plan (plan-of c s)] (vec (keep-indexed (fn [i k] (when-let [f (spot plan k)] [(keyword (str "smelt-" i)) f])) ["furnace-1" "furnace-2"]))))
(defn loads
  "[[slot args] ..] smelting the carried ore {item n} in the free furnaces fs [[slot cell] ..]: one kind split over them, several one kind each, most first."
  [fs carried ignore-zones?]
  (let [kinds (sort-by (comp - val) (filter (comp pos? val) (select-keys carried ores)))
        k (count fs)
        one (fn [[slot f] item n] [slot {:furnace f :item item :count (min 64 n) :fetch {:what #{:item} :how #{:chest}} :ignore-zones? ignore-zones?}])]
    (cond (or (zero? k) (empty? kinds)) []
          (= 1 (count kinds)) (let [[item n] (first kinds)]
                                (vec (keep-indexed (fn [i fs1] (let [share (+ (quot n k) (if (< i (rem n k)) 1 0))] (when (pos? share) (one fs1 item share)))) fs)))
          :else (mapv (fn [fs1 [item n]] (one fs1 item n)) fs kinds))))

(defn shears? [c] (boolean (some #(= "shears" (:name %)) (u/inventory (:primitives c)))))
