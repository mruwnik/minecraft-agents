(ns jobs.lib.spent
  "What a job placed, so its result and done event can say what it used and where. jobs.lib.tidy/place! calls note!
  on every placed block; the job reads log from its memory and puts summary and text in its result and event.
  Log entries: {:item name :cell [x y z]}."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]))

(def max-cells "Cells named in a text before it ends with the last one." 5)

(defn note!
  "Job memory: item was placed at cell (a map with :x :y :z or a vector)."
  [c item cell]
  (let [cell (if (map? cell) [(:x cell) (:y cell) (:z cell)] (vec cell))]
    (ctx/update-mem! c update :placed-log (fnil conj []) {:item item :cell cell})))

(defn log [c] (:placed-log (ctx/mem c) []))

(defn summary
  "{:spent {item count} :spent-at [cell ...]} of a log."
  [entries]
  {:spent (frequencies (map :item entries)) :spent-at (mapv :cell entries)})

(defn text
  "\"used 7 oak_log (what at x y z ...)\" for a log, nil when nothing was placed."
  [entries what]
  (when (seq entries)
    (let [cells (mapv :cell entries)
          shown (if (> (count cells) max-cells) (conj (vec (take (dec max-cells) cells)) '... (last cells)) cells)
          counts (->> (frequencies (map :item entries)) (sort-by (comp - val)))]
      (str "used " (str/join ", " (map (fn [[item n]] (str n " " item)) counts))
           " (" what " at " (str/join " " (map #(if (= '... %) "..." (pr-str %)) shown)) ")"))))
