(ns jobs.lib.crops
  (:require [jobs.lib.look :as look]))

(defn seen-crops
  "The crops of the given names the body has seen within radius, still standing, nearest first, as {:name :pos :age}
  (the age as last seen)."
  [p names radius max]
  (->> (look/seen-blocks p {:names names :radius radius :max max :live? true :properties? true})
       (map (fn [b] (assoc b :age (some-> (get-in b [:properties :age]) js/Number))))))

(def ripe-age {"wheat" 7 "carrots" 7 "potatoes" 7 "beetroots" 3})

(def seed-of {"wheat" "wheat_seeds" "carrots" "carrot" "potatoes" "potato" "beetroots" "beetroot_seeds"})

(defn inc-in [m k] (update m k (fnil inc 0)))

(defn count-entry
  "Count a fail on pos in the list m[k] of {:pos :n}; at max fails the entry goes and (on-max m pos) is the result."
  [m k pos max on-max]
  (let [n (inc (or (:n (first (filter #(= pos (:pos %)) (k m)))) 0))]
    (if (>= n max)
      (on-max (update m k (fn [entries] (vec (remove #(= pos (:pos %)) entries)))) pos)
      (update m k (fn [entries] (conj (vec (remove #(= pos (:pos %)) entries)) {:pos pos :n n}))))))

(defn count-debt
  "Count a fail (field f) on the debt at pos in m[:replant]; at max fails (on-max m pos) is the result."
  [m f pos max on-max]
  (let [n (inc (or (f (first (filter #(= pos (:pos %)) (:replant m)))) 0))]
    (if (>= n max)
      (on-max m pos)
      (update m :replant (fn [debts] (mapv #(if (= pos (:pos %)) (assoc % f n) %) debts))))))
