(ns dashboard.ui.hudmodel
  "The HUD and inventory of a body as plain data (from hud.json). Pure."
  (:require [clojure.string :as str]
            [dashboard.items :as items]))

(defn icons
  "Ten icons for a 0-20 value: :full, :half or :empty each."
  [value]
  (let [v (if (number? value) value 0)]
    (mapv (fn [i] (cond (>= v (* 2 (inc i))) :full (>= v (inc (* 2 i))) :half :else :empty)) (range 10))))

(def roman ["" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X"])

(defn duration-text [ticks]
  (when (and (number? ticks) (pos? ticks))
    (let [s (js/Math.floor (/ ticks 20))
          sec (mod s 60)]
      (str (quot s 60) ":" (if (< sec 10) (str "0" sec) sec)))))

(defn effect-text [{:keys [name amplifier duration]}]
  (let [level (inc (or amplifier 0))]
    (str/join " " (remove nil? [name (if (< level (count roman)) (roman level) level) (duration-text duration)]))))

(defn held-text [held]
  (if-not held "empty hand" (items/title (:name held) (:count held))))

(defn xp-model [xp]
  {:level (or (:level xp) 0)
   :percent (js/Math.round (* 100 (max 0 (min 1 (or (:progress xp) 0)))))})

(defn slot [by-slot n]
  (if-let [{:keys [name count]} (get by-slot n)]
    {:slot n :name name :count count :label (items/label name) :title (items/title name count) :icon (items/icon-src name)}
    {:slot n :empty? true}))

(defn slots
  "{:hotbar [9] :main [[9] [9] [9]] :armor [head chest legs feet] :offhand slot}, every slot {:slot n} or filled in."
  [inventory]
  (let [by-slot (into {} (map (juxt :slot identity)) inventory)
        at #(slot by-slot %)]
    {:hotbar (mapv at (range 36 45))
     :main (mapv (fn [row] (mapv at (range (+ 9 (* 9 row)) (+ 18 (* 9 row))))) (range 3))
     :armor (mapv at (range 5 9))
     :offhand (at 45)}))
