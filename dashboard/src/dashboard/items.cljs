(ns dashboard.items
  "Short labels, titles and icon paths for inventory items. Pure."
  (:require [clojure.string :as str]))

(def name-re #"^[a-z0-9_]+$")

(defn label
  "A 2-3 letter tag for a slot without an icon: initials of the words, or the first 3 letters of a one-word name."
  [name]
  (let [words (remove str/blank? (str/split (str name) #"_"))]
    (cond
      (empty? words) ""
      (= 1 (count words)) (let [w (first words)] (str (str/upper-case (subs w 0 1)) (subs w 1 (min 3 (count w)))))
      :else (str/upper-case (apply str (map first (take 3 words)))))))

(defn title [name n]
  (str name (when (and (number? n) (> n 1)) (str " ×" n))))

(defn icon-candidates
  "The texture files that may hold the item's picture, best first (items, then blocks); nil for a name that is not a plain item id."
  [name]
  (when (and (string? name) (re-find name-re name))
    [(str "item/" name ".png") (str name ".png") (str name "_front.png") (str name "_side.png")]))

(defn icon-src [name]
  (when (icon-candidates name) (str "/api/item-icon/" name ".png")))
