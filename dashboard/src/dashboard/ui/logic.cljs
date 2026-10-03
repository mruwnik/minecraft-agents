(ns dashboard.ui.logic
  (:require [clojure.string :as str]))

(def min-scale 0.05)
(def max-scale 200)

(defn time-ago-text [ms]
  (if-not (number? ms)
    "never"
    (let [s (js/Math.floor (/ (max ms 0) 1000))]
      (cond
        (< s 60) (str s "s ago")
        (< s 3600) (str (quot s 60) "m ago")
        (< s 86400) (str (quot s 3600) "h ago")
        :else (str (quot s 86400) "d ago")))))

(defn engine-body? [body] (some? (:engine body)))

(defn sort-bodies [bodies]
  (vec (sort-by (juxt (complement :up) :name) bodies)))

(defn split-bodies [bodies]
  (let [{engine true foreign false} (group-by engine-body? bodies)]
    {:engine (sort-bodies engine) :foreign (vec (sort-by :name foreign))}))

(defn world-from-search [search]
  (let [w (.get (js/URLSearchParams. search) "world")]
    (when-not (str/blank? w) w)))

(defn with-world [path world]
  (if (str/blank? world) path (str path "?world=" (js/encodeURIComponent world))))

(defn api-url [path world params]
  (let [pairs (concat (when-not (str/blank? world) [["world" world]])
                      (map (fn [[k v]] [(name k) v]) params))]
    (if (empty? pairs)
      path
      (str path "?" (str/join "&" (map (fn [[k v]] (str k "=" (js/encodeURIComponent (str v)))) pairs))))))

(defn page-for-path [path]
  (case (str/replace path #"/+$" "")
    "/villages" :villages
    "/villagers" :villagers
    "/blueprints" :blueprints
    :main))

(defn filter-chat [msgs needle hide-whispers?]
  (let [n (str/lower-case (or needle ""))
        hit? (fn [m] (or (str/blank? n)
                         (some #(str/includes? (str/lower-case (str %)) n) [(:from m) (:to m) (:message m)])))]
    (filterv #(and (hit? %) (not (and hide-whispers? (:to %)))) msgs)))

(defn zoom-view [view factor px py]
  (let [s (:scale view)
        s2 (min max-scale (max min-scale (* s factor)))]
    {:scale s2
     :origin-x (+ (:origin-x view) (- (/ px s) (/ px s2)))
     :origin-z (+ (:origin-z view) (- (/ py s) (/ py s2)))}))

(defn pan-view [view dx dy]
  (-> view
      (update :origin-x - (/ dx (:scale view)))
      (update :origin-z - (/ dy (:scale view)))))

(defn center-view [view x z width height]
  (assoc view
         :origin-x (- x (/ width 2 (:scale view)))
         :origin-z (- z (/ height 2 (:scale view)))))

(defn clock-text [clock]
  (if-not (map? clock)
    "no clock"
    (let [t (or (:timeOfDay clock) (:time-of-day clock) 0)
          hour (mod (+ (quot t 1000) 6) 24)
          minute (js/Math.floor (* 60 (/ (mod t 1000) 1000)))
          pad (fn [n] (if (< n 10) (str "0" n) (str n)))
          day-no (when (number? (:day clock)) (str "day " (:day clock) " "))
          daytime? (if (boolean? (:day clock)) (:day clock) (< (mod t 24000) 13000))]
      (str day-no (pad hour) ":" (pad minute) (if daytime? " (day)" " (night)")))))

(defn clock-ms-text [t]
  (if-not (number? t)
    (str t)
    (let [d (js/Date. t)
          pad (fn [n] (if (< n 10) (str "0" n) (str n)))]
      (str (pad (.getHours d)) ":" (pad (.getMinutes d)) ":" (pad (.getSeconds d))))))

;; items carry :px :py; nearest within radius r, else nil
(defn pick-nearest [items x y r]
  (let [d (fn [i] (js/Math.hypot (- (:px i) x) (- (:py i) y)))]
    (->> items (filter #(<= (d %) r)) (sort-by d) first)))

(defn cooldown-text [reflex]
  (let [state (if (:cooling? reflex) "cooling" "ready")]
    (if (:cooldown-s reflex) (str state " (" (:cooldown-s reflex) "s)") state)))
