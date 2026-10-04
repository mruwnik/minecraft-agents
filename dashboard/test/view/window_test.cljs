(ns view.window-test
  "The toroidal column window (ported from test/view-web-scene.test.mjs)."
  (:require [clojure.test :refer [deftest are is]]
            [view.window :as w]))

(def radius 1)
(def N (inc (* 2 radius)))

(defn start []
  (let [owners (js/Map.)
        ^js moved (w/move-window 0 0 radius (js/Map.) owners)]
    {:columns (.-columns moved) :fresh (.-fresh moved) :owners owners}))

(deftest a-first-window-holds-fresh-pending-columns-each-owning-its-slot
  (let [{:keys [^js columns ^js fresh ^js owners]} (start)]
    (is (= 9 (.-size columns)))
    (is (= 9 (alength fresh)))
    (is (= 9 (.-size owners)))
    (is (every? #(= "pending" (.-status ^js %)) (es6-iterator-seq (.values columns))))))

(deftest distance-is-from-the-eye-chunk
  (let [{:keys [^js columns]} (start)]
    (is (= 0 (.-dist ^js (.get columns "0.0"))))
    (is (= js/Math.SQRT2 (.-dist ^js (.get columns "1.1"))))))

(deftest kept-columns-get-their-distance-from-the-new-eye-chunk
  (let [{:keys [^js columns ^js owners]} (start)
        ^js moved (w/move-window 1 0 radius columns owners)]
    (is (identical? (.get columns "0.0") (.get (.-columns moved) "0.0")))
    (is (= 1 (.-dist ^js (.get (.-columns moved) "0.0"))))))

(deftest moving-keeps-what-it-can-and-drops-what-it-left
  (are [dx dz fresh-count gone]
       (let [{:keys [^js columns ^js owners]} (start)
             ^js moved (w/move-window dx dz radius columns owners)
             fresh (set (.-fresh moved))
             now (.-columns moved)]
         (and (= 9 (.-size now))
              (= fresh-count (count fresh))
              (every? (fn [[k column]] (= (identical? (.get columns k) column) (not (contains? fresh column))))
                      (es6-iterator-seq (.entries now)))
              (every? #(not (.has now %)) gone)))
    1 0 3 ["-1.-1" "-1.0" "-1.1"]
    0 1 3 ["-1.-1" "0.-1" "1.-1"]
    0 0 0 []
    10 -7 9 ["-1.0" "0.0" "1.0"]))

(deftest the-slot-of-a-departed-column-is-taken-over-by-the-column-that-wraps-onto-it
  (let [{:keys [^js columns ^js owners]} (start)
        slot (w/slot-key -1 0 N)]
    (is (= "-1.0" (.get owners slot)))
    (let [^js moved (w/move-window 1 0 radius columns owners)]
      (is (= slot (w/slot-key 2 0 N)))
      (is (= (w/key-of 2 0) (.get owners slot)))
      (is (some #(and (= 2 (.-cx ^js %)) (= 0 (.-cz ^js %))) (.-fresh moved))))))

(deftest every-slot-is-owned-by-exactly-one-wanted-column-after-any-move
  (let [{:keys [columns ^js owners]} (start)]
    (loop [^js columns columns
           [[x z] & more] [[1 0] [1 1] [-3 2] [-3 2] [0 0]]]
      (when (some? x)
        (let [^js now (.-columns (w/move-window x z radius columns owners))
              cs (vec (es6-iterator-seq (.values now)))]
          (is (= 9 (count (set (map #(w/slot-key (.-cx ^js %) (.-cz ^js %) N) cs)))))
          (is (every? #(= (w/key-of (.-cx ^js %) (.-cz ^js %)) (.get owners (w/slot-key (.-cx ^js %) (.-cz ^js %) N))) cs))
          (recur now more))))))

(deftest modulo-wraps-negatives
  (are [a n expected] (= expected (w/modulo a n))
    -1 5 4
    5 5 0
    -6 5 4))
