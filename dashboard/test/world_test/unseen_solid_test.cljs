(ns world-test.unseen-solid-test
  (:require [cljs.test :refer [deftest is]]
            [engine.path.blocks :as blocks]
            [world-test.unseen-solid :as w]))

(defn- id [name] (w/open-ids) (first (blocks/state-ids (blocks/default-state-table) name)))

(defn- world
  "(fn [x y z]) -> {:id :block 0 :sky 0}: stone where (solid? x y z), else air; dark throughout."
  [solid?]
  (let [stone (id "stone") air (id "air")]
    (fn [x y z] {:id (if (solid? x y z) stone air) :block 0 :sky 0})))

(def hall "A stone floor under open air, the feet at y 64." (world (fn [_ y _] (< y 64))))

(def room "A 9-cube of air (x z -4..4, y 64..72) in stone." (world (fn [x y z] (not (and (<= -4 x 4) (<= -4 z 4) (<= 64 y 72))))))

(defn- by-d
  "{:solid :open} of the dark cells d past the frontier at walking height, of all q."
  [tally d]
  (apply merge-with + {:solid 0 :open 0} (keep (fn [[[_ d' lit q dy adj] v]] (when (and (= d d') (not lit) (= w/pooled-q q) (= 1 dy) (= w/pooled-adj adj)) v)) tally)))

(deftest in-the-dark-the-unseen-past-a-cut-ray-is-open-in-a-hall-and-solid-past-a-rooms-walls
  (let [h (w/stand-tally hall 0 64 0)
        r (w/stand-tally room 0 64 0)]
    (doseq [d [1 2 3]]
      (is (> (:open (by-d h d)) (:solid (by-d h d))) (str "hall, " d " past the dark frontier: mostly open")))
    (doseq [d [3 4 8]]
      (is (zero? (:open (by-d r d))) (str "room, " d " past the dark frontier: all its walls' rock")))
    (is (every? (fn [[[_ _ lit]]] (false? lit)) h) "no light: every frontier is dark")
    (is (= #{0 w/pooled-q} (set (map #(nth (key %) 3) h))) "a floor under open air: the lowest q bucket")))

(deftest the-prior-is-fitted-over-chunks-and-a-thin-bucket-takes-its-pooled-q-fit
  (let [k (fn [q] [64 1 false q 1 w/pooled-adj])
        chunk (fn [& qs] (into {} (concat (map (fn [[q v]] [(k q) v]) (partition 2 qs))
                                          [[(k w/pooled-q) (apply merge-with + (take-nth 2 (rest qs)))]])))
        r (w/report [(chunk 0 {:solid 1 :open 9} 2 {:solid 5 :open 5}) (chunk 0 {:solid 3 :open 7}) (chunk 0 {:solid 2 :open 8})] 1)
        {[[[_ [_ _ q0]] _ [_ [_ _ q2]]]] :dark [[[_ [_ _ n0]]]] :n-dark} (first (:sections r))]
    (is (> 1e-3 (js/Math.abs (- 0.2 (/ (first q0) (apply + q0))))) "the fit keeps the mean")
    (is (= 30 n0))
    (is (> 1e-3 (js/Math.abs (- (/ 0.8 3) (/ (first q2) (apply + q2))))) "one chunk of q 2: d 1 pooled over q (chunk shares 0.3 0.3 0.2)")))

(deftest a-cell-beside-a-seen-solid-one-is-counted-apart
  (let [h (w/stand-tally hall 0 64 0)
        under (fn [adj] (apply merge-with + {:solid 0 :open 0}
                               (keep (fn [[[_ _ _ q dy a] v]] (when (and (= w/pooled-q q) (= 0 dy) (= adj a)) v)) h)))]
    (is (pos? (:solid (under 1))) "rock under the floor, beside the floor's seen face")
    (is (zero? (:open (under 1))) "a hall's floor is rock all through")))
