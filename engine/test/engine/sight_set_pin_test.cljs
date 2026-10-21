(ns engine.sight-set-pin-test
  "The set of cells one full sight pass records, pinned on a few worlds: a speed-up of the ray loop must not change it."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.fake.node :as node]
            [engine.perception :as perception]
            [engine.test-util :as tu]))

(defn seen-set
  "[count checksum] of the (pos, name) pairs of every seen cell within 24 blocks after a forced pass."
  [blocks light yaw]
  (let [p (tu/blind (tu/fake {:blocks blocks}))
        per (perception/create (fake-raw/create p) {:rand (constantly 0.5)})]
    (swap! (fake/state p) merge {:yaw yaw} light)
    (perception/pass! per)
    (reduce (fn [[n sum] pos]
              (let [b (perception/seen-block per pos)]
                (if (or (nil? b) (:unknown b))
                  [n sum]
                  [(inc n) (mod (+ (* 31 sum) (hash [pos (:name b)])) 1000000007)])))
            [0 0]
            (for [x (range -24 25) y (range 56 80) z (range -24 25)] [x y z]))))

(def pillars
  (merge (tu/box -24 56 -24 24 63 24 "stone")
         (into {} (for [x (range -20 21 5) z (range -20 21 7) y (range 64 68)] [(str x "," y "," z) "stone"]))
         (tu/box -6 64 8 6 70 9 "stone")
         {"0,65,8" "air" "0,64,8" "air" "0,65,9" "air" "0,64,9" "air" "0,66,14" "gold_block" "3,65,12" "glass"}))

(def cave
  (merge (apply dissoc (tu/box -10 60 -10 10 72 10 "stone") (keys (tu/box -4 64 -4 4 67 4 "stone")))
         {"2,65,3" "gold_block"}))

(deftest pillared-field-lit
  (is (= [1858 200791081] (seen-set pillars {} 0))))

(deftest pillared-field-dark-facing-east
  (is (= [101 679848052] (seen-set pillars {:light-default [0 0] :light {[3 65 3] [0 14]}} 270))))

(deftest sealed-room-dark
  (is (= [96 711454558] (seen-set cave {:light-default [0 0]} 45))))

;; The same pin over the real raw world (engine/js/raw-world.mjs on prismarine chunk columns, engine/js/sight-bench-world.mjs).
(def bench-world (delay (node/require-here "./js/sight-bench-world.mjs")))

(defn store-checksum
  "[sections cells checksum] of the state ids in memory (not their times): the seen set."
  [per]
  (let [st (:st per)
        secs (mapcat (fn [^js m] (es6-iterator-seq (.values m))) (es6-iterator-seq (.values (.-stores ^js st))))]
    (reduce (fn [[n c sum] ^js sec]
              (let [^js ids (.-ids sec)]
                (loop [i 0 c c sum sum]
                  (if (< i 4096)
                    (let [v (aget ids i)]
                      (recur (inc i) (if (pos? v) (inc c) c)
                             (if (pos? v) (mod (+ (* sum 31) (* v (+ i (* 4096 (+ (.-cx sec) (* 64 (+ (.-cz sec) (* 64 (.-sy sec))))))))) 1000000007) sum)))
                    [(inc n) c sum]))))
            [0 0 0]
            (sort-by (fn [^js s] [(.-cx s) (.-sy s) (.-cz s)]) secs))))

(defn real-rig
  "{:per :pass!} over the real raw world of `kind` (hills, cave, pen): (pass! i) moves the eye to the i-th of eight cells,
  turns it and casts a whole pass."
  [kind]
  (let [^js w (.makeRaw ^js @bench-world kind)
        ^js bot (.-bot w)
        ^js pos (.. bot -entity -position)
        x0 (.-x pos)
        per (perception/create (.-raw w) {:rand (constantly 0.5)})]
    {:per per
     :pass! (fn [i]
              (set! (.-x pos) (+ x0 (mod i 8)))
              (set! (.-y pos) (inc (.groundAt ^js @bench-world kind (.-x pos) (.-z pos))))
              (set! (.. bot -entity -yaw) (* i 0.7))
              (perception/pass! per))}))

(defn real-seen [kind]
  (let [{:keys [per pass!]} (real-rig kind)]
    (dotimes [i 4] (pass! i))
    (store-checksum per)))

(deftest real-raw-world-hills (is (= [59 69407 827997028] (real-seen "hills"))))
(deftest real-raw-world-cave-dark (is (= [4 232 97244877] (real-seen "cave"))))
(deftest real-raw-world-pen-with-fences (is (= [50 93195 634842412] (real-seen "pen"))))
