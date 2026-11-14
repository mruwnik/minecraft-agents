(ns jobs.lib.look-line
  "What the body remembers of the ground along a straight line, as a player recalls a coast he walked: the water of it."
  (:require [jobs.lib.look :as look]))

(def stride
  "Blocks between the columns read along a line: a memory section's width (store sections are 16 blocks)."
  16)

(defn column-kind
  "How the column at x z reads from y: :wet when the first seen block not air, from near-blocks over y down to near-blocks
  under it, is water, :dry when it is another block, nil when the band holds no seen block."
  [p x y z]
  (let [top (+ (js/Math.floor y) look/near-blocks)]
    (loop [cy top]
      (when (>= cy (- top (* 2 look/near-blocks)))
        (let [b (look/seen-block p {:x x :y cy :z z})
              n (:name b)]
          (cond (or (nil? b) (:unknown b) (#{"air" "cave_air" "void_air"} n)) (recur (dec cy))
                (= "water" n) :wet
                :else :dry))))))

(defn wet-along
  "{:wet :dry :unknown} columns counted every stride blocks of the straight line from [x y z] to [x y z], the end not read:
  :wet and :dry as column-kind, :unknown a column holding no seen block."
  [p [x0 y0 z0] [x1 y1 z1]]
  (let [len (js/Math.hypot (- x1 x0) (- z1 z0))
        n (js/Math.floor (/ len stride))]
    (frequencies
     (for [i (range 1 (inc n))
           :let [f (/ (* i stride) len)
                 kind (column-kind p (js/Math.floor (+ x0 (* f (- x1 x0)))) (+ y0 (* f (- y1 y0))) (js/Math.floor (+ z0 (* f (- z1 z0)))))]]
       (or kind :unknown)))))
