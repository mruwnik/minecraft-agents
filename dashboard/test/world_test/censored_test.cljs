(ns world-test.censored-test
  (:require [cljs.test :refer [deftest is are]]
            [world-test.censored :as c]))

(defn near? [a b] (< (js/Math.abs (- a b)) 1e-3))

(defn mass-at [{:keys [at p]} x] (reduce + 0 (map (fn [a q] (if (= a x) q 0)) at p)))

(deftest times-seen-exactly-fit-their-own-share
  (let [f (c/fit [[1 1] [2 2] [2 2] [4 4]])]
    (is (= [0.25 0.5 0.25] (map #(mass-at f %) [1 2 4])))
    (is (= [1 2 2 4] (map #(c/quantile f %) [0.2 0.3 0.7 0.9])))))

(deftest a-time-censored-on-the-right-passes-its-share-to-the-later-times-as-kaplan-meier-does
  (let [f (c/fit [[1 1] [2 nil] [3 3] [4 4]])]
    (are [x share] (near? share (mass-at f x))
      1 0.25
      2 0
      3 0.375
      4 0.375)))

(deftest a-time-censored-at-a-tie-is-past-it-as-kaplan-meier-has-it
  (is (near? (/ 1 3) (mass-at (c/fit [[0 0] [0 nil] [5 5]]) 0))))

(deftest a-time-seen-within-a-gap-goes-where-the-exact-times-say
  (is (near? 0.75 (mass-at (c/fit [[0 2] [1 1] [1 1] [3 3]]) 1))))

(deftest past-every-seen-time-the-quantile-is-its-lower-bound
  (is (= 5 (c/quantile (c/fit [[1 1] [5 nil]]) 0.95))))

(def ramp "n times spread evenly from lo to hi." (fn [n lo hi] (mapv #(let [x (+ lo (* (- hi lo) (/ (+ % 0.5) n)))] [x x]) (range n))))

(def kinds {"zombie" (ramp 40 0 2) "creeper" (ramp 80 0 1.6) "enderman" (ramp 20 0.2 2.2) "rare" (ramp 2 0.2 1.8)})

(deftest a-kind-weighs-its-seen-starts-against-the-spread-the-kinds-show
  (let [{:keys [alpha]} (c/between kinds)
        shrunk (c/shrink kinds [0.05 0.5 0.95])]
    (is (< 3 alpha 30) "kinds a little apart give a finite prior strength")
    (is (near? (/ 40 (+ 40 alpha)) (get-in shrunk ["zombie" :w])))
    (is (< 0.6 (get-in shrunk ["zombie" :w]) 0.95) "the zombie's own starts weigh most")
    (is (< (get-in shrunk ["rare" :w]) 0.25) "a kind with 2 starts is mostly the others'")
    (is (every? (fn [{:keys [w own rest pooled]}] (every? true? (map #(near? %3 (+ (* w %1) (* (- 1 w) %2))) own rest pooled))) (vals shrunk))
        "each decile lies w of the way from the others' to its own")))

(deftest kinds-that-differ-by-chance-alone-are-each-the-pool
  (let [same {"a" (ramp 30 0 1) "b" (ramp 30 0 1)}]
    (is (= ##Inf (:alpha (c/between same))))
    (is (= 0 (get-in (c/shrink same [0.5]) ["a" :w])))))

(deftest a-censored-time-tells-of-the-mean-as-much-as-the-others-spread-it-closes
  (is (near? 2 (c/information [[1 1] [0 nil] [9 nil]] (c/fit (ramp 20 0 2)))) "seen 1, censored before all others 0, past all 1"))

(deftest a-kind-censored-past-every-other-start-weighs-as-one-seen
  (let [late (assoc kinds "late" [[5 nil]])]
    (is (near? (/ 1 (+ 1 (:alpha (c/between late)))) (get-in (c/shrink late [0.5]) ["late" :w])))))

(deftest a-kind-censored-before-every-other-start-is-the-others
  (is (= 0 (get-in (c/shrink (assoc kinds "unseen" [[0 nil]]) [0.5]) ["unseen" :w]))))
