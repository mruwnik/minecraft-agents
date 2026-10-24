(ns world-test.way-prior-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.event-counts :as c]
            [world-test.way-prior :as w]))

(defn pos [t x & {:as more}] (merge {:t t :k :pos :gen "g" :x x :y 64 :z 0} more))
(defn flee [t] {:t t :k :decided :gen "g" :job "j1" :mob "zombie" :decision :flee})

(deftest a-flight-starts-where-the-body-last-stood-and-a-walk-where-it-planned-one-spot-a-kind-a-section
  (let [spots (w/spots [(pos 0 1) (flee 500) (pos 600 2 :walk? true) (pos 700 3 :walk? true) (flee 800)
                        (pos 1000 40) (flee 5000)])]
    (is (= [[:flee [0 4 0]] [:walk [0 4 0]]] (map :id spots)) "the second flight and walk in the section add nothing; a flight 4 s after its last position has none")
    (is (= {:x 1 :y 64 :z 0} (:at (first spots))))))

(deftest a-walk-child-s-plan-is-read-as-a-walk-spot
  (is (:walk? (c/scan-line "{:time-ms 5, :generation-id \"g\", :kind :planned, :context {:job-id \"j2/trees/dig/walk\"}, :data {:pos {:x 1.5, :y 64, :z 2.5}}}")))
  (is (not (:walk? (c/scan-line "{:time-ms 5, :generation-id \"g\", :kind :planned, :context {:job-id \"j2/trees\"}, :data {:pos {:x 1.5, :y 64, :z 2.5}}}")))))

(deftest the-setting-is-the-sky-light-at-the-feet
  (is (= [:surface :roofed :cave nil] (map w/setting-of [15 7 0 nil]))))

(deftest places-whose-ways-all-lead-on-or-all-shut-give-a-weak-prior-at-their-mean
  (let [{:keys [mean strength at-edge spots]} (w/fit (concat (repeat 30 {:k 10 :n 10}) (repeat 10 {:k 0 :n 10}) [{:k 0 :n 0}]))]
    (is (= 40 spots) "a spot with no seen way tells nothing")
    (is (< 0.7 mean 0.8))
    (is (< strength 1) "each place is one way or the other: its own counts soon outweigh the prior")
    (is (false? at-edge))))

(deftest places-that-differ-only-by-chance-give-a-prior-without-bound
  (let [{:keys [mean at-edge]} (w/fit (repeat 40 {:k 5 :n 10}))]
    (is (< 0.45 mean 0.55))
    (is (true? at-edge) "a + b grows to the searched end: the spots are one binomial")))

(deftest settings-apart-beyond-chance-keep-their-own-means
  (let [rows (concat (map #(hash-map :setting :cave :k (if (even? %) 2 1) :n 10) (range 20))
                     (map #(hash-map :setting :surface :k (if (even? %) 9 8) :n 10) (range 20)))
        {:keys [cave surface all]} (w/by-setting rows)]
    (is (pos? (:tau2 all)))
    (is (< 0.1 (:shrunk cave) 0.2) "the settings' spread dwarfs each mean's own error: little pulled in")
    (is (< 0.8 (:shrunk surface) 0.9))))

(deftest a-later-read-replaces-one-only-when-it-read-more-ways
  (let [old {:id [:a] :k 3 :n 9}]
    (is (= old (get (w/fold {[:a] old} [{:id [:a] :k 0 :n 0}]) [:a])) "a section evicted from memory since")
    (is (= 12 (:n (get (w/fold {[:a] old} [{:id [:a] :k 5 :n 12}]) [:a]))))))
