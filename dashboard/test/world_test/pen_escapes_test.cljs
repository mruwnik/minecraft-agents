(ns world-test.pen-escapes-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.pen-escapes :as p]))

(deftest no-rows-leave-the-weak-guess
  (is (= {:runs 0 :escapes 0 :animal-s 0 :setting {:escapes 0.5 :animal-s 150}} (p/report {}))))

(deftest counted-rows-add-to-the-guess-as-pseudo-counts
  (is (= {:runs 2 :escapes 3 :animal-s 1800 :setting {:escapes 3.5 :animal-s 1950}}
         (p/report {"a" {:escapes 1 :animal-s 600} "b" {:escapes 2 :animal-s 1200}}))))

(deftest a-row-folded-once-stays
  (is (= {:pen-escapes {"a" {:escapes 1 :animal-s 600}}}
         (p/fold {:pen-escapes {"a" {:escapes 1 :animal-s 600}}} {"a" {:escapes 5 :animal-s 1}}))))

(deftest an-animal-out-between-polls-was-inside-half-a-step-more
  (is (= {:escapes 1 :animal-s 61} (p/row 1 60 2 1)))
  (is (= {:escapes 0 :animal-s 120} (p/row 0 120 2 1)))
  (is (= {:escapes 2 :animal-s 124} (p/row 2 60 2 2))))
