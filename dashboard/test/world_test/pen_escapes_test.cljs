(ns world-test.pen-escapes-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.pen-escapes :as p]))

(deftest no-rows-leave-the-educated-guess
  (is (= {:runs 0 :escapes 0 :animal-s 0 :setting {:escapes 1 :animal-s 300}} (p/report {}))))

(deftest counted-rows-add-to-the-guess-as-pseudo-counts
  (is (= {:runs 2 :escapes 3 :animal-s 1800 :setting {:escapes 4 :animal-s 2100}}
         (p/report {"a" {:escapes 1 :animal-s 600} "b" {:escapes 2 :animal-s 1200}}))))

(deftest a-row-folded-once-stays
  (is (= {:pen-escapes {"a" {:escapes 1 :animal-s 600}}}
         (p/fold {:pen-escapes {"a" {:escapes 1 :animal-s 600}}} {"a" {:escapes 5 :animal-s 1}}))))

(deftest an-escaped-animal-counts-half-the-watch
  (is (= {:escapes 1 :animal-s 2100} (p/row 4 600 1 1)))
  (is (= {:escapes 0 :animal-s 2400} (p/row 4 600 0 1)))
  (is (= {:escapes 0 :animal-s 4800} (p/row 4 600 0 2))))
