(ns world-test.far-side-rows-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.far-side-rows :as f]))

(defn line [t data] (pr-str {:time-ms t :kind :clear-path.far-side :data data}))

(deftest a-far-side-event-gives-its-rows-and-other-lines-none
  (is (= [5 {:seen 1 :rows 3 :dug 2 :open true}] (f/scan-line (line 5 {:seen 1 :rows 3 :dug 2 :open true :text "x"}))))
  (is (nil? (f/scan-line (pr-str {:time-ms 5 :kind :hurt :data {}})))))

(deftest unseen-rows-are-counted-wall-and-each-open-far-side-once
  (is (= {:doors 2 :open 1 :censored 1 :setting {:k 5 :n 6}}
         (f/report {["A" 1] {:seen 1 :rows 3 :dug 3 :open true}
                    ["A" 2] {:seen 2 :rows 5 :dug 5 :open false}}))
      "a door given up adds its wall rows, no open one")
  (is (= {:doors 1 :open 0 :censored 1 :setting {:k 4 :n 4}}
         (f/report {["A" 1] {:seen 1 :rows 5 :dug 5 :open false}}))
      "a censored door's rows are wall past :seen, never an exact end")
  (is (= {:doors 1 :open 0 :censored 1 :setting {:k 1 :n 1}}
         (f/report {["A" 1] {:seen 1 :rows 6 :dug 2 :open false}}))
      "rows planned but never dug are no wall looked at")
  (is (= {:doors 1 :open 0 :censored 1 :setting {:k 0 :n 0}}
         (f/report {["A" 1] {:seen 3 :rows 6 :dug 2 :open false}}))
      "dug rows within what was seen add nothing"))

(deftest a-row-folded-once-stays
  (is (= {:far-side {:a {:seen 1 :rows 2 :open true}}}
         (f/fold {:far-side {:a {:seen 1 :rows 2 :open true}}} {:a {:seen 1 :rows 9 :open false}}))))
