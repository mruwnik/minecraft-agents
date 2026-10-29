(ns world-test.item-costs-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.item-costs :as ic]))

(defn line [job t kind data]
  (pr-str {:generation-id "g" :time-ms t :source :job :kind kind :context {:job-id job} :data data}))

(deftest a-gather-costs-its-seconds-to-its-last-mine-over-the-items-its-mines-got
  (let [rows (ic/spans (keep ic/scan-line [(line "j1/gather-raw_iron/stair" 1000 :stair.step {})
                                           (line "j1/gather-raw_iron" 31000 :mine.done {:got 0})
                                           (line "j1/gather-raw_iron/stair/ore" 50000 :mine.done {:got 9})
                                           (line "j1/gather-raw_iron" 61000 :mine.done {:got 2})
                                           (line "j2/gather-raw_iron" 0 :mine.done {:got 1})
                                           (line "j3/gather-oak_log/fell" 0 :planned {})
                                           (line "j4/craft" 0 :craft.done {:made 4})]))]
    (is (= {["g" "j1/gather-raw_iron"] {:item "raw_iron" :s 60 :got 2} ["g" "j2/gather-raw_iron"] {:item "raw_iron" :s 0 :got 1}} rows)
        "a fruitless mine's seconds count; a child's got and a gather with no mine.done do not")
    (is (= {"raw_iron" {:s 60 :got 3 :calls 2 :s-per-item 20}} (ic/report rows)))))

(deftest a-gather-read-again-keeps-its-longer-span
  (is (= {:item-costs {:a {:s 9 :got 1} :b {:s 1 :got 1}}}
         (ic/fold {:item-costs {:a {:s 9 :got 1}}} {:a {:s 4 :got 1} :b {:s 1 :got 1}}))))
