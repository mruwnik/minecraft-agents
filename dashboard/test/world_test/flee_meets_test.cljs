(ns world-test.flee-meets-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.flee-meets :as m]))

(defn line [t job kind text] (pr-str {:kind kind :time-ms t :context {:job-id job} :message text}))

(deftest a-flight-counts-as-met-again-when-another-run-names-its-mob-in-the-window
  (let [lines [(line 1000 "j1" :hostile.fled "fled 30 blocks from zombie 4711")
               (line 2000 "j1/step" :retreat.target "flee step away from zombie 4711")
               (line 5000 "j2" :hostile.fled "fled 30 blocks from skeleton 815")
               (line 9000 "j3" :retreat.target "flee step away from zombie 4711")
               (line 99000 "j4" :retreat.target "flee step away from skeleton 815")]]
    (is (= {:k 1 :n 2} (m/report lines 60000)) "the zombie's own run does not count; the skeleton is named again too late")))
