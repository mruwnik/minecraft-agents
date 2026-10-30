(ns world-test.look-seconds-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.look-seconds :as l]))

(defn line [data] (pr-str {:kind :sense.looked :data data}))

(deftest a-looked-event-gives-its-wall-seconds-per-look
  (is (= {:looks 2 :ms 540} (l/scan-line (line {:looks 2 :wall-ms 540}))))
  (is (nil? (l/scan-line (line {:looks 2}))) "an event from before the field")
  (is (nil? (l/scan-line (line {:looks 2 :wall-ms 540 :surveyed? true}))) "a survey's wall time is more than its looks")
  (is (some? (l/scan-line (line {:looks 2 :wall-ms 540 :surveyed? false})))))

(deftest the-setting-is-the-median-seconds-per-look
  (is (= {:n 3 :looks 4 :median 0.3 :mean 0.45 :setting 0.3}
         (l/report [{:looks 1 :ms 300} {:looks 2 :ms 600} {:looks 1 :ms 900}])))
  (is (= {:n 0 :looks 0} (l/report []))))
