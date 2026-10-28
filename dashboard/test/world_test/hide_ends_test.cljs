(ns world-test.hide-ends-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.hide-ends :as h]))

(defn line [data] (pr-str {:kind :ended :message "hostile-near → jobs.survival.respond-to-hostile: done" :data data}))

(deftest a-flight-ended-hidden-says-whether-the-danger-stayed
  (is (= :spent (h/scan-line (line {:ended :hidden :hold :spent :dug-out true}))))
  (is (= :quiet (h/scan-line (line {:ended :hidden :hold :quiet}))))
  (is (nil? (h/scan-line (line {:ended :gone}))) "a flight that walked away hid nowhere")
  (is (= {:k 2 :n 3} (h/report [:spent :quiet :spent]))))
