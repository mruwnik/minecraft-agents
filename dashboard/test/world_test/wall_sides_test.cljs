(ns world-test.wall-sides-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.wall-sides :as w]))

(defn line [t data] (pr-str {:time-ms t :kind :enter-room.arrived :data data}))

(deftest an-arrived-event-gives-its-sides-and-other-lines-none
  (is (= [5 {:seen 2 :unseen 3 :closed true}] (w/scan-line (line 5 {:seen 2 :unseen 3 :closed true}))))
  (is (nil? (w/scan-line (pr-str {:time-ms 5 :kind :hurt :data {}})))))

(deftest closed-rooms-count-all-their-unseen-sides-closed-and-open-ones-one-open
  (is (= {:rooms 3 :closed 2 :open 1 :setting {:k 5 :n 6}}
         (w/report {["A" 1] {:seen 1 :unseen 2 :closed true} ["A" 2] {:seen 0 :unseen 3 :closed true}
                    ["A" 3] {:seen 2 :unseen 4 :closed false} ["A" 4] {:seen 3 :unseen 0 :closed true}}))
      "a room with nothing unseen says nothing about unseen sides"))

(deftest a-row-folded-once-stays
  (is (= {:walls {:a {:unseen 1 :closed true}}}
         (w/fold {:walls {:a {:unseen 1 :closed true}}} {:a {:unseen 1 :closed false}}))))
