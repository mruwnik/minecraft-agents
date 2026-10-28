(ns world-test.wall-sides-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.wall-sides :as w]))

(defn line [t data] (pr-str {:time-ms t :kind :enter-room.arrived :data data}))

(deftest an-arrived-event-gives-its-sides-and-other-lines-none
  (is (= [5 {:seen 2 :unseen 3 :open-sides 1 :closed-sides 2}]
         (w/scan-line (line 5 {:seen 2 :unseen 3 :open-sides 1 :closed-sides 2 :closed false}))))
  (is (nil? (w/scan-line (line 5 {:seen 2 :unseen 3 :closed true}))) "an event without the side counts says nothing")
  (is (nil? (w/scan-line (pr-str {:time-ms 5 :kind :hurt :data {}})))))

(deftest sides-are-counted-as-recorded-not-by-room
  (is (= {:rooms 2 :closed 5 :open 3 :setting {:k 5 :n 8}}
         (w/report {["A" 1] {:seen 1 :unseen 3 :open-sides 0 :closed-sides 3}
                    ["A" 2] {:seen 2 :unseen 5 :open-sides 3 :closed-sides 2}
                    ["A" 3] {:seen 3 :unseen 0 :open-sides 0 :closed-sides 0}
                    ["A" 4] {:seen 1 :unseen 4}}))
      "a room with nothing unseen, or an old row without the counts, says nothing about unseen sides"))

(deftest a-row-folded-once-stays
  (is (= {:walls {:a {:unseen 1 :closed-sides 1}}}
         (w/fold {:walls {:a {:unseen 1 :closed-sides 1}}} {:a {:unseen 1 :closed-sides 0}}))))
