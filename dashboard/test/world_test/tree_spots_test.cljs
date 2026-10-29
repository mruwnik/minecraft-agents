(ns world-test.tree-spots-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.tree-spots :as w]))

(defn world
  "A reader of the blocks in m ({[x y z] name}); everything else is air, lit 15."
  [m]
  (fn [x y z] {:name (get m [x y z] "air") :sky 15 :block 0}))

(def oak-trunk {[0 63 0] "grass_block" [0 64 0] "oak_log" [0 65 0] "oak_log" [0 66 0] "oak_log"})

(deftest a-tree-base-counts-plantable-when-its-room-and-light-are-there
  (is (= {:plantable 1} (w/tally (world oak-trunk) [[0 0]]))))

(deftest a-roofed-base-or-one-on-bad-soil-or-the-upper-log-is-no-plantable-base
  (is (= {:unfit 1} (w/tally (world (assoc oak-trunk [0 68 0] "stone")) [[0 0]])) "stone in the growth space")
  (is (= {} (w/tally (world (assoc oak-trunk [0 63 0] "stone")) [[0 0]])) "no soil: no tree grew here")
  (is (= {:unfit 1} (w/tally (fn [x y z] (assoc ((world oak-trunk) x y z) :sky 3)) [[0 0]])) "dark"))

(deftest the-report-pools-the-counts
  (let [r (w/report [{:plantable 9 :unfit 1} {:plantable 8 :unfit 2}] 1)]
    (is (= [17 3 0.85] ((juxt :plantable :unfit :p-plantable) r)))))
