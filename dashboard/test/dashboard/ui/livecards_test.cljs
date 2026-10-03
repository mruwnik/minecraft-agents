(ns dashboard.ui.livecards-test
  (:require [cljs.test :refer [deftest are]]
            [dashboard.ui.livecards :as lc]))

(deftest flags-from-search
  (are [search expected] (= expected (lc/flags search))
    "" {:nogl? false :fps? false}
    "?fps=1" {:nogl? false :fps? true}
    "?nogl=1" {:nogl? true :fps? false}
    "?world=w&nogl=1&fps=1" {:nogl? true :fps? true}
    "?nogl=0&fps=0" {:nogl? false :fps? false}
    "?nogl=" {:nogl? false :fps? false}))

(deftest wants-scene
  (are [supported? flags status expected] (= expected (lc/wants-scene? supported? flags status))
    true {:nogl? false} :working true
    true {:nogl? false} :idle true
    true {:nogl? false} :trouble true
    true {:nogl? false} :offline false
    true {:nogl? true} :working false
    false {:nogl? false} :working false
    nil {:nogl? false} :working false))

(deftest show-canvas
  (are [stats expected] (= expected (lc/show-canvas? stats))
    nil false
    {} false
    {:fps 0 :loaded 0} false
    {:fps 0.0} false
    {:fps 5.5 :loaded 0} true
    {:fps 0 :loaded 3} true))

(deftest fps-label
  (are [stats expected] (= expected (lc/fps-label stats))
    nil "- fps"
    {} "- fps"
    {:fps 0} "0.0 fps"
    {:fps 5} "5.0 fps"
    {:fps 5.46} "5.5 fps"
    {:fps 12.04 :loaded 4} "12.0 fps"))
