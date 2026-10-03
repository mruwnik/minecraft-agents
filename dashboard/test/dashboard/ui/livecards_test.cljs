(ns dashboard.ui.livecards-test
  (:require [cljs.test :refer [deftest are]]
            [dashboard.ui.livecards :as lc]))

(deftest flags-from-search
  (are [search expected] (= expected (lc/flags search))
    "" {:nogl? false :fps? false :allive? false}
    "?fps=1" {:nogl? false :fps? true :allive? false}
    "?nogl=1" {:nogl? true :fps? false :allive? false}
    "?allive=1" {:nogl? false :fps? false :allive? true}
    "?world=w&nogl=1&fps=1&allive=1" {:nogl? true :fps? true :allive? true}
    "?nogl=0&fps=0&allive=0" {:nogl? false :fps? false :allive? false}
    "?nogl=" {:nogl? false :fps? false :allive? false}))

(deftest hub-status
  (are [present? supported? waited expected] (= expected (lc/hub-status present? supported? waited))
    false nil 0 :loading
    false nil 7999 :loading
    false nil 8000 :unsupported
    true true 0 :supported
    true false 0 :unsupported
    true nil 0 :unsupported))

(deftest render-mode
  (are [status flags expected] (= expected (lc/render-mode status flags))
    :loading {:nogl? false} :pending
    :supported {:nogl? false} :hub
    :unsupported {:nogl? false} :still
    :supported {:nogl? true} :still
    :loading {:nogl? true} :still))

(deftest card-plan
  (are [flags status expected] (= expected (lc/card-plan flags status))
    {:allive? false} :working :live
    {:allive? false} :idle :live
    {:allive? false} :trouble :live
    {:allive? false} :offline :snapshot
    {:allive? true} :offline :live
    {:allive? true} :working :live))

(deftest next-snapshot
  (are [wanted done blocked expected] (= expected (lc/next-snapshot wanted done blocked))
    {} {} #{} nil
    {"B" 5 "A" 4} {} #{} "A"
    {"B" 5 "A" 4} {"A" {:mtime 4}} #{} "B"
    {"B" 5 "A" 4} {"A" {:mtime 3}} #{} "A"
    {"B" 5 "A" 4} {"A" {:mtime 4} "B" {:mtime 5}} #{} nil
    {"A" nil} {} #{} nil
    {"A" 4} {"A" {:mtime 4 :failed? true}} #{} nil
    {"B" 5 "A" 4} {} #{"A"} "B"
    {"A" 4} {} #{"A"} nil))

(deftest cooling-down
  (are [closed-at now expected] (= expected (lc/cooling-down closed-at now))
    {} 5000 #{}
    {"A" 4500} 5000 #{"A"}
    {"A" 4000} 5000 #{}
    {"A" 4001 "B" 1000} 5000 #{"A"}))

(deftest transition-plans
  ;; a body going offline closes its live scene (card-view :live -> :still, which takes a snapshot) and the reverse starts a live one
  (are [from to expected] (= expected [(lc/card-view :hub (lc/card-plan {:allive? false} from) true nil)
                                       (lc/card-view :hub (lc/card-plan {:allive? false} to) true nil)])
    :working :offline [:live :still]
    :trouble :offline [:live :still]
    :offline :working [:still :live]
    :offline :idle [:still :live]))

(deftest keep-record
  (are [record expected] (= expected (lc/keep-record? record))
    nil false
    {:mtime 1 :bitmap :bmp} false
    {:mtime 1 :failed? true} true))

(deftest snapshot-state
  (are [ready? waited expected] (= expected (lc/snapshot-state ready? waited))
    true 0 :ready
    true 99999 :ready
    false 0 :waiting
    false 19999 :waiting
    false 20000 :timeout))

(deftest card-view
  (are [mode plan has-view? still expected] (= expected (lc/card-view mode plan has-view? still))
    :pending :live true nil :blank
    :still :live true nil :img
    :still :snapshot false nil :noview
    :hub :live true nil :live
    :hub :live false nil :noview
    :hub :snapshot false nil :noview
    :hub :snapshot true nil :still
    :hub :snapshot true {:mtime 3} :still
    :hub :snapshot true {:mtime 3 :failed? true} :img))

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

(deftest no-world-data
  (are [stats expected] (= expected (lc/no-world-data? stats))
    {:loaded 0 :status "live"} true
    {:loaded 0 :status "ok"} true
    {:loaded 0} true
    {:loaded 0 :status "connecting"} false
    {:loaded 0 :status "unsupported"} false
    {:loaded 1 :status "live"} false
    {:loaded nil} false
    {} false
    nil false))

(deftest card-view-follows-the-body-going-offline-and-back
  ;; the preview element type is chosen by card-view; a different one remounts the component (live-canvas closes its scene
  ;; on unmount, still-canvas takes a snapshot on mount), so the transition is exactly a change of this value
  (are [status still expected] (= expected (lc/card-view :hub (lc/card-plan {:allive? false} status) true still))
    :working nil :live
    :manual nil :live
    :offline nil :still
    :offline {:mtime 3 :failed? true} :img
    :idle nil :live))
