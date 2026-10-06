(ns dashboard.ui.livecards-test
  (:require [cljs.test :refer [deftest are is]]
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
    {:allive? false} :offline :last-image
    {:allive? false} :manual :live
    {:allive? true} :offline :live
    {:allive? true} :working :live))

(deftest transition-plans
  ;; a body going offline swaps its live canvas (which closes its scene on unmount) for the server's image; the reverse opens a live scene
  (are [from to expected] (= expected [(lc/card-view :hub (lc/card-plan {:allive? false} from) true)
                                       (lc/card-view :hub (lc/card-plan {:allive? false} to) true)])
    :working :offline [:live :img]
    :trouble :offline [:live :img]
    :offline :working [:img :live]
    :offline :idle [:img :live]))

(deftest card-view
  (are [mode plan has-view? expected] (= expected (lc/card-view mode plan has-view?))
    :pending :live true :blank
    :pending :live false :blank
    :pending :last-image true :img
    :pending :last-image false :noview
    :still :live true :img
    :still :live false :noview
    :still :last-image true :img
    :hub :live true :live
    :hub :live false :noview
    :hub :last-image false :noview
    :hub :last-image true :img))

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
  ;; on unmount), so the transition is exactly a change of this value
  (are [status expected] (= expected (lc/card-view :hub (lc/card-plan {:allive? false} status) true))
    :working :live
    :manual :live
    :offline :img
    :idle :live))

(deftest add-scene-returns-nil-when-the-hub-is-full
  (let [hub #js {:addScene (fn [_] (throw (js/Error. "too many scenes")))}]
    (is (nil? (lc/add-scene hub "w" "Bot")))))

(deftest add-scene-addresses-the-body-as-world-slash-name
  (let [seen (atom nil)
        hub #js {:addScene (fn [opts] (reset! seen (.-agent opts)) :scene)}]
    (is (= :scene (lc/add-scene hub "w" "Bot")))
    (is (= "w/Bot" @seen))))
