(ns dashboard.ui.plans-events-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.ui.plans-events :as pe]))

(deftest the-view-and-mode-default-to-birds-eye-and-diff
  (are [plans expected] (= expected (pe/view-state plans))
    {} {:view :bird :mode "diff"}
    {:view :layer :mode "plan"} {:view :layer :mode "plan"}
    {:selected "a" :mode "world"} {:view :bird :mode "world"}))

(deftest picking-a-layer-switches-to-the-layer-view
  (is (= {:layer 64 :view :layer :mode "plan"} (pe/with-layer {:mode "plan" :view :bird} 64))))

(deftest view-and-mode-are-set-as-given
  (is (= {:view :bird} (pe/with-view {:view :layer} :bird)))
  (is (= {:mode "world"} (pe/with-mode {} "world"))))

(deftest selecting-another-plan-keeps-the-view-and-mode
  (is (= {:selected "b" :detail nil :detail-failed nil :layer nil :element nil :view :layer :mode "plan"}
         (pe/with-selected {:selected "a" :detail {:x 1} :layer 70 :element "e" :view :layer :mode "plan"} "b"))))

(deftest a-new-world-keeps-the-view-and-mode
  (is (= {:active? true :selected-wanted? false :view :layer :mode "world"}
         (pe/after-world-change {:active? true :selected-wanted? false :items [1] :selected "a" :layer 5 :view :layer :mode "world"}))))
