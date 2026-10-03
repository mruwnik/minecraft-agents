(ns dashboard.ui.rail-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.rail :as rail]))

(deftest every-page-has-the-chat-toggle
  (are [page] (= [:chat] (mapv :id (rail/toggles page {})))
    :bodies :plans :villages :jobs :blueprints))

(deftest the-map-page-has-chat-places-and-players-in-that-order
  (is (= [:chat :places :players] (mapv :id (rail/toggles :map {})))))

(deftest a-toggle-is-pressed-while-its-list-is-shown
  (is (= [true false true]
         (mapv :pressed? (rail/toggles :map {:chat-open? true :places-open? false :players-open? true})))))

(deftest a-toggle-dispatches-its-own-event
  (is (= [:toggle-chat :toggle-places :toggle-players] (mapv :event (rail/toggles :map {})))))

(deftest a-toggle-title-says-what-a-click-does
  (is (= ["hide chat" "show places" "show players"]
         (mapv :title (rail/toggles :map {:chat-open? true :places-open? false :players-open? false})))))
