(ns dashboard.ui.views
  "The shell: left rail, top bar with live counts, the page, and the chat panel on the right."
  (:require [re-frame.core :as rf]
            [dashboard.ui.bodies :as bodies]
            [dashboard.ui.cards :as cards]
            [dashboard.ui.chat :as chat]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.map :as map-ui]))

(def rail-items
  [[:bodies "/" "Bodies"] [:map "/map" "Map"] [:plans "/plans" "Plans"] [:villages "/villages" "Villages"]
   [:villagers "/villagers" "Villagers"] [:blueprints "/blueprints" "Blueprints"] [:jobs "/jobs" "Jobs"]])

(defn rail [page]
  (let [world @(rf/subscribe [:current-world])]
    [:nav#rail
     (for [[k path label] rail-items]
       ^{:key k} [:a.rail-item {:href (logic/with-world path world) :class (when (= k page) "active")} label])]))

(defn world-select []
  (let [names @(rf/subscribe [:world-names])
        current @(rf/subscribe [:current-world])]
    [:select {:title "which world to show" :value (or current "")
              :on-change #(rf/dispatch [:set-world (.. % -target -value)])}
     (for [n names] ^{:key n} [:option {:value n} n])]))

(def count-pills [[:working "working"] [:idle "idle"] [:trouble "in trouble"] [:offline "offline"]])

(defn count-pill [counts [k label]]
  ^{:key k}
  [:span.pill.count {:class (name k) :title label}
   [:b (get counts k 0)] " " label])

(defn topbar [page]
  (let [counts @(rf/subscribe [:status-counts])]
    [:header#topbar
     [:h1 "agent bodies"]
     [world-select]
     (into [:div.counts] (map #(count-pill counts %)) count-pills)
     [:span.spacer]
     [:span.dim.mono @(rf/subscribe [:clock-text])]
     [:span.dim @(rf/subscribe [:status])]
     (when (= page :map) [:button {:on-click #(rf/dispatch [:fit])} "fit everything"])]))

(defn map-page []
  [:main.map-page
   [:div#maps [map-ui/map-view]]
   [:aside [cards/places-panel]]])

(defn shell [page content]
  [:div#shell
   [rail page]
   [:div#col
    [topbar page]
    [:div#work
     [:div#content content]
     [chat/chat-panel]]]
   [bodies/detail-modal]])
