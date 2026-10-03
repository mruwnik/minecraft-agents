(ns dashboard.ui.views
  (:require [re-frame.core :as rf]
            [dashboard.ui.cards :as cards]
            [dashboard.ui.chat :as chat]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.map :as map-ui]))

(defn nav-link [path label world]
  [:a {:href (logic/with-world path world)} label])

(defn world-select []
  (let [names @(rf/subscribe [:world-names])
        current @(rf/subscribe [:current-world])]
    [:select {:title "which world to show" :value (or current "")
              :on-change #(rf/dispatch [:set-world (.. % -target -value)])}
     (for [n names] ^{:key n} [:option {:value n} n])]))

(defn header [page]
  (let [world @(rf/subscribe [:current-world])]
    [:header
     [:h1 [:a {:href (logic/with-world "/" world)} "agent bodies"]]
     [world-select]
     [nav-link "/villages" "villages →" world]
     [nav-link "/villagers" "villagers →" world]
     [nav-link "/blueprints" "blueprints →" world]
     [:span.muted @(rf/subscribe [:clock-text])]
     [:span.muted @(rf/subscribe [:counts-text])]
     [:span.spacer]
     [:span.muted @(rf/subscribe [:status])]
     (when (= page :main) [:button {:on-click #(rf/dispatch [:fit])} "fit everything"])]))

(defn main-page []
  [:main
   [:div#mapwrap
    [:div#maps [map-ui/map-view]]
    [chat/chat-drawer]]
   [cards/side-panel]
   [cards/actions-popup]])
