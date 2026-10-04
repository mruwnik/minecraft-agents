(ns dashboard.ui.views
  "The shell: left rail, top bar with live counts, the page, the chat panel, and the toggle bar on the right."
  (:require [re-frame.core :as rf]
            [dashboard.ui.bodies :as bodies]
            [dashboard.ui.chat :as chat]
            [dashboard.ui.detail :as detail]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.map :as map-ui]
            [dashboard.ui.mappanels :as mappanels]
            [dashboard.ui.rail :as rail]))

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

(defn count-pill [{:keys [state label count pressed? dim? title]}]
  ^{:key state}
  [:button.pill.count {:class [(name state) (when pressed? "on") (when dim? "dim")]
                       :title title :aria-pressed pressed?
                       :on-click #(rf/dispatch [:toggle-status-filter state])}
   [:b count] " " label])

(defn topbar [page]
  (let [chips @(rf/subscribe [:status-chips])]
    [:header#topbar
     [:h1 "Minecraft agents"]
     [world-select]
     (into [:div.counts] (map count-pill) chips)
     [:span.spacer]
     [:span.dim.mono @(rf/subscribe [:clock-text])]
     [:span.dim @(rf/subscribe [:status])]
     (when (= page :map)
       [:<> [:button {:on-click #(rf/dispatch [:fit-home])} "home"]
        [:button {:on-click #(rf/dispatch [:fit])} "fit all"]
        [:button {:on-click #(rf/dispatch [:fit-bodies])} "fit bodies"]
        [:button {:class (when @(rf/subscribe [:terrain?]) "on") :title "terrain under the map, from the dumped chunk columns"
                  :on-click #(rf/dispatch [:toggle-terrain])} "terrain"]])]))

(defn map-page []
  [:main.map-page
   [:div#maps [map-ui/map-view]]
   [mappanels/players-panel]
   [mappanels/places-panel]])

(defn shell [page content]
  [:div#shell
   [rail page]
   [:div#col
    [topbar page]
    [:div#work
     [:div#content content]
     [chat/chat-panel]
     [rail/rail-bar page]]]
   [detail/modal]])
