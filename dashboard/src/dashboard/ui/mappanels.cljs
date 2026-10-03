(ns dashboard.ui.mappanels
  "The two lists beside the map, each shown or hidden from the right-hand bar: the places and the players."
  (:require [re-frame.core :as rf]
            [dashboard.ui.bodies :as bodies]
            [dashboard.ui.cards :as cards]))

(defn side-panel
  "A panel shown or hidden by its toggle on the right-hand bar (dashboard.ui.rail); nothing when hidden."
  [{:keys [id title count open?]} & content]
  (when open?
    [:aside {:id id}
     [:section
      [:div.sidebar [:h2 (str title " (" count ")")]]
      (into [:div.sidebody] content)]]))

(defn places-panel []
  (let [places @(rf/subscribe [:places])
        selected @(rf/subscribe [:selected])]
    (into [side-panel {:id "placespanel" :title "places" :count (count places)
                       :open? @(rf/subscribe [:places-open?])}]
          (map #(cards/place-row selected %))
          places)))

(defn player-row [selected {:keys [kind name where status seen-by] :as row}]
  ^{:key (str kind name)}
  [:div.place.player {:class (when (= selected {:kind kind :name name}) "sel")
                      :on-click #(rf/dispatch [:player-click row])}
   [:span.name name] " "
   (if status [bodies/status-pill status] [:span.muted (str "player" (when seen-by (str ", seen by " seen-by)))])
   [:div.muted where]])

(defn players-panel []
  (let [rows @(rf/subscribe [:player-rows])
        selected @(rf/subscribe [:selected])]
    (into [side-panel {:id "playerspanel" :title "players" :count (count rows)
                       :open? @(rf/subscribe [:players-open?])}]
          (map #(player-row selected %))
          rows)))
